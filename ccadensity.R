#!/usr/bin/env Rscript

suppressPackageStartupMessages (
  library (vegan)
)

usage <- "Usage: <Taxa file> <Environment file> <Taxa Categories> <Output File> <Formula>"

# Load the command line arguments.
args <- commandArgs (trailingOnly = TRUE)

# Verify the command line arguments.
if (length (args) < 5) stop (usage)

taxaFile <- args[1]
envFile <- args[2]
taxaCategoriesFile <- args[3]
outputFile <- args[4]
formula <- args[5]

# Verify the input files exist.
if (! file.exists (taxaFile)) stop ("Taxa file not found.")
if (! file.exists (envFile)) stop ("Environment file not found.")
if (! file.exists (taxaCategoriesFile)) stop ("Taxa Categories file not found.")

# Load the data files.
taxa <- read.table (taxaFile, header = TRUE)
env <- read.table (envFile, header = TRUE)
taxa_cat <- read.table (taxaCategoriesFile, header = TRUE)

# Verify the formula
formula <- as.formula (formula)
formula.vars <- all.vars (formula)
if (formula.vars[1] != "taxa") stop ("Invalid formula, taxa not defined.")

taxa <- taxa[intersect (rownames(env), rownames(taxa)),, drop = FALSE]
taxa <- taxa[rowSums (taxa) > 0,, drop = FALSE]
taxa <- taxa[, colSums (taxa) > 0, drop = FALSE]

taxa_cat <- taxa_cat[colnames (taxa),, drop = FALSE]
taxa_cat <- taxa_cat[!is.na (taxa_cat[, 'category']),]

if (nrow(taxa_cat) <= 1) stop ("Not enough taxa in category.")

# Remove empty rows and columns.
env <- env[rownames (taxa),, drop = FALSE]

# Run CCA.
taxa.cca <- cca (formula = formula, data = env)

# Figure out the categories.
cats <- taxa_cat[!duplicated (taxa_cat$category),]$category
cats <- cats[order (cats)]

# Calculate the height of the image.
height <- 50 * length (cats)

# Save the species plot to a file.
if (grepl (".png$", outputFile)) png (outputFile, width=630, height=height)
if (grepl (".pdf$", outputFile)) pdf (outputFile, width=630, height=height)
if (grepl (".svg$", outputFile)) svg (outputFile, width=630/72, height=height/72)

# Draw the CCA Density plot.
par (mfrow=c (length (cats), 1))
par (mar=c (1,4.1,1,1)) # 5.1 4.1 4.1 2.1
par (oma=c (1,0,0,0))
for (i in 1:length (cats)) {
  pts <- scores (taxa.cca)$species[rownames (taxa_cat[taxa_cat$category == cats[i],]),'CCA1', drop = FALSE]
  if (length (pts) > 1) {
    suppressWarnings (
      plot (density (pts), main=cats[i], xlim=c(-2,2), ylab="Density", axes=FALSE)
    )
  } else {
    plot (pts, 1.0, main=cats[i], xlim=c(-2,2), ylab="Singleton", axes=FALSE)
  }
  Axis (side=2, labels=TRUE)
  if (i == length (cats)) {
    Axis (side=1, labels=TRUE)
  } else {
    Axis (side=1, labels=FALSE)
  }
}

# Finish the script.
q ()
