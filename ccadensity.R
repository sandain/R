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

# Save the species plot to a file.
if (grepl (".png$", outputFile)) png (outputFile)
if (grepl (".pdf$", outputFile)) pdf (outputFile)
if (grepl (".svg$", outputFile)) svg (outputFile)

# Draw the CCA Density plot.
cats <- taxa_cat[!duplicated (taxa_cat$category),]$category
s <- scores (taxa.cca)$species
par (mfrow=c (length (cats), 1))
for (i in 1:length (cats)) {
  plot (density (s[rownames (taxa_cat[taxa_cat$category == cats[i],]),'CCA1']), main=cats[i], xlim=c(-2,2))
}

# Finish the script.
q ()
