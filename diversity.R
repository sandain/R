#! /usr/bin/env Rscript

suppressPackageStartupMessages (library (vegan))
suppressPackageStartupMessages (library (ggplot2))
suppressPackageStartupMessages (library (gridExtra))

usage <- "Usage: <Data File> <Environment File> <Environmental Variable> <Output File> <Title>"

# Load the command line arguments.
args <- commandArgs (trailingOnly = TRUE)

# Verify the command line arguments.
if (length (args) < 5) stop (usage)

dataFile <- args[1]
envFile <- args[2]
envVar <- args[3]
outputFile <- args[4]
title <- args[5]

# Verify the data files exist.
if (! file.exists (dataFile)) stop ("Data file not found.")
if (! file.exists (envFile)) stop ("Environment file not found.")

# Load the data files.
data <- read.table (dataFile, header = TRUE)
env <- read.table (envFile, header = TRUE)

# Remove rows and columns with no data.
data <- data[, intersect (rownames (env), colnames (data)), drop = FALSE]
data <- data[rowSums (data) > 0,,drop = FALSE]
data <- data[, colSums (data) > 0,drop = FALSE]
env <- env[colnames (data),, drop = FALSE]

# The main data file comes in transposed from what we need.
data <- t (data)

# Figure out the groups in the environment.
envGroups <- env[! duplicated (env[,envVar]), envVar]
envGroups <- envGroups[! is.na (envGroups)]

# Calculate diversity indices.
d <- data.frame (
  row.names = rownames (data),
  category = env[,envVar],
  chao1 = double (nrow (data)),
  shannon = double (nrow (data)),
  simpson = double (nrow (data)),
  invsimpson = double (nrow (data))
)
for (i in 1: nrow (data)) {
  d$chao1[i] <- estimateR (data[i,])[2]
  d$shannon[i] <- diversity (data[i,], index = "shannon")
  d$simpson[i] <- diversity (data[i,], index = "simpson")
  d$invsimpson[i] <- diversity (data[i,], index = "invsimpson")
}


# Save the plot to a file.
if (grepl (".png$", outputFile)) png (outputFile)
if (grepl (".pdf$", outputFile)) pdf (outputFile)
if (grepl (".svg$", outputFile)) svg (outputFile)
if (grepl (".tif$", outputFile)) tiff (outputFile, compression="lzw")

p1 <- ggplot(d, aes(x=category, y=chao1)) + geom_dotplot(binaxis='y', stackdir='center', stackratio=1.5, dotsize=.5) + scale_y_continuous(trans='log10')
p2 <- ggplot(d, aes(x=category, y=shannon)) + geom_dotplot(binaxis='y', stackdir='center', stackratio=1.5, dotsize=.5) + scale_y_continuous(trans='log10')
p3 <- ggplot(d, aes(x=category, y=simpson)) + geom_dotplot(binaxis='y', stackdir='center', stackratio=1.5, dotsize=.5) + scale_y_continuous(trans='log10')
p4 <- ggplot(d, aes(x=category, y=invsimpson)) + geom_dotplot(binaxis='y', stackdir='center', stackratio=1.5, dotsize=.5) + scale_y_continuous(trans='log10')

grid.arrange (p1, p2, p3, p4, nrow = 2)

# Finish the script.
q ()
