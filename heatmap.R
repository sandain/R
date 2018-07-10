#!/usr/bin/env Rscript

usage <- "Usage: <Data File> <Output File>"

# Load the command line arguments.
args <- commandArgs (trailingOnly = TRUE)

# Verify the command line arguments.
if (length (args) < 2) stop (usage)

dataFile <- args[1]
outputFile <- args[2]

# Verify the data file exists.
if (! file.exists (dataFile)) stop ("Data file not found.")

# Load the data file.
data <- read.table (dataFile, header = TRUE)

# Make sure the data is formated as a matrix.
data <- as.matrix (data)

# Convert each column into percentages.
data <- t (t (data) / colSums (data))

# Cluster data using complete linkage.
data.dist <- dist (data)
data.cluster <- hclust (data.dist, method="complete")

# Create a dendrogram of the clustered rows.
data.rowv <- rowMeans (data, na.rm=TRUE)
data.dendrogram <- as.dendrogram (data.cluster)
data.dendrogram <- reorder (data.dendrogram, data.rowv)

# Save the plot to a file.
if (grepl (".png$", outputFile)) png (outputFile)
if (grepl (".pdf$", outputFile)) pdf (outputFile)
if (grepl (".svg$", outputFile)) svg (outputFile)
if (grepl (".tif$", outputFile)) tiff (outputFile, compression="lzw")

# Draw the heat map.
heatmap (data, Rowv=data.dendrogram, Colv=NA, labRow=NA)

# Finish the script.
q ()
