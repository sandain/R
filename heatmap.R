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

# Save the plot to a file.
if (grepl (".png$", outputFile)) png (outputFile)
if (grepl (".pdf$", outputFile)) pdf (outputFile)
if (grepl (".svg$", outputFile)) svg (outputFile)
if (grepl (".tif$", outputFile)) tiff (outputFile, compression="lzw")

# Draw the heat map.
heatmap (data[nrow (data):1,], Rowv=NA, Colv=NA, margins=c(10,30), col=colorRampPalette (c ("white","red"))(100), scale="none")
legend (x="topleft", legend=c("min", "ave", "max"), fill=colorRampPalette (c ("white","red"))(3))

# Finish the script.
q ()
