#!/usr/bin/env Rscript

suppressPackageStartupMessages (
  library (Ternary)
)

usage <- "Usage: <Data File A> <Data File B> <Data File C> <Output File> <Axis Title A> <Axis Title B> <Axis Title C> <Title>"

colors <- c ("#e6194b", "#fabebe", "#aa6e28", "#f58231", "#ffd8b1", "#808000", "#ffe119", "#fffac8", "#d2f53c", "#3cb44b", "#aaffc3", "#f032e6", "#008080", "#46f0f0", "#000080", "#0082c8", "#911eb4", "#e6beff", "#f032e6")

# Load the command line arguments.
args <- commandArgs (trailingOnly = TRUE)

# Verify the command line arguments.
if (length (args) < 8) stop (usage)

dataFileA <- args[1]
dataFileB <- args[2]
dataFileC <- args[3]
outputFile <- args[4]
axisTitleA <- args[5]
axisTitleB <- args[6]
axisTitleC <- args[7]
title <- args[8]

# Verify the data files exist.
if (! file.exists (dataFileA)) stop ("Data file A not found.")
if (! file.exists (dataFileB)) stop ("Data file B not found.")
if (! file.exists (dataFileC)) stop ("Data file C not found.")

# Load the data files.
dataA <- read.table (dataFileA, header = TRUE)
dataB <- read.table (dataFileB, header = TRUE)
dataC <- read.table (dataFileC, header = TRUE)

# Verify that all data tables contain the same rows.
if (! all.equal (rownames (dataA), rownames (dataB)))
  stop ("Data files A and B do not have the same rows")
if (! all.equal (rownames (dataB), rownames (dataC)))
  stop ("Data files B and C do not have the same rows")
if (! all.equal (rownames (dataA), rownames (dataC)))
  stop ("Data files A and C do not have the same rows")

# Calculate the data table.
data <- data.frame (row.names = rownames (dataA))
data$A <- rowSums (dataA)
data$B <- rowSums (dataB)
data$C <- rowSums (dataC)

# Remove rows with no data.
data <- data[rowSums (data) > 0,,drop = FALSE]

# Convert the data table into percentages.
data.percent <- data.frame (row.names=rownames (data))
for (i in 1:ncol (data)) {
  data.percent[,i] <- data[,i] / sum (data[,i])
}

# Save the plot to a file.
if (grepl (".png$", outputFile)) png (outputFile)
if (grepl (".pdf$", outputFile)) pdf (outputFile)
if (grepl (".svg$", outputFile)) svg (outputFile)
if (grepl (".tif$", outputFile)) tiff (outputFile, compression = "lzw")

# Create the plot.
TernaryPlot (alab=axisTitleA, blab=axisTitleB, clab=axisTitleC, main=title)
for (i in 1:nrow (data)) {
  TernaryPoints (data.percent[i,], pch=21, col="#000000", bg="#ff0000", cex=log (sum (data[i,]) / 5000))
}

# Finish the script.
q ()
