#!/usr/bin/Rscript

suppressPackageStartupMessages (
  library (vegan)
)

usage <- "Usage: <Data File> <Environment File> <Environmental Variable> <Output File> <Title>"

colors <- c ("red", "blue", "green")

# Load the command line arguments.
args <- commandArgs (trailingOnly = TRUE)

# Verify the command line arguments.
if (length (args) < 4) stop (usage)

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

# The main data file comes in transposed from what we need.
data <- t (data)

# Remove rows and columns with no data.
data <- data[rowSums (data) > 0,,drop = FALSE]
data <- data[, colSums (data) > 0,drop = FALSE]

envGroups <- env[! duplicated (env[,envVar]), envVar]

# Save the plot to a file.
if (grepl (".png$", outputFile)) png (outputFile)
if (grepl (".pdf$", outputFile)) pdf (outputFile)
if (grepl (".svg$", outputFile)) svg (outputFile)
if (grepl (".tif$", outputFile)) tiff (outputFile, compression="lzw")

# Run MDS on the data.
data.mds <- metaMDS (data)

# Create an empty plot sized to fit the MDS output.
data.plot <- plot (data.mds, type="n", main=title)

# Draw ovals around each environmental variable.
ordiellipse (data.plot, env[,envVar], kind="se", conf=0.99)

# Draw bounding boxes around each environmental variable.
#ordihull (data.plot, env[,envVar])

# Draw sites from MDS output colored by environmental variable.
for (i in 1: length (envGroups)) {
  points (data.plot$sites[env[,envVar] == 1,], col="black", bg=colors[i], pch=21)
}

# Add labels to the sites.
text (data.plot$sites, labels=rownames(data.plot$sites), pos=4)

# Add a legend.
legend ("topright", legend=envGroups, col=rep("black",3), pt.bg=colors, pch=21)

# Finish the script.
q ()
