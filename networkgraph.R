#!/usr/bin/env Rscript

suppressPackageStartupMessages (
  library (igraph)
)

usage <- "Usage: <Data File> <Environment File> <Output File> <Title> <Cutoff>"

# Load the command line arguments.
args <- commandArgs (trailingOnly = TRUE)

# Verify the command line arguments.
if (length (args) < 5) stop (usage)

dataFile <- args[1]
envFile <- args[2]
outputFile <- args[3]
title <- args[4]
cutoff <- args[5]

# Verify the data files exist.
if (! file.exists (dataFile)) stop ("Data file not found.")
if (! file.exists (envFile)) stop ("Environment file not found.")

# Load the data files.
data <- read.table (dataFile, header = TRUE)
env <- read.table (envFile, header = TRUE)

# Remove rows and columns with no data.
data <- data[,intersect (rownames (env), colnames (data)),drop = FALSE]
data <- data[rowSums (data) > 0,,drop = FALSE]
data <- data[, colSums (data) > 0,drop = FALSE]
env <- env[colnames (data),, drop = FALSE]

# Save the plot to a file.
if (grepl (".png$", outputFile)) png (outputFile)
if (grepl (".pdf$", outputFile)) pdf (outputFile)
if (grepl (".svg$", outputFile)) svg (outputFile)
if (grepl (".tif$", outputFile)) tiff (outputFile, compression="lzw")

# Build the adjacency matrix.
adjm <- matrix (nrow=nrow (env), ncol=nrow (env))
for (i in 1:nrow (env)) {
  for (j in 1:i) {
    if (i == j) {
      # Don't create an edge if the indices are the same.
      adjm[i,j] <- 0
    }
    else {
      # Count the number of species in common between the two indices.
      adjm[i,j] <- sum (data[,i] > 0 & data[,j] > 0, na.remove = TRUE)
    }
    # Don't create an edge if the cutoff is not met.
    if ((adjm[i,j] / nrow (data)) < cutoff) adjm[i,j] <- 0
  }
}

# Create the graph from the adjacency matrix.
g <- graph.adjacency (adjm, mode="lower", weighted=TRUE)

# Layout the graph in a circle.
g.layout <- layout.circle (g)

# Rename the vertices.
V(g)$name <- as.vector (env[,'name'])

# Change the color of the vertices.
V(g)$color <- as.vector (env[,'color'])

# Change the size of the vertices.
V(g)$size <- as.vector (sapply (colSums (data), log))

# Plot the graph.
plot (g, main=title, layout=g.layout, edge.curved=0.5, vertex.label="")

# Add vertex labels to the plot.
for (i in 1:nrow (env)) {
  label <- V(g)$name[i]
  angle <- atan (-1.0 * g.layout[i,1] / g.layout[i,2]) * 180 / pi
  loc <- g.layout[i,] * 1.2
  if (angle < 0) {
    angle <- 90 + angle
  }
  else {
    angle <- 270 + angle
  }
  text (loc[1], loc[2], labels=label, srt=angle, cex=0.7, col="black")
}

# Add the legend to the plot.
g.legend <- as.vector (env[! duplicated (env[,'category']), 'category'])
g.colors <- as.vector (env[! duplicated (env[,'category']), 'color'])
legend (-1.7, -1.3, legend=g.legend, fill=g.colors, ncol=6, bty="n")

# Finish the script.
q ()
