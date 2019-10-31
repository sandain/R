#!/usr/bin/Rscript

suppressPackageStartupMessages (
  library (vegan)
)

usage <- "Usage: <Data File> <Environment File> <Environmental Variable> <Output File>"

# Load the command line arguments.
args <- commandArgs (trailingOnly = TRUE)

# Verify the command line arguments.
if (length (args) < 4) stop (usage)

dataFile <- args[1]
envFile <- args[2]
envVar <- args[3]
outputFile <- args[4]

# Verify the data files exist.
if (! file.exists (dataFile)) stop ("Data file not found.")
if (! file.exists (envFile)) stop ("Environment file not found.")

# Load the data files.
data <- read.table (dataFile, header = TRUE)
env <- read.table (envFile, header = TRUE)

# Remove rows and columns with no data.
data <- data[,intersect (rownames(env), colnames(data)),drop = FALSE]
data <- data[rowSums (data) > 0,,drop = FALSE]
data <- data[, colSums (data) > 0,drop = FALSE]
env <- na.omit (env[colnames (data),])

# The main data file comes in transposed from what we need.
data <- t (data)

# Run anosim.
data.ano <- anosim (data, env[,envVar], distance = "bray")

# Output the anosim summary.
summary (data.ano)

# Save the plot to a file.
if (grepl (".png$", outputFile)) png (outputFile)
if (grepl (".pdf$", outputFile)) pdf (outputFile)
if (grepl (".svg$", outputFile)) svg (outputFile)
if (grepl (".tif$", outputFile)) tiff (outputFile, compression="lzw")

# Draw the anosim plot.
plot (data.ano)

# Finish the script.
q ()
