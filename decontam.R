#!/usr/bin/env Rscript

suppressPackageStartupMessages (
  library (decontam)
)

usage <- "Usage: <Data File> <Environment File> <Control Variable> <DNA Quantitation Variable> <Output File> <Contaminants File>"

# Load the command line arguments.
args <- commandArgs (trailingOnly = TRUE)

# Verify the command line arguments.
if (length (args) != 6) stop (usage)

dataFile <- args[1]
envFile <- args[2]
ctrlVar <- args[3]
quantVar <- args[4]
outputFile <- args[5]
contaminantsFile <- args[6]

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

# Swap zero values with a real small value instead.
env[env[,quantVar] == 0, quantVar] <- 0.000001

# Use the decontam combined method to detect contaminants.
contaminants <- isContaminant (t (data), conc=env[,quantVar], neg=env[,ctrlVar], method="combined")

# Output the output file and the contaminants file.
write.table (data[contaminants$contaminant == FALSE,], file=outputFile, sep="\t", eol = "\n", col.names=NA)
write.table (data[contaminants$contaminant == TRUE,], file=contaminantsFile, sep="\t", eol = "\n", col.names=NA)

# Finish the script.
q ()
