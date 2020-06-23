#!/usr/bin/Rscript

suppressPackageStartupMessages (
  library (vegan)
)

usage <- "Usage: <Data File> <Environment File> <Environmental Variable>"

# Load the command line arguments.
args <- commandArgs (trailingOnly = TRUE)

# Verify the command line arguments.
if (length (args) < 3) stop (usage)

dataFile <- args[1]
envFile <- args[2]
envVar <- args[3]

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

# Run anosim all groups.

print ("All")
print (anosim (data, env[,envVar], distance = "bray"))

# Run anosim between groups.
envGroups <- env[! duplicated (env[,envVar]), envVar]
ac <- combn (envGroups, 2)
for (i in 1:ncol (ac)) {
  print (paste (ac[,i], collapse = "&"))
  print (anosim (data[env[,envVar] == ac[1,i] | env[,envVar] == ac[2,i],], env[,envVar], distance = "bray"))
}

# Finish the script.
q ()
