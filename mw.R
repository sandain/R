#!/usr/bin/env Rscript

usage <- "Usage: <Data file> <Environment File> <Environmental Variable>"

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

envGroups <- env[! duplicated (env[,envVar]), envVar]

ac <- combn (envGroups, 2)

for (i in 1:ncol (ac)) {
  print (paste (ac[,i], collapse = "&"))
  print (wilcox.test (data.env[,ac[1,i]], data.env[,ac[2,i]]))
}

# Finish the script.
q ()
