#!/usr/bin/Rscript

usage <- "Usage: <Data file>"

# Load the command line arguments.
args <- commandArgs (trailingOnly = TRUE)

# Verify the command line arguments.
if (length (args) < 1) stop (usage)

dataFile <- args[1]

# Verify the data file exists.
if (! file.exists (dataFile)) {
  stop ("Data file not found.")
}

# Load the data file.
data <- read.table (dataFile, header = TRUE)

ac <- combn (colnames (data), 2)

for (i in 1:ncol (ac)) {
  print (paste (ac[,i], collapse = "&"))
  print (wilcox.test (data[,ac[1,i]], data[,ac[2,i]]))
}

# Finish the script.
q ()
