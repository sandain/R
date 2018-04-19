#!/usr/bin/env Rscript

suppressPackageStartupMessages (
  library (venneuler)
)

usage <- "Usage: <Data file>  <Environment File> <Environmental Variable> <Output File> <Title>"

#-----------------------------------------------------------------------------
# Calculate the number of combinations for a given number of items.
#
# @param n The number of items.
# @param i The number of items to combine.
#-----------------------------------------------------------------------------
numcombin <- function (n, i) {
  factorial (n) / (factorial (i) * factorial (n - i))
}

#-----------------------------------------------------------------------------
# Plot a Venn diagram.
#
# @param x The data to plot.
#-----------------------------------------------------------------------------
vennplot <- function (x) {
  # Get the length of the data.
  n <- ncol (x)

  # Calculate the number of combinations.
  s <- 0
  for (i in 1:n) s <- s + numcombin (n, i)

  # Initialize array of comparisons.
  a <- rep (0, s)
  a.names <- rep ("",s)

  # Fill the array of comparisons.
  h <- 0
  for (i in 1:n) {
    for (j in 1:numcombin (n, i)) {
      h <- h + 1
      # Figure out which columns need to match and not match.
      ac <- combn (colnames (x), i)[,j]
      nac <- setdiff (colnames (x), ac)
      # Figure out the rows that meet the match/not match requirements.
      ab <- rep (TRUE, nrow (x))
      for (k in 1:length (ac)) ab <- ab & x[,ac[k]]
      if (length (nac) > 0) for (k in 1:length (nac)) ab <- ab & !x[,nac[k]]
      # Sum the number of rows that meet the requirements.
      a[h] <- sum (ab)
      a.names[h] <- paste (ac, collapse = "&")
    }
  }
  # Assign the names of each comparison to the array.
  names (a) <- a.names
  # Plot the Venn diagram.
  va <- venneuler (a)
  plot (va)
  return (list (va=va, a=a))
}

# Load the command line arguments.
args <- commandArgs (trailingOnly = TRUE)

# Verify the command line arguments.
if (length (args) < 5) stop (usage)

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

# Remove rows and columns with no data.
data <- data[rowSums (data) > 0,,drop = FALSE]
data <- data[, colSums (data) > 0,drop = FALSE]

envGroups <- env[! duplicated (env[,envVar]), envVar]
data.env <- matrix (nrow = nrow (data), ncol = length (envGroups))
rownames (data.env) <- rownames (data)
colnames (data.env) <- envGroups
for (i in 1: length (envGroups)) {
  d <- data[,env[,envVar] == envGroups[i]]
  if (is.null(ncol (d))) {
    data.env[,i] <- d
  }
  else {
    data.env[,i] <- rowSums (d)
  }
}

# Save the plot to a file.
if (grepl (".png$", outputFile)) png (outputFile)
if (grepl (".pdf$", outputFile)) pdf (outputFile)
if (grepl (".svg$", outputFile)) svg (outputFile)
if (grepl (".tif$", outputFile)) tiff (outputFile, compression="lzw")

vennplot (data.env)
title (title)

# Finish the script.
q ()
