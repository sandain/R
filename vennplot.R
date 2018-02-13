#!/usr/bin/Rscript

suppressPackageStartupMessages (
  library (venneuler)
)

usage <- "Usage: <Data file> <Output File> <Title>"

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
  n <- length (x)

  # Calculate the number of combinations.
  s <- 0
  for (i in 1:n) s <- s + numcombin (n, i)

  # Initialize array of comparisons.
  a <- rep (0, s)
  a.names <- rep ("",s)

  # Fill the array of comparisons.
  c <- 0
  for (i in 1:n) {
    for (j in 1:numcombin (n, i)) {
      c <- c + 1
      ac <- combn (colnames (x), i)[,j]
      nac <- setdiff (colnames (x), ac)
      if (identical (nac, character (0))) nac = c ()
 
      a.names[c] <- paste (ac, collapse = "&")
      ab <- rep (TRUE, nrow (x))
      for (k in 1:length (ac)) ab <- ab & x[,ac[k]]
      if (length (nac) > 0) for (k in 1:length (nac)) ab <- ab & !x[,nac[k]]
      a[c] <- sum (ab)
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
if (length (args) < 3) stop (usage)

dataFile <- args[1]
outputFile <- args[2]
title <- args[3]

# Verify the data file exists.
if (! file.exists (dataFile)) {
  stop ("Data file not found.")
}

# Load the data file.
t <- read.table (dataFile, header = TRUE)

# Save the plot to a file.
if (grepl (".png$", outputFile)) png (outputFile)
if (grepl (".pdf$", outputFile)) pdf (outputFile)
if (grepl (".svg$", outputFile)) svg (outputFile)

vennplot (t)
title (title)

# Finish the script.
q ()
