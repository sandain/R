#! /usr/bin/env Rscript

suppressPackageStartupMessages (library (vegan))
suppressPackageStartupMessages (library (ggplot2))
suppressPackageStartupMessages (library (gridExtra))
suppressPackageStartupMessages (library (egg))

usage <- "Usage: <Data File> <Environment File> <Environmental Variable> <Output File> <Title> <Minimum Size>"

# Load the command line arguments.
args <- commandArgs (trailingOnly = TRUE)

# Verify the command line arguments.
if (length (args) < 6) stop (usage)

dataFile <- args[1]
envFile <- args[2]
envVar <- args[3]
outputFile <- args[4]
title <- args[5]
minimum <- as.numeric (args[6])

# Verify the data files exist.
if (! file.exists (dataFile)) stop ("Data file not found.")
if (! file.exists (envFile)) stop ("Environment file not found.")

# Load the data files.
data <- read.table (dataFile, header = TRUE)
env <- read.table (envFile, header = TRUE)

# Remove rows and columns with no data.
data <- data[, intersect (rownames (env), colnames (data)), drop = FALSE]
data <- data[rowSums (data) > 0,, drop = FALSE]

# Remove columns with sums less than minimum.
cols <- colnames (data)[colSums (data) < minimum]
if (length (cols) > 0) {
  message (cat ("Insufficient data to include:\n", paste (cols, '(', colSums (data[,cols,drop=FALSE]), ')\n')))
  data <- data[, colSums (data) >= minimum, drop = FALSE]
}

# Grab the environmental information.
env <- env[colnames (data),, drop = FALSE]

# The main data file comes in transposed from what we need.
data <- t (data)

# Rarefy data.
data <- rrarefy (data, minimum)

# Figure out the groups in the environment.
envGroups <- env[! duplicated (env[,envVar]), envVar]
envGroups <- envGroups[! is.na (envGroups)]

# Calculate diversity indices.
d <- data.frame (
  row.names = rownames (data),
  category = env[,envVar],
  chao1 = double (nrow (data)),
  shannon = double (nrow (data)),
  simpson = double (nrow (data))
)
for (i in 1: nrow (data)) {
  d$chao1[i] <- estimateR (data[i,])[2]
  d$shannon[i] <- diversity (data[i,], index = "shannon")
  d$simpson[i] <- diversity (data[i,], index = "simpson")
}

# Calculate the average of the diversity indicies.
a <- data.frame (
  row.names = envGroups,
  category = envGroups,
  chao1 = double (length (envGroups)),
  shannon = double (length (envGroups)),
  simpson = double (length (envGroups))
)
for (i in 1: length (envGroups)) {
  a$chao1[i] <- mean(d[d[,"category"] == envGroups[i], "chao1"])
  a$shannon[i] <- mean(d[d[,"category"] == envGroups[i], "shannon"])
  a$simpson[i] <- mean(d[d[,"category"] == envGroups[i], "simpson"])
}

# Save the plot to a file.
if (grepl (".png$", outputFile)) png (outputFile, width=216, height=504)
if (grepl (".pdf$", outputFile)) pdf (outputFile, width=216, height=504)
if (grepl (".svg$", outputFile)) svg (outputFile, width=3, height=7)
if (grepl (".tif$", outputFile)) tiff (outputFile, compression="lzw", width=216, height=504)

p1 <- ggplot () +
  geom_col (data=a, aes (x=category, y=chao1)) +
  geom_dotplot (data=d, aes (x=category, y=chao1), binaxis='y', stackdir='center', stackratio=1.5, dotsize=.5) +
  theme_grey (base_size = 15) +
  theme (axis.title.x=element_blank (),axis.text.x = element_blank ())
p2 <- ggplot () +
  geom_col (data=a, aes (x=category, y=shannon)) +
  geom_dotplot (data=d, aes (x=category, y=shannon), binaxis='y', stackdir='center', stackratio=1.5, dotsize=.5) +
  theme_grey (base_size = 15) +
  theme (axis.title.x=element_blank (),axis.text.x = element_blank ())
p3 <- ggplot () +
  geom_col (data=a, aes (x=category, y=simpson)) +
  geom_dotplot (data=d, aes (x=category, y=simpson), binaxis='y', stackdir='center', stackratio=1.5, dotsize=.5) +
  theme_grey (base_size = 15) +
  theme (axis.title.x=element_blank (),axis.text.x = element_text(angle=90, hjust=1))

ggarrange (p1, p2, p3, nrow = 3, top = title)

# Finish the script.
q ()
