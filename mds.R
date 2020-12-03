#!/usr/bin/env Rscript

suppressPackageStartupMessages (
  library (vegan)
)

usage <- "Usage: <Data File> <Environment File> <Environmental Variable> <Output File> <Title>"

colors <- c ("red", "blue", "green", "purple", "orange", "pink", "magenta", "cyan", "gold")

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
data <- data[, intersect (rownames (env), colnames (data)), drop = FALSE]
data <- data[rowSums (data) > 0,,drop = FALSE]
data <- data[, colSums (data) > 0,drop = FALSE]
env <- env[colnames (data),, drop = FALSE]

# The main data file comes in transposed from what we need.
data <- t (data)

# Figure out the groups in the environment.
envGroups <- env[! duplicated (env[,envVar]), envVar]
envGroups <- envGroups[! is.na (envGroups)]

# Save the plot to a file.
if (grepl (".png$", outputFile)) png (outputFile)
if (grepl (".pdf$", outputFile)) pdf (outputFile)
if (grepl (".svg$", outputFile)) svg (outputFile)
if (grepl (".tif$", outputFile)) tiff (outputFile, compression="lzw")

# Run MDS on the data.
data.mds <- metaMDS (data)

# Create an empty plot sized to fit the MDS output.
data.plot <- plot (data.mds, type="n", main=title)

# Figure out environmental group colors.
col <- rep ('#000000', length (envGroups))
bg <- rep ('#000000', length (envGroups))
pch <- rep (21, length (envGroups))
for (i in 1: length (envGroups)) {
  if (! is.null (env[env[,envVar] == envGroups[i],'col'][1])) {
    col[i] <- as.vector (env[env[,envVar] == envGroups[i],'col'][1])
  }
  if (! is.null (env[env[,envVar] == envGroups[i],'bg'][1])) {
    bg[i] <- as.vector (env[env[,envVar] == envGroups[i],'bg'][1])
  } else {
    ci <- i %% length (colors)
    if (ci == 0) ci <- length (colors)
    bg[i] <- colors[ci]
  }
  if (! is.null (env[env[,envVar] == envGroups[i],'pch'][1])) {
    pch[i] <- as.vector (env[env[,envVar] == envGroups[i],'pch'][1])
  }
}

# Draw ellipses around the mean of each environmental variable.
for (i in 1: length (envGroups)) {
  # Don't draw an ellipse if there are less than three points.
  if (sum (env[,envVar] == envGroups[i]) < 3) next
  ordiellipse (data.plot, env[,envVar], kind="se", conf=0.99, col=bg[i], show.groups = envGroups[i], alpha=75, draw="polygon")
}

# Draw bounding boxes around each environmental variable.
#ordihull (data.plot, env[,envVar])

# Draw sites from MDS output colored by environmental variable.
for (i in 1: length (envGroups)) {
  x <- data.plot$sites[env[,envVar] == envGroups[i],1]
  y <- data.plot$sites[env[,envVar] == envGroups[i],2]
  points (x=x, y=y, col=col[i], bg=bg[i], pch=pch[i])
}

# Add labels to the sites.
text (data.plot$sites, labels=rownames (data.plot$sites), pos=4)

# Add a legend.
legend ("topleft", legend=envGroups, col='#000000', pt.cex=1.5, pt.bg=bg, pch=pch)

# Finish the script.
q ()
