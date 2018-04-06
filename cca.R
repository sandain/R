#!/usr/bin/env Rscript

suppressPackageStartupMessages (
  library (vegan)
)

usage <- "Usage: <Taxa file> <Environment file> <Taxa Categories> <Output File> <Legend Location> <Formula>"

# Custom version of Dave Roberts' ordtest {labdsv} code.
# Instead of extracting ordpoints from a provided
# ordination, let the user provide the ordpoints directly.
my.ordtest <- function (
  ordpoints, var, dim = c (1:ncol (ordpoints)),
  index = 'euclidean', nitr = 1000
) {
  tdist <- 0
  observed <- 0
  reps <- rep (0, nitr - 1)
  var <- factor (var)
  for (i in levels (var)) {
    mask <- var == i
    tdist <- tdist + sum (
      dist (ordpoints[mask,dim], index)
    )
  }
  observed <- tdist
  for (i in 1:(nitr - 1)) {
    tdist <- 0
    var <- sample (var, length (var), replace = FALSE)
    for (j in levels (var)) {
      mask <- var == j
      s <- sum (dist (ordpoints[mask,dim], index))
      tdist <- tdist + s
    }
    reps[i] <- tdist
  }
  reps
  out <- list (
    obs = observed,
    p = (sum (reps <= observed) + 1) / nitr,
    reps = reps
  )
  out
}

# Custom version of Dave Roberts' alt.ordtest code.
# Instead of extracting ordpoints from a provided
# ordination, let the user provide the ordpoints directly.
alt.ordtest <- function (
  ordpoints, var, index = "euclidean", nitr = 1000
) {
  if (sum (var) <= 1) {
    out <- list (obs = 0, p = 1, reps = rep (1, nitr - 1))
  }
  else {
    tdist <- 0
    observed <- 0
    reps <- rep (0, nitr - 1)
    tdist <- sum (dist (ordpoints[var,], index))
    observed <- tdist
    for (i in 1:(nitr - 1)) {
      tdist <- 0
      var <- sample (var, length (var), replace = FALSE)
      tdist <- sum (dist (ordpoints[var,], index))
      reps[i] <- tdist
    }
    out <- list (
      obs = observed,
      p = (sum (reps <= observed) + 1) / nitr,
      reps = reps
    )
  }
  out
}

# This was a private method used by the plot.cca {vegan} function that was
# copy-pasted for use here.
#
# Scaling of arrows to 'fill' a plot with vectors centred at 'at'.
# Plot dims from 'par(\"usr\")' and arrow heads are in 'x'.
ordiArrowMul <- function (x, at = c (0,0), fill = 0.75) {
  u <- par ('usr')
  u <- u - rep (at, each=2)
  r <- c (range (x[,1], na.rm = TRUE), range (x[,2], na.rm = TRUE))
  ## 'rev' takes care of reversed axes like xlim(1,-1)
  rev <- sign (diff (u))[-2]
  if (rev[1] < 0)
    u[1:2] <- u[2:1]
  if (rev[2] < 0)
    u[3:4] <- u[4:3]
  u <- u / r
  u <- u[is.finite (u) & u > 0]
  fill * min (u)
}

# This was a private method used by the plot.cca {vegan} function that was
# copy-pasted for use here.
#
# Location of the text at the point of the arrow. 'vect' are the
# coordinates of the arrow heads, and 'labels' are the text used to
# label these heads, '...' passes arguments (such as 'cex') to
# strwidth() and strheight().
ordiArrowTextXY <- function (vect, labels, ...) {
  w <- strwidth (labels, ...)
  h <- strheight (labels, ...)
  ## slope of arrows
  b <- vect[,2] / vect[,1]
  ## offset based on string dimensions
  off <- cbind (sign (vect[,1]) * (w / 2 + h / 4), 0.75 * h * sign (vect[,2]))
  ## move the centre of the string to the continuation of the arrow
  for(i in 1:nrow (vect)) {
    move <- off[i,2] / b[i]
    ## arrow points to the top/bottom of the text box
    if (is.finite (move) && abs (move) <= abs (off[i,1]))
      off[i,1] <- move
    else {
      ## arrow points to a side of the text box
      move <- b[i] * off[i, 1]
      off[i,2] <- move
    }
  }
  off + vect[,c(1,2)]
}

# Draw arrows on the plot.  Based on code found in the
# plot.cca {vegan} function.
drawArrows <- function (
  biplot, col = 'blue', cex = 1.0, fill = 0.60
) {
  draw.mul <- ordiArrowMul (biplot, fill = fill)
  arrows (
    0, 0,
    draw.mul * biplot[,1], draw.mul * biplot[,2],
    length = 0.05, col = col
  )
  draw.biplabs <- ordiArrowTextXY (
    draw.mul * biplot,
    rownames (biplot),
    cex = cex
  )
  text (
    draw.biplabs,
    rownames (biplot),
    col = col, cex = cex
  )
}

# Draw the legend on the plot.
drawLegend <- function (
  x, y, species, categories, cex = 1.0, ncol = 1
) {
  # Grab a list of the unique community categories for the
  # legend.
  draw.leg <- categories[! duplicated (categories$category),]
  # The cex column and rownames are not used for the legend.
  draw.leg$cex <- NULL
  rownames (draw.leg) <- NULL
  # Build the legend.
  draw.leg$text <- rep ("", times = nrow (draw.leg))
  for (i in 1:nrow (draw.leg)) {
    # Calculate the number of HFS that belong to category i.
    n <- sum (categories$category == draw.leg$category[i])
    pvalue <- ""
    if (n > 1) {
      # Calculate the p-value using Dave Roberts' alt.ortest for category i.
      dev <- categories[rownames (species), 'category'] ==
        draw.leg$category[i]
      dev[is.na (dev)] <- FALSE
      aot <- alt.ordtest (species, dev)
      pvalue <- sprintf ("%.3f", aot$p)
    }
    # Create the legend for category i.
    draw.leg$text[i] <- sprintf (
      "%s (%d): %s", draw.leg$category[i], n, pvalue
    )
  }
  # Sort the legend.
  draw.leg <- draw.leg[with (draw.leg, order (category)),]
  # Draw the legend.
  legend (
    x, y,
    legend = draw.leg$text,
    pch = as.vector (draw.leg$pch),
    col = as.vector (draw.leg$col),
    pt.bg = as.vector (draw.leg$bg),
    cex = cex,
    ncol = ncol
  )
}

# A custom plotting function to display the species points
# in the ordination space.  This function only works with
# cca objects from {vegan} and mfso objects from {fso}.
my.species.plot <- function (
  x,
  taxa,
  categories = data.frame (
    row.names = rownames (taxa),
    category = rep (NA, nrow (taxa)),
    pch = rep (1, nrow (taxa)),
    col = rep ('black', nrow (taxa)),
    bg = rep (NA, nrow (taxa)),
    cex = rep (1.0, nrow (taxa)),
    label = rep (NA, nrow (taxa))
  ),
  lg.x = 'bottomleft',
  lg.y = 'NULL',
  scaling = "species",
  cex = 1.0,
  ...
) {
  if (inherits (x, c ("cca"))) {
    draw.scores <- scores (x)
    draw.species <- draw.scores$species
    draw.xlab <- colnames (draw.species)[1]
    draw.ylab <- colnames (draw.species)[2]
  } else if (inherits (x, c ("mfso"))) {
    draw.species <- wascores (x$mu, taxa)
    draw.xlab <- names (x$data)[1]
    draw.ylab <- names (x$data)[2]
  } else {
    stop (cat (
      "my.species.plot is only defined for",
      "cca and mfso objects",
      sep = " "
    ))
  }
  # Draw small gray dots for all of the species points.
  plot (
    draw.species,
    xlim = range (apply (draw.species, 1, range)),
    ylim = range (apply (draw.species, 2, range)),
    xlab = draw.xlab,
    ylab = draw.ylab,
    pch = 3,
    col = 'gray',
    cex = 0.2,
    ...
  )
  # Draw the legend.
  if (! lg.x == "none" && ! all (is.na (categories$category))) {
    draw.legend <- drawLegend (
      lg.x,
      lg.y,
      draw.species,
      categories,
      cex = 0.8
    )
  }
  # Draw the arrows for the constraining variables on plot.
  if (inherits (x, c ("cca"))) {
    if (is.null (draw.scores$biplot)) {
      draw.scores$biplot <- scores(x, c (1,2), "bp", scaling)
    }
    drawArrows (draw.scores$biplot, col = 'blue', cex = cex)
  }
  # Draw all points defined in the categories matrix.
  for (i in 1:nrow (categories)) {
    draw.cat <- categories[i,]
    points (
      draw.species[rownames (draw.cat),1],
      draw.species[rownames (draw.cat),2],
      pch = as.vector (draw.cat$pch),
      col = as.vector (draw.cat$col),
      bg = as.vector (draw.cat$bg),
      cex = as.vector (draw.cat$cex)
    )
  }
  # Add labels defined in the categories matrix.
  for (i in 1:nrow (categories)) {
    draw.cat <- categories[i,]
    text (
      draw.species[rownames (draw.cat),1],
      draw.species[rownames (draw.cat),2],
      draw.cat$label,
      col = as.vector (draw.cat$col),
      cex = 0.75,
      pos = 4
    )
  }
}

# Load the command line arguments.
args <- commandArgs (trailingOnly = TRUE)

# Verify the command line arguments.
if (length (args) < 6) stop (usage)

taxaFile <- args[1]
envFile <- args[2]
taxaCategoriesFile <- args[3]
outputFile <- args[4]
legendLocation <- args[5]
formula <- args[6]

# Verify the input files exist.
if (! file.exists (taxaFile)) {
  stop ("Taxa file not found.")
}
if (! file.exists (envFile)) {
  stop ("Environment file not found.")
}
if (! file.exists (taxaCategoriesFile)) {
  stop ("Taxa Categories file not found.")
}

# Load the data files.
taxa <- read.table (taxaFile, header = TRUE)
env <- read.table (envFile, header = TRUE)
taxa_cat <- read.table (taxaCategoriesFile, header = TRUE)

# Verify the formula
formula <- as.formula (formula)
formula.vars <- all.vars (formula)
if (formula.vars[1] != "taxa") stop ("Invalid formula, taxa not defined.")
for (i in 2:length (formula.vars)) {
  if (! formula.vars[i] %in% colnames (env)) {
    stop ("Invalid formula, environmental data not defined!\n")
  }
  # Remove rows with NA for this variable.
  env <- env[!is.na (env[,formula.vars[i]]),]
}

taxa <- taxa[intersect (rownames(env), rownames(taxa)),,drop = FALSE]
taxa <- taxa[rowSums (taxa) > 0,,drop = FALSE]
taxa <- taxa[, colSums (taxa) > 0,drop = FALSE]

taxa_cat <- taxa_cat[colnames (taxa),, drop = FALSE]
taxa_cat <- taxa_cat[!is.na (taxa_cat[, 'category']),]

if (nrow(taxa_cat) <= 1) stop ("Not enough taxa in category.")

# Remove empty rows and columns.
env <- env[rownames (taxa),,drop = FALSE]

# Run CCA.
taxa.cca <- cca (formula = formula, data = env)

# Run my.ordtest on the CCA result.
taxa.cca.ot <- my.ordtest (
  taxa.cca$CCA$v,
  taxa_cat$category
)

# Output the CCA results.
message (cat ("CCA:"))
print (taxa.cca)
#alias (taxa.cca, names = TRUE)
message (cat ("ordtest pvalue:", taxa.cca.ot$p, sep = ' '))

# Save the species plot to a file.
if (grepl (".png$", outputFile)) png (outputFile)
if (grepl (".pdf$", outputFile)) pdf (outputFile)
if (grepl (".svg$", outputFile)) svg (outputFile)

# Draw the CCA plot.
my.species.plot (
  taxa.cca,
  taxa,
  taxa_cat,
  legendLocation,
  scaling = "symmetric"
)

# Finish the script.
q ()
