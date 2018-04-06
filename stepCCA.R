#!/usr/bin/env Rscript

suppressPackageStartupMessages (
  library (vegan)
)

usage <- "Usage: <Taxa file> <Environment file>"

# Environmental variables.
vars <- c (
  "latitude", "longitude", "boron", "arsenic", "silicon_dioxide", "chlorine",
  "sodium", "potassium", "pH", "sulfide", "magnesium", "calcium", "sulfate",
  "zinc", "calcium_carbonate", "iron", "manganese", "temperature"
)

# P-value cutoff.
pvalue <- 0.05

# The number of iterations.
numitr <- 1000

# Dave Robert's code. Modified to return the results.
# http://ecology.msu.montana.edu/labdsv/R/labs/lab12/lab12.html
step.cca <- function (taxa,start,add,numitr=99) {
    perm.cca <- function (taxa,start,add,numitr) {
        rndeig <- rep (0,numitr)
        if (!is.null (start)) {
            for (i in 1:numitr) {
                tmp <- data.frame (start,sample (add,replace=FALSE))
                tmp2 <- cca (taxa,tmp)
                rndeig[i] <- sum (tmp2$CCA$eig)
            }
        }
        else {
            for (i in 1:numitr) {
                tmp <- data.frame (sample (add,replace=FALSE))
                tmp2 <- cca (taxa,tmp)
                rndeig[i] <- sum (tmp2$CCA$eig)
            }
        }
        return (rndeig)
    }
    res <- data.frame (names (add),rep (NA,ncol (add)),rep (NA,ncol (add)))
    names (res) <- c ('variable','delta_eig','p_val')
    for (i in 1:ncol (add)) {
        if (! any (is.na (add[,i]))) {
            if (!is.null (start)) tmp <- data.frame (start,add[,i])
            else tmp <- add[,i]
            tmp <- cca (taxa,tmp)
            res[i,2] <- sum (tmp$CCA$eig)
        }
        rndeig <- perm.cca (taxa,start,add[,i],numitr)
        if (!is.null (start)) {
            full <- data.frame (start,add[,i])
            base <- cca (taxa,start)
            basval <- sum (base$CCA$eig)
        }
        else {
            full <- add[,i]
            basval <- 0
        }
        res[i,3] <- (sum (rndeig >= res[i,2]) + 1) / (numitr + 1)
        res[i,2] <- res[i,2] - basval
    }
    res <- res[rev (order (res$delta_eig)),]
    cat (paste ("Baseline = ", format (basval, 3), "\n"))
    print (res)
    res
}

# Load the command line arguments.
args <- commandArgs (trailingOnly = TRUE)

# Verify the command line arguments.
if (length (args) < 2) stop (usage)

taxaFile <- args[1]
envFile <- args[2]

# Verify the input files exist.
if (! file.exists (taxaFile)) stop ("Taxa file not found.")
if (! file.exists (envFile)) stop ("Environment file not found.")

# Load the data files.
taxa <- read.table (taxaFile, header = TRUE)
env <- read.table (envFile, header = TRUE)

# Remove empty rows and columns.
taxa <- taxa[rownames (env),, drop = FALSE]
taxa <- taxa[rowSums (taxa) > 0,, drop = FALSE]
taxa <- taxa[, colSums (taxa) > 0, drop = FALSE]

# Individually run CCA on each variable to find the variable that accounts for
# the most constrained variability.
answer <- rep (0, length (vars))
for (i in 1:length (vars)) {
  f <- as.formula (paste ("taxa", vars[i], sep = ' ~ '))
  c <- cca (formula = f, data = env)
  answer[i] <- c$CCA$tot.chi / c$tot.chi * 100
}

# Output the constrained variability of each variable.
names (answer) <- vars
for (i in 1:length (answer)) {
  cat (paste (names (answer[i]), answer[i], sep = '\t'), '\n')
}

# Sort the variables to find the variable that accounts for the most
# constrained variability.
answer <- sort (answer, decreasing = TRUE)
start <- as.data.frame (env[,names (answer[1])])
names (start) <- names (answer[1])

repeat {
  # Create a dataframe containing the variables to test with step.cca.
  add <- data.frame (matrix (ncol = length (vars), nrow = nrow (env)))
  colnames (add) <- vars
  for (i in 1:length (vars)) {
    add[,vars[i]] <- env[,vars[i]]
  }
  # Remove variables in start from add.
  for (i in 1:length (start)) {
    add[,names (start[i])] <- NULL
  }
  # Run Dave Roberts' step.cca code.
  s <- step.cca (taxa, start = start, add = add, numitr = numitr)
  # Keep running until the variable at the top of the list has a p_val greater
  # than the cutoff.
  if (s[1,3] > pvalue) break
  # Add the variable at the top of the list to 'start'.
  var <- as.character (s[1,1])
  start[,var] <- env[,var]
}

cat ("Significant variables:", names (start), '\n', sep = ' ')

# Finish the script.
q ()
