#!/usr/bin/env Rscript
# Read arguments passed to the script

library(optparse)
option_list <- list(
  # make_option(c("-h", "--help"), help = "You are here.", default = FALSE),
  make_option(c("-i", "--input"), type = "character", 
              help = "A file (path) to the source file of genotypes, e.g. 'dataset.csv' or 
                     './data/dataset.csv' if nested in /data/ relative to the working di-
                     rectory (where script is ran from). File should have a comma as del-
                     imiters and tab separating columns. It should have sample name and 
                     quality index columns, along with loci names."),
  make_option(c("-a", "--alleleMismatch"), default = "integer", help = 
                "Integer. Number of mismatched alleles (m-hat). Related to matchThreshold 
                 and cutHeight."),
  make_option(c("-m", "--matchThreshold"), type = "double", help = 
                "Double. Dissimilarity score which constitutes a match when identifying 
                individuals (s-hat)."),
  make_option(c("-c", "--cutHeight"), type = "double", 
              help = "Double. Used for dynamic cutting of the tree by amCluster."),
  make_option(c("-o", "--output"), type = "character", default = "default_output.csv",
              help = "Character. The way results will be output. Can be 'html' or 'csv'."),
  make_option(c("-p", "--profile"), type = "logical",
              help = "Logical. Can be TRUE or FALSE. Profiling is done only if requested. 
                      Results in a figures stored locally.", action = "store_true"),
  make_option(c("-f", "--fig"), type = "character", default = "profile_result.png",
              help = "Name of the resulting figure.")
)

opt <- parse_args(OptionParser(option_list=option_list))

# Load packages
library(allelematch)

# Process input parameters
if (any(grepl("--input", x = args))) {
  input <- args[grepl("--input", x = args)]
  input <- sub("--input=", "", x = input)
} else {
  stop("No input detected. Check path or existance of file.")
}

if (any(grepl("--alleleMismatch", x = args))) {
  cmd.amm <- args[grepl("--alleleMismatch", x = args)]
  cmd.amm <- sub("--alleleMismatch=", "", x = cmd.amm)
  cmd.amm <- as.numeric(cmd.amm)
} else {
  cmd.amm <- NULL
}

if (any(grepl("--matchThreshold", x = args))) {
  cmd.mth <- args[grepl("--matchThreshold", x = args)]
  cmd.mth <- sub("--matchThreshold=", "", x = cmd.mth)
  cmd.mth <- as.numeric(cmd.mth)
} else {
  cmd.mth <- NULL
}

if (any(grepl("--cutHeight", x = args))) {
  cmd.cht <- args[grepl("--cutHeight", x = args)]
  cmd.cht <- sub("--cutHeight=", "", x = cmd.cht)
  cmd.cht <- as.numeric(cmd.cht)
} else {
  cmd.cht <- NULL
}

if (any(grepl("--output", x = args))) {
  out <- args[grepl("--output", x = args)]
  out <- sub("--output=", "", x = out)
} else {
  message("No output specified, defaulting to 'default_output.csv'.")
  out <- "default_output.csv"
}

if (any(grepl("--profile", x = args))) {
  do.profile <- args[grepl("--profile", x = args)]
  do.profile <- sub("--profile=", "", x = do.profile)
  do.profile <- as.logical(do.profile)
} else {
  message("Profiling will not be performed because not not called explicitly.")
  do.profile <- FALSE
}

if (all(unlist(lapply(sapply(ls(pattern = "cmd."), FUN = get), FUN = is.null)), !do.profile)) {
  stop("Please specify at least one parameter (alleleMismatch, matchThreshold or cutHeight), see --help.")
}

if (any(grepl("--fig", x = args))) {
  fig.name <- args[grepl("--fig", x = args)]
  fig.name <- sub("--fig=", "", x = fig.name)
} else {
  message("No figure name specified, using default name 'profile_result.png'.")
  fig.name <- "profile_result.png"
}

# Import data (can be csv, even SQL statement)
# Implementing reading from a csv with decimal delimiter and tab as separator. 
message("Importing dataset...")
xy <- read.table(input, header = TRUE, dec = ",", sep = "\t")

# Convert the imported dataset into a proper structure for matching
message("Creating dataset for further processing...")
d.xy <- amDataset(xy, missingCode = NA, indexColumn = "Samp", metaDataColumn = "QualityIndex") # TODO: add metadata columns

# Do profiling. Figure is saved to working directory.
if (do.profile) {
  message("Profiling... This may take a while.")
  png(fig.name,  width = 500, height = 500)
  prof <- amUniqueProfile(amDatasetFocal = d.xy, verbose = TRUE)
  dev.off()
} else {
  result <- amUnique(amDatasetFocal = d.xy, 
                     alleleMismatch = cmd.amm, 
                     matchThreshold = cmd.mth,
                     cutHeight = cmd.cht
  )
  
  # Produce result as specified in output
  # Catch csv, if not, assume html is requested
  if (grepl("\\.csv", out)) {
    amCSV.amUnique(x = result, csvFile = out)
  } else {
    amHTML.amUnique(x = result, htmlFile = out)
  }
}
