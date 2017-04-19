#!/usr/bin/env Rscript

# This script will perform matching of genotypes where user can control matching parameters.
# It is also possible to run profiling to determine best parameters. Output can be an image.
# Result is controlled through parameters and can be HTML or CSV.
# Verbose logging is done to a file, controlled by a parameter (see --help for more info).
# Read arguments passed to the script

library(optparse)
option_list <- list(
  make_option(c("-i", "--input"), type = "character", default = NULL, 
              help = "A file (path) to the source file of genotypes, e.g. 'dataset.csv' or 
                     './data/dataset.csv' if nested in /data/ relative to the working di-
                     rectory (where script is ran from). File should have a comma as del-
                     imiters and tab separating columns. It should have sample name and 
                     quality index columns, along with loci names."),
  make_option(c("-a", "--alleleMismatch"), type = "integer", default = NA,
              help = "Integer. Number of mismatched alleles (m-hat). Related to matchThreshold 
                 and cutHeight."),
  make_option(c("-m", "--matchThreshold"), type = "double", default = NA, 
              help = "Double. Dissimilarity score which constitutes a match when identifying 
                individuals (s-hat)."),
  make_option(c("-c", "--cutHeight"), type = "double", default = NA,
              help = "Double. Used for dynamic cutting of the tree by amCluster."),
  make_option(c("-o", "--output"), type = "character", default = "default_output.csv",
              help = "Character. The way results will be output. Can be 'html' or 'csv'."),
  make_option(c("-p", "--profile"), type = "logical", metavar = "", default = FALSE,
              action = "store_true",
              help = "If specified, profiling will be performed (but not matching)."),
  make_option(c("-f", "--fig"), type = "character", default = "profile_result.png",
              help = "Name of the resulting figure."),
  make_option(c("-v", "--verbose"), type = "character", default = "debugging.txt",
              help = "Name of file into where verbose debugging text is parsed.")
)

opt <- parse_args(OptionParser(option_list = option_list))

# Load packages
library(allelematch)

# Process input parameters
if (is.null(opt$input)) {
  stop("No input detected. Check path or existance of file.")
} else {
  input <- opt$input
}

# Import data (can be csv, even SQL statement)
# Implementing reading from a csv with decimal delimiter and tab as separator. 
message("Importing dataset...")
cat("Importing dataset.\n", file = opt$verbose) # create a new debugging file, overwrite any previous
xy <- read.table(input, header = TRUE, sep = ";")
cat("[OK] Successfully imported dataset.\n", file = opt$verbose, append = TRUE)

cat("Reshaping data.\n", file = opt$verbose, append = TRUE)
xy <- reshape(xy, timevar = "marker", idvar = "sample", direction = "wide")
names(xy) <- gsub("measured\\.", replacement = "", x = names(xy))
cat("[OK] Done reshaping data.\n", file = opt$verbose, append = TRUE)

# Convert the imported dataset into a proper structure for matching
message("Creating dataset for further processing...")
cat("Preparing dataset for analysis.\n", file = opt$verbose, append = TRUE)
d.xy <- amDataset(xy, missingCode = NA, indexColumn = "sample")
cat("[OK] Dataset ready for analysis.\n", file = opt$verbose, append = TRUE)

# Do profiling. Figure is saved to working directory.
if (opt$profile) {
  message("Profiling... This may take a while.")
  cat("Performing profiling.\n", file = opt$verbose, append = TRUE)
  
  png(filename = opt$fig,  width = 500, height = 500) # output figure size, adapt if needed
  prof <- tryCatch(amUniqueProfile(amDatasetFocal = d.xy, verbose = TRUE),
                   error = function(e) e,
                   warning = function(w) w)
  
  prof.ok <- any(class(prof) %in% c("error", "warning"))
  
  if (prof.ok) {
    plot.new()
    plot.window(xlim = c(-10, 10), ylim = c(-5, 5))
    text(x = 0, y = 1, cex = 2, labels = "Profiling failed with error message:")
    text(x = 0, y = 0, cex = 2, labels = prof$message)
    
    cat("[STOP] Profiling took a turn for the worse.\n", file = opt$verbose, append = TRUE)
  }
  
  dev.off()
  
  if (prof.ok) {
    return(sprintf("[STOP] Profiling failed, please see error log in %s.", opt$verbose))
  }
  
  cat("[OK] Done performing profiling, figure saved.\n", file = opt$verbose, append = TRUE)
  message("Figure saved.")
} else {
  mp <- c("alleleMismatch", "matchThreshold", "cutHeight")
  opt[mp] <- lapply(opt[mp], FUN = function(x) {
    if (is.na(x)) {
      x <- NULL
    }
    x
  })
  
  if (all(is.null(opt$alleleMismatch), is.null(opt$matchThreshold), is.null(opt$cutHeight))) {
    stop("Please specify at least one parameter (alleleMismatch, matchThreshold or cutHeight), see --help.")
  }
  
  cat("Performing matching.\n", file = opt$verbose, append = TRUE)
  message("Performing matching...")
  
  result <- tryCatch(amUnique(amDatasetFocal = d.xy, 
                     alleleMismatch = opt$alleleMismatch, 
                     matchThreshold = opt$matchThreshold,
                     cutHeight = opt$cutHeight),
                     error = function(e) e,
                     warning = function(w) w)
  
  match.ok <- any(class(result) %in% c("error", "warning"))
  
  if (match.ok) {
    
  }
  
  cat("[OK] Matching done.\n", file = opt$verbose, append = TRUE)
  
  # Produce result as specified in output
  # Catch csv, if not, assume html is requested
  message("Writing output...")
  
  if (grepl("\\.csv$", opt$output)) {
    suppressMessages(require(tidyr))
    
    cat("Writing result to CSV.\n", file = opt$verbose, append = TRUE)
    
    amCSV.amUnique(x = result, csvFile = opt$output)
    
    # Reread the data and reflow it into a long format, subset only 
    # relevant columns and resave it
    rein <- read.table(opt$output, header = TRUE, sep = ",")
    rein <- gather(rein, key = markerName, value = value, -uniqueGroup, -rowType, -uniqueIndex,
                   -matchIndex, -nUniqueGroup, -alleleMismatch, -matchThreshold, -cutHeight,
                   -Psib, -score)[, c("uniqueGroup", "rowType", "uniqueIndex", "matchIndex", "Psib",
                                      "markerName", "value")]
    write.table(rein, file = opt$output, quote = FALSE, sep = ",",
                col.names = TRUE, row.names = FALSE)
    
    cat("[OK] Done writing CSV file.\n", file = opt$verbose, append = TRUE)
  } else {
    cat("Writing data to HTML.\n", file = opt$verbose, append = TRUE)
    amHTML.amUnique(x = result, htmlFile = opt$output)
    cat("[OK] Done writing data to HTML.\n", file = opt$verbose, append = TRUE)
  }
}
