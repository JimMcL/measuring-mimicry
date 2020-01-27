# Script to calculate and save morphometric analysis of outlines. Reads in a
# set of outline images files (unfortunately they must be jpeg files). The image
# files are described in a CSV file called "outlines.csv". Then performs
# geometric morphometric analysis on the images.
#
# Usage: Rscript cache_morphometrics.R {Dorsal|Lateral|Both}
# 
# Output: 
source("morpho_fns.R", local = TRUE)

OUTLINE_DIR <- "../data"
OUTPUT_DIR <- "../output"
GLOBAL_OUTDIR <- "../../output"



# Calculates Mahalanobis distance from every point in the specified coe to the
# centroid of the models in the coe
#
# @param coe A Momocs Coe object, which contains morphometric coefficients for
#   one or more shapes.
# @param retain proportion of variation to use in the distance calculation.
# @param modelType One of the values in the column `coe$fac$mimicType`. Defines
#   the rows that are interpreted as models.
#
# @returns numeric vector of distance from each row in coe to the midpoint of
#   the models in coe.
MorphoMahalanobisDist <- function(coe, retain = .99, modelType = "model") {
  
  # Start with PCA. This eliminates contant dimensions which would stop the
  # mahalanobis distance calculation from working, and also drastically reduces
  # the amount of data, hence speeds up the calculations
  p <- PCA(coe)
  # Discard constant dimensions and convert to a list of columns
  compToRetain <- scree_min(p, prop = retain)
  data <- as.data.frame(p$x[, 1:compToRetain])
  
  # Calculate centre and covariance of all models (i.e. ants)
  models <- data[coe$fac$mimicType == modelType, ]
  centre <- apply(models, 2, mean, na.rm = TRUE)
  cov <- var(models)
  
  # Calculate distance of each point to the centroid of the models
  md <- sqrt(mahalanobis(data, centre, cov))
  
  md
}

.csvName <- function (level, angle = NULL) {
  suf <- if (is.null(angle))
    ""
  else
    paste0("-", tolower(angle))
  file.path(OUTPUT_DIR, sprintf("Geomorpho-accuracy-%s%s.csv", level, suf))
}


RunMorphoAnalysis <- function(photos, angles = c("Dorsal", "Lateral"), force = FALSE, subsample = NULL) {

  # Report missing outlines
  missing <- !file.exists(photos$file)
  if (sum(missing) > 0) {
    message(sprintf("There are %d missing outline files.\n%s\n", sum(missing), paste(photos$file[missing], collapse = "\n")))
  }
  
  exist <- !missing
  photos <- photos[exist, ]

  # x <- cbind(coe$fac, accuracy)
  # if (!dir.exists(OUTPUT_DIR))
  #   dir.create(OUTPUT_DIR)
  # write.csv(x, .csvName(level, angle), row.names = FALSE)
  
  calcAccuracy <- function(coe) {
    # Calculate distance from each point to centroid of ants
    md <- MorphoMahalanobisDist(coe)
    # Invert distance to get accuracy (i.e. points further away from the ants
    # are less accurate), and scale so the result is a number from 1 (perfect
    # accuracy) to 0 (worst accuracy). Note that 0 means the worst accuracy
    # within this dataset
    1 - (md / max(md))
  }

  # Given species accuracy scores for dorsal and lateral angles, averages them.
  averageAnglesToSpecies <- function(species) {
    
    # Calculate average accuracy per species
    a <- aggregate(species$accuracy, list(species$species), mean)
    names(a) <- c("species", "accuracy")
    # Record angles that were used to derive accuracy
    a$derivedFrom <- sapply(a$species, function(s) paste(c(species[species$species == s, ][["angle"]]), collapse = ","))
    
    # Merge in all the other columns (excluding accuracy, angle and species which we already have)
    cols <- names(species)
    cols <- cols[!cols %in% c("angle", "accuracy", "species")]
    cbind(a, species[match(a$species, species$species), cols])
  }
  
  # Calculate morphometric accuracy separately for each angle
  getMorphoForAngle <- function(angle) {
    # Do morphometric analysis
    photos <- photos[photos$angle == angle, ]
    if (!is.null(subsample))
      photos <- photos[sample(nrow(photos), subsample), ]
    # Perform morphological analysis on these photos. This step can be very
    # slow, so the results will be cached, and the cached results will be
    # returned if the same set of photos is processed
    m <- GetMorphoForPhotos(photos, force = force)
    # Add individual and species accuracies
    m$individual$Coe$fac$accuracy <- calcAccuracy(m$individual$Coe)
    m$species$Coe$fac$accuracy <- calcAccuracy(m$species$Coe)
    
    m
  }
  morpho <- lapply(angles, getMorphoForAngle)
  
  # Combine angles for individuals and species
  individual <- do.call(rbind, lapply(morpho, function(m) m[["individual"]]$Coe$fac))
  species <- do.call(rbind, lapply(morpho, function(m) m[["species"]]$Coe$fac))
  
  # Export the output CSV files
  if (!dir.exists(OUTPUT_DIR))
    dir.create(OUTPUT_DIR)
  write.csv(individual, .csvName("individual", "angle"), row.names = FALSE)
  write.csv(species, .csvName("species", "angle"), row.names = FALSE)
  
  # Average species/angles to species, and write to CSV
  sp <- averageAnglesToSpecies(species)
  write.csv(sp, .csvName("species"), row.names = FALSE)
}

CopyCsvs <- function(fromDir = OUTPUT_DIR, toDir = GLOBAL_OUTDIR) {
  # Now copy all CSV files to the global output directory
  csvs <- list.files(fromDir, ".*\\.csv$", full.names = TRUE)
  copied <- file.copy(csvs, toDir, overwrite = TRUE, copy.mode = TRUE, copy.date = TRUE)
  if (any(copied)) {
    cat(sprintf("Unable to copy file(s): %s\n", csvs[!copied]))
  }
}

ReportStats <- function(outlines) {
  cat(sprintf("%d images: %d dorsal and %d lateral\n", 
              nrow(outlines), sum(outlines$angle == "Dorsal"), sum(outlines$angle == "Lateral")))

  sp <- read.csv(.csvName("species"), stringsAsFactors = FALSE)
  cat(sprintf("%d species: %d mimics, %d models and %d non-mimics\n",
              length(unique(sp$species)), 
              sum(sp$mimicType == "mimic"),
              sum(sp$mimicType == "model"),
              sum(sp$mimicType == "non-mimic")
  ))
}

###################################################################

# This script can be run from Rstudio or the command line
# Normal, morphometric analyse is cached if the set of photos in unchanged.
# Set force to TRUE (or specify --force on the command line) to ignore the cache.
force <- FALSE
if (length(commandArgs(TRUE)) > 0) {
  if (commandArgs(TRUE)[1] != "--force")
    stop("Usage : [--force]")
  force <- TRUE
}

startTime <- proc.time()

# Read the list of outlines
outlines <- read.csv(file.path(OUTLINE_DIR, "outlines.csv"), stringsAsFactors = FALSE)
# Calculate geometric morphometric accuracy scores for dorsal and lateral outlines
RunMorphoAnalysis(outlines, force = force)

# Now copy all CSV files to the global output directory
CopyCsvs()

ShowTime("Total processing time", startTime)
ReportStats(outlines)