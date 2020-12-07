# Script to calculate and save morphometric analysis of outlines. Reads in a
# set of outline images files (unfortunately they must be jpeg files). The image
# files are described in a CSV file called "outlines.csv". Then performs
# geometric morphometric analysis on the images.
#
# Usage: Rscript Analysis.R [--force]
# 
# Output: CSV files in the ../output directory

#devtools::install_github("JimMcL/JUtils")
library(JUtils)

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
MorphoMahalanobisDist <- function(coe, retain, modelType = "model") {
  
  # Start with PCA. This eliminates constant dimensions that would stop the
  # Mahalanobis distance calculation from working, and also drastically reduces
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

# Example function to plot the results of the morphmetric analysis. Plots the
# points on the first 2 principal components.
#
# Use with care, since plotting just 2 principal components can obscure
# meaningful patterns in the data.
PlotInMorphospace <- function(coe, title, ...) {
  p <- PCA(coe)
  # This shouldn't be needed, but without it, some points get clipped!
  par(xpd = NA)
  # Plot the result in morphospace. Momocs knows how to plot a PCA
  plot_PCA(p, f = "mimicType", labelgroups = TRUE, chull = FALSE, center_origin = FALSE, palette = col_solarized, zoom = .9) %>% 
    layer_points(cex = .8) %>%
    layer_ellipses %>%
    layer_labelgroups(cex = .9) %>%
    layer_title(title)
}

# Example function to plot probability densities of accuracy by mimic type
PlotMimicTypeDensities <- function(coe, ...) {
  types <- levels(coe$fac$mimicType)
  densities <- lapply(types, function(mt) density(coe$fac$accuracy[m$individual$Coe$fac$mimicType == mt]))
  # Create an empty plot
  xlim <- range(lapply(densities, function(d) d$x), na.rm = TRUE)
  ylim <- range(lapply(densities, function(d) d$y), na.rm = TRUE)
  plot(NULL, xlim = xlim, ylim = ylim, ...)
  # Plot each of the calculated densities
  for(i in seq_len(length(densities))) {
    lines(d[[i]], col = i, lwd = 2)
  }
  legend("topleft", legend = types, col = seq_along(types), lwd = 2)
}

# Returns the name of the output CSV file for the level (individual or species)
# and angle (Dorsal, Lateral or NULL)
.csvName <- function (level, angle = NULL) {
  suf <- if (is.null(angle))
    ""
  else
    paste0("-", tolower(angle))
  file.path(OUTPUT_DIR, sprintf("Geometric morphometrics-accuracy-%s%s.csv", level, suf))
}

# Uses geometric morphometrics to calculate mimetic accuracy.
# 
# @param outlines Data frame describing the photos to be analysed. Must include the
#   columns required by MorphoAnalysisForPhotos, plus an outlineId column. 
RunMorphoAnalysis <- function(outlines, angles = c("Dorsal", "Lateral"), force = FALSE, subsample = NULL, retain = 0.9) {

  # Report missing outlines
  missing <- !file.exists(outlines$file)
  if (sum(missing) > 0) {
    # There's something wrong with the outlines file or else there are images missing
    message(sprintf("There are %d missing outline files.\n%s\n", sum(missing), paste(outlines$file[missing], collapse = "\n")))
  }
  exist <- !missing
  outlines <- outlines[exist, ]

  # Calculates and returns the mimetic accuracy for all points in the dataset.
  # Accuracy of a point is the Mahalanobis distance from the point to centroid
  # of all model shapes, negated and scaled so acuracies range from 0 to 1.
  calcAccuracy <- function(coe) {
    # Calculate distance from each point to centroid of ants
    md <- MorphoMahalanobisDist(coe, retain = retain)
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
    photos <- outlines[outlines$angle == angle, ]
    if (!is.null(subsample))
      photos <- photos[sample(nrow(photos), subsample), ]
    # Perform morphological analysis on these photos. This step can be very
    # slow, so the results will be cached, and the cached results will be
    # returned if the same set of photos is processed
    m <- GetMorphoForPhotos(photos, force = force)
    # Add individual and species accuracies
    m$individual$Coe$fac$accuracy <- calcAccuracy(m$individual$Coe)
    m$species$Coe$fac$accuracy <- calcAccuracy(m$species$Coe)

    # We could plot points if desired
    #PlotInMorphospace(m$individual$Coe, title = angle)
    # Could also plot density distributions of accuracies for different types
    #PlotMimicTypeDensities(m$individual$Coe, main = angle)
    
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

# Copies CSV files from one directory to another
CopyCsvs <- function(fromDir = OUTPUT_DIR, toDir = GLOBAL_OUTDIR) {
  # Now copy all CSV files to the global output directory
  csvs <- list.files(fromDir, ".*\\.csv$", full.names = TRUE)
  copied <- file.copy(csvs, toDir, overwrite = TRUE, copy.mode = TRUE, copy.date = TRUE)
  if (any(copied)) {
    cat(sprintf("Unable to copy file(s): %s\n", csvs[!copied]))
  }
}

# Generates a PCA plot of morphology of individuals
SamplePCAPlot <- function(outlines, angle) {
  photos <- outlines[outlines$angle == angle, ]
  # Get morphological analysis on these photos - hopefully it is cached
  m <- GetMorphoForPhotos(photos, force = FALSE)
  # Plot
  PlotInMorphospace(m$individual$Coe, title = angle, chull = FALSE)
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

# Calculate geometric morphometric accuracy scores for dorsal and lateral
# outlines, creating CSV files in the OUTPUT_DIR directory
RunMorphoAnalysis(outlines, force = force)

# Now copy all CSV files to the global output directory
CopyCsvs()

ShowTime("Total processing time", startTime)
ReportStats(outlines)
JPlotToPNG("../output/morpho-pca.png", SamplePCAPlot(outlines, "Dorsal"), units = "px", width = 900, res = 160)
JPlotToPDF("../output/morpho-pca.pdf", SamplePCAPlot(outlines, "Dorsal"))
