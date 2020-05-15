# Functions to perform outline morphometric analysis on body outline images.
# Input is a set of photos and meta data, output is a Fourier characterisation
# of photos, individuals, species and mimic types.
#
# These calculations are _slow_, so results are saved and reused if the same
# list of photos is processed.

suppressMessages(library(Momocs))


# Loads a set of photos, converts them to outlines, subsamples and smooths them.
#
# photos - data frame which specifies the set of photos to be loaded. 
#          Must have a "file" column. Used as the "fac" for the Out object.
# sampleSize - each outline is subsampled to this number of points
# 
# value: momocs Out object
.loadPhotos <- function(photos, sampleSize = 1600) {
  # Convert images to outlines
  coords <- import_jpg(as.character(photos$file))
  
  # Subsample or interpolate points to a standard number of points.
  coords <- lapply(coords, function(m) {
    if (nrow(m) < sampleSize) {
      coo_interpolate(m, sampleSize)
    } else {
      coo_sample(m, sampleSize)
    }
  })
  
  o <- Out(coords, fac = photos)
  
  # Close the outline and smooth
  coo_close(o) %>% coo_smooth(5)
}

# Removes the named columns from the specified data frame
.removeCols <- function(df, colsToRemove) {
  # Ignore missing columns
  colsToKeep <- setdiff(names(df), colsToRemove)
  df[, colsToKeep]
}

# Performs a morphometric analysis on a set of photos
#
# @param photos data frame with a row for each photo to be processed. Columns:
#     file location of jpg file
#     specimenId unique ID of specimen
#     species scientific name of species
#     mimicType a factor classifying specimens according to their role
#        in the mimicry complex. The factor may have any values, eg,
#        "mimic", "model", "non-mimic". An average shape is calculated
#        for each mimic type.
# @param bodyLength optional
#
# @returns list with elements "photo", "individual", "species", "type". photo
#   is a Momocs Coe object, remaining elements are lists with members "Coe" and
#   "shp". 
MorphoAnalysisForPhotos <- function(photos, startTime = NULL) {
  
  # specimenId and speciedId must be a factors with no unused levels
  photos$specimenId <- droplevels(as.factor(photos$specimenId))
  photos$species <- droplevels(as.factor(photos$species))
  photos$mimicType <- droplevels(as.factor(photos$mimicType))
  
  .st <- function(msg) {
    ShowTime(msg, startTime)
  }
  # .st <- function(...) {}
  
  # Load photos and convert to subsampled, smoothed outlines
  outlines <- .loadPhotos(photos)
  .st(sprintf("Loaded %d outlines in", length(outlines)))
  
  # Align them
  pt <- proc.time()
  aligned <- fgProcrustes(outlines, coo = TRUE)
  .st(sprintf("Procrustes alignment (%g secs/shape):", round((proc.time() - pt)[3] / length(aligned$coo), 1)))
  
  # Run the elliptical fourier analysis
  minCoords <- min(sapply(outlines$coo, length)) / 2
  fr <- efourier(aligned, norm = T, nb.h = minCoords %/% 2)
  .st("Fourier analysis")

  # Average multiple photo outlines to individual outlines.
  # Note that specimen id is called imageableid
  individual <- mshapes(fr, 'specimenId')
  # Remove columns that make no sense for an individual potentially averaged from multiple photos
  individual$Coe$fac <- .removeCols(individual$Coe$fac, c("outlineId", "file", "ptype", "state", "source", "camera", 
                                                          "rating", "ftype", "url", "decimalLatitude", 
                                                          "decimalLongitude", "coordinateUncertaintyInMeters", "elevation", 
                                                          "locationRemarks", "time", "day", "month", "year", "recordedBy", 
                                                          "siteId", "Species", "Angle", "Unique.ID", "Unique.qualifier",
                                                          "Potential.Source.for.image", "Image.found", "Aspect", "photoDescription", 
                                                          "File", "File.exists.", "Adjustments..angle.adjustments..interpolation.",
                                                          "Notes.1"))
  # Average individuals to species
  species <- mshapes(individual$Coe, 'species')
  # Remove columns that don't make sense for a species (or are duplicated). Leave bodylength so it can be averaged
  species$Coe$fac <- .removeCols(species$Coe$fac, 
                                 c("specimenId", "description", "individualCount", "sex", 
                                   "lifeStage", "form", "notes", "idconfidence", "other", "ref", 
                                   "disposition", "numphotos", 
                                   "Sex", "Notes", "Done."))
  # Average species to mimic types. This gives us average shapes for
  # whatever mimic types exist in the data
  types <- mshapes(species$Coe, 'mimicType')
  # Remove columns that don't make sense for a type
  types$Coe$fac$species <- NULL
  .st("Shape averaging")
  
  # Fix up mean values in averaged groups, i.e. take mean instead of first
  # value. mshapes just takes the value from the first row of a group
  .agg <- function(specific, group, idCol, valueCol = 'bodylength', fun = mean, na.rm = TRUE) {
    r <- c()
    for(i in 1:nrow(group)) {
      r[i] <- fun(specific[[valueCol]][specific[[idCol]] == group[[idCol]][i]], na.rm = na.rm)
    }
    r
  }
  if ("bodylength" %in% names(individual$Coe$fac)) {
    species$Coe$fac$bodylength <- .agg(individual$Coe$fac, species$Coe$fac, 'species', 'bodylength')
    types$Coe$fac$bodylength <- .agg(species$Coe$fac, types$Coe$fac, 'mimicType', 'bodylength')
  }
  
  list(photo = fr, individual = individual, species = species, type = types)
}

# Print elapsed time. Silent noop if startTime is null.
#
# startTime <- proc.time()
# ...
# ShowTime("Long process:", startTime)
# 
# value - elapsed seconds
ShowTime <- function(msg, startTime) {
  if (!is.null(startTime)) {
    elapsedSecs <- (proc.time() - startTime)[3]
    elapsed <- elapsedSecs
    if (elapsed >= 3600) {
      elapsed <- elapsed / 3600
      units <- "hours"
    } else if (elapsed >= 60) {
      elapsed <- elapsed / 60
      units <- "mins"
    } else {
      units <- "secs"
    }
    cat(sprintf(paste(msg, "%g %s\n"), round(elapsed, 2), units))
    invisible(elapsedSecs)
  }
}

.getMorphoCacheFileName <- function(angle = c("Dorsal", "Lateral")) {
  angle <- match.arg(angle)
  file.path(OUTLINE_DIR, paste0("morpho-", angle, '.rds'))
}

# Reads in a precalculated morphometric analysis, checks if it matches the
# photos, and if not, recalculates it. All photos are assumed to have been taken
# from the same angle (photos$angle). If photos are not specified, precalculated
# results are unconditionally returned (and angle must be specified).
# 
# To obtain pre-calculated morphometric analysis for Dorsal outlines, call 
# \code{GetMorphoForPhotos(NULL, "Dorsal")}
#
# @para photos Data frame describing the photos to be analysed. Must include the
#   columns required by MorphoAnalysisForPhotos, plus an outlineId column. If NULL,
#   the cached analysis is returned (angle must be specified in this case).
# @param angle Viewing angle of photos to be analysed.
# @see MorphoAnalysisForPhotos for return value
GetMorphoForPhotos <- function(photos, angle = photos[1,]$angle, verbose = TRUE, force = FALSE) {
  
  # Try to read in the result of the last analysis
  analFile <- .getMorphoCacheFileName(angle)
  result <- tryCatch(readRDS(analFile), 
                     error = function(c) NULL,
                     warning = function(c) NULL)
  
  # Did we manage to read in an up-to-date file? Assume photos can't change unless their ids change
  #cat(sprintf("Null result? %s, null photos? %s, idsets equal? %s, verbose? %s\n", is.null(result), is.null(photos), setequal(result$photo$id, photos$id), verbose))
  if (force || is.null(result) || (!is.null(photos) && !setequal(result$photo$outlineId, photos$outlineId))) {
    if (is.null(photos)) {
      stop(sprintf("Looks like you tried to use cached morphometrics which don't exist! Run the script cache_morphometrics.R"))
    }
    if (verbose) {
      reason <- if (force) "recalculation requested" else if (is.null(result)) "no cached data" else "requested outline ids have changed"
      cat(sprintf("Recalculating morphometrics for %s photos, %s\n", angle, reason))
    }
    # Something has changed. Perform morphometric analysis on all the photos
    startTime <- if(verbose) proc.time() else NULL
    
    result <- MorphoAnalysisForPhotos(photos, startTime)
    saveRDS(result, analFile)
    
    ShowTime("Total calculation time", startTime)
  }
  
  result
}

UncacheMorpho <- function() {
  fn <- .getMorphoCacheFileName("Lateral")
  if (file.exists(fn))
      file.remove(fn)
  fn <- .getMorphoCacheFileName("Dorsal")
  if (file.exists(fn))
      file.remove(fn)
}

##############################################################################

# Reads in the pre-calculated morphometric scores
ReadMorphoAccuracy <- function(level = c("species", "individuals"), angle = c("dorsal", "lateral")) {
  angle <- match.arg(angle)
  level <- match.arg(level)
  filename <- file.path(OUTLINE_DIR, paste0("accuracy-", level, "-", angle, ".csv"))
  if (!file.exists(filename))
    stop(paste0("Pre-calculated morphometric results not found: ", filename))
  read.csv(filename, stringsAsFactors = FALSE)
}

