#!Rscript
# 
# R script to interpret raw labels from google vision.  Input is a CSV
# file with google vision labels and probabilities for each image.
# Output is some CSV files with accuracy per image, accuracy per
# species and angle, and accuracy per species.
#
# Usage : Rscript extract-accuracy.R

# Defines the set of photos to be processed
PHOTO_INFO_URL <- "data/mimics.csv"

# Where to write intermediate and output files
OUTDIR <- "output"
# WWhere to copy output files
GLOBAL_OUTDIR <- "../output"
# name of intermediate file
RAW_LABELS_FN <- "gv_raw_labels.csv"
GOOGLE_VISION_IMAGE_LABELS_CSV <- file.path(OUTDIR, RAW_LABELS_FN)


# Reads in pre-calculated Google image labels with scores, and
# calculates an antLike score (i.e. mimetic accuracy) for each photo
# as the highest probability associated with any label ending in the
# word "ant".
#
# @returns Data frame with columns \code{imageUrl} and \code{accuracy}.
ReadGoogleVisionImageAccuracy <- function() {
  gv <- read.csv(GOOGLE_VISION_IMAGE_LABELS_CSV, stringsAsFactors = FALSE)
  urls <- unique(gv$id)
  scores <- sapply(urls, function(url) {
    labels <- gv[gv$id == url, ]
    # Which label should we use? Pick the highest probability label
    # that ends in the word "ant", e.g. could be "Carpenter ant"
    candidates <- grep("^ant$|\\Want", labels$label, value = TRUE, ignore.case = TRUE)
    labels <- labels[labels$label %in% candidates, ]
    best <- which.max(labels$probability)
    # Accuracy is just the probability of the "best" label
    p <- labels[best, "probability"]
    # Assume no ant label means it's a crap mimic, accuracy = 0
    ifelse (length(p) == 0, 0, p)
  })
  data.frame(imageUrl = names(scores), antLike = scores)
}

# Reads in the photo list, and manipulates it slightly to get it the
# way we want it
ReadPhotoPoolInfo <- function() {
  info <- read.csv(PHOTO_INFO_URL, stringsAsFactors = FALSE)
  # Get rid of empty rows
  info <- info[info$species != "", ]
  # Convert any type of lateral (e.g. "lateral right side") to lateral
  info$angle[grep("lateral", info$angle, ignore.case = T)] <- "Lateral"
  # Classify as ant, mimic or non-mimic
  info$mimicType <- ifelse(info$family == "Formicidae", "ant", 
                           ifelse(info$isMimic, "mimic", "non-mimic"))
  info
}

# Adds photo info to user scores.
AddPhotoInfo <- function(scores, urlCol = "imageUrl") {
  # Read in photo info (ant, mimic, non-mimic etc.)
  info <- ReadPhotoPoolInfo()
  
  # Match info to scores by matching URLs
  mi <- match(scores[[urlCol]], info$webUrl)
  info <- info[mi, ]
  # Combine
  scores <- cbind(scores, info)
  
  whosBad <- which(is.na(scores$webUrl))
  if (length(whosBad) > 0) {
    badUrls <- unique(scores$imageUrl[whosBad])
    message(sprintf("%d images are missing descriptions: %s", length(whosBad), JToSentence(badUrls)))
  }
  scores
}

# Averages image stats to species and angle stats.
GoogleVisionSpeciesAngleStats <- function(gv,
                                          includeAngles = c("all", "Dorsal", "Lateral", "Posterior", "Anterior"),
                                          fun = mean,
                                          byCols = c("class", "family", "species", "angle", "isMimic")) {
  gv <- AddPhotoInfo(gv)
  
  # Optionally exclude some angles because google vision isn't very good at them
  if (length(includeAngles) > 1 || includeAngles != "all") {
    gv <- gv[gv$angle %in% includeAngles, ]
  }
  
  by <- lapply(byCols, function(c) gv[[c]])
  a <- aggregate(gv$antLike, by = by, FUN = function(v) { c(fun(v), length(v)) } )
  # Flatten the aggregated column
  a <- cbind(a[, seq_along(byCols)], unlist(a$x))
  names(a) <- c(byCols, "accuracy", "numPhotos")
  # Order on accuracy then byCols
  a <- a[do.call(order, lapply(byCols, function(c) a[[c]])), ]
  a[order(a$accuracy, decreasing = TRUE), ]
}

# Averages 
GoogleVisionSpeciesStats <- function(gv, fun = mean, includeAngles = "all") {
  GoogleVisionSpeciesAngleStats(gv, fun = fun, byCols = c("class", "family", "species",  "isMimic"), includeAngles = includeAngles)
}

# Copies CSV files from one directory to another, excluding any that
# match the specified pattern.
CopyCsvs <- function(fromDir = OUTDIR, excluding = RAW_LABELS_FN, toDir = GLOBAL_OUTDIR) {
  # Now copy all CSV files to the global output directory
  csvs <- list.files(fromDir, ".*\\.csv$", full.names = TRUE)
  # Don't copy them all
  csvs <- csvs[!grepl(excluding, csvs)]
  copied <- file.copy(csvs, toDir, overwrite = TRUE, copy.mode = TRUE, copy.date = TRUE)
  if (any(copied)) {
    cat(sprintf("Unable to copy file(s): %s\n", csvs[!copied]))
  }
}


################################################################################
# Do it!

# Get scores from Google vision raw data CSV
gv <- ReadGoogleVisionImageAccuracy()

###
# Save accuracy per image
gv <- gv[order(gv$antLike), ]
write.csv(gv, "output/Machine learning-accuracy-images.csv", row.names = FALSE)

###
# Calculate and write accuracy per species/angle
sa <- GoogleVisionSpeciesAngleStats(gv)
write.csv(sa, file.path(OUTDIR, "Machine learning-accuracy-species-angle.csv"), row.names = FALSE)
# Calculate and write accuracy per species. Only use dorsal and
# lateral, because google vision can't even identify an ant when
# photographed head-on
sp <- GoogleVisionSpeciesStats(gv, includeAngles = c("Dorsal", "Lateral"))
write.csv(sp, file.path(OUTDIR, "Machine learning-accuracy-species.csv"), row.names = FALSE)

####
# Now copy all CSV files, apart from the raw labels file, to the global output directory
CopyCsvs()

