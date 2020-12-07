# Reports the state of the database

# Calculates right and wrong scores for each image.
# Only uses non-rejected sessions to calculate scores.
ImageStats <- function(db, sessionIds = NULL, from = NULL, to = NULL) {
  # Get scores
  scores <- GetDecisionScores(db, sessionIds = NULL, from = from, to = to)
  if (nrow(scores) == 0)
    return(data.frame())
  
  # Get candidate sessions
  sessions <- SessionStats(scores)
  # Only use non-rejected sessions
  sessionIds <- sessions$sessionId[!sessions$reject]
  scores <- scores[scores$sessionId %in% sessionIds, ]
  if (nrow(scores) == 0)
    return(data.frame())
  
  groupBy <- list(scores$class, scores$family, scores$species, scores$angle, scores$mimicType, scores$webUrl, scores$antennalIllusion, scores$background)
  r <- aggregate(scores$correct, groupBy, function(x) c(right = sum(x), wrong = sum(!x), accuracy = mean(!x)))
  # Weird - currently r contains a column (x) with a matrix! Flatten it out into 2 columns
  r <- cbind(r[, 1:8], unlist(r$x))
  # Add in mean and sd of decision times
  t <- aggregate(scores$time, groupBy, function(x) c(mean = mean(x), sd = sd(x)))
  r <- cbind(r, unlist(t$x))
  colnames(r) <- c("class", "family", "species", "angle", "mimicType", "webUrl", "antennalIllusion", "background", "right", "wrong", "accuracy", "decisionTimeMean", "decisionTimeSD")
  r
}

# Calculate accuracy from right and wrong scores for each species/angle, using only non-rejected sessions.
# "Accuracy" here means likelihood of being misclassified.
SpeciesAngleStats <- function(db, from = ETHICS_FROM, to = NULL) {
  is <- ImageStats(db, from = from, to = to)
  is$numPhotos <- 1
  sp <- aggregate(cbind(right, wrong, numPhotos) ~ class + family + species + angle + mimicType, data = is, sum)
  # There are two ways we could calculate species accuracy scores:
  # 1) averaging photo accuracy across all photos for the species and angle, or
  # 2) calculate right / (right + wrong) for all photos for species/angle.
  # I'm using 2) so that photos with fewer decisions don't overpower those with many decisions
  sp$accuracy <- sp$wrong / (sp$right + sp$wrong)
  # Sort
  sp <- sp[order(sp$class, sp$family, sp$species, sp$angle), ]
  sp
}

# Calculate right and wrong scores for each species
# 
# @param anglesToInclude If not NULL, only photos with angles in this list will be included in the results.
SpeciesStats <- function(db, anglesToInclude = NULL, from = ETHICS_FROM, to = NULL) {
  is <- ImageStats(db, from = from, to = to)
  if (!is.null(anglesToInclude)) {
    is <- is[is$angle %in% anglesToInclude, ]
  }
  is$numPhotos <- 1
  sp <- aggregate(cbind(right, wrong, numPhotos) ~ class + family + species + mimicType, data = is, sum)
  sp$accuracy <- sp$wrong / (sp$right + sp$wrong)
  # Sort
  sp <- sp[order(sp$class, sp$family, sp$species), ]
  sp
}

