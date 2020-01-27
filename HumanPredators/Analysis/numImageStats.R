# Estimating numbers of images required for human trials

# We hope this is conservative
numberOfParticipants <- 350
trialsPerParticipant <- 1

# Trial size
imagesPerTrial <- 30

####
# How many images in the total pool so that each image is (likely to be) viewed at least minScores times?
# An analytical solution is beyond my capabilities
scoreStats <- function(numTrials, imagesPerTrial, numImages, repeats = 100) {
  scoreCounts <- integer(numImages)
  minScores <- .Machine$integer.max
  for (j in seq_len(repeats)) {
    trialScoreCounts <- integer(numImages)
    for(i in seq_len(numTrials)) {
      s <- sample.int(numImages, imagesPerTrial, replace = FALSE)
      trialScoreCounts[s] <- trialScoreCounts[s] + 1
    }

    minScores <- min(minScores, min(trialScoreCounts[s]))
    scoreCounts <- scoreCounts + trialScoreCounts
  }
  scoreCounts <- scoreCounts / repeats
  c(mean = mean(scoreCounts), sd = sd(scoreCounts), min = minScores)
}


numImages <- 120
ss <- scoreStats(numberOfParticipants * trialsPerParticipant, imagesPerTrial, numImages)
cat(sprintf("%d participants, %d trials per participant, %d images per trial, %d unique images\n", 
            numberOfParticipants, trialsPerParticipant, imagesPerTrial, numImages))
cat(sprintf("Each image will be viewed %g +-%g times, minimum views %g", ss["mean"], signif(ss["sd"], 2), round(ss["min"])))
