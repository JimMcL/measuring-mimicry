# Report on local species, to aid in designing predator trials
source("../sampleIt.R")
source("scripts/db.R")

PlotLocalMimicAccuracy <- function(plotSpecies = FALSE, sessionIds = NULL, from = ETHICS_FROM, to = NULL) {
  # Returns the 95% confidence interval of the mean
  CI95ofMean <- function(v) {
    b <- boot::boot(as.numeric(v), function(data, idx) mean(data[idx]), R = 1000)
    ci <- boot::boot.ci(b, type = "norm")
    c(mean = mean(v), ci$normal[2:3])
  }
  scores <- MDbRun(GetDecisionScores, sessionIds = sessionIds, from = from, to = to)
  # Scrap rejected sessions
  sessions <- SessionStats(scores)
  rejectedSessions <- sessions$sessionId[sessions$reject]
  scores <- scores[!scores$sessionId %in% rejectedSessions, ]
  # Only interested in mimics for this
  scores <- scores[scores$mimicType == "mimic", ]
  
  # Calculate 95% CI
  ciaa <- aggregate(!scores$correct, by = list(scores$imageUrl), FUN = CI95ofMean)
  names(ciaa) <- c("imageUrl", "CI")
  # Join in extra columns
  c <- scores[, c("imageUrl", "species", "angle", "mimicType")]
  c <- c[order(c$imageUrl), ]
  c <- c[!duplicated(c), ]
  ciaa <- cbind(ciaa, c[match(ciaa$imageUrl, c$imageUrl), c("species", "angle", "mimicType")])
  ciaa <- ciaa[order(ciaa$CI[,1], decreasing = TRUE), ]
  ciaa$local <- ciaa$species %in% GetSydneySpeciesNames()
  
  # Plot CI for species
  ciSp <- aggregate(!scores$correct, by = list(scores$species), FUN = CI95ofMean)
  names(ciSp) <- c("species", "CI")
  ciSp <- ciSp[order(ciSp$CI[,1], decreasing = TRUE), ]
  ciSp$local <- ciSp$species %in% GetSydneySpeciesNames()
  col <- ifelse(ciSp$local, "red", "lightGrey")
  op <- par(mar = c(11, 6, 4, 1) + .1)
  boxplot(t(ciSp$CI[, 2:3]), names = rep("", length.out = nrow(ciSp)), col = col, ylab = "Mimetic accuracy", main = "Accuracy of local vs non-local species per image")
        text(1:nrow(ciSp) + .2, par("usr")[3] - .025, 
       srt = 45, pos = 2, #adj = 1.2, 
       xpd = TRUE,
       labels = ciSp$species)
  par(op)
  print(ciSp[ciSp$local, ])
  
  
  # if (!plotSpecies) {
  #   angleCol <- factor(ciaa$angle)
  #   col <- ifelse(ciaa$local, angleCol, "lightGrey")
  #   boxplot(t(ciaa$CI[, 2:3]), ylab = "Mimetic accuracy", main = "Accuracy of local vs non-local species per image", col = col)
  #   legend("topright", levels(angleCol), fill = seq_along(levels(angleCol)))
  # }
  #   
  # if (plotSpecies) {
  #   op <- par(ask = TRUE)
  #   for(sp in unique(ciaa$species)) {
  #     col <- ifelse(ciaa$species == sp, angleCol, "grey")
  #     boxplot(t(ciaa$CI[, 2:3]), ylab = "Mimetic accuracy", main = sprintf("Accuracy of %s", sp), col = col)
  #     legend("topright", levels(angleCol), fill = seq_along(levels(angleCol)))
  #   }
  #   par(op)
  # }
  # 
  # print(ciaa[ciaa$local, ])
}

PlotLocalMimicAccuracy(plotSpecies = T)
