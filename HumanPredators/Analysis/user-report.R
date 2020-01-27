# User statistics, may be useful when sending emails to participants

UserStats <- function(userId) {
  ss <- MDbRun(MDbGetSessionsWithScores, MDbSessionWhere(other = sprintf("userId = '%s'", userId)))
  sessionIds <- unique(ss$sessionId)
  
  ssp <- AddPhotoInfo(ss)
  mimics <- ssp[ssp$mimicType == "mimic", ]
  ants <- ssp[ssp$mimicType == "ant", ]
  nonMimics <- ssp[ssp$mimicType == "non-mimic", ]
  
  cat(sprintf("%d sessions, with an average of %g decisions per trial\n", length(sessionIds), nrow(ss) / length(sessionIds)))

  l <- lapply(list(ssp, mimics, ants, nonMimics), function(d) c(sum(d$correct), sum(!d$correct), round(100 * sum(d$correct) / nrow(d))))
  t <- do.call(rbind, l)
  colnames(t) <- c("Correct", "Incorrect", "% correct")
  rownames(t) <- c("Total", "Mimics", "Ants", "Non-mimics")
  t
}

# UserStats("210a7881-8b9c-7598-070b-1f1dcdcdde79")
