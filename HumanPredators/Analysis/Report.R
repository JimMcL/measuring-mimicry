library(JUtils)
library(dabestr)
source("scripts/functions.R")
source("scripts/db.R")
source("scripts/download-firebase.R")
source("scripts/derive-stats.R")
source("scripts/session-info.R")

# Some functionality to explore the data as an aid to setting trial parameters and data analysis

#MDbBringUpToDate(QueryFirebase)

.fmtEstMeanDiff <- function(est, variableName = rlang::as_name(est$x), controlGroup = est$result$control_group, testGroup = est$result$test_group) {
  r <- est$result
  CI95CoversZero <- sign(r$bca_ci_low) != sign(r$bca_ci_high)
  paste(sprintf("Does %s have a significant effect on mimetic accuracy (i.e. is %s different from %s)? %s\n",
                variableName, controlGroup, testGroup, ifelse(CI95CoversZero, "No", "Yes")),
        sprintf("%s %s score %g%% %s %s on average than %s %s, 95%% CI %g%%:%g%%\n",
                testGroup, variableName, 
                abs(round(100 * r$difference)), ifelse(r$difference < 0, "lower", "higher"),
                rlang::as_label(est$y), controlGroup, variableName,
                round(100 * r$bca_ci_low), round(100 * r$bca_ci_high)))
}

.reportEstMeanDiff <- function(...) {
  cat(.fmtEstMeanDiff(...))
}


#############################################################################################
# Numbers of decisions by decision type. We aim to have roughly equally ant/non-ant decisions

PlotDecisionBalance <- function(from = ETHICS_FROM, to = NULL) {
  x <- MDbRun(dbGetQuery, paste("SELECT imageUrl, score",
                                "FROM score", 
                                CreateWhere(GetIdClause("score", sessionIds, colName = "sessionId"), 
                                            GetCreationClause("score", from, to))))
  plot(as.factor(x$score))
  print(table(x$score))
}

#PlotDecisionBalance()


#############################################################################################
# Distibutions of accuracy

PlotAccuracyDistributions <- function(from = ETHICS_FROM, to = NULL) {
  sp <- MDbRun(SpeciesAngleStats, from = from, to = to)
  sp$accuracy[sp$mimicType == "non-mimic"] <- 1 - sp$accuracy[sp$mimicType == "non-mimic"]

  sp$angle[!sp$angle %in% c("Dorsal", "Lateral")] <- "Other"
  combos <- expand.grid(unique(sp$mimicType), unique(sp$angle))
  combos <- combos[order(combos$Var1, combos$Var2), ]
  combos$colour <- ifelse(combos$Var1 == "mimic", "red",
                          ifelse(combos$Var1 == "non-mimic", "blue", "black"))
  combos$line <- ifelse(combos$Var2 == "Dorsal", 1,
                        ifelse(combos$Var2 == "Lateral", 2, 3))
  d <- apply(combos, 1, function(k) {
    a <- sp$accuracy[sp$mimicType == k[1] & sp$angle == k[2]]
    if (length(a) > 0)
      density(a)
    else
      NULL})
  JPlotDensities(d, col = combos$colour, lty = combos$line, ylim = c(0, 40))
  legend(x = 0.3, y = 39, apply(combos[,c("Var1", "Var2")], 1, paste, collapse = ", "), lty = combos$line, col = combos$colour, lwd = 2)
}
PlotAccuracyDistributions()

#############################################################################################
# What is a good escape timeout?

AnalyseResponseTimes <- function(sessionIds = NULL, from = ETHICS_FROM, to = NULL) {
  s <- MDbRun(GetDecisionScores, sessionIds = sessionIds, from = from, to = to)
  types <- unique(s$mimicType)
  
  # # Estimation plot
  # s$mt <- as.factor(s$mimicType)
  # x <- dabestr::dabest(s, mt, time, types)
  # plot(x)

  densities <- lapply(types, function(tp) density(s$time[s$mimicType == tp]))
  xlim <- range(lapply(densities, function(d) d$x), na.rm = TRUE)
  ylim <- range(lapply(densities, function(d) d$y), na.rm = TRUE)
  
  plot(NULL, xlim = xlim, ylim = ylim)
  lines(density(s$time[s$mimicType == "ant"]), col = "red")
  lines(density(s$time[s$mimicType == "non-mimic"]), col = "green")
  lines(density(s$time[s$mimicType == "mimic"]), col = "blue")
  legend("topright", types, lwd = 2, col = c("red", "green", "blue"), inset = c(0.01, 0.01))
}

#AnalyseResponseTimes()

AnalyseEscapePattern <- function(sessionIds = NULL, from = ETHICS_FROM, to = NULL) {
  s <- MDbRun(GetDecisionScores, sessionIds = sessionIds, from = from, to = to)
  hist(s$imageNumber[s$score == "escape"], breaks = length(unique(s$imageNumber) + 1))
  # Conclusion: Looks like people quickly learn to be fast, and get faster over the trial
}

# AnalyseEscapePattern()

# Does accuracy change over time? If so, we could limit trial lengths
AnalyseAccuracyOverTime <- function(sessionIds = NULL, from = ETHICS_FROM, to = NULL) {
  s <- MDbRun(GetDecisionScores, sessionIds = sessionIds, from = from, to = to)
  proportionCorrect <- aggregate(s$correct, list(s$imageNumber), FUN = sum)$x / table(s$imageNumber)
  barplot(proportionCorrect)
  l <- lm(proportionCorrect ~ seq_along(unique(s$imageNumber)))
  print(summary(l))
  # Conclusion: Accuracy doesn't seem to suffer (e.g. due to fatigue) over a trial
}

#AnalyseAccuracyOverTime()

# Does screen resolution affect accuracy of mimic assessment?
MimicAccuracyVsScreenResolution <- function(sessionIds = NULL, from = ETHICS_FROM, to = NULL) {
  s <- MDbRun(GetDecisionScores, sessionIds = sessionIds, from = from, to = to)
  # Only interested in mimics for this
  s <- s[s$mimicType == "mimic", ]
  proportionCorrect <- aggregate(s$correct, list(s$devicePixelRatio), FUN = sum)$x / table(s$devicePixelRatio)
  barplot(proportionCorrect)
  l <- lm(proportionCorrect ~ seq_along(unique(s$devicePixelRatio)))
  print(summary(l))
  abline(l)
  # Conclusion: no, device pixel ratio doesn't affect accuracy scores.
  # However, it's worth noting that we don't display higher resolutino images on higher reolution screens
}
MimicAccuracyVsScreenResolution()

#############################################################################################
# Attempt to analyse which sessions should be rejected.
# Does failure rate on ants or non mimics affect failure rate on mimics?

AnalyseRejectRates <- function(sessionIds = NULL, from = ETHICS_FROM, to = NULL) {
  scores <- SessionStats(MDbRun(GetDecisionScores, sessionIds = sessionIds, from = from, to = to))
  
  # Are error rates correlated?
  p <- par(mfrow = c(3, 1))

  .p <- function(x, y) {
    f <- reformulate(x, y)
    plot(f, data = scores, pch = 16, col = as.integer(scores$reject) + 1, main = "Session error rates")
    l <- lm(f, data = scores)
    abline(l)
    legend("topright", c("Rejected", "Not rejected"), pch = 16, col = c(1, 2))
    summary(l)
  }  
  
  .p("ants_error_rate", "nonMimics_error_rate")
  .p("ants_error_rate", "mimics_error_rate")
  .p("nonMimics_error_rate", "mimics_error_rate")

  par(p)
}

#AnalyseRejectRates()

# Do people make more mistakes on ants or non-mimics?
RejectReasons <- function(from = ETHICS_FROM) {
  scores <- SessionStats(MDbRun(GetDecisionScores, sessionIds = NULL, from = from, to = NULL))
  # Compare ant and non-mimic error rates in rejected sessions
  d <- tidyr::gather(scores[scores$reject, c("ants_error_rate", "nonMimics_error_rate")], key = Group, value = Measurement)
  d <- d[d$Measurement > 0, ]
  db <- dabestr::dabest(d, x = Group, y = Measurement,
                        idx = unique(d$Group))
  plot(db)
}

###
# Does angle make a systematic difference to accuracy?

AnalyseAngleAccuracy <- function(sessionIds = NULL, from = ETHICS_FROM, to = NULL) {
  # Get on species/angles with photos
  sp <- MDbRun(SpeciesAngleStats, from = from)
  # Only interested in mimics for this
  sp <- sp[sp$mimicType == "mimic", ]
  
  est <- dabest(sp, angle, accuracy, unique(sp$angle))
  print(plot(est))
  .reportEstMeanDiff(est)
}

AnalyseAngleAccuracy()


###
# Does background make a systematic difference to accuracy?

AnalyseBackgroundAccuracy <- function(sessionIds = NULL, from = ETHICS_FROM, to = NULL) {
  # Get on species/angles with photos
  sp <- MDbRun(ImageStats, sessionIds = sessionIds, from = from, to = to)
  # Only interested in mimics for this
  sp <- sp[sp$mimicType == "mimic", ]
  
  est <- dabest(sp, background, accuracy, unique(sp$background))
  plot(est)
  # Conclusion: background makes a small but significant difference to mean
  # accuracy; natural backgrounds are rated between .008 and .2 more accurate
  # (i.e. likely to be misidentified)

  .reportEstMeanDiff(est, "background")  
  r <- est$result
  CI95CoversZero <- sign(r$bca_ci_low) != sign(r$bca_ci_high)
  cat(sprintf("Does background have an effect on mimetic accuracy? %s\n", ifelse(CI95CoversZero, "No", "Yes")))
  cat(sprintf("%s backgrounds score %g%% %s on average than %s backgrounds\n",
              r$test_group, abs(round(100 * r$difference)), ifelse(r$difference < 0, "lower", "higher"), r$control_group))
}

AnalyseBackgroundAccuracy()


###
# Does antennal illusion make a difference to accuracy?

AnalyseAntennalIllusion <- function(sessionIds = NULL, from = ETHICS_FROM, to = NULL) {
  # Get on species/angles with photos
  sp <- MDbRun(ImageStats, sessionIds = sessionIds, from = from, to = to)
  # Only interested in mimics for this
  sp <- sp[sp$mimicType == "mimic", ]
  
  op <- par(mfrow = c(1, 2)) # Doesn't work coz ggplot!
  
  sp$`antennal illusion` <- as.character(sp$antennalIllusion)
  est <- dabest(sp, `antennal illusion`, accuracy, unique(sp$`antennal illusion`))
  plot(est)
  cat(paste("For all images:\n", .fmtEstMeanDiff(est)))

  # Look for species that have illusion and no illusion
  illusionSpecies <- unique(sp[sp$antennalIllusion, "species"])
  pairs <- sp$species %in% illusionSpecies & !sp$antennalIllusion
  pairedSpecies <- unique(sp[pairs, "species"])

  est <- dabest(sp[sp$species %in% pairedSpecies, ], `antennal illusion`, accuracy, unique(sp$`antennal illusion`), paired = TRUE, id.column = species)
  plot(est)
  cat(paste("For species with images showing and other images not showing the illusion:\n", .fmtEstMeanDiff(est)))
  
  par(op)

  # Conclusion: no evidence that antennal illusion works!
}

AnalyseAntennalIllusion()

### 
# Confidence in values
AccuracyConfidence <- function(sessionIds = NULL, from = ETHICS_FROM, to = NULL) {
  # Returns the 95% confidence interval of the mean
  CI95ofMean <- function(v) {
    b <- boot::boot(as.numeric(v), function(data, idx) mean(data[idx]), R = 1000)
    ci <- boot::boot.ci(b, type = "norm")
    c(mean = mean(v), ci$normal[2:3])
  }
  scores <- MDbRun(GetDecisionScores, sessionIds = sessionIds, from = from, to = to)
  # Only interested in mimics for this
  scores <- scores[scores$mimicType == "mimic", ]
  cia <- sapply(unique(scores$imageUrl), function(img) CI95ofMean(scores[scores$imageUrl == img, "correct"]))
  boxplot(cia[2:3,order(cia[1,], decreasing = TRUE)], ylab = "Mimetic accuracy", main = "95% confidence intervals of accuracy")
  d <- data.frame(imageUrl = colnames(cia), CIlower = cia[1, ], CIupper = cia[2, ])
  rownames(d) <- NULL
  idx <- match(d$imageUrl, scores$imageUrl)
  d <- cbind(d, scores[idx, c("imageUrl", "family", "class", "species", "forma", 
                              "sex", "angle", "webUrl", "isMimic", "background", "antennalIllusion", 
                              "photographer", "mimicType")])
  d <- d[order(d$species, d$angle), ]
}
AccuracyConfidence()

##################################################################################
# Very preliminary attempt at analysis using a linear mixed-effects model

library(MASS)
library(car)
library(lme4)

sp <- MDbRun(ImageStats, sessionIds = sessionIds, from = from, to = to)
# Only interested in mimics for this
sp <- sp[sp$mimicType == "mimic", ]
sp$antenna <- factor(ifelse(sp$antennalIllusion, "yes", "no"))
sp$angle <- factor(sp$angle)
sp$background <- factor(sp$background)

l <- lmer(accuracy ~ species + angle + (1|antenna|background), data = sp)
summary(l)
Anova(l)
