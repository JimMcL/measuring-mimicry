library(dabestr)
source("scripts/functions.R")
source("scripts/db.R")
source("scripts/download-firebase.R")
source("scripts/derive-stats.R")
source("scripts/session-info.R")

# Some functionality to explore the data as an aid to setting trial parameters and data analysis

# Can't do this any more since ethics expired
#MDbBringUpToDate(QueryFirebase)

.fmtEstMeanDiff <- function(est, variableName = rlang::as_name(est$x), controlGroup = r$control_group, testGroup = r$test_group) {
  r <- dabestr::mean_diff(est)$result
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

PlotDecisionBalance <- function(from = ETHICS_FROM, to = ETHICS_TO) {
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

PlotAccuracyDistributions <- function(from = ETHICS_FROM, to = ETHICS_TO) {
  sp <- MDbRun(SpeciesAngleStats, from = from, to = to)
  # sp$accuracy[sp$mimicType == "non-mimic"] <- 1 - sp$accuracy[sp$mimicType == "non-mimic"]

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
  JPlotDensities(d, col = combos$colour, lty = combos$line, ylim = c(0, 40),
                 main = "Accuracy by species", xlab = "Proportion  misclassified", ylab = "Density")
  legend(x = 0.3, y = 39, apply(combos[,c("Var1", "Var2")], 1, paste, collapse = ", "), lty = combos$line, col = combos$colour, lwd = 2)
}
#PlotAccuracyDistributions()

#############################################################################################
# What is a good escape timeout?

AnalyseResponseTimes <- function(sessionIds = NULL, from = ETHICS_FROM, to = ETHICS_TO) {
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

AnalyseEscapePattern <- function(sessionIds = NULL, from = ETHICS_FROM, to = ETHICS_TO) {
  s <- MDbRun(GetDecisionScores, sessionIds = sessionIds, from = from, to = to)
  hist(s$imageNumber[s$score == "escape"], breaks = length(unique(s$imageNumber) + 1))
  # Conclusion: Looks like people quickly learn to be fast, and get faster over the trial
}

# AnalyseEscapePattern()

# Does accuracy change over game time? If so, we could limit trial lengths
AnalyseAccuracyOverTime <- function(sessionIds = NULL, from = ETHICS_FROM, to = ETHICS_TO) {
  s <- MDbRun(GetDecisionScores, sessionIds = sessionIds, from = from, to = to)
  proportionCorrect <- aggregate(s$correct, list(s$imageNumber), FUN = sum)$x / table(s$imageNumber)
  barplot(proportionCorrect)
  l <- lm(proportionCorrect ~ seq_along(unique(s$imageNumber)))
  print(summary(l))
  # Conclusion: Accuracy doesn't seem to suffer (e.g. due to fatigue) over a trial
}

#AnalyseAccuracyOverTime()

groupedLinearRegression <- function(scores, type, column) {
  # Get requested types
  s <- scores[scores$mimicType == type, ]
  # Group on requested column, getting proportion correct for each group
  proportionCorrect <- aggregate(s$correct, list(s[[column]]), FUN = function(x) sum(x) / length(x))
  names(proportionCorrect) <- c("groupCol", "proportionCorrect")
  plot(proportionCorrect, pch = 21, bg = "lightGrey", xlab = column, main = JCapWords(paste0(type, "s")))
  l <- lm(proportionCorrect ~ groupCol, data = proportionCorrect)
  print(summary(l))
  abline(l)
  cat(sprintf("%s varies from %g to %g with %d distinct values\n", column, min(s[[column]]), max(s[[column]]), length(unique(s[[column]]))))
  cat(sprintf("Does %s affect mimic accuracy? %s\n", 
              column, ifelse(coef(summary(l))[2,4] < 0.05, "Yes", "No")))
}


#############################################################################################
# Attempt to analyse which sessions should be rejected.
# Does failure rate on ants or non mimics affect failure rate on mimics?

AnalyseRejectRates <- function(sessionIds = NULL, from = ETHICS_FROM, to = ETHICS_TO) {
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
RejectReasons <- function() {
  scores <- SessionStats(MDbRun(GetDecisionScores, sessionIds = NULL, from = ETHICS_FROM, to = ETHICS_TO))
  # Compare ant and non-mimic error rates in rejected sessions
  d <- tidyr::gather(scores[scores$reject, c("ants_error_rate", "nonMimics_error_rate")], key = Group, value = Measurement)
  d <- d[d$Measurement > 0, ]
  db <- dabestr::dabest(d, x = Group, y = Measurement,
                        idx = unique(d$Group))
  plot(dabestr::mean_diff(db))
}

###
# Does angle make a systematic difference to accuracy?

AnalyseAngleAccuracy <- function(sessionIds = NULL, from = ETHICS_FROM, to = ETHICS_TO) {
  # Get on species/angles with photos
  sp <- MDbRun(SpeciesAngleStats, from = from)
  # Only interested in mimics for this
  sp <- sp[sp$mimicType == "mimic", ]
  
  est <- dabestr::dabest(sp, angle, accuracy, unique(sp$angle))
  print(plot(dabestr::mean_diff(est)))
  .reportEstMeanDiff(est)
}

#AnalyseAngleAccuracy()


###
# Does background make a systematic difference to accuracy?

AnalyseBackgroundAccuracy <- function(sessionIds = NULL, from = ETHICS_FROM, to = ETHICS_TO) {
  # Get on species/angles with photos
  sp <- MDbRun(ImageStats, sessionIds = sessionIds, from = from, to = to)
  # Only interested in mimics for this
  sp <- sp[sp$mimicType == "mimic", ]
  
  est <- dabestr::dabest(sp, background, accuracy, unique(sp$background))
  plot(dabestr::mean_diff(est))
  # Conclusion: background makes a small but significant difference to mean
  # accuracy; natural backgrounds are rated between .008 and .2 more accurate
  # (i.e. likely to be misidentified)

  .reportEstMeanDiff(est, "background")  
}

#AnalyseBackgroundAccuracy()


###
# Does antennal illusion make a difference to accuracy?

AnalyseAntennalIllusion <- function(sessionIds = NULL, from = ETHICS_FROM, to = ETHICS_TO) {
  # Get on species/angles with photos
  sp <- MDbRun(ImageStats, sessionIds = sessionIds, from = from, to = to)
  # Only interested in mimics for this
  sp <- sp[sp$mimicType == "mimic", ]
  
  op <- par(mfrow = c(1, 2)) # Doesn't work coz ggplot!
  
  sp$`antennal illusion` <- as.character(sp$antennalIllusion)
  est <- dabestr::dabest(sp, `antennal illusion`, accuracy, unique(sp$`antennal illusion`))
  plot(dabestr::mean_diff(est))
  cat(paste("For all images:\n", .fmtEstMeanDiff(est)))

  # Look for species that have illusion and no illusion
  illusionSpecies <- unique(sp[sp$antennalIllusion, "species"])
  pairs <- sp$species %in% illusionSpecies & !sp$antennalIllusion
  pairedSpecies <- unique(sp[pairs, "species"])
  pairedSp <- sp[sp$species %in% pairedSpecies, ]
  
  est <- dabestr::dabest(pairedSp, `antennal illusion`, accuracy, unique(sp$`antennal illusion`), paired = TRUE, id.column = species)
  plot(dabestr::mean_diff(est))
  cat(paste("For species with images both showing and other images not showing the illusion:\n", .fmtEstMeanDiff(est)))
  
  par(op)

  # Conclusion: mixed, but paired test doesn't show an effect and it is probably
  # more reliable, because the unpaired test may just show that mimics that
  # perform the illusion are better mimics
}

#AnalyseAntennalIllusion()

### 
# Confidence in values
AccuracyConfidence <- function(sessionIds = NULL, from = ETHICS_FROM, to = ETHICS_TO) {
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
#AccuracyConfidence()

### 
# Does the audience make a difference?
# 
# Bar plot shows number of decisions made each week.
# Lines show proportion of decisions that were correct for mimics, ants and non-mimics
AudienceEffect <- function() {
  # Get list of all decisions
  scores <- MDbRun(GetDecisionScores, sessionIds = NULL, from = ETHICS_FROM, to = ETHICS_TO)
  # Group them into weeks
  dates <- as.Date(with_tz(ymd_hms(scores$created_at)))
  # Get day numbers relative to the first date
  days <- as.numeric(dates - min(dates))
  # Get week numbers
  scores$weeks <- floor(days / 7)
  .score <- function(week, mimicType) {
    inWeek <- scores$weeks == week
    ofType <- scores$mimicType == mimicType
    # Proportion correct
    sum(scores$correct[inWeek & ofType]) / sum(inWeek & ofType)
  }
  .nDecisions <- function(week) { sum(scores$weeks == week) }
  
  xVals <- unique(scores$weeks)
  counts <- sapply(xVals, .nDecisions)
  # Ignore weeks with < 100 decisions
  xVals <- xVals[counts > 100]
  counts <- counts[counts > 100]
  
  # Given a week number relative to the start of the data, returns its starting date
  .weekToDate <- function(week) {
    date <- min(dates) + week * 7
    strftime(date, "%d %b %Y")
  }

  # Get accuracy scores
  types <- c("mimic", "ant", "non-mimic")
  sc <- sapply(types, function(type) sapply(xVals, .score, type))
  
  # Summarise in words
  cat(sprintf("Mimic accuracy varied from %.2g (on %s) to %.2g (%s)\n",
              min(sc[,1]), .weekToDate(xVals[which.min(sc[,1])]),
              max(sc[,1]), .weekToDate(xVals[which.max(sc[,1])])))
  
  par(mar = c(7, 4, 4, 4) + .1)
  
  yMax <- 4000 # Gives us nicely rounded axis labels
  xScale <- barplot(counts / yMax, space = 2, xaxt = "n", ylim = c(0, 1), xlab = "", ylab = "Proportion correct", main = "Accuracy by audience")
  lines(xScale, sc[,1], lwd = 2, col = "red", lty = 1)
  lines(xScale, sc[,2], lwd = 2, col = "blue", lty = 2)
  lines(xScale, sc[,3], lwd = 3, col = "purple", lty = 3)
  legend("left", JCapWords(paste0(colnames(sc), "s")), col = c("red", "blue", "purple"), lwd = c(2, 2, 3), lty = c(1, 2, 3), inset = c(.01, 0))
  # X axis labels
  labels <- .weekToDate(xVals)
  axis(1, at = xScale, labels = labels, las = 2)
  # RHS axis labels for bar plot
  axis(4, at = seq(0, 1, length.out = 5), labels = round(seq(0, yMax, length.out = 5)))
  mtext("No. of decisions", 4, line = 3)
  
  # Label some important times
  notes <- data.frame(label = c("MQ biology HDR conference", "ASSAB conference",
                       "General public outreach", "General public outreach", "MQ open day demo",
                       "Biol undergrads", "Biol undergrads", "Advertised on social media"),
             date = dmy(c("13-Jun-2019", "07-Jul-2019", "10-Aug-2019", "13-Aug-2019", "17-Aug-2019", "11-Sep-2019", "13-Sep-2019", "30-Sep-2019")))
  notes$week <- floor(as.numeric(notes$date - min(dates)) / 7)
  notes$x <- match(notes$week, xVals)
  # Get rid of overlain notes
  notes <- notes[!duplicated(notes$x), ]
  points(xScale[notes$x], sc[notes$x, 1], pch = 16)
  text(xScale[notes$x], sc[notes$x, 1], notes$label, pos = c(1, 3), xpd = TRUE)
}
cat("\nEffect of audience\n==================\n")
#AudienceEffect()
JPlotToPNG("../../output/audience-effect.png", AudienceEffect,
           units = "px", width = 750, aspectRatio = 4 / 3)

.getScoresWithRejection <- function() {
  scores <- MDbRun(GetDecisionScores, sessionIds = NULL, from = ETHICS_FROM, to = ETHICS_TO)
  sessions <- SessionStats(scores)
  scores$rejected <- scores$sessionId %in% sessions$sessionId[sessions$reject]
  scores
}

# Does audience experience make a difference?
ExperienceEffect <- function() {
  # Get list of all decisions
  scores <- MDbRun(GetDecisionScores, sessionIds = NULL, from = ETHICS_FROM, to = ETHICS_TO)
  scores$`First time` <- ifelse(scores$firstTime, "Newby", "Experienced")
  scores$`Correct` <- ifelse(scores$correct, "Right", "Wrong")
  .rep <- function(tp) {
    mm <- scores[scores$mimicType == tp, ]
    tt <- table(mm$`First time`, mm$correct)
    acc <- (tt / rowSums(tt))[, 2]
    cat(sprintf("%s correct classifications: first timers %.2g%% (n = %d), repeat players %.2g%% (n = %d)\n",
                JCapWords(tp), 100 * acc[2], sum(mm$firstTime), 100 * acc[1], sum(!mm$firstTime)))
    ch <- chisq.test(tt)
    cat(sprintf("chi-squared = %.2g, %s = %g, n = %d, p-value = %.2g\n", ch$statistic, names(ch$parameter), ch$parameter, nrow(mm), ch$p.value))
  }
  .rep("mimic")
  #.rep("ant")
  #.rep("non-mimic")
}
cat("\nEffect of player experience\n===========================\n")
ExperienceEffect()

# Does screen resolution or size affect accuracy of mimic assessment?
MimicAccuracyVsScreen <- function() {
  s <- MDbRun(GetDecisionScores, sessionIds = NULL, from = ETHICS_FROM, to = ETHICS_TO)
  groupedLinearRegression(s, "mimic", "devicePixelRatio")
  # groupedLinearRegression(s, "ant", "devicePixelRatio")
  # groupedLinearRegression(s, "non-mimic", "devicePixelRatio")
  
  # Conclusion: no, device pixel ratio doesn't affect accuracy scores.
  # However, it's worth noting that we don't display higher resolution images on higher resolution screens
  
  # Get window diagonal
  s$diagonal <- sqrt(s$screenheight ^ 2 + s$screenWidth ^ 2)
  groupedLinearRegression(s, "mimic", "diagonal")
  # groupedLinearRegression(s, "ant", "diagonal")
  # groupedLinearRegression(s, "non-mimic", "diagonal")
  # Conclusion: no, screen size doesn't affect accuracy scores for mimics.
}
cat("\nEffect of screen size & resolution\n==================================\n")
MimicAccuracyVsScreen()


# Is there a significant difference in categorisation between rejected & non-rejected sessions?
IsQualityRejectionUseful <- function() {
  # Get list of all decisions
  scores <- .getScoresWithRejection()
  
  .rep <- function(tp) {
    mm <- scores[scores$mimicType == tp, ]
    tt <- table(mm$rejected, mm$correct)
    acc <- (tt / rowSums(tt))[, 2]
    cat(sprintf("Not rejected got %.2g%% of %s correct (n = %d), rejected got %.2g%% correct (n = %d)\n",
                100 * acc[1], paste0(tp, "s"), sum(mm$rejected), 100 * acc[2], sum(!mm$rejected)))
    ch <- chisq.test(tt)
    cat(sprintf("chi-squared = %g, %s = %g, p-value = %g\n", signif(ch$statistic, 2), names(ch$parameter), ch$parameter, signif(ch$p.value, 2)))
  }
  .rep("mimic")
  #.rep("ant")
  #.rep("non-mimic")
}
cat("\nIs our quality control useful?\n==============================\n")
IsQualityRejectionUseful()

##################################################################################
# Very preliminary attempt at analysis using a linear mixed-effects model

# library(MASS)
# library(car)
# library(lme4)
# 
# sp <- MDbRun(ImageStats, sessionIds = sessionIds, from = from, to = to)
# # Only interested in mimics for this
# sp <- sp[sp$mimicType == "mimic", ]
# sp$antenna <- factor(ifelse(sp$antennalIllusion, "yes", "no"))
# sp$angle <- factor(sp$angle)
# sp$background <- factor(sp$background)
# 
# l <- lmer(accuracy ~ species + angle + (1|antenna|background), data = sp)
# summary(l)
# Anova(l)
