# Location of CSV file containing information about each photo, i.e. taxon, is mimic?
PHOTO_INFO_URL <- "https://jimmcl.github.io/EatUp/photo_info.csv"
#PHOTO_INFO_URL <- "http://192.168.0.10:8080/photo_info.csv"

# Colours for plotting
COL_BEFORE_CUTOFF <- "#f8766d"
COL_AFTER_CUTOFF <- "#619cff"

# Quality control - participants who incorrectly score too many ants or non mimics are not included in the analysis
QC_ACCURACY_THRESHOLD <- 0.2


# Date that human ethics permission was granted
ETHICS_FROM <- ISOdatetime(2019, 5, 27, 0, 0, 0, tz = "")



# Given user's scores augmented with photo information, rates them as correct or incorrect
ScoresAreCorrect <- function(scores) {
  (scores$score == "notAnt") == (scores$family != "Formicidae")
}

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
# 
AddPhotoInfo <- function(scores, urlCol = "imageUrl") {
  # Read in photo info (ant, mimic, non-mimic etc.)
  info <- ReadPhotoPoolInfo()
  
  # Match info to scores by matching URLs
  mi <- match(scores[[urlCol]], info$webUrl)
  info <- info[mi, ]
  # Combine
  scores <- cbind(scores, info)
  
  # Add correct flag
  if ("score" %in% names(scores))
    scores$correct <- ScoresAreCorrect(scores)
  
  whosBad <- which(is.na(scores$webUrl))
  if (length(whosBad) > 0) {
    badUrls <- unique(scores$imageUrl[whosBad])
    message(sprintf("%d images are missing descriptions: %s", length(whosBad), JToSentence(badUrls)))
  }
  scores
}

# Decide whether the session is too low quality to use.
# This is just a heuristic. 
ShouldRejectSession <- function(sessionScores) {
  sessionScores$ants_error_rate > QC_ACCURACY_THRESHOLD | sessionScores$nonMimics_error_rate > QC_ACCURACY_THRESHOLD
}

ScoreSession <- function(scores) {
  # We treat Nan errors rates as 0, since there was nothing to get wrong
  .errRate <- function(total, correct) {
    x <- (total - correct) / total
    x[is.nan(x)] <- 0
    x
  }

  ants <- scores$mimicType == "ant"
  mimics <- scores$mimicType == "mimic"
  nonMimics <- scores$mimicType == "non-mimic"

  total_correct <- sum(scores$correct)
  total_scores <- nrow(scores)
  ants_correct <- sum(ants & scores$correct)
  ants_total = sum(ants)
  mimics_correct = sum(mimics & scores$correct)
  mimics_total = sum(mimics)
  nonMimics_correct = sum(nonMimics & scores$correct)
  nonMimics_total = sum(nonMimics)

  scores <- data.frame(
    sessionId = scores$sessionId[1],
    sessionDate = scores$created_at[1],
    total_correct = total_correct,
    total_scores = total_scores,
    total_error_rate = .errRate(total_scores, total_correct),
    ants_correct = ants_correct,
    ants_total = ants_total,
    ants_error_rate = .errRate(ants_total, ants_correct),
    mimics_correct = mimics_correct,
    mimics_total = mimics_total,
    mimics_error_rate = .errRate(mimics_total, mimics_correct),
    nonMimics_correct = nonMimics_correct,
    nonMimics_total = nonMimics_total,
    nonMimics_error_rate = .errRate(nonMimics_total, nonMimics_correct),
    stringsAsFactors = FALSE
  )
  
  # Decide whether the session is too low quality to use
  scores$reject <- ShouldRejectSession(scores)
  
  scores
}


SessionStats <- function(scores) {
  do.call(rbind, lapply(unique(scores$sessionId), function(sessionId) ScoreSession(scores[scores$sessionId == sessionId, ])))
}

# Given some session search criteria, returns all scores from sessions that satisfy the criteria.
# Each row is a single decision by a participant.
# NOTE bad/rejected sessions are included in these results.
GetDecisionScores <- function(db, sessionIds = NULL, from = NULL, to = NULL) {
  scores <- MDbGetSessionScores(db, sessionIds = sessionIds, from = from, to = to)
  AddPhotoInfo(MDbDropType(scores))
}

ScoreSummary <- function(db, sessionIds = NULL, from = NULL, to = NULL) {
  scores <- SessionStats(GetDecisionScores(db, sessionIds = sessionIds, from = from, to = to))
  # Get all sessions
  all <- SessionStats(GetDecisionScores(db))
  # Get all sessions apart from the set of interest
  allOthers <- all[!all$sessionId %in% scores$sessionId, ]

  .values <- function(scores) {
    c(numSession = length(unique(scores$sessionId)),
      numDecisions = sum(scores$total_scores),
      numMimicDecisions = sum(scores$mimics_total))
  }

  rbind(all = .values(all), selection = .values(scores))
}

PlotCompareSession <- function(db, legendLabels = NULL, sessionIds = NULL, from = NULL, to = NULL, addSampleSize = FALSE) {
  # Get focal session scores
  scores <- SessionStats(GetDecisionScores(db, sessionIds = sessionIds, from = from, to = to))
  # Get all sessions apart from the set of interest
  allOthers <- SessionStats(GetDecisionScores(db))
  allOthers <- allOthers[!allOthers$sessionId %in% scores$sessionId, ]
  
  .values <- function(scores) {
    if (is.null(scores)) {
      c(NA, NA, NA, NA)
    } else {
      c(1 - mean(scores$ants_error_rate),
        1 - mean(scores$nonMimics_error_rate),
        1 - mean(scores$mimics_error_rate),
        sum(scores$reject) / nrow(scores))
    }
  }
  
  if (addSampleSize && !is.null(legendLabels)) {
    .addN <- function(lbl, scr) sprintf("%s (N = %d)", lbl, ifelse(is.null(scr), 0, nrow(scr)))
    legendLabels <- c(.addN(legendLabels[1], allOthers), .addN(legendLabels[[2]], scores))
  }

  col <- c(COL_BEFORE_CUTOFF, COL_AFTER_CUTOFF)
  if (nrow(allOthers) == 0) {
    values <- rbind(.values(scores))
    legendLabels <- NULL
    col <- col[2]
  } else
    values <- rbind(.values(allOthers), .values(scores))
  # Draw empty plot first so that the grid lines are behind the bars
  cex <- 1.5
  barplot(values, beside = TRUE,
          names = c("Ants", "Non-mimics", "Mimics", "Reject rate"),
          cex.names = cex, cex.axis = cex,
          ylim = c(0, 1))
  grid(nx = NA, ny = 10)
  barplot(values, beside = TRUE,
          add = TRUE,
          col = col,
          cex.names = cex, cex.axis = cex,
          ylab = "Success rate", cex.lab = cex,
          legend.text = legendLabels,
          args.legend = list(cex = cex, x = "bottomleft", inset = c(0.01, 0.01)),
          main = "Successful identification rates")
}

# Plot a histogram of number of responses over time
PlotResponsesByTime <- function(db, cutoffTime = NULL, from = NULL, to = NULL) {
  
  # Get all sessions
  scores <- MDbGetSessions(db, CreateWhere(GetCreationClause("session", from, to)))
  
  # Convert to local times
  dates <- as.Date(with_tz(ymd_hms(scores$created_at)))
  # Get day numbers relative to the first date. I think there may be a problem here with timezones
  #days <- .asDayNum(dates)
  days <- as.numeric(dates - min(dates))
  
  # Always extend x to today
  td <- as.numeric(today() - min(dates))
  
  col <- COL_AFTER_CUTOFF
  columns <- seq.int(0, td)
  
  # Work out where the cutoff is
  if (!is.null(cutoffTime)) {
    cutoff <- .asDayNum(cutoffTime)
    col <- ifelse(columns < cutoff, COL_BEFORE_CUTOFF, COL_AFTER_CUTOFF)
  }
  breaks <- td
  title <- "Responses per day"
  if (breaks > 200) {
    breaks <- td / 7
    title <- "Responses per week"
    
  }
  hist(days, 
       breaks = if(td > 200) td / 7 else td, 
       col = col, 
       xlim = c(0, td),
       main = title,
       axes = FALSE,
       xlab = NA, ylab = "No. of responses")
  axis(side = 2)

  labs <- seq(from = min(as.Date(dates)), to = today(), by = "day")
  labs <- strftime(labs, "%d %b")
  # Want maximum density of x-axis labels
  labelDensity <- 2 # Labels per inch
  winWidth <- par()$fin[1]
  numLabels <- labelDensity * winWidth
  labelCols <- seq.int(1, length(labs), floor(length(labs) / numLabels))
  text(cex = 1, x = columns[labelCols], y = -1.5, adj = 1, labels = labs[labelCols], xpd = TRUE, srt = 45)
}

# Converts a vector of values to an HTML row
HTMLRow <- function(cells, type = "td", rowClass = NULL) {
  .HTMLCell <- function(c) sprintf("<%s>%s</%s>", type, c, type)

  paste0(c("<tr",
           if(!is.null(rowClass)) { paste0(" class='", rowClass, "'")} else { "" },
           ">",
           sapply(cells, .HTMLCell),
           "</tr>"),
         collapse = "")
}

# Builds an HTML table showing photos with their accuracy scores
HTMLBuildImageScoresTable <- function(mimicType = "mimic", sessionIds = NULL, from = NULL, to = NULL) {
  # Get all scores
  stats <- MDbRun(ImageStats, from = from, to = to)
  # Only display requested type
  stats <- stats[stats$mimicType == mimicType, ]
  # Order on score, from good to bad mimics
  stats <- stats[order(stats$accuracy, decreasing = TRUE), ]
  
  # Check for no scores within the time
  if (length(stats) == 0) {
    return("<span class=\"warn\">No mimic scores are available for the time range</span>")
  }
  
  .trim <- function(s) sub("^ *| *$", "", s)
  
  .imgRow <- function(r) {
    score <- round(as.numeric(r["accuracy"]), 2)
    fmt <- paste0("<tr><td>",
                  "<img src=\"%s%s\"><div class=\"accuracy\">",
                  "<span style=\"width:%d%%\"></span>",
                  "<div>Accuracy = %d%%</div>",
                  "</div>",
                  "Deceived&nbsp;=&nbsp;%s Not&nbsp;deceived&nbsp;=&nbsp;%s",
                  "<span class='species'>%s</span>")
    sprintf(fmt, 
            BASE_IMG_URL,
            r["webUrl"],
            round(score * 100),
            round(score * 100),
            .trim(r["wrong"]), 
            .trim(r["right"]),
            r["species"]
    )
  }
  
  title <- sprintf("%s accuracy scores: %d images, %d decisions", JCapitalise(mimicType), nrow(stats), sum(stats$right) + sum(stats$wrong))
  paste0("<span class=\"subhead\">", title, "</span>",
         "<table class=\"scores\">",
         paste(apply(stats, MARGIN = 1, FUN = .imgRow), collapse = ""),
         "</table>",
         collapse = "")
}

HTMLBuildImageStatsTable <- function() {
  info <- ReadPhotoPoolInfo()
  
  # Treat lateral right side as lateral
  info$angle[info$angle == "Lateral right side"] <- "Lateral"
  info$angle[info$angle == ""] <- "Unspecified"
  
  # Tabulate
  t <- as.matrix(table(info[, c("mimicType", "angle")]))
  t <- cbind(Total = rowSums(t), t)
  # Add a totals row
  t <- rbind(t, colSums(t))
  rownames(t)[dim(t)[1]] <- "Total"
  
  .row <- function(i) {
    HTMLRow(c(rownames(t)[i], t[i,]), rowClass = if(i == dim(t)[1]) "total" else NULL)
  }
  
  paste0("<span class=\"subhead\">Image pool statistics</span>",
         "<table class=\"stats pretty\">",
         HTMLRow(c("", colnames(t)), type = "th"),
         paste0(sapply(seq_len(nrow(t)), .row), collapse = ""),
         "</table>",
         collapse = "")
  
}

# from <- Sys.Date() - 1
# #from <- NULL
# sessionIds <- "0e042212-93c9-ce89-0ca7-c8bdaaccdee1"
# sessionIds <- NULL
# MDbRun(PlotCompareSession, legendLabels = c("Before today", "Today"), sessionIds = sessionIds, from = from)

# Crappy debugging function to output an HTML file with image scores
OutputScoresTable <- function(filename, sessionIds = NULL, from = NULL, to = NULL) {
  sink(filename)
  cat(paste0("<html><head>",
             "<link href=\"www/eatup.css\" rel=\"stylesheet\">",
             "</head>",
             "<body>"));
  cat(HTMLBuildImageScoresTable(sessionIds = sessionIds, from = from, to = to))
  cat(paste0("</body></html>"))
  sink()
}
