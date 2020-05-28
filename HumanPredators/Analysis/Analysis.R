# Obtains data from the EatUp web game, and creates output CSV files with image,
# species/angle, and species mimetic accuracy scores

# If run from the command line, it is assumed that the working directory is the
# EatUp subdirectory. If not, you will get a warning such as 
# In file(filename, "r", encoding = encoding) :
#   cannot open file 'scripts/functions.R': No such file or directory

library(JUtils)
if (!file.exists("scripts/functions.R"))
  stop("This script must be run from the EatUp directory")
source("scripts/functions.R")
source("scripts/db.R")
source("scripts/download-firebase.R")
source("scripts/derive-stats.R")
source("scripts/session-info.R")

# Output for this (i.e. human predators) method
OUTPUT_DIR <- "../../output"
# Output for the entire project
GLOBAL_OUTDIR <- "../../../output"


CopyCsvs <- function(fromDir = OUTPUT_DIR, toDir = GLOBAL_OUTDIR) {
  # Now copy all CSV files to the global output directory
  csvs <- list.files(fromDir, ".*\\.csv$", full.names = TRUE)
  copied <- file.copy(csvs, toDir, overwrite = TRUE, copy.mode = TRUE, copy.date = TRUE)
  if (any(copied)) {
    cat(sprintf("Unable to copy file(s): %s\n", csvs[!copied]))
  }
}

# Exports CSV files of mimetic accuracy for images, species and species/angles
CreateOutputFiles <- function(from = ETHICS_FROM, to = ETHICS_TO) {
  if (!dir.exists(OUTPUT_DIR))
    dir.create(OUTPUT_DIR)
  
  # Report on image scores
  sp <- MDbRun(ImageStats, sessionIds = sessionIds, from = from, to = to)
  write.csv(sp, file.path(OUTPUT_DIR, "Human predators-accuracy-images.csv"), row.names = FALSE)
  
  # Report on species/angles with photos
  sp <- MDbRun(SpeciesAngleStats, from = from, to = to)
  write.csv(sp, file.path(OUTPUT_DIR, "Human predators-accuracy-species-angle.csv"), row.names = FALSE)
  # Report on species
  sp <- MDbRun(SpeciesStats, from = from, anglesToInclude = c("Lateral", "Dorsal", "Lateral right side"))
  write.csv(sp, file.path(OUTPUT_DIR, "Human predators-accuracy-species.csv"), row.names = FALSE)
  
  # Copy results to global output directory
  CopyCsvs()
}

ReportSessionsInfo <- function() {
  scores <- SessionStats(MDbRun(GetDecisionScores, sessionIds = NULL, from = ETHICS_FROM, to = ETHICS_TO))
  nRejected <- sum(scores$reject)
  n <- nrow(scores)
  cat(sprintf("%d sessions, of which %d were rejected (%d%%)\n", n, nRejected, round(nRejected * 100 / n)))

  # Now look at non-rejected sessions  
  good <- scores[!scores$reject, ]
  cat(sprintf("From non-rejected sessions, %d total decisions on mimics\n", sum(good$mimics_total)))
}


#### 
# No longer query the online database, as ethics permission has expired
# Get the current results from the Firebase database. Results are stored in an Sqlite database locally
#updated <- MDbBringUpToDate(QueryFirebase)
#if (length(updated$session) > 0)
#  message(sprintf("Local database was updated with %d new sessions\n", length(updated$session)))
####

# Now query the local database to analyse the results and write output files
CreateOutputFiles()

ReportSessionsInfo()