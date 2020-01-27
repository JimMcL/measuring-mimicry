library(DBI)
library(jsonlite)
library(lubridate)

# Base data directory, contains scores database and photo_info CSV file
BASE_DIR <- "."


# ==== Schema ====

# CREATE TABLE `email` (
#  `type` TEXT,                -- always 'email'
#  `email` TEXT,               -- email address
#  `userId` TEXT,              -- random UUID identifies user
#  `sessionId` TEXT,           -- sessionId, joins session.sessionId
#  `created_at` TEXT,          -- creation date from data logger service or client (ISO 8601 format)
#  `id` TEXT                   -- record id from data logger service
# );
#
# CREATE TABLE `session` (
#  `type` TEXT,                -- always 'session'
#  `userId` TEXT,              -- random UUID identifies user (really browser/computer - there's no user info unless they enter an email address)
#  `firstTime` INTEGER,        -- True if this is the first time the user has played the game
#  `screenWidth` INTEGER,      -- Physical screen width
#  `screenheight` INTEGER,     -- Physical screen height
#  `devicePixelRatio` INTEGER, -- Ratio of CSS pixels to physical pixels
#  `userAgent` TEXT,           -- Browser user agent string
#  `sessionId` TEXT,           -- UUID that identifies this session
#  `created_at` TEXT,          -- creation date from data logger service or client (ISO 8601 format)
#  `id` TEXT                   -- record id from data logger service
# );
# 
# CREATE TABLE `score` (
#  `type` TEXT,                -- always 'score'
#  `imageUrl` TEXT,            -- URL of image being scored
#  `imageNumber` INTEGER,      -- Display number of image within session, first is 1
#  `score` TEXT,               -- 'ant', 'notAnt' or 'escape' (i.e. timeout)
#  `time` INTEGER,             -- user decision time (or else timeout time) in milliseconds
#  `sessionId` TEXT,           -- UUID of session, joins session.sessionId
#  `created_at` TEXT,          -- creation date from data logger service or client (ISO 8601 format)
#  `id` TEXT                   -- record id from data logger service
# );
# 
# ==== Database storage ====

TAB_SESSION <- "session"
TAB_SCORE <- "score"
TAB_EMAIL <- "email"
TABS_ALL <- c(TAB_SESSION, TAB_SCORE, TAB_EMAIL)

MDbGetDbs <- function(dir = NULL) {
  if (is.null(dir))
    dir <- BASE_DIR
  dbConnect(RSQLite::SQLite(), file.path(dir, "mimics.sqlite"))
}

MDbCreateIndices <- function(db) {
  dbExecute(db, "CREATE UNIQUE INDEX score_id ON score (id)")
  dbExecute(db, "CREATE UNIQUE INDEX session_id ON session (id)")
}

# ==== Date functions ====

FMT_ISO8601 = "%Y-%m-%dT%H:%M:%S"

# Converts a date to a character string in UTC.
# Note: to get a date in local time, try: ISOdatetime(2019, 6, 13, 0, 0, 0, tz = "")
DateToChar <- function(d) {
  if (is.character(d))
    d
  else 
    # Lubridate assumes UTC which should be ok with this data
    #ymd_hms(d)
    # I think this is better
    strftime(d, format = FMT_ISO8601, tz = "UTC")
}

# Needs work
DateToLocalStr <- function(d) {
  if (is.character(d))
    d <- strptime(d, "%Y-%m-%dT%H:%M:%S")
  as.character(d)
}

# ==== Query functions ====

# Calculates scores for each image


# Creates a WHERE clause to restrict rows to a date range
GetCreationClause <- function(table, from = NULL, to = NULL) {
  where <- ""
  addClause <- function(clause) paste0(where, ifelse(nchar(where) > 0, " AND ", ""), clause)
  
  if (!is.null(from))
    where <- addClause(sprintf("%s.created_at >= '%s'", table, DateToChar(from)))
  if (!is.null(to))
    where <- addClause(sprintf("%s.created_at < '%s'", table, DateToChar(to)))
  where
}

# Creates a WHERE clause to restrict rows to a set of IDs
GetIdClause <- function(table, ids, colName = "id") {
  if (is.null(ids) || length(ids) == 0)
    return("")
  sprintf("%s.%s IN (%s)", table, colName, paste(sprintf("'%s'", ids), collapse = ","))
}

AddToWhere <- function(expr, where = "", junction = "AND") {
  if (nchar(expr) > 0) {
    expr <- paste0("(", expr, ")")
    if (nchar(where) > 0)
      where <- paste(where, junction, expr)
    else
      where <- paste("WHERE", expr)
  }
  where
}

CombineWhereClauses <- function(..., junction = "AND") {
  clauses <- list(...)
  clauses <- Filter(function(s) s != "", clauses)
  if (length(clauses) == 0)
    ""
  else
    sprintf("(%s)", paste(clauses, collapse = paste0(" ", junction, " ")))
}

CreateWhere <- function(..., junction = "AND") {
  w <- CombineWhereClauses(..., junction = junction)
  if (nchar(w) > 0)
    w <- paste("WHERE", w)
  w
}

# Returns the table without the type column
MDbDropType <- function(data) {
  data[, -which(names(data) == "type")]
} 

# Returns raw scores for each image, i.e. number of "ant", "notAnt" and "escape" scores
MDbGetImageCounts <- function(db, sessionIds = NULL, from = NULL, to = NULL) {
  dbGetQuery(db, paste("SELECT imageUrl, score, count(*) AS count",
                       "FROM score", 
                       CreateWhere(GetIdClause("score", sessionIds, colName = "sessionId"), 
                                   GetCreationClause("score", from, to)),
                       "GROUP BY imageUrl, score"))
}

MDbGetImageScores <- function(db, from = NULL, to = NULL) {
  scores <- MDbGetImageCounts(db, from = from, to = to)
  sapply(unique(scores$imageUrl), function(image) {
    # Calculate ant-like score as number of ant or escape classifications on total number of classifications
    imgScores <- scores[scores$imageUrl == image, ]
    sum(imgScores[(imgScores$score == 'ant' | imgScores$score == 'escape'), "count"]) /
      sum(imgScores[, "count"])
  })
}

# Returns a complete WHERE clause for restricting session rows
MDbSessionWhere <- function(sessionIds = NULL, from = NULL, to = NULL, other = NULL, ...) {
  CreateWhere(
    GetIdClause("session", sessionIds, colName = "sessionId"), 
    GetCreationClause("session", from, to),
    other, 
    ...)
}

MDbGetSessions <- function(db, where = "", order = "ORDER BY session.created_at") {
  qry <- paste("SELECT *",
               "FROM session", 
               where,
               order)
  dbGetQuery(db, qry)
}

MDbGetSessionsWithScores <- function(db, where = "", order = "ORDER BY session.created_at, score.imageNumber") {
  qry <- paste("SELECT *",
               "FROM session", 
               "INNER JOIN score ON score.sessionId = session.sessionId", 
               where,
               order)
  dbGetQuery(db, qry)
}

MDbGetSessionScores <- function(db, sessionIds = NULL, from = NULL, to = NULL, where = NULL) {
  MDbGetSessionsWithScores(db, MDbSessionWhere(sessionIds, from, to))
}

MDbGetEmails <- function(db, where = "", order = "ORDER BY email.created_at") {
  dbGetQuery(db, paste("SELECT DISTINCT(email), created_at AS email FROM email", 
                       where,
                       order))
}

# ==== Download & update functions ====
# Functions to update the database with information from a logging service

MDbRowExists <- function(db, table, id) {
  if (!dbExistsTable(db, table)) {
    FALSE
  } else {
    qry <- sprintf("SELECT count(*) FROM %s where id = ?", table)
    param <- list(as.character(id))
    dbGetQuery(db, qry, param = param)[[1]] > 0
  }
}

.MDbcollectRows <- function(type, data) {
  # Collect elements with correct type
  l <- data[sapply(data, function(j) j$type == type)]
  do.call(rbind, lapply(l, function(r) as.data.frame(r)))
}

# Returns the number of rows added to the table
.MDbaddRowsToDb <- function(db, type, data) {
  rows <- .MDbcollectRows(type, data)
  affected <- character(0)
  if (!is.null(rows) && nrow(rows) > 0) {
    # Work out which rows aren't already in the database
    newRows <- sapply(rows$id, function(id) {!MDbRowExists(db, type, id)})
    rows <- rows[newRows, ]
    dbWriteTable(db, type, rows, append = TRUE)
    affected <- as.character(rows$id)
  }
  affected
}

# Brings the database up-to-date by downloading data from a logging service.
# @param queryFn Function with 1 parameter, startTime, date/time in ISO 8601
#   format. Returns a list of all records no older than the specified startTime.
# @returns vector with counts of rows added to tables
MDbBringUpToDate <- function(queryFn, dir = NULL) {
  .updateFn <- function(db) {
    scoreTableExists <- dbExistsTable(db, TAB_SCORE)
    # Sessions table should contain the oldest record, use it so as to be conservative
    qry <- sprintf("SELECT MAX(created_at) from %s", TAB_SESSION)
    startTime <- NULL
    if (dbExistsTable(db, TAB_SESSION)) {
      startTime <- dbGetQuery(db, qry)[[1]]
    }
    
    # Get data from logging service
    data <- queryFn(startTime)
    res <- sapply(TABS_ALL, function(tab) .MDbaddRowsToDb(db, tab, data))

    if (!scoreTableExists && dbExistsTable(db, TAB_SCORE))
      MDbCreateIndices(db)
  
    res  
  }

  MDbRun(.updateFn, dir = dir)
}

# Runs a function, passing in an open database as the first argument
MDbRun <- function(fun, ..., dir = NULL) {
  db <- MDbGetDbs(dir)
  tryCatch( {
    fun(db, ...)
  },
  finally = dbDisconnect(db)
  )  
}


##########################################################################

ReportImageScores <- function(dir = NULL, from = NULL, to = NULL) {
  MDbRun(function(db) print(MDbGetImageScores(db, from = from, to = to)))
}

#ReportSessions()
