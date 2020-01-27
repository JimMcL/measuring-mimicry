# NOT USED, see DownloadFirebase.R

###############################################
#### TODO Pagination of data from AdaFruit ####
###############################################

# Download latest human scores from the AdaFruit IO data store, and add them to the scores database

library(httr)
library(jsonlite)
source("db.R")

# ==== AdaFruit fetcher ====

AF_USERNAME <- "FruitJim"
AF_FEED_KEY <- "mimics"
AF_API_KEY <- "2464cd35a78145bf8c61b212e2b70723"
AF_PARAMS <- list(include = "value,created_at,id")

queryAdaFruit <- function(startTime = NULL) {
  url <- paste0("https://io.adafruit.com/api/v2/", AF_USERNAME, "/feeds/", AF_FEED_KEY, "/data")
  
  params <- AF_PARAMS
  if (!is.null(startTime))
    params$start_time <- startTime
  r <- GET(url, query = params, add_headers(`X-AIO-Key` = AF_API_KEY))
  h <- headers(r)
  if (http_status(r)$category != "Success")
    stop(paste("error querying data from AdaFruit IO:", http_status(r)$message))
  c <- content(r, "parsed")
  
  row <- fromJSON(c$value)
  # Date/time in ISO 8601 format which allows the use of string comparisons
  row$created_at <- c$created_at
  row$id <- c$id
  row
}

# collectRows <- function(type, data) {
#   # Collect elements with correct type
#   l <- data[sapply(data, function(j) fromJSON(j$value)$type == type)]
#   rows <- lapply(l, function(e) {
#     row <- fromJSON(e$value)
#     # Date/time in ISO 8601 format which allows the use of string comparisons
#     row$created_at <- e$created_at
#     row$id <- e$id
#     as.data.frame(row)
#   })
#   do.call(rbind, rows)
# }
# 
# rowExists <- function(db, table, id) {
#   if (!dbExistsTable(db, table)) {
#     FALSE
#   } else {
#     qry <- sprintf("SELECT count(*) FROM %s where id = ?", table)
#     param <- list(as.character(id))
#     dbGetQuery(db, qry, param = param)[[1]] > 0
#   }
# }
# 
# addRowsToDb <- function(db, type, data) {
#   rows <- collectRows(type, data)
#   affected <- 0
#   if (!is.null(rows) && nrow(rows) > 0) {
#     # Work out which rows aren't already in the database
#     newRows <- sapply(rows$id, function(id) {!rowExists(db, type, id)})
#     rows <- rows[newRows, ]
#     dbWriteTable(db, type, rows, append = TRUE)
#     affected <- nrow(rows)
#   }
#   if (affected == 0)
#     message(sprintf("No new rows in table %s", type))
#   else
#     message(sprintf("Wrote %d rows to table %s", affected, type))
# }
# 
# bringTablesUpToDate <- function(db) {
#   scoreTableExists <- dbExistsTable(db, TAB_SCORE)
#   # Sessions table should contain the oldest record, use it so as to be conservative
#   qry <- sprintf("SELECT MAX(created_at) from %s", TAB_SESSION)
#   startTime <- NULL
#   if (dbExistsTable(db, TAB_SESSION)) {
#     startTime <- dbGetQuery(db, qry)[[1]]
#   }
#   data <- queryAdaFruit(startTime)
#   for (tab in TABS_ALL) {
#     addRowsToDb(db, tab, data)
#   }
#   if (!scoreTableExists && dbExistsTable(db, TAB_SCORE))
#     CreateIndices(db)
# }
# 
# db <- GetDbs()
# bringTablesUpToDate(db)
# dbDisconnect(db)
# db <- NULL


# Note - untested after refactoring!
MDbBringUpToDate(queryAdaFruit)