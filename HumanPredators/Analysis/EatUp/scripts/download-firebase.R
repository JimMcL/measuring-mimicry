# Functions to download data from the Firebase cloud database.
# We use the realtime Database, NOT Cloud Firestore.

#library(fireData)

# ==== Firebase fetcher ====

API_KEY <- Sys.getenv("API_KEY")
DB_URL <- Sys.getenv("DATABASE_URL")
PROJECT_ID <- Sys.getenv("PROJECT_ID")
PROJECT_DOMAIN <- Sys.getenv("AUTH_DOMAIN")

userEmail <- Sys.getenv("USER_EMAIL")
userPassword <- Sys.getenv("USER_PASSWORD")
DB_DOCUMENT <- "mimic-scores"

.getFBUrl <- function() {
  url <- paste0(DB_URL, "/", DB_DOCUMENT, ".json")

  # Firebase is set up so that only this user is allowed to read the data
  user <- auth(API_KEY, email = userEmail, password = userPassword)
  url <- paste0(url, "?", "auth=", user$idToken)
}

# Copied from the fireData package which no longer installs on my system (3 Aug 2019).
# https://github.com/Kohze/fireData
# 
#' @title The user authentication function:
#' @description fireData::auth checks the validity of a login and returns the temporary JWT user token. FireData_auth can be used to store individual user data in specified directories that are only accessible to that specific user.
#' @param projectAPI The Firebase Project API {string}
#' @param email The user email {string}
#' @param password The user password {string}
#' @return Returns the content of the firebase API request, such as the state of registration, idToken, and validity of the user password.
#' @export
#' @examples
#' \dontrun{
#' auth(projectAPI = "AIzaSyAjZLO9-CRV3gObpwdFz-k8AiTOxHSBmdc", email = "robin@kohze.com", password = "12341234")
#' }
auth <- function(projectAPI, email="prompt", password="prompt"){
  if (password == "prompt" && email == "prompt") {
    email <- readline(prompt = "Email: ")
    password <- readline(prompt = "Password: ")
    print(paste0("Connecting to",  project_api, ":"))
  }
  AuthUrl = paste0("https://www.googleapis.com/identitytoolkit/v3/relyingparty/verifyPassword?key=", projectAPI)
  userData = httr::POST(url = AuthUrl, body = list("email" = email, "password" = password, "returnSecureToken" = "True"), encode = "json")
  return(httr::content(userData))
}

# Queries the Firebase database for any trial date as old as or newer than the specified start time.
# @return List of lists, each inner list is a "row" of data
QueryFirebase <- function(startTime = NULL) {
  
  url <- .getFBUrl()

  if (!is.null(startTime)) {
    url <- paste0(url, '&orderBy="created_at"&startAt="', startTime, '"')
  }
  
  # message(url)
  data = httr::GET(url)
  
  content <- jsonlite::fromJSON(httr::content(data,"text"))
  if (length(content) == 0 || is.null(content)) 
    stop(sprintf("No data found in Firebase location %s", DB_DOCUMENT))
  if (!is.null(content$error))
    stop(paste("Unable to query Firebase", content$error))
  
  # Give each element an ID value
  for(id in names(content)) {
    content[[id]]$id <- id
  }
  content
}

# Eg of usage
# fb <- QueryFirebase("2019-12-24T14:20:52.071Z")

