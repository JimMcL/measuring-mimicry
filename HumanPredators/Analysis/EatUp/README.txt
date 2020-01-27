This directory contains an R Shiny app for presenting some simple analysis of humans scoring of mimics. It is particularly useful for showing groups of players how they scored.

Currently it is running on shinyapps.io at
https://jimmclean.shinyapps.io/EatUp/

photo_info.csv describes each photo, identified by its URL

Sessions/scores are downloaded and stored locally in an SQLITE databse (function MDbBringUpToDate). The database is created if it doesn't already exist.

