# Builds a CSV describing photos from my sampleIt database.
# Output file is photo_info.csv.

library(tools)
source("../../../../../apps/sampleIt/API/R/sampleIt.R")

# Get a list of images used in the website
photos <- list.files("../../EatUp/images", pattern = "^[0-9]+.jpg", ignore.case = TRUE)
ids <- file_path_sans_ext(photos)

# Query the database
#TODO seems to be a bug in sampleIt, this doesn't work!
#ph <- SIQueryPhotos(sprintf("ftype=photo&photos.id=[%s]", paste(ids, collapse=',')))
ph <- SIQueryPhotos("ftype=photo")
ph <- ph[ph$id %in% as.integer(ids), ]

# Get specimen info for each photo
sp <- SIQuerySpecimensForPhotos(ph)

# Join photos and specimens
data <- merge(sp, ph, by.x = "id", by.y = "imageableid")

# Convert URL to the form reported by the website
data$webUrl <- sprintf("images/%d.jpg", data$id.y)

SpecimenIsMimic <- function(sp) {
  grepl("mimic", sp$other, ignore.case = TRUE)
}

# Derive type
data$isMimic <- SpecimenIsMimic(data)
write.csv(data[, c("family", "class", "species", "angle", "webUrl", "isMimic")], file = "photo_info.csv", row.names = FALSE)
