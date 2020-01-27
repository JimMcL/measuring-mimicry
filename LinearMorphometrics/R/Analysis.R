# Performs linear morphometric analysis to estimate mimetic accuracy for various ant mimics

DATA_DIR <- "../data"
OUTPUT_DIR <- "../output"
GLOBAL_OUTDIR <- "../../output"

##########################################################################
# Functions

# Replaces all NAs in a column with the mean value of the column. This means
# that cells that were NA are no longer NA, but don't contribute to variance
ReplaceNAs <- function(df, column, replacementValue = mean(df[,column], na.rm = TRUE)) {
  nas <- which(is.na(df[,column]))
  df[nas, column] <- replacementValue
  df
}

# Copies CSV files from one directory to another
CopyCsvs <- function(fromDir = OUTPUT_DIR, toDir = GLOBAL_OUTDIR) {
  # Now copy all CSV files to the global output directory
  csvs <- list.files(fromDir, ".*\\.csv$", full.names = TRUE)
  copied <- file.copy(csvs, toDir, overwrite = TRUE, copy.mode = TRUE, copy.date = TRUE)
  if (any(copied)) {
    cat(sprintf("Unable to copy file(s): %s\n", csvs[!copied]))
  }
}


##########################################################################
####
# Read in measurements of mimics and models

mimics <- read.csv(file.path(DATA_DIR, "mimics.csv"), skip = 1, stringsAsFactors = FALSE)
mimics$mimicType <- "mimic"
#TODO models <- read.csv(file.path(DATA_DIR, "models.csv"), skip = 1, stringsAsFactors = FALSE)
#TODO models$mimicType <- "model"
#TODO p <- rbind(mimics, models)
p <- mimics

# Create a species column (i.e. "Genus species")
p$species <- paste(p$Genus, p$Species)


#### 
# Work out which columns are measurements, and convert them to numeric

# Most columns are numeric, but are read as characters because they contain "No data"
p[p == "No data"] <- NA

# Convert to numeric
numericCols <- grep("width|height|length", names(p), value = TRUE, ignore.case = TRUE)
# We don't want ratio columns as they contain derived values, we will just work with the raw data
numericCols <- numericCols[!grepl("ratio", numericCols, ignore.case = TRUE)]
for (c in numericCols) {
  if (any(na.omit(p[[c]] == "N/A")))
    warning(sprintf("N/A value(s) in column %s, row(s) %s", c, paste(which(p[[c]] == "N/A"), collapse = ", ")))
  p[[c]] <- as.numeric(p[[c]])
}

####
# Calculate Mahalanobis distance

# Change any NAs to the mean of the column. The alternative is to throw out any rows containing NAs
for (c in numericCols) {
  p <- ReplaceNAs(p, c)
}

# Start with PCA. This eliminates contant dimensions which would stop the
# Mahalanobis distance calculation from working. We retain almost all the
# variation in the dataset (i.e. keep 99%)
retain <- .99

# Note that Penney et al. (2012) used a GCDA (discriminating between species)
# instead of a PCA at this step. They then calculated Mahalanobis distances
# using the first 3 canonical variates (axes), which accounted for only 80.6% of
# the variation between species. A GCDA is related to a PCA, but it maximises
# distances between species, while minimising variation within species. However,
# Penney et al. (2012) did not fully explain their reasoning for choosing to
# apply a GCDA - it seems to imply that predators are trying to differentiate
# between species rather than between mimics and models.

pca <- prcomp(reformulate(numericCols), data = p, scale. = FALSE)
# Calculate cumulative proportions explained by each component
prop <- cumsum(pca$sdev^2 / sum(pca$sdev^2))
# How many components do we need to retain the desired proprtion of variation?
compToRetain <- which(prop >= retain)[1]
# Truncate the dataset to the desired columns
data <- as.data.frame(pca$x[, 1:compToRetain])

# Calculate centre and covariance of all models (i.e. ants). This is required
# for calculating Mahalanobis distance
#TODO models <- data[p$mimicType == "model", ]
#TODO For now, just randomly pick some rows to be models
models <- data[sample(nrow(p), 10), ]  # TODO delete
centre <- apply(models, 2, mean, na.rm = TRUE)
cov <- var(models)

# Calculate distance of each point to the centroid of the models
md <- sqrt(mahalanobis(data, centre, cov))

# Convert to accuracy
p$accuracy <- 1 - md / max(md)

# Output the accuracy per specimen
if (!dir.exists(OUTPUT_DIR))
  dir.create(OUTPUT_DIR)
write.csv(p, file.path(OUTPUT_DIR, "Linear morphometrics-accuracy-individuals.csv"), row.names = FALSE)

# Average specimen accuracies to obtain species accuracies
# Note that I'm keeping the constructed "species column, not the original "Species" column which is specific epithet
a <- aggregate(p[, c(numericCols, "accuracy")], list(p$Family, p$Genus, p$species, p$mimicType), mean)
names(a) <- c("Family", "Genus", "species", "mimicType", numericCols, "accuracy")
write.csv(a, file.path(OUTPUT_DIR, "Linear morphometrics-accuracy-species.csv"), row.names = FALSE)

# Now copy all CSV files to the global output directory
CopyCsvs()