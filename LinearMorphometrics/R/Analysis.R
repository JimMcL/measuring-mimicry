# Performs linear morphometric analysis to estimate mimetic accuracy for various ant mimics

source("general-functions.R")
source("ant-mimic-functions.R")

##########################################################################
####
# Read in measurements of mimics and models

mimics <- read.csv(file.path(DATA_DIR, "Mimics.csv"), skip = 1, stringsAsFactors = FALSE)
mimics$mimicType <- "mimic"

models <- read.csv(file.path(DATA_DIR, "Models.csv"), skip = 0, stringsAsFactors = FALSE)
models$mimicType <- "model"

# Combine mimics and models into a single data frame. Discard any unused columns
# from mimics, and map model columns to mimic columns
p <- rbind(mimics[, COLUMN_MAP], MapModelColsToMimicCols(models))

# Create a species column (i.e. "Genus species")
p$species <- paste(p$Genus, p$Species)

# Work out which columns are measurements, and convert them to numeric.
# Most columns are numeric, but are read as characters because they contain the text "No data"
p[p == "No data"] <- NA

# Constriction columns contain "N/A" meaning no constriction. In
# CalcMimeticAccuracy, NAs are replaced with the mean value for the column they
# are in, which means they don't contribute to variation 
constrictionCols <- grep("constriction", names(p), value = TRUE, ignore.case = TRUE)
p[, constrictionCols][p[, constrictionCols] == "N/A"] <- NA

# Convert to numeric
numericCols <- grep("width|height|length|score", names(p), value = TRUE, ignore.case = TRUE)
for (c in numericCols) {
  if (any(na.omit(p[[c]] == "N/A")))
    warning(sprintf("N/A value(s) in column %s, row(s) %s", c, paste(which(p[[c]] == "N/A"), collapse = ", ")))
  p[[c]] <- as.numeric(p[[c]])
}

####
# Calculate accuracy

# We want to normalise all length measurements by expressing them in units of head width for each specimen.
# That way, we are comparing shape/proportions, not size.
# Columns to be normalised are all the length/width/height columns
normalCols <- grep("width|height|length", numericCols, value = TRUE, ignore.case = TRUE)
pn <- NormaliseLengths(p, normalCols, "Prosoma..width")
numericCols <- numericCols[numericCols != "Prosoma..width"]

pn$accuracy <- CalcMimeticAccuracy(p, numericCols, modelIndices = p$mimicType == "model", scale = TRUE, retain = .99)

#### 
# Save the results

# Output the accuracy per specimen
if (!dir.exists(OUTPUT_DIR))
  dir.create(OUTPUT_DIR)
write.csv(pn, file.path(OUTPUT_DIR, "Linear morphometrics-accuracy-individuals.csv"), row.names = FALSE)

# Take the average of specimen accuracies to obtain species accuracies
# Note that I'm keeping the constructed "species" column, not the original "Species" column which is specific epithet
a <- aggregate(pn[, c(numericCols, "accuracy")], list(p$Family, p$Genus, p$species, p$mimicType), mean)
names(a) <- c("Family", "Genus", "species", "mimicType", numericCols, "accuracy")
write.csv(a, file.path(OUTPUT_DIR, "Linear morphometrics-accuracy-species.csv"), row.names = FALSE)

# Now copy all CSV files to the global output directory
CopyCsvs()
