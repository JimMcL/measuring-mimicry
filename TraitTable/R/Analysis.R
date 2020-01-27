# Reads in raw trait table data, averages to species values, changes column
# names to match those of the other methods in this project, and writes out the
# result as CSV

OUT_CSV <- "../output/Trait table-accuracy-species.csv"
GLOBAL_OUTDIR <- "../../output"

# 2nd line contains proper headers
raw <- read.csv("../data/Raw data table.csv", skip = 1, stringsAsFactors = FALSE)

# Convert values to numeric so they can be averaged
raw[raw == "No data"] <- NA
raw[raw == "N/A"] <- NA

# Most column are numeric, but are read as characters because they contained "No data"
# Convert to numeric
numericCols <- grep("width|height|ratio|length|accuracy", names(raw), value = TRUE, ignore.case = TRUE)
# Unfortunately, "ratio" matches "colouration"!
numericCols <- numericCols[!grepl("note", numericCols, ignore.case = TRUE)]
for (c in numericCols) {
  raw[[c]] <- as.numeric(raw[[c]])
}

# Take means of numeric columns for each species
a <- aggregate(raw[, numericCols], list(raw$Family, raw$Genus, raw$Species), mean, na.rm = TRUE)
names(a) <- c("Family", "Genus", "Species", numericCols)

# Calculate sample size for each species
ss <- sapply(seq_len(nrow(a)), function(i) sum(raw$Genus == a$Genus[i] & raw$Species == a$Species[i]))

# Create column names consistent with other methods (see ../README)
a <- cbind(species = paste(a$Genus, a$Species), accuracy = a$Mimic.Accuracy, n = ss, a)

# Write out the result
write.csv(a, OUT_CSV, row.names = FALSE)
# Copy to global output directory
copied <- file.copy(OUT_CSV, GLOBAL_OUTDIR, overwrite = TRUE, copy.mode = TRUE, copy.date = TRUE)
if (!copied) {
  cat(sprintf("Unable to copy file: %s\n", OUT_CSV))
}
