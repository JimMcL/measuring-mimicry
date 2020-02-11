# Functions used by the linear morphometrics analysis.

DATA_DIR <- "../data"
OUTPUT_DIR <- "../output"
GLOBAL_OUTDIR <- "../../output"

# This vector defines 2 things - the set of column names to analyse in the
# mimics data, and how the model data column names map to the mimic column
# names. Vector values are model column names, vector names are model column
# names. In other words, model trait on the left, mimic trait on the right
COLUMN_MAP <- c(
  # Model column                   Mimic column
  "Unique.ID"                  = "Unique.I.D.",
  "Family"                     = "Family",
  #"Subfamily"                  = ,
  "Genus"                      = "Genus",
  "Species"                    = "Species",
  "Leg.II.Femur..width"        = "Leg.III.Femur..width",
  "Leg.II.Femur..length"       = "Leg.III.Femur..length",
  "Head..width"                = "Prosoma..width",
  "Head...Thorax..length"      = "Prosoma..length",
  "Gaster..width"              = "Opisthosoma..width",
  "Gaster..height"             = "Opisthosoma.height",
  "Total.Abdomen..length..gaster...postpetiole." = "Opisthosoma..length",
  "Petiole.length"             = "Pedicel..length",
  "Total.body.length"          = "Body.length",
  "Head..height"               = "Prosoma.height",
  "Improved.colour.score"      = "Improved.colour.score", # Derived value for models
  "Neck..constriction..height" = "Constriction.height", 
  "Neck..constriction..width"  = "Constriction.width",
  "Between.gaster...postpetiole..constriction..height" = "Constriction.height.1",
  "Between.gaster...postpetiole..constriction..width..i.e..petiole.width." = "Constriction.width.1",
  #"Mimic.Accuracy"             = "Mimic.Accuracy",
  "mimicType"                  = "mimicType"
)

if (any(duplicated(names(COLUMN_MAP))))
  stop(sprintf("Duplicate names in COLUMN MAP: %s", paste(names(COLUMN_MAP)[duplicated(names(COLUMN_MAP))], collapse = ", ")))
if (any(duplicated(COLUMN_MAP)))
  stop(sprintf("Duplicate values in COLUMN MAP: %s", paste(COLUMN_MAP[duplicated(COLUMN_MAP)], collapse = ", ")))


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


#############################################################################
# Accuracy calculation

# Normalises length values in the data frame by dividing by a reference value.
#
# This step is performed so that linear morphometrics are independant of body
# size. 
#
# @param df Data frame to normalise.
# @param lengthCols Names of columns to be normalised.
# @param refCol Name of the column to normalise by.
#
# @return \code{df} with length columns normalised.
NormaliseLengths <- function(df, lengthCols, refCol) {
  # Normalise all lengths by dividing by prosomoa/head width. This means that size itself is not a factor in mimetic accuracy
  df[, lengthCols] <- df[, lengthCols] / df[[refCol]]
  # Remove the reference column since it's no longer informative
  cols <- names(df)
  df <- df[, cols[cols != refCol]]
  df
}

# Changes model trait names to match mimic trait names.
# Only relevant traits are mapped
MapModelColsToMimicCols <- function(models) {
  # Need to derive some values since there's no direct equivalent in ants and spiders
  
  # In order to compare Improved.colour.score for mimics, we need to give models a
  # corresponding value. Since the improved colouration is implicitly making it
  # mimics ant-like, give ants a score of 1
  models$Improved.colour.score <- 1
  
  # First select only the columns of interest, and in the correct order
  models <- models[, names(COLUMN_MAP)]
  # Now rename the columns to the mimic equivalent
  setNames(models, COLUMN_MAP)
}

# Function to calculate mimetic accuracy using Mahalanobis distance
#
# @param df Data set containing specimen trait measurements. Each row is a
#   specimen, each column a trait.
# @param dataCols Names of the columns in `df` which are to be included in the
#   calculation. All of the named columns must be numeric.
# @param retain Proprotion of variance to retain after PCA. Any components not
#   required to represent this level of variance are not included in the
#   accuracy calculation.
CalcMimeticAccuracy <- function(df, dataCols = colnames(df), retain = .99) {
  
  # Change any NAs to the mean of the column. The alternative is to throw out any rows containing NAs
  for (c in dataCols) {
    df <- ReplaceNAs(df, c)
  }
  
  # Start with PCA. This eliminates contant dimensions which would stop the
  # Mahalanobis distance calculation from working. By default, we retain almost
  # all the variation in the dataset (i.e. keep 99%)
  
  # Note that Penney et al. (2012) used a GCDA (discriminating between species)
  # instead of a PCA at this step. They then calculated Mahalanobis distances
  # using the first 3 canonical variates (axes), which accounted for only 80.6% of
  # the variation between species. A GCDA is related to a PCA, but it maximises
  # distances between species, while minimising variation within species. However,
  # Penney et al. (2012) did not fully explain their reasoning for choosing to
  # apply a GCDA - it seems to imply that predators are trying to differentiate
  # between species rather than between mimics and models.
  
  pca <- prcomp(reformulate(dataCols), data = df, scale. = FALSE)
  
  # Calculate cumulative proportions explained by each component
  prop <- cumsum(pca$sdev^2 / sum(pca$sdev^2))
  # How many components do we need to retain the desired proportion of variation?
  compToRetain <- which(prop >= retain)[1]
  # Truncate the dataset to the desired columns
  data <- as.data.frame(pca$x[, 1:compToRetain])
  
  # Calculate centre and covariance of all models (i.e. ants). This is required
  # for calculating Mahalanobis distance
  models <- data[df$mimicType == "model", ]
  centre <- apply(models, 2, mean, na.rm = TRUE)
  cov <- var(models)
  
  # Calculate distance of each point to the centroid of the models
  md <- sqrt(mahalanobis(data, centre, cov))
  
  # Convert distance to accuracy
  1 - md / max(md)
}

