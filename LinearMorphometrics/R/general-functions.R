# General purpose functions used by the linear morphometrics analysis.

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

# Function to calculate mimetic accuracy using Mahalanobis distance
#
# @param df Data set containing specimen trait measurements. Each row is a
#   specimen, each column a trait.
# @param dataCols Names of the columns in `df` which are to be included in the
#   calculation. All of the named columns must be numeric.
# @param modelIndices Vector, either row numbers or a boolean vector, used to
#   identify models within \code{df}.
# @param retain Proprotion of variance to retain after PCA. Any components not
#   required to represent this level of variance are not included in the
#   accuracy calculation.
CalcMimeticAccuracy <- function(df, dataCols = colnames(df), modelIndices, retain = .99) {
  
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
  models <- data[modelIndices, ]
  centre <- apply(models, 2, mean, na.rm = TRUE)
  cov <- var(models)
  
  # Calculate distance of each point to the centroid of the models
  md <- sqrt(mahalanobis(data, centre, cov))
  
  # Convert distance to accuracy
  1 - md / max(md)
}

