# Functions and data specific to our ant-mimic analysis

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

