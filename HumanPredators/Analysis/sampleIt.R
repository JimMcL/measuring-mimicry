# Functionality for interfacing with my sampleIt database

SAMPLEIT_DIR <- "../../../../../apps/sampleIt/API/R"
SAMPLEIT_API <- file.path(SAMPLEIT_DIR, "sampleIt.R")
source(SAMPLEIT_API)


# Returns a list of species found in the Sydney region
GetSydneySpeciesNames <- function() {
  # Rough spatial query
  sp <- SIQuerySpecimens("bounds=-34.1128,150.56406,-33.42786,151.38803")
  unique(sp$species)
}
