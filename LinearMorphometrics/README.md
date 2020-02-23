# Linear morphometrics to estimate mimetic accuracy


This directory contains R scripts and data to use linear morphometrics to
calculate the static visual mimetic accuracy of a variety of ant
mimics.

A number of phenotypic traits considered salient to ant mimicry were measured. Both mimics and ants were measured. The measurements are recorded in CSV files in the `data` subdirectory. The R scripts (in the `R` subdirectory) read in the measurements, then calculate the Mahalanobis distance  from each row (which represents a single specimen) to centroid of all the ant rows. The Mahalanobis distance measures the distance of a multivariate point to a set of points, taking the shape of the set into account. The Mahalanobis distance measures how similar the points are, so the shorter the distance from a point to the average ant shape, the more ant-like the point is. To convert Mahalanobis distance to an accuracy value, we first scaled it to the range `[0, 1]`, then subtracted the result from 1. The new number ranges from 0 to 1, with accuracy of 0 being least the  accurate mimics, and 1 being entirely ant-like.

## Input

- `data/mimics.csv` was copied from `../Trait table/Raw data table.csv`
- `data/models.xlsx` was created by measuring ant traits that we considered homologous or analagous to the traits measured in the mimics.
- `data/models.csv` was created by exporting `data/models.xlsx` as a CSV file.

## Processing

 - `R/Analysis.R` is an R script that reads the input data, analyses it and writes the output files. It can be run from within Rstudio or from the command line.

## Output
- `output/Linear morphometrics-accuracy-species.csv` - output file with row
    per species, columns are taxonomic details, whether the species is
    considered a mimic, mimetic accuracy, and the number of photos
    used to derive the accuracy
- `output/Linear morphometrics-accuracy-individuals.csv` - output file with row
    per measured specimen, columns are values from trait table measurements normalised to units of Promosa width, and linear morphometric accuracy in the column `accuracy`.
    

