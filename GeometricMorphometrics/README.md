---
title: "Geometric morphometrics to estimate mimetic accuracy"
output: html_document
---

This directory contains scripts and data to calculate the similarity
of body shapes between a selection of ant mimics and an "average" ant
body shape. This is used as a measure of mimetic accuracy. _Geometric
morphometrics_ is a broad term - here it is used as a technique to
characterise and compare shapes, which are the outlines of insects and
spiders. Shape comparisons often work by comparing _landmarks_, which
are homologous points on different shapes. Our shapes do not always
have homologous points, so instead we compare whole shapes.

Most of the morphometric calculations are performed by the `Momocs`
package (https://cran.r-project.org/web/packages/Momocs/index.html).

Step 1 is a manual process, which consists of obtaining dorsal and
lateral photographs of mimics and their models, and converting them to
silhouettes. All of the silhouette images must have consistent body
position and orientation within the image, and the same pixel
size. The result of step one on our data is the jpeg files in the
`data` subdirectory. Unfortunately, the Momocs package doesn't seem
able to handle PNG files. The `data` subdirectory also contains a CSV
file, `outlines.csv`, that defines the metadata (i.e. taxonomic
information, whether it is a mimic, etc.) for each outline.

Step 2 is to apply the morphometric analysis to the outlines. The `R`
subdirectory contains R scripts and an Rstudio project to perform the
analysis. The output is a set of CSV files in the `output`
subdirectory, which are also copied into the `../output`
directory. The `R/Analysis.R` script is the main script, and can be
run from the command line or from Rstudio.