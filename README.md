---
title: "Methods for measuring mimicry"
output: html_document
---

This project contains data and scripts that implement multiple methods
for evaluating the visual accuracy of mimics. Each of the methods
produce one or more CSV files as output. All output CSV files are
accessible in the `output` directory of this project (as described
below). The columns in the CSV files are format-specific, but
always contain a `species` and an `accuracy` column.

The methods are described in the paper "Mick's Mimicry Methods",
Kelly et al. 2020. If you make use of any of this functionality in your
research, please cite the paper.

We envisage that methods would mostly be treated as templates, that
is, the methods source code downloaded and then modified appropriately
for the task at hand. 

# Implementation
Most code in this project is in `R`, however the machine learning module includes a `Ruby` script, and the human predators module web site is implemented in `HTML`, `CSS` and `JavaScript`.

# Directory structure
Each method has a directory containing method-specific data and code. Most method directories contain a `README` file, and `data`, `output` and `R` subdirectories. In addition to the method-specific directories, there is a global output directory `output`, which contains the results of mimetic accuracy calculations for each of the methods, and a global analysis directory `R` which contains R scripts (and an Rstudio project) to compare the results from the different methods.

## Method-specific directories
- `HumanPredators` contains a web application (called `EatUp`) for measuring human assessments of mimetic accuracy based on photos, and an R project for analysing the web application data. [More info](HumanPredators/README.html).
- `MachineLearning` contains scripts and output for machine learning assessment of mimetic accuracy. It is applied to the same photos as used by the human predators method. [More info](MachineLearning/README.html).
- `TraitTable` contains a manually created table of trait values, and a simple R script to convert the trait table to the common format output by other methods. [More info](TraitTable/README.html).
- `LinearMorphometrics` contains trait measurement data for mimics and models, and an `R` script to perform a multivariate comparison of mimics to models. [More info](LinearMorphometrics/README.html).
- `GeometricMorphometrics` contains data and `R` code for statistically analysing and comparing shapes (in this case, the body outlines of ant mimics and ants).  [More info](GeometricMorphometrics/README.html).

## Global directories

- `output` contains data produced by each of the methods as well as the global method comparison analysis.
- `R` contains code for comparing the outputs of the various methods.

## Output CSV structure

For each method, there will be one or more CSV files in the `output` directory. `<method>-accuracy-species.csv` exists for every method.

| Column      | Description | Included |
| ------      | ----------- | -------- |
| `species`   | Species (or morphospecies) name | Always |
| `accuracy`  | Mimetic accuracy, larger numbers are more accurate | Always |
| `mimicType` | One of `mimic`, `model`, or `non-mimic` | If absent, all rows have an assumed value of `mimic` |
| `angle`     | One of `Dorsal`, `Lateral` | Present in files named `<method>-accuracy-species-angle.csv` |

## R Environment
<!-- Output from devtools::session_info() -->
Following is the environment used to develop this project. man (most?) of the packages are not used by this project.

```
 Session info -------------------------------------------------------------------------------------------------
 setting  value                       
 version  R version 3.6.1 (2019-07-05)
 os       Windows 10 x64              
 system   x86_64, mingw32             
 ui       RStudio                     
 language (EN)                        
 collate  English_Australia.1252      
 ctype    English_Australia.1252      
 tz       Australia/Sydney            
 date     2020-01-26                  

- Packages -----------------------------------------------------------------------------------------------------
 package       * version date       lib source                        
 assertthat      0.2.1   2019-03-21 [1] CRAN (R 3.6.1)                
 backports       1.1.4   2019-04-10 [1] CRAN (R 3.6.0)                
 callr           3.3.1   2019-07-18 [1] CRAN (R 3.6.1)                
 cli             1.1.0   2019-03-19 [1] CRAN (R 3.6.1)                
 colorspace    * 1.4-1   2019-03-18 [1] CRAN (R 3.6.1)                
 crayon          1.3.4   2017-09-16 [1] CRAN (R 3.6.1)                
 desc            1.2.0   2018-05-01 [1] CRAN (R 3.6.1)                
 devtools        2.1.0   2019-07-06 [1] CRAN (R 3.6.1)                
 digest          0.6.20  2019-07-04 [1] CRAN (R 3.6.1)                
 evaluate        0.14    2019-05-28 [1] CRAN (R 3.6.1)                
 fs              1.3.1   2019-05-06 [1] CRAN (R 3.6.1)                
 glue            1.3.1   2019-03-12 [1] CRAN (R 3.6.1)                
 htmltools       0.3.6   2017-04-28 [1] CRAN (R 3.6.1)                
 JUtils        * 0.1.0   2019-08-19 [1] Github (JimMcL/JUtils@ab1b341)
 knitr           1.23    2019-05-18 [1] CRAN (R 3.6.1)                
 magrittr        1.5     2014-11-22 [1] CRAN (R 3.6.1)                
 memoise         1.1.0   2017-04-21 [1] CRAN (R 3.6.1)                
 packrat         0.5.0   2018-11-14 [1] CRAN (R 3.6.1)                
 pkgbuild        1.0.3   2019-03-20 [1] CRAN (R 3.6.1)                
 pkgload         1.0.2   2018-10-29 [1] CRAN (R 3.6.1)                
 prettyunits     1.0.2   2015-07-13 [1] CRAN (R 3.6.1)                
 processx        3.4.1   2019-07-18 [1] CRAN (R 3.6.1)                
 ps              1.3.0   2018-12-21 [1] CRAN (R 3.6.1)                
 R6              2.4.0   2019-02-14 [1] CRAN (R 3.6.1)                
 Rcpp            1.0.2   2019-07-25 [1] CRAN (R 3.6.1)                
 remotes         2.1.0   2019-06-24 [1] CRAN (R 3.6.1)                
 rlang           0.4.2   2019-11-23 [1] CRAN (R 3.6.2)                
 rmarkdown       1.14    2019-07-12 [1] CRAN (R 3.6.1)                
 rprojroot       1.3-2   2018-01-03 [1] CRAN (R 3.6.1)                
 rsconnect       0.8.15  2019-07-22 [1] CRAN (R 3.6.1)                
 rstudioapi      0.10    2019-03-19 [1] CRAN (R 3.6.1)                
 sessioninfo     1.1.1   2018-11-05 [1] CRAN (R 3.6.1)                
 TeachingDemos * 2.10    2016-02-12 [1] CRAN (R 3.6.2)                
 testthat        2.2.1   2019-07-25 [1] CRAN (R 3.6.1)                
 usethis         1.5.1   2019-07-04 [1] CRAN (R 3.6.1)                
 withr           2.1.2   2018-03-15 [1] CRAN (R 3.6.1)                
 xfun            0.8     2019-06-25 [1] CRAN (R 3.6.1)                
 yaml            2.2.0   2018-07-25 [1] CRAN (R 3.6.0)                
```