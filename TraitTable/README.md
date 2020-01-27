---
title: "Mimicry Quality Assessment Table (trait table)"
output: html_document
---


This directory contains data and basic scripts for the Mimicry Quality Assessment Table, or Trait Table. The spreadsheet `data/Raw data table.xlsx` contains the full compiled dataset. The file `data/Raw data table.csv` contains a single sheet ("`Raw data`") from the spreadsheet, manually exported in CSV format.

The trait table requires no processing beyond the accuracy formula embedded in the spreadsheet. The R script `Analysis.R` in the `R` subdirectory functions to convert the trait table data into a format compatible with the other methods in this project. It reads in the `data/Raw data table.csv`, averages values for species, and outputs `output/Trait table-accuracy-species.csv`. It also copies the output file into the global output directory, `../../output`. The script can be run from the command line or from Rstudio.