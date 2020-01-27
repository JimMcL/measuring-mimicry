# Analysis of human predators to assess mimetic accuracy

This directory contains R scripts to analysis the results of the
`EatUp` web game for using humans as predators to assess mimetic
accuracy. While there is only 1 Rstudio project (in <code>EatUp/Human&nbsp;trials.Rproj</code>), logically there are 2 components plus some additional miscellaneous functionality.

1. The `EatUp` subdirectory contains a Shiny app for displaying realtime results of people playing the game. We have successfully used it at conferences and classes, where we get the audience to play the game, and then show them their collective results.

1. `Analysis.R` can be run (from the command line or from Rstudio) to generate output CSV files from the current state of the remote Google Firebase database. 