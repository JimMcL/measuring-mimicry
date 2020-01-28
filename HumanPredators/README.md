# Human predators to assess mimetic accuracy


This folder contains an implementation of an online game using humans
as predators to assess the accuracy of ant mimics. Players are
presented with a sequence of photos of arthropods, which consist of
ants, ant-mimics and non mimics. Players are required to decide
whether each photo portrays an ant. Mimics are given an accuracy
score, which is simply the proportion of players who misclassified the
mimic as an ant.

There are two components:

1. The [`EatUp`](EatUp) subdirectory contains the online game itself. The game is implemented in HTML, CSS and
JavaScript, and uses Google Firebase for data storage.

2. The [`Analysis`](Analysis) subdirectory contains multiple R scripts for analysis of results. There are two analysis sub-components which share some common functionality.
   a. Static analysis which produces CSV files containing accuracy scores.
   b. A Shiny web app which can be used as an outreach tool to show groups of players their combined results in real-time. 

Refer to `README` files in the [EatUp](EatUp/README.html) and [Analysis](Analysis/README.html) subdirectories for more information.

Finally, the [`output`](output) subdirectory contains the output from this
method, which is a number of `CSV` files containing accuracy
scores. The CSV files are copied to the global output directory for
this project.

 * [`Human predators-accuracy-images.csv`](Human%20predators-accuracy-images.csv) contains accuracy scores for each image.
 * [`Human predators-accuracy-species-angles.csv`](Human%20predators-accuracy-species-angles.csv) contains accuracy scores averaged for each species and angle.
 * [`Human predators-accuracy-species.csv`](Human%20predators-accuracy-species.csv) contains accuracy scores averaged for each species.
