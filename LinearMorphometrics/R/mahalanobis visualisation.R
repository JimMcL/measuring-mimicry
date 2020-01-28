# Experiment to visualise what Mahalanobis distance calculates. Mahalanobis
# distance is used for calculating mimetic accuracy in linear morphometrics.
#
# Thois script plots random "models" which are clustered in an ellipse, then
# plots lots of points coloured by mahalanobis distance from model centroid, and
# draws contours of distance from centroid.
#
# Mahalanobis distance is then visually compared to a metric (applied to the
# same data) that simply minimises the ratio y/x, such as might be used in a
# trait table.

library(colorspace)

complexToCartesian <- function(cx) {
  matrix(data = c(Re(cx), Im(cx)), ncol = 2)
}

# Displays a colour gradient in a legend.
# 
# Based on https://stackoverflow.com/a/58189259/1287461.
# 
# @param ... Additional parameters passed to legend. At a minimum, x must be specified.
# @param colours Colours representing the gradient.
# @param labels Legend labels from top to bottom.
# @param y.intersp Passed directly to legend. Defines how tightly colours are packed together.
GradientLegend <- function(..., palette, labels = c("1.0", "0.0", "-1.0"), y.intersp = 0.2) {
  # Create legend labels. Empty spaces are NAs, just stick the specified labels
  # into the vector at appropriate indices
  n <- length(palette)
  leg <- rep(NA, n)
  # Intersperse the labels evenly along the legend
  labelIndices <- seq(1, n, by = (n - 1) / (length(labels) - 1))
  leg[labelIndices] <- labels
  # Draw it
  legend(...,
         legend = leg,
         fill = palette,
         border = NA,
         y.intersp = y.intersp)
}

color.gradient <- function(x, palette) {
  palette[findInterval(x, seq(min(x), max(x), length.out = length(palette)))]
}

# Number of models
n <- 100
x <- rnorm(n, sd = 1, mean = -1)
y <- rnorm(n, sd = 1.5)
# Ensure all values >0 to emulate physical measurements
x <- x - min(x) + 10
y <- y - min(y) + 20
# Rotate
c <- complex(real = x, imaginary = y)
model <- complex(modulus = Mod(c), argument = Arg(c) - .8)

# Create a grid of mimics
xr <- extendrange(Re(model), f = .5)
yr <- extendrange(Im(model), f = .5)
ncols <- 100
mx <- seq(xr[1], xr[2], length.out = ncols)
my <- seq(yr[1], yr[2], length.out = ncols)
# Create data frame which consists of all pairs of x & y
mimic <- data.frame(x = rep(mx, each = ncols),
                    y = rep(my, times = ncols))

# Calculate mahalanobis distance of each mimic from model centroid
centre <- c(mean(Re(model)), mean(Im(model)))
cov <- var(complexToCartesian(model))
md <- mahalanobis(mimic, centre, cov)

# Calculate an alternate accuracy metric, simply the ratio of y to x. This kind
# of measure is used by e.g. the trait table to test for thin legs
ratio <- mimic$y / mimic$x

palName <- "Heat"
palRev <- TRUE
pal <- sequential_hcl(100, palette = palName, rev = palRev)

# Plot a single distance metric with colour gradient and contours.
.plotMetric <- function(accuracy, ...) {
  plot(mimic, col = color.gradient(accuracy, pal), pch = 16, asp = 1,
       xlab = "Trait 1", ylab = "Trait 2", ...)
  points(model, pch = 16, col = "black", cex = .8, asp = 1)
  # Don't label contours because the actual values are meaningless
  contour(mx, my, z = matrix(accuracy, nrow = ncols, byrow = TRUE), add = TRUE, drawlabels = FALSE)
  GradientLegend("bottomleft", labels = c("Better", "Worse"),
                 palette = sequential_hcl(31, palette = palName, rev = palRev))
}
  
# Minimising ratio y/x
par(mfrow = c(1, 2))
.plotMetric(ratio, main = "Minimal y/x ratio")

# Mahalanobis distance
.plotMetric(md, main = "Mahalanobis distance")

