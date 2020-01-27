# Plots a list of densities as lines
JPlotDensities <- function(densities, cols = 1:length(densities), fillCols = NULL, lty = 1, lwd = 2, add = FALSE, xlim = NULL, ylim = NULL, includeInX = numeric(0), ...) {
  
  # Create empty plot
  if (!add) {
    if (is.null(xlim))
      xlim <- range(lapply(densities, function(d) d$x), na.rm = TRUE)
    xlim <- range(c(xlim, includeInX))
    if (is.null(ylim))
      ylim <- range(lapply(densities, function(d) d$y), na.rm = TRUE)
    plot(NULL, xlim = xlim, ylim = ylim, ...)
  }
  
  # Recycle lty if it's a single number
  if (length(lty) == 1)
    lty <- rep(lty, length.out = length(densities))
  
  # Optionally fill shapes first
  if (!is.null(fillCols)) {
    i <- 1
    for(d in densities) {
      if (!is.na(fillCols[i]))
        polygon(d, col = fillCols[i], border = NA)
      i <- i + 1
    }
  }
  
  # Plot densities as lines
  i <- 1
  for(d in densities) {
    lines(d, col = cols[i], lty = lty[i], lwd = lwd)
    i <- i + 1
  }
}

