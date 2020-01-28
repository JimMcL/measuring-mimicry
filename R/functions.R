library(colorspace)

##############################################################
# Functions to plot a network diagram using classical multidimensional scaling.


# Displays a colour gradient in a legend.
# 
# Based on https://stackoverflow.com/a/58189259/1287461.
# 
# @param ... Additional parameters passed to legend. At a minimum, x must be specified.
# @param colours Colours representing the gradient.
# @param labels Legend labels from top to bottom.
# @param y.intersp Passed directly to legend. Defines how tightly colours are packed together.
GradientLegend <- function(..., colours = diverge_hcl(21), labels = c("1.0", "0.0", "-1.0"), y.intersp = 0.2) {
  # Create legend labels. Empty spaces are NAs, just stick the specified labels
  # into the vector at appropriate indices
  n <- length(colours)
  leg <- rep(NA, n)
  # Intersperse the labels evenly along the legend
  labelIndices <- seq(1, n, by = (n - 1) / (length(labels) - 1))
  leg[labelIndices] <- labels
  # Draw it
  legend(...,
         legend = leg,
         fill = colorRampPalette(colors = colours)(n),
         border = NA,
         y.intersp = y.intersp)
}

# Plots the network implied by a correlation matrix using multidimensional scaling.
# 
# @param borderWidth If > 0, uses a crude hack to try to outline the edges in the network.
# @param borderColor Colour of edge borders.
MyPlotNetwork <- function(cor, xFactor = 0.05, yFactor = 0.05, labelPos = 1, leg.cex = 1, borderWidth = NA, borderColour = "black") {
  points <- cmdscale(1 - cor, k = 2)
  x <- points[,1]
  y <- points[,2]
  par(mar = c(0, 0, 0, 1.2) + .1)
  plot(x, y, xlim = extendrange(x, f = xFactor), ylim = extendrange(y, f = yFactor), asp = 1,
       type = "n", axes = FALSE, xaxt = "n", yaxt = "n", xlab = "", ylab = "")
  
  # Get correlations as a number between 0 and 1 inclusive
  .scale <- function(n, mi = min(n, na.rm = TRUE), ma = max(n, na.rm = TRUE)) (n - mi) / (ma - mi)
  scaled <- cor
  scaled[scaled == 1] <- NA
  # Line widths are a function of this
  wsc <- .scale(scaled)

  # Make colour represent correlation relative to [-1, 1]
  csc <- .scale(scaled, -1, 1)
  nColours <- 21
  pal <- diverge_hcl(nColours, h = c(240, 0), c = 100, l = c(65, 97), rev = TRUE)
  
  pairs <- combn(seq_len(nrow(points)), 2)
  apply(pairs, 2, function(p) segments(x[p[1]], y[p[1]], x[p[2]], y[p[2]], 
                                       lwd = borderWidth + 8 * wsc[p[1], p[2]], col = borderColour))
  apply(pairs, 2, function(p) segments(x[p[1]], y[p[1]], x[p[2]], y[p[2]], 
                                       lwd = 1 + 8 * wsc[p[1], p[2]],
                                       col = pal[round(csc[p[1], p[2]] * nColours)]))
  points(x, y, pch = 21, bg = "white", cex = 1.3)
  
  shadowtext(x, y, labels = colnames(cor), cex=1, pos = labelPos, col = "#303030", bg = "white", r = .2,
             theta = seq(0, 2 * pi, length.out = 100))
  GradientLegend("right", colours = rev(pal), bty = "n", inset = c(-0.04, 0), xpd = NA, cex = leg.cex)
}

##############################################################
# Miscellaneous plotting

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

DrawLMTexT <- function(l, side = 1, adj = .99, firstLine = 0, lineInc = 1, ...) {
  sl <- summary(l)
  t <- bquote(F[.(sl$fstatistic[2])][","][.(sl$fstatistic[3])] ~ "=" ~ .(signif(sl$fstatistic, 2)))
  # White background behind text
  #rect(par("usr")[2] - 1.2 * strwidth(t), par("usr")[3] + .01, par("usr")[2] - .01, par("usr")[3] + 4 * strheight(t), col = "white", border = NA)
  line <- firstLine
  mtext(bquote(R^2 ~ "=" ~ .(round(sl$r.squared, 2))), side, line, adj = adj, ...)
  line <- line + lineInc
  mtext(t, side, line, adj = adj, padj = 0.2, ...)
  line <- line + lineInc
  mtext(bquote(p ~ "=" ~ .(signif(sl$coefficients[2,4], 2))), side, line, adj = adj, ...)
}

##############################################################
# Boostrapping and randomisation

# Bootstrap 95% confidence interval of correlation coefficient
BootstrapCorCI <- function(x, y, nreps = 10000) {
  stopifnot(length(x) == length(y))
  
  rboot <- 0
  n <- length(x)
  for(i in 1:nreps) {
    samp <- sample(1:n, n, replace = TRUE)
    rboot[i] <- cor.test(x[samp], y[samp])$estimate
  }
  quantile(rboot, c(0.025, 0.975))
}

# Randomisation test (yields upper bound on p value, two sided test)
RandomisationCorTest <- function(x, y, nreps = 10000) {
  stopifnot(length(x) == length(y))
  
  rboot <- 0
  n <- length(x)
  for(i in 1:nreps) {
    samp <- sample(1:n, n, replace = FALSE)
    rboot[i] <- cor.test(x, y[samp])$estimate
  }
  sum(abs(cor.test(x,y)$estimate) < abs(rboot)) / nreps # approximate two-sided test
}

######################################################
# Tests

# # random bivariate data
# n <- 15
# x <- rnorm(n)
# y <- 3 * x + rnorm(n)
# 
# summary(lm(y ~ x))
# cor.test(x,y)
# BootstrapCorCI(x, y)
# RandomisationCorTest(x, y)
