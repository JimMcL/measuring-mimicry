# Reads google vision accuracy score for images from the global output directory

#devtools::install_github("JimMcL/JUtils")
library(JUtils)
library(TeachingDemos)
source("functions.R")

DATA_DIR <- "../output"


# Returns a list of all method names with data in the output directory
GetAllMethodPrefixes <- function(dir = DATA_DIR) {
  csvs <- list.files(dir, "*.\\.csv")
  unique(gsub("-.*", "", csvs))
}

# Loads up all the species CSV files in the specified directory. Each data frame
# is given an extra column, which is the name of the method and is derived from
# the name of the CSV file. Returns a list of data frame.
# 
# @param methods only methods matching this pattern will be loaded
LoadAccuracies <- function(dir = DATA_DIR, methods = ".*", fileSuffix = "-accuracy-species.csv") {

  .loadAccuracy <- function(csv) {
    method <- gsub(fileSuffix, "", basename(csv))
    # Ensure there's a mimicType column
    d <- read.csv(csv, stringsAsFactors = FALSE)
    # Sanity check for required columns
    reqd <- c("species", "accuracy")
    missing <- !reqd %in% names(d)
    if (any(missing)) {
      stop(sprintf("Missing %s column in %s", paste(reqd[missing], collapse = ", "), csv))
    }
    if (!"mimicType" %in% names(d))
      # No such column implies all mimics
      d$mimicType <- "mimic"
    # Add a method column
    cbind(d, method)
  }
  
  
  # If methods is a list, turn it into a regex
  if (length(methods) > 1)
    methods <- paste(methods, collapse = "|")
  
  csvs <- list.files(dir, paste0(".*", fileSuffix), full.names = TRUE)
  csvs <- csvs[grep(methods, csvs)]
  
  a <- lapply(csvs, .loadAccuracy)
  # Name the list items with their method names
  names(a) <- sapply(a, function(df) df[1, "method"])
  a
}

CalcPairwiseCovariance <- function(name1, values1, name2, values2, subset = "All", ...) {
  
  # We only are interested in mimics
  values1 <- values1[values1$mimicType == "mimic", ]
  values2 <- values2[values2$mimicType == "mimic", ]
  
  compareRanks <- FALSE
  if (compareRanks) {
    values1$accuracy <- rank(values1$accuracy)
    values2$accuracy <- rank(values2$accuracy)
  }
  
  # Merge on species names. This means we only end up with rows (i.e. species) common to both datasets
  m <- merge(values1[, c("species", "accuracy")], values2[, c("species", "accuracy")], by = "species")
  acc1 <- m$accuracy.x
  acc2 <- m$accuracy.y
  if (nrow(m) < 5) {
    stop(sprintf("Too few species are common to %s and %s: %d\n", name1, name2, nrow(m)))
  }
  
  ci <- BootstrapCorCI(acc1, acc2, ...)

  pearson <- suppressWarnings(cor.test(acc1, acc2, method = "pearson"))
  # print(pearson)
  spearman <- suppressWarnings(cor.test(acc1, acc2, method = "spearman"))
  # print(spearman)
  
  rank1 <- rank(acc1, ties.method = "average")
  rank2 <- rank(acc2, ties.method = "average")
  kendall <- suppressWarnings(cor.test(rank1, rank2, method = "kendall"))
  # print(kendall)
  
  # Calculate regression to get adjusted r squared, p value
  l <- lm(acc1 ~ acc2)
  
  # For a discussion of what adjusted R squared is, see https://stats.stackexchange.com/a/63097
  # Note that names are modified by c if the value is named, so eg Spearman becomes Spearman.rho
  c(Pearson = pearson$estimate, 
    `R-squared` = summary(l)$r.square,
    Adj.R.squared = summary(l)$adj.r.squared,
    p = summary(l)$coefficients[2,4],
    ci.lower = ci[1],
    ci.upper = ci[2],
    n = length(acc1),
    Spearman = spearman$estimate,
    Spearman.p = spearman$p.value)
}

# Plots bootstrapped 95% confidence intervals of correlations between pairs of
# methods, NOT corrected for multiple comparisons. Each pair of methods is shown
# by two vertical lines, each drawn with the colour and line style of one of the
# two methods in the pair.
PlotCI <- function(df, subset, ylimExtra = c(-.1, 0)) {
  
  allMethods <- unique(c(df$method1, df$method2))
  .methodCol <- function(method) match(method, allMethods)
  .methodLty <- function(method) match(method, allMethods)
  
  n <- nrow(df)
  plot(NULL, 
       xlim = c(1, n + .15), ylim = range(c(df$`ci.lower.2.5%`, df$`ci.upper.97.5%`)) + ylimExtra,
       main = sprintf("95%% CI of pairwise correlations, %s shapes", subset),
       xaxt = "n", xlab = "", ylab = "Pearson's correlation")
  title(xlab = "Method pair",  line = 1)
  sapply(seq(ceiling(par()$usr[3] * 10) / 10, floor(par()$usr[4] * 10) / 10, .1), function(a) abline(a = a, b = 0, col = "#f0f0f0"))
  abline(a = 0, b = 0)
  lwd <- 3
  x <- 1:n
  segments(x, df$`ci.lower.2.5%`, x, df$`ci.upper.97.5%`, col = .methodCol(df$method1), lty = .methodLty(df$method1), lwd = lwd)
  x <- x + .1
  segments(x, df$`ci.lower.2.5%`, x, df$`ci.upper.97.5%`, col = .methodCol(df$method2), lty = .methodLty(df$method2), lwd = lwd)
  legend("bottomleft", legend = allMethods, inset = c(0.01, 0.01), lwd = lwd, col = .methodCol(allMethods), lty = .methodLty(allMethods), seg.len = 4)
}

LoadAllAccuracies <- function(subset = c("All", "Dorsal", "Lateral")) {
  subset <- match.arg(subset)
  
  # Get a list of all method names
  methods <- GetAllMethodPrefixes()
  
  # Derive angle of interest and file name suffix from human-readable subset
  if (subset == "All") {
    fileSuffix <- "-accuracy-species.csv"
    fallbackSuffix <- NULL
  } else {
    fileSuffix <- "-accuracy-species-angle.csv"
    fallbackSuffix <- "-accuracy-species.csv"
  }
  
  acc <- LoadAccuracies(fileSuffix = fileSuffix)
  
  # Apply optional subset
  if (subset != "All") {
    # If we get to here, we know that only methods with angle-specific data are in the list
    acc <- lapply(acc, function(a) a[a$angle == subset, ])
  }
  
  # Are methods missing from this subset, e.g. MQAT doesn't have angle-specific file?
  missing <- methods[!methods %in% names(acc)]
  if (length(missing) > 0) {
    # Load up fallbacks, i.e. for those that don't have angle-specific accuracies, just use species accuracies
    acc <- c(acc, LoadAccuracies(methods = missing, fileSuffix = fallbackSuffix))
  }

  acc  
}

CompareAllMethods <- function(subset, alpha = 0.05, method = "spearman", ...) {

  acc <- LoadAllAccuracies(subset)
  
  # Build pairs of methods in the order that makes it easy to transcribe into Table 1
  n <- length(unique(names(acc))) - 1
  if (n != 4)
    stop("Unexpected methods in data")
  pairs <- expand.grid(col = 1:n, row = 1:n)
  # Remove pairs that are blank in the table (the lower-right diagonal)
  pairs <- pairs[rowSums(pairs) <= 5, ]
  # Replace indices with method names
  rows <- c("Machine learning", "Human predators", "Geometric morphometrics", "Trait table")
  cols <- c("Linear morphometrics", "Trait table", "Geometric morphometrics", "Human predators")
  if (!all(names(acc) %in% c(rows, cols)))
    stop("Unexpected methods in data")
  pairs <- data.frame(rows = rows[pairs$row], cols = cols[pairs$col], stringsAsFactors = FALSE)
  
  # Get indices of all pairs
  cc <- apply(pairs, 1, function(pair) {
    rowMethod <- pair[1]
    colMethod <- pair[2]
    CalcPairwiseCovariance(rowMethod, acc[[rowMethod]], colMethod, acc[[colMethod]], subset, method = method, ...)
    })
  cc <- as.data.frame(t(cc))
  row.names(cc) <- paste(pairs$rows, pairs$cols, sep = "-")
  cc$method1 <- pairs$row
  cc$method2 <- pairs$col
  
  # Adjust p-values for multiple comparisons, by controlling the false discovery
  # rate. This is more powerful than controlling the family-wise error rate,
  # such as e.g. Bonferroni correction
  cc$pAdj <- p.adjust(cc$Spearman.p, "BH")

  # For presentation, round to 2 digits and add a significance column
  #rep <- cbind(round(cc[, c("Adj.R.squared", "pAdj", "n", "Pearson.cor", "p")], 2), sig = ifelse(cc$pAdj < alpha, "*", ""))
  rep <- cbind(round(cc[, c("Spearman.rho", "pAdj", "n")], 2), sig = ifelse(cc$pAdj < alpha, "*", ""))
  
  # Full info
  cat(sprintf("Comparison of %s mimics\n", tolower(subset)))
  print(rep)
  ma <- which.max(rep$Spearman.rho)
  cat(sprintf("Strongest correlation: %s, rho = %g, p.adj = %g\n", rownames(rep)[ma], rep$Spearman.rho[ma], rep$pAdj[ma]))
  mi <- which.min(rep$Spearman.rho)
  cat(sprintf("Weakest correlation: %s, rho = %g, p.adj = %g\n", rownames(rep)[mi], rep$Spearman.rho[mi], rep$pAdj[mi]))
  
  # Or print out as a matrix
  #xtabs(round(Adj.R.squared, 2) ~ method1 + method2, data = cc)
  #xtabs(round(pAdj, 2) ~ method1 + method2, data = cc)
  
  # Or as a scatter plot
  # pch <- as.numeric(as.factor(cc$method2)) + 20
  # plot(pAdj ~ Adj.R.squared, data = cc, bg = as.factor(cc$method1), col = "#444444", pch = pch, cex = 2, 
  #      xlab = expression(R[adj]^2), ylab = expression(p[adj]))
  # leg <- c(expression(bold("Colours")), unique(cc$method1), expression(bold("Shapes")), unique(cc$method2))
  # pch <- c(NA, rep(21, 4), NA, 1:4 + 20)
  # col <- c(NA, 1:4, NA, rep(1, 4))
  # abline(a = alpha, b = 0, lty = 2, col = "grey") # alpha - statistical significance
  # legend("topright", legend = leg, pch = pch, pt.bg = col, col = "#444444", pt.cex = 1.6, inset = c(0.01, 0.01))
  
  # Or 95% CI as pairs of lines for each correlation
  PlotCI(cc, subset)
  
  invisible(cc)
}

# @param correlation Type of correlation to calculate. Default is the
#   non-parametric spearmans rho because the accuracy values are bounded, which
#   violates an assumption of pearson's correlation coefficient.
# @param p Parameter used to control network layout (Power for Minkowski
#   distance)
PlotCorNetwork <- function(subset, alpha = 0.05, xFactor = 0.05, yFactor = 0.05, leg.cex = 1, correlation = "spearman", p = .75) {
  
  # Load accuracies for all methods
  acc <- LoadAllAccuracies(subset)
  
  # Get data frames with 2 columns, species and method (which was named accuracy), stripping out all non-mimics
  methods <- names(acc)
  l <- lapply(methods, function(meth) {
    ma <- acc[[meth]]
    # Get desired columns for rows containing mimics
    ma <- ma[ma$mimicType == "mimic", c("species", "accuracy")]
    names(ma)[2] <- meth
    ma
    })
  # Merge them on species
  big <- Reduce(function(...) merge(..., all = TRUE), l)
  # Remove species column
  big$species <- NULL
  # Correlate
  # This doesn't give a good layout for our network, hence commented out; plot
  # using Classical (Metric) or else non-dimensional Multidimensional Scaling instead
  #cor <- corrr::correlate(big, method = "pearson", use = "pairwise.complete.obs", quiet = TRUE)
  #corrr::network_plot(cor, min_cor = .1, curved = TRUE, repel = TRUE)
  
  # Calculate correlations
  cor <- cor(big, method = correlation, use = "pairwise.complete.obs")
  # Plot correlations. The p value was selected through trial and error to look good
  MyPlotNetwork(cor, xFactor = xFactor, yFactor = yFactor, leg.cex = leg.cex, labelPos = c(1, 3, 1, 1, 1), p = p)
}

# Plots probability densities of the distributions of the accuracy scores for all methods
PlotMethodDensities <- function(subset = "all") {
  acc <- LoadAllAccuracies(subset)
  dl <- lapply(names(acc), function(m) density(acc[[m]]$accuracy, na.rm = TRUE))
  JPlotDensities(dl)
  abline(v = c(0, 1))
}

##########################################################################



CompareAllMethods("All")
#CompareAllMethods("Dorsal")
#CompareAllMethods("Lateral")

# Method correlation network diagram
# PNG suitable for embedding in a Word document
p <- .8
JPlotToPNG("../output/Figure_1.png",
           PlotCorNetwork("All", xFactor = 0.19, leg.cex = .7, p = p),
           units = "px", width = 900, height = 450, res = 160)
# Same diagram for publishing
JPlotToPDF("../output/Figure_1.pdf",
           PlotCorNetwork("All", xFactor = 0.2, leg.cex = .7, p = p))
