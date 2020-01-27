# Reads google vision accuracy score for images from the global output directory
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

CalcPairwiseCovariance <- function(name1, values1, name2, values2, subset = "All") {
  # We only are interested in mimics
  values1 <- values1[values1$mimicType == "mimic", ]
  values2 <- values2[values2$mimicType == "mimic", ]
  
  # Merge on species names. This means we only end up with rows (i.e. species) common to both datasets
  m <- merge(values1[, c("species", "accuracy")], values2[, c("species", "accuracy")], by = "species")
  acc1 <- m$accuracy.x
  acc2 <- m$accuracy.y
  if (nrow(m) < 5) {
    stop(sprintf("Too few species are common to %s and %s: %d\n", name1, name2, nrow(m)))
  }
  
  ci <- BootstrapCorCI(acc1, acc2)

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
  c(Pearson = pearson$estimate, 
    `R-squared` = summary(l)$r.square,
    Adj.R.squared = summary(l)$adj.r.squared,
    p = summary(l)$coefficients[2,4],
    ci.lower = ci[1],
    ci.upper = ci[2],
    n = length(acc1))
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

CompareAllMethods <- function(subset, alpha = 0.05) {

  acc <- LoadAllAccuracies(subset)

  # Get indices of all pairs
  pairs <- combn(seq_len(length(acc)), 2)
  cc <- apply(pairs, 2, function(pair) {
    method1 <- names(acc)[pair[1]]
    method2 <- names(acc)[pair[2]]
    CalcPairwiseCovariance(method1, acc[[pair[1]]], method2, acc[[pair[2]]], subset)
    })
  cc <- as.data.frame(t(cc))
  row.names(cc) <- apply(pairs, 2, function(pair) paste(names(acc)[pair[1]], names(acc)[pair[2]], sep = "-"))
  cc$method1 <- names(acc)[pairs[1,]]
  cc$method2 <- names(acc)[pairs[2,]]
  
  # Adjust p-values for multiple comparisons, by controlling the false discovery
  # rate. This is more powerful than controlling the family-wise error rate,
  # e.g. using Bonferroni correction
  cc$pAdj <- p.adjust(cc$p, "BH")

  # For presentation, round to 2 digits and add a significance column
  rep <- cbind(round(cc[, c("Pearson.cor", "Adj.R.squared", "p", "pAdj", "n")], 2), sig = ifelse(cc$pAdj < alpha, "*", ""))

  cat(sprintf("Comparison of %s mimics\n", tolower(subset)))
  # Order by method names
  rep <- rep[order(rownames(rep)), ]
  print(rep, 2)
  
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
}

PlotCorNetwork <- function(subset, alpha = 0.05, xFactor = 0.05, yFactor = 0.05, leg.cex = 1) {
  
  # Load accuracies for all methods
  acc <- LoadAllAccuracies(subset)
  
  # Get data frames with 2 columns, species and method
  l <- lapply(names(acc), function(meth) {
    ma <- acc[[meth]][, c("species", "accuracy")]
    names(ma)[2] <- meth
    ma
    })
  # Merge them on species
  big <- Reduce(function(...) merge(..., all = TRUE), l)
  # Remove species column
  big$species <- NULL
  # Correlate
  # This doesn't give a good layout for our network, hence commented out; plot
  # using Classical (Metric) Multidimensional Scaling instead
  #cor <- correlate(big, method = "pearson", use = "pairwise.complete.obs", quiet = TRUE)
  #network_plot(cor, min_cor = .1, curved = FALSE, repel = TRUE)
  
  cor <- cor(big, method = "pearson", use = "pairwise.complete.obs")
  MyPlotNetwork(cor, xFactor = xFactor, yFactor = yFactor, leg.cex = leg.cex)
}

##########################################################################



CompareAllMethods("All")
#CompareAllMethods("Dorsal")
#CompareAllMethods("Lateral")

# Method correlation network diagram
# PNG suitable for embedding in a Word document
JPlotToPNG("../output/Figure_1.png",
           PlotCorNetwork("All", xFactor = 0.19, leg.cex = .7),
           units = "px", width = 900, height = 450, res = 160)
# Same diagram for publishing
JPlotToPDF("../output/Figure_1.pdf",
           PlotCorNetwork("All", xFactor = 0.2, leg.cex = .7))