##########################################################################
#### Plots for publication ####

#devtools::install_github("JimMcL/JUtils")
library(JUtils)
source("general-functions.R")
source("ant-mimic-functions.R")


# Biplot of PCA
df <- read.csv(file.path(OUTPUT_DIR, "Linear morphometrics-accuracy-individuals.csv"))

# Change any NAs to the mean of the column. The alternative is to throw out any rows containing NAs
numericCols <- grep("width|height|length|score", names(df), value = TRUE, ignore.case = TRUE)
for (c in numericCols) {
  df <- ReplaceNAs(df, c)
}

# Prettify names
names(df) <- gsub("\\.+", " ", names(df))
dataCols <- gsub("\\.+", " ", numericCols)

pca <- prcomp(df[, dataCols], scale. = TRUE)
# print(summary(pca))

#outliers <- c("Paradiestus gigantea", "Myrmecotypus rettenmeyeri", "Sphecotypus niger")
outliers <- c()
mimicCol <- "#3050f0"
mimicPch <- 21
nonMimicCol <- "#60c030"
nonMimicPch <- 22
antCol <- "#c01010"
antPch <- 24
isMimic <- df$mimicType == "mimic"
isAnt <- df$mimicType == "model"

.plotPca <- function() {
  par(mar = c(2.1, 2.1, .5, .5) + .1)
  CustomPcaPlot(pca, ifelse(df$species %in% outliers, df$species, ""), 
                xbg = ifelse(isMimic, mimicCol, ifelse(isAnt, antCol, nonMimicCol)),
                pch = ifelse(isMimic, mimicPch, ifelse(isAnt, antPch, nonMimicPch)),
                extendPlot = c(0.05, 0),
                arrowScale = 12, xpd = T)
  legend("topleft", c("Mimics", "Ants", "Non-mimics"),
         cex = 0.9,
         pch = c(mimicPch, antPch, nonMimicPch),
         pt.bg = c(mimicCol, antCol, nonMimicCol), inset = c(0.01, 0.01))

  # # Inset a scree plot
  # par(fig = c(.78, .98, .02, .22), new = TRUE)
  # customScreePlot(pca, cex.axis = 0.6)
}

# dev.off()
# .plotPca()

JPlotToPNG("../output/pca.png", .plotPca, units = "px", width = 900, res = 120, aspectRatio = 1.2)
# Embedding fonts requires Ghostscript to be installed
JPlotToPDF("../output/pca.pdf", .plotPca, width = 240, aspectRatio = 1.2, embedFonts = TRUE)
