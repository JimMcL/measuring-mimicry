library(JUtils)

# Script to generate figures used in the flowchart used to summarise geometric morphometrics workflow.

coords <- import_jpg(c("../data/outline1cb075201eec.jpg", "../data/outline1cb02298599a.jpg", "../data/outline1cb02123296.JPG"))

# Subsample or interpolate points to a standard number of points.
sampleSize <- 30
coords <- lapply(coords, function(m) {
  if (nrow(m) < sampleSize) {
    coo_interpolate(m, sampleSize)
  } else {
    coo_sample(m, sampleSize)
  }
})

o <- Out(coords)

# Close the outline and smooth
outlines <- coo_close(o) %>% coo_smooth(1)

JPlotToPNG("../output/points.png", {
  par(mar = c(0, 2, 0, 2))
  plot(outlines[[1]]$outline1cb075201eec, type = "b", pch = 16, axes = FALSE, xlab = "", ylab = "", asp = 1)
  #text(200, 240, "A", cex = 3)
})


JPlotToPNG("../output/align.png", {
  par(mar = c(0, 2, 0, 2))
  plot(outlines[[1]]$outline1cb075201eec, type = "b", pch = 16, axes = FALSE, xlab = "", ylab = "", asp = 1)
  lines(outlines[[1]]$outline1cb02298599a, type = "b", pch = 16)
  lines(outlines[[1]]$outline1cb02123296, type = "b", pch = 16)
  #text(250, 220, "A, B, C", cex = 3)
})

aligned <- fgProcrustes(coo, coo = TRUE)
minCoords <- min(sapply(outlines$coo, length)) / 2
fr <- efourier(aligned, norm = T, nb.h = minCoords %/% 2)
