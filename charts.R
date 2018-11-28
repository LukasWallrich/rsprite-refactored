# Build the label to go across the top of each results chart.
rSprite.chartLabel <- function (N, tMean, tSD, scaleMin, scaleMax, dp, splitLine) {
  dpformat <- paste("%.", dp, "f", sep="")
  label <- paste("N=", N
                 , " (", scaleMin, "-", scaleMax, ")%%"
                 , "M=", sprintf(dpformat, tMean)
                 , " SD=", sprintf(dpformat, tSD)
                 , sep=""
  )
  
  if (splitLine) {
    label <- unlist(strsplit(label, "%%"))
  }
  else {
    label <- gsub("%%", " ", label)
  }
  
  return (label)
}

# Build a single results chart (grob).
rSprite.buildOneChart <- function (vec, scaleMin, scaleMax, gridSize, xMax, yMax, label) {
  df <- data.frame(vec)
  
  # Avoid showing a large number of empty elements on the right of the X-axis if our upper bound is very large.
  xLimit <- if (((scaleMax - scaleMin) <= 11) || (xMax > scaleMax))
    max(scaleMax, xMax)
  else
    min(scaleMax, (xMax + 2))
  xBreaks <- scaleMin:xLimit
  
  # Allow for room above the highest bar to display the label.
  yLimit <- yMax
  llen <- length(label)
  if (llen > 0) {
    yBump <- round(llen * max(2, yMax * 0.1) * (((gridSize >= 4) + 2) / 2))
    yLimit <- yMax + yBump
  }
  
  yTicks <- c(10, 8, 6, 5, rep(4, 6))[gridSize]
  yTickSize <- round((yMax / (yTicks - 1)) + 1)
  yLabelGaps <- c(1, 2, 3, 4, 5, 10, 20, 25, 50, 100, 200, 250, 500, 1000)
  yGap <- yLabelGaps[yLabelGaps >= yTickSize][1]
  yBreaks <- (0:(yTicks - 1)) * yGap
  
  axisTitleSize <- c(20, 14, 12, 11, 10, rep(8, 5))[gridSize]
  axisTextSize <- c(16, 12, 10, 9, 8, rep(7, 5))[gridSize]
  
  grob <- ggplot(df, aes(x=vec)) +
    geom_bar(fill="#0099ff", width=0.9) +
    expand_limits(x=c(scaleMin, xLimit)) +
    scale_x_continuous(breaks=xBreaks) +
    # Adam Gruer suggested: aes(x=factor(vec, levels=scaleMin:xLimit)))) + scale_x_discrete(drop=FALSE)
    scale_y_continuous(limits=c(0, yLimit), breaks=yBreaks) +
    theme(axis.title=element_text(size=axisTitleSize)) +
    theme(axis.text=element_text(size=axisTextSize)) +
    labs(x="response", y="count")
  
  if (llen > 0) {
    if (gridSize <= 10) {
      labelTextSize <- axisTitleSize * 0.352778 * (1 - (0.1 * (gridSize >= 8)))     # see StackOverflow 36547590
      labelText <- paste(label, collapse="\n")
      labelY <- (yLimit + 1 - llen) - (gridSize >= 5) - (gridSize >= 7)
      grob <- grob + annotate("text", x=round((xLimit + scaleMin) / 2), y=labelY, label=labelText, size=labelTextSize)
    }
  }
  
  flipXThreshold <- c(50, 30, 10, 15, 10, rep(3, 5))[gridSize]
  if (length(xBreaks) > flipXThreshold) {
    grob <- grob + theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5))
  }
  
  return(grob)
}

# Build a grid containing all the results charts.
rSprite.buildCharts <- function (sample, scaleMin, scaleMax, gridSize) {
  rows <- sample$rows
  if (nrow(rows) > 1) {
    rows <- rows[order(apply(rows, 1, skewness)),]
  }
  
  xMax <- max(rows)
  yMax <- max(unlist(apply(rows, 1, table)))
  grobs <- apply(rows, 1, function (x) {
    rSprite.buildOneChart(x, scaleMin, scaleMax, gridSize, xMax, yMax, sample$label)
  })
  layoutMatrix <- matrix(1:(gridSize ^ 2), nrow=gridSize, ncol=gridSize, byrow=TRUE)
  grid.arrange(grobs=grobs, layout_matrix=layoutMatrix)
}
