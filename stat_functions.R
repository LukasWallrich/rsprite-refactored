#!/usr/bin/env Rscript

# See if a mean is GRIM-consistent. If not, return the nearest mean that is.
rSprite.checkGrim <- function (N, tMean, dp) {
  gMean <- tMean
  int <- round(tMean * N)           # nearest integer; doesn't matter if this rounds up or down
  frac <- int / N
  dif <- abs(tMean - frac)
  gran <- ((0.1 ^ dp) / 2) + rSprite.dust   # allow for rounding errors
  if (dif > gran) {
    gMean <- round(int / N, dp)
    dpformat <- paste("%.", dp, "f", sep="")
    s <- paste("Mean ", sprintf(dpformat, tMean), " fails GRIM test - using ", sprintf(dpformat, gMean), sep="")
    rSprite.message(s, shinyType="warning")
  }

  return(gMean)
}

# Determine minimum and maximum SDs for given scale ranges, N, and mean.
rSprite.sdLimits <- function (N, tMean, scaleMin, scaleMax, dp) {
  result <- c(rSprite.huge, -rSprite.huge)      # impossible values

  aMax <- scaleMin                              # "aMax" means "value of a to produce the max SD"
  aMin <- floor(tMean)
  bMax <- max(scaleMax, scaleMin + 1, aMin + 1) # sanity check (just scaleMax would normally be ok)
  bMin <- aMin + 1

  total <- round(tMean * N)
  for (abm in list(c(aMin, bMin, 1), c(aMax, bMax, 2))) {
    a <- abm[1]
    b <- abm[2]
    m <- abm[3]

    k <- round((total - (N * b)) / (a - b))
    k <- min(max(k, 1), N - 1)   # ensure there is at least one of each of two numbers
    vec <- c(rep(a, k), rep(b, N - k))
    result[m] <- round(sd(vec), dp)
  }

  return(result)
}

# Make a single tweak to the data to try and move the SD in the desired direction.
# This will usually subtract 1 (or 2) from one element and add the same to another,
#  thus preserving the mean, but occasionally it will just do one of those things,
#  if the resulting mean is still GRIM-consistent.
rSprite.delta <- function (vec, tMean, tSD, scaleMin, scaleMax, dp=2, fixed=c()) {
# Most of the time we change a pair of numbers by +/- 1, but 2 allows us to jump over fixed numbers.
  delta <- 1
  if (    (runif(1) < 0.2)
       && ((length(vec[vec > (scaleMin + 1)])) > 0)    # Check there is a number we can decrement by 2!
     ) {
    delta <- 2
  }

# Select an element to decrement. This should be greater than the minimum, and not
#  1 greater than a fixed value (because decrementing it would give us the fixed value).
# For better performance, at the risk of modest bias, we select from unique elements only.
  uniqueCanDec <- !duplicated(vec)
  notFixedDec <- if (length(fixed) > 0) !(vec %in% (fixed + delta)) else TRUE
  indexCanDec <- uniqueCanDec & (vec > (scaleMin + delta - 1)) & notFixedDec
  if (length(indexCanDec) == 0) {
    return(vec)           # Go back and try again.
  }

# Decide if we need to increase or decrease the SD.
  fullVec <- c(vec, fixed)
  increaseSD <- (sd(fullVec) < tSD)

# If we want to make the SD larger, there is no point in decrementing the largest item,
#  unless we have no choice.
  if (increaseSD) {
    indexCanDecTry1 <- indexCanDec & (vec < max(vec))
    if (sum(indexCanDecTry1) > 0) {
      indexCanDec <- indexCanDecTry1
    }
  }

  whichCanDec <- which(indexCanDec)
  whichWillDec <- whichCanDec[as.integer(runif(1) * length(whichCanDec)) + 1];
  willDec <- vec[whichWillDec]

# Select an element to increment. This should be smaller than the maximum,
#  and not 1 less than a fixed value.
  vecInc <- vec
  vecInc[whichWillDec] <- rSprite.huge        # mark the element that we decremented, so we can exclude it
  uniqueCanInc <- !duplicated(vecInc)
  notFixedInc <- if (length(fixed) > 0) !(vecInc %in% (fixed - delta)) else TRUE
  indexCanInc <- uniqueCanInc & (vecInc < (scaleMax - delta + 1)) & (vecInc != rSprite.huge) & notFixedInc

# If we want to make the SD smaller, there is no point in incrementing an item larger than
#   the one that we are going to decrement, unless we have no other choice.
  if (!increaseSD) {
    indexCanIncTry1 <- indexCanInc & (vec < willDec)
    if (sum(indexCanIncTry1) > 0) {
      indexCanInc <- indexCanIncTry1
    }
  }

# There is no point in incrementing an element that is <delta> smaller than the one
#  that we are going to decrement, unless we have no other choice.
  dontInc <- willDec - delta
  indexCanIncTry2 <- indexCanInc & (vecInc != dontInc)
  if (sum(indexCanIncTry2) > 0) {
    indexCanInc <- indexCanIncTry2
  }

# If we can't find an element to increment, just return the current vector unchanged and let our caller sort it out.
  if (sum(indexCanInc) < 1) {
    return(vec)           # Go back and try again.
  }

  whichCanInc <- which(indexCanInc)
  whichWillInc <- whichCanInc[as.integer(runif(1) * length(whichCanInc)) + 1];

# Another option is to only change one of the cells (decrement one without incrementing another,
#  or vice versa).
# This enables us to explore different means that still round to the same target value.
# I could have probably written this more elegantly if I'd thought of this issue before I wrote
#  the code above to select candidates to be both incremented and decremented, but there we are.
# So what we do here is to perform either the decrement or the increment first, and then see if
#  the mean is still GRIM-consistent with the target mean. If it is, in a proportion of cases,
#  we don't adjust the other cell.
# This can probably be optimised further, by considering whether we want to increase or decrease the SD.
# For some reason that I don't understand, the most "even" exploration of the means seems to occur
#  if the proportion of cases where we change the mean is not too large.

  decFirst <- (runif(1) < 0.5)
  if (decFirst) {
    vec[whichWillDec] <- vec[whichWillDec] - delta
  }
  else {
    vec[whichWillInc] <- vec[whichWillInc] + delta
  }

  doInc <- TRUE
  if (runif(1) < 0.1) {
    newFullVec <- c(vec, fixed)
    newMean <- mean(newFullVec)
    if (round(newMean, dp) == tMean) {         # New mean is GRIM-consistent, so we will keep it.
      doInc <- FALSE
    }
  }

  if (doInc) {
    if (decFirst) {
      vec[whichWillInc] <- vec[whichWillInc] + delta
    }
    else {
      vec[whichWillDec] <- vec[whichWillDec] - delta
    }
  }

  return(vec)
}

# Find a single vector of responses that matches the target mean and SD.
# Assumes that the mean has been checked for GRIM consistency (see rSprite.getSample).
rSprite.seekVector <- function (N, tMean, tSD, scaleMin, scaleMax, dp=2, fixed=c(), label) {
# Generate some random starting data.
  rN <- N - length(fixed)
  vec <- pmax(pmin(as.integer(runif(rN) * 2 * tMean), scaleMax), scaleMin)
  result <- c()

  if (length(fixed) > 0) {         # replace any of the fixed numbers with a random non-fixed number
    whichFixed <- which(vec %in% fixed)
    notFixed <- sample(setdiff(scaleMin:scaleMax, fixed), length(whichFixed), replace=TRUE)
    vec[whichFixed] <- notFixed
  }

# Adjust mean of starting data.
  gran <- ((0.1 ^ dp) / 2) + rSprite.dust   # allow for rounding errors
  meanOK <- FALSE
  maxStartLoops <- N * (scaleMax - scaleMin)

  for (i in 1:maxStartLoops) {
    fullVec <- c(vec, fixed)
    cMean <- mean(fullVec)
    dif <- abs(cMean - tMean)
    if (dif < gran) {
      meanOK <- TRUE
      break;
    }

# Identify numbers that we can increment or decrement.
# This should exclude numbers that would become one of the fixed values.
    deltaMean <- 1
    if (runif(1) < 0.2) {
      deltaMean <- 2       # This allows us to "jump over" the fixed values, if they are not at the extremities.
    }

    increaseMean <- (cMean < tMean)
    if (increaseMean) {
      filter <- (vec < (scaleMax - deltaMean + 1)) & (!(vec %in% (fixed - deltaMean)))
    }
    else {
      filter <- (vec > (scaleMin + deltaMean - 1)) & (!(vec %in% (fixed + deltaMean)))
    }

    canBumpMean <- which(filter)
    bumpMean <- canBumpMean[as.integer(runif(1) * length(canBumpMean)) + 1]   # select a changeable number
    vec[bumpMean] <- vec[bumpMean] + (if (increaseMean) deltaMean else -deltaMean)
  }

  if (!meanOK) {
    s <- "Couldn't initialize data with correct mean"  # This is actually a coding error if mean is in range
    rSprite.message(s, shinyType="error")
    return(result)
  }

  maxLoops <- max(round(N * ((scaleMax - scaleMin) ^ 2)), 1000)  # this maybe needs some more testing for pathological conditions
  found <- FALSE
  gran <- ((0.1 ^ dp) / 2) + rSprite.dust   # allow for rounding errors

  for (i in 1:maxLoops) {
    cSD <- sd(c(vec, fixed))
    if (abs(cSD - tSD) <= gran) {
      result <- vec
      break
    }

    vec <- rSprite.delta(vec, tMean, tSD, scaleMin, scaleMax, dp, fixed)
    if (length(vec) == 0) {    # rSprite.delta() failed (but may have generated its own message(s)).
      break
    }
  }

  return(result)
}

# Generate a sample of one or more unique SPRITE solutions.
rSprite.getSample <- function (maxCases, N, tMean, tSD, scaleMin, scaleMax, dp=2, fixed=c()) {
  result <- list(rows=c(), label="")

  # Generate a sample of one or more unique SPRITE solutions.
  rSprite.getSample <- function (maxCases, N, tMean, tSD, scaleMin, scaleMax, dp=2, fixed=c()) {
    result <- list(rows=c(), label="")

  # Check mean is possible with GRIM; if not, identify the nearest valid mean.
    tMean <- rSprite.checkGrim(N, tMean, dp)

  # Determine minimum and maximum SDs.
    sdLimits <- rSprite.sdLimits(N, tMean, scaleMin, scaleMax, dp)

    for (m in 1:2) {
      mSD <- sdLimits[m]
      s <- ""
      if ((m == 1) && (mSD > tSD)) {
        s <- "small; minimum="
      }
      else if ((m == 2) && (mSD < tSD)) {
        s <- "large; maximum="
      }

      if (s != "") {
        dpformat <- paste("%.", dp, "f", sep="")
        s <- paste("Target SD ", sprintf(dpformat, tSD), " is too ", s, sprintf(dpformat, mSD), sep="")
        rSprite.message(s, shinyType="warning")
        return(result)
      }
    }

    if (scaleMin >= scaleMax) {
      s <- paste("Scale minimum should be smaller than maximum")
      rSprite.message(s, shinyType="warning")
      return(result)
    }

    result$rows <- c()
    result$label <- rSprite.chartLabel(N, tMean, tSD, scaleMin, scaleMax, dp, (maxCases > 9))
    for (i in 1:(maxCases * 8)) {   # 8 is arbitrary; break early if we find enough unique cases.
      vec <- rSprite.seekVector(N, tMean, tSD, scaleMin, scaleMax, dp, fixed, result$label)
      if (length(vec) == 0) {       # If no solution was found on this run, return any we found up to now.
        if (length(result$rows) == 0) {
          s <- paste("No solution found for ", paste(result$label, collapse=" "), sep="")
          rSprite.message(s, shinyType="warning")
        }

        return(result)              # This may be slightly unsatisfactory if solutions are just very hard to come by.
      }

      fullVec <- sort(c(vec, fixed))         # Sorting lets us find duplicates more easily.
      if (length(result$rows) == 0) {
        result$rows <- matrix(fullVec, nrow=1)
      }
      else {
        newRows <- rbind(result$rows, fullVec)
        if (tail(duplicated(newRows), 1)) {  # The solution we just found is a duplicate.
          dups <- dups + 1
          if (dups > maxDups) {
            break
          }
          else {
            next
          }
        }

        result$rows <- newRows
      }

      nCases <- nrow(result$rows)
      if (nCases == maxCases) {
        incomplete <- FALSE
        break
      }

  # Calculate the maximum number of consecutive duplicates we will accept before deciding to give up.
  # The value of 0.00001 below is our nominal acceptable chance of missing a valid solution;
  #  however, it's extremely likely that all possible solutions are not all equally likely to be found.
  # So we also set a floor of 100 attempts.
      maxDups <- max(round(log(0.00001) / log(nCases / (nCases + 1))), 100)
      dups <- 0
    }

    if (nCases < maxCases) {
      was <- if (nCases == 1) "was" else "were"
      s <- paste(maxCases, " unique examples were requested, but only ", nrow(result$rows), " ", was, " found", sep="")
      rSprite.message(s, shinyType="warning")
    }

    return(result)
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

    grob <- ggplot(df, aes(x=factor(vec, levels=xBreaks))) +
            geom_bar(fill="#0099ff", width=0.9) +
            scale_x_discrete(drop=FALSE) +
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
