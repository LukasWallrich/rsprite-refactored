# Store a message for future display (unless otherwisde specified).
# If we have unloaded Shiny for debugging, show the message immediately.
rSprite.message <- function (s, shinyType="default", shinyNow=FALSE) {
  if (!exists("shinyUI")) {
    cat("rSPRITE message: |", s, "| (shinyType=", shinyType, ")", "\n", sep="")
    return()
  }
  
  message <- paste(shinyType, s, sep="%%")
  if (shinyNow) {
    rSprite.shinyMessages(list(message))
  }
  else {
    rSprite.messageList <<- append(rSprite.messageList, message)
  }
}

# Function to display one or more notification messages.
rSprite.shinyMessages <- function (messageList) {
  lapply(rSprite.notifIdList, function (x) {
    removeNotification(x)
  })
  rSprite.notifIdList <<- list()
  
  uniqueMessages <- unique(unlist(messageList))
  sapply(uniqueMessages, function (x) {
    split <- unlist(strsplit(x, "%%"))
    messageType <- split[1]
    messageText <- split[2]
    id <- showNotification(messageText, type=messageType, duration=NULL, closeButton=FALSE)
    rSprite.notifIdList <<- append(rSprite.notifIdList, id)
  })
}

rSprite.helpText <- c(
  "rSPRITE is an implementation by Nick Brown of SPRITE, an idea by James Heathers."
  , "<br/><br/>"
  , "rSPRITE simulates data from an integer (e.g., Likert-type) scale in the form of bar charts."
  , "<br/><br/>"
  , "You can request up to 100 samples to be presented on a square grid."
  , " You need to specify the minimum and maximum item values of the scale,"
  , " and the mean, standard deviation, and size of the sample."
  , " The charts are presented in increasing order of skewness, from top left to bottom right."
  , "<br/><br/>"
  , "Optionally, you can provide a fixed value and a count;"
  , " this forces every sample to contain exactly that many occurrences of that value,"
  , " which may be outside the scale range."
  , "<br/><br/>"
  , "You can also download the individual values that make up the bar charts to a CSV file."
  , "<br/><br/>"
  , "If you check the box labeled 'Use fixed seed', you will get the same results on every run;"
  , " this can be useful when reporting problems, but otherwise, leave this box unchecked."
  , "<br/><br/>"
  , "rSPRITE may not always find every solution when there are only a few to be found."
  , " If you get a message saying that fewer results were found than you hoped for,"
  , " please try a couple more times to see if one or two more solutions show up."
  , "<br/><br/>"
  , "A general observation: rSPRITE is a tool and not a complete system."
  , " Like any tool, it has the potential to be used incorrectly."
  , " If you ask it do something silly, it will do it, very probably without warning you."
  , "<br/><br/>"
  , "For more information on SPRITE in general, see"
  , " <a href=https://hackernoon.com/introducing-sprite-and-the-case-of-the-carthorse-child-58683c2bfeb>here</a>."
  , "<br/><br/>"
  , "Please report bugs to nicholasjlbrown@gmail.com"
  , "<br/><br/>"
  , "Privacy policy: rSPRITE does not collect any information about you whatsoever."
  , " If you are using this code in a web browser at shinyapps.io, you can find the RStudio"
  , " terms of use <a href='https://www.rstudio.com/about/rstudio-service-terms-of-use/'>here</a>."
)

rSprite.notifIdList <<- list()
rSprite.messageList <<- list()
rSprite.prevGo <<- 0
rSprite.prevHelp <<- 0
rSprite.plotData <<- c()
