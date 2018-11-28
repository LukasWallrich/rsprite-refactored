rSprite.huge <- 1e15
rSprite.dust <- 1e-12

N <- 45
tMean <- 3.532
tSD <- 1.561
dp <- 2
scaleMin <- 1
scaleMax <- 7
fixedValue <- ""
fixedCount <- ""
fixedSeed <- 0

#tMean <- 19.4
#tSD <- 19.9
#dp <- 1
#scaleMin <- 0
#scaleMax <- 41
#fixedValue <- 0
#fixedCount <- 21
#fixedSeed <- 1

dstep <- c(0.1, 0.01, 0.001)[dp]

rSprite.notifIdList <<- list()
rSprite.messageList <<- list()
rSprite.prevGo <<- 0
rSprite.prevHelp <<- 0
rSprite.plotData <<- c()
