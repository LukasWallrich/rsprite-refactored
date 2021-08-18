# rsprite-refactored
@setgree separated rsprite into constituent files to do some troubleshooting for Code Ocean and shared it.

I then added some functions to make it easier to run 'out of the box' - primarily to install packages as needed. Now, rSPRITE should just work when you run the following in R or RStudio:

```
# install.packages("shiny") - if necessary
library(shiny)

# Easiest way is to use runGitHub from the shiny package
runGitHub("rsrpite-refactored", "LukasWallrich")
```

Code is lifted from https://osf.io/pwjad/ and all credit to Nick Brown and James Heathers for the substance of it.
 
