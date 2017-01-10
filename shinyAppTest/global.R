library(shinydashboard)
library(shiny)
library(data.table)
#source("ShinyPSASorterwdplyr.R")


## Colours - see help(validColors)
colPalette <- c("red", "yellow", "aqua", "blue", "light-blue", "green",
                "navy",  "teal", "olive", "lime", "orange", "fuchsia",
                "purple", "maroon",  "black")

skin_colour    <- colPalette[4]
box_background <- colPalette[1]
filename <- 'file1'



## -------- Global functions to abstract code from shiny application -------- #

## Identity function example
x <- function(x)x
