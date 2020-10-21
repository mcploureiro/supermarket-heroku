
library (shiny)
library (shinyjs)
library (dplyr)
library (ggplot2)
library (grid)
library (gridExtra)
library (DT)

myData <- read.csv ( "myJoin2.txt" ) 
myPerson <- unique(myData$pe_name) #),"") #), levels=c(unique(myData$pe_name),"") )
myFill <- c ( "chocolate3", "paleturquoise3", "seagreen4") #, "white")
names ( myFill ) <- myPerson