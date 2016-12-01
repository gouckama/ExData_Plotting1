## This code builds the 4th plot as required for the Week 1 Project
## of the Exploritory Data Analysis course.

## To build the plot
## 1. Change any setwd statements as appropriate for your system.
## 2. Ensure internet access to download the required files.
## 3. Ensure needed librarys have been installed (sqldf, png).
## 4. Ensure the source files for plots 2 and 3 are available.
##    You may need to change the source statements to reflect your system.
## 5. From R execute: BuildPlot4PNG()

## There are 4 functions coded:
##
## FileData: this function reads the data for the plot.
##
## BuildVoltage: this function builds plot2 displaying on the current
##  graphics device.
##
## BuildGlobalReactivePower: this function builds plot2 displaying on the current
##  graphics device.
##
## BuildPlot4PNG: this functions builds the plot2.PNG file.
##
## Note: other functions are used but are referenced in other source files used to
## build plot 2 and plot 3.  Hence, these source files must be available also.

## This function reads the data required for the Week 1 Project
## of the Exploritory Data Analysis course.
## This project consists of 4 plots from the same data.
## Each plot is meant to run independently as per project requirements.
## Hence, this function FileData is present in all 4
## plot code sets.
## If run on another device, the working directory may need to be changed accordingly.
## It is assumed that the sqldf package has been installed.
FileData <- function()
{
  setwd("~/Coursera/Exploritory Data Analysis")
  
  ## The data set that will contain the required data.
  myData <- NULL
  
  ## Keeps track if the data has been downloaded and read into memory.
  dataRead <- FALSE
  
  ReadData <- function()
  {
    if(!dataRead){
      ## Set the working directory to do the analysis.
      ## Download the data and unzip the file.
      download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip", destfile="Dataset.zip")
      unzip(zipfile="Dataset.zip")
      
      ## Using sqldf here to load the data
      library(sqldf)
      
      ## Note as per the project documentation, date format is dd/mm/YYYY; hence,
      ## Feb 1, 2007 is 01/02/2007 for example.
      myData <<- read.csv.sql(file = "household_power_consumption.txt", sep=";", header = TRUE, sql = "select * from file where Date = '1/2/2007' or Date = '2/2/2007'", row.names = FALSE)
      
      ## Create a new date time field based on dates.
      ## Determine actual date time as dates versus characters.
      myData <<- data.frame(myData, strptime(paste(myData$Date, myData$Time), "%d/%m/%Y %X"))
      ## Rename the new column to be more friendly.
      names(myData)[10] <<- "CombinedDateTime"
      
      dataRead <<- TRUE
    } ## end if
    
  }
  
  ## Ultimately retrieves the data.
  ## However, it keeps track if the data has been read already.
  ## If it has been read, it does NOT redownload and load the data.
  DataSet <- function() 
  {
    if(!dataRead) ReadData()
    
    myData
  }
  
  ## Exposes the function to ultimately return myData.
  list(DataSet = DataSet)
} ## end function FileData

## This function builds the voltage graph or the top right graph for plot4.
##
## Parameters
## 
## inReadData: defaults to TRUE and executes functionReadData to read in the data.
##   Allowed values: TRUE, FALSE
BuildVoltage <- function(inFD = NULL) {

  ## If a FileData reference is not created, make one.
  if(is.null(inFD)) inFD <- FileData()

  ## Create the line graph via plot.
  ## Set the X label.
  ## Set the Y label
  ## Set the type to line.
  plot(inFD$DataSet()$CombinedDateTime, inFD$DataSet()$Voltage, type = "l", xlab = "datetime", ylab = "Voltage")
} ## end function BuildVoltage

## This function builds the global reactive power graph or the lower right graph for plot4.
##
## Parameters
## 
## inReadData: defaults to TRUE and executes functionReadData to read in the data.
##   Allowed values: TRUE, FALSE
BuildGlobalReactivePower <- function(inFD = NULL) {
  
  ## If a FileData reference is not created, make one.
  if(is.null(inFD)) inFD <- FileData()

  ## Create the line graph via plot.
  ## Set the X label.
  ## Set the Y label
  ## Set the type to line.
  plot(inFD$DataSet()$CombinedDateTime, inFD$DataSet()$Global_reactive_power, type = "l", xlab = "datetime", ylab = "Global_reactive_power")
} ## end function BuildGlobalReactivePower

## This function builds the PNG file required for plot4.
## It is assumed the PNG library has been installed.
## The PNG file is placed in the same directory where the data was read from.
## It is also assumed that that the code used to build plots 2 and 3 are 
## available.
##
## Parameters
## 
## inReadData: defaults to TRUE and executes functionReadData to read in the data.
##   Allowed values: TRUE, FALSE
BuildPlot4PNG <- function() {
  
  ## This is the directory where the PNG gets saved.
  setwd("~/Coursera/Exploritory Data Analysis")

  library(png)

  ## Set up the file data variable.
  ## Ultimately this results in the data only being downloaded and read once
  ## versus every time for each plot.
  myFileData <- FileData()
  
  ## This contains the code for the Global Active Power plot.
  source("~/Coursera/Exploritory Data Analysis/plot2.R")

  ## This contains the code for the submeterming plot.
  source("~/Coursera/Exploritory Data Analysis/plot3.R")
  
  ## Clear any existing plots.
  graphics.off()
  
  ## Set up the file.
  png("plot4.png", width = 480, height = 480)

  ## Build Combined Graph
  par(mfrow=c(2,2))
  
  ## Build the first plot
  ## Need to change Y label to match the example
  BuildGlobalActivePower(inYLabel = "Global Active Power", inFD = myFileData)

  ## Build the second plot
  BuildVoltage(inFD = myFileData)

  ## Build the third plot
  ## Remove the legend box line to match the example
  BuildSubMetering(inLengendBorder = "n", inFD = myFileData)

  ## Build the fourth plot
  BuildGlobalReactivePower(inFD = myFileData)
  
  ## Close the file.
  graphics.off()
  
} ## end function BuildPlot4PNG
