## This code builds the 2nd plot as required for the Week 1 Project
## of the Exploritory Data Analysis course.

## To build the plot
## 1. Change any setwd statements as appropriate for your system.
## 2. Ensure internet access to download the required files.
## 3. Ensure needed librarys have been installed (sqldf, png).
## 4. From R execute: BuildPlot2PNG()

## There are 3 functions coded:
##
## FileData: this function reads the data for the plot.
##
## BuildGlobalActivePower: this function builds plot2 displaying on the current
##  graphics device.
##
## BuildPlot2PNG: this functions builds the plot2.PNG file.

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

## This function builds the plot required for plot2.
##
## Parameters
## 
## inYLabel: string, defaults to "Global Active Power (kilowatts)"
##   This plot is actually used more than once; however the y label changes.
## inFD: a reference to the FileData variable.
BuildGlobalActivePower <- function(inFD = NULL, inYLabel = "Global Active Power (kilowatts)") {

  ## If a FileData reference is not created, make one.
  if(is.null(inFD)) inFD <- FileData()

  ## Create the line graph via plot.
  ## Remove the X label.
  ## Set the Y label
  ## Set the type to line.
  plot(inFD$DataSet()$CombinedDateTime, inFD$DataSet()$Global_active_power, type = "l", xlab = "", ylab = inYLabel)

} ## end function BuildGlobalActivePower

## This function builds the PNG file required for plot2.
## It is assumed the PNG library has been installed.
BuildPlot2PNG <- function() {

  ## This is the directory where the PNG gets saved.
  setwd("~/Coursera/Exploritory Data Analysis")

  ## Set up the file data variable.
  myFileData <- FileData()
  
  library(png)
  
  ## Clear any existing plots.
  graphics.off()
  
  ## Set up the file.
  png("plot2.png", width = 480, height = 480)
  
  ## Build the plot.
  BuildGlobalActivePower(inFD = myFileData)
  
  ## Close the file.
  graphics.off()
  
} ## end function BuildPlot2PNG
