## The function 'pollutantmean' calculates the mean of a
## pollutant (sulfate or nitrate) across a specified list of monitors. The
## function 'pollutantmean' takes three arguments: 'directory',
## 'pollutant', and 'id'. Given a vector monitor ID numbers,
## 'pollutantmean' reads that monitors' particulate matter data from the
## directory specified in the 'directory' argument and returns the mean of
## the pollutant across all of the monitors, ignoring any missing values
## coded as NA. 

## Sample example output:
## source("pollutantmean.R")
## pollutantmean("specdata", "sulfate", 1:10)
## 
## ## [1] 4.064
## 
## pollutantmean("specdata", "nitrate", 70:72)
## 
## ## [1] 1.706
## 
## pollutantmean("specdata", "nitrate", 23)
## 
## ## [1] 1.281

pollutantmean <- function(directory, pollutant, id = 1:332){
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  
  pollutantData <- data.frame()
  
  ##loop to read each file present in id vector
  for (fileNum in rep(id)){
    
    ##Create file name with the required format
    fileName <- paste(directory, "/", sprintf("%03d", fileNum), ".csv", sep = "")

    ##Read each file data
    pollutantData <- rbind(pollutantData, read.table(fileName, sep = ",", header = TRUE))
  }
  
  ##Create matrix for pollutant data by removing NA values
  matrixPollutant <- as.matrix(pollutantData[pollutant], na.rm = TRUE)
  
  ##Calculate mean of pollutant
  mean <- round(mean(matrixPollutant, na.rm = TRUE), 3)
  
  mean
}