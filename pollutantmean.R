#courseID: rprog-013

source("submitscript1.R")
path <- "C:/Users/rcoleman/Documents/GitHub/Coursera/R Programming/rprog-data-specdata/"

setwd("C:/Users/rcoleman/Documents/GitHub/Coursera/R Programming/")

################################part 1#################################################
pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  newPath <- paste0(path,directory,"/")
  df <- data.frame(matrix(vector(), 0, 4, dimnames=list(c(), c("Date", "sulfate", "nitrate","ID"))), stringsAsFactors=F)
  
  fileNames <- list.files(path = newPath)
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  for(i in id)
  {
    df <- rbind(df,read.csv(paste0(newPath,fileNames[i])))
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
  }
    
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  if(pollutant=="nitrate")
  {
    return(mean(df$nitrate,na.rm=TRUE))
  }
  if (pollutant == "sulfate")
  {
    return(mean(df$sulfate,na.rm=TRUE))
  }
}
##################################part 3##############################################
