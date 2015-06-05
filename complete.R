#courseID: rprog-010
library(plyr)

source("submitscript1.R")
path <- "C:/Users/rcoleman/Documents/GitHub/Coursera/R Programming/rprog-data-specdata/"

setwd("C:/Users/rcoleman/Documents/GitHub/Coursera/R Programming/")

complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  newPath <- paste0(path,directory,"/")
  df <- data.frame(matrix(vector(), 0, 2, dimnames=list(c(), c("id", "nobs"))), stringsAsFactors=F)
  
  fileNames <- list.files(path = newPath)
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  for(i in id)
  {
    df_temp <- read.csv(paste0(newPath,fileNames[i]))
    df_temp <- df_temp[complete.cases(df_temp),]
    df <- rbind(df,data.frame(i,nrow(df_temp)))
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
  }
  df <- rename(df,c("i"="id","nrow.df_temp."="nobs"))
  return(df)
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
}