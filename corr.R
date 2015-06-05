#courseID: rprog-010
options (digits = 4)

library(plyr)

path <- "C:/Users/rcoleman/Documents/GitHub/Coursera/R Programming/rprog-data-specdata/"

setwd("C:/Users/rcoleman/Documents/GitHub/Coursera/R Programming/")

corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  newPath <- paste0(path,directory,"/")
  df <- data.frame(matrix(vector(), 0, 4, dimnames=list(c(), c("Date", "sulfate","nitrate","ID"))), stringsAsFactors=F)
  corrVec <- numeric()
  
  fileNames <- list.files(path = newPath)
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  for(i in 1:length(fileNames))
  {
    df_temp <- read.csv(paste0(newPath,fileNames[i]))
    df_temp <- df_temp[complete.cases(df_temp),]
    if(threshold == 0)
    {
      corr <- cor(df_temp$sulfate,df_temp$nitrate)
      corrVec <- c(corrVec,corr)
    }
    else if(nrow(df_temp) > threshold)
    {
      corr <- cor(df_temp$sulfate,df_temp$nitrate)
      corrVec <- c(corrVec,corr)
    }
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
  }
  
  return(corrVec)
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
}