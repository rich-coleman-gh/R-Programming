library(ggplot2)

setwd("C:/Users/rcoleman/Documents/GitHub/Coursera/R Programming")

hospital_df <- read.csv("C:/Users/rcoleman/Documents/GitHub/Coursera/R Programming/rprog-data-ProgAssignment3-data/hospital-data.csv")

df <- read.csv("C:/Users/rcoleman/Documents/GitHub/Coursera/R Programming/rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv")

#df[,12] <- as.numeric(as.character(df[,11]))

#hist(df[,11])

best <- function(state,outcome)
{
  if(state %in% df$State)
  {
    df_temp <- df[df$State==state,]
    df_temp$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack <- as.numeric(as.character(df_temp$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack))
    df_temp$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure <- as.numeric(as.character(df_temp$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure))
    df_temp$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia <- as.numeric(as.character(df_temp$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia))
    
    if(outcome=="heart attack")
    {
      df_temp <- df_temp[complete.cases(df_temp$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack),]
      df_temp <- df_temp[df_temp$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack == min(df_temp$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack),]
      df_temp <- df_temp[order(df_temp$Hospital.Name),]
    }
    if(outcome=="heart failure")
    {
      df_temp <- df_temp[complete.cases(df_temp$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure),]
      df_temp <- df_temp[df_temp$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure == min(df_temp$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure),]
      df_temp <- df_temp[order(df_temp$Hospital.Name),]
    }
    if(outcome=="pneumonia")
    {
      df_temp <- df_temp[complete.cases(df_temp$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia),]
      df_temp <- df_temp[df_temp$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia == min(df_temp$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia),]
      df_temp <- df_temp[order(df_temp$Hospital.Name),]
    } 
    as.character(head(df_temp$Hospital.Name,1))
  }
  else
  {
   stop("invalid state")
  }
}