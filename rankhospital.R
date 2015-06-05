#courseID: rprog-010
setwd("C:/Users/rcoleman/Documents/GitHub/Coursera/R Programming")

df <- read.csv("C:/Users/rcoleman/Documents/GitHub/Coursera/R Programming/rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv")

rankhospital <- function(state,outcome,num="best")
{
  
  if(state %in% df$State && outcome %in% c("heart attack","heart failure","pneumonia"))
  {
    #prep data frame by converting factors into numeric
    df_temp <- df[df$State==state,]
    df_temp$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack <- as.numeric(as.character(df_temp$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack))
    df_temp$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure <- as.numeric(as.character(df_temp$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure))
    df_temp$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia <- as.numeric(as.character(df_temp$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia))
    
    #based on outcome apply apropriate filters to data frame
    if(outcome=="heart attack")
    {
      df_temp <- df_temp[complete.cases(df_temp$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack),]
      df_temp <- df_temp[order(df_temp$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,df_temp$Hospital.Name),]
    }
    else if(outcome=="heart failure")
    {
      df_temp <- df_temp[complete.cases(df_temp$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure),]
      df_temp <- df_temp[order(df_temp$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,df_temp$Hospital.Name),]
    }
    else if(outcome=="pneumonia")
    {
      df_temp <- df_temp[complete.cases(df_temp$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia),]
      df_temp <- df_temp[order(df_temp$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia,df_temp$Hospital.Name),]
    } 
    
    #get true row number of rank
    if(num=="best")
    {
      num = 1
    }else if (num=="worst")
    {
      num = nrow(df_temp)
    }
    
    #check to see if rank is greater than existing rank in data frame
    if(num<=nrow(df_temp))
    {
      as.character(df_temp[num,"Hospital.Name"])
    }
    else {
      NA
    }
  }
  else
  {
    stop("invalid outcome")
  }
}