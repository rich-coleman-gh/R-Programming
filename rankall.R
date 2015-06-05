library(plyr)

#courseID: rprog-010
setwd("C:/Users/rcoleman/Documents/GitHub/Coursera/R Programming")

df <- read.csv("C:/Users/rcoleman/Documents/GitHub/Coursera/R Programming/rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv")

rankall <- function(outcome,num="best")
{
  
  if(outcome %in% c("heart attack","heart failure","pneumonia"))
  {
    #prep data frame by converting factors into numeric
    df_temp <- df
    df_temp$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack <- as.numeric(as.character(df_temp$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack))
    df_temp$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure <- as.numeric(as.character(df_temp$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure))
    df_temp$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia <- as.numeric(as.character(df_temp$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia))
    
    #based on outcome apply apropriate filters to data frame
    if(outcome=="heart attack")
    {
      df_temp <- df_temp[complete.cases(df_temp$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack),]
      df_temp <- transform(df_temp,rank = ave(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, State, FUN = function(x) rank(x, ties.method = "first")))
    }
    else if(outcome=="heart failure")
    {
      df_temp <- df_temp[complete.cases(df_temp$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure),]
      df_temp <- transform(df_temp,rank = ave(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, State, FUN = function(x) rank(x, ties.method = "first")))
    }
    else if(outcome=="pneumonia")
    {
      df_temp <- df_temp[complete.cases(df_temp$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia),]
      df_temp <- transform(df_temp,rank = ave(Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia, State, FUN = function(x) rank(x, ties.method = "first")))
    } 
    #break up data frame by state
    df_temp <- df_temp[order(df_temp$State,df_temp$rank,df_temp$Hospital.Name),]
    
    df_temp <- df_temp[c("Hospital.Name","State","rank")]
    
    df_state <- split(df_temp,df_temp$State)
    #apply ranking function by state (very much like window function in sql) 
    temp <- lapply(df_state,function(x)
    {
      if(num=="best")
      {
        x[x$rank==1,]
      }else if (num=="worst")
      {
        x[x$rank==nrow(x),]
      }else if (num<=nrow(x))    #check to see if rank is greater than existing rank in data frame
      {
        x[x$rank==num,]
      }
      else
      {
        c('NA', as.character(x$State[1]))
      }
    })
    
    #reformat list into data frame
    df_final <-  do.call(rbind,temp)
    df_final <- df_final[c("Hospital.Name","State")]
    #rename columns
    df_final <- rename(df_final, c("Hospital.Name"="hospital","State"="state"))
    rownames(df_final) <- NULL
  }
  else
  {
    stop("invalid outcome")
  }
  df_final
}
