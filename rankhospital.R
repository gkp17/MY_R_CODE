## Author: Padmini Gollapalli
## 

## Functions for getting the hospital of rank 'num' for the given State and Outcome

rankhospital <- function(state_abr, outcome_name, num) 
{
  ##Read the file into outcome and convert "Not Available" in the file to NA
  outcome <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available", colClasses = "character")
  ##Create a String Vector of all states
  state_str<-unique(outcome[,"State"])
  
  
  if(length(i <- grep(state_abr, state_str)))  ##Makes sure that state_abr belongs in state_str vector
  {
    
    outcome_state <- outcome[outcome$State==state_abr,]  ##Filtering the outcome on the state specified
    
    col_num<-0
    
    ##Getting the column numbers for each of the 3 outcomes in the file
    if(outcome_name=="heart attack")
      col_num<-match("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",names(outcome_state))
    else if (outcome_name=="heart failure")
      col_num<-match("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",names(outcome_state))
    else if(outcome_name=="pneumonia")
      col_num<-match("Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia",names(outcome_state))
    else
      stop("invalid outcome")
    
    ##Remove the NA rows for the specified outcome
    nona_out_state<-outcome_state[!is.na(outcome_state[,col_num]),]
    num_rows<-nrow(nona_out_state)
    
    ##Order the data first on the outcome value (ascending) and then on the Hospital Name (ascending)
    ord_state<-nona_out_state[ order(suppressWarnings(as.numeric(nona_out_state[,col_num])),nona_out_state[,"Hospital.Name"]), ]
    if(is.numeric(num))
    {
      if(num<=num_rows)
        return(ord_state[num,"Hospital.Name"])
      else
        return(NA)
    }
    else
    {
        if(identical(num,"best"))
        {
          return(ord_state[1,"Hospital.Name"])
        }
        if(identical(num,"worst"))
        {
          return(ord_state[num_rows,"Hospital.Name"])
        }
    }
  }
  else
  {
    stop("invalid state")
    
  }
  

  
  
}