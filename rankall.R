## Author: Padmini Gollapalli
## 

## Functions for getting the hospital of rank 'num' for the given State and Outcome

rankall <- function(outcome_name, num="best") 
{
  ##Read the file into outcome and convert "Not Available" in the file to NA
  outcome <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available", colClasses = "character")
 
  ##Create a String Vector of all states
  state_str<-unique(outcome[,"State"])

  state_str<-sort(state_str,na.last = NA)

  ##k is counter to see if we are adding to result for the first time or after first time.  Do not use rbind the first time
  k<-as.numeric(0)
    result <- data.frame(hospital=NA, state=NA)
##    cat("nrows in result:",nrow(result),"k==",k,"\n\n")
    
##    colnames(result)=c("hospital", "state")
##    print(result)
    for(i in state_str)
    {
##      print("GKP Inside for loop")
      
      col_num<-0
      
      ##Getting the column numbers for each of the 3 outcomes in the file
      if(outcome_name=="heart attack")
        col_num<-match("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",names(outcome))
      else if (outcome_name=="heart failure")
        col_num<-match("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",names(outcome))
      else if(outcome_name=="pneumonia")
        col_num<-match("Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia",names(outcome))
      else
        stop("invalid outcome")
      
##      cat("i=",i,", colnum=",col_num)
      
      outcome_state <- outcome[outcome$State==i,]  ##Filter for the ith state
      
      ##Remove the NA rows for the specified outcome
      nona_out<-outcome_state[!is.na(outcome_state[,col_num]),]
      nona_out<-nona_out[,c(2,7,col_num)]  ##Filtering down to useful columns only
      num_rows<-nrow(nona_out)
##      cat(", num_rows = ",num_rows, ", head of nona_out" )
      ##print(head(nona_out))
      
      ##Order the data first on the outcome value (ascending) and then on the Hospital Name (ascending)
      ord_state<-nona_out[ order(suppressWarnings(as.numeric(nona_out[,3])),nona_out[,"Hospital.Name"]), ]
##            print("*******ord_state********************")
      ##print(ord_state)
      
      if(is.numeric(num))
      {
        if(num<=num_rows)
        {
          state_row<-data.frame(hospital=NA, state=NA)
          state_row[1,1]<-ord_state[num,c("Hospital.Name")]
          state_row[1,2]<-ord_state[num,c("State")]
  ##        cat("******GKP4 state_row:\n")
  ##        print(state_row)
          if(k==0)
          {
            result<-state_row
            k<-1
          }
          else
            result<-rbind(result,state_row)
    ##      cat("**************GKP1***********k==",k,"\n")
          ##print(result)
        }
        else  ##Else statement for rank <= num_rows
        {
            NA_row<-data.frame(hospital=NA, state=NA)
            ##colnames(NA_row)=c("hospital", "state")
            NA_row[1,2]=i
      ##      cat("******GKP2 NA_row:\n")
        ##    print(NA_row)
            if(k==0)
            {
              result<-NA_row
              k<-1
            }
            else
              result<-rbind(result,NA_row)
  ##          cat("######GKP3########### k==",k,"\n")
            ##print(result)
        } 
      }
      else    ##Else statement for nonnumeric num value
      {
        if(identical(num,"best"))
        {
          state_row<-data.frame(hospital=NA, state=NA)
          state_row[1,1]<-ord_state[1,c("Hospital.Name")]
          state_row[1,2]<-ord_state[1,c("State")]
    ##      cat("******GKP4 state_row:\n")
    ##      print(state_row)
          if(k==0)
          {
            result<-state_row
            k<-1
          }
          else
            result<-rbind(result,state_row)
        }
        if(identical(num,"worst"))
        {
          state_row<-data.frame(hospital=NA, state=NA)
          state_row[1,1]<-ord_state[num_rows,c("Hospital.Name")]
          state_row[1,2]<-ord_state[num_rows,c("State")]
  ##        cat("******GKP4 state_row:\n")
  ##        print(state_row)
          if(k==0)
          {
            result<-state_row
            k<-1
          }
          else
            result<-rbind(result,state_row)
        }
      }
    }
##    print("***************GKP returning result******************")
    return(result)

  ##data_by_state <- split(full_data[, c("Hospital.Name", "State", column)], full_data$State)
##    split_out<-split(nona_out, nona_out$State)

  ##How can one calculate the average miles per gallon (mpg) by number of cylinders in the car (cyl)?
  ##sapply(split(mtcars$mpg, mtcars$cyl), mean)

}