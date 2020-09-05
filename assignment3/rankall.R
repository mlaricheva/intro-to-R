rankall <- function(outcome_name, num = "best") {
  # Reading data and checking the entries (similar to ''best'' function)
  outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  outcomes_list <- c("heart attack", "heart failure", "pneumonia")
  outcomes_columns <- c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack","Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure","Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
  outcome_col <- NULL
  if(!outcome_name %in% outcomes_list){
    stop("invalid outcome")
  } else {
    for(j in 1:length(outcomes_list)){
      if(outcome_name==outcomes_list[j]) {outcome_col<-outcomes_columns[j]}
    }
  }
  # drop extra columns
  outcome<-outcome[c("State","Hospital.Name",outcome_col)]
  # sorting data frame by state and by outcome
  outcome[, outcome_col] <- suppressWarnings(as.numeric(outcome[, outcome_col])) # changing values to numeric
  # Sort values and drop NAs
  outcome_sorted <- outcome[order(outcome[["State"]],outcome[[outcome_col]],outcome[["Hospital.Name"]],na.last = NA),]
  
  if(num=="worst"){
    result<-outcome_sorted[tapply(1:nrow(outcome_sorted),outcome_sorted[["State"]],function(x) tail(x,n=1)),] 
  } else if (num=="best"){
    result<-outcome_sorted[tapply(1:nrow(outcome_sorted),outcome_sorted[["State"]],function(x) x[1]),] 
  } else{
    result<-outcome_sorted[tapply(1:nrow(outcome_sorted),outcome_sorted[["State"]],function(x) x[num]),]
  }
  
  
  #result<-outcome_sorted[tapply(1:nrow(outcome_sorted),outcome_sorted[["State"]],function(x) tail(x,n=1)),] 
  # The problem is that results contains NAs where num is higher than the number of hospitals in the state. It's not the most elegant solution, but we can clean it up manually by mapping ordered list of states with NAs in result dataframe
  
  states_list <- unique(outcome_sorted[["State"]])
  idx <- which(is.na(result$State), arr.ind = TRUE)
  result[idx,1]<-states_list[idx]
  # dropping indexes for prettier output
  rownames(result) <- NULL
  result
}