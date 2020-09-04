
## best function returns the first hospital (by alphabetical order) with the lowest mortality rate in a chosen outcome and located in a chosen state 
best<- function(state,outcome_name){
  # ''state'' -- the 2-character abbreviated name of a state
  # ''outcome'' -- one of the following ("heart attack", "heart failure", "pneumonia")
  
  outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  # First, we define the list of possible states and outcomes. For outcomes, it's also necessary to define the corresponding column names (as there are only 3 possible outcomes, it's faster than reformatting the string)
  
  states_list <- unique(outcome[,7]) 
  outcomes_list <- c("heart attack", "heart failure", "pneumonia")
  outcomes_columns <- c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack","Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure","Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
  outcome_col <- NULL
  
  # Validating the entered values
  if(!state %in% states_list){
    stop("invalid state")
  } 
  if(!outcome_name %in% outcomes_list){
    stop("invalid outcome")
  } else {
    # Finding a corresponding column for chosen outcome
    for(j in 1:length(outcomes_list)){
      if(outcome_name==outcomes_list[j]) {outcome_col<-outcomes_columns[j]}
    }
  }
  
  state_outcome <- subset(outcome,outcome$State==state) # selecting only the hospitals located in the chosen state
  state_outcome[, outcome_col] <- suppressWarnings(as.numeric(state_outcome[, outcome_col])) # changing values to numeric
  
  # To deal with ties, we will preliminary sort the data frame by alphabetical order and only then find the best score
  outcome_sorted <- state_outcome[order(state_outcome$Hospital.Name),]
  # Initializing the best score and best name variables
  best_num <- outcome_sorted[[outcome_col]][[1]]
  best_name <- outcome_sorted$Hospital.Name[[1]]
  # Searching for better score in sorted data frame
  for(i in 1:nrow(outcome_sorted)){
    if(!is.na(outcome_sorted[[outcome_col]][[i]]) & (outcome_sorted[[outcome_col]][[i]] < best_num) ){
      best_num<-outcome_sorted[[outcome_col]][[i]]
      best_name<-outcome_sorted$Hospital.Name[[i]]
    }
  }
  best_name # returning the result
}
```