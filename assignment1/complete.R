complete <- function(directory, id = 1:332){
  ## 'directory' specifies the location of csv files
  ## 'id' is an integer vector of all ids to be used
  
  # function returns the dataframe of the following structure: 
  ## id nobs
  ## 1 117
  ## 2 1041
  
  ## where nobs -- the number of totally observed cases
  
  source("read_data.R")
  df <- read_data("specdata") ## reading data
  
  result = NULL # variable for a future dataframe with results
  
  for (i in id){
    ids_df <- subset(df, (ID == i) & (!is.na(df[["nitrate"]])) & (!is.na(df[["sulfate"]])), select = c("ID", "sulfate"))  #select subset with no na's
    nobs<-nrow(ids_df) # getting the length of subset df
    result <- rbind(result,data.frame(i, nobs)) # saving results
  }
  result
}