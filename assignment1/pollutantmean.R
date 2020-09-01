pollutantmean <- function(directory, pollutant, id = 1:332){
  ## 'directory' specifies the location of csv files
  ## 'pollutant' is character vector, either "sulfate" or "nitrate"
  ## 'id' is an integer vector of all ids to be used
  
  # function returns means of pollutant across all listed monitors (ignoring the NAs)
  
  file_names <- dir(paste("./",directory, sep=''),full.names = TRUE) # getting the list of files
  tables <- lapply(file_names,read.csv) # creating a list of tables
  df <- do.call(rbind, tables) # combining tables into one df
  
  ids_df <- subset(df, (ID %in% id) & (!is.na(df[[pollutant]])), select = c(pollutant, "ID"))  #select subset with chosen ids and pollutant values
  mean(ids_df[[pollutant]])
}