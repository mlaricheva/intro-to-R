read_data<-function(directory){
  file_names <- dir(paste("./",directory, sep=''),full.names = TRUE) # getting the list of files
  tables <- lapply(file_names,read.csv) # creating a list of tables
  df <- do.call(rbind, tables) # combining tables into one df
  df
}