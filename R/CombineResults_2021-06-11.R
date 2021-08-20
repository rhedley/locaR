
#Function to search for and combine runs into a single dataframe, and write.

#Note, csv files should have a column called "Keep", with Y for yes (Also accepted
#Yes or y or yes).

#paths = full paths to csv files.

combineResults = function(paths) {
  
  i=1
  for(i in 1:length(paths)) {
    data = read.csv(paths[i], stringsAsFactors = F)
    ind = data$Keep %in% c('y', 'Y', 'Yes', 'yes')
    data = data[ind,]
    
    if(i==1) {OUT = data} else {OUT = rbind(OUT, data)}
  }
  
  #Sort. First by start time, then by species. Note, an individual identifier would be nice.
  OUT = OUT[order(OUT$First),]
  OUT = OUT[order(OUT$Species),]
  
  return(OUT)
  
}


