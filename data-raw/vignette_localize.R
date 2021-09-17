#vignette using solo::localize().

#loop to read mp3 files.

devtools::load_all()


#prepare the necessary pieces to use the createWavList function.
#Two basic things needed for now: file paths to each file and file names.

#paths to example data provided in the solo package.
filepaths = list.files(system.file('data', package = 'solo'), pattern = '.mp3', full.names = T)

#Add station names to wavList. Note the order of the vector stationNames must be the
#same as the order of the file paths. The following code maintains the order.
stationNames <- sapply(strsplit(basename(filepaths), '_'), '[[', 1)

#Apply these as names to the filepaths vector (useful later).
names(filepaths) <- stationNames

#read coordinates.

coordinates <- read.csv(system.file('data', 'Vignette_Coordinates.csv', package = 'solo'), stringsAsFactors = F)

#add stations as row names (useful for later).
row.names(coordinates) <- coordinates$Station

#read detections.

detections <- read.csv(system.file('data', 'Vignette_Detections_20200617_090000.csv', package = 'solo'), stringsAsFactors=F)

#specify locFolder, where jpegs will be created. For this example, I created a folder in my D:/ drive.

locFolder <- "D:/soloVignette"

#now use a loop to localize.

i <- 1
for(i in 1:nrow(detections)) {
  row <- detections[i,]

  if(row$Station1 == "" | is.na(row$Station1)) {next}

  #get names of relevant stations for this detection.
  stationSubset <- unlist(row[1,paste0('Station',1:6)])
  #remove NA stations, if applicable.
  stationSubset <- stationSubset[!is.na(stationSubset)]
  stationSubset <- stationSubset[stationSubset != '']


  #make a new wavList containing only the stations of interest.

  pathSubset = filepaths[stationSubset]


  #use createWavList() to create a list of wave objects.
  #The arguments from and to are from row$From and row$To.
  #Buffer is set to 0.2 seconds (added to either side of the detection)
  #channels can be set to NULL, since we want to use the left channel (default).
  #adjustments can be set to NULL, since all files were well synchronized in advance.
  #We can set index = i, so that if there is an error, we can pinpoint which detection
  #it came from.
  wl <- createWavList(paths = pathSubset, names = stationSubset,
                      from = row$From, to = row$To, buffer = 0.2, index=i)

  #Get low and high frequency.
  F_Low <- row$F_Low
  F_High <- row$F_High

  #make a new coordinates data.frame with only relevant stations.
  #Subsetting by the stations vector ensures that the order is the
  #same as the wl object.

  crd <- coordinates[stationSubset,]

  #Create jpeg name.
  jpg <- paste0(formatC(i,width=4,flag='0'), '.jpeg')

  #localize(). Will leave most parameters at their default values.
  loc <- localize(wavList = wl, coordinates = crd, locFolder = locFolder,
                  F_Low = F_Low, F_High = F_High, jpegName = jpg, keep.SearchMap = T)

  if(i == 1) {OUT = cbind(row,loc$location)} else {OUT = rbind(OUT, cbind(row,loc$location))}

}

write.csv(OUT, file.path(locFolder, 'vignette_localizations.csv'))

