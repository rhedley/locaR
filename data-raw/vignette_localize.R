#vignette using solo::localize().

#loop to read mp3 files.

devtools::load_all()

#Create list of file names (these are the example data provided in the solo package).
files <- paste0('Ex-', 1:9, '_0+1_20200617$090000.mp3')

#initialize wavList
wavList = list()

for(i in 1:9) {
  wav <- tuneR::readMP3(system.file('data',files[i], package = 'solo'))

  wavList <- append(wavList, wav)

}

#clean up workspace.
rm(wav)

#Add station names to wavList.
stations <- sapply(strsplit(files, '_'), '[[', 1)

names(wavList) <- stations

#clean up workspace.
rm(stations)

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

  #Get start and end of detection.
  #Note: I "Buffer" them out by 0.2 seconds, to account for the fact that
  #start and end times may not be exact, and the transmission delay means the
  #sound arrives at different times to different microphones.
  from <- row$From - 0.2
  to <- row$To + 0.2

  #Get low and high frequency.
  F_Low <- row$F_Low
  F_High <- row$F_High

  #get names of relevant stations.
  stations <- unlist(row[1,paste0('Station',1:6)])

  #remove NA stations, if applicable.
  stations <- stations[!is.na(stations)]
  stations <- stations[stations != '']

  #make a new wavList containing only the stations of interest.

  wl <- wavList[stations]

  #extract relevant times from wavs.
  for(j in 1:length(wl)) {
    wl[[j]] <- tuneR::extractWave(wl[[j]], from = from, to = to, xunit = 'time')
  }

  #make a new coordinates data.frame with only relevant stations.
  #Subsetting by the stations vector ensures that the order is the
  #same as the wl object.

  crd <- coordinates[stations,]

  #Create jpeg name.
  jpg <- paste0(formatC(i,width=4,flag='0'), '.jpeg')

  #localize(). Will leave most parameters at their default values.
  loc <- localize(wavList = wl, coordinates = crd, locFolder = locFolder,
                  F_Low = F_Low, F_High = F_High, jpegName = jpg, keep.SearchMap = T)

  if(i == 1) {OUT = cbind(row,loc$location)} else {OUT = rbind(OUT, cbind(row,loc$location))}

}

write.csv(OUT, file.path(locFolder, 'vignette_localizations.csv'))

