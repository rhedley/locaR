#checkSettings
settings <- "D:/School stuff/Manuscripts/3DLocalization/Surveys/20200617_0900/Run1/TDLO_20200617_0900_Run1_Settings.csv"

#Probably need to just cut out output folder, and extend detections file, then create output folder
#where detections file is located.

checkSettings <- function(settings) {

  #If it's a file path, read it to data frame.
  if(is.character(settings)) {
    if(!file.exists(settings)) {stop('Settings must be either a data frame or a valid csv file path')}
    settings <- read.csv(settings, stringsAsFactors = FALSE)
  }

  st <- split(settings$Value, f=settings$Setting)

  #check that various files exist.
  if(!file.exists(st$AdjustmentsFile) & !is.null(st$AdjustmentsFile)) {stop('Invalid adjustments file')}
  if(!file.exists(st$ChannelsFile) & !is.null(st$ChannelsFile)) {stop('Invalid channels file')}
  if(!file.exists(st$CoordinatesFile)) {stop('Coordinates file must be specified')}
  if(!file.exists(st$DetectionsFile)) {stop('Invalid detections file')}

  #check that output and site wavs directory exists.
  if(!dir.exists(st$OutputFolder)) {stop('Invalid output folder')}
  if(!dir.exists(st$SiteWavsFolder)) {stop('Invalid site wavs folder')}

  #check buffer, date, time, resolution, margin.
  if(is.na(tryCatch(as.numeric(st$Buffer), error = function(e) NA))) {stop('Buffer must be numeric')}
  if(is.na(tryCatch(as.numeric(st$Margin), error = function(e) NA))) {stop('Margin must be numeric')}
  if(is.na(tryCatch(as.numeric(st$Resolution), error = function(e) NA))) {stop('Resolution must be numeric')}
  if(is.na(tryCatch(as.numeric(st$Date), error = function(e) NA))) {stop('Date must be numeric')}
  if(is.na(tryCatch(as.numeric(st$Time), error = function(e) NA))) {stop('Time must be numeric')}
  if(is.na(tryCatch(as.numeric(st$Zmax), error = function(e) NA))) {stop('Zmax must be numeric')}
  if(is.na(tryCatch(as.numeric(st$Zmin), error = function(e) NA))) {stop('Zmin must be numeric')}
  if(is.na(tryCatch(as.numeric(st$SurveyLengthInSeconds), error = function(e) NA))) {stop('SurveyLengthInSeconds must be numeric')}

}





