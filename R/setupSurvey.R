#Function to set up a new "survey" - i.e. a recording session within an array.
#By setting up the survey with this particular format and directory structure,
#the solo package can automate much of the localization workflow.

setupSurvey <- function(folder,
                        run = 1,
                        projectName,
                        coordinatesFile,
                        siteWavsFolder,
                        adjustmentsFile = NULL,
                        channelsFile = NULL,
                        date,
                        time,
                        surveyLengthInSeconds,
                        margin = 10,
                        zMin = -1,
                        zMax = 20,
                        resolution = 1,
                        buffer = 0.2) {

  #Check if base folder exists.
  if(!dir.exists(folder)) {stop('specify an existing base folder for surveys.')}

  #time to character.
  timeChar <- formatC(as.numeric(time),
                      width = 6, flag = '0', format = 'd')

  #Name for surveyFolder.
  surveyFolder = file.path(folder, paste0(date, '_', timeChar))
  #Name for runFolder.
  runFolder = file.path(surveyFolder, paste0('Run',run))
  #Name for specFolder.
  specFolder = file.path(runFolder, 'Spectrograms')
  #Name for locFolder.
  locFolder = file.path(runFolder, 'Localizations')

  #If run == 1, check that date_time folder does not exist, then create.
  if(run == 1) {
    if(dir.exists(surveyFolder)) {
      stop('If run == 1, survey folder (i.e. folder/date_time) should not exist.')
    } else {
      dir.create(surveyFolder)
      dir.create(runFolder)
      dir.create(specFolder)
      dir.create(locFolder)
    }
  }
  #If run > 1, check that date_time folder exists, and create new run folder.
  if(run > 1) {
    if(!dir.exists(surveyFolder)) {
      stop('If run > 1, survey folder (i.e. folder/date_time) must exist.')
    } else {
      dir.create(runFolder)
      dir.create(specFolder)
      dir.create(locFolder)
    }
  }

  #create empty detections file.
  detections <- read.csv(system.file('data', 'Empty_Detections.csv', package = 'solo'))

  #name for detections csv.
  detectionsFile <- file.path(runFolder,
                       paste0(projectName, "_", date, '_', timeChar, '_Run', run, '_Detections.csv'))
  #write empty detections file.
  write.csv(detections, detectionsFile, row.names = F)

  #create empty channels file if channels is NULL.
  if(is.null(channelsFile)) {
    channels <- read.csv(system.file('data', 'Empty_Channels.csv', package = 'solo'))

    #name for channels csv.
    channelsFile <- file.path(surveyFolder,
                              paste0(projectName, "_", date, '_',
                                     timeChar, '_Channels.csv'))
    #write empty channels file.
    write.csv(channels, channelsFile, row.names = F)
  }


  settings <- createSettings(projectName = projectName,
                             run = run,
                             detectionsFile = detectionsFile,
                             coordinatesFile = coordinatesFile,
                             siteWavsFolder = siteWavsFolder,
                             adjustmentsFile = ifelse(is.null(adjustmentsFile), '', adjustmentsFile),
                             channelsFile = channelsFile,
                             date = date,
                             time = time,
                             surveyLengthInSeconds = surveyLengthInSeconds,
                             margin = margin,
                             zMin = zMin,
                             zMax = zMax,
                             resolution = resolution,
                             buffer = buffer, write.csv = TRUE)

  print(paste0('Survey successfully created at ', surveyFolder))
  return(settings)
}




