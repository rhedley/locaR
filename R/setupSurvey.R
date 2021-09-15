#' Set up a new "survey" with a standardized structure recognized by the package.
#'
#' \code{setupSurvey} sets up the folder structure for a new "survey", which
#' corresponds to a single recording session. By setting up
#' a standardized folder structure, the package functions can carry out much of the
#' data wrangling automatically using the \code{localizeSingle} and \code{localizeMultiple}
#' functions. The extra work required to set up surveys in a standard format can
#' save time later on.
#'
#' @param folder Character. Path to the directory where the survey will be created.
#' @param projectName,run,coordinatesFile,siteWavsFolder Arguments passed to
#'     \code{\link{createSettings}}
#' @param adjustmentsFile Character. File path to the adjustments file (csv).
#'     Set to NULL if no adjustments to file start times are needed.
#' @param channelsFile Character. File path to the adjustments file (csv). If NULL,
#'     an empty channels file (csv) will be created.
#' @param date Numeric. Eight digit number representing a date in the format
#'     YYYYMMDD.
#' @param time Numeric. Five or six digit number representing the start time
#'     of a recording session (90000 = 09:00:00, and 160000 = 16:00:00).
#' @param surveyLength,margin,zMin,zMax,resolution,buffer Arguments describing the area to be searched
#'     for sound sources. Passed to \code{\link{createSettings}}.
#' @return data.frame containing the settings generated using \code{\link{createSettings}}.
#'     This data.frame is identical to that produced by reading the settingsFile csv, which
#'     is also written to file.
#' @export
setupSurvey <- function(folder,
                        projectName,
                        run = 1,
                        coordinatesFile,
                        siteWavsFolder,
                        adjustmentsFile = NULL,
                        channelsFile = NULL,
                        date,
                        time,
                        surveyLength,
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
  #name for detections csv.
  detectionsFile <- file.path(runFolder,
                              paste0(projectName, "_", date, '_', timeChar,
                                     '_Run', run, '_Detections.csv'))

  #If run == 1, check that date_time folder does not exist, then create.
  if(run == 1) {

    if(dir.exists(surveyFolder)) {
      #If survey folder already exists, check its structure and print a statement.
      check <- all(dir.exists(c(runFolder, specFolder, locFolder)),
                              file.exists(detectionsFile),
                              is.null(ChannelsFile) | file.exists(ChannelsFile))

      message('Run == 1 and survey folder (i.e. /folder/date_time) exists.\n
                          Folders not over-written. Folder structure appears to be ',
                          ifelse(check,'correct','incorrect'))

      #return the settings file.
      settingsPath <-  settingsPath(detectionsFile, projectName, date, timeChar, run)

      if(file.exists(settingsPath)) {
        settings <- read.csv(settingsPath, stringsAsFactors = F)
        return(settings)
      } else {return(NA)}
    }

    if(!dir.exists(surveyFolder)) {
      #If the survey folder does not exist, create the appropriate folder structure.
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
                             surveyLength = surveyLength,
                             margin = margin,
                             zMin = zMin,
                             zMax = zMax,
                             resolution = resolution,
                             buffer = buffer, write.csv = TRUE)

  message('Survey successfully created at ', surveyFolder)

  return(settings)
}




