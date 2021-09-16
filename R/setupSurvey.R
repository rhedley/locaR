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

  #Get named vector of survey paths.
  sp <- surveyPaths(folder = folder, projectName = projectName, date = date,
                    time = time, run = run)

  #If run == 1 and survey folder exists, all the function does is check
  #folder structure for validity.
  if(run == 1 & dir.exists(sp['surveyFolder'])) {
    #If survey folder already exists, check its structure and print a statement.
    check <- all(dir.exists(c(sp['runFolder'], sp['specFolder'], sp['locFolder'])),
                 file.exists(sp['detectionsFile']),
                 is.null(channelsFile) | file.exists(channelsFile),
                 file.exists(sp['settingsFile']))

    message('Run == 1 and survey folder (i.e. /folder/date_time) exists.\n
                          New files not written. Folder structure appears to be ',
            ifelse(check,'correct','incorrect'))

    if(file.exists(sp['settingsFile'])) {
      settings <- read.csv(sp['settingsFile'], stringsAsFactors = F)
      return(settings)
    } else {return(NA)}
  }

  #If run > 1 & surveyFolder doesn't exist, throw an error.
  if(run > 1 & !dir.exists(sp['surveyFolder'])) {
    stop('If run > 1, survey folder (i.e. folder/date_time) must exist.')
  }

  #Create folder structure. Note that dir.create fails if a folder already exists.
  #User will see warnings.
  dir.create(sp['surveyFolder'])
  dir.create(sp['runFolder'])
  dir.create(sp['specFolder'])
  dir.create(sp['locFolder'])

  #write empty detections file if one doesn't exist.
  if(file.exists(sp['detectionsFile'])) {
    message('Detections file already exists. File not overwritten.')
  } else {
    #create and write empty detections file.
    detections <- read.csv(system.file('data', 'Empty_Detections.csv', package = 'solo'))
    write.csv(detections, sp['detectionsFile'], row.names = F)
  }

  #create and write empty channels file if channels is NULL.
  if(is.null(channelsFile)) {
    channels <- read.csv(system.file('data', 'Empty_Channels.csv', package = 'solo'))
    write.csv(channels, sp['channelsFile'], row.names = F)
  }


  settings <- createSettings(projectName = projectName,
                             run = run,
                             detectionsFile = sp['detectionsFile'],
                             coordinatesFile = coordinatesFile,
                             siteWavsFolder = siteWavsFolder,
                             adjustmentsFile = ifelse(is.null(adjustmentsFile), '', adjustmentsFile),
                             channelsFile = ifelse(is.null(channelsFile),
                                                   sp['channelsFile'], channelsFile),
                             date = date,
                             time = time,
                             surveyLength = surveyLength,
                             margin = margin,
                             zMin = zMin,
                             zMax = zMax,
                             resolution = resolution,
                             buffer = buffer, write.csv = FALSE)

  #Create and write Settings File if one does not exist.
  if(file.exists(sp['settingsFile'])) {
    message('Settings file already exists. File not overwritten.')
  } else {
    write.csv(settings, file = sp['settingsFile'], row.names = F)
  }

  message('Survey successfully created at ', sp['surveyFolder'])

  return(settings)
}




