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
#'     Optional argument.
#' @param channelsFile Character. File path to the adjustments file (csv). If missing,
#'     an empty channels file (csv) will be created.
#' @param date Numeric. Eight digit number representing a date in the format
#'     YYYYMMDD.
#' @param time Numeric. Five or six digit number representing the start time
#'     of a recording session (90000 = 09:00:00, and 160000 = 16:00:00).
#' @param tempC Numeric. Temperature in degrees C, which is used to calculate
#'     the speed of sound in air using the equation 331.45*sqrt(1+tempC/273.15).
#' @param soundSpeed Numeric. The speed of sound in meters per second. If missing,
#'     the speed of sound is calculated based on the specified
#'     temperature (assuming the transmission medium is air). If soundSpeed is
#'     specified, the tempC value is over-ridden.
#' @param surveyLength,margin,zMin,zMax,resolution,buffer Arguments describing the area to be searched
#'     for sound sources. Passed to \code{\link{createSettings}}.
#' @return data.frame containing the settings generated using \code{\link{createSettings}}.
#'     This data.frame is identical to that produced by reading the settingsFile csv, which
#'     is also written to file.
#' @examples
#'     survey <- setupSurvey(folder = tempdir(), projectName = 'Ex', run = 1,
#'         coordinatesFile = system.file('extdata', 'Vignette_Coordinates.csv', package = 'locaR'),
#'         siteWavsFolder = tempdir(),
#'         date = '20200617', time = '090000', surveyLength = 7)
#' @export
setupSurvey <- function(folder,
                        projectName,
                        run = 1,
                        coordinatesFile,
                        siteWavsFolder,
                        adjustmentsFile,
                        channelsFile,
                        date,
                        time,
                        tempC = 15,
                        soundSpeed,
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
                 ifelse(missing(channelsFile),TRUE,file.exists(channelsFile)),
                 file.exists(sp['settingsFile']))

    message('Run == 1 and survey folder (i.e. /folder/date_time) exists.\nNew files not written. Folder structure appears to be ', ifelse(check,'correct','incorrect'))

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
    detections <- read.csv(system.file('extdata', 'Empty_Detections.csv', package = 'locaR'))
    write.csv(detections, sp['detectionsFile'], row.names = F)
  }

  #create and write empty channels file if channels is NULL.
  if(missing(channelsFile)) {
    channels <- read.csv(system.file('extdata', 'Empty_Channels.csv', package = 'locaR'))
    write.csv(channels, sp['channelsFile'], row.names = F)
  }

  adjustmentsFile <- ifelse(missing(adjustmentsFile), substitute(), adjustmentsFile)

  soundSpeed <- ifelse(missing(soundSpeed), substitute(), soundSpeed)

  settings <- createSettings(projectName = projectName,
                             run = run,
                             detectionsFile = sp['detectionsFile'],
                             coordinatesFile = coordinatesFile,
                             siteWavsFolder = siteWavsFolder,
                             adjustmentsFile = adjustmentsFile,
                             channelsFile = ifelse(missing(channelsFile),
                                                   sp['channelsFile'], channelsFile),
                             date = date,
                             time = time,
                             tempC = tempC,
                             soundSpeed = soundSpeed,
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




