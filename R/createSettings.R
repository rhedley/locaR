#' Create settings file (csv) or data frame by defining the localization settings.
#'
#' \code{createSettings} takes a series of arguments and creates a data
#' frame (or csv) with standard structure that can be read by other functions in
#' the locaR package.
#'
#' @param projectName Character. A string specifying the name of a project.
#' @param run Numeric. Within each survey, start with run = 1, then count
#'     upwards. So, run 2 would be used to re-localize sounds that were poorly
#'     localized in run 1, etc. Running them again with slightly different
#'     settings (e.g. start/end times or low/high frequencies)
#'     can improve results.
#' @param detectionsFile Character. File path to the detections file (csv).
#' @param coordinatesFile Character. File path to the coordinates file (csv).
#' @param siteWavsFolder Character. Folder path of the directory containing
#'     audio files. The folder path will be searched recursively if using
#'     \code{\link{localizeSingle}} or \code{\link{localizeMultiple}}.
#' @param adjustmentsFile Character. File path to the adjustments file (csv).
#'     Set to NULL if no adjustments to file start times are needed.
#' @param channelsFile Character. File path to the channels file (csv),
#'     specifying which channel (1 or 2) to use for each recording unit.
#' @param date Numeric. Eight digit number representing a date in the format
#'     YYYYMMDD.
#' @param time Numeric. Five or six digit number representing the start time
#'     of a recording session (90000 = 09:00:00, and 160000 = 16:00:00).
#' @param tempC Numeric. Temperature in degrees C, which is used to calculate
#'     the speed of sound in air using the equation 331.45*sqrt(1+tempC/273.15).
#' @param soundSpeed Numeric. The speed of sound in meters per second. Default is
#'     NULL, in which case the speed of sound is calculated based on the specified
#'     temperature (assuming the transmission medium is air). If soundSpeed is
#'     specified, the tempC value is over-ridden.
#' @param surveyLength Numeric. Length of the survey, in seconds.
#' @param margin distance (in meters) to extend the search grid
#'     beyond the x-y limits of the microphone locations. The same buffer is
#'     applied to x and y coordinates.
#' @param zMin distance (in meters) to begin grid search relative to the
#'     microphone with the lowest elevation. Typically a small negative number
#'     to ensure that the grid search begins slightly below the lowest
#'     microphone.
#' @param zMax distance (in meters) to end search relative to the microphone
#'     with the highest elevation. Typically a positive number to ensure
#'     that the grid search ends well above the highest microphone.
#' @param resolution resolution of the search map, in meters.
#' @param buffer Amount of time (in seconds) to expand each detection. This
#'     accounts for imprecise time boundaries, and also the differences imposed
#'     by time delays between different microphones (e.g. two microphones
#'     separated by some amount will receive the same sound at different times).
#' @param write.csv Logical. Whether or not to write a settingsFile csv. The
#'     csv will be written to the same directory as the detections file.
#' @return A data frame with columns Setting and Value specifying the value
#'     of each setting needed for localization.
createSettings <- function(projectName,
                           run = 1,
                          detectionsFile,
                          coordinatesFile,
                          siteWavsFolder,
                          adjustmentsFile,
                          channelsFile,
                          date,
                          time,
                          tempC = 15,
                          soundSpeed = NULL,
                          surveyLength,
                          margin = 10,
                          zMin = -1,
                          zMax = 10,
                          resolution = 1,
                          buffer = 0.2, write.csv = FALSE) {

  #create settings data frame.
  settings <- data.frame(Setting = c('DetectionsFile',
                                     'CoordinatesFile',
                                     'SiteWavsFolder',
                                     'AdjustmentsFile',
                                     'ChannelsFile',
                                     'Date',
                                     'Time',
                                     'tempC',
                                     'soundSpeed',
                                     'SurveyLength',
                                     'Margin',
                                     'Zmin',
                                     'Zmax',
                                     'Resolution',
                                     'Buffer'),
                        Value = c(detectionsFile,
                                  coordinatesFile,
                                  siteWavsFolder,
                                  ifelse(is.null(adjustmentsFile), '', adjustmentsFile),
                                  channelsFile,
                                  date,
                                  formatC(time, width=6, flag='0', format='d'),
                                  tempC,
                                  ifelse(is.null(soundSpeed), '', soundSpeed),
                                  surveyLength,
                                  margin,
                                  zMin,
                                  zMax,
                                  resolution,
                                  buffer), stringsAsFactors = F)

  #write to file, if needed.
  if(write.csv) {
    sp <- surveyPaths(folder = dirname(dirname(dirname(detectionsFile))), projectName, date,
                        time = time, run = run)

    write.csv(settings, file = sp['settingsFile'], row.names = F)
  }

  return(settings)
}







