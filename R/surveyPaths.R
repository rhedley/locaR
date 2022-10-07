#' Get paths for standardized survey workflow.
#'
#' Function that takes arguments of a base folder and a project name, date, time,
#' and run, and returns the appropriate filepaths for a standardized survey
#' workflow.
#'
#' @param folder Character. Path to the directory where the survey is to be created.
#' @param projectName Character. A string specifying the name of a project.
#' @param date Numeric. Eight digit number representing a date in the format
#'     YYYYMMDD.
#' @param time Numeric. Five or six digit number representing the start time
#'     of a recording session (90000 = 09:00:00, and 160000 = 16:00:00).
#' @param run Numeric. Within each survey, start with run = 1, then count
#'     upwards. So, run 2 would be used to re-localize sounds that were poorly
#'     localized in run 1, etc. Running them again with slightly different
#'     settings (e.g. start/end times or low/high frequencies)
#'     can improve results.
#' @return Named vector of paths to surveyFolder, runFolder, specFolder,
#' locFolder, detectionsFile, channelsFile, and settingsFile.
#' @examples
#'     surveyPaths(folder = tempdir(), projectName = 'Ex', date = '20200617', time = '090000', run = 1)
#' @export
surveyPaths <- function(folder, projectName, date, time, run) {

  #time to character.
  timeChar <- formatC(as.numeric(time),
                      width = 6, flag = '0', format = 'd')

  #Name for surveyFolder.
  surveyFolder <- file.path(folder, paste0(date, '_', timeChar))
  #Name for runFolder.
  runFolder <- file.path(surveyFolder, paste0('Run',run))
  #Name for specFolder.
  specFolder <- file.path(runFolder, 'Spectrograms')
  #Name for locFolder.
  locFolder <- file.path(runFolder, 'Localizations')
  #name for detections csv.
  detectionsFile <- file.path(runFolder,
                              paste(projectName, date, timeChar,  paste0('Run',run),
                                    'Detections.csv', sep = '_'))

  settingsFile <- file.path(runFolder,
                    paste(projectName, date, timeChar,  paste0('Run',run),
                          'Settings.csv', sep = '_'))

  channelsFile <- file.path(surveyFolder,
                            paste(projectName, date, timeChar, 'Channels.csv', sep="_"))

  vec <- c(surveyFolder, runFolder, specFolder, locFolder, detectionsFile, settingsFile, channelsFile)
  names(vec) <- c('surveyFolder', 'runFolder', 'specFolder', 'locFolder',
                  'detectionsFile', 'settingsFile', 'channelsFile')

  return(vec)

}













