
settingsPath <- function(detectionsFile, projectName, date, timeChar, run) {
  path <- file.path(dirname(detectionsFile),
                    paste(projectName,
                          date,
                          timeChar,
                          paste0('Run',run),
                          'Settings.csv', sep = '_'))
  return(path)
}



