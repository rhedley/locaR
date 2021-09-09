#createSettings

createSettings <- function(projectName,
                           run = 1,
                          detectionsFile,
                          coordinatesFile,
                          siteWavsFolder,
                          adjustmentsFile,
                          channelsFile,
                          date,
                          time,
                          surveyLengthInSeconds,
                          margin = 10,
                          zMin = -1,
                          zMax = 10,
                          resolution = 1,
                          buffer = 0.2, write.csv = TRUE) {

  #create settings data frame.
  settings <- data.frame(Setting = c('DetectionsFile',
                                     'CoordinatesFile',
                                     'SiteWavsFolder',
                                     'AdjustmentsFile',
                                     'ChannelsFile',
                                     'Date',
                                     'Time',
                                     'SurveyLengthInSeconds',
                                     'Margin',
                                     'Zmin',
                                     'Zmax',
                                     'Resolution',
                                     'Buffer'),
                        Value = c(detectionsFile,
                                  coordinatesFile,
                                  siteWavsFolder,
                                  adjustmentsFile,
                                  channelsFile,
                                  date,
                                  formatC(time, width=6, flag='0', format='d'),
                                  surveyLengthInSeconds,
                                  margin,
                                  zMin,
                                  zMax,
                                  resolution,
                                  buffer))

  #write to file, if needed.
  if(write.csv) {
    path = paste0(dirname(detectionsFile), '/', projectName, '_', date, '_', formatC(time, width=6, flag='0', format='d'), '_Run', run, '_Settings.csv')

    write.csv(settings, file = path, row.names = F)
  }

  return(settings)
}







