#emptySettings

emptySettings <- function(projectName,
                          outputFolder,
                          detectionsFile,
                          coordinatesFile,
                          siteWavsFolder,
                          adjustmentsFile,
                          channelsFile,
                          date,
                          time,
                          surveyLengthInSeconds,
                          margin=10,
                          zMin=-1,
                          zMax=10,
                          resolution=1,
                          buffer=0.2) {






  settings <- data.frame(Setting = c('OutputFolder',
                                     'DetectionsFile',
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
                        Value = c(outputFolder,
                                  detectionsFile,
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

  path = paste0(outputFolder, '/', projectName, '_', date, '_', formatC(time, width=6, flag='0', format='d'), '_Settings.csv')

  write.csv(settings, file = path, row.names = F)

  return(path)
}

?formatC






