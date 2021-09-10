#' Process settings file to extract relevant information.
#'
#' \code{processSettings} reads information from a settings file (csv) and
#' combines them into a list for subsequent localization.
#'
#' @param settingsFile Filepath to the settings file (csv).
#' @param settings data.frame created either by reading a settings file (csv) or
#'     using the \code{\link{createSettings}} function.
#' @param getFilepaths Logical, indicating whether to add filepath information
#'     using \code{\link{getFilepaths}}.
#' @return A list with information needed for sound localization, including
#'     microphone coordinates, the existing detections, channels to use
#'     for each recording unit, and information specifying the size and
#'     resolution of the grid within which to localize sound sources.
processSettings <- function(settingsFile = NULL, settings = NULL, getFilepaths = FALSE) {

  #Either use settings or read settingsFile.
  if(is.data.frame(settings)) {Settings <- settings} else {
    if(file.exists(settingsFile)) {
      Settings <- read.csv(settingsFile, stringsAsFactors=F)
    } else {stop('settingsFile or settings must be specified.')}
  }

  Settings <- split(Settings$Value, f = Settings$Setting)

  coordsFile <- Settings$CoordinatesFile

  siteFolder <- Settings$SiteWavsFolder

  detectionsFile <- Settings$DetectionsFile

  outputFolder <- dirname(Settings$DetectionsFile)

  adjustmentsFile <- Settings$AdjustmentsFile

  channelsFile <- Settings$ChannelsFile

  detectionsFile <- Settings$DetectionsFile

  margin <- as.numeric(Settings$Margin)

  zMin <- as.numeric(Settings$Zmin)

  zMax <- as.numeric(Settings$Zmax)

  resolution <- as.numeric(Settings$Resolution)

  buffer <- as.numeric(Settings$Buffer) #Can this be deprecated?

  date <- Settings$Date

  time <- Settings$Time

  time <- formatC(as.numeric(time), width=6, flag="0", format = 'd')

  surveyLength <- as.numeric(Settings$SurveyLengthInSeconds)

  coords <- read.csv(coordsFile, stringsAsFactors=F)
  coords <- coords[,c('Station', 'Zone', 'Easting', 'Northing', 'Elevation')]

  if(file.exists(detectionsFile)) {
    detections <- read.csv(detectionsFile, stringsAsFactors=F)
  } else {detections <- NA}

  if(!is.na(adjustmentsFile) & adjustmentsFile != "" & !is.null(adjustmentsFile)) {
    adjustments <- read.csv(adjustmentsFile, stringsAsFactors=F)
  } else {adjustments <- NA}

  channels <- read.csv(channelsFile, stringsAsFactors=F)

  if(nrow(channels) > 0) {
    coords <- coords[coords$Station %in% channels$Station,]

    #Check that all stations listed in channels file have coordinates.
    if(sum(channels$Station %in% coords$Station) != nrow(channels)) {
      stop('Some stations listed in channelsFile were missing coordinates')
    }
    if(sum(is.na(coords$Easting)) > 0 |
       sum(is.na(coords$Northing)) > 0 |
       sum(is.na(coords$Elevation)) > 0) {
      stop('Some stations listed in channelsFile were missing coordinates')
    }

    #Check that all stations listed in detections file have coordinates.
    statVec <- unique(unlist(c(detections[paste0('Station', 1:6)])))
    statVec <- statVec[!is.na(statVec) & statVec != '']
    if(sum(statVec %in% coords$Station) != length(statVec)) {
      stop('Some stations listed in detectionsFile were missing coordinates')
    }
  }

  st <- list(detections = detections,
            coords=coords,
            channels=channels,
            adjustments=adjustments,
            siteFolder = siteFolder,
            date=date,
            time=time,
            outputFolder = outputFolder,
            resolution = resolution,
            margin = margin,
            zMin = zMin,
            zMax = zMax,
            buffer = buffer,
            surveyLength = surveyLength)

  if(getFilepaths) {
    st$files = getFilepaths(settings = st)
  }

  return(st)

}
