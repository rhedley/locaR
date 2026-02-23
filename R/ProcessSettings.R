#' Process settings file to extract relevant information.
#'
#' \code{processSettings} reads information from a settings file (csv) and
#' combines them into a list for subsequent localization.
#'
#' @param settingsFile Filepath to the settings file (csv).
#' @param settings data.frame created either by reading a settings file (csv) or
#'     using the \code{\link{createSettings}} function. Not needed if settingsFile is specified.
#' @param getFilepaths Logical, indicating whether to add filepath information
#'     using \code{\link{getFilepaths}}.
#' @param types Character. If getFilepaths is TRUE, which types of files to look for ('wav' or 'mp3').
#' @return A list with information needed for sound localization, including
#'     microphone coordinates, the existing detections, channels to use
#'     for each recording unit, and information specifying the size and
#'     resolution of the grid within which to localize sound sources.
#' @examples
#'     #Read example data
#'     settings <- read.csv(system.file('extdata', 'Ex_20200617_090000_Settings.csv',
#'                                      package = 'locaR'), stringsAsFactors = FALSE)
#'
#'     #Over-write default values for SiteWavsFolder, CoordinatesFile, and ChannelsFile
#'     settings$Value[settings$Setting == 'SiteWavsFolder'] <-
#'                    system.file('extdata', package = 'locaR')
#'     settings$Value[settings$Setting == 'CoordinatesFile'] <-
#'                    system.file('extdata', 'Vignette_Coordinates.csv', package = 'locaR')
#'     settings$Value[settings$Setting == 'ChannelsFile'] <-
#'                    system.file('extdata', 'Vignette_Channels.csv', package = 'locaR')
#'
#'     #Run processSettings() function
#'     st <- processSettings(settings = settings, getFilepaths = FALSE)
#' @export
processSettings <- function(settingsFile, settings, getFilepaths = FALSE, types = 'wav') {

  #Either use settings or read settingsFile.
  if(missing(settingsFile) & missing(settings)) {
    stop('settingsFile or settings must be specified.')
  }

  #Read settings, or if it is not a dataframe
  if(!missing(settings)) {
    if(is.data.frame(settings)) {Settings <- settings}
  } else {
    if(file.exists(settingsFile)) {
      Settings <- read.csv(settingsFile, stringsAsFactors = FALSE)
    } else {stop('If settings is not a data frame, settingsFile must point to an existing csv file.')}
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

  buffer <- as.numeric(Settings$Buffer)

  date <- Settings$Date

  time <- Settings$Time

  time <- formatC(as.numeric(time), width=6, flag="0", format = 'd')

  tempC <- as.numeric(Settings$tempC)

  soundSpeed <- Settings$soundSpeed

  surveyLength <- as.numeric(Settings$SurveyLength)

  coords <- read.csv(coordsFile, stringsAsFactors = FALSE)
  coords <- coords[,c('Station', 'Zone', 'Easting', 'Northing', 'Elevation')]

  if(file.exists(detectionsFile)) {
    detections <- read.csv(detectionsFile, stringsAsFactors = FALSE)
    #Replace invalid characters.
    if(nrow(detections) > 0){
      detections[,paste0('Station', 1:6)][detections[,paste0('Station', 1:6)] == '' |
                                            detections[,paste0('Station', 1:6)] == 'NaN'] <- NA
      if(any(detections$to < detections$from)) {
        stop('Some detections have negative duration.')
      }
    }
  } else {detections <- NA}

  if(!is.na(adjustmentsFile) & adjustmentsFile != "") {
    adjustments <- read.csv(adjustmentsFile, stringsAsFactors = FALSE)
  } else {adjustments <- NA}

  if(!is.na(soundSpeed) & soundSpeed != "") {
    soundSpeed <- Settings$soundSpeed
  } else {soundSpeed <- NA}

  channels <- read.csv(channelsFile, stringsAsFactors = FALSE)

  if(nrow(channels) > 0) {
    coords <- coords[coords$Station %in% channels$Station,]

    #Check that all stations listed in channels file have coordinates.
    if(!all(channels$Station %in% coords$Station)) {
      stop(paste('Some stations listed in channelsFile were missing coordinates. Check stations', channels$Station[!(channels$Station %in% coords$Station)]))
    }
    if(any(is.na(c(coords$Easting, coords$Northing, coords$Elevation)))) {
      stop('Some stations listed in channelsFile were missing coordinates')
    }

    #Check that all stations listed in detections file have coordinates.
    statVec <- unique(unlist(c(detections[paste0('Station', 1:6)])))
    statVec <- statVec[!is.na(statVec)]
    if(!all(statVec %in% coords$Station)) {
      stop('Some stations listed in detectionsFile were missing coordinates. Check stations', statVec[!(statVec %in% coords$Station)])
    }
  }

  st <- list(detections = detections,
            coords=coords,
            channels=channels,
            adjustments=adjustments,
            siteFolder = siteFolder,
            date=date,
            time=time,
            tempC=tempC,
            soundSpeed=soundSpeed,
            outputFolder = outputFolder,
            resolution = resolution,
            margin = margin,
            zMin = zMin,
            zMax = zMax,
            buffer = buffer,
            surveyLength = surveyLength)

  if(getFilepaths) {
    st$files = getFilepaths(settings = st, types = types)
  }

  return(st)

}
