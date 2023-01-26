#' Localize detected sounds
#'
#' \code{localizeSingle} is an internal function implemented within \code{localizeMultipe}. Its basic function is to take an index value corresponding to a detection, extract that detection, extract the relevant coordinates, and feed all relevant metadata into the `localize()` function.
#'
#' @param st List. Localization settings object generated using
#'     \code{\link{processSettings}}.
#' @param index Numeric. Index to be localized within a detection file.
#' @param plot Logical. Whether to plot jpegs.
#' @param InitData List. An InitData list created by running localization with
#'     keep.InitData = TRUE. Providing an InitData list saves computation time,
#'     but is only possible if the SearchGrid and stations used for localization
#'     remain unchanged. Default is NULL, which means the InitData will be
#'     calculated anew.
#' @param keep.InitData Logical. Whether to store the InitData list.
#' @param keep.SearchMap Logical. Whether to keep the SearchMap list with
#'     power estimates and coordinates of each grid cell.
#'     Should only be set to TRUE if the SearchMap is needed
#'     for some other reason (e.g. making a publication-ready figure or
#'     conducting more involved analysis with overlapping sources, etc.).
#' @return List, containing the location of the sound source (global maximum),
#'     and optionally the InitData and SearchMap lists.

localizeSingle <- function(st, index, plot = TRUE, InitData = NULL,
                           keep.InitData = TRUE, keep.SearchMap = FALSE) {

  locFolder <- file.path(st$outputFolder, 'Localizations')
  dir.create(locFolder, showWarnings = FALSE)

  #Get detections data frame
  d <- st$detections

  #Check if the row index has data.
  if(is.na(d$Station1[index])) {
    location <- data.frame(Easting = NA, Northing = NA, Elevation = NA, Power = NA)
    return(list(location = location))
  }

  #Extract row with data.
  row <- d[index,]

  #Get station names
  stations <- sort(as.vector(as.matrix(row[1, paste0('Station', 1:6)])))
  stations <- stations[stations != "" & !is.na(stations)]

  #Create coordinates to be passed to localize().
  row.names(st$files) <- st$files$Station
  coordinates <- st$files[stations,c('Station','Easting', 'Northing', 'Elevation')]

  #create wavList to be passed to localize().
  #Station names
  names <- row.names(coordinates)

  #File paths
  paths <- st$files[names,'Path']

  #Channel to read.
  channels <- st$files[names,'Channel']

  #Amount to adjust start time (taking into account file names as well as user-specified adjustments)
  adjustments <- st$files[names,'Adjustment']

  wavList <- createWavList(paths = paths, names = names, from = row$From, to = row$To, buffer = st$buffer,
                           channels = channels, adjustments = adjustments, index = index)

  #make name for jpeg.
  jpegName <- paste0(formatC(index,width=4,flag='0'), '.jpeg')

  #Deal with NA, NULL or '' soundSpeed values.
  soundSpeed <- st$soundSpeed

  if(is.null(soundSpeed)) {
    soundSpeed <- substitute()
  } else {
    if(is.na(st$soundSpeed)) {soundSpeed <- substitute()} else {
      if(st$soundSpeed == '') {soundSpeed <- substitute()}
    }
  }


  OUT <- localize(wavList = wavList, coordinates = coordinates,
                  margin = st$margin, zMin = st$zMin, zMax = st$zMax,
                  resolution = st$resolution, F_Low = row$F_Low,
                  F_High =  row$F_High, locFolder = locFolder,
                  jpegName = jpegName, tempC = st$tempC, soundSpeed = soundSpeed, plot = plot,
                  InitData = InitData, keep.InitData = keep.InitData,
                  keep.SearchMap = keep.SearchMap)

  return(OUT)
}

