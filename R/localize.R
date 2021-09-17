#' Localize detected sounds
#'
#' \code{localize} and the two related functions \code{localizeSingle} and \code{localizeMultiple}
#' are the basic functions for localizing sounds. They take audio data as inputs, alongside relevant
#'  metadata (e.g. coordinates and a variety of settings), and estimate the location of the
#'  dominant sound source. The \code{localize} function takes as arguments the minimal
#' amount of information needed for localization. Localization is conducted on the
#' full duration of the Wave objects in wavList. Effectively this means the user
#' must wrangle the data and clip the audio themselves, but this affords the greatest
#' flexibility in terms of how the user chooses to organize their data.
#' The \code{localizeSingle} and \code{localizeMultiple} functions, in contrast,
#' automate much of the data wrangling process, but require data to be organized in a
#' very specific way (e.g. folder structure, file structures). Thus, the latter two
#' functions tradeoff flexibility for increased automation. All three functions use the
#' same underlying localization algorithm (\code{localizeSingle} and \code{localizeMultiple}
#' pass their data to \code{localize} after the data has been wrangled).
#'
#' @param wavList list of Wave objects. The name of the Wave objects MUST be
#'     present in the coordinates data.frame.
#' @param coordinates data.frame. Must contain four required columns:
#'     column Station contains a character string with names of each recording
#'     station, while Easting, Northing and Elevation contain the x, y, and z
#'     coordinates of the station, in meters (E.g. UTM coordinates).
#' @param margin,zMin,zMax,resolution Arguments describing the area to be searched
#'     for sound sources. Passed to \code{\link{makeSearchMap}}.
#' @param F_Low,F_High Numeric. The low and high frequency, in Hz, of the sound
#'     to be localized.
#' @param tempC Numeric. Temperature in degrees C, which affects the speed of sound.
#' @param plot Logical. Whether to plot jpegs.
#' @param locFolder Character. File path to the folder where localization jpegs
#'     (heatmaps and spectrograms) are to be created. Only required if plot = TRUE.
#' @param jpegName Character. Name of the jpeg, ending in extension .jpeg.
#'     Only required if plot = TRUE.
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
#' @param st List. Localization settings object generated using
#'     \code{\link{processSettings}}. Only needed for \code{localizeSingle} or
#'     \code{localizeMultiple}.
#' @param index,indices Numeric or 'all'. Indices to be localized within a detection file.
#'     Setting to 1 localizes the first row, c(7:10) localizes rows 7-10, and 'all'
#'     localizes all rows (ignoring rows that have no entry in the Station1 column).
#' @return List, containing the location of the sound source (global maximum),
#'     and optionally the InitData and SearchMap lists.
#' @export

localize <- function(wavList,coordinates,margin = 10,zMin = -1,zMax = 20,
                     resolution = 1, F_Low = 2000, F_High = 8000, tempC = 15,
                     plot = TRUE, locFolder = NULL, jpegName = '000.jpeg',
                     InitData = NULL, keep.InitData = TRUE,keep.SearchMap = FALSE) {

  #check that names of wavList correspond with names of coordinates.

  if(length(names(wavList)) < length(wavList)) {
    stop('wavList must be named.')
  }

  if(sum(!names(wavList) %in% coordinates$Station) > 0) {
    stop('Some names in wavList not found in coordinates!')
  }

  #Get station names
  stations <- names(wavList)

  #Create NodePos object from station names. This also filters out stations
  #in the coordinates that are not in wavList.
  row.names(coordinates) <- coordinates$Station
  NodePos <- as.matrix(coordinates[stations,c('Easting', 'Northing', 'Elevation')])
  colnames(NodePos) <- c('Easting', 'Northing', 'Elevation')
  row.names(NodePos) <- stations

  #Create SearchMap (Grid around Nodes, plus user-specified margins around outside)
  SearchMap <- makeSearchMap(easting = NodePos[,'Easting'],
                            northing = NodePos[,'Northing'],
                            elevation = NodePos[,'Elevation'],
                            margin = margin, zMin = zMin, zMax = zMax,
                            resolution = resolution)
  #Create Para list.
  #Get sample rate
  Fs <- wavList[[1]]@samp.rate

  #Get DataLen

  DataLen <- length(wavList[[1]]@left)

  #Define speed of sound.

  Vc <- 331.45*sqrt(1+tempC/273.15)

  #Create Para list.
  Para <- list(GCCMethod = "PHAT", Fs=Fs, DataLen=DataLen, Vc=Vc, tempC=tempC,
            FL = F_Low, FH=F_High)

  #LevelFlag (not really needed, since there is only one option)
  LevelFlag <- 2

  #Create InitData if needed.
  if(is.null(InitData)) {
    InitData = MSRP_Init(NodeInfo = list(Num = nrow(NodePos), Pos = NodePos),
                         SearchMap, Para, LevelFlag)
  } else {
    print('Inherited InitData in 0 seconds.')
  }
  #Need to add a step here to check if the provided InitData actually corresponds to the
  #area we want to search.

  #Create FrameData.
  Data <- matrix(0,nrow=nrow(NodePos), ncol=Para$DataLen)
  #Assign row names to Data - same order as NodePos.
  row.names(Data) <- row.names(NodePos)

  for(i in 1:nrow(NodePos)) {
    #Station name
    name <- row.names(NodePos)[i]

    #Subtract DC offset and round
    Data[i,] <- round(wavList[[name]]@left - mean(wavList[[name]]@left))
  }

  locstarttime <- proc.time()
  #Run MRSP
  SMap <- MSRP_RIJ_HT(NodeInfo = list(Num = nrow(NodePos), Pos = NodePos),
                     SearchMap, Data, Para, LevelFlag, InitData)
  message('Localized detection in ',round((proc.time()-locstarttime)['elapsed'],1),' seconds.')

  #Extract global maximum location.
  locationInd <- which(SMap == max(SMap), arr.ind = T)
  xInd <- SearchMap$XMap[locationInd]
  yInd <- SearchMap$YMap[locationInd]
  zInd <- SearchMap$ZMap[locationInd]
  location <- data.frame(Easting = xInd, Northing = yInd, Elevation = zInd, Power = max(SMap))

  if(plot) {
    #Check that locFolder was specified.
    if(is.null(locFolder)) {stop('Error: Specify locFolder for outputs.')}
    if(!dir.exists(locFolder)) {stop('locFolder does not exist.')}

    jpeg(file.path(locFolder, jpegName),
         width = 15, height = 15, units = 'in', res=100)
    on.exit(dev.off(), add = TRUE)
    par(mar=c(0,0,0,0))
    par(oma=c(0,0,0,0))
    m <- matrix(c(1:6,0,rep(7,4),0), ncol = 2)
    layout(m)

    #Plot 1
    validationSpec(wavList = wavList, coordinates = NodePos,
                   locationEstimate = location, tempC = tempC, F_Low = F_Low,
                   F_High = F_High, from = NULL, to = NULL)

    #Plot 2
    locHeatmap(SearchMap = SearchMap, SMap = SMap,
               NodeInfo = list(Num = nrow(NodePos), Pos = NodePos),
               location = location, mar = c(9,3,8,0))
  }

  #Return list with location data.
  OUT <- list(location = location)

  if(keep.InitData) {
    OUT <- append(OUT, list(InitData = InitData))
  }

  if(keep.SearchMap) {
    OUT <- append(OUT, list(SearchMap = SearchMap, SMap = SMap))
  }

  return(OUT)

}


#' @rdname localize
localizeSingle <- function(st, index, tempC = 15, plot = TRUE, InitData = NULL,
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

  OUT <- localize(wavList = wavList, coordinates = coordinates,
                  margin = st$margin, zMin = st$zMin, zMax = st$zMax,
                  resolution = st$resolution, F_Low = row$F_Low,
                  F_High =  row$F_High, locFolder = locFolder,
                  jpegName = jpegName, tempC = tempC, plot = plot,
                  InitData = InitData, keep.InitData = keep.InitData,
                  keep.SearchMap = keep.SearchMap)

  return(OUT)
}


#' @rdname localize
localizeMultiple = function(st, indices = 'all', tempC = 15, plot=TRUE, InitData=NULL) {

  detect <- st$detections

  #If indices is numeric, extract those first.
  if(is.numeric(indices)) {
    detect <- detect[indices,]
  }

  #Remove NULL detections.
  detect <- detect[detect$Station1 != "" & !is.na(detect$Station1),]

  #replace original.
  st$detections <- detect

  #Replace "all" with numbers.
  if(is.character(indices)) {
    if(indices == 'all') {indices <- 1:nrow(st$detections)}
  }

  #New indices to correct for removed rows.
  newIndices <- 1:nrow(st$detections)

  for(i in 1:length(newIndices)) {

    #First check whether InitData should be kept for index i

    currentRow = st$detections[newIndices[i],]

    if(i < length(newIndices)) {
      nextRow = st$detections[newIndices[i+1],]

      currentStations = as.vector(as.matrix(currentRow[,paste0('Station', 1:6)]))

      nextStations = as.vector(as.matrix(nextRow[,paste0('Station', 1:6)]))

      #Compare current stations and next stations. If they are identical, keep.InitData = TRUE
      keep.InitData = (sum(currentStations %in% nextStations) == length(currentStations) &
                         sum(nextStations %in% currentStations) == length(nextStations))
    } else {keep.InitData = TRUE}

    #InitData will generally be NULL for the first detection, inherited (sometimes) thereafter.

    loc = localizeSingle(st, index = newIndices[i], plot=plot,
                         keep.InitData = keep.InitData, InitData = InitData)

    currentRow$Easting = loc$location$Easting
    currentRow$Northing = loc$location$Northing
    currentRow$Elevation = loc$location$Elevation
    currentRow$Power = loc$location$Power
    currentRow$Keep = NA

    #If keep.InitData is false, reset InitData to NULL to overwrite InitData.
    #Otherwise, get it from the loc object.
    if(!keep.InitData) {InitData = NULL} else {InitData = loc$InitData}

    if(i == 1) {
      OUT = currentRow
    } else {OUT = rbind(OUT, currentRow)}
  }
  return(OUT)

}





