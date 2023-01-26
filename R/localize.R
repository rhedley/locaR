#' Localize detected sounds
#'
#' \code{localize} and the related function \code{localizeMultiple}
#' are the basic functions for localizing sounds. They take audio data as inputs, alongside relevant
#'  metadata (e.g. coordinates and a variety of settings), and estimate the location of the
#'  dominant sound source. The \code{localize} function takes as arguments the minimal
#' amount of information needed for localization. Localization is conducted on the
#' full duration of the Wave objects in wavList. Effectively this means the user
#' must wrangle the data and clip the audio themselves, but this affords the greatest
#' flexibility in terms of how the user chooses to organize their data.
#' The \code{localizeMultiple} function, in contrast,
#' automates much of the data wrangling process, but requires data to be organized in a
#' very specific way (e.g. folder structure, file structures). Thus, the latter
#' function trades off flexibility for increased automation. Both functions use the
#' same underlying localization algorithm - \code{localizeMultiple}
#' passes its data to \code{localize} after the data has been wrangled.
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
#' @param tempC Numeric. Temperature in degrees C, which is used to calculate
#'     the speed of sound in air using the equation 331.45*sqrt(1+tempC/273.15).
#' @param soundSpeed Numeric. The speed of sound in meters per second. If missing,
#'     the speed of sound is calculated based on the specified
#'     temperature (assuming the transmission medium is air). If soundSpeed is
#'     specified, the tempC value is over-ridden.
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
#' @param indices Numeric or 'all'. Indices to be localized within a detection file.
#'     Setting to 1 localizes the first row, c(7:10) localizes rows 7-10, and 'all'
#'     localizes all rows (ignoring rows that have no entry in the Station1 column).
#' @return List, containing the location of the sound source (global maximum),
#'     and optionally the InitData and SearchMap lists.
#' @references
#' Cobos, M., Martí, A., & J.J. López. 2011. A modified SRP-PHAT functional for robust real-time sound source localization with scalable spatial sampling. IEEE Signal Processing Letters. 18:71-74. doi:10.1109/LSP.2010.2091502.
#' @examples
#'     ## example for localize()
#'     #Get filepaths for example data.
#'     fp <- list.files(system.file('extdata', package = 'locaR'), pattern = '.mp3', full.names = T)
#'     #Add names.
#'     names(fp) <- sapply(strsplit(basename(fp), '_'), '[[', 1)
#'     #Load first row of detection data.
#'     row <- read.csv(system.file('extdata',
#'          'Vignette_Detections_20200617_090000.csv', package = 'locaR'),
#'           stringsAsFactors=F)[1,]
#'     #Get non-empty Station columns.
#'     stationSubset <- unlist(row[1,paste0('Station',1:6)])
#'     stationSubset <- stationSubset[!is.na(stationSubset) & stationSubset != '']
#'     #Create wav list.
#'     wl <- createWavList(paths = fp[stationSubset], names = stationSubset,
#'            from = row$From, to = row$To, buffer = 0.2, index=1)
#'     #Read coordinates.
#'     coordinates <- read.csv(system.file('extdata', 'Vignette_Coordinates.csv',
#'                                         package = 'locaR'), stringsAsFactors = F)
#'     row.names(coordinates) <- coordinates$Station
#'     #Subset coordinates.
#'     crd <- coordinates[stationSubset,]
#'     #Localize.
#'     loc <- localize(wavList = wl, coordinates = crd, locFolder = tempdir(),
#'                F_Low = row$F_Low, F_High = row$F_High, jpegName = '0001.jpeg', keep.SearchMap = T)
#'
#'     ## Example for localizeMultiple().
#'     #list mp3 files.
#'     f.in <- list.files(system.file('extdata', package = 'locaR'), full.names = T, pattern='mp3$')
#'     #create wav names.
#'     f.out <- file.path(tempdir(), basename(f.in))
#'     #change extension.
#'     substr(f.out, nchar(f.out)-2, nchar(f.out)) <- 'wav'
#'     #Convert mp3 to wav, as required for this particular example.
#'     for(i in 1:length(f.in)) {
#'       y <- tuneR::readMP3(f.in[i])
#'       tuneR::writeWave(y, filename = f.out[i])
#'     }
#'     #Set up survey.
#'     survey <- setupSurvey(folder = tempdir(), projectName = 'Ex', run = 1,
#'                coordinatesFile = system.file('extdata', 'Vignette_Coordinates.csv',
#'                                              package = 'locaR'),
#'                siteWavsFolder = tempdir(), date = '20200617', time = '090000', surveyLength = 7)
#'     #read example detections.
#'     dets <- read.csv(system.file('extdata', 'Vignette_Detections_20200617_090000.csv',
#'                                  package = 'locaR'))
#'     #over-write empty detections file.
#'     write.csv(dets, file.path(tempdir(), '20200617_090000',
#'               'Run1', 'Ex_20200617_090000_Run1_Detections.csv'), row.names = F)
#'     #Process settings.
#'     st <- processSettings(settings = survey, getFilepaths = TRUE, types = 'wav')
#'     #localize
#'     locs <- localizeMultiple(st = st, indices = 1:2)
#' @export

localize <- function(wavList,coordinates,margin = 10,zMin = -1,zMax = 20,
                     resolution = 1, F_Low = 2000, F_High = 8000, tempC = 15,
                     soundSpeed, plot = TRUE, locFolder, jpegName = '000.jpeg',
                     InitData = NULL, keep.InitData = TRUE,keep.SearchMap = FALSE) {

  #check that names of wavList correspond with names of coordinates.
  if(length(names(wavList)) < length(wavList)) {
    stop('wavList must be named.')
  }

  if(sum(!names(wavList) %in% coordinates$Station) > 0) {
    stop('Some names in wavList not found in coordinates!')
  }

  #If soundSpeed is missing, calculate based on tempC in air.
  if(missing(soundSpeed)) {
    Vc <- 331.45*sqrt(1+tempC/273.15)
  } else {
    Vc <- soundSpeed
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

  #Create Para list.
  Para <- list(GCCMethod = "PHAT", Fs=Fs, DataLen=DataLen, Vc=Vc, tempC=tempC,
            FL = F_Low, FH=F_High)

  #LevelFlag (not really needed, since there is only one option)
  LevelFlag <- 2

  #Create InitData if needed.
  if(is.null(InitData)) {
    InitData <- MSRP_Init(NodeInfo = list(Num = nrow(NodePos), Pos = NodePos),
                         SearchMap, Para, LevelFlag)
  } else {
    message('Inherited InitData in 0 seconds.')
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
    if(missing(locFolder)) {stop('Error: Specify locFolder for outputs.')}
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
                   locationEstimate = location, soundSpeed = Vc, F_Low = F_Low,
                   F_High = F_High)

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
#' @export
localizeMultiple = function(st, indices = 'all', plot=TRUE, InitData=NULL) {

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

  for(i in 1:nrow(st$detections)) {

    #First check whether InitData should be kept for index i

    currentRow = st$detections[i,]

    if(i < nrow(st$detections)) {
      nextRow = st$detections[i+1,]

      currentStations = as.vector(as.matrix(currentRow[,paste0('Station', 1:6)]))

      nextStations = as.vector(as.matrix(nextRow[,paste0('Station', 1:6)]))

      #Compare current stations and next stations. If they are identical, keep.InitData = TRUE
      keep.InitData = (sum(currentStations %in% nextStations) == length(currentStations) &
                         sum(nextStations %in% currentStations) == length(nextStations))
    } else {keep.InitData = TRUE}

    #InitData will generally be NULL for the first detection, inherited (sometimes) thereafter.

    loc = localizeSingle(st, index = i, plot=plot, InitData = InitData,
                         keep.InitData = keep.InitData)

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





