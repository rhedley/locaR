#' Localize detected sounds
#'
#' \code{localize} and the two related functions \code{localizeSingle} and \code{localizeMultiple}
#' are the basic functions for localizing sounds. The basis of the functions is to take wav files
#' as inputs, alongside relevant metadata (e.g. coordinates and a variety of settings), and to
#' output the location of the sound source.
#'
#'

localize <- function(wavList,
                     coordinates,
                     margin = 10,
                     zMin = -1,
                     zMax = 20,
                     resolution = 1,
                     F_Low = 2000,
                     F_High = 8000,
                     locFolder = NULL,
                     jpegName = '000.jpeg',
                     tempC=15,
                     plot=TRUE,
                     InitData=NULL,
                     keep.InitData=TRUE,
                     keep.SearchMap=FALSE) {

  #check that names of wavList correspond with names of coordinates.
  colnames(coordinates) <- tolower(colnames(coordinates))
  if(sum(!names(wavList) %in% coordinates$station) > 0) {
    stop('Some names in wavList not found in coordinates!')
  }

  #Check that locFolder was specified.
  if(is.null(locFolder)) {stop('Error: Specify locFolder for outputs.')}
  if(!dir.exists(locFolder)) {stop('locFolder does not exist.')}

  #Get station names
  stations <- names(wavList)

  #Create NodePos object from station names
  row.names(coordinates) <- coordinates$station
  NodePos <- as.matrix(coordinates[stations,c('easting', 'northing', 'elevation')])
  colnames(NodePos) <- c('Easting', 'Northing', 'Elevation')

  #Create SearchMap (Grid around Nodes, plus user-specified margins around outside)
  SearchMap = makeSearchMap(easting = NodePos[,'Easting'],
                            northing = NodePos[,'Northing'],
                            elevation = NodePos[,'Elevation'],
                            margin = margin, zMin = zMin, zMax = zMax,
                            resolution = resolution)
  #Create Para list.
  #Get sample rate
  Fs = wavList[[1]]@samp.rate

  #Get DataLen

  DataLen = length(wavList[[1]]@left)

  #Define speed of sound.

  Vc = 331.45*sqrt(1+tempC/273.15)

  #Create Para list.
  Para=list()
  Para$GCCMethod = 'PHAT'
  Para$Fs = Fs
  Para$DataLen = DataLen
  Para$Vc = Vc
  Para$tempC = tempC
  Para$FL = F_Low
  Para$FH = F_High

  #LevelFlag
  LevelFlag = 2

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
  Data=matrix(0,nrow=nrow(NodePos), ncol=Para$DataLen)
  #Assign row names to Data - same order as NodePos.
  row.names(Data) = row.names(NodePos)

  for(i in 1:nrow(NodePos)) {
    #Station name
    name = row.names(NodePos)[i]

    #Subtract DC offset and round
    Data[i,] = round(wavList[[name]]@left - mean(wavList[[name]]@left))
  }

  locstarttime = proc.time()
  #Run MRSP
  SMap = MSRP_RIJ_HT(NodeInfo = list(Num = nrow(NodePos), Pos = NodePos),
                     SearchMap, Data, Para, LevelFlag, InitData)
  print(paste('Localized detection in',round((proc.time()-locstarttime)['elapsed'],1),'seconds.'))

  #Extract global maximum location.
  locationInd = which(SMap == max(SMap), arr.ind = T)
  xInd = SearchMap$XMap[locationInd]
  yInd = SearchMap$YMap[locationInd]
  zInd = SearchMap$ZMap[locationInd]
  location = data.frame(Easting = xInd, Northing = yInd, Elevation = zInd, Power = max(SMap))

  if(plot) {

    jpeg(file.path(locFolder, jpegName),
         width = 15, height = 15, units = 'in', res=100)
    par(mar=c(0,0,0,0))
    par(oma=c(0,0,0,0))
    m <- matrix(c(1:6,0,rep(7,4),0), ncol = 2)
    layout(m)

    #Plot 1
    validationSpec(wavList = wavList, coordinates = coordinates,
                   locationEstimate = location, tempC = tempC, F_Low = F_Low,
                   F_High = F_High, from = NULL, to = NULL)

    #Plot 2
    locHeatmap(SearchMap = SearchMap, SMap = SMap,
               NodeInfo = list(Num = nrow(NodePos), Pos = NodePos),
               location = location, mar = c(9,3,8,0))
    dev.off()
  }

  #Return list with location data.
  OUT = list(location=location)

  if(keep.InitData) {
    OUT = append(OUT, list(InitData = InitData))
  }

  if(keep.SearchMap) {
    OUT = append(OUT, list(SearchMap = SearchMap, SMap = SMap))
  }

  return(OUT)

}

#' @describeIn localize Localize single detection within a standardized survey workflow.
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
  for(i in 1:nrow(coordinates)) {
    #Station name
    name <- row.names(coordinates)[i]

    #File path
    wav <- st$files$Path[st$files$Station == name]

    #Channel to read.
    channel <- st$files$Channel[st$files$Station == name]

    #Amount to adjust start time (taking into account file names as well as user-specified adjustments)
    adj <- st$files$Adjustment[st$files$Station == name]

    #Check that the data to be read does not reach negative numbers.
    if((row$From - st$buffer - adj) < 0) {
      warning(paste('Index', index, 'had negative start time relative to file start'))
    }

    #Read wav or mp3 file.
    if(substr(wav, nchar(wav)-2, nchar(wav)) == 'wav') {
      W <- tuneR::readWave(wav, from = row$From-st$buffer-adj,
                           to=row$To+st$buffer-adj, units='seconds')
    }
    if(substr(wav, nchar(wav)-2, nchar(wav)) == 'mp3') {
      W <- tuneR::readMP3(wav)
      W <- tuneR::extractWave(W, from = row$From-st$buffer-adj,
                              to=row$To+st$buffer-adj, xunit = 'time')
    }

    #Extract correct channel.
    if(channel == 1) {
      W <- tuneR::mono(W, which = 'left')
    }
    if(channel == 2) {
      W <- tuneR::mono(W, which = 'right')
    }

    #W@left <- round(W@left - mean(W@left))

    if(i == 1) {wavList <- list(W)} else {wavList <- append(wavList, list(W))}

  }

  names(wavList) <- row.names(coordinates)

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

#' @describeIn localize Localize multiple detections in a standardized survey workflow.
localizeMultiple = function(st, indices, tempC = 15, plot=TRUE, InitData=NULL) {

  #Remove Null detections.
  detect <- st$detections

  detect <- detect[detect$Station1 != "" & !is.na(detect$Station1),]

  #replace original.
  st$detections <- detect

  if(is.character(indices)) {
    if(indices == 'all') {indices <- 1:nrow(st$detections)}
  }

  for(i in 1:length(indices)) {

    #First check whether InitData should be kept for index i

    currentRow = st$detections[indices[i],]

    if(i < length(indices)) {
      nextRow = st$detections[indices[i+1],]

      currentStations = as.vector(as.matrix(currentRow[,paste0('Station', 1:6)]))

      nextStations = as.vector(as.matrix(nextRow[,paste0('Station', 1:6)]))

      #Compare current stations and next stations. If they are identical, keep.InitData = TRUE
      keep.InitData = (sum(currentStations %in% nextStations) == length(currentStations) &
                         sum(nextStations %in% currentStations) == length(nextStations))
    } else {keep.InitData = TRUE}

    #InitData will generally be NULL for the first detection, inherited (sometimes) thereafter.

    loc = localizeSingle(st, index = indices[i], plot=plot,
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







