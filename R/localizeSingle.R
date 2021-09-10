#localizeSingle####

#Localize detection with a specified index in a settings object.

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
  #
  #
  #
  #
  #
  # #Create SearchMap (Grid around Nodes, plus user-specified margins around outside)
  # SearchMap = makeSearchMap(easting = NodePos[,'Easting'],
  #                           northing = NodePos[,'Northing'],
  #                           elevation = NodePos[,'Elevation'],
  #                           margin = st$margin, zMin = st$zMin, zMax = st$zMax,
  #                           resolution = st$resolution)
  # #Create Para list.
  # #Get sample rate
  # exampleWav = st$files$Path[min(which(!is.na(st$files$Path)))]
  # Fs = readWave(exampleWav, header=T)$sample.rate
  #
  # #Get DataLen
  #
  # DataLen = Fs * (row$Last - row$First + 2*st$buffer)
  #
  # #Define speed of sound.
  #
  # Vc = 331.45*sqrt(1+tempC/273.15)
  #
  # #Create Para list.
  # Para=list()
  # Para$GCCMethod = 'PHAT'
  # Para$Fs = Fs
  # Para$DataLen = DataLen
  # Para$Vc = Vc
  # Para$tempC = tempC
  # Para$FL = row$F_Low
  # Para$FH = row$F_High
  #
  # #LevelFlag
  # LevelFlag = 2
  #
  # #Create InitData if needed.
  # if(is.null(InitData)) {
  #   InitData = MSRP_Init(NodeInfo = list(Num = nrow(NodePos), Pos = NodePos),
  #                        SearchMap, Para, LevelFlag)
  # } else {
  #   print('Inherited InitData in 0 seconds.')
  # }
  # #Need to add a step here to check if the provided InitData actually corresponds to the
  # #area we want to search.
  #
  # #Create FrameData
  # Data=matrix(0,nrow=nrow(NodePos), ncol=Para$DataLen)
  # #Assign row names to Data - same order as NodePos.
  # row.names(Data) = row.names(NodePos)
  #
  #
  # locstarttime = proc.time()
  # #Run MRSP
  # SMap = MSRP_RIJ_HT(NodeInfo = list(Num = nrow(NodePos), Pos = NodePos),
  #                    SearchMap, Data, Para, LevelFlag, InitData)
  # print(paste('Localized detection',index,'in',round((proc.time()-locstarttime)['elapsed'],1),'seconds.'))
  #
  # #Extract global maximum location.
  # locationInd = which(SMap == max(SMap), arr.ind = T)
  # xInd = SearchMap$XMap[locationInd]
  # yInd = SearchMap$YMap[locationInd]
  # zInd = SearchMap$ZMap[locationInd]
  # location = data.frame(Easting = xInd, Northing = yInd, Elevation = zInd, Power = max(SMap))
  #
  # if(plot) {
  #   jpeg(file.path(locFolder, paste0(formatC(index,width=4,flag='0'), '.jpeg')),
  #        width = 15, height = 15, units = 'in', res=100)
  #   par(mar=c(0,0,0,0))
  #   par(oma=c(0,0,0,0))
  #   m <- matrix(c(1:6,0,rep(7,4),0), ncol = 2)
  #   layout(m)
  #
  #   #Plot 1
  #   validationSpec(st, index = index, locationEstimate = location, tempC = tempC)
  #
  #   #Empty plots if needed.
  #   if(length(stations)<6) {
  #     for(i in 1:(6-length(stations))) {
  #       plot.new()
  #     }
  #   }
  #
  #   #Plot 2
  #   xyMap = apply(SMap, c(1,2), FUN = mean)
  #   imagep(x=SearchMap$XMap[1,,1], y=SearchMap$YMap[,1,1], t(xyMap), las=1,
  #          drawPalette=F)
  #   points(NodePos[,c('Easting', 'Northing')], cex=3)
  #   text(x = NodePos[,'Easting'], y = NodePos[,'Northing'], labels = row.names(NodePos), pos=3, cex=2)
  #   points(x=xInd, y=yInd, pch=21, bg='gray', cex=3)
  #   dev.off()
  # }
  #
  # #Return list with location data.
  # OUT = list(location=location)
  #
  # if(keep.InitData) {
  #   OUT = append(OUT, list(InitData = InitData))
  # }
  #
  # if(keep.SearchMap) {
  #   OUT = append(OUT, list(SearchMap = SearchMap, SMap = SMap))
  # }

  return(OUT)
}
