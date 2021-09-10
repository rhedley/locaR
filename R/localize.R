


#localize####


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





