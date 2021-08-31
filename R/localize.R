


#localize####


localize <- function(wavList, coordinates, margin=10, zMin=-1, zMax=20, resolution=1,
                     F_Low=2000, F_High=8000, locFolder=NULL, tempC=15, plot=TRUE, InitData=NULL,
                     keep.InitData=TRUE, keep.SearchMap=FALSE) {

  #check that names of wavList correspond with names of coordinates.
  colnames(coordinates) <- tolower(colnames(coordinates))
  if(sum(!names(wavList) %in% coordinates$station) > 0) {stop('Some names in wavList not found
                                                              in coordinates!')}

  #Check that locFolder was specified.
  if(is.null(locFolder)) {stop('Error: Specify locFolder for outputs.')}
  if(!dir.exists(locFolder)) {stop('locFolder does not exist.')}

  #Get station names
  stations <- names(wavList)

  #Create NodePos object from station names
  row.names(coordinates) <- coordinates$station
  NodePos <- as.matrix(coordinates[stations,c('easting', 'northing', 'elevation')])
  colnames(NodePos) <- c('Easting', 'Northing', 'Elevation')

  #Create NodeInfo (Num and Pos)
  NodeInfo = list(Num = nrow(NodePos), Pos = NodePos)

  #Create SearchMap (Grid around Nodes, plus user-specified margins around outside)
  SearchMap = makeSearchMap(easting = NodeInfo$Pos[,'Easting'],
                            northing = NodeInfo$Pos[,'Northing'],
                            elevation = NodeInfo$Pos[,'Elevation'],
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
    InitData = MSRP_Init(NodeInfo,SearchMap,Para,LevelFlag)
  } else {
    print('Inherited InitData in 0 seconds.')
  }
  #Need to add a step here to check if the provided InitData actually corresponds to the
  #area we want to search.

  #Create FrameData.
  Data=matrix(0,nrow=NodeInfo$Num, ncol=Para$DataLen)
  #Assign row names to Data - same order as NodeInfo$Pos.
  row.names(Data) = row.names(NodeInfo$Pos)

  for(i in 1:NodeInfo$Num) {
    #Station name
    name = row.names(NodeInfo$Pos)[i]

    #Subtract DC offset and round
    Data[i,] = round(wavList[[name]]@left - mean(wavList[[name]]@left))
  }

  locstarttime = proc.time()
  #Run MRSP
  SMap = MSRP_RIJ_HT(NodeInfo,SearchMap,Data,Para,LevelFlag,InitData)
  print(paste('Localized detection in',round((proc.time()-locstarttime)['elapsed'],1),'seconds.'))

  #Extract global maximum location.
  locationInd = which(SMap == max(SMap), arr.ind = T)
  xInd = SearchMap$XMap[locationInd]
  yInd = SearchMap$YMap[locationInd]
  zInd = SearchMap$ZMap[locationInd]
  location = data.frame(Easting = xInd, Northing = yInd, Elevation = zInd, Power = max(SMap))

  if(plot) {
    jpeg(file.path(locFolder, paste0(formatC(index,width=4,flag='0'), '.jpeg')),
         width = 15, height = 15, units = 'in', res=100)
    par(mar=c(0,0,0,0))
    par(oma=c(0,0,0,0))
    m <- matrix(c(1:6,0,rep(7,4),0), ncol = 2)
    layout(m)

    #Plot 1
    validationSpec(st, index = index, locationEstimate = location, tempC = tempC)

    #Empty plots if needed.
    if(length(stations)<6) {
      for(i in 1:(6-length(stations))) {
        plot.new()
      }
    }

    #Plot 2
    xyMap = apply(SMap, c(1,2), FUN = mean)
    imagep(x=SearchMap$XMap[1,,1], y=SearchMap$YMap[,1,1], t(xyMap), las=1,
           drawPalette=F)
    points(NodeInfo$Pos[,c('Easting', 'Northing')], cex=3)
    text(x = NodeInfo$Pos[,'Easting'], y = NodeInfo$Pos[,'Northing'], labels = row.names(NodeInfo$Pos), pos=3, cex=2)
    points(x=xInd, y=yInd, pch=21, bg='gray', cex=3)
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





