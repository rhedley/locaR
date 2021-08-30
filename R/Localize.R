
#LocalizeDetection####

#Localize detection with a specified index in a settings object.


localizeDetection = function(st, index, tempC=15, plot=TRUE, InitData=NULL,
                             keep.InitData=TRUE, keep.SearchMap=FALSE) {
  require(tuneR)
  require(oce)

  locFolder = file.path(st$outputFolder, 'Localizations')
  dir.create(locFolder, showWarnings = FALSE)

  #Get detections data frame
  d = st$detections

  #Check if the row index has data.
  if(is.na(d$Station1[index])) {
    location = data.frame(Easting = NA, Northing = NA, Elevation = NA, Power = NA)
    return(list(location=location))
  }

  #Extract row with data.
  row = d[index,]

  #Get station names
  stations = sort(as.vector(as.matrix(row[1, paste0('Station', 1:6)])))

  #Create NodePos object from station names
  row.names(st$files) = st$files$Station
  NodePos = as.matrix(st$files[stations,c('Easting', 'Northing', 'Elevation')])

  #Create NodeInfo (Num and Pos)
  NodeInfo = list()
  NodeInfo$Num = nrow(NodePos)
  NodeInfo$Pos = NodePos

  #Create SearchMap (Grid around Nodes, plus user-specified margins around outside)
  SearchMap = makeSearchMap(easting = NodeInfo$Pos[,'Easting'],
                            northing = NodeInfo$Pos[,'Northing'],
                            elevation = NodeInfo$Pos[,'Elevation'],
                            margin = st$margin, zMin = st$zMin, zMax = st$zMax,
                            resolution = st$resolution)
  #Create Para list.
  #Get sample rate
  exampleWav = st$files$Path[min(which(!is.na(st$files$Path)))]
  Fs = readWave(exampleWav, header=T)$sample.rate

  #Get DataLen

  DataLen = Fs * (row$Last - row$First + 2*st$buffer)

  #Define speed of sound.

  Vc = 331.45*sqrt(1+tempC/273.15)

  #Create Para list.
  Para=list()
  Para$GCCMethod = 'PHAT'
  Para$Fs = Fs
  Para$DataLen = DataLen
  Para$Vc = Vc
  Para$tempC = tempC
  Para$FL = row$F_Low
  Para$FH = row$F_High

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

  #Create FrameData
  Data=matrix(0,nrow=NodeInfo$Num, ncol=Para$DataLen)
  #Assign row names to Data - same order as NodeInfo$Pos.
  row.names(Data) = row.names(NodeInfo$Pos)
  for(i in 1:NodeInfo$Num) {
    #Station name
    name = row.names(NodeInfo$Pos)[i]

    #File path
    wav = st$files$Path[st$files$Station == name]

    #Channel to read.
    channel = st$channels$Channel[st$channels$Station == name]

    #Amount to adjust start time (taking into account file names as well as user-specified adjustments)
    adj = st$files$Adjustment[st$files$Station == name]

    #Check that the data to be read does not reach negative numbers.
    if((row$First-st$buffer-adj)<0) {
      warning(paste('Index', index, 'had negative start time relative to file start'))
    }

    #Read wav file.
    W=readWave(wav, from = row$First-st$buffer-adj, to=row$Last+st$buffer-adj, units='seconds')

    #Extract correct channel.
    if(channel == 1) {
      Data[i,] = W@left - mean(W@left)
    } else {
      Data[i,] = W@right - mean(W@right)
    }
    Data[i,] = round(Data[i,])
  }

  locstarttime = proc.time()
  #Run MRSP
  SMap = MSRP_RIJ_HT(NodeInfo,SearchMap,Data,Para,LevelFlag,InitData)
  print(paste('Localized detection',index,'in',round((proc.time()-locstarttime)['elapsed'],1),'seconds.'))

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


#localizeMultiple####

#This function will localize multiple detections. In particular, it optimizes whether
#to keep.InitData or not, which can speed things up about twofold.

localizeMultiple = function(st, indices, tempC = 15, plot=TRUE, InitData=NULL) {

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

    loc = localizeDetection(st, index = indices[i], plot=plot,
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




#validationSpec####

#Function to stack spectrograms after correcting for delay, to check whether the estimated location
#is reasonable.

validationSpec = function(st, index, locationEstimate = x$location, tempC=15) {

  #Get station info.
  sts = st$files[st$files$Station %in% st$detections[index, paste0('Station', 1:6)],]
  sts = sts[order(sts$Station),]

  #combined bird location and station coordinates.

  all = data.frame(ID=c('bird', sts$Station), Easting = c(locationEstimate$Easting, sts$Easting),
                   Northing = c(locationEstimate$Northing, sts$Northing), Elevation = c(locationEstimate$Elevation, sts$Elevation))

  D = as.matrix(dist(all[,c('Easting', 'Northing', 'Elevation')], upper=T, diag=T))
  colnames(D) = all$ID
  rownames(D) = all$ID
  Dists = D['bird',2:ncol(D)]
  #Speed of sound
  Vc = 331.45*sqrt(1+tempC/273.15)
  #Time delays.
  Delays = (Dists - min(Dists))/Vc

  first = st$detections$First[index]
  last = st$detections$Last[index]
  Length = last-first


  nmics = nrow(sts)

  i=1
  for(i in 1:nmics) {
    #get file path
    file = sts$Path[i]
    #get sample rate
    Fs=readWave(file, header=T)$sample.rate
    #get bitrate
    Br=readWave(file, header=T)$bits
    #get adjustment
    A=sts$Adjustment[i]
    Ch = sts$Channel[i]
    #Adjust start time. Simultaneously adjusting for recording start time offset, and
    #transmission delay. Start 0.1 seconds before detection to get visual of onset.
    ADJ.first = first + Delays[sts$Station[i]] - A - 0.1
    #Adjust end time in same way. For detection longer than
    ADJ.last = max(last + Delays[sts$Station[i]] - A, ADJ.first + 2.1)

    if(Ch==1 | is.na(Ch)){
      sound1 = readWave(file, from=ADJ.first, to=ADJ.last, units='seconds')@left
    } else {
      sound1 = readWave(file, from=ADJ.first, to=ADJ.last, units='seconds')@right
    }
    sound1 = spectro(sound1, f=Fs,  wl = 256, plot=F, ovlp=50, norm=F)

    sound1$mic=sts$Station[i]
    sound1$channel=Ch
    sound1$first=first
    sound1$last=last
    sound1$distance=Dists[sts$Station[i]]
    if(i==1) {
      SoundList=list(a=sound1)
    } else {
      SoundList=append(SoundList, list(a=sound1))
    }
  }
  #SpatialAliasingIndex=SpatialAlias(MicCoords=sts, BirdCoords=data.frame(Easting=Data$Easting[i], Northing=Data$Northing[i], Elevation=Data$Elevation[i]))

  #get box to draw on spectrogram.
  xbox = c(0.1, Length+0.1)
  ybox = c(st$detections$F_Low[index], st$detections$F_High[index])

  for(k in 1:length(SoundList)) {
    mySpectro(SoundList[[k]])
    if(k==1) legend('topright', legend=paste0('Spatial Aliasing Index = ', NA), cex=2, bty='n')
    rect(xleft = xbox[1], xright = xbox[2], ybottom = ybox[1]/1000, ytop = ybox[2]/1000, border='red', lty=2, lwd=2)
  }
}


