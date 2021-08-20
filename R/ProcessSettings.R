#Put all R functions here.

#getMics####
#Given date and time, get all active microphones and coordinates.

getFilepaths = function(settings) {
  
  if(is.character(settings)) {
    if(file.exists(settings)) {
      st = processSettings(settings)
    }
  } else {
    if(is.list(settings)) {
      st = settings
    } else {
      stop('settings must be a filepath or a list')
    }
  }
  
  #Load the files that are from that date and time.
  Files = list.files(st$siteFolder, recursive=T, full.names=T, pattern=paste0(st$date,'.*', st$time, '.*wav'))
  Files = data.frame(Path=Files, CorrFile=basename(Files), Station=parseWAFileNames(filenames=basename(Files), model='SM3')$prefix, stringsAsFactors = F)
  #remove extra mics
  extra = Files$Station[!Files$Station %in% st$channels$Station]
  if(length(extra) > 0) {
    Files = Files[!Files$Station %in% extra,]
    warning(paste0('Removing files from ', paste(extra), ' not found in channels file.\n'))
  }
  #Add missing files if needed.
  missing = st$channels$Station[!st$channels$Station %in% Files$Station]
  if(length(missing) > 0) {
    mdf = data.frame(Path=NA, CorrFile=NA, Station=missing)
    Files = rbind(Files, mdf)
    Files = Files[order(Files$Station),]
    warning(paste0('No file found for ', paste(missing), '. Adding placeholders to file list.\n'))
  }
  
  #Correct file names where necessary.
  for(i in 1:nrow(Files)) {
    if(Files$CorrFile[i] %in% st$adjustments$Filename) {
      Files$CorrFile[i]=st$adjustments$Adjusted[st$adjustments$Filename==Files$CorrFile[i]]
    }
  }
  
  #Get adjustment for each file. Different approach for manual versus filename-based adjustment.
  #Get value based on filename.

  
  Files$Adjustment = as.numeric(parseWAFileNames(Files$CorrFile)$time) - as.numeric(st$time)*100
  #Then get files requiring manual
  Odd = st$adjustments[st$adjustments$DifferenceAdjMinusOrig != 1,]
  #Adjust those manually.
  for(i in 1:nrow(Files)) {
    C=Files$CorrFile[i]
    if(C %in% Odd$Adjusted) {
      A = Odd$DifferenceAdjMinusOrig[Odd$Adjusted==C]
      Files$Adjustment[i]=A
    } 
  }
  
  Files = merge(Files, st$coords, by='Station')
  
  #Strip unnecessary columns.
  Files = Files[,colnames(Files) %in% c('Station', 'Path', 'CorrFile', 'Adjustment', 'Zone', 'Easting',
                                        'Northing', 'Elevation')]
  #Merge with channels data.
  
  Files$Channel = merge(Files, st$channels, by='Station')$Channel
  
  #Return dataframe containing files and metadata.
  return(Files)
  
}

#mySpectro####

#Spectrogram function
mySpectro= function(ListOfData) {
  if(is.na(ListOfData)[1]) {plot(0, axes=F, xlab=NA, ylab=NA, col='white')} else{
    Mic=ListOfData$mic
    Channel=ListOfData$channel
    first=ListOfData$first
    last=ListOfData$last
    if(Mic==1 & Channel=='a') {
      par(mfrow=c(5,6))
      par(mar=c(0,0,0,0))
      par(oma=c(0,0,0,0))
    }
    imagep(ListOfData$time, ListOfData$freq, t(ListOfData$amp), 
           drawPalette=F, ylim=c(0,10),xlim=c(0,1), mar=rep(0,4), axes=F,
           breaks=seq(-0,85,length.out=21), col=rev(gray.colors(20, 0,1)))
    legend('topleft', legend=paste0(Mic, ifelse(Channel==2,'b','a')), bty='n', cex=2)
    legend('bottomright', legend=paste0('Distance = ', round(ListOfData$distance,1), ' m'),
           bty='n', cex=2)
    axis(side=1, labels=NA, tck=0.015, at=seq(0,5,0.05))
    abline(v=seq(0.05,5,0.1), lty=2)
    abline(v=seq(0,5,0.1), lty=3)
    box()
  }
}


#SpatialAlias####

#Spatial aliasing index function.
SpatialAlias = function(MicCoords, BirdCoords) {
  BirdCoords[2:9,]=BirdCoords
  BirdCoords$Easting=BirdCoords$Easting+c(0,5,sqrt(12.5),0,-sqrt(12.5),-5,-sqrt(12.5),0,sqrt(12.5))
  BirdCoords$Northing=BirdCoords$Northing+c(0,0,sqrt(12.5),5,sqrt(12.5),0,-sqrt(12.5),-5,-sqrt(12.5))
  
  #Combine bird and microphone coordinates.
  all=rbind(BirdCoords,data.frame(Easting=MicCoords$Easting, Northing=MicCoords$Northing, Elevation=MicCoords$Elevation))
  BirdIDs=paste0('Bird',c('C', 'E', 'NE', 'N', 'NW', 'W' ,'SW', 'S', 'SE'))
  all$ID=c(BirdIDs, MicCoords$Station)
  D = as.matrix(dist(all[,c('Easting', 'Northing', 'Elevation')], upper=T, diag=T))
  colnames(D) = all$ID
  rownames(D) = all$ID
  Dists = D[BirdIDs,10:ncol(D)]
  Delays = Dists
  for(i in 1:nrow(Delays)) {
    x=Delays[i,]
    x = (x - min(x))/345
    Delays[i,]=x
  }
  Diffs=Delays[2:9,]
  for(i in 1:nrow(Diffs)) {
    Diffs[i,]=abs(Diffs[i,]-Delays[1,])
  }
  Change=rowSums(Diffs)
  names(Change)=BirdIDs[2:9]
  
  TheoreticalMax=0.06
  Index=(0.06-min(Change))/0.06
  return(round(Index,2)) #Higher values of Index mean more spatial aliasing.
}

#processSettings#####

#Process settings file.
#Need to add more error checking, but this will be a critical function.

processSettings = function(settingsFile) {
  
  Settings = read.csv(settingsFile, stringsAsFactors=F)
  Settings = split(Settings$Value, f=Settings$Setting)
  
  coordsFile = Settings$CoordinatesFile
  
  siteFolder = Settings$SiteWavsFolder
  
  detectionsFile = Settings$DetectionsFile
  
  outputFolder = Settings$OutputFolder
  
  adjustmentsFile = Settings$AdjustmentsFile
  
  channelsFile = Settings$ChannelsFile
  
  MSRPFolder = Settings$MSRPFunctionsFolder #To be deprecated.
  
  detectionsFile = paste0(outputFolder, '/',detectionsFile)
  
  margin = as.numeric(Settings$Margin)
  
  zMin = as.numeric(Settings$Zmin)
  
  zMax = as.numeric(Settings$Zmax)
  
  resolution = as.numeric(Settings$Resolution)
  
  intervals = as.numeric(Settings$Intervals)
  
  buffer = as.numeric(Settings$Buffer) #Can this be deprecated?
  
  spacing = as.numeric(Settings$Spacing) #Can this be deprecated? Only used when one microphone is defined... Could easily use nearest neighbour for that.
  
  date = Settings$Date
  
  time = Settings$Time
  
  time = formatC(as.numeric(time), width=4, flag="0")
  
  surveyLength = as.numeric(Settings$SurveyLengthInSeconds)
  
  coords=read.csv(coordsFile, stringsAsFactors=F)
  coords = coords[,c('Station', 'Zone', 'Easting', 'Northing', 'Elevation')]
  
  if(file.exists(detectionsFile)) {
    detections = read.csv(detectionsFile, stringsAsFactors=F)
  } else {detections = NA}
  
  adjustments = read.csv(adjustmentsFile, stringsAsFactors=F)
  
  channels = read.csv(channelsFile, stringsAsFactors=F)
  
  coords = coords[coords$Station %in% channels$Station,]
  
  #Check that all stations listed in channels file have coordinates.
  if(sum(channels$Station %in% coords$Station) != nrow(channels)) {stop('Some stations listed in channelsFile were missing coordinates')}
  if(sum(is.na(coords$Easting))>0 | sum(is.na(coords$Northing))>0 | sum(is.na(coords$Elevation))>0) {stop('Some stations listed in channelsFile were missing coordinates')}
  
  #Check that all stations listed in detections file have coordinates.
  statVec = unique(unlist(c(detections[paste0('Station', 1:6)])))
  statVec = statVec[!is.na(statVec) & statVec != '']
  if(sum(statVec %in% coords$Station) != length(statVec)) {stop('Some stations listed in detectionsFile were missing coordinates')}
  
  
  st = list(detections = detections, coords=coords, channels=channels, adjustments=adjustments, siteFolder = siteFolder, date=date, time=time, intervals=intervals,
            MSRPFolder = MSRPFolder, outputFolder = outputFolder, resolution = resolution, 
            spacing = spacing, margin = margin, zMin = zMin, zMax = zMax, buffer = buffer, surveyLength = surveyLength)
  
  return(st)
  
}

#parseWAFileName####

#parse relevant info from Wildlife Acoustics filename.
#Should change this so it accepts a vector of models.

parseWAFileNames = function(filenames, model='SM3') {
  if(!model %in% c('SM2', 'SM3', 'SM4')) {stop('model must be SM2, SM3 or SM4')}
  
  df = data.frame(filename = filenames, prefix=NA, channels=NA, date=NA, time=NA, ext=NA, stringsAsFactors=F)
  
  for(i in 1:nrow(df)) {
    
    fn = df$filename[i]
    if(is.na(fn)) {next}
    replaceDS = gsub('\\$', '_', fn)
    spl = strsplit(replaceDS, '_')
    
    if(model == 'SM3') {
      df$prefix[i] = sapply(spl, '[[', 1)
      df$channels[i] = sapply(spl, '[[', 2)
      df$date[i] = sapply(spl, '[[', 3)
      temp = sapply(spl, '[[', 4)
      df$time[i] = sapply(strsplit(temp, '\\.'), '[[', 1)
      df$ext[i] = sapply(strsplit(temp, '\\.'), '[[', 2)
    } else {
      df$prefix[i] = sapply(spl, '[[', 1)
      df$date[i] = sapply(spl, '[[', 2)
      temp = sapply(spl, '[[', 3)
      df$time[i] = sapply(strsplit(temp, '\\.'), '[[', 1)
      df$ext[i] = sapply(strsplit(temp, '\\.'), '[[', 2)
    }
  }

  return(df)
  
}














