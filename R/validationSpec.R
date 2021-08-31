#validationSpec####

#Function to stack spectrograms after correcting for delay, to check whether the estimated location
#is reasonable.
#Need to change the arguments to align with localize() rather than localizeSingle().

validationSpec = function(st, index, locationEstimate = x$location, tempC=15) {

  #Get station info.
  sts = st$files[st$files$Station %in% st$detections[index, paste0('Station', 1:6)],]
  sts = sts[order(sts$Station),]

  #combined bird location and station coordinates.

  all = data.frame(ID=c('bird', sts$Station), Easting = c(locationEstimate$Easting, sts$Easting),
                   Northing = c(locationEstimate$Northing, sts$Northing),
                   Elevation = c(locationEstimate$Elevation, sts$Elevation))

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
