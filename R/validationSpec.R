#validationSpec####

#Function to stack spectrograms after correcting for delay, to check whether the estimated location
#is reasonable.
#Need to change the arguments to align with localize() rather than localizeSingle().

validationSpec <- function(wavList, coordinates, locationEstimate, from = NULL,
                           to = NULL, tempC = 15, F_Low, F_High) {

  if(is.matrix(coordinates)) {
    coordinates <- as.data.frame(coordinates)
  }

  #combined bird location and station coordinates.
  if(!'Station' %in% colnames(coordinates)) {
    coordinates$Station <- row.names(coordinates)
  } else {
    coordinates$Station = as.character(coordinates$Station)
  }


  all = data.frame(ID=c('bird', coordinates$Station),
                   Easting = c(locationEstimate$Easting, coordinates$Easting),
                   Northing = c(locationEstimate$Northing, coordinates$Northing),
                   Elevation = c(locationEstimate$Elevation, coordinates$Elevation))

  D = as.matrix(dist(all[,c('Easting', 'Northing', 'Elevation')], upper=T, diag=T))
  colnames(D) = all$ID
  rownames(D) = all$ID
  Dists = D['bird',2:ncol(D)]
  #Speed of sound
  Vc = 331.45*sqrt(1+tempC/273.15)
  #Time delays.
  Delays = (Dists - min(Dists))/Vc

  if(is.null(from) & is.null(to)) {
    from = 0.2
    Length = length(wavList[[1]]@left) / wavList[[1]]@samp.rate
    to = Length - 0.2
  } else {
    Length = to - from
  }

  nmics = nrow(coordinates)

  for(i in 1:nmics) {

    #get sample rate
    Fs <- wavList[[1]]@samp.rate
    #get bitrate
    Br <- wavList[[1]]@bit

    #Adjust start time. Simultaneously adjusting for recording start time offset, and
    #transmission delay. Start 0.1 seconds before detection to get visual of onset.
    ADJ.first = from + Delays[coordinates$Station[i]] - 0.1
    #Adjust end time in same way. For detection longer than
    ADJ.last = to + Delays[coordinates$Station[i]]

    sound1 = tuneR::extractWave(wavList[[i]], from=ADJ.first, to=ADJ.last, xunit='time')

    sound1 = seewave::spectro(sound1, f=Fs,  wl = 256, plot=F, ovlp=50, norm=F)

    sound1$mic=coordinates$Station[i]
    sound1$channel=1
    sound1$from=from
    sound1$to=to
    sound1$distance=Dists[coordinates$Station[i]]
    if(i==1) {
      SoundList=list(a=sound1)
    } else {
      SoundList=append(SoundList, list(a=sound1))
    }
  }

  #get box to draw on spectrogram.
  xbox = c(0.1, Length-0.2)
  ybox = c(F_Low, F_High)

  for(k in 1:6) {
    if(k > length(SoundList)) {
      plot.new()
      next
    }

    validSpectro(ListOfData = SoundList[[k]])
    rect(xleft = xbox[1], xright = xbox[2], ybottom = ybox[1]/1000, ytop = ybox[2]/1000, border='red', lty=2, lwd=2)
  }
}
