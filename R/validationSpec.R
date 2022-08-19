#' Create validation spectrograms.
#'
#' This function is used inside the \code{localize} function to create the panels of
#' synchronized spectrograms for manual review.
#'
#' @param wavList list of Wave objects. The name of the Wave objects MUST be
#'     present in the coordinates data.frame.
#' @param coordinates data.frame. Must contain four required columns:
#'     column Station contains a character string with names of each recording
#'     station, while Easting, Northing and Elevation contain the x, y, and z
#'     coordinates of the station, in meters (E.g. UTM coordinates).
#' @locationEstimate Dataframe with one row containing columns Easting, Northing and Elevation,
#'     specifying the estimated location of the sound source.
#' @param from,to Numeric. The portion of the wavs to plot.
#' @param tempC Numeric. The ambient temperature in celsius, which is used to calculate the speed
#'     of sound in air if none is specified.
#' @param soundSpeed Numeric. The speed of sound. If not provided, tempC will be used to calculate
#'     the speed of sound in air.
#' @param F_Low,F_High Numeric. The low and high frequency, in Hz, of the sound
#'     to be localized.
#' @export

validationSpec <- function(wavList, coordinates, locationEstimate, from = NULL,
                           to = NULL, tempC = 15, soundSpeed = NULL, F_Low, F_High) {

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

  #Define speed of sound based on speed in air if not already defined.
  if(is.null(soundSpeed)) {
    Vc <- 331.45*sqrt(1+tempC/273.15)
  } else {
    Vc <- soundSpeed
  }

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
  xbox = c(0.1, Length-0.2-0.1)
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
