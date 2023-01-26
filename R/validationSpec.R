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
#' @param locationEstimate Dataframe with one row containing columns Easting, Northing and Elevation,
#'     specifying the estimated location of the sound source.
#' @param from,to Numeric. The portion of the wavs to plot. If missing, the whole wav will be plotted.
#' @param tempC Numeric. The ambient temperature in celsius, which is used to calculate the speed
#'     of sound in air if none is specified.
#' @param soundSpeed Numeric. The speed of sound. If missing, tempC will be used to calculate
#'     the speed of sound in air.
#' @param F_Low,F_High Numeric. The low and high frequency, in Hz, of the sound
#'     to be localized.
#' @examples
#'     #Get filepaths for example data.
#'     fp <- list.files(system.file('extdata', package = 'locaR'),
#'                       pattern = '.mp3', full.names = TRUE)
#'     #Add names.
#'     names(fp) <- sapply(strsplit(basename(fp), '_'), '[[', 1)
#'     #Load first row of detection data.
#'     row <- read.csv(system.file('extdata',
#'          'Vignette_Detections_20200617_090000.csv', package = 'locaR'),
#'           stringsAsFactors = FALSE)[1,]
#'     #Get non-empty Station columns.
#'     stationSubset <- unlist(row[1,paste0('Station',1:6)])
#'     stationSubset <- stationSubset[!is.na(stationSubset) & stationSubset != '']
#'     #Create wav list.
#'     wl <- createWavList(paths = fp[stationSubset], names = stationSubset,
#'            from = row$From, to = row$To, buffer = 0.2, index=1)
#'     #Read coordinates.
#'     coordinates <- read.csv(system.file('extdata', 'Vignette_Coordinates.csv',
#'                             package = 'locaR'), stringsAsFactors = FALSE)
#'     row.names(coordinates) <- coordinates$Station
#'     #Subset coordinates.
#'     crd <- coordinates[stationSubset,]
#'     #Localize.
#'     loc <- localize(wavList = wl, coordinates = crd, locFolder = tempdir(),
#'                F_Low = row$F_Low, F_High = row$F_High, jpegName = '0001.jpeg',
#'                keep.SearchMap = TRUE)
#'     #Create validation spectrogram.
#'     par(mfrow = c(6,1))
#'     validationSpec(wavList = wl, coordinates = crd, locationEstimate = loc$location,
#'                                  F_Low = row$F_Low, F_High = row$F_High)
#' @export

validationSpec <- function(wavList, coordinates, locationEstimate, from,
                           to, tempC = 15, soundSpeed, F_Low, F_High) {

  if(is.matrix(coordinates)) {
    coordinates <- as.data.frame(coordinates)
  }

  #combined bird location and station coordinates.
  if(!'Station' %in% colnames(coordinates)) {
    coordinates$Station <- row.names(coordinates)
  } else {
    coordinates$Station <- as.character(coordinates$Station)
  }


  all <- data.frame(ID = c('bird', coordinates$Station),
                   Easting = c(locationEstimate$Easting, coordinates$Easting),
                   Northing = c(locationEstimate$Northing, coordinates$Northing),
                   Elevation = c(locationEstimate$Elevation, coordinates$Elevation))

  D <- as.matrix(dist(all[,c('Easting', 'Northing', 'Elevation')], upper = TRUE, diag = TRUE))
  colnames(D) <- rownames(D) <- all$ID
  Dists <- D['bird',2:ncol(D)]

  #Define speed of sound based on speed in air if not already defined.
  if(missing(soundSpeed)) {
    Vc <- 331.45*sqrt(1+tempC/273.15)
  } else {
    Vc <- soundSpeed
  }

  #Time delays.
  Delays <- (Dists - min(Dists))/Vc

  if(missing(from) & missing(to)) {
    from <- 0.2
    Length <- length(wavList[[1]]@left) / wavList[[1]]@samp.rate
    to <- Length - 0.2
  } else {
    Length <- to - from
  }

  #Create a list of spectrograms.
  for(i in 1:nrow(coordinates)) {

    #get sample rate
    Fs <- wavList[[1]]@samp.rate
    #get bitrate
    Br <- wavList[[1]]@bit

    #Adjust start time. Simultaneously adjusting for recording start time offset, and
    #transmission delay. Start 0.1 seconds before detection to get visual of onset.
    ADJ.first <- from + Delays[coordinates$Station[i]] - 0.1
    #Adjust end time in same way. For detection longer than
    ADJ.last <- to + Delays[coordinates$Station[i]]

    sound1 <- tuneR::extractWave(wavList[[coordinates$Station[i]]], from=ADJ.first, to=ADJ.last, xunit='time')

    sound1 <- seewave::spectro(sound1, f=Fs,  wl = 256, plot = FALSE, ovlp=50, norm = FALSE)

    sound1$mic <- coordinates$Station[i]
    sound1$from <- from
    sound1$to <- to
    sound1$distance <- Dists[coordinates$Station[i]]
    if(i==1) {
      SoundList <- list(a=sound1)
    } else {
      SoundList <- append(SoundList, list(a=sound1))
    }
  }

  #get box to draw on spectrogram.
  xbox <- c(0.1, Length-0.2-0.1)
  ybox <- c(F_Low, F_High)

  #Draw spectrograms, Six spectrograms are drawn.
  for(k in 1:6) {
    if(k > length(SoundList)) {
      plot.new()
      next
    }

    if(is.na(SoundList[[k]])[1]) {
      plot(0, axes = FALSE, xlab=NA, ylab=NA, col='white')
    } else {
      oce::imagep(SoundList[[k]]$time, SoundList[[k]]$freq, t(SoundList[[k]]$amp),
                  drawPalette = FALSE, ylim=c(0,10),xlim=c(0,1), mar=rep(0,4), axes = FALSE,
                  breaks=seq(-0,85,length.out=21), col=rev(gray.colors(20, 0,1)))
      legend('topleft',
             legend = SoundList[[k]]$mic,
             bty='n', cex=2)
      legend('bottomright',
             legend = paste0('Distance = ', round(SoundList[[k]]$distance, 1), ' m'),
             bty='n', cex=2)
      axis(side = 1, labels = NA, tck = 0.015, at = seq(0,5,0.05))
      abline(v = seq(0.05,5,0.1), lty = 2)
      abline(v = seq(0,5,0.1), lty = 3)
      box()
    }

    rect(xleft = xbox[1], xright = xbox[2], ybottom = ybox[1]/1000, ytop = ybox[2]/1000,
         border='red', lty=2, lwd=2)
  }
}
