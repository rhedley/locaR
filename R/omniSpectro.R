#' Generate grid of spectrograms for detecting sounds of interest.
#'
#' \code{omniSpectro} creates a grid of time-synchronized spectrograms, to
#' facilitate the manual detection of birds across a microphone array. By opening
#' the resulting jpeg images in an image viewing program (e.g. the standard Microsoft Photos app),
#' short clips of sounds can be viewed across an entire microphone array at once.
#' The authors of this package have found this to be an efficient way to view spectrograms,
#' while effectively eliminating the likelihood of double-counting sound sources that
#' may be clearly detectable on many microphones at the same time. At the present time,
#' this function only works when a settings object, st, is provided.
#'
#' @param st List. Localization settings object generated using
#'     \code{\link{processSettings}}.
#' @param lm layout matrix generated using the `layoutMatrix()` function, or a
#'     user-generated matrix in the same format. This matrix controls how the spectrograms
#'     from each station are mapped to rows and columns.
#' @param intervalLength Integer The length of each view interval to be generated, in seconds.
#'     Consecutive windows overlap, by default by 1 second. Setting intervalLength = 5 will therefore
#'     create 6-second spectrogram views, with one second overlap (e.g.
#'     0 to 6, then 5 to 11, 10 to 16, etc.).
#' @param intervals Integer or 'all'. Which intervals to write to jpeg. For testing
#'     purposes, it is often desirable to set this to, e.g. intervals = 1:5, which
#'     will create only the first five view windows, to ensure the function is working.
#' @examples
#'     #First need to convert mp3 example data to wav.
#'     #list mp3 files.
#'     f.in <- list.files(system.file('extdata', package = 'locaR'), full.names = TRUE, pattern='mp3$')
#'     #create wav names.
#'     f.out <- file.path(tempdir(), basename(f.in))
#'     #change extension.
#'     substr(f.out, nchar(f.out)-2, nchar(f.out)) <- 'wav'
#'     #Convert mp3 to wav, as required for this particular example.
#'     for(i in 1:length(f.in)) {
#'       y <- tuneR::readMP3(f.in[i])
#'       tuneR::writeWave(y, filename = f.out[i])
#'     }
#'     #Set up survey.
#'     survey <- setupSurvey(folder = tempdir(), projectName = 'Ex', run = 1,
#'                coordinatesFile = system.file('extdata', 'Vignette_Coordinates.csv',
#'                                              package = 'locaR'),
#'                siteWavsFolder = tempdir(), date = '20200617', time = '090000', surveyLength = 7)
#'     #Process settings.
#'     st <- processSettings(settings = survey, getFilepaths = TRUE, types = 'wav')
#'     #Set up layout matrix.
#'     lm <- layoutMatrix(st = st, start = 'topleft', byrow = TRUE, nrow = 3, ncol = 3)
#'     #create detection spectrograms.
#'     omniSpectro(st, lm, intervalLength = 7)
#' @export


omniSpectro = function(st, lm, intervalLength = 5, intervals = 'all') {


  specDir <- file.path(st$outputFolder,'Spectrograms')

  sl <- st$surveyLength
  il <- intervalLength

  output <- data.frame(begin=seq(0,sl-il,il),
                      end=seq(il,sl,il))

  output$text <- paste0('Sound', formatC(output$begin, width=nchar(sl), flag='0'))

  nmics <- nrow(st$files)

  #mySpec function.  Note this is not the same as the other mySpectro function! Need to make two
  #different functions! Function takes list of data and a label
  mySpec <- function(ListOfData, Sound) {
    if(is.na(ListOfData)[1]) {plot(0, axes = FALSE, xlab=NA, ylab=NA, col='white')} else{
      Mic <- ListOfData$mic
      Channel <- ListOfData$channel
      first <- ListOfData$first
      last <- ListOfData$last
      oce::imagep(ListOfData$time, ListOfData$freq, t(ListOfData$amp),
             drawPalette = FALSE, ylim=c(0,10), mar=rep(0,4), axes = FALSE,
             breaks=seq(-0,85,length.out=21), col=rev(gray.colors(20, 0,1)))
      legend('topleft', legend=paste0(Mic, ifelse(Channel==2,'b','a')), bty='n', cex=2)
      labs <- (first+1):(last-1)
      text(x=seq(1,length(labs),length.out=length(labs)),y=rep(1,length(first:last)-2),labels=labs)
      axis(side=1, labels=NA, tck=0.05, at=seq(1,length(labs),length.out=length(labs)))
      axis(side=1, labels=NA, tck=0.025, at=seq(0.5,length(labs)+0.5,length.out=length(labs)+1))
      axis(side=2, labels=NA, tck=0.025, at=seq(2,10,2))
      box()
      if(Mic==1) {text(x=5,y=9, labels=paste0(Sound))}
    }
  }

  dir.create(specDir, showWarnings = FALSE)

  #make plot of the microphone layout.
  jpeg(paste0(specDir,'/MicLayout.jpeg'), height=7, width=7, units='in', res=200)
  on.exit(if(dev.cur()>1) {dev.off()}, add = TRUE)
  par(mar=c(4,6,1,1))
  plot(st$files$Easting, st$files$Northing, pch=20, las=1,
       xlim=c(min(st$files$Easting)-30, max(st$files$Easting)+30),
       ylim=c(min(st$files$Northing)-30, max(st$files$Northing)+30), xlab='Easting', ylab='Northing')
  text(st$files$Easting, st$files$Northing, labels=st$files$Station, pos=3, cex=0.5)
  dev.off()

  #Make a plot of the spectrograms for each sound.

  if(sum(intervals == 'all')) {
    intervals <- 1:nrow(output)
  }

  for(i in intervals) {
    #Start of interval
    first <- output$begin[i]
    #End of interval (1 second overlap)
    last <- output$end[i]+1
    #length of interval
    Length <- last-first
    for(j in 1:nmics) {
      #Station from layoutMatrix
      s <- t(lm)[j]
      #file for that station.
      file <- as.character(st$files$Path[st$files$Station==s])

      #create sound1
      sound1 <- NA

      #If there is a sound file, read it.
      if(!is.na(file)) {

        #Get sample rate.
        Fs <- tuneR::readWave(file, header = TRUE)$sample.rate

        #Get bitrate
        Br <- tuneR::readWave(file, header = TRUE)$bits

        #Get time adjustment
        A <- st$files$Adjustment[st$files$Station==s]

        #Channel
        Ch <- st$files$Channel[st$files$Station==s]

        #Read appropriate channel from file.
        if(Ch == 1 | is.na(Ch)){
          sound1 <- tuneR::readWave(file, from = first-A, to = last-A, units = 'seconds')@left
        } else {
          sound1 <- tuneR::readWave(file, from = first-A, to = last-A, units = 'seconds')@right
        }

        #If there is an adjustment, and this is the first interval, add white noise to beginning.
        #This will bring files with different start times into alignment.
        if(A > 0 & first==0) {
          sound1 <- tuneR::bind(tuneR::noise(kind='white', duration=Fs*A,
                                             samp.rate=Fs, bit=Br, pcm = TRUE),
                             tuneR::Wave(sound1[1:(length(sound1)-Fs*A)],
                                         samp.rate=Fs, bit=Br, pcm = TRUE))
        }
        #Spectrogram
        sound1 <- seewave::spectro(sound1, f=Fs,  plot = FALSE, ovlp=0, norm = FALSE)

        sound1$mic <- st$files$Station[st$files$Station==s]
        sound1$channel <- st$files$Channel[st$files$Station==s]
        sound1$first <- first
        sound1$last <- last
      }

      #combine into list.
      if(j==1) {
        soundList <- list(a=sound1)
      } else {
        soundList <- append(soundList, list(a=sound1))
      }
    }

    #Create jpeg.
    jpeg(paste0(specDir, '/',output$text[i], '.jpeg'), width=30, height=15, units='in', res=200)
    on.exit(if(dev.cur()>1) {dev.off()}, add = TRUE)
    par(mfrow=c(nrow(lm),ncol(lm)))
    par(mar=c(0,0,0,0))
    par(oma=c(0,0,0,0))
    lapply(soundList, mySpec, Sound=output$text[i])
    dev.off()

    print(paste0(output$begin[i], ' seconds'))
  }
}


