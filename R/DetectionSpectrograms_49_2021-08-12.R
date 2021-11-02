
#omniSpectro####

#omniSpectro creates a visualization of many synchronized spectrograms, for the purpose
#of detecting singing birds within an array.

omniSpectro = function(st, layoutMatrix, intervalLength = 5, intervals = 'all') {

  specDir = file.path(st$outputFolder,'Spectrograms')

  sl = st$surveyLength
  il = intervalLength

  output = data.frame(begin=seq(0,sl-il,il),
                      end=seq(il,sl,il))

  output$text=paste0('Sound', formatC(output$begin, width=nchar(sl), flag='0'))

  nmics=nrow(st$files)

  #mySpectro function.  Note this is not the same as the other mySpectro function! Need to make two
  #different functions!
  mySpec= function(ListOfData, Sound) {
    if(is.na(ListOfData)[1]) {plot(0, axes=F, xlab=NA, ylab=NA, col='white')} else{
      Mic=ListOfData$mic
      Channel=ListOfData$channel
      first=ListOfData$first
      last=ListOfData$last
      imagep(ListOfData$time, ListOfData$freq, t(ListOfData$amp),
             drawPalette=F, ylim=c(0,10), mar=rep(0,4), axes=F,
             breaks=seq(-0,85,length.out=21), col=rev(gray.colors(20, 0,1)))
      legend('topleft', legend=paste0(Mic, ifelse(Channel==2,'b','a')), bty='n', cex=2)
      labs=(first+1):(last-1)
      text(x=seq(1,length(labs),length.out=length(labs)),y=rep(1,length(first:last)-2),labels=labs)
      axis(side=1, labels=NA, tck=0.05, at=seq(1,length(labs),length.out=length(labs)))
      axis(side=1, labels=NA, tck=0.025, at=seq(0.5,length(labs)+0.5,length.out=length(labs)+1))
      axis(side=2, labels=NA, tck=0.025, at=seq(2,10,2))
      box()
      if(Mic==1) {text(x=5,y=9, labels=paste0(Sound))}
    }
  }

  dir.create(specDir, showWarnings=F)

  #make plot of the microphone layout.
  jpeg(paste0(specDir,'/MicLayout.jpeg'), height=7, width=7, units='in', res=200)
  par(mar=c(4,6,1,1))
  plot(st$files$Easting, st$files$Northing, pch=20, las=1,
       xlim=c(min(st$files$Easting)-30, max(st$files$Easting)+30),
       ylim=c(min(st$files$Northing)-30, max(st$files$Northing)+30), xlab='Easting', ylab='Northing')
  text(st$files$Easting, st$files$Northing, labels=st$files$Station, pos=3, cex=0.5)
  dev.off()

  #Make a plot of the spectrograms for each sound.

  if(sum(intervals == 'all')) {
    intervals=1:nrow(output)
  }

  i=1
  j=1
  for(i in intervals) {
    #Start of interval
    first = output$begin[i]
    #End of interval (1 second overlap)
    last = output$end[i]+1
    #length of interval
    Length = last-first
    for(j in 1:nmics) {
      #Station from layoutMatrix
      s = t(layoutMatrix)[j]
      #file for that station.
      file = as.character(st$files$Path[st$files$Station==s])

      #create sound1
      sound1 = NA

      #If there is a sound file, read it.
      if(!is.na(file)) {
        #Get sample rate
        Fs = readWave(file, header=T)$sample.rate

        #Get bitrate
        Br = readWave(file, header=T)$bits

        #Get time adjustment
        A = st$files$Adjustment[st$files$Station==s]

        #Channel
        Ch = st$files$Channel[st$files$Station==s]

        #Read appropriate channel from file.
        if(Ch == 1 | is.na(Ch)){
          sound1 = readWave(file, from = first-A, to = last-A, units = 'seconds')@left
        } else {
          sound1 = readWave(file, from = first-A, to = last-A, units = 'seconds')@right
        }

        #If there is an adjustment, and this is the first interval, add white noise to beginning.
        #This will bring files with different start times into alignment.
        if(A > 0 & first==0) {
          sound1=tuneR::bind(noise(kind='white', duration=Fs*A, samp.rate=Fs, bit=Br, pcm=T),
                      Wave(sound1[1:(length(sound1)-Fs*A)], samp.rate=Fs, bit=Br, pcm=T))
        }
        #Spectrogram
        sound1 = spectro(sound1, f=Fs,  plot=F, ovlp=0, norm=F)

        sound1$mic=st$files$Station[st$files$Station==s]
        sound1$channel=st$files$Channel[st$files$Station==s]
        sound1$first=first
        sound1$last=last
      }

      #combine into list.
      if(j==1) {
        soundList=list(a=sound1)
      } else {
        soundList=append(soundList, list(a=sound1))
      }
    }

    #Create jpeg.
    jpeg(paste0(specDir, '/',output$text[i], '.jpeg'), width=30, height=15, units='in', res=200)
      par(mfrow=c(nrow(layoutMatrix),ncol(layoutMatrix)))
      par(mar=c(0,0,0,0))
      par(oma=c(0,0,0,0))
      lapply(soundList, mySpec, Sound=output$text[i])
    dev.off()

    print(paste0(output$begin[i], ' seconds'))
  }
}

#omniSpectro(st, layoutMatrix = lm, intervals = 1:5)

