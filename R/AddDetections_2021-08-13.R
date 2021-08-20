
#Adds detections to omniSpectro spectrograms, which allows visualization of which birds have and 
#have not been detected. Spectrograms are written to a new directory within the Spectrograms 
#directory.

addDetections = function(st, intervalLength = 5, layoutMatrix) {
  
  require(jpeg)
  
  #New output directory.
  newdir=paste0(st$outputFolder, '/Spectrograms/SpectrogramsWDetections/')
  if(!dir.exists(newdir)) {dir.create(newdir)}
  
  maxSeconds = ceiling(max(st$detections$Last, na.rm=T)/intervalLength)*intervalLength
  startVec = seq(0,maxSeconds-intervalLength, by=intervalLength)
  endVec = seq(intervalLength, maxSeconds, by=intervalLength)
  
  i=1
  for(i in 1:length(startVec)) {
    sub = st$detections[(st$detections$First >= startVec[i] & st$detections$First <= endVec[i] & !is.na(st$detections$First)) |
                       (st$detections$Last >= startVec[i] & st$detections$Last <= endVec[i] & !is.na(st$detections$First)) ,]
    
    #Get the original spectrogram file name.
    
    Ifile = paste0(dirname(newdir),'/Sound',formatC(startVec[i], width=3, flag='0'), '.jpeg')
    #Check that it exists.
    if(!file.exists(Ifile)) {stop("Spectrogram file does not exist")}
    
    #Make new spectrogram name.
    Ofile = paste0(newdir,'Sound',formatC(startVec[i], width=3, flag='0'), '.jpeg')
    if(nrow(sub)==0) {
      file.copy(Ifile, Ofile)
      next
    } 
    
    #load original jpeg.
    
    jpg=readJPEG(Ifile)
    
    
    
    #Start the jpeg.
    jpeg(Ofile, width = 30, height=15, res=100, units='in')
    par(mar=c(0,0,0,0))
    par(oma=c(0,0,0,0))
    plot(NA, type='p', axes=F, xlab=NA, ylab=NA, xlim=c(0,7*(intervalLength+1)), ylim=c(0,7*10), xaxs='i', yaxs='i')
    rasterImage(jpg, 0, 0, 7*(intervalLength+1), 7*10, xaxs='i')
    
    j=1
    for(j in 1:nrow(sub)) {
      #Define the box for the detection.
      start = sub$First[j]
      if(is.na(start)) {next}
      end = sub$Last[j]
      fmin = sub$F_Low[j]
      fmax = sub$F_High[j]
      
      
      sts = sub[j,paste0("Station", 1:6)]
      sts = sts[!is.na(sts)]
      sts = sts[sts != '']
      if(length(sts)==0) {next}
      
      #Which rows and columns of the layoutMatrix contain the stations with detections.
      l = matrix(FALSE, nrow=nrow(layoutMatrix), ncol=ncol(layoutMatrix))
      l[layoutMatrix %in% sts] = TRUE
      inds = which(l, arr.ind = TRUE)
      
      

      #Loop through and if the station is one of those detected, draw a box. If not, do nothing.
      k=1
      for(k in 1:nrow(inds)) {
        s = layoutMatrix[inds[k,1], inds[k,2]]
        row = inds[k,1] 
        col = inds[k,2]
        bottomleft = c((col-1)*(intervalLength+1),7*10-(row)*10)
        startRel = max(start - startVec[i], 0)
        endRel = min(end - startVec[i], endVec[i]+1-startVec[i])
        BoxX = c(bottomleft[1]+startRel, bottomleft[1]+endRel)
        BoxY = c(bottomleft[2]+fmin/1000, bottomleft[2]+fmax/1000)
        rect(xleft = BoxX[1], ybottom = BoxY[1], xright = BoxX[2], ytop = BoxY[2], border = rgb(1,0,0,0.5), lty=2, lwd=3)
      }
    }
    dev.off()
    print(paste0('Interval ', i, ' complete!'))
  }
  
}



