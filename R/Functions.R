#Put all R functions here.



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














