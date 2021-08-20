

#Script to animate results.

finalResults = read.csv("D:/School stuff/Manuscripts/3DLocalization/Surveys/20200613_0500/Final/TDLO_20200613_0500_Detections_Localized.csv", stringsAsFactors=F)
library(sp)
lengthInSeconds=180
projection = CRS("+proj=utm +zone=12")
googleKey = ""

outputFolder="D:/School stuff/Manuscripts/3DLocalization/Surveys/20200613_0500/Final/Animation"




animateResults = function(finalResults, projection, googleKey, lengthInSeconds=180, outputFolder) {
  require(sp)
  require(leaflet)
  require(ggmap)
  require(magick)
  
  #Make individual identifier.
  finalResults$iid = paste0(finalResults$Species, finalResults$Individual)
  finalResults$iidnum = as.numeric(as.factor(finalResults$iid))
  #sort by iid.
  finalResults = finalResults[order(finalResults$iid),]
  
  if(!has_google_key()) {
    register_google(key=googleKey)
    if(!has_google_key()) {stop("Google Key authentication failed.")}
  }
  
  if(class(projection) != 'CRS') {stop('Ensure projection is a CRS object')}
  
  data = SpatialPointsDataFrame(coords = cbind(finalResults$Easting, finalResults$Northing), data=finalResults, proj4string = projection)
  
  transformed = spTransform(data, CRS("+proj=longlat"))
  
  #Get min and max coordinates.
  minCoords = apply(coordinates(transformed), 2, min)
  maxCoords = apply(coordinates(transformed), 2, max)
  
  #Get midpoint.
  midPoint = as.vector(minCoords+(maxCoords-minCoords)/2)
  
  myMap <- get_googlemap(center=midPoint, zoom = 17, maptype='satellite',
                         ylab='Latitude')
  
  intS = seq(0,lengthInSeconds-1,1)
  intE = intS+1
  
  i=55
  for(i in 1:length(intS)) {
    
    print(paste0('Creating jpeg ', i))
    intervalStart = intS[i]
    intervalEnd = intE[i]
    
    #Get relevan data for the interval.
    current = fadeDetections(intervalStart=intervalStart,intervalEnd=intervalEnd, transformed=transformed)
    
    #Get past data, which will be shown as faded points.
    past = fadeDetections(intervalStart=0,intervalEnd=intervalStart, transformed=transformed)
    past$alpha = 0.3
    
    #If no data were found, artificially generate filler.
    if(!is.data.frame(current)) {
      current = data.frame(x=0.1,y=0.1,alpha=0,names='None')
    }
    
    #If no data were found, artificially generate filler.
    if(!is.data.frame(past)) {
      past = data.frame(x=0.1,y=0.1,alpha=0,names='None')
    }
    
    #File path.
    fp = paste0(outputFolder, '/', formatC(i, width=3, flag='0'), '.jpg')
    
    #Write jpeg.
    jpeg(fp, height=5, width=5, res=100, units='in')
    

    print(ggmap(myMap) +
            xlab('Longitude') +
            ylab('Latitude') +
            geom_point(aes(x=x, y=y), data=current, color=rgb(0,0,1,alpha=current$alpha), na.rm=T) +
            geom_text(aes(x=x, y=y, label=names), data=current, color=rgb(1,1,1,alpha=current$alpha), nudge_y=0.0001, na.rm=T) +
            geom_point(aes(x=x, y=y), data=past, color=rgb(1,1,1,alpha=past$alpha), na.rm=T)
    )
    dev.off()
    print('Jpeg created!')

  }
}



fadeDetections = function(intervalStart=3, intervalEnd=4, transformed) {
  
  from = max(intervalStart-4,0)
  to = intervalEnd
  subset = transformed[transformed$First>=from & transformed$Last<=to,]
  
  if(nrow(subset) == 0) {
    df=NA
  } else {
    xcoords = coordinates(subset)[,1]
    ycoords = coordinates(subset)[,2]
    names = subset@data$iid
    alpha = (1-(to-subset$Last)/5)#Opaqueness
    df=data.frame(x=xcoords, y=ycoords, names=names,alpha=alpha)
  }
  
  return(df)
}

