
#localizeMultiple####

#This function will localize multiple detections. In particular, it optimizes whether
#to keep.InitData or not, which can speed things up about twofold.

localizeMultiple = function(st, indices, tempC = 15, plot=TRUE, InitData=NULL) {

  for(i in 1:length(indices)) {

    #First check whether InitData should be kept for index i

    currentRow = st$detections[indices[i],]

    if(i < length(indices)) {
      nextRow = st$detections[indices[i+1],]

      currentStations = as.vector(as.matrix(currentRow[,paste0('Station', 1:6)]))

      nextStations = as.vector(as.matrix(nextRow[,paste0('Station', 1:6)]))

      #Compare current stations and next stations. If they are identical, keep.InitData = TRUE
      keep.InitData = (sum(currentStations %in% nextStations) == length(currentStations) &
                         sum(nextStations %in% currentStations) == length(nextStations))
    } else {keep.InitData = TRUE}

    #InitData will generally be NULL for the first detection, inherited (sometimes) thereafter.

    loc = localizeDetection(st, index = indices[i], plot=plot,
                            keep.InitData = keep.InitData, InitData = InitData)

    currentRow$Easting = loc$location$Easting
    currentRow$Northing = loc$location$Northing
    currentRow$Elevation = loc$location$Elevation
    currentRow$Power = loc$location$Power
    currentRow$Keep = NA

    #If keep.InitData is false, reset InitData to NULL to overwrite InitData.
    #Otherwise, get it from the loc object.
    if(!keep.InitData) {InitData = NULL} else {InitData = loc$InitData}

    if(i == 1) {
      OUT = currentRow
    } else {OUT = rbind(OUT, currentRow)}
  }
  return(OUT)

}



