#' Create a heatmap to visualize localization output.
#'
#' This function can be used to create a heatmap from the localization grid search. In general,
#' this function should only be used internally, but it could be useful for making customized figures.
#'
#' @param SearchMap An array created by the localize() function containing x, y and z coordinates.
#'     Created by setting keep.SearchMap = TRUE when running the localize() function.
#' @param SMap An array created by the localize() function containing the power values.
#'     Created by setting keep.SearchMap = TRUE when running the localize() function.
#' @param NodeInfo A list with two elements. First element Num is numeric, specifying the number of
#'     microphones used for localization. Second element Pos is a matrix of coordinates with column names
#'     Easting, Northing and Elevation, and row names corresponding to the Station (i.e. location) names.
#' @param location Data frame. The location estimate of the sound source. Four columns: Easting, Northing,
#'     Elevation, Power. Data frame should only contain one row.
#' @param mar Numeric vector with four elements. Passed to \code{oce::imagep()} for plotting.
#' @examples
#'     #Get filepaths for example data.
#'     fp <- list.files(system.file('extdata', package = 'locaR'), pattern = '.mp3', full.names = T)
#'     #Add names.
#'     names(fp) <- sapply(strsplit(basename(fp), '_'), '[[', 1)
#'     #Load first row of detection data.
#'     row <- read.csv(system.file('extdata',
#'          'Vignette_Detections_20200617_090000.csv', package = 'locaR'),
#'           stringsAsFactors=F)[1,]
#'     #Get non-empty Station columns.
#'     stationSubset <- unlist(row[1,paste0('Station',1:6)])
#'     stationSubset <- stationSubset[!is.na(stationSubset) & stationSubset != '']
#'     #Create wav list.
#'     wl <- createWavList(paths = fp[stationSubset], names = stationSubset,
#'            from = row$From, to = row$To, buffer = 0.2, index=1)
#'     #Read coordinates.
#'     coordinates <- read.csv(system.file('extdata',
#'                             'Vignette_Coordinates.csv', package = 'locaR'),
#'                              stringsAsFactors = F)
#'     row.names(coordinates) <- coordinates$Station
#'     #Subset coordinates.
#'     crd <- coordinates[stationSubset,]
#'     #Localize.
#'     loc <- localize(wavList = wl, coordinates = crd, locFolder = tempdir(),
#'                F_Low = row$F_Low, F_High = row$F_High, jpegName = '0001.jpeg', keep.SearchMap = T)
#'     #Convert crd (coordinates) to matrix called NodePos.
#'     NodePos <- as.matrix(crd[,c('Easting', 'Northing', 'Elevation')])
#'     colnames(NodePos) <- c('Easting', 'Northing', 'Elevation')
#'     row.names(NodePos) <- crd$Station
#'     #Plot heatmap with locHeatmap().
#'     locHeatmap(SearchMap = loc$SearchMap, SMap = loc$SMap,
#'                 NodeInfo = list(Num = 5, Pos = NodePos), location = loc$location,
#'                 mar = c(0,0,0,0))
#' @export

locHeatmap = function(SearchMap, SMap, NodeInfo, location, mar) {

  xyMap = apply(SMap, c(1,2), FUN = mean)

  oce::imagep(x = SearchMap$XMap[1,,1], y = SearchMap$YMap[,1,1], t(xyMap), las=1,
         drawPalette=F, xlab = 'Easting', ylab = 'Northing', mar = mar)

  points(NodeInfo$Pos[,c('Easting', 'Northing')], cex=3)

  text(x = NodeInfo$Pos[,'Easting'], y = NodeInfo$Pos[,'Northing'],
       labels = row.names(NodeInfo$Pos), pos=3, cex=2)

  points(x=location$Easting, y=location$Northing, pch=21, bg='gray', cex=3)
}


