#' Create a heatmap to visualize localization output.
#'
#' This function can be used to create a heatmap from the localization grid search. In general,
#' this function should only be used internally, but it could be useful for making customized figures.
#'
#' @param searchMap An array created by the localize() function containing x, y and z coordinates.
#'     Created by setting keep.SearchMap = TRUE when running the localize() function.
#' @param SMap An array created by the localize() function containing the power values.
#'     Created by setting keep.SearchMap = TRUE when running the localize() function.
#' @param NodeInfo A list with two elements. First element Num is numeric, specifying the number of
#'     microphones used for localization. Second element Pos is a matrix of coordinates with column names
#'     Easting, Northing and Elevation, and row names corresponding to the Station (i.e. location) names.
#' @param location Data frame. The location estimate of the sound source. Four columns: Easting, Northing,
#'     Elevation, Power. Data frame should only contain one row.
#' @param mar Numeric vector with four elements. Passed to \code{oce::imagep()} for plotting.
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


