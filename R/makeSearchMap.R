#' Create a grid over which to search for sound sources.
#'
#' \code{makeSearchMap} creates the three-dimensional array over which to search
#' for sound sources.
#'
#' The localization algorithms used in this package can search for sound sources
#' over areas with arbitrary size and with arbitrary resolution. However, speed
#' can sometimes be slow. Generally speaking, the speed of localization
#' calculations correlates directly with the number of grid cells to be
#' searched. Speed can therefore be increased by searching a smaller area
#' (i.e. by reducing the margin, increasing zMin, or decreasing zMax),
#' or by searching with a coarser grain (i.e. by increasing the resolution).
#'
#' The final list defining the search map includes three arrays containing
#' x, y and z coordinates of
#' each grid cell, as well as the resolution and range of values in the x,
#' y and z directions. This list is passed to other functions for localization.
#'
#' @param easting vector of x coordinates of microphones.
#' @param northing vector of y coordinates of microphones.
#' @param elevation vector of z coordinates of microphones.
#' @param margin distance (in meters) to buffer around the outside of the
#'     microphone locations. The same buffer is applied to x and y coordinates.
#' @param zMin distance (in meters) to begin search relative to the microphone
#'     with the lowest elevation. Typically a small negative number to ensure
#'     that the grid search begins slightly below the lowest microphone.
#' @param zMax distance (in meters) to end search relative to the microphone
#'     with the highest elevation. Typically a positive number to ensure
#'     that the grid search ends well above the highest microphone.
#' @param resolution resolution of the search map, in meters.
#' @return A list defining the search map.
makeSearchMap  <- function(easting, northing, elevation, margin = 10, zMin = -1, zMax = 10, resolution = 1) {

  #Offset X, Y and Z coordinates to UTM grid.
  posdf <- as.data.frame(NodeInfo$Pos)

  XLim1 <- min(easting) - margin
  XLim2 <- max(easting) + margin
  YLim1 <- min(northing) - margin
  YLim2 <- max(northing) + margin
  ZLim1 <- min(elevation) + zMin #Z is offset by the minimum at the low end, and maximum at the high end.
  ZLim2 <- max(elevation) + zMax

  XAxis <- seq(XLim1, XLim2, resolution)
  YAxis <- seq(YLim1, YLim2, resolution)
  ZAxis <- seq(ZLim1, ZLim2, resolution)

  meshgrid <- expand.grid(YAxis, XAxis, ZAxis)

  XMap <- array(meshgrid[,2], dim = c(length(YAxis), length(XAxis), length(ZAxis)))
  YMap <- array(meshgrid[,1], dim = c(length(YAxis), length(XAxis), length(ZAxis)))
  ZMap <- array(meshgrid[,3], dim = c(length(YAxis), length(XAxis), length(ZAxis)))

  SearchMap <- list(XMap = XMap, YMap = YMap, ZMap = ZMap, XDen = resolution, YDen = resolution, ZDen = resolution,
                   XLim = c(XLim1, XLim2), YLim = c(YLim1, YLim2), ZLim = c(ZLim1, ZLim2))

  return(SearchMap)
}
