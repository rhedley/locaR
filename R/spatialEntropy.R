#' Calculate spatial entropy.
#'
#' This function returns a measure of spatial entropy within a user-specified distance of the
#' estimated source location. The Power values from the location search are first restricted to
#' the desired buffer_size distance (i.e. within buffer_size meters of the maximum value), then
#' converted to a probability density function. Normalized spatial entropy (Shannon's entropy) is then
#' calculated using the probability density function values. By default, entropy is calculated
#' within 10 meters of the location estimate, but other values may be desirable depending on the size
#' and resolution of the array.
#'
#' @param SearchMap An array created by the localize() function containing x, y and z coordinates.
#'     Created by setting keep.SearchMap = TRUE when running the localize() function.
#' @param SMap An array created by the localize() function containing the power values.
#'     Created by setting keep.SearchMap = TRUE when running the localize() function.
#' @param location Data frame. The location estimate of the sound source. Four columns: Easting, Northing,
#'     Elevation, Power. Data frame should only contain one row.
#' @param buffer_size Numeric. The distance from `location` within which the spatial entropy should
#'     be calculated. If method == `cylinder`, all cells within this distance in the x-y direction are
#'     used. If method == `sphere`, distance is calculated in three dimensions.
#' @param alpha Numeric. Tuning parameter which scales the sensitivity when converting from power values
#'     to a probability density function. Smaller alpha values will push entropy values closer to 1, and larger
#'     alpha values will push entropy values towards zero.
#' @return Numeric. Spatial entropy of the location estimate.
#' @examples
#'     \donttest{
#'     #Get filepaths for example data.
#'     fp <- list.files(system.file('extdata', package = 'locaR'), pattern = '.mp3', full.names = TRUE)
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
#'     coordinates <- read.csv(system.file('extdata',
#'                             'Vignette_Coordinates.csv', package = 'locaR'),
#'                              stringsAsFactors = FALSE)
#'     row.names(coordinates) <- coordinates$Station
#'     #Subset coordinates.
#'     crd <- coordinates[stationSubset,]
#'     #Localize.
#'     loc <- localize(wavList = wl, coordinates = crd, locFolder = tempdir(),
#'                F_Low = row$F_Low, F_High = row$F_High,
#'                jpegName = '0001.jpeg', keep.SearchMap = TRUE)
#'
#'     #Calculate spatial entropy.
#'     spatialEntropy(SearchMap = loc$SearchMap, SMap = loc$SMap, location = loc$location,
#'           buffer_size = 10, method = 'cylinder', alpha = 5)
#'     }
#' @export

spatialEntropy <- function(SearchMap, SMap, location, buffer_size = 10, method = c('cylinder', 'sphere'),
                           alpha = 5) {

  method <- match.arg(method)

  #If cylinder, use only x and y values within buffer_size distance of the point.
  if(method == 'cylinder') {
    nearby <- sqrt((SearchMap$XMap - location$Easting)^2 + (SearchMap$YMap - location$Northing)^2) < buffer_size
  }
  #If sphere, use x,y and z values within buffer_size distance of the point.
  if(method == 'sphere') {
    nearby <- sqrt((SearchMap$XMap - location$Easting)^2 +
                     (SearchMap$YMap - location$Northing)^2 +
                     (SearchMap$ZMap - location$Elevation)^2) < buffer_size
  }

  #Extract Power values near the maximum point.
  search_values <- SMap[nearby]

  #Normalize to Probability Density Function (PDF).
  prob_map <- exp(alpha*search_values) / sum(exp(alpha*search_values))

  #Calculate spatial entropy. Adding normalization by dividing by log(N).
  spat_ent <- -sum(prob_map*log(prob_map)) / log(length(prob_map))

  return(spat_ent)
}








