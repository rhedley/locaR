#' Specify the spatial layout of microphones.
#'
#' \code{layoutMatrix} creates a matrix of station names, which correspond to
#' the layout of stations in space.This is passed to the \code{omniSpectro} function
#' for the purposes of generating spectrograms that align with the spatial
#' orientation of stations. The four user-specified arguments indicate where
#' the first station occurs (e.g. topleft means the first
#' station is in the northwest; the "first" station means the one with the name
#' that would appear first when sorted alphabetically). byrow means the stations
#' increase along rows (either left to right or right to left) and nrow and
#' ncol indicate how many rows and columns of microphones there are
#' (assuming the array has a rectangular shape).
#' Note that this layout function is provided for convenience, but users can easily
#' specify their own custom layouts manually.
#'
#'  @param st List. Localization settings object generated using
#'      \code{\link{processSettings}}.
#'  @param stationNames Character vector. Vector of station names. Not required if st is provided.
#'  @param start Character. When sorted alphabetically, the location of the first station name.
#'  @param byrow Logical. An indicator of whether station names increase along
#'      rows (TRUE) or along columns (FALSE)
#'  @param nrow Numeric. The number of rows of microphones in the layout.
#'  @param ncol Numeric. The number of columns of microphones in the layout.
#'  @return Matrix, containing the station names within the array. If done correctly,
#'  the matrix rows and columns should align with the spatial layout of the stations
#'  in the field.
#'  @export

layoutMatrix = function(st, stationNames = NULL,
                        start = c('topleft', 'topright', 'bottomleft', 'bottomright'),
                        byrow = TRUE, nrow, ncol) {

  #Get the station name vector either from the st object or the stationNames vector.

  if(!is.null(stationNames)) {stVec <- stationNames} else {
    if(is.list(st)) {
      stVec <- st$files$Station
    } else {
      stop('st (List) or stationNames (character vector) must be specified.')
    }
  }

  stVec <- as.character(stVec)

  stations <- data.frame(station=sort(stVec), row=NA, col=NA, stringsAsFactors=F)

  #eight combinations of start (4) and byrow (2)
  stations$row <- rep(1:nrow, each = ncol) #Top to bottom
  if(start %in% c('bottomleft', 'bottomright')) {stations$row <- rev(stations$row)} #reverse if starting from bottom.
  stations$col <- rep(1:ncol, times = nrow) #left to right.
  if(start %in% c('topright', 'bottomright')) {stations$col <- rev(stations$col)} #reverse if starting from right.

  #Create the layout matrix.
  m <- matrix(NA, nrow = nrow, ncol = ncol)

  #Store the station names.
  for(i in 1:nrow(stations)) {
    m[stations$row[i],stations$col[i]] <- stations$station[i]
  }

  #Additional adjustments (reverse rows/columns and or transpose) needed when byrow is false.
  if(!byrow) {
    if(start %in% c('bottomleft', 'topright')) {
      m <- m[nrow:1,]
      m <- m[,ncol:1]
    }
    m <- t(m)
  }

  return(m)

}





