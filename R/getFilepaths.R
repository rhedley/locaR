#' Get filepath information for a date and time.
#'
#' \code{getFilepaths} reads information from a settings file (csv) or a
#' settings list and returns the file paths and other information as a
#' dataframe.
#'
#' @param settings Either a filepath to a settings file (csv) or a settings
#'     list. If a filepath, the filepath will first be passed to
#'     \code{\link{processSettings}}.
#' @return A data frame with station names, coordinates, filepaths, and any
#'     recording start-time adjustments.
getFilepaths <- function(settings) {

  #Load settings to a list.
  if(is.character(settings)) {
    if(file.exists(settings)) {
      st <- processSettings(settings)
    }
  } else {
    if(is.list(settings)) {
      st <- settings
    } else {
      stop('settings must be a filepath or a list')
    }
  }

  #Load the files from that date and time.
  Files <- list.files(st$siteFolder, recursive = T, full.names = T, pattern = paste0(st$date,'.*', st$time, '.*wav'))
  Files <- data.frame(Path = Files, CorrFile = basename(Files), Station = parseWAFileNames(filenames = basename(Files), model = 'SM3')$prefix, stringsAsFactors = F)

  #remove extra mics
  extra <- Files$Station[!Files$Station %in% st$channels$Station]
  if(length(extra) > 0) {
    Files <- Files[!Files$Station %in% extra,]
    warning(paste0('Removing files from ', paste(extra), ' not found in channels file.\n'))
  }

  #Add missing files if needed.
  missing <- st$channels$Station[!st$channels$Station %in% Files$Station]
  if(length(missing) > 0) {
    mdf <- data.frame(Path=NA, CorrFile=NA, Station=missing)
    Files <- rbind(Files, mdf)
    Files <- Files[order(Files$Station),]
    warning(paste0('No file found for ', paste(missing), '. Adding placeholders to file list.\n'))
  }

  #Correct file names where necessary.
  for(i in 1:nrow(Files)) {
    if(Files$CorrFile[i] %in% st$adjustments$Filename) {
      Files$CorrFile[i] <- st$adjustments$Adjusted[st$adjustments$Filename == Files$CorrFile[i]]
    }
  }

  #Get adjustment for each file. Different approach for manual versus filename-based adjustment.
  #Get value based on filename.
  Files$Adjustment <- as.numeric(parseWAFileNames(Files$CorrFile)$time) - as.numeric(st$time)*100

  #Then get files requiring manual
  Odd <- st$adjustments[st$adjustments$DifferenceAdjMinusOrig != 1,]

  #Adjust those manually.
  for(i in 1:nrow(Files)) {
    C <- Files$CorrFile[i]
    if(C %in% Odd$Adjusted) {
      A <- Odd$DifferenceAdjMinusOrig[Odd$Adjusted==C]
      Files$Adjustment[i] <- A
    }
  }

  Files <- merge(Files, st$coords, by='Station')

  #Strip unnecessary columns.
  Files <- Files[,colnames(Files) %in% c('Station', 'Path', 'CorrFile', 'Adjustment', 'Zone', 'Easting',
                                         'Northing', 'Elevation')]

  #Merge with channels data.
  Files$Channel <- merge(Files, st$channels, by='Station')$Channel

  #Return dataframe containing files and metadata.
  return(Files)

}
