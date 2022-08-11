#' Get filepath information for a date and time.
#'
#' \code{getFilepaths} reads information from a settings file (csv) or a
#' settings list and returns the file paths and other information as a
#' dataframe.
#'
#' @param settings Either a filepath to a settings file (csv) or a settings
#'     list. If a filepath, the filepath will first be passed to
#'     \code{\link{processSettings}}.
#' @param types Character, specifying the file type to be searched for. Either 'wav' or 'mp3'.
#' @return A data frame with station names, coordinates, filepaths, and any
#'     recording start-time adjustments.
#' @export
getFilepaths <- function(settings, types = 'wav') {

  #Load settings to a list.
  if(is.character(settings)) {
    if(file.exists(settings)) {
      st <- processSettings(settings)
    }
  } else if(is.list(settings) & !is.data.frame(settings)) {
      st <- settings
  } else {
      stop('settings must be a filepath or a list')
  }

  #Change time to represent minutes rather than seconds.
  timeChar <- formatC(as.numeric(st$time), width = 6, flag = '0', format = 'd')
  minute <- substr(timeChar, 1, 4)


  #Load the files from that date and time.
  if(types == 'wav') {
    Files <- list.files(st$siteFolder, recursive = T, full.names = T,
                          pattern = paste0(st$date,'.*', minute, '.*wav'))
  }
  if(types == 'mp3') {
    Files <- list.files(st$siteFolder, recursive = T, full.names = T,
                        pattern = paste0(st$date,'.*', minute, '.*mp3'))
  }

  Files <- data.frame(Path = Files, CorrFile = basename(Files),
                      Station = parseWAFileNames(filenames = basename(Files),
                                                 model = 'SM3')$prefix,
                      stringsAsFactors = F)

  #remove extra mics
  if(nrow(st$channels) > 0) {
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
  }


  #Correct file names where necessary. If no adjustments, then adjustments assumed to be 0.
  if(!is.na(st$adjustments)) {
    for(i in 1:nrow(Files)) {
      if(Files$CorrFile[i] %in% st$adjustments$Filename) {
        Files$CorrFile[i] <- st$adjustments$Adjusted[st$adjustments$Filename == Files$CorrFile[i]]
      }
    }

    #Get adjustment for each file. Different approach for manual versus filename-based adjustment.
    #Get value based on filename.
    Files$Adjustment <- as.numeric(parseWAFileNames(Files$CorrFile)$time) - as.numeric(st$time)

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
  } else {
    Files$Adjustment <- 0
  }

  Files <- merge(Files, st$coords, by='Station')

  #Strip unnecessary columns.
  Files <- Files[,colnames(Files) %in% c('Station', 'Path', 'CorrFile', 'Adjustment', 'Zone', 'Easting',
                                         'Northing', 'Elevation')]

  #Merge with channels data. If Channels not specified, default to 1.
  if(nrow(st$channels) > 0) {
    Files$Channel <- merge(Files, st$channels, by='Station')$Channel
  } else {
    Files$Channel = 1
  }

  #Return dataframe containing files and metadata.
  return(Files)

}
