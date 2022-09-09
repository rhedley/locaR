#' Parse Wildlife Acoustics-type file names.
#'
#' This function parses the information in file names that are structured according to Wildlife Acoustics'
#' naming convention. Specifically, the format prefix_date_time.wav or prefix_mic_date_time.wav.
#'
#' @param filenames Character vector of file names.
#' @return A data frame with prefix, channels, date, time and extension information.
#' @examples
#' \dontrun{
#' parseWAFileNames("Ex-4_0+1_20200617$090000.wav") #One file.
#' parseWAFileNames(c("Ex-4_0+1_20200617$090000.wav", "Ex-4_0+1_20200618$053000.wav")) #Multiple files.
#' }

parseWAFileNames = function(filenames) {

  df = data.frame(filename = basename(filenames),
                  prefix=NA, channels=NA, date=NA, time=NA,
                  ext=NA, stringsAsFactors=F)

  for(i in 1:nrow(df)) {

    fn = df$filename[i]

    if(is.na(fn)) {next}

    replaceDS = gsub('\\$', '_', fn)

    spl = strsplit(replaceDS, '_')

    chars <- length(spl[[1]])

    if(!chars %in% c(3,4)) {stop('File name not recognized.')}

    df$prefix[i] = sapply(spl, '[[', 1)

    df$channels[i] = sapply(spl, '[[', ifelse(chars == 4, 2, NA))

    df$date[i] = sapply(spl, '[[', chars - 1)

    temp = sapply(spl, '[[', chars)

    df$time[i] = sapply(strsplit(temp, '\\.'), '[[', 1)

    df$ext[i] = sapply(strsplit(temp, '\\.'), '[[', 2)

  }

  return(df)

}
