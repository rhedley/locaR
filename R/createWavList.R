#' Create a list of Wave objects.
#'
#' This function reads in portions of a set of synchronized .wav files. It is intended to be used to load
#' sounds of interest for localization.
#'
#' @param paths Character vector. File paths to the set of .wav files to be read.
#' @param names Character vector. Station names for the files. Must have the same length and
#'     the names must occur in the same order as the \code{paths} variable.
#' @param from Numeric. Start time, in seconds, of the sound of interest, relative to
#'     the start of the file.
#' @param to Numeric. End time, in seconds, of the sound of interest, relative to
#'     the start of the file.
#' @param buffer Numeric. Amount of blank space around each sound of interest to be read.
#' @param adjustments Numeric vector. Amount, in seconds, to adjust the start times of recordings,
#'     if not already synchronized. Vector must be of the same length as the
#'     \code{paths} variable. If not specified, default is no adjustment.
#' @param channels Numeric vector. The channel to be read from each .wav file. Left = 1, Right = 2. If
#'     missing, default is left channel (channel 1) for all recordings.
#' @param index Numeric. If using this function within a loop, pass the index i to the function, which
#'     can help with troubleshooting if an error occurs.
#' @return Named list of Wave objects.
#' @export
#' @examples
#'     #list example mp3 files.
#'     wavs <- list.files(system.file('extdata', package = 'locaR'),
#'                        pattern = 'mp3$', full.names = TRUE)
#'     #get names of mp3 locations.
#'     nms <- substr(basename(wavs), 1, 4)
#'     #create wave list.
#'     wl <- createWavList(paths = wavs, names = nms, from = 1, to = 2, buffer = 0.1)

createWavList <- function(paths, names, from, to, buffer,
                          adjustments, channels, index = 'unknown') {

  #Check if channels or adjustments are NULL, if so, define to default.
  if(missing(channels)) {channels = rep(1, length(paths))}
  if(missing(adjustments)) {adjustments = rep(0, length(paths))}

  #Check that paths, names, channels, adjustments are all the same length, and that
  #from, to and buffer are numeric and length 1.
  N = length(paths)

  check <- all(length(names) == N, length(channels) == N, length(adjustments) == N)
  if(!check) {stop(paste('Invalid paths, names, channels, and/or adjustments arguments. Index', index))}
  check <- all(length(from) == 1, length(to) == 1, is.numeric(from),
               is.numeric(to), length(buffer) == 1, is.numeric(buffer))
  if(!check) {stop(paste('Invalid from, to or buffer arguments. Index', index))}

  #create wavList to be passed to localize().
  for(i in 1:length(paths)) {
    #Station name
    name <- names[i]

    #File path
    wav <- paths[i]

    #Channel to read.
    channel <- channels[i]

    #Amount to adjust start time (taking into account file names as well as user-specified adjustments)
    adj <- adjustments[i]

    #Adjusted start time.
    new_start_time <- from - buffer - adj

    #Check that the data to be read does not reach negative numbers.
    if((new_start_time) < 0) {
      warning(paste('Negative start time relative to file start. Index',index))
    }

    #Adjusted end time.
    new_end_time <- to + buffer - adj

    #Read wav or mp3 file.
    if(substr(wav, nchar(wav)-2, nchar(wav)) == 'wav') {
      W <- tuneR::readWave(wav, from = new_start_time,
                           to=new_end_time, units='seconds')
    }
    if(substr(wav, nchar(wav)-2, nchar(wav)) == 'mp3') {
      W <- tuneR::readMP3(wav)
      W <- tuneR::extractWave(W, from = new_start_time,
                              to=new_end_time, xunit = 'time')
    }

    #Extract correct channel.
    if(!channel %in% c(1,2)) {stop(paste('Channel must be set to 1 or 2. Index', index))}
    W <- tuneR::mono(W, which = ifelse(channel == 1, 'left', 'right'))

    if(length(W) == 0) {stop(paste('Resulting sound file', names[i], 'has length 0. This error may be caused by selecting channel 2 from a mono sound file.'))}

    if(i == 1) {wavList <- list(W)} else {wavList <- append(wavList, list(W))}

  }

  names(wavList) <- names

  return(wavList)

}
