#createWavList

createWavList <- function(paths, names, from, to, buffer,
                          adjustments = NULL, channels = NULL, index = 'unknown') {

  #Check if channels or adjustments are NULL, if so, define to default.
  if(is.null(channels)) {channels = rep(1, length(paths))}
  if(is.null(adjustments)) {adjustments = rep(0, length(paths))}

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

    if(i == 1) {wavList <- list(W)} else {wavList <- append(wavList, list(W))}

  }

  names(wavList) <- names

  return(wavList)

}
