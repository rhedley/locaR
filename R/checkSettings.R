#' Check the validity of a settings file or data.frame.
#'
#' Several checks are run:
#' \enumerate{
#' \item \code{settings} is either a valid file or a data.frame.
#' \item That the adjustments file is either an existing file or ""
#' \item That the channels file is either an existing file or NULL.
#' \item That the coordinates file exists.
#' \item That the detections file exists.
#' \item That the siteWavsFolder exists.
#' \item That buffer, margin, resolution, date, time, zMin, zMax and surveyLength
#'     can all be recognized as numbers.
#' \item That tempC or soundSpeed have been defined.
#' }
#'
#' @param settings Character or data.frame. Either the path to a settingsFile (csv)
#'     or a data.frame containing settings.
#' @return Logical, indicating whether all checks were passed or not.

checkSettings <- function(settings) {

  pass <- TRUE
  #Check if settings is either data frame or valid file path.
  if(is.character(settings)) {
    if(!file.exists(settings)) {
      message('Settings must be either a data frame or a valid csv file path')
      pass <- FALSE
    }
    settings <- read.csv(settings, stringsAsFactors = FALSE)
  }

  if(!is.data.frame(settings)) {
    message('Settings must be either a data frame or a valid csv file path')
    pass <- FALSE
  }

  #Split.
  st <- split(settings$Value, f=settings$Setting)


  #Check 2: adjustments file.

  if(!file.exists(st$AdjustmentsFile) & st$AdjustmentsFile != "") {
    message('Adjustments file should be either a file that exists, or ""')
    pass <- FALSE
  }

  #Check 3.
  if(!file.exists(st$ChannelsFile) & !is.null(st$ChannelsFile)) {
    message('Invalid channels file')
    pass <- FALSE
  }

  #Check 4.
  if(!file.exists(st$CoordinatesFile)) {
    message('Coordinates file must be specified')
    pass <- FALSE
  }

  #Check 5.
  if(!file.exists(st$DetectionsFile)) {
    message('Invalid detections file')
    pass <- FALSE
  }

  #Check 6.
  if(!dir.exists(st$SiteWavsFolder)) {
    message('Invalid site wavs folder')
    pass <- FALSE
  }

  #check buffer, date, time, resolution, margin.
  if(is.na(tryCatch(as.numeric(st$Buffer), error = function(e) NA))) {
    message('Buffer must be numeric')
    pass <- FALSE
  }
  if(is.na(tryCatch(as.numeric(st$Margin), error = function(e) NA))) {
    message('Margin must be numeric')
    pass <- FALSE
  }
  if(is.na(tryCatch(as.numeric(st$Resolution), error = function(e) NA))) {
    message('Resolution must be numeric')
    pass <- FALSE
  }
  if(is.na(tryCatch(as.numeric(st$Date), error = function(e) NA))) {
    message('Date must be numeric')
    pass <- FALSE
  }
  if(is.na(tryCatch(as.numeric(st$Time), error = function(e) NA))) {
    message('Time must be numeric')
    pass <- FALSE
  }
  if(is.na(tryCatch(as.numeric(st$tempC), error = function(e) NA))) {
    tempDefined <- FALSE #temp not numeric.
    if(st$tempC != '') {
      message('tempC must be blank or numeric')
      pass <- FALSE
    }
  }
  if(is.na(tryCatch(as.numeric(st$soundSpeed), error = function(e) NA))) {
    speedDefined <- FALSE #soundSpeed not numeric.
    if(st$soundSpeed != '') {
      message('soundSpeed must be blank or numeric')
      pass <- FALSE
    }
  }

  if(!(speedDefined | tempDefined)) {
    message('tempC or soundSpeed must be defined.')
    pass <- FALSE
  }

  if(is.na(tryCatch(as.numeric(st$Zmax), error = function(e) NA))) {
    message('Zmax must be numeric')
    pass <- FALSE
  }
  if(is.na(tryCatch(as.numeric(st$Zmin), error = function(e) NA))) {
    message('Zmin must be numeric')
    pass <- FALSE
  }
  if(is.na(tryCatch(as.numeric(st$SurveyLength), error = function(e) NA))) {
    message('SurveyLength must be numeric')
    pass <- FALSE
  }

  return(pass)

}





