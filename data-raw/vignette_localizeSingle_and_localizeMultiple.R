
#Vignette for localizeMultiple.


devtools::load_all()

#First, let's set up a "survey" corresponding to the recording session in the example data.
#The data was from date = 20200617 (June 17, 2020) at time = 90000 (09:00:00).
#We will first define a folder in which to create the survey.

folder <- 'D:/'

#Specify a project name.
projectName <- 'Ex'

#path to detections file (csv).
detectionsFile <- system.file('data', 'Vignette_Detections_20200617_090000.csv', package = 'solo')

#path to coordinates file (csv).
coordinatesFile <- system.file('data', 'Vignette_Coordinates.csv', package = 'solo')

#siteWavsFolder can just be the package folder, since searches occur recursively.
siteWavsFolder <- system.file(package = 'solo')

#Date.
date <- 20200617

#Time.
time <- 90000

#Survey length for this example is just 7 seconds.
surveyLengthInSeconds <- 7

#run the setupSurvey function. Note, we are not specifying an adjustmentsFile, because
#all the files were well synchronized, and we are not specifying a channelsFile because
#all files are mono. We are leaving various arguments at their default values (e.g. margin, zMin).
#We store the output of the function as an object called settings.
settings <- setupSurvey(folder = folder,
            run = 1,
            projectName = projectName,
            coordinatesFile = coordinatesFile,
            siteWavsFolder = siteWavsFolder,
            adjustmentsFile = NULL,
            channelsFile = NULL,
            date = date,
            time = time,
            surveyLengthInSeconds = surveyLengthInSeconds, zMax = 20)
settings


#The setupSurvey function creates an empty detections file. We want to use the
#detections we've already made, so let's replace the filepath in the settings object.
settings$Value[1] = detectionsFile
settings

#Now we need to process the settings to create a list. We will set getFilePaths
# to TRUE so that the path to each file is automatically found.
st <- processSettings(settings = settings, getFilepaths = TRUE)

#Now solo has set the output folder to the solo/data folder. Revert.
st$outputFolder <- file.path(folder, paste0(date, '_',
                                            formatC(time, width = 6, flag = '0', format = 'd')),
                                            'Run1')

#Now everything is set up, so we can run localizeSingle.

# loc <- localizeSingle(st = st, index = 1)
#
# loc$location

locs <- localizeMultiple(st = st, indices = 'all')

write.csv(locs, 'D:/20200617_090000/Run1/Localizations/vignette_locs.csv')





