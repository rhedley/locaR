
folder = "D:/School stuff/Manuscripts/3DLocalization/Surveys/"
projectName = 'TDLO'
coordinatesFile = "D:/School stuff/Localization/UpdatedWorkflow_2020-11-11/MASTER_LocalizationStations_2021-04-16.csv"
siteWavsFolder = 'E:/TDLO/TDLO-001/'
adjustmentsFile = NULL
channelsFile = NULL
date = 20200515
time = 60000
surveyLengthInSeconds = 180

settings <- setupSurvey(folder = folder,
                 projectName = projectName,
                 coordinatesFile = coordinatesFile,
                 siteWavsFolder = siteWavsFolder,
                 adjustmentsFile = adjustmentsFile,
                 channelsFile = channelsFile,
                 date = date,
                 time = time,
                 surveyLengthInSeconds = surveyLengthInSeconds)
settings
