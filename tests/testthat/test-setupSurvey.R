library(locaR)
context("Set up survey")

test_that("Survey creation.", {
  #Without recursive search.
  s <- setupSurvey(folder = tempdir(),
                   projectName = 'Ex',
                   run = 1,
                   coordinatesFile = system.file('extdata',"Vignette_Coordinates.csv", package = 'locaR'),
                   siteWavsFolder = system.file('extdata', package = 'locaR'),
                   date = 20200617,
                   time = 90000,
                   surveyLength = 180)
  expect_is(s, 'data.frame')
  #Recursive search through folders.
  s <- setupSurvey(folder = tempdir(),
                   projectName = 'Ex',
                   run = 1,
                   coordinatesFile = system.file('extdata',"Vignette_Coordinates.csv", package = 'locaR'),
                   siteWavsFolder = system.file(package = 'locaR'),
                   date = 20200617,
                   time = 90000,
                   surveyLength = 180)
  expect_is(s, 'data.frame')
}
)

