library(locaR)
context("Set up survey")

test_that("Survey creation.", {
  s <- setupSurvey(folder = system.file('data-raw',package = 'locaR'),
                   projectName = 'TDLO',
                   run = 1,
                   coordinatesFile = system.file('data',"Vignette_Coordinates.csv", package = 'locaR'),
                   siteWavsFolder = system.file(package = 'locaR'),
                   date = 20200617,
                   time = 90000,
                   surveyLength = 180)
  expect_is(s, 'data.frame')
  s <- setupSurvey(folder = system.file('data-raw',package = 'locaR'),
                   projectName = 'TDLO',
                   run = 1,
                   coordinatesFile = system.file('data',"Vignette_Coordinates.csv", package = 'locaR'),
                   siteWavsFolder = system.file(package = 'locaR'),
                   date = 20200617,
                   time = 90000,
                   surveyLength = 180)
  expect_is(s, 'data.frame')
}
)

