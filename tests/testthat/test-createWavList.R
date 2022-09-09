library(locaR)
context("Create wavList")

test_that("wavList creation.", {
  folder <- system.file('extdata', package = 'locaR')
  files <- list.files(folder, pattern = '.mp3')
  paths <- file.path(folder, files)
  names <- sapply(strsplit(files, '_'), '[[', 1)
  wavList <- createWavList(paths = paths, names = names, from = 1, to = 2,
                           buffer = 0.2)
  expect_is(wavList, 'list')
  expect_is(wavList[[1]], 'Wave')
}
)
