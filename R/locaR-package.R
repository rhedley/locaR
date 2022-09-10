#' locaR: Sound localization in R.
#'
#' The locaR package contains functions for localizing sounds using R.
#' Localizations are carried out using the modified steered response power
#' algorithm of Cobos et al. (2011) which carries out a grid-search to find
#' the location in three dimensions where a sound was most likely to have
#' originated.
#'
#' @docType package
#' @name locaR
#' @importFrom stats fft dist
#' @importFrom utils read.csv write.csv
#' @importFrom graphics abline axis box layout legend par plot plot.new points rect text
#' @importFrom grDevices dev.cur dev.off gray.colors jpeg
NULL

utils::globalVariables(c("MSRP_HT_Level0", "MSRP_HT_Level1", "MSRP_HT_Level2", "MSRP_HT_Level3"),
                       package = 'locaR')

