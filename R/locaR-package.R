#' @keywords internal
#' @details The locaR package contains functions for localizing sounds using R.
#' Localizations are carried out using the modified steered response power
#' algorithm of Cobos et al. (2011) which carries out a grid-search to find
#' the location in three dimensions where a sound was most likely to have
#' originated.
#' @references
#' Cobos, M., Martí, A., & J.J. López. 2011. A modified SRP-PHAT functional for robust real-time sound source localization with scalable spatial sampling. IEEE Signal Processing Letters. 18:71-74. doi:10.1109/LSP.2010.2091502.
"_PACKAGE"

## usethis namespace: start
#' @importFrom graphics abline
#' @importFrom graphics axis
#' @importFrom graphics box
#' @importFrom graphics layout
#' @importFrom graphics legend
#' @importFrom graphics par
#' @importFrom graphics plot
#' @importFrom graphics plot.new
#' @importFrom graphics points
#' @importFrom graphics rect
#' @importFrom graphics text
#' @importFrom grDevices dev.cur
#' @importFrom grDevices dev.off
#' @importFrom grDevices gray.colors
#' @importFrom grDevices jpeg
#' @importFrom stats dist
#' @importFrom stats fft
#' @importFrom utils browseURL
#' @importFrom utils read.csv
#' @importFrom utils write.csv
## usethis namespace: end
NULL

utils::globalVariables(c("MSRP_HT_Level0", "MSRP_HT_Level1", "MSRP_HT_Level2", "MSRP_HT_Level3"),
                       package = 'locaR')
