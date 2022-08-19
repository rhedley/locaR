#' Create validation spectrograms.
#'
#' This function is used inside the \code{localize} function to create the panels of
#' synchronized spectrograms for manual review.
#'
#' @param ListOfData List containing several required elements to make a single panel of the plot.
#' @details The plot is created using the `imagep` function from the `oce` package, in the active
#'     graphical device.
validSpectro= function(ListOfData) {
  if(is.na(ListOfData)[1]) {plot(0, axes=F, xlab=NA, ylab=NA, col='white')} else{
    Mic=ListOfData$mic
    Channel=ListOfData$channel
    first=ListOfData$first
    last=ListOfData$last
    if(Mic==1 & Channel=='a') {
      par(mfrow=c(5,6))
      par(mar=c(0,0,0,0))
      par(oma=c(0,0,0,0))
    }
    oce::imagep(ListOfData$time, ListOfData$freq, t(ListOfData$amp),
                drawPalette=F, ylim=c(0,10),xlim=c(0,1), mar=rep(0,4), axes=F,
                breaks=seq(-0,85,length.out=21), col=rev(gray.colors(20, 0,1)))
    legend('topleft', legend=paste0(Mic, ifelse(Channel==2,'b','a')), bty='n', cex=2)
    legend('bottomright', legend=paste0('Distance = ', round(ListOfData$distance,1), ' m'),
           bty='n', cex=2)
    axis(side=1, labels=NA, tck=0.015, at=seq(0,5,0.05))
    abline(v=seq(0.05,5,0.1), lty=2)
    abline(v=seq(0,5,0.1), lty=3)
    box()
  }
}















