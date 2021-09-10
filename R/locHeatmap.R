#locHeatmap

locHeatmap = function(SearchMap, SMap, NodeInfo, location, mar) {

  xyMap = apply(SMap, c(1,2), FUN = mean)

  oce::imagep(x = SearchMap$XMap[1,,1], y = SearchMap$YMap[,1,1], t(xyMap), las=1,
         drawPalette=F, xlab = 'Easting', ylab = 'Northing', mar = mar)

  points(NodeInfo$Pos[,c('Easting', 'Northing')], cex=3)

  text(x = NodeInfo$Pos[,'Easting'], y = NodeInfo$Pos[,'Northing'],
       labels = row.names(NodeInfo$Pos), pos=3, cex=2)

  points(x=location$Easting, y=location$Northing, pch=21, bg='gray', cex=3)
}


