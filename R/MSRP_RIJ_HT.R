#' Internal function for localization.
#'
#' This function uses the InitData and other info to calculate the likelihood of sound
#' sources coming from each location. Note: the LevelFlag argument is currently redundant because
#' there is only one option. Similarly, the MSRP_HT_Level2 function could be rolled into
#' the MSRP_RIJ_HT function in the future, but for now is kept separate.
#'
#' @param NodeInfo List with elements Num, Pos.
#' @param SearchMap List with elements XDen, YDen, ZDen, XMap, YMap, ZMap
#' @param Data Matrix containing the wave samples.
#' @param Para List with Fs, Vc (speed of sound), and DataLen
#' @param LevelFlag Integer. Only value currently supported is 2.
#' @param InitData List. Created with the MSRP_Init function.
#' @author Tim Huang.
#' @return List.
MSRP_RIJ_HT = function(NodeInfo, SearchMap, Data, Para, LevelFlag, InitData) {
  ###########################################################
  # % MSRP function implemented by Tim Huang
  # % Ver 1.2
  # % with Hilbert Transform for GCC signals
  # %
  # % Input value
  # % NodeInfo: struct with 'Pos' and 'Num'
  # %
  # % SearchMap: struct with 'XMap','YMap','ZMap' and XDen , YDen and ZDen
  # % XMap, YMap,ZMap could are 2D or 3D matrix.
  # %
  # % Data: signal data, N by L matirx where N is the number of sensers and L
  # % is the sampling points of data
  # %
  # % Para: struct with
  # %   Vc speed of sound;
  # %   Fs sampling frequency;
  # %   GCCMethod
  # %   FH  upband of the band pass filter for GCC
  # %   FL  lowerband of the band pass filter for GCC
  # %
  # %
  # % LevelFlag
  # %   I wrote 4 kinds of algorithms from level 0 to 3. Higher
  # %   level one run faster but cost more RAM. Level 3 and 4 requires an
  # %   Initialization progress with function 'MSRP_Init' and InitData input
  # %       0 No initialization step required,
  # %       1 No initialization step required,
  # %       1 with some initialized value
  # %       2 Fully initialized, only available for computer with large RAM
  # %       size
  # %
  # % InitData:
  #   %  could be set as [] when LevelFlag is 0 or 1, otherwise could be obtaind
  # %  by 'InitData = MSRP_Init(NodeInfo,SearchMap,Para,LevelFlag)'
  # %
  # % Output value
  # % SMap: a 2D or 3D matrix of Power map from MSRP with its size equals to
  # % SearchMap size.
  # %
  # % Level 0 or Level 1 (NOT CURRENTLY AVAILABLE)
  # %
  # % SMap = MSRP_RIJ(NodeInfo,SearchMap,Data,Para,LevelFlag,[])
  # %
  # % Level 2 or Level 3 (ONLY LEVEL 2 AVAILABLE)
  # %
  # % outside loop
  # % InitData = MSRP_Init(NodeInfo,SearchMap,Para,LevelFlag);
  # %
  # % In a loop
  # % for
  # %   FrameData = ...
  # %   SMap = MSRP_RIJ(NodeInfo,SearchMap,FrameData,Para,LevelFlag,InitData)
  # %   Ind = localMaximum(SMap,WinSize,true);
  # %   Ind = Ind(1);
  # %   TarLocation = [SearchMap.XMap(Ind),SearchMap.YMap(Ind),SearchMap.ZMap(Ind)]
  # % end


  #narginchk(5,6) #Not sure how to do this line in R.

  # step 1: Generate MSRP map (3D)

  xLevelFlag = paste0('x',LevelFlag)

  usedFun <- switch(xLevelFlag, x0 = MSRP_HT_Level0, x1 = MSRP_HT_Level1,
                    x2 = MSRP_HT_Level2, x3 = MSRP_HT_Level3, stop("Invalid LevelFlag argument"))

  SMap = usedFun(NodeInfo,SearchMap,Data,Para,InitData)

  return(SMap)
}


#' @rdname MSRP_RIJ_HT
MSRP_HT_Level2 = function(NodeInfo,SearchMap,Data,Para,InitData) {

  N = NodeInfo$Num
  MaxDataLen = ncol(Data)
  ML2 = ncol(Data) * 2-1
  NIJ = N*(N-1)/2
  k = 0
  Rij = matrix(0, nrow=NIJ,ncol=ML2)
  for (i in 1:(NodeInfo$Num-1)) {
    for (j in (i+1):NodeInfo$Num) {
      k  = k+1
      #Rij(k,:) = Rij_GCC(Data(j,:),Data(i,:),Para)
      Rij[k,] = abs(seewave::hilbert(Rij_GCC(data1=Data[j,],data2=Data[i,],Para), f=Para$Fs)) #Check this.
    }
  }

  #Bump Rij[,1] to end. This is needed due to a minor difference between the output
  #of seewave::hilbert and matlab::hilbert.
  Rij = cbind(Rij[,2:ncol(Rij)], Rij[,1])

  SMap = array(0, dim = dim(SearchMap$XMap))
  kk = length(SMap)

  for (k in 1:kk) {
    dT1 = InitData$TIndk[[k]]$T1
    dT2 = InitData$TIndk[[k]]$T2

    sub1 = c()
    sub2 = c()
    for (kij in 1:NIJ) {
      sub1 = c(sub1, rep(kij, dT2[kij]-dT1[kij]+1))
      sub2 = c(sub2,dT1[kij]:dT2[kij])
      #SMap(k) = SMap(k)+  max(Rij(kij,sub2))
    }
    #From https://stackoverflow.com/questions/4452039/converting-between-matrix-subscripts-and-linear-indices-like-ind2sub-sub2ind-in
    tR = Rij[(sub2-1)*nrow(Rij)+sub1]
    #SMap(k) = sum(tR(tR>0))
    #SMap(k) = max(tR)
    SMap[k] = sum(tR)
  }
  return(SMap)
}

