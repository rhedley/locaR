#' Create InitData.
#'
#' Internal function which creates the InitData list.
#'
#' @param NodeInfo List with elements Num, Pos.
#' @param SearchMap List with elements XDen, YDen, ZDen, XMap, YMap, ZMap
#' @param Data Matrix containing the wave samples.
#' @param Para List with Fs, Vc (speed of sound), and DataLen
#' @param LevelFlag Integer. Only value currently supported is 2.
#' @param InitData List. Created with the MSRP_Init function.
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
  # % Level 0 or Level 1
  # %
  # % SMap = MSRP_RIJ(NodeInfo,SearchMap,Data,Para,LevelFlag,[])
  # %
  # % Level 2 or Level 3
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

# step 2: Searching. Skipping Level0 and Level1 for now.

# function SMap = MSRP_HT_Level0(NodeInfo,SearchMap,Data,Para)
# N = NodeInfo.Num;
# NPos = NodeInfo.Pos;
# Fs = Para.Fs;
# Vc = Para.Vc;
# den = max([SearchMap.XDen,SearchMap.YDen,SearchMap.ZDen]);
# MaxDataLen = size(Data,2);
# ML2 = size(Data,2) * 2-1;
#
# NIJ = N*(N-1)/2;
# IJList = zeros(NIJ,2);
# k = 0;
# for i = 1:NodeInfo.Num-1
# for j = i+1:NodeInfo.Num
# k  = k+1;
# IJList(k,1:2) = [i,j];
# end
# end
#
# SMap = zeros(size(SearchMap.XMap));
# kk = numel(SMap);
#
# for k = 1:kk
# dV = sqrt(sum((ones(N,1)*[SearchMap.XMap(k),SearchMap.YMap(k),SearchMap.ZMap(k)]-NPos).^2,2));
# dPx = ((SearchMap.XMap(k)-NPos(IJList(:,2),1))./dV(IJList(:,2))-...
#        (SearchMap.XMap(k)-NPos(IJList(:,1),1))./dV(IJList(:,1)))/Vc;
# dPy = ((SearchMap.YMap(k)-NPos(IJList(:,2),2))./dV(IJList(:,2))-...
#        (SearchMap.YMap(k)-NPos(IJList(:,1),2))./dV(IJList(:,1)))/Vc;
# dPz = ((SearchMap.ZMap(k)-NPos(IJList(:,2),3))./dV(IJList(:,2))-...
#        (SearchMap.ZMap(k)-NPos(IJList(:,1),3))./dV(IJList(:,1)))/Vc;
# dP  = sqrt(sum([dPx,dPy,dPz].^2,2));
# phi   = atan2(dPx,dPy);
# theta = acos(dPz./dP);
# d = 0.5*den*min(1./abs([cos(phi).*sin(theta),sin(phi).*sin(theta),cos(theta)]),[],2);
# dT1 = round(((dV(IJList(:,2))-dV(IJList(:,1)))/Vc-dP.*d)*Fs)+MaxDataLen;
# dT2 = round(((dV(IJList(:,2))-dV(IJList(:,1)))/Vc+dP.*d)*Fs)+MaxDataLen;
# dT1 = max([ones(size(dT1)),dT1],[],2);
# dT1 = min([ones(size(dT1))*ML2,dT1],[],2);
# dT2 = max([ones(size(dT2)),dT2],[],2);
# dT2 = min([ones(size(dT2))*ML2,dT2],[],2);
#
# for kij = 1:NIJ
# Rij = Rij_GCC(Data(IJList(kij,2),:),Data(IJList(kij,1),:),Para);
# Rij_HT = abs(hilbert(Rij));% New
# tR = Rij_HT(dT1(kij):dT2(kij));
# SMap(k) = SMap(k)+  sum(tR);
# end
# end
#
#
# function SMap = MSRP_HT_Level1(NodeInfo,SearchMap,Data,Para)
#
# N = NodeInfo.Num;
# NPos = NodeInfo.Pos;
# Fs = Para.Fs;
# Vc = Para.Vc;
# den = max([SearchMap.XDen,SearchMap.YDen,SearchMap.ZDen]);
# MaxDataLen = size(Data,2);
# ML2 = size(Data,2) * 2-1;
# NIJ = N*(N-1)/2;
# IJList = zeros(NIJ,2);
# k = 0;
# Rij = zeros(NIJ,ML2);
# for i = 1:NodeInfo.Num-1
# for j = i+1:NodeInfo.Num
# k  = k+1;
# IJList(k,1:2) = [i,j];
# %Rij(k,:) = Rij_GCC(Data(j,:),Data(i,:),Para);
# Rij(k,:) = abs(hilbert(Rij_GCC(Data(j,:),Data(i,:),Para)));
# end
# end
#
# SMap = zeros(size(SearchMap.XMap));
# kk = numel(SMap);
#
# for k = 1:kk
# dV = sqrt(sum((ones(N,1)*[SearchMap.XMap(k),SearchMap.YMap(k),SearchMap.ZMap(k)]-NPos).^2,2));
# dPx = ((SearchMap.XMap(k)-NPos(IJList(:,2),1))./dV(IJList(:,2))-...
#        (SearchMap.XMap(k)-NPos(IJList(:,1),1))./dV(IJList(:,1)))/Vc;
# dPy = ((SearchMap.YMap(k)-NPos(IJList(:,2),2))./dV(IJList(:,2))-...
#        (SearchMap.YMap(k)-NPos(IJList(:,1),2))./dV(IJList(:,1)))/Vc;
# dPz = ((SearchMap.ZMap(k)-NPos(IJList(:,2),3))./dV(IJList(:,2))-...
#        (SearchMap.ZMap(k)-NPos(IJList(:,1),3))./dV(IJList(:,1)))/Vc;
# dP  = sqrt(sum([dPx,dPy,dPz].^2,2));
# phi   = atan2(dPx,dPy);
# theta = acos(dPz./dP);
# d = 0.5*den*min(1./abs([cos(phi).*sin(theta),sin(phi).*sin(theta),cos(theta)]),[],2);
# dT1 = round(((dV(IJList(:,2))-dV(IJList(:,1)))/Vc-dP.*d)*Fs)+MaxDataLen;
# dT2 = round(((dV(IJList(:,2))-dV(IJList(:,1)))/Vc+dP.*d)*Fs)+MaxDataLen;
# dT1 = max([ones(size(dT1)),dT1],[],2);
# dT1 = min([ones(size(dT1))*ML2,dT1],[],2);
# dT2 = max([ones(size(dT2)),dT2],[],2);
# dT2 = min([ones(size(dT2))*ML2,dT2],[],2);
#
# sub1 = [];
# sub2 = [];
# for kij = 1:NIJ
# sub1 = [sub1,kij*ones(1,dT2(kij)-dT1(kij)+1)];
# sub2 = [sub2,dT1(kij):dT2(kij)];
# %SearchMap.SMap(k) = SearchMap.SMap(k)+  mean(Rij(dT1(kij):dT2(kij)));
# end
# tR = Rij(sub2ind(size(Rij),sub1,sub2));
# % SMap(k) = sum(tR(tR>0));
# SMap(k) = sum(tR);
# end

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



# function SMap = MSRP_Level3(NodeInfo,SearchMap,Data,Para,InitData)
#
# N = NodeInfo.Num;
# MaxDataLen = size(Data,2);
# ML2 = size(Data,2) * 2-1;
# NIJ = N*(N-1)/2;
# k = 0;
#
# Rij = zeros(NIJ,ML2);
# for i = 1:NodeInfo.Num-1
# for j = i+1:NodeInfo.Num
# k  = k+1;
# %Rij(k,:) = Rij_GCC(Data(j,:),Data(i,:),Para);
# Rij(k,:) = abs(hilbert(Rij_GCC(Data(j,:),Data(i,:),Para)));
# end
# end
#
# SMap = zeros(size(SearchMap.XMap));
# kk = numel(SMap);
#
# for k = 1:kk
# tR = Rij(InitData.TIndk(k).IndList);
# %SMap(k) = sum(tR(tR>0));
# SMap(k) = sum(tR);
# end
