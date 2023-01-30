#' Create InitData.
#'
#' Internal function which creates the InitData list.
#'
#' @param NodeInfo List with elements Num, Pos.
#' @param Para List with Fs, Vc (speed of sound), and DataLen
#' @param SearchMap List with elements XDen, YDen, ZDen, XMap, YMap, ZMap
#' @param LevelFlag Integer. Only value currently supported is 2.
#' @return List.
MSRP_Init = function(NodeInfo, SearchMap, Para, LevelFlag) {

  N = NodeInfo$Num #Numbered nodes

  NPos = as.matrix(NodeInfo$Pos) #Positions

  Fs = Para$Fs #Sample rate

  Vc = Para$Vc #Speed of sound in m/s

  den = max(c(SearchMap$XDen,SearchMap$YDen,SearchMap$ZDen)) #Density of search map

  ML2 = Para$DataLen * 2-1

  MaxDataLen = Para$DataLen

  NIJ = N*(N-1)/2 #Number of pairwise comparisons between nodes.

  IJList = data.frame(matrix(NA,nrow=NIJ,ncol=2)) #List of all pairwise comparisons.

  k = 0
  for (i in 1:(NodeInfo$Num-1)) {
    for (j in (i+1):NodeInfo$Num) {
      k  = k+1
      IJList[k,1:2] = c(i,j)
    }
  }
  IJ1 = IJList[,1]
  IJ2 = IJList[,2]

  kk = length(SearchMap$XMap)

  if (LevelFlag <2) {
    InitData = list()
  } else {
    InitData = list()
    #InitData$TIndk = list(list(T1 = matrix(0, nrow=NIJ, ncol=1),
    #                      T2 = matrix(0, nrow=NIJ, ncol=1),
    #                      IndList = c()))
    InitData$TIndk = lapply(1:kk, function(i) list(T1 = rep(0,NIJ),
                               T2 = rep(0,NIJ),
                               IndList = c())) #I think this is what we want. Preallocated list.

    ones = matrix(1, nrow=1,ncol=NIJ) #Make this outside the loop for speed.
    onesML = ones*ML2

    xyz = proc.time()
    for(k in 1:kk) {

      #Make sure this line does matrix multiplication.
      #dV: Vector of distance in m from location kk to each of N nodes.
      #2023-01-30: added 0.0001 to avoid zero distances which led to divide by zero error.
      dV = sqrt(colSums((c(SearchMap$XMap[k],SearchMap$YMap[k],SearchMap$ZMap[k]) - t(NPos))^2)) + 0.0001

      #dPx: distance in the x direction from location kk to IJList nodes, normalized by total distance
      #to the node, divided by Vc. I don't understand the point of this, but so be it.
      dPx = ((SearchMap$XMap[k]-NPos[IJ2,1])/dV[IJ2] -
             (SearchMap$XMap[k]-NPos[IJ1,1])/dV[IJ1])/Vc
      dPy = ((SearchMap$YMap[k]-NPos[IJ2,2])/dV[IJ2] -
             (SearchMap$YMap[k]-NPos[IJ1,2])/dV[IJ1])/Vc
      dPz = ((SearchMap$ZMap[k]-NPos[IJ2,3])/dV[IJ2] -
             (SearchMap$ZMap[k]-NPos[IJ1,3])/dV[IJ1])/Vc

      #dP is the euclidean distance of the above values.
      dP  = sqrt(colSums(rbind(dPx,dPy,dPz)^2))


      phi   = atan2(dPx,dPy) #Why is this x then y when arguments go Y, X?

      theta = acos(dPz/dP)

      d = 0.5*den*matrixStats::colMins(1/abs(rbind(cos(phi)*sin(theta),sin(phi)*sin(theta),cos(theta))))

      #I think this is converting time to samples?
      dT1 = matrix(round(((dV[IJ2]-dV[IJ1])/Vc-dP*d)*Fs)+MaxDataLen, nrow=1)

      dT2 = matrix(round(((dV[IJ2]-dV[IJ1])/Vc+dP*d)*Fs)+MaxDataLen, nrow=1)


      dT1 = matrixStats::colMaxs(rbind(ones,dT1)) #Use pmin and pmax? Or just write over values<1
      dT1 = matrixStats::colMins(rbind(onesML,dT1)) #Is this not just recursively writing over values?
      dT2 = matrixStats::colMaxs(rbind(ones,dT2)) #Use pmin and pmax? Or just write over values<1
      dT2 = matrixStats::colMins(rbind(onesML,dT2))

      InitData$TIndk[[k]]$T1 = dT1
      InitData$TIndk[[k]]$T2 = dT2

      #sub1 = c() #Revisit.
      #sub2 = c()

      #Removed below here, since I have been using LevelFlag == 2
      # if (LevelFlag >2) {
      #   for (kij in 1:NIJ) {
      #     sub1 = c(sub1,kij*ones(1,dT2[kij]-dT1[kij]+1))
      #     sub2 = c(sub2,dT1(kij):dT2(kij))
      #   }
      #
      #   #From https://stackoverflow.com/questions/4452039/converting-between-matrix-subscripts-and-linear-indices-like-ind2sub-sub2ind-in
      #   InitData$TIndk[k]$IndList = (sub2-1)*NIJ + sub1
      #
      # }


    }
    message('Created InitData in ', round((proc.time()-xyz)[3],1), ' seconds')
  }

  return(InitData)

}









