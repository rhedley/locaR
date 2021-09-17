
#data1: samples from wav file 1
#data2: samples from wav file 2
#Para: list with components GCCMethod, FL, FH, Fs


Rij_GCC = function(data1, data2, Para) {

  data1 = c(data1,rep(0, length(data1)))
  data2 = c(data2,rep(0, length(data2)))

  X1 = fft(data1)
  X2 = fft(data2)
  R = (X1*Conj(X2))

  if(Para$GCCMethod == 'PHAT') {
    # In case that "abs(X1 * Conj(X2) == 0)"
    AR = abs(R)
    ValidInd = AR>0
    W = rep(0, length(data1))
    W[ValidInd] = 1/AR[ValidInd]
    R = R * W
  }


  # Band selection from FL to FH
  Ind1 = ceiling(Para$FL/Para$Fs * length(X1))
  Ind2 = floor(Para$FH/Para$Fs* length(X1))
  Ind3 = length(X1)+2-Ind2;
  Ind4 = length(X1)+2-Ind1;

  R[1:Ind1] = 0
  R[Ind2:Ind3]= 0
  R[Ind4:length(R)] = 0
  Rij = SynchWave::fftshift(signal::ifft(R))
  Rij = Rij[1:(length(Rij)-1)]

  return(Rij)
}











