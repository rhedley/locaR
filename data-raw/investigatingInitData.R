

#load("C:/Users/Richard Hedley/Documents/GitHub/solo/data-raw/InitDataLocalize.RData")
#load("C:/Users/Richard Hedley/Documents/GitHub/solo/data-raw/InitDataLocalizeSingle.RData")

l$TIndk[[1]]$T1
y$TIndk[[1]]$T1

locT1 = rep(NA, 10*length(l$TIndk))
lsT1 = rep(NA, 10*length(y$TIndk))

i=42
for(i in 1:length(locT1)) {
  t = floor(i/10)+1
  j = i - floor(i/10)*10
  if(j == 0) {j=10}
  locT1[i] = l$TIndk[[t]]$T1[j]
  lsT1[i] = y$TIndk[[t]]$T1[j]
}

plot(locT1, lsT1)
hist(locT1)


#T2 comparison.
locT2 = rep(NA, 10*length(l$TIndk))
lsT2 = rep(NA, 10*length(y$TIndk))

i=42
for(i in 1:length(locT2)) {
  t = floor(i/10)+1
  j = i - floor(i/10)*10
  if(j == 0) {j=10}
  locT2[i] = l$TIndk[[t]]$T2[j]
  lsT2[i] = y$TIndk[[t]]$T2[j]
}

plot(locT2, lsT2)

#T2 - T1 comparison.

locDiff = locT2 - locT1
lsDiff = lsT2 - lsT1

plot(locDiff, lsDiff)





