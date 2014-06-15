source("Phase2Functions.R")

Stats<-StatSummary(AllData)
  print(Stats[[1]])
  print(Stats[[2]])

BasicComparisons(AllData)

MassByTypePlot(Complete)

typeAnalysis<-TypeAnalysis(AllData)

FlakingProportion(Complete)
FlakingProportionCortIncluded(Complete)
g<- CortComparison(Complete)
  chisq.test(c(g[1,4]),c(g[2,4]))$p.value  #CCS, rfs
  chisq.test(c(g[3,4]),c(g[4,4]))$p.value #CCS,mos
  chisq.test(c(g[5,4]),c(g[6,4]))$p.value #CCS bas
  chisq.test(c(g[9,4]),c(g[10,4]))$p.value #Dyke material, bas
  

Prep(Proximal[!is.na(Proximal$P.w.) & !is.na(Proximal$P.t.),])

Cont<-ContAnalysis(Proximal[!is.na(Proximal$EPA.angle) & !is.na(Proximal$P.w.) & !is.na(Proximal$P.t.),])

FDSD(Complete)
