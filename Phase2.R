library(ggplot2)
library(reshape)
library(foreach)
library(raster)
library(reshape2)
library(agricolae)
source("Phase2Functions.R")

AllData<-SmallerData(DataWithMass)
Complete<-SmallerData(Complete)
Proximal<-SmallerData(Proximal)

Stats<-StatSummary(AllData)
  print(Stats[[1]])
  print(Stats[[2]])

BasicComparisons(AllData)

MassByTypePlot(AllData)

typeAnalysis<-TypeAnalysis(AllData)
  print(typeAnalysis[[1]])
  print(typeAnalysis[[2]])

Flake<-FlakingDirection(Complete)
  print(Flake[[1]])
  print(Flake[[2]])

Prep(Proximal[!is.na(Proximal$P.w.) & !is.na(Proximal$P.t.),])

Cont<-ContAnalysis(Proximal[!is.na(Proximal$EPA.angle) & !is.na(Proximal$P.w.) & !is.na(Proximal$P.t.),])

FDSD(Complete)