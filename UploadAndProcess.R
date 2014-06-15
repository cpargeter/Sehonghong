source("findMass.R")

DT<-cleanData()
DataWithMass<-CalculateMass(DT)
Complete<-DataWithMass[DataWithMass$Flake.portion %in% c("complete",
              "proximal but nearly complete") | DataWithMass$ID == 1363,]
Complete$Flake.portion<-factor(Complete$Flake.portion)

Proximal<-DataWithMass[DataWithMass$Flake.portion %in% c("complete",
               "proximal but nearly complete","proximal"),]
Proximal$Flake.portion<-factor(Proximal$Flake.portion)

AllData<-ReduceToThreeTimePeriods(DataWithMass)
Complete<-ReduceToThreeTimePeriods(Complete)
Proximal<-ReduceToThreeTimePeriods(Proximal)