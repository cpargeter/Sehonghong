library(caret)
library(foreach)
library(data.table)
library(stringr)

#Import Data, create ID column and create map to get map tool number to ID, clean up data
#based on walking through database with Justin to determine missing, incorrect or unlabeled
#categorial data

cleanData<- function(){
DT = read.csv("Pilot data03.23.14.csv",header=T, sep=",",na.string=c('NA','<NA'), stringsAsFactors=F)
DT[,1]<-c(1:length(DT[,2])) #Creates an id in the first column that is a unique number
DT[,8:18]<- apply(DT[8:18],2,as.numeric)

#Levels as factors
DT$Level<-factor(DT$Level,levels = c("rfs","mos","bas","rf","barf"))

#Changes to Rock type, and create Rock Categories
DT$Rock[DT$X.=="2112"]<-"quartzite" #Fill in missing rock types
DT<-DT[!DT$X. %in% c(965,1961,1968,2036,2299,2303,2308,2310),]
DT$Rock_Categories <- ifelse(DT$Rock %in% c("Agate","CCS general","Chalcedony","Chert"),"CCS",
                      ifelse(DT$Rock %in% c("Basaltic rock","Tuff"),"Volcanics",
                      DT$Rock))
DT$Rock_Categories<-factor(DT$Rock_Categories)
#Changes to Flake Portion for 'core' or 'core on flake'
changeToCore = c(28,79,263,288,321,328,637,697,999,1033,1175,1178,1382,1399,1403,1439,
           1440,1444,1536,1538,1947,2465,2537,2543)
DT$Flake.portion[DT$X. %in% changeToCore]<-"core"
DT$Flake.portion[DT$X. %in% c(271,868,997,1053,1066,1114,1334)]<-"core on flake"
DT$Flake.portion[DT$X. %in% c(87,846,1268,2212)]<-"proximal"

#Changes to Platform Preparation on Dorsal
DT$P.prep.on.dorsal.[DT$X. == 585]<- "None (A1);"
DT$P.prep.on.dorsal.[DT$X. == 657]<- "small removals following scar ridges (A3)"
DT$P.prep.on.dorsal.[DT$X. %in% c(241,694)]<- "Other"

#Changes to Cone
DT$Cone[DT$X. %in% c(30,37,86,106,107,803,2246)]<- "none"

#Changes Cort of 1 to 100%
DT$Cort[DT$Cort == "1"]<-"100%"
DT$Cort[DT$Cort == "1-9%"]<-"1-10%"

#Changes Dorsal Scar Pattern
DT$Dorsal.scar.pattern[DT$X. == 2102]<-"NA"

#Changes Frac.initiation.point
DT$Frac.initiation.point[DT$X. == 1397]<-"NA"

#Changes to Type
DT$Type<-str_trim(DT$Type)
DT$Type[DT$Type==""]<-"Other"

#Changes to Termination
DT$Type[DT$Termination==""]<-"NA"

colnames(DT)[1]<-"ID"
Data.Table<-DT[!DT$X. %in% c(86, 1439, 2299,2303,2308,2310),]

Data.Map <-Data.Table[,c(1:2)] #Creates a map of Original ID to new ID
Data.Table$Flake.portion = factor(Data.Table$Flake.portion)
colnames(Data.Table)[8:12]<-c("L.max","L.tech","W.mid","T.mid","T.bulb")
colnames(Data.Table)[21:24]<-c("P.morph","P.prep","P.prep.dorsal","P.abr")
Data.Table[!is.na(Data.Table$ID),]
}

CalculateMass<-function(Data.Table){
  Data.Table_Sub<-Data.Table[Data.Table$Flake.portion == "complete" | 
                               Data.Table$Flake.portion == "proximal but nearly complete" |
                               Data.Table$ID == 1363,]
  Data.Table_Sub$MaxL = apply(Data.Table_Sub[8:9], 1, max, na.rm = TRUE)
  Data.Table_Sub$MaxT = apply(Data.Table_Sub[11:12], 1, max, na.rm = TRUE)
  Data.Table_Sub$VolumeMax = (Data.Table_Sub$W.mid*Data.Table_Sub$MaxL*Data.Table_Sub$MaxT)
  
  
  Data.Table_Other<-Data.Table[!Data.Table$ID %in% Data.Table_Sub$ID,]
Data.Table_Sub[Data.Table_Sub ==""]<-NA
Data.Table_Sub<-Data.Table_Sub[!is.na(Data.Table_Sub$L.max),]
for (i in c(8:12,18)){Data.Table_Sub[,i]<-as.numeric(Data.Table_Sub[,i])}
  
#Split Data into training and testing for mass calculations
Data.training = Data.Table_Sub[!is.na(Data.Table_Sub$MASS),]
Training.ID<-Data.training[,1]
Data.test = Data.Table_Sub[!Data.Table_Sub$ID %in% Training.ID,]
training<-Data.training[,c(37,20,19,18)]
training<-training[complete.cases(training),]
training$Rock<-factor(training$Rock)
SplitRockType<-split(training,training$Rock)
map<-data.frame("Rock"=character(),"Edge"=character(),"Density" = numeric())
foreach (data = SplitRockType) %do% {
  rock = as.character(data$Rock[1])
  data$Edge.profile<-factor(data$Edge.profile)
  splitData<-split(data,data$Edge.profile)
  foreach (s.data = splitData) %do% {
    edge<-as.character(s.data$Edge.profile[1])
    s.data$density = s.data$MASS/s.data$VolumeMax
   map.d<-data.table(Rock = rock, Edge = edge, Density = mean(s.data$density))
    map<-rbind(map,map.d)
  }
}
colnames(Data.test)[19]<-c("Edge")
colnames(Data.training)[19]<-c("Edge")
mappedData.test<-merge(Data.test,map,by=c("Rock","Edge"))
mappedData.test$set<-'test'
mappedData.train<-merge(Data.training,map,by=c("Rock","Edge"))
mappedData.train$set<-'train'
mappedData.test$MASS<-mappedData.test$VolumeMax*mappedData.test$Density
Data.Table_Other$set<-'Other'
colnames(Data.Table_Other)[19]<-"Edge"
Data.Table_Other$Density<-0
AllData_Mass<-rbind(mappedData.test[,c(1:34,38)],mappedData.train[,c(1:34,38)],
                    Data.Table_Other[,c(1:34,36)])
#PlotMass(AllData_Mass[,c(19,37)])
AllData_Mass
}

#Check of mass distribution
PlotMass<- function(Mass){
ggplot(Mass,aes(x = MASS)) + 
  geom_histogram(data=subset(Mass,set == 'train'),fill = "red", alpha = 0.2) +
  geom_histogram(data=subset(Mass,set == 'test'),fill = "blue", alpha = 0.2)
}
