
#Desity Plots
for (i in 1:length(sum_Data)){
  x = sum_Data[i]
  nm = names(x)
  plot(density(x[,1]),main = paste("Density of Data - ",nm),xlab=nm)
}
#Box Plot
Box(sum_Data)
CompareLevelPlots(df)

#Split by level: Plots with p-value of t-test (Welch's used)
CompareLevelPlots<-function(df){
  for (i in c(9:19)){
    dT = df[df$Level %in% c("rfs","mos","bas"),]
    t = t.test(dT[dT$Level %in% c("rfs","mos"),i],dT[dT$Level == "bas",i])
    p = round(t[3][[1]],4)
    dT[,i]<-log(dT[,i])
    nm <- colnames(dT)[i]
    g<-ggplot(dT, aes(x = dT[,i], fill = Level)) + geom_density(alpha = 0.5)+
      ggtitle(paste("Log of ",nm, " Split by levels; rfs/mos to bas difference p-value: ",p))
    ggsave(file = paste(nm, "Levels_Log",sep=".",".pdf"), width=16, height=8)
  }}



Box<-function(df){
  boxplot(L.max~Level,data=dT, main = "L Max")
  boxplot(L.tech~Level,data=dT,main = "L Tech")
  boxplot(W.mid~Level,data=dT, main = "W mid")
  boxplot(T.mid~Level,data=dT, main = "T mid")
}

#Check with Justin regarding which lengths and widths to use for this; 
#Now using L.tech, W.mid
Cluster<-function(df){
  
  Complete<-dT[dT$Flake.portion == "complete" | 
                 dT$Flake.portion == "proximal but nearly complete",]
  Complete$L.W<-log(Complete$L.max/Complete$W.mid)
  Complete$W.T<-log(Complete$W.mid/Complete$T.mid)
  g<-ggplot(Complete, aes(x = Complete$L.W, fill = Level)) + geom_density(alpha = 0.5)
  g+ geom_vline(xintercept=2)
  
  p <-ggplot(Complete, aes(x = Complete$W.T, fill = c(Level,Rock_Categories))
             + geom_density(alpha = 0.5)
  t.test(Complete[Complete$Level=="mos",39],Complete[Complete$Level == "rfs",39])
  t.test(Complete[Complete$Level=="bas",39],Complete[Complete$Level == "rfs",39])
  
  ggplot(Complete,aes(L.max,W.mid))+ geom_point(aes(color=factor(Level)))
}

Platforms<function(df){
  df$Level<-factor(df$Level)
  Plat<-df[df$Flake.portion == "complete" | 
             df$Flake.portion == "proximal but nearly complete"|
             df$Flake.portion == "proximal",]
  ggplot(Plat,aes(P.t.,EPA.angle))+ geom_point(aes(color=factor(Level)))
  Pl_rfs<-Plat[Plat$Level=="rfs",]
  Pl_mos<-Plat[Plat$Level=="mos",]
  Pl_bas<-Plat[Plat$Level=="bas",]
  lm1<-lm(EPA.angle~P.t.,data = Pl_rfs)
  lm2<-lm(EPA.angle~P.t.,data = Pl_mos)
  lm3<-lm(EPA.angle~P.t., data = Pl_bas)
  
  plot(lm1)
  
}


ContAnalysis<-function(df){
  df$Plat.Shape<-df$P.w./df$P.t.
  print(aggregate(EPA.angle~ Rock_Categories +Level,data=df,
                  function(x) c(mean =mean(x), CoefVar=cv(x))))
  
  df$Rock_Categories<-factor(df$Rock_Categories)
  dt<-df[df$EPA.angle>0,]
  #EPA Angle
  
  t<-lapply(split(dt,dt$Rock_Categories),function(x){
    with(x,kruskal(EPA.angle,Level,p.adj="bon",group=FALSE))})
  print("EPA Angle")
  print("CCS")
  print(t$CCS$comparison)
  print("Dyke Material")
  print(t$`Dyke material`$comparison)
  print("Hornfels")
  print(t$Hornfels$comparison)
  
  t1<-lapply(split(dt,dt$Level),function(x){
    with(x,kruskal(EPA.angle,Rock_Categories,p.adj="bon",group=FALSE))})
  
  print("rfs")
  print(t1$rfs$comparison)
  print("mos")
  print(t1$mos$comparison)
  print("bas")
  print(t1$bas$comparison)
  
  #Platform Shape
  
  print(aggregate(Plat.Shape~ Rock_Categories +Level,data=df,
                  function(x) c(mean =mean(x), CoefVar=cv(x) )))
  
  t2<-lapply(split(df,df$Rock_Categories),function(x){
    with(x,kruskal(Plat.Shape,Level,p.adj="bon",group=FALSE))})
  print("Platform Shape")
  print("CCS")
  print(t2$CCS$comparison)
  print("Dyke Material")
  print(t2$`Dyke material`$comparison)
  print("Hornfels")
  print(t2$Hornfels$comparison)
  
  t3<-lapply(split(df,df$Level),function(x){
    with(x,kruskal(Plat.Shape,Rock_Categories,p.adj="bon",group=FALSE))})
  
  print("rfs")
  print(t3$rfs$comparison)
  print("mos")
  print(t3$mos$comparison)
  print("bas")
  print(t3$bas$comparison)
  
  list(t,t1,t2,t3) 
}