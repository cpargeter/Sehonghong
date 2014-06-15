StatSummary<-function(df) {
  #Split by Level (Rock[1], edge[2], Flake.portion[8])
  #summary of columns 9-19
  #1) General Statistics
  #Print summary Data for complete flakes only: L.tech, W.mid, T.mid, T.bulb,TtoW, Mass
  df$LtoW<-df$L.tech/df$W.mid
  df.complete.m<-melt(df,id = c(5,34,6,9),measure = c(11:14,36,20))
  df.complete.m$Level<-factor(df.complete.m$Level)
  sp<-split(df.complete.m,df.complete.m$Level)
  
  stat.Table_Complete<-foreach (d = sp, .combine=rbind) %do%{
    d<-d[complete.cases(d),]
    l <- cbind(Measure = "Count",Level = d$Level[1],
               dcast(d, Rock_Categories ~ variable,length, subset=.(Flake.portion=="complete")))
    me<- cbind(Measure = "Mean",Level = d$Level[1],
               dcast(d, Rock_Categories ~ variable,mean,subset=.(Flake.portion=="complete")))
    md<- cbind(Measure = "Median",Level = d$Level[1],
               dcast(d, Rock_Categories ~ variable,median,subset=.(Flake.portion=="complete")))
    sd<- cbind(Measure = "St Dev",Level = d$Level[1],
               dcast(d, Rock_Categories ~ variable,function(x){
                 ifelse(length(x) > 2,sd(x),0)},subset=.(Flake.portion=="complete")))
    cv<-cbind(Measure = "CV",Level = d$Level[1],
              dcast(d, Rock_Categories ~ variable,function(x){
                ifelse(length(x) > 2,cv(x),0)},subset=.(Flake.portion=="complete")))
    rbind(l,me,md,sd,cv)
  }
  
  stat.Table_Complete[,c(4:9)]<-round(stat.Table_Complete[,c(4:9)],1)
  stat.Table_Complete<-stat.Table_Complete[with(stat.Table_Complete,
                                                order(Level,Rock_Categories)),]
  
  #Print summary Data for complete, proximal and nearly proximal
  #P.w., P.t., EPA.angle,Length.Profile
  df.m<-melt(df,id = c(5,34,6,9),measure = c(15,16,18,19))
  df.m$Level<-factor(df.m$Level)
  sp<-split(df.m,df.m$Level)
  stat.Table<-foreach (d = sp, .combine=rbind) %do%{
    d<-d[complete.cases(d),]
    l <- cbind(Measure = "Count",Level = d$Level[1],dcast(d, Rock_Categories ~ variable,subset=.(Flake.portion=="complete" | Flake.portion =='proximal' | Flake.portion == "proximal but nearly complete"),length))
    me<- cbind(Measure = "Mean",Level = d$Level[1],dcast(d, Rock_Categories ~ variable, subset=.(Flake.portion=="complete"| Flake.portion =='proximal' | Flake.portion == "proximal but nearly complete"), mean))
    md<- cbind(Measure = "Median",Level = d$Level[1],dcast(d, Rock_Categories ~ variable, subset=.(Flake.portion=="complete"| Flake.portion =='proximal' | Flake.portion == "proximal but nearly complete"), median))
    sd<- cbind(Measure = "St Dev",Level = d$Level[1],dcast(d, Rock_Categories ~ variable, subset=.(Flake.portion=="complete"| Flake.portion =='proximal' |Flake.portion == "proximal but nearly complete"), function(x){ifelse(length(x) > 2,sd(x),0)}))
    cv<- cbind(Measure = "CV",Level = d$Level[1],dcast(d, Rock_Categories ~ variable,subset=.(Flake.portion=="complete"| Flake.portion =='proximal' |Flake.portion == "proximal but nearly complete"),function(x){ifelse(length(x) > 2,cv(x),0)}))
    rbind(l,me,md,sd,cv)
  }
  
  stat.Table[,c(4:7)]<-round(stat.Table[,c(4:7)],1)
  stat.Table<-stat.Table[with(stat.Table,order(Level,Rock_Categories)),]
  list(stat.Table_Complete, stat.Table)
}  

BasicComparisons<-function(df){
  #Length Technnical
  lapply(split(df,df$Level),function(x){
    p1<-with(x,aov(L.tech~Rock_Categories))
    TukeyHSD(p1,"Rock_Categories")})
  
  lapply(split(df,df$Rock_Categories),function(x){
    p1<-with(x,aov(L.tech~Level))
    TukeyHSD(p1,"Level")})
  
  #Mid Width
  lapply(split(df,df$Level),function(x){
    p1<-with(x,aov(W.mid~Rock_Categories))
    TukeyHSD(p1,"Rock_Categories")})
  
  lapply(split(df,df$Rock_Categories),function(x){
    p1<-with(x,aov(W.mid~Level))
    TukeyHSD(p1,"Level")})
  
  #Mid Thickness
  lapply(split(df,df$Level),function(x){
    p1<-with(x,aov(T.mid~Rock_Categories))
    TukeyHSD(p1,"Rock_Categories")})
  
  lapply(split(df,df$Rock_Categories),function(x){
    p1<-with(x,aov(T.mid~Level))
    TukeyHSD(p1,"Level")})
  
  #Bulb Thickness
  lapply(split(df,df$Level),function(x){
    p1<-with(x,aov(T.bulb~Rock_Categories))
    TukeyHSD(p1,"Rock_Categories")})
  
  lapply(split(df,df$Rock_Categories),function(x){
    p1<-with(x,aov(T.bulb~Level))
    TukeyHSD(p1,"Level")})
  
  #Length to Width
  lapply(split(df,df$Level),function(x){
    p1<-with(x,aov(L.tech/W.mid~Rock_Categories))
    TukeyHSD(p1,"Rock_Categories")})
  
  lapply(split(df,df$Rock_Categories),function(x){
    p1<-with(x,aov(L.tech/W.mid~Level))
    TukeyHSD(p1,"Level")})
  
  #Mass
  lapply(split(df,df$Level),function(x){
    p1<-with(x,aov(MASS~Rock_Categories))
    TukeyHSD(p1,"Rock_Categories")})
  
  lapply(split(df,df$Rock_Categories),function(x){
    p1<-with(x,aov(MASS~Level))
    TukeyHSD(p1,"Level")}) 
}

MassByTypePlot<-function(df){
  
  dfT<-df[df$MASS<50,]
  dfT<-dfT[!is.na(dfT$Rock_Categories),]
  ggplot(dfT, aes(x=Rock_Categories, y=MASS, color=Level)) + geom_point()
  
  
  df$Level<-factor(df$Level)
  df$Rock_Categories<-factor(df$Rock_Categories)
  td<-with(df,xtabs(MASS ~ Rock_Categories+Level))
  tb<-with(df, tapply(MASS, list(Rock_Categories,Level),mean))
  
  tb.df <- as.data.frame(tb)
  tb.df <- data.frame(Material=rownames(tb.df),tb.df)
  tb.df<-tb.df[complete.cases(tb.df),]
  tb.df.m<-melt(tb.df,id =c(1),measure=c(2:4))
  colnames(tb.df.m)<-c("Material","Level","Average.Mass")
  
  td.df <- as.data.frame(td)
  td.df<-td.df[!td.df$Freq==0,]
  colnames(td.df)<-c("Material","Level","Total.Mass")
  
  t<-merge(tb.df.m,td.df,by=c("Material","Level"))
  
  
  g<-ggplot(t,aes(x=Material,y=Total.Mass,fill = Material)) + 
    geom_bar(stat="identity")+ facet_wrap(~Level)+ scale_fill_brewer()
  plot(g)
  
  g<-ggplot(t,aes(x=Material,y=Average.Mass,fill = Material)) + 
    geom_bar(stat="identity")+ facet_wrap(~Level)+ scale_fill_brewer()
  plot(g)
  
}

TypeAnalysis<-function(df){
  df$MainType<-ifelse(substr(df$Type, 1, 1) == "A","A",
    ifelse(substr(df$Type, 1, 1) == "B","B",
      ifelse(substr(df$Type, 1, 1) == "C","C",
        "Other")))
  dt_clust<-df[,c(5,34,36,11,12,14:16,18)]
  print(TypeProportions(df))
  print(TypeProportions(df))
  
  df.t<-df[,c(34,5,36,11,12,14:16,18)]
  aggregate(cbind(x1, x2)~year+month, data=df1, sum, na.rm=TRUE)
  dt<-aggregate(cbind(L.tech,W.mid,T.bulb,P.w.,P.t.,EPA.angle)~Rock_Categories+Level +MainType,data = df.t,mean,na.rm=T)
  rownames(dt)<-paste(dt[,1],dt[,2],dt[,3])
  d<-dist(dt)
  clusts<-hclust(d,"cent")
  plot(clusts, main = "Clustering by Length, Width,Bulb Thichness, Platform Width, Platform Thickness and EPA Angle",
    xlab = "")
}

FlakingProportion<-function(df){
  df<-df[!is.na(df$Dorsal.scar.pattern),]
  f.t<-aggregate(Rock~Level + Rock_Categories,data=df,FUN=length)
  
  d<-aggregate(df$Rock_Categories,list(df$Dorsal.scar.pattern,
                  df$Rock_Categories,df$Level),length)
  colnames(d)<-c("Dorsal.scar.pattern","Rock_Categories","Level","Count")
  d<-merge(d,f.t,by=c("Rock_Categories","Level"),all.x=TRUE)
  colnames(d)[5]<-"Total"
  d$Proportion<-round(d$Count/d$Total,3)
  d
  dcast(d,Rock_Categories + Level ~ Dorsal.scar.pattern, value.var="Proportion")
}
FlakingProportionCortIncluded<-function(df){
  df<-df[!is.na(df$Cort),]
  f.t<-aggregate(Rock~Level + Rock_Categories,data=df,FUN=length)
  df$Cort60<-ifelse(df$Cort %in% c("100%","61-90%","91-99%"),"Above_60","Below_60")
  
  g<-aggregate(df$Rock_Categories,list(df$Dorsal.scar.pattern,
                    df$Cort60,df$Rock_Categories,df$Level),length)
  colnames(g)<-c("Dorsal.scar.pattern","Cort60","Rock_Categories","Level","Count")
  g<-merge(g,f.t,by=c("Rock_Categories","Level"),all.x=TRUE)
  colnames(g)[6]<-"Total"
  g$Proportion<-round(g$Count/g$Total,3)
  g<-g[with(g,order(Rock_Categories,Level,Dorsal.scar.pattern)),]
  dcast(g,Rock_Categories + Level +Dorsal.scar.pattern~ Cort60, value.var="Proportion")
}
CortComparison<-function(df){
  df<-df[!is.na(df$Cort),]
  df$Cort60<-ifelse(df$Cort %in% c("100%","61-90%","91-99%"),"Above_60","Below_60")
  
  g<-aggregate(df$Dorsal.scar.pattern,list(df$Rock_Categories,df$Level,df$Cort60),summary)
  print(g)
}

Prep<-function(df){
  f.t<-aggregate(Rock~Level + Rock_Categories,data=df,FUN=length)
  
  df$Prep<-ifelse(df$P.prep %in% c("Faceted (bulb negatives are present)",
                        "Residual faceting (ridges, but no bulb negatives)",
                        "Dihedral","Reduced to an edge"),"Prepared",
                    ifelse(df$P.prep == "Absent (broken platform)","Broken",
                      "Unprepared"))
  d<-aggregate(df$Rock_Categories,list(df$Prep,
                      df$Rock_Categories,df$Level),length)
  colnames(d)<-c("Prep","Rock_Categories","Level","Count")
  d<-merge(d,f.t,by=c("Rock_Categories","Level"),all.x=TRUE)
  colnames(d)[5]<-"Total"
  d$Proportion<-round(d$Count/d$Total,3) 
  d<-d[,c(1,2,3,6)]
  dcast(d,Rock_Categories + Level ~ Prep, value.var="Proportion")
}  
 
ContAnalysis<-function(df){
  
  df$Plat.Shape<-df$P.w./df$P.t.
  df$Rock_Categories<-factor(df$Rock_Categories)
  dt<-df[df$EPA.angle>0,]
#EPA Angle
  print(aggregate(EPA.angle~ Rock_Categories +Level,data=dt,
                function(x) c(mean =mean(x), CoefVar=cv(x))))

  lapply(split(dt,dt$Level),function(x){
      p1<-with(x,aov(EPA.angle~Rock_Categories))
      tukey.edit(TukeyHSD(p1,"Rock_Categories"),"EPA Angle","Level",x$Level[1])
      TukeyHSD(p1,"Rock_Categories")})

    lapply(split(dt,dt$Rock_Categories),function(x){
      p1<-with(x,aov(EPA.angle~Level))
      tukey.edit(TukeyHSD(p1,"Level"),"EPA Angle","Rock Category",x$Rock_Categories[[1]])
      TukeyHSD(p1,"Level")}) 
#Platform Shape
  print(aggregate(Plat.Shape~ Rock_Categories +Level,data=df,
                function(x) c(mean =mean(x), CoefVar=cv(x) )))

    lapply(split(df,df$Level),function(x){
      p1<-with(x,aov(Plat.Shape~Rock_Categories))
      tukey.edit(TukeyHSD(p1,"Rock_Categories"),"Plaform Shape","Level",x$Level[1])
      TukeyHSD(p1,"Rock_Categories")})
    
    lapply(split(df,df$Rock_Categories),function(x){
      p1<-with(x,aov(Plat.Shape~Level))
      tukey.edit(TukeyHSD(p1,"Level"),"Plaform Shape","Rock Category",x$Rock_Categories[[1]])
      TukeyHSD(p1,"Level")})
}
  

FDSD<-function(df){
  #Shape of convexity
  t<-split(df,list(df$Rock_Categories,df$Level))
  
  print(data.frame(rfs = c(ChiSqTest(t,1,2)$p.value, "*", "*"),
    mos = c(round(ChiSqTest(t,4,5)$p.value,4), ChiSqTest(t,4,6)$p.value, ChiSqTest(t,5,6)$p.value),
    bas = c(ChiSqTest(t,7,8)$p.value, ChiSqTest(t,7,9)$p.value,ChiSqTest(t,8,9)$p.value),
  row.names=c("CCS vs Dyke material","CCS vs Hornfels","Dyke material vs Hornfels")))
     
  
  #rfs-mos, rfs-bas, mos-bas
  data.frame(CCS=c(ChiSqTest(t,1,4)$p.value,ChiSqTest(t,1,7)$p.value,ChiSqTest(t,4,7)$p.value),
    Dyke = c(ChiSqTest(t,2,5)$p.value, ChiSqTest(t,2,8)$p.value, ChiSqTest(t,5,8)$p.value),
    Hornfels = c("*","*", ChiSqTest(t,6,9)$p.value),
    row.names=c("rfs-mos", "rfs-bas", "mos-bas"))
  
}

TypeProportion<-function(df){
  #Create MainType which classifies the types into A, B, C, Other
  #merge the main table and the MainType which 
  d<-aggregate(df$Rock_Categories,list(df$MainType,df$Rock_Categories,df$Level),length)
  colnames(d)<-c("MainType","Rock_Categories","Level","Count")
  d<-d[,c(3,2,1,4)]
  dt<-aggregate(df$ID,list(df$Rock_Categories,df$Level),length)
  colnames(dt)<-c("Rock_Categories","Level","Total")
  d<-merge(d,dt,by=c("Rock_Categories","Level"),all.x=TRUE)
  d<-d[order(d$Level),]
  d$Proportion<-round(d$Count/d$Total,3)
  d
}

TypeSubProportion<-function(df){
  #Create table by Type
  g<-aggregate(df$Rock_Categories,list(df$Type,df$Rock_Categories,df$Level),
             length)
  colnames(g)<-c("Type","Rock_Categories","Level","Count")
  g<-g[,c(3,2,1,4)]
  g<-merge(g,dt,by=c("Rock_Categories","Level"),all.x=TRUE)
  g<-g[order(g$Level),]
  g$Proportion<-round(g$Count/g$Total,3)
  g
}

ChiSqTest<-function(t,list1,list2){
  test.m<-merge(aggregate(ID ~ Edge,data=t[[list1]],length),
                aggregate(ID ~ Edge,data=t[[list2]],length),by="Edge",all=T)
  test.m[is.na(test.m)]<-0
  rownames(test.m)<-test.m$Edge
  test.m$Edge<-NULL
  fisher.test(test.m,workspace=2e+07,hybrid=TRUE)
}

tukey.edit <- function (x,data,group,Level,...) {
    for (i in seq_along(x)) {
        xi <- x[[i]][, -4, drop = FALSE]
        yvals <- nrow(xi):1
        dev.hold()
        on.exit(dev.flush())
        plot(c(xi[, "lwr"], xi[, "upr"]), rep.int(yvals, 2), 
            type = "n", axes = FALSE, xlab = "", ylab = "", main = "", ...)
        axis(1, ...)
        axis(2, at = nrow(xi):1, labels = dimnames(xi)[[1L]], srt = 0, ...)
        abline(h = yvals, lty = 1, lwd = 0.5, col = "lightgray")
        abline(v = 0, lty = 2, lwd = 0.5, ...)
        segments(xi[, "lwr"], yvals, xi[, "upr"], yvals, ...)
        segments(as.vector(xi), rep.int(yvals - 0.1, 3), as.vector(xi), 
            rep.int(yvals + 0.1, 3), ...)
        title(main = paste(group,":",Level), 
          xlab = paste("Differences in mean levels of", data))
        box()
    }}