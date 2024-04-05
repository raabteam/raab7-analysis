#RAAB5

#v1 - 16/09/21 - RB

unt<-as.data.frame(unilat.vi)
unt[,c(2:7)]<-NA
names(unt)<-c("vi.level",
              
              "male.n",
              "male.pct",
              
              "female.n",
              "female.pct",
              
              "total.n",
              "total.pct")


for (i in 1:length(unilat.vi[1:3]))
  
{
  
  unt$female.n[i]<-sum(raab[raab$gender=="female",unilat.vi[i]],na.rm=T)
  unt$male.n[i]<-sum(raab[raab$gender=="male",unilat.vi[i]],na.rm=T)
  unt$total.n[i]<-sum(raab[,unilat.vi[i]],na.rm=T)
  
  unt$female.pct[i]<-sum(raab[raab$gender=="female",unilat.vi[i]],na.rm=T)/sum(raab$vi.denom[raab$gender=="female"],na.rm=T)
  unt$male.pct[i]<-sum(raab[raab$gender=="male",unilat.vi[i]],na.rm=T)/sum(raab$vi.denom[raab$gender=="male"],na.rm=T)
  unt$total.pct[i]<-sum(raab[,unilat.vi[i]],na.rm=T)/sum(raab$vi.denom,na.rm=T)
  
  unt$female.pct.lci[i]<-bennett.lci(unt$female.pct[i],raab[raab$gender=="female",unilat.vi[i]],raab$vi.denom[raab$gender=="female"],raab$clusterId[raab$gender=="female"])
  unt$male.pct.lci[i]<-bennett.lci(unt$male.pct[i],raab[raab$gender=="male",unilat.vi[i]],raab$vi.denom[raab$gender=="male"],raab$clusterId[raab$gender=="male"])
  unt$total.pct.lci[i]<-bennett.lci(unt$total.pct[i],raab[,unilat.vi[i]],raab$vi.denom,raab$clusterId)
  
  unt$female.pct.uci[i]<-bennett.uci(unt$female.pct[i],raab[raab$gender=="female",unilat.vi[i]],raab$vi.denom[raab$gender=="female"],raab$clusterId[raab$gender=="female"])
  unt$male.pct.uci[i]<-bennett.uci(unt$male.pct[i],raab[raab$gender=="male",unilat.vi[i]],raab$vi.denom[raab$gender=="male"],raab$clusterId[raab$gender=="male"])
  unt$total.pct.uci[i]<-bennett.uci(unt$total.pct[i],raab[,unilat.vi[i]],raab$vi.denom,raab$clusterId)
  
  unt$female.adj.pct[i]<-prop.age.adjust(female.subpop, raab[raab$gender=="female",], raab[raab$gender=="female",unilat.vi[i]], raab$vi.denom[raab$gender=="female"])
  unt$male.adj.pct[i]<-prop.age.adjust(male.subpop, raab[raab$gender=="male",], raab[raab$gender=="male",unilat.vi[i]], raab$vi.denom[raab$gender=="male"])
  unt$total.adj.pct[i]<-prop.age.sex.adjust(popfives, raab, raab[,unilat.vi[i]], raab$vi.denom)
  
  unt$female.adj.pct.lci[i]<-bennett.lci(unt$female.adj.pct[i],raab[raab$gender=="female",unilat.vi[i]],raab$vi.denom[raab$gender=="female"],raab$clusterId[raab$gender=="female"])
  unt$male.adj.pct.lci[i]<-bennett.lci(unt$male.adj.pct[i],raab[raab$gender=="male",unilat.vi[i]],raab$vi.denom[raab$gender=="male"],raab$clusterId[raab$gender=="male"])
  unt$total.adj.pct.lci[i]<-bennett.lci(unt$total.adj.pct[i],raab[,unilat.vi[i]],raab$vi.denom,raab$clusterId)
  
  unt$female.adj.pct.uci[i]<-bennett.uci(unt$female.adj.pct[i],raab[raab$gender=="female",unilat.vi[i]],raab$vi.denom[raab$gender=="female"],raab$clusterId[raab$gender=="female"])
  unt$male.adj.pct.uci[i]<-bennett.uci(unt$male.adj.pct[i],raab[raab$gender=="male",unilat.vi[i]],raab$vi.denom[raab$gender=="male"],raab$clusterId[raab$gender=="male"])
  unt$total.adj.pct.uci[i]<-bennett.uci(unt$total.adj.pct[i],raab[,unilat.vi[i]],raab$vi.denom,raab$clusterId)
  
}  

unt[nrow(unt)+1,]<-NA
unt$vi.level[nrow(unt)]<-"moderate.severe.unilat"

unt$female.n[unt$vi.level=="moderate.severe.unilat"]<-sum(raab$unilat.msvi[raab$gender=="female"],na.rm=T)
unt$male.n[unt$vi.level=="moderate.severe.unilat"]<-sum(raab$unilat.msvi[raab$gender=="male"],na.rm=T)
unt$total.n[unt$vi.level=="moderate.severe.unilat"]<-sum(raab$unilat.msvi,na.rm=T)

unt$female.pct[unt$vi.level=="moderate.severe.unilat"]<-sum(raab$unilat.msvi[raab$gender=="female"],na.rm=T)/sum(raab$vi.denom[raab$gender=="female"],na.rm=T)
unt$male.pct[unt$vi.level=="moderate.severe.unilat"]<-sum(raab$unilat.msvi[raab$gender=="male"],na.rm=T)/sum(raab$vi.denom[raab$gender=="male"],na.rm=T)
unt$total.pct[unt$vi.level=="moderate.severe.unilat"]<-sum(raab$unilat.msvi,na.rm=T)/sum(raab$vi.denom,na.rm=T)

unt$female.pct.lci[unt$vi.level=="moderate.severe.unilat"]<-bennett.lci(unt$female.pct[unt$vi.level=="moderate.severe.unilat"],raab$unilat.msvi[raab$gender=="female"],raab$vi.denom[raab$gender=="female"],raab$clusterId[raab$gender=="female"])
unt$male.pct.lci[unt$vi.level=="moderate.severe.unilat"]<-bennett.lci(unt$male.pct[unt$vi.level=="moderate.severe.unilat"],raab$unilat.msvi[raab$gender=="male"],raab$vi.denom[raab$gender=="male"],raab$clusterId[raab$gender=="male"])
unt$total.pct.lci[unt$vi.level=="moderate.severe.unilat"]<-bennett.lci(unt$total.pct[unt$vi.level=="moderate.severe.unilat"],raab$unilat.msvi,raab$vi.denom,raab$clusterId)

unt$female.pct.uci[unt$vi.level=="moderate.severe.unilat"]<-bennett.uci(unt$female.pct[unt$vi.level=="moderate.severe.unilat"],raab$unilat.msvi[raab$gender=="female"],raab$vi.denom[raab$gender=="female"],raab$clusterId[raab$gender=="female"])
unt$male.pct.uci[unt$vi.level=="moderate.severe.unilat"]<-bennett.uci(unt$male.pct[unt$vi.level=="moderate.severe.unilat"],raab$unilat.msvi[raab$gender=="male"],raab$vi.denom[raab$gender=="male"],raab$clusterId[raab$gender=="male"])
unt$total.pct.uci[unt$vi.level=="moderate.severe.unilat"]<-bennett.uci(unt$total.pct[unt$vi.level=="moderate.severe.unilat"],raab$unilat.msvi,raab$vi.denom,raab$clusterId)

unt$female.adj.pct[unt$vi.level=="moderate.severe.unilat"]<-prop.age.adjust(female.subpop, raab[raab$gender=="female",], raab$unilat.msvi[raab$gender=="female"], raab$vi.denom[raab$gender=="female"])
unt$male.adj.pct[unt$vi.level=="moderate.severe.unilat"]<-prop.age.adjust(male.subpop, raab[raab$gender=="male",], raab$unilat.msvi[raab$gender=="male"], raab$vi.denom[raab$gender=="male"])
unt$total.adj.pct[unt$vi.level=="moderate.severe.unilat"]<-prop.age.sex.adjust(popfives, raab, raab$unilat.msvi, raab$vi.denom)

unt$female.adj.pct.lci[unt$vi.level=="moderate.severe.unilat"]<-bennett.lci(unt$female.adj.pct[unt$vi.level=="moderate.severe.unilat"],raab$unilat.msvi[raab$gender=="female"],raab$vi.denom[raab$gender=="female"],raab$clusterId[raab$gender=="female"])
unt$male.adj.pct.lci[unt$vi.level=="moderate.severe.unilat"]<-bennett.lci(unt$male.adj.pct[unt$vi.level=="moderate.severe.unilat"],raab$unilat.msvi[raab$gender=="male"],raab$vi.denom[raab$gender=="male"],raab$clusterId[raab$gender=="male"])
unt$total.adj.pct.lci[unt$vi.level=="moderate.severe.unilat"]<-bennett.lci(unt$total.adj.pct[unt$vi.level=="moderate.severe.unilat"],raab$unilat.msvi,raab$vi.denom,raab$clusterId)

unt$female.adj.pct.uci[unt$vi.level=="moderate.severe.unilat"]<-bennett.uci(unt$female.adj.pct[unt$vi.level=="moderate.severe.unilat"],raab$unilat.msvi[raab$gender=="female"],raab$vi.denom[raab$gender=="female"],raab$clusterId[raab$gender=="female"])
unt$male.adj.pct.uci[unt$vi.level=="moderate.severe.unilat"]<-bennett.uci(unt$male.adj.pct[unt$vi.level=="moderate.severe.unilat"],raab$unilat.msvi[raab$gender=="male"],raab$vi.denom[raab$gender=="male"],raab$clusterId[raab$gender=="male"])
unt$total.adj.pct.uci[unt$vi.level=="moderate.severe.unilat"]<-bennett.uci(unt$total.adj.pct[unt$vi.level=="moderate.severe.unilat"],raab$unilat.msvi,raab$vi.denom,raab$clusterId)

unt$extrapolated.female.n<-format( unt$female.adj.pct * sum(female.subpop$population), digits=1, scientific=F)
unt$extrapolated.male.n<-format( unt$male.adj.pct * sum(male.subpop$population), digits=1, scientific=F)
unt$extrapolated.total.n<-format( unt$total.adj.pct * sum(popfives$population), digits=1, scientific=F)

row.order<-c("blind.unilat","severe.unilat","moderate.unilat","moderate.severe.unilat","mild.unilat")
unt<-unt %>% arrange(match(unt$vi.level,row.order))

lcis<-grep("lci",names(unt))
ucis<-grep("uci",names(unt))
unt[,lcis][unt[,lcis]<0]<-0
unt[,ucis][unt[,ucis]>1]<-1

pcts<-grep("pct",names(unt))
unt[,pcts] <- round( unt[,pcts] * 100, 1)
unt[,pcts] <- format( unt[,pcts], nsmall=1 )

unt[nrow(unt),c(2:ncol(unt))]<-"*"
