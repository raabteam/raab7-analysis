#RAAB6

#v1 01/09/21 RB

newtab4<-data.frame(re.vi.levels)
newtab4[,2:22] <- NA
names(newtab4) <- c("vi.level",
                 
                 "female.n",
                 "female.pct",
                 "female.pct.lci",
                 "female.pct.uci",
                 "female.adj.pct",
                 "female.adj.pct.lci",
                 "female.adj.pct.uci",
                 
                 "male.n",
                 "male.pct",
                 "male.pct.lci",
                 "male.pct.uci",
                 "male.adj.pct",
                 "male.adj.pct.lci",
                 "male.adj.pct.uci",
                 
                 "total.n",
                 "total.pct",
                 "total.pct.lci",
                 "total.pct.uci",
                 "total.adj.pct",
                 "total.adj.pct.lci",
                 "total.adj.pct.uci")


for (i in 1:length(re.vi.levels))
{
  
  newtab4$female.n[i]<-sum(raab[raab$gender=="female",re.vi.levels[i]],na.rm=T)
  newtab4$male.n[i]<-sum(raab[raab$gender=="male",re.vi.levels[i]],na.rm=T)
  newtab4$total.n[i]<-sum(raab[,re.vi.levels[i]],na.rm=T)
  
  newtab4$female.pct[i]<-sum(raab[raab$gender=="female",re.vi.levels[i]],na.rm=T)/sum(raab$vi.denom[raab$gender=="female"],na.rm=T)
  newtab4$male.pct[i]<-sum(raab[raab$gender=="male", re.vi.levels[i]],na.rm=T)/sum(raab$vi.denom[raab$gender=="male"],na.rm=T)
  newtab4$total.pct[i]<-sum(raab[,re.vi.levels[i]],na.rm=T)/sum(raab$vi.denom,na.rm=T)
  
  newtab4$female.pct.lci[i]<-bennett.lci(newtab4$female.pct[i],raab[raab$gender=="female",re.vi.levels[i]],raab$vi.denom[raab$gender=="female"],raab$clusterNumber[raab$gender=="female"])
  newtab4$male.pct.lci[i]<-bennett.lci(newtab4$male.pct[i],raab[raab$gender=="male", re.vi.levels[i]],raab$vi.denom[raab$gender=="male"],raab$clusterNumber[raab$gender=="male"])
  newtab4$total.pct.lci[i]<-bennett.lci(newtab4$total.pct[i],raab[,re.vi.levels[i]],raab$vi.denom,raab$clusterNumber)
  
  newtab4$female.pct.uci[i]<-bennett.uci(newtab4$female.pct[i],raab[raab$gender=="female", re.vi.levels[i]],raab$vi.denom[raab$gender=="female"],raab$clusterNumber[raab$gender=="female"])
  newtab4$male.pct.uci[i]<-bennett.uci(newtab4$male.pct[i],raab[raab$gender=="male", re.vi.levels[i]],raab$vi.denom[raab$gender=="male"],raab$clusterNumber[raab$gender=="male"])
  newtab4$total.pct.uci[i]<-bennett.uci(newtab4$total.pct[i],raab[,re.vi.levels[i]],raab$vi.denom,raab$clusterNumber)
  
  newtab4$female.adj.pct[i]<-prop.age.adjust(female.subpop, raab[raab$gender=="female",], raab[raab$gender=="female", re.vi.levels[i]], raab$vi.denom[raab$gender=="female"])
  newtab4$male.adj.pct[i]<-prop.age.adjust(male.subpop, raab[raab$gender=="male",], raab[raab$gender=="male", re.vi.levels[i]], raab$vi.denom[raab$gender=="male"])
  newtab4$total.adj.pct[i]<-prop.age.sex.adjust(popfives, raab, raab[,re.vi.levels[i]], raab$vi.denom)
  
  newtab4$female.adj.pct.lci[i]<-bennett.lci(newtab4$female.adj.pct[i],raab[raab$gender=="female", re.vi.levels[i]],raab$vi.denom[raab$gender=="female"],raab$clusterNumber[raab$gender=="female"])
  newtab4$male.adj.pct.lci[i]<-bennett.lci(newtab4$male.adj.pct[i],raab[raab$gender=="male", re.vi.levels[i]],raab$vi.denom[raab$gender=="male"],raab$clusterNumber[raab$gender=="male"])
  newtab4$total.adj.pct.lci[i]<-bennett.lci(newtab4$total.adj.pct[i],raab[,re.vi.levels[i]],raab$vi.denom,raab$clusterNumber)
  
  newtab4$female.adj.pct.uci[i]<-bennett.uci(newtab4$female.adj.pct[i],raab[raab$gender=="female", re.vi.levels[i]],raab$vi.denom[raab$gender=="female"],raab$clusterNumber[raab$gender=="female"])
  newtab4$male.adj.pct.uci[i]<-bennett.uci(newtab4$male.adj.pct[i],raab[raab$gender=="male", re.vi.levels[i]],raab$vi.denom[raab$gender=="male"],raab$clusterNumber[raab$gender=="male"])
  newtab4$total.adj.pct.uci[i]<-bennett.uci(newtab4$total.adj.pct[i],raab[,re.vi.levels[i]],raab$vi.denom,raab$clusterNumber)

}

newtab4$extrapolated.female.n<-format( newtab4$female.adj.pct * sum(female.subpop$population), digits = 1, big.mark = " ", big.interval= 3L, scientific=F)
newtab4$extrapolated.male.n<-format( newtab4$male.adj.pct * sum(male.subpop$population), digits = 1, big.mark = " ", big.interval= 3L, scientific=F)
newtab4$extrapolated.total.n<-format( newtab4$total.adj.pct * sum(popfives$population), digits = 1, big.mark = " ", big.interval= 3L, scientific=F)

lcis<-grep("lci",names(newtab4))
ucis<-grep("uci",names(newtab4))
newtab4[,lcis][newtab4[,lcis]<0]<-0
newtab4[,ucis][newtab4[,ucis]>1]<-1

pcts<-grep("pct",names(newtab4))
newtab4[,pcts]<-round(newtab4[,pcts] * 100, 1)
newtab4[,pcts]<-format(newtab4[,pcts], nsmall=1)

