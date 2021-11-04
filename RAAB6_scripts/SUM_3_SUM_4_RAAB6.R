#RAAB6

#v. 5th May 2021 - IM
#v. 19th July 2021 - RB - updated to work with PEEK population format and logMar acuity measurements

sum3<-data.frame(vi.levels)
sum3[,2:22] <- NA
names(sum3) <- c("vi.level",
                 
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


for (i in 1:length(vi.levels))
{
  
  sum3$female.n[i]<-sum(raab[raab$gender=="female",vi.levels[i]],na.rm=T)
  sum3$male.n[i]<-sum(raab[raab$gender=="male",vi.levels[i]],na.rm=T)
  sum3$total.n[i]<-sum(raab[,vi.levels[i]],na.rm=T)
  
  sum3$female.pct[i]<-sum(raab[raab$gender=="female",vi.levels[i]],na.rm=T)/sum(raab$vi.denom[raab$gender=="female"],na.rm=T)
  sum3$male.pct[i]<-sum(raab[raab$gender=="male",vi.levels[i]],na.rm=T)/sum(raab$vi.denom[raab$gender=="male"],na.rm=T)
  sum3$total.pct[i]<-sum(raab[,vi.levels[i]],na.rm=T)/sum(raab$vi.denom,na.rm=T)
  
  sum3$female.pct.lci[i]<-bennett.lci(sum3$female.pct[i],raab[raab$gender=="female",vi.levels[i]],raab$vi.denom[raab$gender=="female"],raab$clusterNumber[raab$gender=="female"])
  sum3$male.pct.lci[i]<-bennett.lci(sum3$male.pct[i],raab[raab$gender=="male",vi.levels[i]],raab$vi.denom[raab$gender=="male"],raab$clusterNumber[raab$gender=="male"])
  sum3$total.pct.lci[i]<-bennett.lci(sum3$total.pct[i],raab[,vi.levels[i]],raab$vi.denom,raab$clusterNumber)
  
  sum3$female.pct.uci[i]<-bennett.uci(sum3$female.pct[i],raab[raab$gender=="female",vi.levels[i]],raab$vi.denom[raab$gender=="female"],raab$clusterNumber[raab$gender=="female"])
  sum3$male.pct.uci[i]<-bennett.uci(sum3$male.pct[i],raab[raab$gender=="male",vi.levels[i]],raab$vi.denom[raab$gender=="male"],raab$clusterNumber[raab$gender=="male"])
  sum3$total.pct.uci[i]<-bennett.uci(sum3$total.pct[i],raab[,vi.levels[i]],raab$vi.denom,raab$clusterNumber)
  
  sum3$female.adj.pct[i]<-prop.age.adjust(female.subpop, raab[raab$gender=="female",], raab[raab$gender=="female",vi.levels[i]], raab$vi.denom[raab$gender=="female"])
  sum3$male.adj.pct[i]<-prop.age.adjust(male.subpop, raab[raab$gender=="male",], raab[raab$gender=="male",vi.levels[i]], raab$vi.denom[raab$gender=="male"])
  sum3$total.adj.pct[i]<-prop.age.sex.adjust(popfives, raab, raab[,vi.levels[i]], raab$vi.denom)
  
  sum3$female.adj.pct.lci[i]<-bennett.lci(sum3$female.adj.pct[i],raab[raab$gender=="female",vi.levels[i]],raab$vi.denom[raab$gender=="female"],raab$clusterNumber[raab$gender=="female"])
  sum3$male.adj.pct.lci[i]<-bennett.lci(sum3$male.adj.pct[i],raab[raab$gender=="male",vi.levels[i]],raab$vi.denom[raab$gender=="male"],raab$clusterNumber[raab$gender=="male"])
  sum3$total.adj.pct.lci[i]<-bennett.lci(sum3$total.adj.pct[i],raab[,vi.levels[i]],raab$vi.denom,raab$clusterNumber)
  
  sum3$female.adj.pct.uci[i]<-bennett.uci(sum3$female.adj.pct[i],raab[raab$gender=="female",vi.levels[i]],raab$vi.denom[raab$gender=="female"],raab$clusterNumber[raab$gender=="female"])
  sum3$male.adj.pct.uci[i]<-bennett.uci(sum3$male.adj.pct[i],raab[raab$gender=="male",vi.levels[i]],raab$vi.denom[raab$gender=="male"],raab$clusterNumber[raab$gender=="male"])
  sum3$total.adj.pct.uci[i]<-bennett.uci(sum3$total.adj.pct[i],raab[,vi.levels[i]],raab$vi.denom,raab$clusterNumber)
  
}

sum3$extrapolated.female.n<-format( (sum3$female.adj.pct * sum(female.subpop$population)), digits=0, big.interval = 3L, big.mark = " ", scientific=F)
sum3$extrapolated.male.n<-format( (sum3$male.adj.pct * sum(male.subpop$population)), digits=0, big.interval = 3L, big.mark = " ", scientific=F)
sum3$extrapolated.total.n<-format( (sum3$total.adj.pct * sum(popfives$population)), digits=0, big.interval = 3L, big.mark = " ", scientific=F)

pcts<-grep("pct",names(sum3))
sum3[,pcts]<-round( sum3[,pcts] * 100, 1 )
sum3[,pcts]<-format(sum3[,pcts],nsmall=1)
