#RAAB6

asa6<-as.data.frame(cumulative.vi)
asa6[,c(2:7)]<-NA
names(asa6)<-c("vi.level",
               
               "male.n",
               "male.pct",
               
               "female.n",
               "female.pct",
               
               "total.n",
               "total.pct")


for (i in 1:length(cumulative.vi))

{
  
  asa6$female.n[i]<-sum(raab[raab$gender=="female",cumulative.vi[i]],na.rm=T)
  asa6$male.n[i]<-sum(raab[raab$gender=="male",cumulative.vi[i]],na.rm=T)
  asa6$total.n[i]<-sum(raab[,cumulative.vi[i]],na.rm=T)
  
  asa6$female.pct[i]<-sum(raab[raab$gender=="female",cumulative.vi[i]],na.rm=T)/sum(raab$vi.denom[raab$gender=="female"],na.rm=T)
  asa6$male.pct[i]<-sum(raab[raab$gender=="male",cumulative.vi[i]],na.rm=T)/sum(raab$vi.denom[raab$gender=="male"],na.rm=T)
  asa6$total.pct[i]<-sum(raab[,cumulative.vi[i]],na.rm=T)/sum(raab$vi.denom,na.rm=T)
  
  asa6$female.pct.lci[i]<-bennett.lci(asa6$female.pct[i],raab[raab$gender=="female",cumulative.vi[i]],raab$vi.denom[raab$gender=="female"],raab$clusterId[raab$gender=="female"])
  asa6$male.pct.lci[i]<-bennett.lci(asa6$male.pct[i],raab[raab$gender=="male",cumulative.vi[i]],raab$vi.denom[raab$gender=="male"],raab$clusterId[raab$gender=="male"])
  asa6$total.pct.lci[i]<-bennett.lci(asa6$total.pct[i],raab[,cumulative.vi[i]],raab$vi.denom,raab$clusterId)
  
  asa6$female.pct.uci[i]<-bennett.uci(asa6$female.pct[i],raab[raab$gender=="female",cumulative.vi[i]],raab$vi.denom[raab$gender=="female"],raab$clusterId[raab$gender=="female"])
  asa6$male.pct.uci[i]<-bennett.uci(asa6$male.pct[i],raab[raab$gender=="male",cumulative.vi[i]],raab$vi.denom[raab$gender=="male"],raab$clusterId[raab$gender=="male"])
  asa6$total.pct.uci[i]<-bennett.uci(asa6$total.pct[i],raab[,cumulative.vi[i]],raab$vi.denom,raab$clusterId)
  
  asa6$female.adj.pct[i]<-prop.age.adjust(female.subpop, raab[raab$gender=="female",], raab[raab$gender=="female",cumulative.vi[i]], raab$vi.denom[raab$gender=="female"])
  asa6$male.adj.pct[i]<-prop.age.adjust(male.subpop, raab[raab$gender=="male",], raab[raab$gender=="male",cumulative.vi[i]], raab$vi.denom[raab$gender=="male"])
  asa6$total.adj.pct[i]<-prop.age.sex.adjust(popfives, raab, raab[,cumulative.vi[i]], raab$vi.denom)
  
  asa6$female.adj.pct.lci[i]<-bennett.lci(asa6$female.adj.pct[i],raab[raab$gender=="female",cumulative.vi[i]],raab$vi.denom[raab$gender=="female"],raab$clusterId[raab$gender=="female"])
  asa6$male.adj.pct.lci[i]<-bennett.lci(asa6$male.adj.pct[i],raab[raab$gender=="male",cumulative.vi[i]],raab$vi.denom[raab$gender=="male"],raab$clusterId[raab$gender=="male"])
  asa6$total.adj.pct.lci[i]<-bennett.lci(asa6$total.adj.pct[i],raab[,cumulative.vi[i]],raab$vi.denom,raab$clusterId)
  
  asa6$female.adj.pct.uci[i]<-bennett.uci(asa6$female.adj.pct[i],raab[raab$gender=="female",cumulative.vi[i]],raab$vi.denom[raab$gender=="female"],raab$clusterId[raab$gender=="female"])
  asa6$male.adj.pct.uci[i]<-bennett.uci(asa6$male.adj.pct[i],raab[raab$gender=="male",cumulative.vi[i]],raab$vi.denom[raab$gender=="male"],raab$clusterId[raab$gender=="male"])
  asa6$total.adj.pct.uci[i]<-bennett.uci(asa6$total.adj.pct[i],raab[,cumulative.vi[i]],raab$vi.denom,raab$clusterId)

}  


asa6$extrapolated.female.n <- format( asa6$female.adj.pct * sum(female.subpop$population), digits = 1, scientific=F)
asa6$extrapolated.male.n<-format( asa6$male.adj.pct * sum(male.subpop$population), digits = 1, scientific=F)
asa6$extrapolated.total.n<-format( asa6$total.adj.pct * sum(popfives$population), digits = 1, scientific=F)

lcis<-grep("lci",names(asa6))
ucis<-grep("uci",names(asa6))
asa6[,lcis][asa6[,lcis]<0]<-0
asa6[,ucis][asa6[,ucis]>1]<-1

pcts<-grep("pct",names(asa6))
asa6[,pcts]<-round( asa6[,pcts] * 100, 1 )
asa6[,pcts]<-format( asa6[,pcts], nsmall=1 )

