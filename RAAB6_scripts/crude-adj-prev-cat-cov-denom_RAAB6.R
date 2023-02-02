#RAAB7

#v1 16/10/22 - RB

catcovdenom<-as.data.frame(vi.levels)
catcovdenom[,c(2:25)]<-NA
names(catcovdenom)<-c("vi.level",
                  
                  "male.n",
                  "male.pct",
                  "male.pct.lci",
                  "male.pct.uci",
                  "male.adj.pct",
                  "male.adj.pct.lci",
                  "male.adj.pct.uci",
                  "extrapolated.male.n",
                  
                  "female.n",
                  "female.pct",
                  "female.pct.lci",
                  "female.pct.uci",
                  "female.adj.pct",
                  "female.adj.pct.lci",
                  "female.adj.pct.uci",
                  "extrapolated.female.n",
                  
                  "total.n",
                  "total.pct",
                  "total.pct.lci",
                  "total.pct.uci",
                  "total.adj.pct",
                  "total.adj.pct.lci",
                  "total.adj.pct.uci",
                  "extrapolated.total.n")

catcovdenom$female.n[catcovdenom$vi.level=="blind"]<-sum(raab$z_case_360[raab$gender=="female"],na.rm=T)
catcovdenom$female.n[catcovdenom$vi.level=="severe.vi"]<-sum(raab$z_case_660[raab$gender=="female"],na.rm=T)
catcovdenom$female.n[catcovdenom$vi.level=="moderate.vi"]<-sum(raab$z_case_618[raab$gender=="female"],na.rm=T)
catcovdenom$female.n[catcovdenom$vi.level=="mild.vi"]<-sum(raab$z_case_612[raab$gender=="female"],na.rm=T)

catcovdenom$male.n[catcovdenom$vi.level=="blind"]<-sum(raab$z_case_360[raab$gender=="male"],na.rm=T)
catcovdenom$male.n[catcovdenom$vi.level=="severe.vi"]<-sum(raab$z_case_660[raab$gender=="male"],na.rm=T)
catcovdenom$male.n[catcovdenom$vi.level=="moderate.vi"]<-sum(raab$z_case_618[raab$gender=="male"],na.rm=T)
catcovdenom$male.n[catcovdenom$vi.level=="mild.vi"]<-sum(raab$z_case_612[raab$gender=="male"],na.rm=T)

catcovdenom$total.n[catcovdenom$vi.level=="blind"]<-sum(raab$z_case_360,na.rm=T)
catcovdenom$total.n[catcovdenom$vi.level=="severe.vi"]<-sum(raab$z_case_660,na.rm=T)
catcovdenom$total.n[catcovdenom$vi.level=="moderate.vi"]<-sum(raab$z_case_618,na.rm=T)
catcovdenom$total.n[catcovdenom$vi.level=="mild.vi"]<-sum(raab$z_case_612,na.rm=T)


catcovdenom$female.pct[catcovdenom$vi.level=="blind"]<-sum(raab$z_case_360[raab$gender=="female"],na.rm=T)/sum(raab$vi.denom[raab$gender=="female"],na.rm=T)
catcovdenom$female.pct[catcovdenom$vi.level=="severe.vi"]<-sum(raab$z_case_660[raab$gender=="female"],na.rm=T)/sum(raab$vi.denom[raab$gender=="female"],na.rm=T)
catcovdenom$female.pct[catcovdenom$vi.level=="moderate.vi"]<-sum(raab$z_case_618[raab$gender=="female"],na.rm=T)/sum(raab$vi.denom[raab$gender=="female"],na.rm=T)
catcovdenom$female.pct[catcovdenom$vi.level=="mild.vi"]<-sum(raab$z_case_612[raab$gender=="female"],na.rm=T)/sum(raab$vi.denom[raab$gender=="female"],na.rm=T)

catcovdenom$male.pct[catcovdenom$vi.level=="blind"]<-sum(raab$z_case_360[raab$gender=="male"],na.rm=T)/sum(raab$vi.denom[raab$gender=="male"],na.rm=T)
catcovdenom$male.pct[catcovdenom$vi.level=="severe.vi"]<-sum(raab$z_case_660[raab$gender=="male"],na.rm=T)/sum(raab$vi.denom[raab$gender=="male"],na.rm=T)
catcovdenom$male.pct[catcovdenom$vi.level=="moderate.vi"]<-sum(raab$z_case_618[raab$gender=="male"],na.rm=T)/sum(raab$vi.denom[raab$gender=="male"],na.rm=T)
catcovdenom$male.pct[catcovdenom$vi.level=="mild.vi"]<-sum(raab$z_case_612[raab$gender=="male"],na.rm=T)/sum(raab$vi.denom[raab$gender=="male"],na.rm=T)

catcovdenom$total.pct[catcovdenom$vi.level=="blind"]<-sum(raab$z_case_360,na.rm=T)/sum(raab$vi.denom,na.rm=T)
catcovdenom$total.pct[catcovdenom$vi.level=="severe.vi"]<-sum(raab$z_case_660,na.rm=T)/sum(raab$vi.denom,na.rm=T)
catcovdenom$total.pct[catcovdenom$vi.level=="moderate.vi"]<-sum(raab$z_case_618,na.rm=T)/sum(raab$vi.denom,na.rm=T)
catcovdenom$total.pct[catcovdenom$vi.level=="mild.vi"]<-sum(raab$z_case_612,na.rm=T)/sum(raab$vi.denom,na.rm=T)

catcovdenom$female.pct.lci[catcovdenom$vi.level=="blind"]<-bennett.lci(catcovdenom$female.pct[catcovdenom$vi.level=="blind"], raab$z_case_360[raab$gender=="female"], raab$vi.denom[raab$gender=="female"], raab$clusterNumber[raab$gender=="female"])
catcovdenom$female.pct.lci[catcovdenom$vi.level=="severe.vi"]<-bennett.lci(catcovdenom$female.pct[catcovdenom$vi.level=="severe.vi"], raab$z_case_660[raab$gender=="female"], raab$vi.denom[raab$gender=="female"],raab$clusterNumber[raab$gender=="female"])
catcovdenom$female.pct.lci[catcovdenom$vi.level=="moderate.vi"]<-bennett.lci(catcovdenom$female.pct[catcovdenom$vi.level=="moderate.vi"], raab$z_case_618[raab$gender=="female"], raab$vi.denom[raab$gender=="female"],raab$clusterNumber[raab$gender=="female"])
catcovdenom$female.pct.lci[catcovdenom$vi.level=="mild.vi"]<-bennett.lci(catcovdenom$female.pct[catcovdenom$vi.level=="mild.vi"], raab$z_case_612[raab$gender=="female"], raab$vi.denom[raab$gender=="female"],raab$clusterNumber[raab$gender=="female"])

catcovdenom$female.pct.uci[catcovdenom$vi.level=="blind"]<-bennett.uci(catcovdenom$female.pct[catcovdenom$vi.level=="blind"], raab$z_case_360[raab$gender=="female"], raab$vi.denom[raab$gender=="female"],raab$clusterNumber[raab$gender=="female"])
catcovdenom$female.pct.uci[catcovdenom$vi.level=="severe.vi"]<-bennett.uci(catcovdenom$female.pct[catcovdenom$vi.level=="severe.vi"], raab$z_case_660[raab$gender=="female"], raab$vi.denom[raab$gender=="female"],raab$clusterNumber[raab$gender=="female"])
catcovdenom$female.pct.uci[catcovdenom$vi.level=="moderate.vi"]<-bennett.uci(catcovdenom$female.pct[catcovdenom$vi.level=="moderate.vi"], raab$z_case_618[raab$gender=="female"], raab$vi.denom[raab$gender=="female"],raab$clusterNumber[raab$gender=="female"])
catcovdenom$female.pct.uci[catcovdenom$vi.level=="mild.vi"]<-bennett.uci(catcovdenom$female.pct[catcovdenom$vi.level=="mild.vi"], raab$z_case_612[raab$gender=="female"], raab$vi.denom[raab$gender=="female"],raab$clusterNumber[raab$gender=="female"])

catcovdenom$male.pct.lci[catcovdenom$vi.level=="blind"]<-bennett.lci(catcovdenom$male.pct[catcovdenom$vi.level=="blind"], raab$z_case_360[raab$gender=="male"], raab$vi.denom[raab$gender=="male"],raab$clusterNumber[raab$gender=="male"])
catcovdenom$male.pct.lci[catcovdenom$vi.level=="severe.vi"]<-bennett.lci(catcovdenom$male.pct[catcovdenom$vi.level=="severe.vi"], raab$z_case_660[raab$gender=="male"], raab$vi.denom[raab$gender=="male"],raab$clusterNumber[raab$gender=="male"])
catcovdenom$male.pct.lci[catcovdenom$vi.level=="moderate.vi"]<-bennett.lci(catcovdenom$male.pct[catcovdenom$vi.level=="moderate.vi"], raab$z_case_618[raab$gender=="male"], raab$vi.denom[raab$gender=="male"],raab$clusterNumber[raab$gender=="male"])
catcovdenom$male.pct.lci[catcovdenom$vi.level=="mild.vi"]<-bennett.lci(catcovdenom$male.pct[catcovdenom$vi.level=="mild.vi"], raab$z_case_612[raab$gender=="male"], raab$vi.denom[raab$gender=="male"],raab$clusterNumber[raab$gender=="male"])

catcovdenom$male.pct.uci[catcovdenom$vi.level=="blind"]<-bennett.uci(catcovdenom$male.pct[catcovdenom$vi.level=="blind"], raab$z_case_360[raab$gender=="male"], raab$vi.denom[raab$gender=="male"],raab$clusterNumber[raab$gender=="male"])
catcovdenom$male.pct.uci[catcovdenom$vi.level=="severe.vi"]<-bennett.uci(catcovdenom$male.pct[catcovdenom$vi.level=="severe.vi"], raab$z_case_660[raab$gender=="male"], raab$vi.denom[raab$gender=="male"],raab$clusterNumber[raab$gender=="male"])
catcovdenom$male.pct.uci[catcovdenom$vi.level=="moderate.vi"]<-bennett.uci(catcovdenom$male.pct[catcovdenom$vi.level=="moderate.vi"], raab$z_case_618[raab$gender=="male"], raab$vi.denom[raab$gender=="male"],raab$clusterNumber[raab$gender=="male"])
catcovdenom$male.pct.uci[catcovdenom$vi.level=="mild.vi"]<-bennett.uci(catcovdenom$male.pct[catcovdenom$vi.level=="mild.vi"], raab$z_case_612[raab$gender=="male"], raab$vi.denom[raab$gender=="male"],raab$clusterNumber[raab$gender=="male"])

catcovdenom$total.pct.lci[catcovdenom$vi.level=="blind"]<-bennett.lci(catcovdenom$total.pct[catcovdenom$vi.level=="blind"], raab$z_case_360, raab$vi.denom,raab$clusterNumber)
catcovdenom$total.pct.lci[catcovdenom$vi.level=="severe.vi"]<-bennett.lci(catcovdenom$total.pct[catcovdenom$vi.level=="severe.vi"], raab$z_case_660, raab$vi.denom,raab$clusterNumber)
catcovdenom$total.pct.lci[catcovdenom$vi.level=="moderate.vi"]<-bennett.lci(catcovdenom$total.pct[catcovdenom$vi.level=="moderate.vi"], raab$z_case_618, raab$vi.denom,raab$clusterNumber)
catcovdenom$total.pct.lci[catcovdenom$vi.level=="mild.vi"]<-bennett.lci(catcovdenom$total.pct[catcovdenom$vi.level=="mild.vi"], raab$z_case_612, raab$vi.denom,raab$clusterNumber)

catcovdenom$total.pct.uci[catcovdenom$vi.level=="blind"]<-bennett.uci(catcovdenom$total.pct[catcovdenom$vi.level=="blind"], raab$z_case_360, raab$vi.denom,raab$clusterNumber)
catcovdenom$total.pct.uci[catcovdenom$vi.level=="severe.vi"]<-bennett.uci(catcovdenom$total.pct[catcovdenom$vi.level=="severe.vi"], raab$z_case_660, raab$vi.denom,raab$clusterNumber)
catcovdenom$total.pct.uci[catcovdenom$vi.level=="moderate.vi"]<-bennett.uci(catcovdenom$total.pct[catcovdenom$vi.level=="moderate.vi"], raab$z_case_618, raab$vi.denom,raab$clusterNumber)
catcovdenom$total.pct.uci[catcovdenom$vi.level=="mild.vi"]<-bennett.uci(catcovdenom$total.pct[catcovdenom$vi.level=="mild.vi"], raab$z_case_612, raab$vi.denom,raab$clusterNumber)

catcovdenom$female.adj.pct[catcovdenom$vi.level=="blind"]<-prop.age.adjust(female.subpop,raab[raab$gender=="female",],raab$z_case_360[raab$gender=="female"],raab$vi.denom[raab$gender=="female"])
catcovdenom$female.adj.pct[catcovdenom$vi.level=="severe.vi"]<-prop.age.adjust(female.subpop,raab[raab$gender=="female",],raab$z_case_660[raab$gender=="female"],raab$vi.denom[raab$gender=="female"])
catcovdenom$female.adj.pct[catcovdenom$vi.level=="moderate.vi"]<-prop.age.adjust(female.subpop,raab[raab$gender=="female",],raab$z_case_618[raab$gender=="female"],raab$vi.denom[raab$gender=="female"])
catcovdenom$female.adj.pct[catcovdenom$vi.level=="mild.vi"]<-prop.age.adjust(female.subpop,raab[raab$gender=="female",],raab$z_case_612[raab$gender=="female"],raab$vi.denom[raab$gender=="female"])

catcovdenom$male.adj.pct[catcovdenom$vi.level=="blind"]<-prop.age.adjust(male.subpop,raab[raab$gender=="male",],raab$z_case_360[raab$gender=="male"],raab$vi.denom[raab$gender=="male"])
catcovdenom$male.adj.pct[catcovdenom$vi.level=="severe.vi"]<-prop.age.adjust(male.subpop,raab[raab$gender=="male",],raab$z_case_660[raab$gender=="male"],raab$vi.denom[raab$gender=="male"])
catcovdenom$male.adj.pct[catcovdenom$vi.level=="moderate.vi"]<-prop.age.adjust(male.subpop,raab[raab$gender=="male",],raab$z_case_618[raab$gender=="male"],raab$vi.denom[raab$gender=="male"])
catcovdenom$male.adj.pct[catcovdenom$vi.level=="mild.vi"]<-prop.age.adjust(male.subpop,raab[raab$gender=="male",],raab$z_case_612[raab$gender=="male"],raab$vi.denom[raab$gender=="male"])

catcovdenom$total.adj.pct[catcovdenom$vi.level=="blind"]<-prop.age.sex.adjust(popfives,raab,raab$z_case_360,raab$vi.denom)
catcovdenom$total.adj.pct[catcovdenom$vi.level=="severe.vi"]<-prop.age.sex.adjust(popfives,raab,raab$z_case_660,raab$vi.denom)
catcovdenom$total.adj.pct[catcovdenom$vi.level=="moderate.vi"]<-prop.age.sex.adjust(popfives,raab,raab$z_case_618,raab$vi.denom)
catcovdenom$total.adj.pct[catcovdenom$vi.level=="mild.vi"]<-prop.age.sex.adjust(popfives,raab,raab$z_case_612,raab$vi.denom)


catcovdenom$female.adj.pct.lci[catcovdenom$vi.level=="blind"]<-bennett.lci(catcovdenom$female.adj.pct[catcovdenom$vi.level=="blind"], raab$z_case_360[raab$gender=="female"], raab$vi.denom[raab$gender=="female"], raab$clusterNumber[raab$gender=="female"])
catcovdenom$female.adj.pct.lci[catcovdenom$vi.level=="severe.vi"]<-bennett.lci(catcovdenom$female.adj.pct[catcovdenom$vi.level=="severe.vi"], raab$z_case_660[raab$gender=="female"], raab$vi.denom[raab$gender=="female"],raab$clusterNumber[raab$gender=="female"])
catcovdenom$female.adj.pct.lci[catcovdenom$vi.level=="moderate.vi"]<-bennett.lci(catcovdenom$female.adj.pct[catcovdenom$vi.level=="moderate.vi"], raab$z_case_618[raab$gender=="female"], raab$vi.denom[raab$gender=="female"],raab$clusterNumber[raab$gender=="female"])
catcovdenom$female.adj.pct.lci[catcovdenom$vi.level=="mild.vi"]<-bennett.lci(catcovdenom$female.adj.pct[catcovdenom$vi.level=="mild.vi"], raab$z_case_612[raab$gender=="female"], raab$vi.denom[raab$gender=="female"],raab$clusterNumber[raab$gender=="female"])
catcovdenom$female.adj.pct.uci[catcovdenom$vi.level=="blind"]<-bennett.uci(catcovdenom$female.adj.pct[catcovdenom$vi.level=="blind"], raab$z_case_360[raab$gender=="female"], raab$vi.denom[raab$gender=="female"],raab$clusterNumber[raab$gender=="female"])
catcovdenom$female.adj.pct.uci[catcovdenom$vi.level=="severe.vi"]<-bennett.uci(catcovdenom$female.adj.pct[catcovdenom$vi.level=="severe.vi"], raab$z_case_660[raab$gender=="female"], raab$vi.denom[raab$gender=="female"],raab$clusterNumber[raab$gender=="female"])
catcovdenom$female.adj.pct.uci[catcovdenom$vi.level=="moderate.vi"]<-bennett.uci(catcovdenom$female.adj.pct[catcovdenom$vi.level=="moderate.vi"], raab$z_case_618[raab$gender=="female"], raab$vi.denom[raab$gender=="female"],raab$clusterNumber[raab$gender=="female"])
catcovdenom$female.adj.pct.uci[catcovdenom$vi.level=="mild.vi"]<-bennett.uci(catcovdenom$female.adj.pct[catcovdenom$vi.level=="mild.vi"], raab$z_case_612[raab$gender=="female"], raab$vi.denom[raab$gender=="female"],raab$clusterNumber[raab$gender=="female"])

catcovdenom$male.adj.pct.lci[catcovdenom$vi.level=="blind"]<-bennett.lci(catcovdenom$male.adj.pct[catcovdenom$vi.level=="blind"], raab$z_case_360[raab$gender=="male"], raab$vi.denom[raab$gender=="male"],raab$clusterNumber[raab$gender=="male"])
catcovdenom$male.adj.pct.lci[catcovdenom$vi.level=="severe.vi"]<-bennett.lci(catcovdenom$male.adj.pct[catcovdenom$vi.level=="severe.vi"], raab$z_case_660[raab$gender=="male"], raab$vi.denom[raab$gender=="male"],raab$clusterNumber[raab$gender=="male"])
catcovdenom$male.adj.pct.lci[catcovdenom$vi.level=="moderate.vi"]<-bennett.lci(catcovdenom$male.adj.pct[catcovdenom$vi.level=="moderate.vi"], raab$z_case_618[raab$gender=="male"], raab$vi.denom[raab$gender=="male"],raab$clusterNumber[raab$gender=="male"])
catcovdenom$male.adj.pct.lci[catcovdenom$vi.level=="mild.vi"]<-bennett.lci(catcovdenom$male.adj.pct[catcovdenom$vi.level=="mild.vi"], raab$z_case_612[raab$gender=="male"], raab$vi.denom[raab$gender=="male"],raab$clusterNumber[raab$gender=="male"])
catcovdenom$male.adj.pct.uci[catcovdenom$vi.level=="blind"]<-bennett.uci(catcovdenom$male.adj.pct[catcovdenom$vi.level=="blind"], raab$z_case_360[raab$gender=="male"], raab$vi.denom[raab$gender=="male"],raab$clusterNumber[raab$gender=="male"])
catcovdenom$male.adj.pct.uci[catcovdenom$vi.level=="severe.vi"]<-bennett.uci(catcovdenom$male.adj.pct[catcovdenom$vi.level=="severe.vi"], raab$z_case_660[raab$gender=="male"], raab$vi.denom[raab$gender=="male"],raab$clusterNumber[raab$gender=="male"])
catcovdenom$male.adj.pct.uci[catcovdenom$vi.level=="moderate.vi"]<-bennett.uci(catcovdenom$male.adj.pct[catcovdenom$vi.level=="moderate.vi"], raab$z_case_618[raab$gender=="male"], raab$vi.denom[raab$gender=="male"],raab$clusterNumber[raab$gender=="male"])
catcovdenom$male.adj.pct.uci[catcovdenom$vi.level=="mild.vi"]<-bennett.uci(catcovdenom$male.adj.pct[catcovdenom$vi.level=="mild.vi"], raab$z_case_612[raab$gender=="male"], raab$vi.denom[raab$gender=="male"],raab$clusterNumber[raab$gender=="male"])

catcovdenom$total.adj.pct.lci[catcovdenom$vi.level=="blind"]<-bennett.lci(catcovdenom$total.adj.pct[catcovdenom$vi.level=="blind"], raab$z_case_360, raab$vi.denom,raab$clusterNumber)
catcovdenom$total.adj.pct.lci[catcovdenom$vi.level=="severe.vi"]<-bennett.lci(catcovdenom$total.adj.pct[catcovdenom$vi.level=="severe.vi"], raab$z_case_660, raab$vi.denom,raab$clusterNumber)
catcovdenom$total.adj.pct.lci[catcovdenom$vi.level=="moderate.vi"]<-bennett.lci(catcovdenom$total.adj.pct[catcovdenom$vi.level=="moderate.vi"], raab$z_case_618, raab$vi.denom,raab$clusterNumber)
catcovdenom$total.adj.pct.lci[catcovdenom$vi.level=="mild.vi"]<-bennett.lci(catcovdenom$total.adj.pct[catcovdenom$vi.level=="mild.vi"], raab$z_case_612, raab$vi.denom,raab$clusterNumber)
catcovdenom$total.adj.pct.uci[catcovdenom$vi.level=="blind"]<-bennett.uci(catcovdenom$total.adj.pct[catcovdenom$vi.level=="blind"], raab$z_case_360, raab$vi.denom,raab$clusterNumber)
catcovdenom$total.adj.pct.uci[catcovdenom$vi.level=="severe.vi"]<-bennett.uci(catcovdenom$total.adj.pct[catcovdenom$vi.level=="severe.vi"], raab$z_case_660, raab$vi.denom,raab$clusterNumber)
catcovdenom$total.adj.pct.uci[catcovdenom$vi.level=="moderate.vi"]<-bennett.uci(catcovdenom$total.adj.pct[catcovdenom$vi.level=="moderate.vi"], raab$z_case_618, raab$vi.denom,raab$clusterNumber)
catcovdenom$total.adj.pct.uci[catcovdenom$vi.level=="mild.vi"]<-bennett.uci(catcovdenom$total.adj.pct[catcovdenom$vi.level=="mild.vi"], raab$z_case_612, raab$vi.denom,raab$clusterNumber)

catcovdenom$extrapolated.female.n<-format( catcovdenom$female.adj.pct * sum(female.subpop$population), digits = 1, scientific=F)
catcovdenom$extrapolated.male.n<-format( catcovdenom$male.adj.pct * sum(male.subpop$population), digits = 1, scientific=F)
catcovdenom$extrapolated.total.n<-format( catcovdenom$total.adj.pct * sum(popfives$population), digits = 1, scientific=F)

catcovdenom$vi.level <- recode_factor(catcovdenom$vi.level,"blind" = "PinVA <3/60","severe.vi" = "PinVA <6/60","moderate.vi" = "PinVA <6/18","mild.vi" = "PinVA <6/12")

lcis<-grep("lci",names(catcovdenom))
ucis<-grep("uci",names(catcovdenom))
catcovdenom[,lcis][catcovdenom[,lcis]<0]<-0
catcovdenom[,ucis][catcovdenom[,ucis]>1]<-1

pcts<-grep("pct",names(catcovdenom))
catcovdenom[,pcts] <- round( catcovdenom[,pcts] * 100, 1)
catcovdenom[,pcts] <- format( catcovdenom[,pcts], nsmall=1 )

catcovdenom$out.names<-paste0("bilateral_cataract_",catcovdenom$vi.level)
