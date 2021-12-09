#RAAB5

#v1 08/09/21 - RB

#bilateral cataract cases

asa7bil<-as.data.frame(vi.levels)
asa7bil[,c(2:25)]<-NA
names(asa7bil)<-c("vi.level",
               
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


asa7bil$female.n[asa7bil$vi.level=="blind"]<-sum(raab$z_case_360[raab$gender=="female"],na.rm=T)
asa7bil$female.n[asa7bil$vi.level=="severe.vi"]<-sum(raab$z_case_660[raab$gender=="female"],na.rm=T)
asa7bil$female.n[asa7bil$vi.level=="moderate.vi"]<-sum(raab$z_case_618[raab$gender=="female"],na.rm=T)

asa7bil$male.n[asa7bil$vi.level=="blind"]<-sum(raab$z_case_360[raab$gender=="male"],na.rm=T)
asa7bil$male.n[asa7bil$vi.level=="severe.vi"]<-sum(raab$z_case_660[raab$gender=="male"],na.rm=T)
asa7bil$male.n[asa7bil$vi.level=="moderate.vi"]<-sum(raab$z_case_618[raab$gender=="male"],na.rm=T)

asa7bil$total.n[asa7bil$vi.level=="blind"]<-sum(raab$z_case_360,na.rm=T)
asa7bil$total.n[asa7bil$vi.level=="severe.vi"]<-sum(raab$z_case_660,na.rm=T)
asa7bil$total.n[asa7bil$vi.level=="moderate.vi"]<-sum(raab$z_case_618,na.rm=T)

asa7bil$female.pct[asa7bil$vi.level=="blind"]<-sum(raab$z_case_360[raab$gender=="female"],na.rm=T)/sum(raab$vi.denom[raab$gender=="female"],na.rm=T)
asa7bil$female.pct[asa7bil$vi.level=="severe.vi"]<-sum(raab$z_case_660[raab$gender=="female"],na.rm=T)/sum(raab$vi.denom[raab$gender=="female"],na.rm=T)
asa7bil$female.pct[asa7bil$vi.level=="moderate.vi"]<-sum(raab$z_case_618[raab$gender=="female"],na.rm=T)/sum(raab$vi.denom[raab$gender=="female"],na.rm=T)

asa7bil$male.pct[asa7bil$vi.level=="blind"]<-sum(raab$z_case_360[raab$gender=="male"],na.rm=T)/sum(raab$vi.denom[raab$gender=="male"],na.rm=T)
asa7bil$male.pct[asa7bil$vi.level=="severe.vi"]<-sum(raab$z_case_660[raab$gender=="male"],na.rm=T)/sum(raab$vi.denom[raab$gender=="male"],na.rm=T)
asa7bil$male.pct[asa7bil$vi.level=="moderate.vi"]<-sum(raab$z_case_618[raab$gender=="male"],na.rm=T)/sum(raab$vi.denom[raab$gender=="male"],na.rm=T)

asa7bil$total.pct[asa7bil$vi.level=="blind"]<-sum(raab$z_case_360,na.rm=T)/sum(raab$vi.denom,na.rm=T)
asa7bil$total.pct[asa7bil$vi.level=="severe.vi"]<-sum(raab$z_case_660,na.rm=T)/sum(raab$vi.denom,na.rm=T)
asa7bil$total.pct[asa7bil$vi.level=="moderate.vi"]<-sum(raab$z_case_618,na.rm=T)/sum(raab$vi.denom,na.rm=T)

asa7bil$female.pct.lci[asa7bil$vi.level=="blind"]<-bennett.lci(asa7bil$female.pct[asa7bil$vi.level=="blind"], raab$z_case_360[raab$gender=="female"], raab$vi.denom[raab$gender=="female"], raab$clusterNumber[raab$gender=="female"])
asa7bil$female.pct.lci[asa7bil$vi.level=="severe.vi"]<-bennett.lci(asa7bil$female.pct[asa7bil$vi.level=="severe.vi"], raab$z_case_660[raab$gender=="female"], raab$vi.denom[raab$gender=="female"],raab$clusterNumber[raab$gender=="female"])
asa7bil$female.pct.lci[asa7bil$vi.level=="moderate.vi"]<-bennett.lci(asa7bil$female.pct[asa7bil$vi.level=="moderate.vi"], raab$z_case_618[raab$gender=="female"], raab$vi.denom[raab$gender=="female"],raab$clusterNumber[raab$gender=="female"])

asa7bil$female.pct.uci[asa7bil$vi.level=="blind"]<-bennett.uci(asa7bil$female.pct[asa7bil$vi.level=="blind"], raab$z_case_360[raab$gender=="female"], raab$vi.denom[raab$gender=="female"],raab$clusterNumber[raab$gender=="female"])
asa7bil$female.pct.uci[asa7bil$vi.level=="severe.vi"]<-bennett.uci(asa7bil$female.pct[asa7bil$vi.level=="severe.vi"], raab$z_case_660[raab$gender=="female"], raab$vi.denom[raab$gender=="female"],raab$clusterNumber[raab$gender=="female"])
asa7bil$female.pct.uci[asa7bil$vi.level=="moderate.vi"]<-bennett.uci(asa7bil$female.pct[asa7bil$vi.level=="moderate.vi"], raab$z_case_618[raab$gender=="female"], raab$vi.denom[raab$gender=="female"],raab$clusterNumber[raab$gender=="female"])

asa7bil$male.pct.lci[asa7bil$vi.level=="blind"]<-bennett.lci(asa7bil$male.pct[asa7bil$vi.level=="blind"], raab$z_case_360[raab$gender=="male"], raab$vi.denom[raab$gender=="male"],raab$clusterNumber[raab$gender=="male"])
asa7bil$male.pct.lci[asa7bil$vi.level=="severe.vi"]<-bennett.lci(asa7bil$male.pct[asa7bil$vi.level=="severe.vi"], raab$z_case_660[raab$gender=="male"], raab$vi.denom[raab$gender=="male"],raab$clusterNumber[raab$gender=="male"])
asa7bil$male.pct.lci[asa7bil$vi.level=="moderate.vi"]<-bennett.lci(asa7bil$male.pct[asa7bil$vi.level=="moderate.vi"], raab$z_case_618[raab$gender=="male"], raab$vi.denom[raab$gender=="male"],raab$clusterNumber[raab$gender=="male"])

asa7bil$male.pct.uci[asa7bil$vi.level=="blind"]<-bennett.uci(asa7bil$male.pct[asa7bil$vi.level=="blind"], raab$z_case_360[raab$gender=="male"], raab$vi.denom[raab$gender=="male"],raab$clusterNumber[raab$gender=="male"])
asa7bil$male.pct.uci[asa7bil$vi.level=="severe.vi"]<-bennett.uci(asa7bil$male.pct[asa7bil$vi.level=="severe.vi"], raab$z_case_660[raab$gender=="male"], raab$vi.denom[raab$gender=="male"],raab$clusterNumber[raab$gender=="male"])
asa7bil$male.pct.uci[asa7bil$vi.level=="moderate.vi"]<-bennett.uci(asa7bil$male.pct[asa7bil$vi.level=="moderate.vi"], raab$z_case_618[raab$gender=="male"], raab$vi.denom[raab$gender=="male"],raab$clusterNumber[raab$gender=="male"])

asa7bil$total.pct.lci[asa7bil$vi.level=="blind"]<-bennett.lci(asa7bil$total.pct[asa7bil$vi.level=="blind"], raab$z_case_360, raab$vi.denom,raab$clusterNumber)
asa7bil$total.pct.lci[asa7bil$vi.level=="severe.vi"]<-bennett.lci(asa7bil$total.pct[asa7bil$vi.level=="severe.vi"], raab$z_case_660, raab$vi.denom,raab$clusterNumber)
asa7bil$total.pct.lci[asa7bil$vi.level=="moderate.vi"]<-bennett.lci(asa7bil$total.pct[asa7bil$vi.level=="moderate.vi"], raab$z_case_618, raab$vi.denom,raab$clusterNumber)

asa7bil$total.pct.uci[asa7bil$vi.level=="blind"]<-bennett.uci(asa7bil$total.pct[asa7bil$vi.level=="blind"], raab$z_case_360, raab$vi.denom,raab$clusterNumber)
asa7bil$total.pct.uci[asa7bil$vi.level=="severe.vi"]<-bennett.uci(asa7bil$total.pct[asa7bil$vi.level=="severe.vi"], raab$z_case_660, raab$vi.denom,raab$clusterNumber)
asa7bil$total.pct.uci[asa7bil$vi.level=="moderate.vi"]<-bennett.uci(asa7bil$total.pct[asa7bil$vi.level=="moderate.vi"], raab$z_case_618, raab$vi.denom,raab$clusterNumber)

asa7bil$female.adj.pct[asa7bil$vi.level=="blind"]<-prop.age.adjust(female.subpop,raab[raab$gender=="female",],raab$z_case_360[raab$gender=="female"],raab$vi.denom[raab$gender=="female"])
asa7bil$female.adj.pct[asa7bil$vi.level=="severe.vi"]<-prop.age.adjust(female.subpop,raab[raab$gender=="female",],raab$z_case_660[raab$gender=="female"],raab$vi.denom[raab$gender=="female"])
asa7bil$female.adj.pct[asa7bil$vi.level=="moderate.vi"]<-prop.age.adjust(female.subpop,raab[raab$gender=="female",],raab$z_case_618[raab$gender=="female"],raab$vi.denom[raab$gender=="female"])

asa7bil$male.adj.pct[asa7bil$vi.level=="blind"]<-prop.age.adjust(male.subpop,raab[raab$gender=="male",],raab$z_case_360[raab$gender=="male"],raab$vi.denom[raab$gender=="male"])
asa7bil$male.adj.pct[asa7bil$vi.level=="severe.vi"]<-prop.age.adjust(male.subpop,raab[raab$gender=="male",],raab$z_case_660[raab$gender=="male"],raab$vi.denom[raab$gender=="male"])
asa7bil$male.adj.pct[asa7bil$vi.level=="moderate.vi"]<-prop.age.adjust(male.subpop,raab[raab$gender=="male",],raab$z_case_618[raab$gender=="male"],raab$vi.denom[raab$gender=="male"])

asa7bil$total.adj.pct[asa7bil$vi.level=="blind"]<-prop.age.sex.adjust(popfives,raab,raab$z_case_360,raab$vi.denom)
asa7bil$total.adj.pct[asa7bil$vi.level=="severe.vi"]<-prop.age.sex.adjust(popfives,raab,raab$z_case_660,raab$vi.denom)
asa7bil$total.adj.pct[asa7bil$vi.level=="moderate.vi"]<-prop.age.sex.adjust(popfives,raab,raab$z_case_618,raab$vi.denom)

asa7bil$female.adj.pct.lci[asa7bil$vi.level=="blind"]<-bennett.lci(asa7bil$female.adj.pct[asa7bil$vi.level=="blind"], raab$z_case_360[raab$gender=="female"], raab$vi.denom[raab$gender=="female"], raab$clusterNumber[raab$gender=="female"])
asa7bil$female.adj.pct.lci[asa7bil$vi.level=="severe.vi"]<-bennett.lci(asa7bil$female.adj.pct[asa7bil$vi.level=="severe.vi"], raab$z_case_660[raab$gender=="female"], raab$vi.denom[raab$gender=="female"],raab$clusterNumber[raab$gender=="female"])
asa7bil$female.adj.pct.lci[asa7bil$vi.level=="moderate.vi"]<-bennett.lci(asa7bil$female.adj.pct[asa7bil$vi.level=="moderate.vi"], raab$z_case_618[raab$gender=="female"], raab$vi.denom[raab$gender=="female"],raab$clusterNumber[raab$gender=="female"])

asa7bil$female.adj.pct.uci[asa7bil$vi.level=="blind"]<-bennett.uci(asa7bil$female.adj.pct[asa7bil$vi.level=="blind"], raab$z_case_360[raab$gender=="female"], raab$vi.denom[raab$gender=="female"],raab$clusterNumber[raab$gender=="female"])
asa7bil$female.adj.pct.uci[asa7bil$vi.level=="severe.vi"]<-bennett.uci(asa7bil$female.adj.pct[asa7bil$vi.level=="severe.vi"], raab$z_case_660[raab$gender=="female"], raab$vi.denom[raab$gender=="female"],raab$clusterNumber[raab$gender=="female"])
asa7bil$female.adj.pct.uci[asa7bil$vi.level=="moderate.vi"]<-bennett.uci(asa7bil$female.adj.pct[asa7bil$vi.level=="moderate.vi"], raab$z_case_618[raab$gender=="female"], raab$vi.denom[raab$gender=="female"],raab$clusterNumber[raab$gender=="female"])

asa7bil$male.adj.pct.lci[asa7bil$vi.level=="blind"]<-bennett.lci(asa7bil$male.adj.pct[asa7bil$vi.level=="blind"], raab$z_case_360[raab$gender=="male"], raab$vi.denom[raab$gender=="male"],raab$clusterNumber[raab$gender=="male"])
asa7bil$male.adj.pct.lci[asa7bil$vi.level=="severe.vi"]<-bennett.lci(asa7bil$male.adj.pct[asa7bil$vi.level=="severe.vi"], raab$z_case_660[raab$gender=="male"], raab$vi.denom[raab$gender=="male"],raab$clusterNumber[raab$gender=="male"])
asa7bil$male.adj.pct.lci[asa7bil$vi.level=="moderate.vi"]<-bennett.lci(asa7bil$male.adj.pct[asa7bil$vi.level=="moderate.vi"], raab$z_case_618[raab$gender=="male"], raab$vi.denom[raab$gender=="male"],raab$clusterNumber[raab$gender=="male"])

asa7bil$male.adj.pct.uci[asa7bil$vi.level=="blind"]<-bennett.uci(asa7bil$male.adj.pct[asa7bil$vi.level=="blind"], raab$z_case_360[raab$gender=="male"], raab$vi.denom[raab$gender=="male"],raab$clusterNumber[raab$gender=="male"])
asa7bil$male.adj.pct.uci[asa7bil$vi.level=="severe.vi"]<-bennett.uci(asa7bil$male.adj.pct[asa7bil$vi.level=="severe.vi"], raab$z_case_660[raab$gender=="male"], raab$vi.denom[raab$gender=="male"],raab$clusterNumber[raab$gender=="male"])
asa7bil$male.adj.pct.uci[asa7bil$vi.level=="moderate.vi"]<-bennett.uci(asa7bil$male.adj.pct[asa7bil$vi.level=="moderate.vi"], raab$z_case_618[raab$gender=="male"], raab$vi.denom[raab$gender=="male"],raab$clusterNumber[raab$gender=="male"])

asa7bil$total.adj.pct.lci[asa7bil$vi.level=="blind"]<-bennett.lci(asa7bil$total.adj.pct[asa7bil$vi.level=="blind"], raab$z_case_360, raab$vi.denom,raab$clusterNumber)
asa7bil$total.adj.pct.lci[asa7bil$vi.level=="severe.vi"]<-bennett.lci(asa7bil$total.adj.pct[asa7bil$vi.level=="severe.vi"], raab$z_case_660, raab$vi.denom,raab$clusterNumber)
asa7bil$total.adj.pct.lci[asa7bil$vi.level=="moderate.vi"]<-bennett.lci(asa7bil$total.adj.pct[asa7bil$vi.level=="moderate.vi"], raab$z_case_618, raab$vi.denom,raab$clusterNumber)

asa7bil$total.adj.pct.uci[asa7bil$vi.level=="blind"]<-bennett.uci(asa7bil$total.adj.pct[asa7bil$vi.level=="blind"], raab$z_case_360, raab$vi.denom,raab$clusterNumber)
asa7bil$total.adj.pct.uci[asa7bil$vi.level=="severe.vi"]<-bennett.uci(asa7bil$total.adj.pct[asa7bil$vi.level=="severe.vi"], raab$z_case_660, raab$vi.denom,raab$clusterNumber)
asa7bil$total.adj.pct.uci[asa7bil$vi.level=="moderate.vi"]<-bennett.uci(asa7bil$total.adj.pct[asa7bil$vi.level=="moderate.vi"], raab$z_case_618, raab$vi.denom,raab$clusterNumber)

asa7bil$extrapolated.female.n<-format( asa7bil$female.adj.pct * sum(female.subpop$population), digits = 0 ,big.mark = " ", big.interval= 3L, scientific=F)
asa7bil$extrapolated.male.n<-format( asa7bil$male.adj.pct * sum(male.subpop$population), digits = 0, big.mark = " ", big.interval= 3L, scientific=F)
asa7bil$extrapolated.total.n<-format( asa7bil$total.adj.pct * sum(popfives$population), digits = 0, big.mark = " ", big.interval= 3L, scientific=F)

asa7bil$vi.level <- recode_factor(asa7bil$vi.level,"blind" = "PinVA <3/60","severe.vi" = "PinVA <6/60","moderate.vi" = "PinVA <6/18","mild.vi" = "PinVA <6/12")

#unilateral cataract cases

raab$denom.618.unicat<-((raab$right_operable_618==1 | raab$left_operable_618==1) & !(raab$z_case_618))+0
raab$denom.660.unicat<-((raab$right_operable_660==1 | raab$left_operable_660==1) & !(raab$z_case_660))+0
raab$denom.360.unicat<-((raab$right_operable_360==1 | raab$left_operable_360==1) & !(raab$z_case_360))+0

asa7uni<-as.data.frame(vi.levels)
asa7uni[,c(2:25)]<-NA
names(asa7uni)<-c("vi.level",
                
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


asa7uni$female.n[asa7uni$vi.level=="blind"]<-sum(raab$denom.360.unicat[raab$gender=="female"],na.rm=T)
asa7uni$female.n[asa7uni$vi.level=="severe.vi"]<-sum(raab$denom.660.unicat[raab$gender=="female"],na.rm=T)
asa7uni$female.n[asa7uni$vi.level=="moderate.vi"]<-sum(raab$denom.618.unicat[raab$gender=="female"],na.rm=T)

asa7uni$male.n[asa7uni$vi.level=="blind"]<-sum(raab$denom.360.unicat[raab$gender=="male"],na.rm=T)
asa7uni$male.n[asa7uni$vi.level=="severe.vi"]<-sum(raab$denom.660.unicat[raab$gender=="male"],na.rm=T)
asa7uni$male.n[asa7uni$vi.level=="moderate.vi"]<-sum(raab$denom.618.unicat[raab$gender=="male"],na.rm=T)

asa7uni$total.n[asa7uni$vi.level=="blind"]<-sum(raab$denom.360.unicat,na.rm=T)
asa7uni$total.n[asa7uni$vi.level=="severe.vi"]<-sum(raab$denom.660.unicat,na.rm=T)
asa7uni$total.n[asa7uni$vi.level=="moderate.vi"]<-sum(raab$denom.618.unicat,na.rm=T)

asa7uni$female.pct[asa7uni$vi.level=="blind"]<-sum(raab$denom.360.unicat[raab$gender=="female"],na.rm=T)/sum(raab$vi.denom[raab$gender=="female"],na.rm=T)
asa7uni$female.pct[asa7uni$vi.level=="severe.vi"]<-sum(raab$denom.660.unicat[raab$gender=="female"],na.rm=T)/sum(raab$vi.denom[raab$gender=="female"],na.rm=T)
asa7uni$female.pct[asa7uni$vi.level=="moderate.vi"]<-sum(raab$denom.618.unicat[raab$gender=="female"],na.rm=T)/sum(raab$vi.denom[raab$gender=="female"],na.rm=T)

asa7uni$male.pct[asa7uni$vi.level=="blind"]<-sum(raab$denom.360.unicat[raab$gender=="male"],na.rm=T)/sum(raab$vi.denom[raab$gender=="male"],na.rm=T)
asa7uni$male.pct[asa7uni$vi.level=="severe.vi"]<-sum(raab$denom.660.unicat[raab$gender=="male"],na.rm=T)/sum(raab$vi.denom[raab$gender=="male"],na.rm=T)
asa7uni$male.pct[asa7uni$vi.level=="moderate.vi"]<-sum(raab$denom.618.unicat[raab$gender=="male"],na.rm=T)/sum(raab$vi.denom[raab$gender=="male"],na.rm=T)

asa7uni$total.pct[asa7uni$vi.level=="blind"]<-sum(raab$denom.360.unicat,na.rm=T)/sum(raab$vi.denom,na.rm=T)
asa7uni$total.pct[asa7uni$vi.level=="severe.vi"]<-sum(raab$denom.660.unicat,na.rm=T)/sum(raab$vi.denom,na.rm=T)
asa7uni$total.pct[asa7uni$vi.level=="moderate.vi"]<-sum(raab$denom.618.unicat,na.rm=T)/sum(raab$vi.denom,na.rm=T)

asa7uni$female.pct.lci[asa7uni$vi.level=="blind"]<-bennett.lci(asa7uni$female.pct[asa7uni$vi.level=="blind"], raab$denom.360.unicat[raab$gender=="female"], raab$vi.denom[raab$gender=="female"],raab$clusterNumber[raab$gender=="female"])
asa7uni$female.pct.lci[asa7uni$vi.level=="severe.vi"]<-bennett.lci(asa7uni$female.pct[asa7uni$vi.level=="severe.vi"], raab$denom.660.unicat[raab$gender=="female"], raab$vi.denom[raab$gender=="female"],raab$clusterNumber[raab$gender=="female"])
asa7uni$female.pct.lci[asa7uni$vi.level=="moderate.vi"]<-bennett.lci(asa7uni$female.pct[asa7uni$vi.level=="moderate.vi"], raab$denom.618.unicat[raab$gender=="female"], raab$vi.denom[raab$gender=="female"],raab$clusterNumber[raab$gender=="female"])

asa7uni$female.pct.uci[asa7uni$vi.level=="blind"]<-bennett.uci(asa7uni$female.pct[asa7uni$vi.level=="blind"], raab$denom.360.unicat[raab$gender=="female"], raab$vi.denom[raab$gender=="female"],raab$clusterNumber[raab$gender=="female"])
asa7uni$female.pct.uci[asa7uni$vi.level=="severe.vi"]<-bennett.uci(asa7uni$female.pct[asa7uni$vi.level=="severe.vi"], raab$denom.660.unicat[raab$gender=="female"], raab$vi.denom[raab$gender=="female"],raab$clusterNumber[raab$gender=="female"])
asa7uni$female.pct.uci[asa7uni$vi.level=="moderate.vi"]<-bennett.uci(asa7uni$female.pct[asa7uni$vi.level=="moderate.vi"], raab$denom.618.unicat[raab$gender=="female"], raab$vi.denom[raab$gender=="female"],raab$clusterNumber[raab$gender=="female"])

asa7uni$male.pct.lci[asa7uni$vi.level=="blind"]<-bennett.lci(asa7uni$male.pct[asa7uni$vi.level=="blind"], raab$denom.360.unicat[raab$gender=="male"], raab$vi.denom[raab$gender=="male"],raab$clusterNumber[raab$gender=="male"])
asa7uni$male.pct.lci[asa7uni$vi.level=="severe.vi"]<-bennett.lci(asa7uni$male.pct[asa7uni$vi.level=="severe.vi"], raab$denom.660.unicat[raab$gender=="male"], raab$vi.denom[raab$gender=="male"],raab$clusterNumber[raab$gender=="male"])
asa7uni$male.pct.lci[asa7uni$vi.level=="moderate.vi"]<-bennett.lci(asa7uni$male.pct[asa7uni$vi.level=="moderate.vi"], raab$denom.618.unicat[raab$gender=="male"], raab$vi.denom[raab$gender=="male"],raab$clusterNumber[raab$gender=="male"])

asa7uni$male.pct.uci[asa7uni$vi.level=="blind"]<-bennett.uci(asa7uni$male.pct[asa7uni$vi.level=="blind"], raab$denom.360.unicat[raab$gender=="male"], raab$vi.denom[raab$gender=="male"],raab$clusterNumber[raab$gender=="male"])
asa7uni$male.pct.uci[asa7uni$vi.level=="severe.vi"]<-bennett.uci(asa7uni$male.pct[asa7uni$vi.level=="severe.vi"], raab$denom.660.unicat[raab$gender=="male"], raab$vi.denom[raab$gender=="male"],raab$clusterNumber[raab$gender=="male"])
asa7uni$male.pct.uci[asa7uni$vi.level=="moderate.vi"]<-bennett.uci(asa7uni$male.pct[asa7uni$vi.level=="moderate.vi"], raab$denom.618.unicat[raab$gender=="male"], raab$vi.denom[raab$gender=="male"],raab$clusterNumber[raab$gender=="male"])

asa7uni$total.pct.lci[asa7uni$vi.level=="blind"]<-bennett.lci(asa7uni$total.pct[asa7uni$vi.level=="blind"], raab$denom.360.unicat, raab$vi.denom,raab$clusterNumber)
asa7uni$total.pct.lci[asa7uni$vi.level=="severe.vi"]<-bennett.lci(asa7uni$total.pct[asa7uni$vi.level=="severe.vi"], raab$denom.660.unicat, raab$vi.denom,raab$clusterNumber)
asa7uni$total.pct.lci[asa7uni$vi.level=="moderate.vi"]<-bennett.lci(asa7uni$total.pct[asa7uni$vi.level=="moderate.vi"], raab$denom.618.unicat, raab$vi.denom,raab$clusterNumber)

asa7uni$total.pct.uci[asa7uni$vi.level=="blind"]<-bennett.uci(asa7uni$total.pct[asa7uni$vi.level=="blind"], raab$denom.360.unicat, raab$vi.denom,raab$clusterNumber)
asa7uni$total.pct.uci[asa7uni$vi.level=="severe.vi"]<-bennett.uci(asa7uni$total.pct[asa7uni$vi.level=="severe.vi"], raab$denom.660.unicat, raab$vi.denom,raab$clusterNumber)
asa7uni$total.pct.uci[asa7uni$vi.level=="moderate.vi"]<-bennett.uci(asa7uni$total.pct[asa7uni$vi.level=="moderate.vi"], raab$denom.618.unicat, raab$vi.denom,raab$clusterNumber)

asa7uni$female.adj.pct[asa7uni$vi.level=="blind"]<-prop.age.adjust(female.subpop,raab[raab$gender=="female",],raab$denom.360.unicat[raab$gender=="female"],raab$vi.denom[raab$gender=="female"])
asa7uni$female.adj.pct[asa7uni$vi.level=="severe.vi"]<-prop.age.adjust(female.subpop,raab[raab$gender=="female",],raab$denom.660.unicat[raab$gender=="female"],raab$vi.denom[raab$gender=="female"])
asa7uni$female.adj.pct[asa7uni$vi.level=="moderate.vi"]<-prop.age.adjust(female.subpop,raab[raab$gender=="female",],raab$denom.618.unicat[raab$gender=="female"],raab$vi.denom[raab$gender=="female"])

asa7uni$male.adj.pct[asa7uni$vi.level=="blind"]<-prop.age.adjust(male.subpop,raab[raab$gender=="male",],raab$denom.360.unicat[raab$gender=="male"],raab$vi.denom[raab$gender=="male"])
asa7uni$male.adj.pct[asa7uni$vi.level=="severe.vi"]<-prop.age.adjust(male.subpop,raab[raab$gender=="male",],raab$denom.660.unicat[raab$gender=="male"],raab$vi.denom[raab$gender=="male"])
asa7uni$male.adj.pct[asa7uni$vi.level=="moderate.vi"]<-prop.age.adjust(male.subpop,raab[raab$gender=="male",],raab$denom.618.unicat[raab$gender=="male"],raab$vi.denom[raab$gender=="male"])

asa7uni$total.adj.pct[asa7uni$vi.level=="blind"]<-prop.age.sex.adjust(popfives,raab,raab$denom.360.unicat,raab$vi.denom)
asa7uni$total.adj.pct[asa7uni$vi.level=="severe.vi"]<-prop.age.sex.adjust(popfives,raab,raab$denom.660.unicat,raab$vi.denom)
asa7uni$total.adj.pct[asa7uni$vi.level=="moderate.vi"]<-prop.age.sex.adjust(popfives,raab,raab$denom.618.unicat,raab$vi.denom)

asa7uni$female.adj.pct.lci[asa7uni$vi.level=="blind"]<-bennett.lci(asa7uni$female.adj.pct[asa7uni$vi.level=="blind"], raab$denom.360.unicat[raab$gender=="female"], raab$vi.denom[raab$gender=="female"],raab$clusterNumber[raab$gender=="female"])
asa7uni$female.adj.pct.lci[asa7uni$vi.level=="severe.vi"]<-bennett.lci(asa7uni$female.adj.pct[asa7uni$vi.level=="severe.vi"], raab$denom.660.unicat[raab$gender=="female"], raab$vi.denom[raab$gender=="female"],raab$clusterNumber[raab$gender=="female"])
asa7uni$female.adj.pct.lci[asa7uni$vi.level=="moderate.vi"]<-bennett.lci(asa7uni$female.adj.pct[asa7uni$vi.level=="moderate.vi"], raab$denom.618.unicat[raab$gender=="female"], raab$vi.denom[raab$gender=="female"],raab$clusterNumber[raab$gender=="female"])

asa7uni$female.adj.pct.uci[asa7uni$vi.level=="blind"]<-bennett.uci(asa7uni$female.adj.pct[asa7uni$vi.level=="blind"], raab$denom.360.unicat[raab$gender=="female"], raab$vi.denom[raab$gender=="female"],raab$clusterNumber[raab$gender=="female"])
asa7uni$female.adj.pct.uci[asa7uni$vi.level=="severe.vi"]<-bennett.uci(asa7uni$female.adj.pct[asa7uni$vi.level=="severe.vi"], raab$denom.660.unicat[raab$gender=="female"], raab$vi.denom[raab$gender=="female"],raab$clusterNumber[raab$gender=="female"])
asa7uni$female.adj.pct.uci[asa7uni$vi.level=="moderate.vi"]<-bennett.uci(asa7uni$female.adj.pct[asa7uni$vi.level=="moderate.vi"], raab$denom.618.unicat[raab$gender=="female"], raab$vi.denom[raab$gender=="female"],raab$clusterNumber[raab$gender=="female"])

asa7uni$male.adj.pct.lci[asa7uni$vi.level=="blind"]<-bennett.lci(asa7uni$male.adj.pct[asa7uni$vi.level=="blind"], raab$denom.360.unicat[raab$gender=="male"], raab$vi.denom[raab$gender=="male"],raab$clusterNumber[raab$gender=="male"])
asa7uni$male.adj.pct.lci[asa7uni$vi.level=="severe.vi"]<-bennett.lci(asa7uni$male.adj.pct[asa7uni$vi.level=="severe.vi"], raab$denom.660.unicat[raab$gender=="male"], raab$vi.denom[raab$gender=="male"],raab$clusterNumber[raab$gender=="male"])
asa7uni$male.adj.pct.lci[asa7uni$vi.level=="moderate.vi"]<-bennett.lci(asa7uni$male.adj.pct[asa7uni$vi.level=="moderate.vi"], raab$denom.618.unicat[raab$gender=="male"], raab$vi.denom[raab$gender=="male"],raab$clusterNumber[raab$gender=="male"])

asa7uni$male.adj.pct.uci[asa7uni$vi.level=="blind"]<-bennett.uci(asa7uni$male.adj.pct[asa7uni$vi.level=="blind"], raab$denom.360.unicat[raab$gender=="male"], raab$vi.denom[raab$gender=="male"],raab$clusterNumber[raab$gender=="male"])
asa7uni$male.adj.pct.uci[asa7uni$vi.level=="severe.vi"]<-bennett.uci(asa7uni$male.adj.pct[asa7uni$vi.level=="severe.vi"], raab$denom.660.unicat[raab$gender=="male"], raab$vi.denom[raab$gender=="male"],raab$clusterNumber[raab$gender=="male"])
asa7uni$male.adj.pct.uci[asa7uni$vi.level=="moderate.vi"]<-bennett.uci(asa7uni$male.adj.pct[asa7uni$vi.level=="moderate.vi"], raab$denom.618.unicat[raab$gender=="male"], raab$vi.denom[raab$gender=="male"],raab$clusterNumber[raab$gender=="male"])

asa7uni$total.adj.pct.lci[asa7uni$vi.level=="blind"]<-bennett.lci(asa7uni$total.adj.pct[asa7uni$vi.level=="blind"], raab$denom.360.unicat, raab$vi.denom,raab$clusterNumber)
asa7uni$total.adj.pct.lci[asa7uni$vi.level=="severe.vi"]<-bennett.lci(asa7uni$total.adj.pct[asa7uni$vi.level=="severe.vi"], raab$denom.660.unicat, raab$vi.denom,raab$clusterNumber)
asa7uni$total.adj.pct.lci[asa7uni$vi.level=="moderate.vi"]<-bennett.lci(asa7uni$total.adj.pct[asa7uni$vi.level=="moderate.vi"], raab$denom.618.unicat, raab$vi.denom,raab$clusterNumber)

asa7uni$total.adj.pct.uci[asa7uni$vi.level=="blind"]<-bennett.uci(asa7uni$total.adj.pct[asa7uni$vi.level=="blind"], raab$denom.360.unicat, raab$vi.denom,raab$clusterNumber)
asa7uni$total.adj.pct.uci[asa7uni$vi.level=="severe.vi"]<-bennett.uci(asa7uni$total.adj.pct[asa7uni$vi.level=="severe.vi"], raab$denom.660.unicat, raab$vi.denom,raab$clusterNumber)
asa7uni$total.adj.pct.uci[asa7uni$vi.level=="moderate.vi"]<-bennett.uci(asa7uni$total.adj.pct[asa7uni$vi.level=="moderate.vi"], raab$denom.618.unicat, raab$vi.denom,raab$clusterNumber)

asa7uni$extrapolated.female.n<-format( asa7uni$female.adj.pct * sum(female.subpop$population), digits = 0, big.mark = " ", big.interval= 3L, scientific=F)
asa7uni$extrapolated.male.n<-format( asa7uni$male.adj.pct * sum(male.subpop$population), digits = 0, big.mark = " ", big.interval= 3L, scientific=F)
asa7uni$extrapolated.total.n<-format( asa7uni$total.adj.pct * sum(popfives$population), digits = 0, big.mark = " ", big.interval= 3L, scientific=F)

asa7uni$vi.level <- recode_factor(asa7uni$vi.level,"blind" = "PinVA <3/60","severe.vi" = "PinVA <6/60","moderate.vi" = "PinVA <6/18","mild.vi" = "PinVA <6/12")

asa7<-rbind(asa7bil,asa7uni)

lcis<-grep("lci",names(asa7))
ucis<-grep("uci",names(asa7))
asa7[,lcis][asa7[,lcis]<0]<-0
asa7[,ucis][asa7[,ucis]>1]<-1

pcts<-grep("pct",names(asa7))
asa7[,pcts] <- round( asa7[,pcts] * 100, 1)
asa7[,pcts] <- format( asa7[,pcts], nsmall=1 )

asa7[c(4,8),c(2:ncol(asa7))]<-"*"
asa7$out.names[1:4]<-paste0("bilateral_cataract_",asa7$vi.level[1:4])
asa7$out.names[5:8]<-paste0("unilateral_cataract_",asa7$vi.level[5:8])