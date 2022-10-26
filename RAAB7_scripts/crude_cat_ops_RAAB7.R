#RAAB7

#v1 16/10/22 - RB

catops<-as.data.frame(c("bilateral.operated","unilateral.operated","total.operated"))
catops[,2:7]<-NA
names(catops)<-c("cat.ops",
                "female.n",
                "female.pct",
                "male.n",
                "male.pct",
                "total.n",
                "total.pct")              

catops$female.n[catops$cat.ops=="bilateral.operated"]<-sum(raab$bilat.operated[raab$gender=="female"],na.rm=T)
catops$female.pct[catops$cat.ops=="bilateral.operated"]<-sum(raab$bilat.operated[raab$gender=="female"],na.rm=T)/sum(raab$vi.denom[raab$gender=="female"],na.rm=T)

catops$male.n[catops$cat.ops=="bilateral.operated"]<-sum(raab$bilat.operated[raab$gender=="male"],na.rm=T)
catops$male.pct[catops$cat.ops=="bilateral.operated"]<-sum(raab$bilat.operated[raab$gender=="male"],na.rm=T)/sum(raab$vi.denom[raab$gender=="male"],na.rm=T)

catops$total.n[catops$cat.ops=="bilateral.operated"]<-sum(raab$bilat.operated,na.rm=T)
catops$total.pct[catops$cat.ops=="bilateral.operated"]<-sum(raab$bilat.operated,na.rm=T)/sum(raab$vi.denom,na.rm=T)


catops$female.n[catops$cat.ops=="unilateral.operated"]<-sum(raab$unilat.operated[raab$gender=="female"],na.rm=T)
catops$female.pct[catops$cat.ops=="unilateral.operated"]<-sum(raab$unilat.operated[raab$gender=="female"],na.rm=T)/sum(raab$vi.denom[raab$gender=="female"],na.rm=T)

catops$male.n[catops$cat.ops=="unilateral.operated"]<-sum(raab$unilat.operated[raab$gender=="male"],na.rm=T)
catops$male.pct[catops$cat.ops=="unilateral.operated"]<-sum(raab$unilat.operated[raab$gender=="male"],na.rm=T)/sum(raab$vi.denom[raab$gender=="male"],na.rm=T)

catops$total.n[catops$cat.ops=="unilateral.operated"]<-sum(raab$unilat.operated,na.rm=T)
catops$total.pct[catops$cat.ops=="unilateral.operated"]<-sum(raab$unilat.operated,na.rm=T)/sum(raab$vi.denom,na.rm=T)


catops$female.n[catops$cat.ops=="total.operated"]<-sum(raab$total.operated[raab$gender=="female"],na.rm=T)
catops$female.pct[catops$cat.ops=="total.operated"]<-sum(raab$total.operated[raab$gender=="female"],na.rm=T)/sum(raab$vi.denom[raab$gender=="female"],na.rm=T)

catops$male.n[catops$cat.ops=="total.operated"]<-sum(raab$total.operated[raab$gender=="male"],na.rm=T)
catops$male.pct[catops$cat.ops=="total.operated"]<-sum(raab$total.operated[raab$gender=="male"],na.rm=T)/sum(raab$vi.denom[raab$gender=="male"],na.rm=T)

catops$total.n[catops$cat.ops=="total.operated"]<-sum(raab$total.operated,na.rm=T)
catops$total.pct[catops$cat.ops=="total.operated"]<-sum(raab$total.operated,na.rm=T)/sum(raab$vi.denom,na.rm=T)

pcts<-grep("pct",names(catops))
catops[,pcts] <- round( catops[,pcts] * 100, 1)
catops[,pcts] <- format( catops[,pcts], nsmall=1 )
