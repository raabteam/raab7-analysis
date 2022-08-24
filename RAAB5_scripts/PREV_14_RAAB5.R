#RAAB5

#v1 08/09/21 - RB
#v2 24/08/22 - RB

raab$denom.618.persons<-(raab$x_case_618==1 | raab$y_case_618==1 | raab$z_case_618==1)+0
raab$denom.660.persons<-(raab$x_case_660==1 | raab$y_case_660==1 | raab$z_case_660==1)+0
raab$denom.360.persons<-(raab$x_case_360==1 | raab$y_case_360==1 | raab$z_case_360==1)+0

raab$ecsc.618.618.num.persons<-(raab$a_case_618_618==1 | raab$b_case_618==1)+0
raab$ecsc.618.660.num.persons<-(raab$a_case_618_660==1 | raab$b_case_618==1)+0
raab$ecsc.618.360.num.persons<-(raab$a_case_618_360==1 | raab$b_case_618==1)+0
raab$ecsc.660.618.num.persons<-(raab$a_case_660_618==1 | raab$b_case_660==1)+0
raab$ecsc.660.660.num.persons<-(raab$a_case_660_660==1 | raab$b_case_660==1)+0
raab$ecsc.660.360.num.persons<-(raab$a_case_660_360==1 | raab$b_case_660==1)+0
raab$ecsc.360.618.num.persons<-(raab$a_case_360_618==1 | raab$b_case_360==1)+0
raab$ecsc.360.660.num.persons<-(raab$a_case_360_660==1 | raab$b_case_360==1)+0
raab$ecsc.360.360.num.persons<-(raab$a_case_360_360==1 | raab$b_case_360==1)+0

raab$csc.618.num.persons<-(raab$x_case_618==1 | raab$y_case_618==1)+0
raab$csc.660.num.persons<-(raab$x_case_660==1 | raab$y_case_660==1)+0
raab$csc.360.num.persons<-(raab$x_case_360==1 | raab$y_case_360==1)+0

#CSC and eCSC table

denom.thresh<-c(rep("618",4),rep("660",4),rep("360",4))
num.thresh<-c(rep(c("csc","ecsc_618","ecsc_660","ecsc_360"),3))

#CSC only table

#denom.thresh<-c("618","660","360")
#num.thresh<-rep("csc",3)
prev14<-as.data.frame(cbind(denom.thresh,num.thresh))

prev14$male.denom[prev14$denom.thresh=="618"]<-sum(raab$denom.618.persons[raab$gender=="male"],na.rm=T)
prev14$male.denom[prev14$denom.thresh=="660"]<-sum(raab$denom.660.persons[raab$gender=="male"],na.rm=T)
prev14$male.denom[prev14$denom.thresh=="360"]<-sum(raab$denom.360.persons[raab$gender=="male"],na.rm=T)

prev14$female.denom[prev14$denom.thresh=="618"]<-sum(raab$denom.618.persons[raab$gender=="female"],na.rm=T)
prev14$female.denom[prev14$denom.thresh=="660"]<-sum(raab$denom.660.persons[raab$gender=="female"],na.rm=T)
prev14$female.denom[prev14$denom.thresh=="360"]<-sum(raab$denom.360.persons[raab$gender=="female"],na.rm=T)

prev14$total.denom[prev14$denom.thresh=="618"]<-sum(raab$denom.618.persons,na.rm=T)
prev14$total.denom[prev14$denom.thresh=="660"]<-sum(raab$denom.660.persons,na.rm=T)
prev14$total.denom[prev14$denom.thresh=="360"]<-sum(raab$denom.360.persons,na.rm=T)

prev14$male.num[prev14$num.thresh=="csc" & prev14$denom.thresh=="618"]<-sum(raab$csc.618.num.persons[raab$gender=="male"],na.rm=T)
prev14$male.num[prev14$num.thresh=="csc" & prev14$denom.thresh=="660"]<-sum(raab$csc.660.num.persons[raab$gender=="male"],na.rm=T)
prev14$male.num[prev14$num.thresh=="csc" & prev14$denom.thresh=="360"]<-sum(raab$csc.360.num.persons[raab$gender=="male"],na.rm=T)

prev14$male.num[prev14$num.thresh=="ecsc_618" & prev14$denom.thresh=="618"]<-sum(raab$ecsc.618.618.num.persons[raab$gender=="male"],na.rm=T)
prev14$male.num[prev14$num.thresh=="ecsc_618" & prev14$denom.thresh=="660"]<-sum(raab$ecsc.618.660.num.persons[raab$gender=="male"],na.rm=T)
prev14$male.num[prev14$num.thresh=="ecsc_618" & prev14$denom.thresh=="360"]<-sum(raab$ecsc.618.360.num.persons[raab$gender=="male"],na.rm=T)
prev14$male.num[prev14$num.thresh=="ecsc_660" & prev14$denom.thresh=="618"]<-sum(raab$ecsc.660.618.num.persons[raab$gender=="male"],na.rm=T)
prev14$male.num[prev14$num.thresh=="ecsc_660" & prev14$denom.thresh=="660"]<-sum(raab$ecsc.660.660.num.persons[raab$gender=="male"],na.rm=T)
prev14$male.num[prev14$num.thresh=="ecsc_660" & prev14$denom.thresh=="360"]<-sum(raab$ecsc.660.360.num.persons[raab$gender=="male"],na.rm=T)
prev14$male.num[prev14$num.thresh=="ecsc_360" & prev14$denom.thresh=="618"]<-sum(raab$ecsc.360.618.num.persons[raab$gender=="male"],na.rm=T)
prev14$male.num[prev14$num.thresh=="ecsc_360" & prev14$denom.thresh=="660"]<-sum(raab$ecsc.360.660.num.persons[raab$gender=="male"],na.rm=T)
prev14$male.num[prev14$num.thresh=="ecsc_360" & prev14$denom.thresh=="360"]<-sum(raab$ecsc.360.360.num.persons[raab$gender=="male"],na.rm=T)

prev14$female.num[prev14$num.thresh=="csc" & prev14$denom.thresh=="618"]<-sum(raab$csc.618.num.persons[raab$gender=="female"],na.rm=T)
prev14$female.num[prev14$num.thresh=="csc" & prev14$denom.thresh=="660"]<-sum(raab$csc.660.num.persons[raab$gender=="female"],na.rm=T)
prev14$female.num[prev14$num.thresh=="csc" & prev14$denom.thresh=="360"]<-sum(raab$csc.360.num.persons[raab$gender=="female"],na.rm=T)
prev14$female.num[prev14$num.thresh=="ecsc_618" & prev14$denom.thresh=="618"]<-sum(raab$ecsc.618.618.num.persons[raab$gender=="female"],na.rm=T)
prev14$female.num[prev14$num.thresh=="ecsc_618" & prev14$denom.thresh=="660"]<-sum(raab$ecsc.618.660.num.persons[raab$gender=="female"],na.rm=T)
prev14$female.num[prev14$num.thresh=="ecsc_618" & prev14$denom.thresh=="360"]<-sum(raab$ecsc.618.360.num.persons[raab$gender=="female"],na.rm=T)
prev14$female.num[prev14$num.thresh=="ecsc_660" & prev14$denom.thresh=="618"]<-sum(raab$ecsc.660.618.num.persons[raab$gender=="female"],na.rm=T)
prev14$female.num[prev14$num.thresh=="ecsc_660" & prev14$denom.thresh=="660"]<-sum(raab$ecsc.660.660.num.persons[raab$gender=="female"],na.rm=T)
prev14$female.num[prev14$num.thresh=="ecsc_660" & prev14$denom.thresh=="360"]<-sum(raab$ecsc.660.360.num.persons[raab$gender=="female"],na.rm=T)
prev14$female.num[prev14$num.thresh=="ecsc_360" & prev14$denom.thresh=="618"]<-sum(raab$ecsc.360.618.num.persons[raab$gender=="female"],na.rm=T)
prev14$female.num[prev14$num.thresh=="ecsc_360" & prev14$denom.thresh=="660"]<-sum(raab$ecsc.360.660.num.persons[raab$gender=="female"],na.rm=T)
prev14$female.num[prev14$num.thresh=="ecsc_360" & prev14$denom.thresh=="360"]<-sum(raab$ecsc.360.360.num.persons[raab$gender=="female"],na.rm=T)

prev14$total.num[prev14$num.thresh=="csc" & prev14$denom.thresh=="618"]<-sum(raab$csc.618.num.persons,na.rm=T)
prev14$total.num[prev14$num.thresh=="csc" & prev14$denom.thresh=="660"]<-sum(raab$csc.660.num.persons,na.rm=T)
prev14$total.num[prev14$num.thresh=="csc" & prev14$denom.thresh=="360"]<-sum(raab$csc.360.num.persons,na.rm=T)
prev14$total.num[prev14$num.thresh=="ecsc_618" & prev14$denom.thresh=="618"]<-sum(raab$ecsc.618.618.num.persons,na.rm=T)
prev14$total.num[prev14$num.thresh=="ecsc_618" & prev14$denom.thresh=="660"]<-sum(raab$ecsc.618.660.num.persons,na.rm=T)
prev14$total.num[prev14$num.thresh=="ecsc_618" & prev14$denom.thresh=="360"]<-sum(raab$ecsc.618.360.num.persons,na.rm=T)
prev14$total.num[prev14$num.thresh=="ecsc_660" & prev14$denom.thresh=="618"]<-sum(raab$ecsc.660.618.num.persons,na.rm=T)
prev14$total.num[prev14$num.thresh=="ecsc_660" & prev14$denom.thresh=="660"]<-sum(raab$ecsc.660.660.num.persons,na.rm=T)
prev14$total.num[prev14$num.thresh=="ecsc_660" & prev14$denom.thresh=="360"]<-sum(raab$ecsc.660.360.num.persons,na.rm=T)
prev14$total.num[prev14$num.thresh=="ecsc_360" & prev14$denom.thresh=="618"]<-sum(raab$ecsc.360.618.num.persons,na.rm=T)
prev14$total.num[prev14$num.thresh=="ecsc_360" & prev14$denom.thresh=="660"]<-sum(raab$ecsc.360.660.num.persons,na.rm=T)
prev14$total.num[prev14$num.thresh=="ecsc_360" & prev14$denom.thresh=="360"]<-sum(raab$ecsc.360.360.num.persons,na.rm=T)

#calculate csc

prev14$male.crude<- prev14$male.num / prev14$male.denom
prev14$female.crude<- prev14$female.num / prev14$female.denom
prev14$total.crude<- prev14$total.num / prev14$total.denom

prev14$male.adjusted[prev14$num.thresh=="csc" & prev14$denom.thresh=="618"]<- prop.age.adjust(male.subpop,raab[raab$gender=="male",],raab$csc.618.num.persons[raab$gender=="male"],raab$denom.618.persons[raab$gender=="male"])
prev14$male.adjusted[prev14$num.thresh=="csc" & prev14$denom.thresh=="660"]<- prop.age.adjust(male.subpop,raab[raab$gender=="male",],raab$csc.660.num.persons[raab$gender=="male"],raab$denom.660.persons[raab$gender=="male"])
prev14$male.adjusted[prev14$num.thresh=="csc" & prev14$denom.thresh=="360"]<- prop.age.adjust(male.subpop,raab[raab$gender=="male",],raab$csc.360.num.persons[raab$gender=="male"],raab$denom.360.persons[raab$gender=="male"])
prev14$male.adjusted[prev14$num.thresh=="ecsc_618" & prev14$denom.thresh=="618"]<- prop.age.adjust(male.subpop,raab[raab$gender=="male",],raab$ecsc.618.618.num.persons[raab$gender=="male"],raab$denom.618.persons[raab$gender=="male"])
prev14$male.adjusted[prev14$num.thresh=="ecsc_618" & prev14$denom.thresh=="660"]<- prop.age.adjust(male.subpop,raab[raab$gender=="male",],raab$ecsc.618.660.num.persons[raab$gender=="male"],raab$denom.660.persons[raab$gender=="male"])
prev14$male.adjusted[prev14$num.thresh=="ecsc_618" & prev14$denom.thresh=="360"]<- prop.age.adjust(male.subpop,raab[raab$gender=="male",],raab$ecsc.618.360.num.persons[raab$gender=="male"],raab$denom.360.persons[raab$gender=="male"])
prev14$male.adjusted[prev14$num.thresh=="ecsc_660" & prev14$denom.thresh=="618"]<- prop.age.adjust(male.subpop,raab[raab$gender=="male",],raab$ecsc.660.618.num.persons[raab$gender=="male"],raab$denom.618.persons[raab$gender=="male"])
prev14$male.adjusted[prev14$num.thresh=="ecsc_660" & prev14$denom.thresh=="660"]<- prop.age.adjust(male.subpop,raab[raab$gender=="male",],raab$ecsc.660.660.num.persons[raab$gender=="male"],raab$denom.660.persons[raab$gender=="male"])
prev14$male.adjusted[prev14$num.thresh=="ecsc_660" & prev14$denom.thresh=="360"]<- prop.age.adjust(male.subpop,raab[raab$gender=="male",],raab$ecsc.660.360.num.persons[raab$gender=="male"],raab$denom.360.persons[raab$gender=="male"])
prev14$male.adjusted[prev14$num.thresh=="ecsc_360" & prev14$denom.thresh=="618"]<- prop.age.adjust(male.subpop,raab[raab$gender=="male",],raab$ecsc.360.618.num.persons[raab$gender=="male"],raab$denom.618.persons[raab$gender=="male"])
prev14$male.adjusted[prev14$num.thresh=="ecsc_360" & prev14$denom.thresh=="660"]<- prop.age.adjust(male.subpop,raab[raab$gender=="male",],raab$ecsc.360.660.num.persons[raab$gender=="male"],raab$denom.660.persons[raab$gender=="male"])
prev14$male.adjusted[prev14$num.thresh=="ecsc_360" & prev14$denom.thresh=="360"]<- prop.age.adjust(male.subpop,raab[raab$gender=="male",],raab$ecsc.360.360.num.persons[raab$gender=="male"],raab$denom.360.persons[raab$gender=="male"])

prev14$female.adjusted[prev14$num.thresh=="csc" & prev14$denom.thresh=="618"]<- prop.age.adjust(female.subpop,raab[raab$gender=="female",],raab$csc.618.num.persons[raab$gender=="female"],raab$denom.618.persons[raab$gender=="female"])
prev14$female.adjusted[prev14$num.thresh=="csc" & prev14$denom.thresh=="660"]<- prop.age.adjust(female.subpop,raab[raab$gender=="female",],raab$csc.660.num.persons[raab$gender=="female"],raab$denom.660.persons[raab$gender=="female"])
prev14$female.adjusted[prev14$num.thresh=="csc" & prev14$denom.thresh=="360"]<- prop.age.adjust(female.subpop,raab[raab$gender=="female",],raab$csc.360.num.persons[raab$gender=="female"],raab$denom.360.persons[raab$gender=="female"])
prev14$female.adjusted[prev14$num.thresh=="ecsc_618" & prev14$denom.thresh=="618"]<- prop.age.adjust(female.subpop,raab[raab$gender=="female",],raab$ecsc.618.618.num.persons[raab$gender=="female"],raab$denom.618.persons[raab$gender=="female"])
prev14$female.adjusted[prev14$num.thresh=="ecsc_618" & prev14$denom.thresh=="660"]<- prop.age.adjust(female.subpop,raab[raab$gender=="female",],raab$ecsc.618.660.num.persons[raab$gender=="female"],raab$denom.660.persons[raab$gender=="female"])
prev14$female.adjusted[prev14$num.thresh=="ecsc_618" & prev14$denom.thresh=="360"]<- prop.age.adjust(female.subpop,raab[raab$gender=="female",],raab$ecsc.618.360.num.persons[raab$gender=="female"],raab$denom.360.persons[raab$gender=="female"])
prev14$female.adjusted[prev14$num.thresh=="ecsc_660" & prev14$denom.thresh=="618"]<- prop.age.adjust(female.subpop,raab[raab$gender=="female",],raab$ecsc.660.618.num.persons[raab$gender=="female"],raab$denom.618.persons[raab$gender=="female"])
prev14$female.adjusted[prev14$num.thresh=="ecsc_660" & prev14$denom.thresh=="660"]<- prop.age.adjust(female.subpop,raab[raab$gender=="female",],raab$ecsc.660.660.num.persons[raab$gender=="female"],raab$denom.660.persons[raab$gender=="female"])
prev14$female.adjusted[prev14$num.thresh=="ecsc_660" & prev14$denom.thresh=="360"]<- prop.age.adjust(female.subpop,raab[raab$gender=="female",],raab$ecsc.660.360.num.persons[raab$gender=="female"],raab$denom.360.persons[raab$gender=="female"])
prev14$female.adjusted[prev14$num.thresh=="ecsc_360" & prev14$denom.thresh=="618"]<- prop.age.adjust(female.subpop,raab[raab$gender=="female",],raab$ecsc.360.618.num.persons[raab$gender=="female"],raab$denom.618.persons[raab$gender=="female"])
prev14$female.adjusted[prev14$num.thresh=="ecsc_360" & prev14$denom.thresh=="660"]<- prop.age.adjust(female.subpop,raab[raab$gender=="female",],raab$ecsc.360.660.num.persons[raab$gender=="female"],raab$denom.660.persons[raab$gender=="female"])
prev14$female.adjusted[prev14$num.thresh=="ecsc_360" & prev14$denom.thresh=="360"]<- prop.age.adjust(female.subpop,raab[raab$gender=="female",],raab$ecsc.360.360.num.persons[raab$gender=="female"],raab$denom.360.persons[raab$gender=="female"])

prev14$total.adjusted[prev14$num.thresh=="csc" & prev14$denom.thresh=="618"]<- prop.age.sex.adjust(popfives,raab,raab$csc.618.num.persons,raab$denom.618.persons)
prev14$total.adjusted[prev14$num.thresh=="csc" & prev14$denom.thresh=="660"]<- prop.age.sex.adjust(popfives,raab,raab$csc.660.num.persons,raab$denom.660.persons)
prev14$total.adjusted[prev14$num.thresh=="csc" & prev14$denom.thresh=="360"]<- prop.age.sex.adjust(popfives,raab,raab$csc.360.num.persons,raab$denom.360.persons)
prev14$total.adjusted[prev14$num.thresh=="ecsc_618" & prev14$denom.thresh=="618"]<- prop.age.sex.adjust(popfives,raab,raab$ecsc.618.618.num.persons,raab$denom.618.persons)
prev14$total.adjusted[prev14$num.thresh=="ecsc_618" & prev14$denom.thresh=="660"]<- prop.age.sex.adjust(popfives,raab,raab$ecsc.618.660.num.persons,raab$denom.660.persons)
prev14$total.adjusted[prev14$num.thresh=="ecsc_618" & prev14$denom.thresh=="360"]<- prop.age.sex.adjust(popfives,raab,raab$ecsc.618.360.num.persons,raab$denom.360.persons)
prev14$total.adjusted[prev14$num.thresh=="ecsc_660" & prev14$denom.thresh=="618"]<- prop.age.sex.adjust(popfives,raab,raab$ecsc.660.618.num.persons,raab$denom.618.persons)
prev14$total.adjusted[prev14$num.thresh=="ecsc_660" & prev14$denom.thresh=="660"]<- prop.age.sex.adjust(popfives,raab,raab$ecsc.660.660.num.persons,raab$denom.660.persons)
prev14$total.adjusted[prev14$num.thresh=="ecsc_660" & prev14$denom.thresh=="360"]<- prop.age.sex.adjust(popfives,raab,raab$ecsc.660.360.num.persons,raab$denom.360.persons)
prev14$total.adjusted[prev14$num.thresh=="ecsc_360" & prev14$denom.thresh=="618"]<- prop.age.sex.adjust(popfives,raab,raab$ecsc.360.618.num.persons,raab$denom.618.persons)
prev14$total.adjusted[prev14$num.thresh=="ecsc_360" & prev14$denom.thresh=="660"]<- prop.age.sex.adjust(popfives,raab,raab$ecsc.360.660.num.persons,raab$denom.660.persons)
prev14$total.adjusted[prev14$num.thresh=="ecsc_360" & prev14$denom.thresh=="360"]<- prop.age.sex.adjust(popfives,raab,raab$ecsc.360.360.num.persons,raab$denom.360.persons)

prev14$male.adjusted.lci[prev14$num.thresh=="csc" & prev14$denom.thresh=="618"]<- bennett.lci(prev14$male.adjusted[prev14$num.thresh=="csc" & prev14$denom.thresh=="618"],raab$csc.618.num.persons[raab$gender=="male"],raab$denom.618.persons[raab$gender=="male"],raab$clusterNumber[raab$gender=="male"])
prev14$male.adjusted.lci[prev14$num.thresh=="csc" & prev14$denom.thresh=="660"]<- bennett.lci(prev14$male.adjusted[prev14$num.thresh=="csc" & prev14$denom.thresh=="660"],raab$csc.660.num.persons[raab$gender=="male"],raab$denom.660.persons[raab$gender=="male"],raab$clusterNumber[raab$gender=="male"])
prev14$male.adjusted.lci[prev14$num.thresh=="csc" & prev14$denom.thresh=="360"]<- bennett.lci(prev14$male.adjusted[prev14$num.thresh=="csc" & prev14$denom.thresh=="360"],raab$csc.360.num.persons[raab$gender=="male"],raab$denom.360.persons[raab$gender=="male"],raab$clusterNumber[raab$gender=="male"])
prev14$male.adjusted.lci[prev14$num.thresh=="ecsc_618" & prev14$denom.thresh=="618"]<- bennett.lci(prev14$male.adjusted[prev14$num.thresh=="ecsc_618" & prev14$denom.thresh=="618"],raab$ecsc.618.618.num.persons[raab$gender=="male"],raab$denom.618.persons[raab$gender=="male"],raab$clusterNumber[raab$gender=="male"])
prev14$male.adjusted.lci[prev14$num.thresh=="ecsc_618" & prev14$denom.thresh=="660"]<- bennett.lci(prev14$male.adjusted[prev14$num.thresh=="ecsc_618" & prev14$denom.thresh=="660"],raab$ecsc.618.660.num.persons[raab$gender=="male"],raab$denom.660.persons[raab$gender=="male"],raab$clusterNumber[raab$gender=="male"])
prev14$male.adjusted.lci[prev14$num.thresh=="ecsc_618" & prev14$denom.thresh=="360"]<- bennett.lci(prev14$male.adjusted[prev14$num.thresh=="ecsc_618" & prev14$denom.thresh=="360"],raab$ecsc.618.360.num.persons[raab$gender=="male"],raab$denom.360.persons[raab$gender=="male"],raab$clusterNumber[raab$gender=="male"])
prev14$male.adjusted.lci[prev14$num.thresh=="ecsc_660" & prev14$denom.thresh=="618"]<- bennett.lci(prev14$male.adjusted[prev14$num.thresh=="ecsc_660" & prev14$denom.thresh=="618"],raab$ecsc.660.618.num.persons[raab$gender=="male"],raab$denom.618.persons[raab$gender=="male"],raab$clusterNumber[raab$gender=="male"])
prev14$male.adjusted.lci[prev14$num.thresh=="ecsc_660" & prev14$denom.thresh=="660"]<- bennett.lci(prev14$male.adjusted[prev14$num.thresh=="ecsc_660" & prev14$denom.thresh=="660"],raab$ecsc.660.660.num.persons[raab$gender=="male"],raab$denom.660.persons[raab$gender=="male"],raab$clusterNumber[raab$gender=="male"])
prev14$male.adjusted.lci[prev14$num.thresh=="ecsc_660" & prev14$denom.thresh=="360"]<- bennett.lci(prev14$male.adjusted[prev14$num.thresh=="ecsc_660" & prev14$denom.thresh=="360"],raab$ecsc.660.360.num.persons[raab$gender=="male"],raab$denom.360.persons[raab$gender=="male"],raab$clusterNumber[raab$gender=="male"])
prev14$male.adjusted.lci[prev14$num.thresh=="ecsc_360" & prev14$denom.thresh=="618"]<- bennett.lci(prev14$male.adjusted[prev14$num.thresh=="ecsc_360" & prev14$denom.thresh=="618"],raab$ecsc.360.618.num.persons[raab$gender=="male"],raab$denom.618.persons[raab$gender=="male"],raab$clusterNumber[raab$gender=="male"])
prev14$male.adjusted.lci[prev14$num.thresh=="ecsc_360" & prev14$denom.thresh=="660"]<- bennett.lci(prev14$male.adjusted[prev14$num.thresh=="ecsc_360" & prev14$denom.thresh=="660"],raab$ecsc.360.660.num.persons[raab$gender=="male"],raab$denom.660.persons[raab$gender=="male"],raab$clusterNumber[raab$gender=="male"])
prev14$male.adjusted.lci[prev14$num.thresh=="ecsc_360" & prev14$denom.thresh=="360"]<- bennett.lci(prev14$male.adjusted[prev14$num.thresh=="ecsc_360" & prev14$denom.thresh=="360"],raab$ecsc.360.360.num.persons[raab$gender=="male"],raab$denom.360.persons[raab$gender=="male"],raab$clusterNumber[raab$gender=="male"])

prev14$male.adjusted.uci[prev14$num.thresh=="csc" & prev14$denom.thresh=="618"]<- bennett.uci(prev14$male.adjusted[prev14$num.thresh=="csc" & prev14$denom.thresh=="618"],raab$csc.618.num.persons[raab$gender=="male"],raab$denom.618.persons[raab$gender=="male"],raab$clusterNumber[raab$gender=="male"])
prev14$male.adjusted.uci[prev14$num.thresh=="csc" & prev14$denom.thresh=="660"]<- bennett.uci(prev14$male.adjusted[prev14$num.thresh=="csc" & prev14$denom.thresh=="660"],raab$csc.660.num.persons[raab$gender=="male"],raab$denom.660.persons[raab$gender=="male"],raab$clusterNumber[raab$gender=="male"])
prev14$male.adjusted.uci[prev14$num.thresh=="csc" & prev14$denom.thresh=="360"]<- bennett.uci(prev14$male.adjusted[prev14$num.thresh=="csc" & prev14$denom.thresh=="360"],raab$csc.360.num.persons[raab$gender=="male"],raab$denom.360.persons[raab$gender=="male"],raab$clusterNumber[raab$gender=="male"])
prev14$male.adjusted.uci[prev14$num.thresh=="ecsc_618" & prev14$denom.thresh=="618"]<- bennett.uci(prev14$male.adjusted[prev14$num.thresh=="ecsc_618" & prev14$denom.thresh=="618"],raab$ecsc.618.618.num.persons[raab$gender=="male"],raab$denom.618.persons[raab$gender=="male"],raab$clusterNumber[raab$gender=="male"])
prev14$male.adjusted.uci[prev14$num.thresh=="ecsc_618" & prev14$denom.thresh=="660"]<- bennett.uci(prev14$male.adjusted[prev14$num.thresh=="ecsc_618" & prev14$denom.thresh=="660"],raab$ecsc.618.660.num.persons[raab$gender=="male"],raab$denom.660.persons[raab$gender=="male"],raab$clusterNumber[raab$gender=="male"])
prev14$male.adjusted.uci[prev14$num.thresh=="ecsc_618" & prev14$denom.thresh=="360"]<- bennett.uci(prev14$male.adjusted[prev14$num.thresh=="ecsc_618" & prev14$denom.thresh=="360"],raab$ecsc.618.360.num.persons[raab$gender=="male"],raab$denom.360.persons[raab$gender=="male"],raab$clusterNumber[raab$gender=="male"])
prev14$male.adjusted.uci[prev14$num.thresh=="ecsc_660" & prev14$denom.thresh=="618"]<- bennett.uci(prev14$male.adjusted[prev14$num.thresh=="ecsc_660" & prev14$denom.thresh=="618"],raab$ecsc.660.618.num.persons[raab$gender=="male"],raab$denom.618.persons[raab$gender=="male"],raab$clusterNumber[raab$gender=="male"])
prev14$male.adjusted.uci[prev14$num.thresh=="ecsc_660" & prev14$denom.thresh=="660"]<- bennett.uci(prev14$male.adjusted[prev14$num.thresh=="ecsc_660" & prev14$denom.thresh=="660"],raab$ecsc.660.660.num.persons[raab$gender=="male"],raab$denom.660.persons[raab$gender=="male"],raab$clusterNumber[raab$gender=="male"])
prev14$male.adjusted.uci[prev14$num.thresh=="ecsc_660" & prev14$denom.thresh=="360"]<- bennett.uci(prev14$male.adjusted[prev14$num.thresh=="ecsc_660" & prev14$denom.thresh=="360"],raab$ecsc.660.360.num.persons[raab$gender=="male"],raab$denom.360.persons[raab$gender=="male"],raab$clusterNumber[raab$gender=="male"])
prev14$male.adjusted.uci[prev14$num.thresh=="ecsc_360" & prev14$denom.thresh=="618"]<- bennett.uci(prev14$male.adjusted[prev14$num.thresh=="ecsc_360" & prev14$denom.thresh=="618"],raab$ecsc.360.618.num.persons[raab$gender=="male"],raab$denom.618.persons[raab$gender=="male"],raab$clusterNumber[raab$gender=="male"])
prev14$male.adjusted.uci[prev14$num.thresh=="ecsc_360" & prev14$denom.thresh=="660"]<- bennett.uci(prev14$male.adjusted[prev14$num.thresh=="ecsc_360" & prev14$denom.thresh=="660"],raab$ecsc.360.660.num.persons[raab$gender=="male"],raab$denom.660.persons[raab$gender=="male"],raab$clusterNumber[raab$gender=="male"])
prev14$male.adjusted.uci[prev14$num.thresh=="ecsc_360" & prev14$denom.thresh=="360"]<- bennett.uci(prev14$male.adjusted[prev14$num.thresh=="ecsc_360" & prev14$denom.thresh=="360"],raab$ecsc.360.360.num.persons[raab$gender=="male"],raab$denom.360.persons[raab$gender=="male"],raab$clusterNumber[raab$gender=="male"])

prev14$female.adjusted.lci[prev14$num.thresh=="csc" & prev14$denom.thresh=="618"]<- bennett.lci(prev14$female.adjusted[prev14$num.thresh=="csc" & prev14$denom.thresh=="618"],raab$csc.618.num.persons[raab$gender=="female"],raab$denom.618.persons[raab$gender=="female"],raab$clusterNumber[raab$gender=="female"])
prev14$female.adjusted.lci[prev14$num.thresh=="csc" & prev14$denom.thresh=="660"]<- bennett.lci(prev14$female.adjusted[prev14$num.thresh=="csc" & prev14$denom.thresh=="660"],raab$csc.660.num.persons[raab$gender=="female"],raab$denom.660.persons[raab$gender=="female"],raab$clusterNumber[raab$gender=="female"])
prev14$female.adjusted.lci[prev14$num.thresh=="csc" & prev14$denom.thresh=="360"]<- bennett.lci(prev14$female.adjusted[prev14$num.thresh=="csc" & prev14$denom.thresh=="360"],raab$csc.360.num.persons[raab$gender=="female"],raab$denom.360.persons[raab$gender=="female"],raab$clusterNumber[raab$gender=="female"])
prev14$female.adjusted.lci[prev14$num.thresh=="ecsc_618" & prev14$denom.thresh=="618"]<- bennett.lci(prev14$female.adjusted[prev14$num.thresh=="ecsc_618" & prev14$denom.thresh=="618"],raab$ecsc.618.618.num.persons[raab$gender=="female"],raab$denom.618.persons[raab$gender=="female"],raab$clusterNumber[raab$gender=="female"])
prev14$female.adjusted.lci[prev14$num.thresh=="ecsc_618" & prev14$denom.thresh=="660"]<- bennett.lci(prev14$female.adjusted[prev14$num.thresh=="ecsc_618" & prev14$denom.thresh=="660"],raab$ecsc.618.660.num.persons[raab$gender=="female"],raab$denom.660.persons[raab$gender=="female"],raab$clusterNumber[raab$gender=="female"])
prev14$female.adjusted.lci[prev14$num.thresh=="ecsc_618" & prev14$denom.thresh=="360"]<- bennett.lci(prev14$female.adjusted[prev14$num.thresh=="ecsc_618" & prev14$denom.thresh=="360"],raab$ecsc.618.360.num.persons[raab$gender=="female"],raab$denom.360.persons[raab$gender=="female"],raab$clusterNumber[raab$gender=="female"])
prev14$female.adjusted.lci[prev14$num.thresh=="ecsc_660" & prev14$denom.thresh=="618"]<- bennett.lci(prev14$female.adjusted[prev14$num.thresh=="ecsc_660" & prev14$denom.thresh=="618"],raab$ecsc.660.618.num.persons[raab$gender=="female"],raab$denom.618.persons[raab$gender=="female"],raab$clusterNumber[raab$gender=="female"])
prev14$female.adjusted.lci[prev14$num.thresh=="ecsc_660" & prev14$denom.thresh=="660"]<- bennett.lci(prev14$female.adjusted[prev14$num.thresh=="ecsc_660" & prev14$denom.thresh=="660"],raab$ecsc.660.660.num.persons[raab$gender=="female"],raab$denom.660.persons[raab$gender=="female"],raab$clusterNumber[raab$gender=="female"])
prev14$female.adjusted.lci[prev14$num.thresh=="ecsc_660" & prev14$denom.thresh=="360"]<- bennett.lci(prev14$female.adjusted[prev14$num.thresh=="ecsc_660" & prev14$denom.thresh=="360"],raab$ecsc.660.360.num.persons[raab$gender=="female"],raab$denom.360.persons[raab$gender=="female"],raab$clusterNumber[raab$gender=="female"])
prev14$female.adjusted.lci[prev14$num.thresh=="ecsc_360" & prev14$denom.thresh=="618"]<- bennett.lci(prev14$female.adjusted[prev14$num.thresh=="ecsc_360" & prev14$denom.thresh=="618"],raab$ecsc.360.618.num.persons[raab$gender=="female"],raab$denom.618.persons[raab$gender=="female"],raab$clusterNumber[raab$gender=="female"])
prev14$female.adjusted.lci[prev14$num.thresh=="ecsc_360" & prev14$denom.thresh=="660"]<- bennett.lci(prev14$female.adjusted[prev14$num.thresh=="ecsc_360" & prev14$denom.thresh=="660"],raab$ecsc.360.660.num.persons[raab$gender=="female"],raab$denom.660.persons[raab$gender=="female"],raab$clusterNumber[raab$gender=="female"])
prev14$female.adjusted.lci[prev14$num.thresh=="ecsc_360" & prev14$denom.thresh=="360"]<- bennett.lci(prev14$female.adjusted[prev14$num.thresh=="ecsc_360" & prev14$denom.thresh=="360"],raab$ecsc.360.360.num.persons[raab$gender=="female"],raab$denom.360.persons[raab$gender=="female"],raab$clusterNumber[raab$gender=="female"])

prev14$female.adjusted.uci[prev14$num.thresh=="csc" & prev14$denom.thresh=="618"]<- bennett.uci(prev14$female.adjusted[prev14$num.thresh=="csc" & prev14$denom.thresh=="618"],raab$csc.618.num.persons[raab$gender=="female"],raab$denom.618.persons[raab$gender=="female"],raab$clusterNumber[raab$gender=="female"])
prev14$female.adjusted.uci[prev14$num.thresh=="csc" & prev14$denom.thresh=="660"]<- bennett.uci(prev14$female.adjusted[prev14$num.thresh=="csc" & prev14$denom.thresh=="660"],raab$csc.660.num.persons[raab$gender=="female"],raab$denom.660.persons[raab$gender=="female"],raab$clusterNumber[raab$gender=="female"])
prev14$female.adjusted.uci[prev14$num.thresh=="csc" & prev14$denom.thresh=="360"]<- bennett.uci(prev14$female.adjusted[prev14$num.thresh=="csc" & prev14$denom.thresh=="360"],raab$csc.360.num.persons[raab$gender=="female"],raab$denom.360.persons[raab$gender=="female"],raab$clusterNumber[raab$gender=="female"])
prev14$female.adjusted.uci[prev14$num.thresh=="ecsc_618" & prev14$denom.thresh=="618"]<- bennett.uci(prev14$female.adjusted[prev14$num.thresh=="ecsc_618" & prev14$denom.thresh=="618"],raab$ecsc.618.618.num.persons[raab$gender=="female"],raab$denom.618.persons[raab$gender=="female"],raab$clusterNumber[raab$gender=="female"])
prev14$female.adjusted.uci[prev14$num.thresh=="ecsc_618" & prev14$denom.thresh=="660"]<- bennett.uci(prev14$female.adjusted[prev14$num.thresh=="ecsc_618" & prev14$denom.thresh=="660"],raab$ecsc.618.660.num.persons[raab$gender=="female"],raab$denom.660.persons[raab$gender=="female"],raab$clusterNumber[raab$gender=="female"])
prev14$female.adjusted.uci[prev14$num.thresh=="ecsc_618" & prev14$denom.thresh=="360"]<- bennett.uci(prev14$female.adjusted[prev14$num.thresh=="ecsc_618" & prev14$denom.thresh=="360"],raab$ecsc.618.360.num.persons[raab$gender=="female"],raab$denom.360.persons[raab$gender=="female"],raab$clusterNumber[raab$gender=="female"])
prev14$female.adjusted.uci[prev14$num.thresh=="ecsc_660" & prev14$denom.thresh=="618"]<- bennett.uci(prev14$female.adjusted[prev14$num.thresh=="ecsc_660" & prev14$denom.thresh=="618"],raab$ecsc.660.618.num.persons[raab$gender=="female"],raab$denom.618.persons[raab$gender=="female"],raab$clusterNumber[raab$gender=="female"])
prev14$female.adjusted.uci[prev14$num.thresh=="ecsc_660" & prev14$denom.thresh=="660"]<- bennett.uci(prev14$female.adjusted[prev14$num.thresh=="ecsc_660" & prev14$denom.thresh=="660"],raab$ecsc.660.660.num.persons[raab$gender=="female"],raab$denom.660.persons[raab$gender=="female"],raab$clusterNumber[raab$gender=="female"])
prev14$female.adjusted.uci[prev14$num.thresh=="ecsc_660" & prev14$denom.thresh=="360"]<- bennett.uci(prev14$female.adjusted[prev14$num.thresh=="ecsc_660" & prev14$denom.thresh=="360"],raab$ecsc.660.360.num.persons[raab$gender=="female"],raab$denom.360.persons[raab$gender=="female"],raab$clusterNumber[raab$gender=="female"])
prev14$female.adjusted.uci[prev14$num.thresh=="ecsc_360" & prev14$denom.thresh=="618"]<- bennett.uci(prev14$female.adjusted[prev14$num.thresh=="ecsc_360" & prev14$denom.thresh=="618"],raab$ecsc.360.618.num.persons[raab$gender=="female"],raab$denom.618.persons[raab$gender=="female"],raab$clusterNumber[raab$gender=="female"])
prev14$female.adjusted.uci[prev14$num.thresh=="ecsc_360" & prev14$denom.thresh=="660"]<- bennett.uci(prev14$female.adjusted[prev14$num.thresh=="ecsc_360" & prev14$denom.thresh=="660"],raab$ecsc.360.660.num.persons[raab$gender=="female"],raab$denom.660.persons[raab$gender=="female"],raab$clusterNumber[raab$gender=="female"])
prev14$female.adjusted.uci[prev14$num.thresh=="ecsc_360" & prev14$denom.thresh=="360"]<- bennett.uci(prev14$female.adjusted[prev14$num.thresh=="ecsc_360" & prev14$denom.thresh=="360"],raab$ecsc.360.360.num.persons[raab$gender=="female"],raab$denom.360.persons[raab$gender=="female"],raab$clusterNumber[raab$gender=="female"])

prev14$total.adjusted.lci[prev14$num.thresh=="csc" & prev14$denom.thresh=="618"]<- bennett.lci(prev14$total.adjusted[prev14$num.thresh=="csc" & prev14$denom.thresh=="618"],raab$csc.618.num.persons,raab$denom.618.persons,raab$clusterNumber)
prev14$total.adjusted.lci[prev14$num.thresh=="csc" & prev14$denom.thresh=="660"]<- bennett.lci(prev14$total.adjusted[prev14$num.thresh=="csc" & prev14$denom.thresh=="660"],raab$csc.660.num.persons,raab$denom.660.persons,raab$clusterNumber)
prev14$total.adjusted.lci[prev14$num.thresh=="csc" & prev14$denom.thresh=="360"]<- bennett.lci(prev14$total.adjusted[prev14$num.thresh=="csc" & prev14$denom.thresh=="360"],raab$csc.360.num.persons,raab$denom.360.persons,raab$clusterNumber)
prev14$total.adjusted.lci[prev14$num.thresh=="ecsc_618" & prev14$denom.thresh=="618"]<- bennett.lci(prev14$total.adjusted[prev14$num.thresh=="ecsc_618" & prev14$denom.thresh=="618"],raab$ecsc.618.618.num.persons,raab$denom.618.persons,raab$clusterNumber)
prev14$total.adjusted.lci[prev14$num.thresh=="ecsc_618" & prev14$denom.thresh=="660"]<- bennett.lci(prev14$total.adjusted[prev14$num.thresh=="ecsc_618" & prev14$denom.thresh=="660"],raab$ecsc.618.660.num.persons,raab$denom.660.persons,raab$clusterNumber)
prev14$total.adjusted.lci[prev14$num.thresh=="ecsc_618" & prev14$denom.thresh=="360"]<- bennett.lci(prev14$total.adjusted[prev14$num.thresh=="ecsc_618" & prev14$denom.thresh=="360"],raab$ecsc.618.360.num.persons,raab$denom.360.persons,raab$clusterNumber)
prev14$total.adjusted.lci[prev14$num.thresh=="ecsc_660" & prev14$denom.thresh=="618"]<- bennett.lci(prev14$total.adjusted[prev14$num.thresh=="ecsc_660" & prev14$denom.thresh=="618"],raab$ecsc.660.618.num.persons,raab$denom.618.persons,raab$clusterNumber)
prev14$total.adjusted.lci[prev14$num.thresh=="ecsc_660" & prev14$denom.thresh=="660"]<- bennett.lci(prev14$total.adjusted[prev14$num.thresh=="ecsc_660" & prev14$denom.thresh=="660"],raab$ecsc.660.660.num.persons,raab$denom.660.persons,raab$clusterNumber)
prev14$total.adjusted.lci[prev14$num.thresh=="ecsc_660" & prev14$denom.thresh=="360"]<- bennett.lci(prev14$total.adjusted[prev14$num.thresh=="ecsc_660" & prev14$denom.thresh=="360"],raab$ecsc.660.360.num.persons,raab$denom.360.persons,raab$clusterNumber)
prev14$total.adjusted.lci[prev14$num.thresh=="ecsc_360" & prev14$denom.thresh=="618"]<- bennett.lci(prev14$total.adjusted[prev14$num.thresh=="ecsc_360" & prev14$denom.thresh=="618"],raab$ecsc.360.618.num.persons,raab$denom.618.persons,raab$clusterNumber)
prev14$total.adjusted.lci[prev14$num.thresh=="ecsc_360" & prev14$denom.thresh=="660"]<- bennett.lci(prev14$total.adjusted[prev14$num.thresh=="ecsc_360" & prev14$denom.thresh=="660"],raab$ecsc.360.660.num.persons,raab$denom.660.persons,raab$clusterNumber)
prev14$total.adjusted.lci[prev14$num.thresh=="ecsc_360" & prev14$denom.thresh=="360"]<- bennett.lci(prev14$total.adjusted[prev14$num.thresh=="ecsc_360" & prev14$denom.thresh=="360"],raab$ecsc.360.360.num.persons,raab$denom.360.persons,raab$clusterNumber)

prev14$total.adjusted.uci[prev14$num.thresh=="csc" & prev14$denom.thresh=="618"]<- bennett.uci(prev14$total.adjusted[prev14$num.thresh=="csc" & prev14$denom.thresh=="618"],raab$csc.618.num.persons,raab$denom.618.persons,raab$clusterNumber)
prev14$total.adjusted.uci[prev14$num.thresh=="csc" & prev14$denom.thresh=="660"]<- bennett.uci(prev14$total.adjusted[prev14$num.thresh=="csc" & prev14$denom.thresh=="660"],raab$csc.660.num.persons,raab$denom.660.persons,raab$clusterNumber)
prev14$total.adjusted.uci[prev14$num.thresh=="csc" & prev14$denom.thresh=="360"]<- bennett.uci(prev14$total.adjusted[prev14$num.thresh=="csc" & prev14$denom.thresh=="360"],raab$csc.360.num.persons,raab$denom.360.persons,raab$clusterNumber)
prev14$total.adjusted.uci[prev14$num.thresh=="ecsc_618" & prev14$denom.thresh=="618"]<- bennett.uci(prev14$total.adjusted[prev14$num.thresh=="ecsc_618" & prev14$denom.thresh=="618"],raab$ecsc.618.618.num.persons,raab$denom.618.persons,raab$clusterNumber)
prev14$total.adjusted.uci[prev14$num.thresh=="ecsc_618" & prev14$denom.thresh=="660"]<- bennett.uci(prev14$total.adjusted[prev14$num.thresh=="ecsc_618" & prev14$denom.thresh=="660"],raab$ecsc.618.660.num.persons,raab$denom.660.persons,raab$clusterNumber)
prev14$total.adjusted.uci[prev14$num.thresh=="ecsc_618" & prev14$denom.thresh=="360"]<- bennett.uci(prev14$total.adjusted[prev14$num.thresh=="ecsc_618" & prev14$denom.thresh=="360"],raab$ecsc.618.360.num.persons,raab$denom.360.persons,raab$clusterNumber)
prev14$total.adjusted.uci[prev14$num.thresh=="ecsc_660" & prev14$denom.thresh=="618"]<- bennett.uci(prev14$total.adjusted[prev14$num.thresh=="ecsc_660" & prev14$denom.thresh=="618"],raab$ecsc.660.618.num.persons,raab$denom.618.persons,raab$clusterNumber)
prev14$total.adjusted.uci[prev14$num.thresh=="ecsc_660" & prev14$denom.thresh=="660"]<- bennett.uci(prev14$total.adjusted[prev14$num.thresh=="ecsc_660" & prev14$denom.thresh=="660"],raab$ecsc.660.660.num.persons,raab$denom.660.persons,raab$clusterNumber)
prev14$total.adjusted.uci[prev14$num.thresh=="ecsc_660" & prev14$denom.thresh=="360"]<- bennett.uci(prev14$total.adjusted[prev14$num.thresh=="ecsc_660" & prev14$denom.thresh=="360"],raab$ecsc.660.360.num.persons,raab$denom.360.persons,raab$clusterNumber)
prev14$total.adjusted.uci[prev14$num.thresh=="ecsc_360" & prev14$denom.thresh=="618"]<- bennett.uci(prev14$total.adjusted[prev14$num.thresh=="ecsc_360" & prev14$denom.thresh=="618"],raab$ecsc.360.618.num.persons,raab$denom.618.persons,raab$clusterNumber)
prev14$total.adjusted.uci[prev14$num.thresh=="ecsc_360" & prev14$denom.thresh=="660"]<- bennett.uci(prev14$total.adjusted[prev14$num.thresh=="ecsc_360" & prev14$denom.thresh=="660"],raab$ecsc.360.660.num.persons,raab$denom.660.persons,raab$clusterNumber)
prev14$total.adjusted.uci[prev14$num.thresh=="ecsc_360" & prev14$denom.thresh=="360"]<- bennett.uci(prev14$total.adjusted[prev14$num.thresh=="ecsc_360" & prev14$denom.thresh=="360"],raab$ecsc.360.360.num.persons,raab$denom.360.persons,raab$clusterNumber)

lcis<-grep("lci",names(prev14))
ucis<-grep("uci",names(prev14))
prev14[,lcis][prev14[,lcis]<0]<-0
prev14[,ucis][prev14[,ucis]>1]<-1

apcts<-grep("adjusted",names(prev14))
prev14[,apcts] <- round( prev14[,apcts] * 100, 1)
prev14[,apcts] <- format( prev14[,apcts] , nsmall=1)


cpcts<-grep("crude",names(prev14))
prev14[,cpcts] <- round( prev14[,cpcts] * 100, 1)
prev14[,cpcts] <- format( prev14[,cpcts] , nsmall=1)
