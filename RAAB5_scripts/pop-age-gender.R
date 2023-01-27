#RAAB5

asa2<-data.frame(age.groups.tens)
asa2[,2:7] <- NA
names(asa2) <- c("age.groups.tens",
                             
                 "female.n",
                 "female.pct",
                         
                 "male.n",
                 "male.pct",
                             
                 "total.n",
                 "total.pct"
                )

mag.subpop<-aggregate(male.subpop$population,by=list(male.subpop$age.groups.tens),FUN=sum)
names(mag.subpop)<-c("age.groups.tens","n")  

fag.subpop<-aggregate(female.subpop$population,by=list(female.subpop$age.groups.tens),FUN=sum)
names(fag.subpop)<-c("age.groups.tens","n")  

asa2$male.n<-mag.subpop$n
asa2$female.n<-fag.subpop$n
asa2$total.n<-asa2$male.n+asa2$female.n

asa2$female.pct<-asa2$female.n/sum(asa2$female.n,na.rm=T)
asa2$male.pct<-asa2$male.n/sum(asa2$male.n,na.rm=T)
asa2$total.pct<-asa2$total.n/sum(asa2$total.n,na.rm=T)

asa2[nrow(asa2)+1,2:7]<-colSums(asa2[,2:7])
asa2[5,1]<-"Total"

pcts <- grep("pct",names(asa2))
asa2[,pcts] <- round( asa2[,pcts]*100, 1)

cnts<-grep("n",names(asa2))
asa2[,cnts]<-format( asa2[,cnts], digits=1, scientific=F )
