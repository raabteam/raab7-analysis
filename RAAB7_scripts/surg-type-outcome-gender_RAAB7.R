#RAAB7

#v1 24/08/21 - IM
#v2 01/09/21 - RB
#v3 18/09/23 - IM - adds no view of lens to the type of surgery options

surgery.type <- c('surgery_type_iol', 'surgery_type_non_iol', 'surgery_type_couching', 'surgery_type_no_view') 

newtab2<-data.frame(surgery.type)
newtab2[,2:7] <- NA
names(newtab2) <- c("surgery.type",
                    
                    "female.n.eyes",
                    "female.pct.eyes",
                    
                    "male.n.eyes",
                    "male.pct.eyes",
                    
                    "total.n.eyes",
                    "total.pct.eyes"
)

for (i in 1:length(surgery.type)) 
  
{
  
  newtab2$female.n.eyes[i] <- sum(raab$right.operated.eyes.denom[raab$surgery_type_right==surgery.type[i] & raab$gender=="female"],na.rm=T) + sum(raab$left.operated.eyes.denom[raab$surgery_type_left==surgery.type[i] & raab$gender=="female"],na.rm=T)
  newtab2$female.pct.eyes[i] <- (newtab2$female.n.eyes[i] / (sum(raab$right.operated.eyes.denom[raab$gender=="female"],na.rm=T) + sum(raab$left.operated.eyes.denom[raab$gender=="female"],na.rm=T)))
  
  newtab2$male.n.eyes[i] <- sum(raab$right.operated.eyes.denom[raab$surgery_type_right==surgery.type[i] & raab$gender=="male"],na.rm=T) + sum(raab$left.operated.eyes.denom[raab$surgery_type_left==surgery.type[i] & raab$gender=="male"],na.rm=T)
  newtab2$male.pct.eyes[i] <- (newtab2$male.n.eyes[i] / (sum(raab$right.operated.eyes.denom[raab$gender=="male"],na.rm=T) + sum(raab$left.operated.eyes.denom[raab$gender=="male"],na.rm=T)))
  
  newtab2$total.n.eyes[i] <- sum(raab$right.operated.eyes.denom[raab$surgery_type_right==surgery.type[i]],na.rm=T) + sum(raab$left.operated.eyes.denom[raab$surgery_type_left==surgery.type[i]],na.rm=T)
  newtab2$total.pct.eyes[i] <- (newtab2$total.n.eyes[i] / (sum(raab$right.operated.eyes.denom,na.rm=T) + sum(raab$left.operated.eyes.denom,na.rm=T)))
  
}

newtab2[nrow(newtab2)+1,c(2,4,6)]<-colSums(newtab2[,c(2,4,6)])
newtab2[nrow(newtab2),c(3,5,7)]<-1
newtab2$surgery.type[nrow(newtab2)]<-"Total"

nt2.pcts<-grep("pct",names(newtab2))
newtab2[,nt2.pcts]<-round( newtab2[,nt2.pcts] * 100, 1)
newtab2[,nt2.pcts]<-format( newtab2[,nt2.pcts], nsmall=1)

newtab3a<-data.frame(oc.tab$oc.levels)
newtab3a[,2:7] <- NA
names(newtab3a) <- c("oc.levels",
                     
                     "female.n.eyes",
                     "female.pct.eyes",
                     
                     "male.n.eyes",
                     "male.pct.eyes",
                     
                     "total.n.eyes",
                     "total.pct.eyes"
)

for (i in 1:length(oc.tab$oc.levels)) 
  
{
  
  newtab3a$female.n.eyes[i] <- sum(raab$right.operated.eyes.denom[raab$right.oc.levels==oc.tab$right.oc.levels[i] & raab$gender=="female"],na.rm=T) + sum(raab$left.operated.eyes.denom[raab$left.oc.levels==oc.tab$left.oc.levels[i] & raab$gender=="female"],na.rm=T)
  newtab3a$female.pct.eyes[i] <- (newtab3a$female.n.eyes[i] / (sum(raab$right.operated.eyes.denom[raab$gender=="female"],na.rm=T) + sum(raab$left.operated.eyes.denom[raab$gender=="female"],na.rm=T)))
  
  newtab3a$male.n.eyes[i] <- sum(raab$right.operated.eyes.denom[raab$right.oc.levels==oc.tab$right.oc.levels[i] & raab$gender=="male"],na.rm=T) + sum(raab$left.operated.eyes.denom[raab$left.oc.levels==oc.tab$left.oc.levels[i] & raab$gender=="male"],na.rm=T)
  newtab3a$male.pct.eyes[i] <- (newtab3a$male.n.eyes[i] / (sum(raab$right.operated.eyes.denom[raab$gender=="male"],na.rm=T) + sum(raab$left.operated.eyes.denom[raab$gender=="male"],na.rm=T)))
  
  newtab3a$total.n.eyes[i] <- sum(raab$right.operated.eyes.denom[raab$right.oc.levels==oc.tab$right.oc.levels[i]],na.rm=T) + sum(raab$left.operated.eyes.denom[raab$left.oc.levels==oc.tab$left.oc.levels[i]],na.rm=T)
  newtab3a$total.pct.eyes[i] <- (newtab3a$total.n.eyes[i] / (sum(raab$right.operated.eyes.denom,na.rm=T) + sum(raab$left.operated.eyes.denom,na.rm=T)))
  
}

newtab3a[nrow(newtab3a)+1,c(2,4,6)]<-colSums(newtab3a[,c(2,4,6)])
newtab3a[nrow(newtab3a),c(3,5,7)]<-1
newtab3a$oc.levels[nrow(newtab3a)]<-"Total"

nt3a.pcts<-grep("pct",names(newtab3a))
newtab3a[,nt3a.pcts]<-round( newtab3a[,nt3a.pcts] * 100,1)
newtab3a[,nt3a.pcts]<-format( newtab3a[,nt3a.pcts], nsmall=1)

newtab3b<-data.frame(oc.tab$oc.levels)
newtab3b[,2:7] <- NA
names(newtab3b) <- c("oc.levels",
                     
                     "female.n.eyes",
                     "female.pct.eyes",
                     
                     "male.n.eyes",
                     "male.pct.eyes",
                     
                     "total.n.eyes",
                     "total.pct.eyes"
)

for (i in 1:length(oc.tab$oc.levels)) 
  
{
  
  newtab3b$female.n.eyes[i] <- sum(raab$right.operated.eyes.denom[raab$right.oc.pinva.levels==oc.tab$right.oc.pinva.levels[i] & raab$gender=="female"],na.rm=T) + sum(raab$left.operated.eyes.denom[raab$left.oc.pinva.levels==oc.tab$left.oc.pinva.levels[i] & raab$gender=="female"],na.rm=T)
  newtab3b$female.pct.eyes[i] <- (newtab3b$female.n.eyes[i] / (sum(raab$right.operated.eyes.denom[raab$gender=="female"],na.rm=T) + sum(raab$left.operated.eyes.denom[raab$gender=="female"],na.rm=T)))
  
  newtab3b$male.n.eyes[i] <- sum(raab$right.operated.eyes.denom[raab$right.oc.pinva.levels==oc.tab$right.oc.pinva.levels[i] & raab$gender=="male"],na.rm=T) + sum(raab$left.operated.eyes.denom[raab$left.oc.pinva.levels==oc.tab$left.oc.pinva.levels[i] & raab$gender=="male"],na.rm=T)
  newtab3b$male.pct.eyes[i] <- (newtab3b$male.n.eyes[i] / (sum(raab$right.operated.eyes.denom[raab$gender=="male"],na.rm=T) + sum(raab$left.operated.eyes.denom[raab$gender=="male"],na.rm=T)))
  
  newtab3b$total.n.eyes[i] <- sum(raab$right.operated.eyes.denom[raab$right.oc.pinva.levels==oc.tab$right.oc.pinva.levels[i]],na.rm=T) + sum(raab$left.operated.eyes.denom[raab$left.oc.pinva.levels==oc.tab$left.oc.pinva.levels[i]],na.rm=T)
  newtab3b$total.pct.eyes[i] <- (newtab3b$total.n.eyes[i] / (sum(raab$right.operated.eyes.denom,na.rm=T) + sum(raab$left.operated.eyes.denom,na.rm=T)))
  
}

newtab3b[nrow(newtab3b)+1,c(2,4,6)]<-colSums(newtab3b[,c(2,4,6)])
newtab3b[nrow(newtab3b),c(3,5,7)]<-1
newtab3b$oc.levels[nrow(newtab3b)]<-"Total"

nt3b.pcts<-grep("pct",names(newtab3b))
newtab3b[,nt3b.pcts]<-round( newtab3b[,nt3b.pcts] * 100, 1)
newtab3b[,nt3b.pcts]<-format( newtab3b[,nt3b.pcts], nsmall=1)