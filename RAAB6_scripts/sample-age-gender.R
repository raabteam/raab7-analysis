#RAAB6

asa1<-data.frame(age.groups.tens)
asa1[,2:13] <- NA
names(asa1) <- c("age.groups.tens",
                 
                 "female.examined.n",
                 "female.examined.pct",
                 "female.nonresponse.n",
                 "female.nonresponse.pct",
                 
                 "male.examined.n",
                 "male.examined.pct",
                 "male.nonresponse.n",
                 "male.nonresponse.pct",
                 
                 "total.examined.n",
                 "total.examined.pct",
                 "total.nonresponse.n",
                 "total.nonresponse.pct")

for (i in 1:length(age.groups.tens)) {
  
  asa1$female.examined.n[i] <- sum(raab$exam_status[raab$gender=="female" & raab$age.groups.tens==age.groups.tens[i]]=="exam_status_examined",na.rm=T) 
  asa1$female.examined.pct[i] <- sum(raab$exam_status[raab$gender=="female" & raab$age.groups.tens==age.groups.tens[i]]=="exam_status_examined",na.rm=T) / sum(raab$exam_status[raab$gender=="female"]=="exam_status_examined",na.rm=T) 
  
  asa1$male.examined.n[i] <- sum(raab$exam_status[raab$gender=="male" & raab$age.groups.tens==age.groups.tens[i]]=="exam_status_examined",na.rm=T) 
  asa1$male.examined.pct[i] <- sum(raab$exam_status[raab$gender=="male" & raab$age.groups.tens==age.groups.tens[i]]=="exam_status_examined",na.rm=T) / sum(raab$exam_status[raab$gender=="male"]=="exam_status_examined",na.rm=T)
  
  asa1$total.examined.n[i] <- sum(raab$exam_status[raab$age.groups.tens==age.groups.tens[i]]=="exam_status_examined",na.rm=T) 
  asa1$total.examined.pct[i] <- sum(raab$exam_status[raab$age.groups.tens==age.groups.tens[i]]=="exam_status_examined",na.rm=T) / sum(raab$exam_status=="exam_status_examined",na.rm=T)

  asa1$female.nonresponse.n[i] <- sum(raab$exam_status[raab$gender=="female" & raab$age.groups.tens==age.groups.tens[i]]!="exam_status_examined",na.rm=T) 
  asa1$female.nonresponse.pct[i] <- sum(raab$exam_status[raab$gender=="female" & raab$age.groups.tens==age.groups.tens[i]]!="exam_status_examined") / length(raab$exam_status[raab$gender=="female"])
  
  asa1$male.nonresponse.n[i] <- sum(raab$exam_status[raab$gender=="male" & raab$age.groups.tens==age.groups.tens[i]]!="exam_status_examined",na.rm=T) 
  asa1$male.nonresponse.pct[i] <- sum(raab$exam_status[raab$gender=="male" & raab$age.groups.tens==age.groups.tens[i]]!="exam_status_examined",na.rm=T) / length(raab$exam_status[raab$gender=="male"])
  
  asa1$total.nonresponse.n[i] <- sum(raab$exam_status[raab$age.groups.tens==age.groups.tens[i]]!="exam_status_examined",na.rm=T) 
  asa1$total.nonresponse.pct[i] <- sum(raab$exam_status[raab$age.groups.tens==age.groups.tens[i]]!="exam_status_examined",na.rm=T) / length(raab$exam_status)
  
}

#Add totals row to bottom of table (for total count of female, male, all)

asa1[nrow(asa1)+1,2:13]<-colSums(asa1[,2:13])
asa1[5,1]<-"Total"

pcts <- grep("pct",names(asa1))
asa1[,pcts] <- round( asa1[,pcts] * 100, 1)

cnts<-grep("\\.n",names(asa1))
asa1[,cnts]<-format( asa1[,cnts], digits=1, scientific=F )
