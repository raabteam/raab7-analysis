#RAAB5

#v1 - 16/09/21 - RB
#v2 - 08/10/21 - RB
#v3 - 29/10/21 - RB

surgery.bars1.total<-data.frame(table(raab$bilateral_operable_cataract,raab$surgery_none_reason))
surgery.bars1.cat.total<-surgery.bars1.total[surgery.bars1.total$Var1==1,c("Var2","Freq")]
names(surgery.bars1.cat.total)<-c("Barrier","total_count")

surgery.bars1.male<-data.frame(table(raab$bilateral_operable_cataract[raab$gender=="male"],raab$surgery_none_reason[raab$gender=="male"]))
surgery.bars1.cat.male<-surgery.bars1.male[surgery.bars.male$Var1==1,c("Var2","Freq")]
names(surgery.bars1.cat.male)<-c("Barrier","male_count")

surgery.bars1.female<-data.frame(table(raab$bilateral_operable_cataract[raab$gender=="female"],raab$surgery_none_reason[raab$gender=="female"]))
surgery.bars1.cat.female<-surgery.bars1.female[surgery.bars.female$Var1==1,c("Var2","Freq")]
names(surgery.bars1.cat.female)<-c("Barrier","female_count")
    
if(all(is.na(raab$surgery_none_reason2))) {
    surgery.bars12.total<-surgery.bars1.cat.total
  } else {
    surgery.bars2.total<-data.frame(table(raab$bilateral_operable_cataract,raab$surgery_none_reason2))
    surgery.bars2.cat.total<-surgery.bars2.total[surgery.bars2.total$Var1==1,c("Var2","Freq")]
    names(surgery.bars2.cat.total)<-c("Barrier","total_count")
    surgery.bars12.total<-rbind(surgery.bars1.cat.total,surgery.bars2.cat.total)
  }

if(all(is.na(raab$surgery_none_reason2[raab$gender=="female"]))) {
  surgery.bars12.female<-surgery.bars1.cat.female
} else {
  surgery.bars2.female<-data.frame(table(raab$bilateral_operable_cataract[raab$gender=="female"],raab$surgery_none_reason2[raab$gender=="female"]))
  surgery.bars2.cat.female<-surgery.bars2.female[surgery.bars2.female$Var1==1,c("Var2","Freq")]
  names(surgery.bars2.cat.female)<-c("Barrier","female_count")
  surgery.bars12.female<-rbind(surgery.bars1.cat.female,surgery.bars2.cat.female)
}

if(all(is.na(raab$surgery_none_reason2[raab$gender=="male"]))) {
  surgery.bars12.male<-surgery.bars1.cat.male
} else {
  surgery.bars2.male<-data.frame(table(raab$bilateral_operable_cataract[raab$gender=="male"],raab$surgery_none_reason2[raab$gender=="male"]))
  surgery.bars2.cat.male<-surgery.bars2.male[surgery.bars2.male$Var1==1,c("Var2","Freq")]
  names(surgery.bars2.cat.male)<-c("Barrier","male_count")
  surgery.bars12.male<-rbind(surgery.bars1.cat.male,surgery.bars2.cat.male)
}


if(nrow(surgery.bars1.total[surgery.bars1.total$Var1==1,])==0) {
    surgery.bars.cat<-data.frame(Barrier=character(),female_count=numeric(),	female_percent=numeric(),	male_count=numeric(),	male_percent=numeric(),	total_count=numeric(),	total_percent=numeric())
    surgery.bars.cat[1,]<-NA
    surgery.bars.cat$Barrier[1]<-"Not applicable"
    surgery.bars.cat[1,2:7]<-"-"
  } else {
    surgery.bars.cat.total<-aggregate(surgery.bars12.total$total_count,by=list(surgery.bars12.total$Barrier),FUN=sum,na.rm=T)
    names(surgery.bars.cat.total)<-c("Barrier","total_count")
    surgery.bars.cat.total$total_percent<-round((surgery.bars.cat.total$total_count/sum(surgery.bars.cat.total$total_count,na.rm=T)*100),1)

    surgery.bars.cat.female<-aggregate(surgery.bars12.female$female_count,by=list(surgery.bars12.female$Barrier),FUN=sum,na.rm=T)
    names(surgery.bars.cat.female)<-c("Barrier","female_count")
    surgery.bars.cat.female$female_percent<-round((surgery.bars.cat.female$female_count/sum(surgery.bars.cat.female$female_count,na.rm=T)*100),1)

    surgery.bars.cat.male<-aggregate(surgery.bars12.male$male_count,by=list(surgery.bars12.male$Barrier),FUN=sum,na.rm=T)
    names(surgery.bars.cat.male)<-c("Barrier","male_count")
    surgery.bars.cat.male$male_percent<-round((surgery.bars.cat.male$male_count/sum(surgery.bars.cat.male$male_count,na.rm=T)*100),1)
    
    surgery.bars.cat.female.male<-merge(surgery.bars.cat.female,surgery.bars.cat.male,by="Barrier",all=T)
    surgery.bars.cat<-merge(surgery.bars.cat.female.male,surgery.bars.cat.total,all=T)
    
    surgery.bars.cat$Barrier<-recode_factor(surgery.bars.cat.total$Barrier,surgery_none_reason_access = "Access", surgery_none_reason_cost = "Cost", surgery_none_reason_denied = "Denied", surgery_none_reason_fear = "Fear", surgery_none_reason_other = "Other", surgery_none_reason_unaware = "Unaware", surgery_none_reason_unnecessary = "Felt not needed")    
    surgery.bars.cat[is.na(surgery.bars.cat)]<-0
    
    surgery.bars.cat[nrow(surgery.bars.cat)+1,]<-NA
    sbcnts<-grep("count",names(surgery.bars.cat))
    sbpcts<-grep("percent",names(surgery.bars.cat))
    surgery.bars.cat[nrow(surgery.bars.cat),sbcnts]<-colSums(surgery.bars.cat[,sbcnts],na.rm=T)
    surgery.bars.cat[nrow(surgery.bars.cat),sbpcts]<-100.0
    
    surgery.bars.cat$Barrier<-as.character(surgery.bars.cat$Barrier)
    surgery.bars.cat[nrow(surgery.bars.cat),1]<-"Total"
    
  }
