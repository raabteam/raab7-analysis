#RAAB7

#v1 - 16/09/21 - RB
#v2 - 08/10/21 - RB
#v3 - 29/10/21 - RB
#v4 - 29/07/22 - RB

surgery.bars<-as.data.frame(c(
  "surgery_none_reason_unaware",
  "surgery_none_reason_denied",
  "surgery_none_reason_access",
  "surgery_none_reason_cost",
  "surgery_none_reason_unnecessary",
  "surgery_none_reason_fear",
  "surgery_none_reason_other"))

names(surgery.bars)<-"Barrier"
for (i in 1:nrow(surgery.bars))
{
  surgery.bars$total_count[i]<-sum(table(grep(surgery.bars$Barrier[i],raab$surgery_none_reason[raab$bilateral_operable_cataract==1])))
  surgery.bars$male_count[i]<-sum(table(grep(surgery.bars$Barrier[i],raab$surgery_none_reason[raab$gender=="male" & raab$bilateral_operable_cataract==1])))
  surgery.bars$female_count[i]<-sum(table(grep(surgery.bars$Barrier[i],raab$surgery_none_reason[raab$gender=="female" & raab$bilateral_operable_cataract==1])))
}

surgery.bars$total_percent<-round((surgery.bars$total_count/sum(surgery.bars$total_count,na.rm=T)*100),1)
surgery.bars$male_percent<-round((surgery.bars$male_count/sum(surgery.bars$male_count,na.rm=T)*100),1)
surgery.bars$female_percent<-round((surgery.bars$female_count/sum(surgery.bars$female_count,na.rm=T)*100),1)


surgery.bars[nrow(surgery.bars)+1,]<-NA
surgery.bars[nrow(surgery.bars),1]<-"Total"

surgery.bars$Barrier<-recode_factor(surgery.bars$Barrier,surgery_none_reason_access = "Cannot access surgery", surgery_none_reason_cost = "Cost", surgery_none_reason_denied = "Surgery denied by provider", surgery_none_reason_fear = "Fear", surgery_none_reason_other = "Other", surgery_none_reason_unaware = "Unaware treatment possible", surgery_none_reason_unnecessary = "Felt not needed", Total = "Total")    

sbcnts<-grep("count",names(surgery.bars))
sbpcts<-grep("percent",names(surgery.bars))
surgery.bars[nrow(surgery.bars),sbcnts]<-colSums(surgery.bars[,sbcnts],na.rm=T)
surgery.bars[nrow(surgery.bars),sbpcts]<-100.0


##### RAAB5/6 format - separate reason1/reason2 columns #####


#surgery.bars1.total<-data.frame(table(raab$bilateral_operable_cataract,raab$surgery_none_reason))
#surgery.bars1.cat.total<-surgery.bars1.total[surgery.bars1.total$Var1==1,c("Var2","Freq")]
#names(surgery.bars1.cat.total)<-c("Barrier","total_count")

#surgery.bars1.male<-data.frame(table(raab$bilateral_operable_cataract[raab$gender=="male"],raab$surgery_none_reason[raab$gender=="male"]))
#surgery.bars1.cat.male<-surgery.bars1.male[surgery.bars1.male$Var1==1,c("Var2","Freq")]
#names(surgery.bars1.cat.male)<-c("Barrier","male_count")


#surgery.bars1.female<-data.frame(table(raab$bilateral_operable_cataract[raab$gender=="female"],raab$surgery_none_reason[raab$gender=="female"]))
#surgery.bars1.cat.female<-surgery.bars1.female[surgery.bars1.female$Var1==1,c("Var2","Freq")]
#names(surgery.bars1.cat.female)<-c("Barrier","female_count")
    
#if(all(is.na(raab$surgery_none_reason2))) {
#    surgery.bars12.total<-surgery.bars1.cat.total
#  } else {
#    surgery.bars2.total<-data.frame(table(raab$bilateral_operable_cataract,raab$surgery_none_reason2))
#    surgery.bars2.cat.total<-surgery.bars2.total[surgery.bars2.total$Var1==1,c("Var2","Freq")]
#    names(surgery.bars2.cat.total)<-c("Barrier","total_count")
#    surgery.bars12.total<-rbind(surgery.bars1.cat.total,surgery.bars2.cat.total)
#  }

#if(all(is.na(raab$surgery_none_reason2[raab$gender=="female"]))) {
#  surgery.bars12.female<-surgery.bars1.cat.female
#} else {
#  surgery.bars2.female<-data.frame(table(raab$bilateral_operable_cataract[raab$gender=="female"],raab$surgery_none_reason2[raab$gender=="female"]))
#  surgery.bars2.cat.female<-surgery.bars2.female[surgery.bars2.female$Var1==1,c("Var2","Freq")]
#  names(surgery.bars2.cat.female)<-c("Barrier","female_count")
#  surgery.bars12.female<-rbind(surgery.bars1.cat.female,surgery.bars2.cat.female)
#}

#if(all(is.na(raab$surgery_none_reason2[raab$gender=="male"]))) {
#  surgery.bars12.male<-surgery.bars1.cat.male
#} else {
#  surgery.bars2.male<-data.frame(table(raab$bilateral_operable_cataract[raab$gender=="male"],raab$surgery_none_reason2[raab$gender=="male"]))
#  surgery.bars2.cat.male<-surgery.bars2.male[surgery.bars2.male$Var1==1,c("Var2","Freq")]
#  names(surgery.bars2.cat.male)<-c("Barrier","male_count")
#  surgery.bars12.male<-rbind(surgery.bars1.cat.male,surgery.bars2.cat.male)
#}

#surgery.bars12.female<-surgery.bars12.male
#names(surgery.bars12.female)[2]<-"female_count"
#surgery.bars12.female$female_count<-0

#if(nrow(surgery.bars1.total[surgery.bars1.total$Var1==1,])==0) {
#    surgery.bars.cat<-data.frame(Barrier=character(),female_count=numeric(),	female_percent=numeric(),	male_count=numeric(),	male_percent=numeric(),	total_count=numeric(),	total_percent=numeric())
#    surgery.bars.cat[1,]<-NA
#    surgery.bars.cat$Barrier[1]<-"Not applicable"
#    surgery.bars.cat[1,2:7]<-"-"

#} else {

#    surgery.bars.cat.total<-aggregate(surgery.bars12.total$total_count,by=list(surgery.bars12.total$Barrier),FUN=sum,na.rm=T)
#    names(surgery.bars.cat.total)<-c("Barrier","total_count")
#    surgery.bars.cat.total$total_percent<-round((surgery.bars.cat.total$total_count/sum(surgery.bars.cat.total$total_count,na.rm=T)*100),1)

#    surgery.bars.cat.female<-aggregate(surgery.bars12.female$female_count,by=list(surgery.bars12.female$Barrier),FUN=sum,na.rm=T)
#    names(surgery.bars.cat.female)<-c("Barrier","female_count")
#    surgery.bars.cat.female$female_percent<-round((surgery.bars.cat.female$female_count/sum(surgery.bars.cat.female$female_count,na.rm=T)*100),1)

#    surgery.bars.cat.male<-aggregate(surgery.bars12.male$male_count,by=list(surgery.bars12.male$Barrier),FUN=sum,na.rm=T)
#    names(surgery.bars.cat.male)<-c("Barrier","male_count")
#    surgery.bars.cat.male$male_percent<-round((surgery.bars.cat.male$male_count/sum(surgery.bars.cat.male$male_count,na.rm=T)*100),1)
    
#    surgery.bars.cat.female.male<-merge(surgery.bars.cat.female,surgery.bars.cat.male,by="Barrier",all=T)
#    surgery.bars.cat<-merge(surgery.bars.cat.female.male,surgery.bars.cat.total,all=T)
    
#    surgery.bars.cat$Barrier<-recode_factor(surgery.bars.cat.total$Barrier,surgery_none_reason_access = "Access", surgery_none_reason_cost = "Cost", surgery_none_reason_denied = "Denied", surgery_none_reason_fear = "Fear", surgery_none_reason_other = "Other", surgery_none_reason_unaware = "Unaware", surgery_none_reason_unnecessary = "Felt not needed")    
#    surgery.bars.cat[is.na(surgery.bars.cat)]<-0
    
#    surgery.bars.cat[nrow(surgery.bars.cat)+1,]<-NA
#    sbcnts<-grep("count",names(surgery.bars.cat))
#    sbpcts<-grep("percent",names(surgery.bars.cat))
#    surgery.bars.cat[nrow(surgery.bars.cat),sbcnts]<-colSums(surgery.bars.cat[,sbcnts],na.rm=T)
#    surgery.bars.cat[nrow(surgery.bars.cat),sbpcts]<-100.0
    
#    surgery.bars.cat$Barrier<-as.character(surgery.bars.cat$Barrier)
#    surgery.bars.cat[nrow(surgery.bars.cat),1]<-"Total"
    
#    surgery.bars.cat[,sbpcts] <- format( surgery.bars.cat[,sbpcts], nsmall=1 )
    
#  }

