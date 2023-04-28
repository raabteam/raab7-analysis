#RAAB5

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

barcalc<-raab[,c('surgery_none_reason_1','surgery_none_reason_2','bilateral_operable_cataract','gender')]
barcalc$surgery_none_reason<-paste0("first_",barcalc$surgery_none_reason_1," second_",barcalc$surgery_none_reason_2)

names(surgery.bars)<-"Barrier"
for (i in 1:nrow(surgery.bars))
{
  surgery.bars$total_count[i]<-sum(table(grep(surgery.bars$Barrier[i],barcalc$surgery_none_reason[barcalc$bilateral_operable_cataract==1])))
  surgery.bars$male_count[i]<-sum(table(grep(surgery.bars$Barrier[i],barcalc$surgery_none_reason[barcalc$gender=="male" & barcalc$bilateral_operable_cataract==1])))
  surgery.bars$female_count[i]<-sum(table(grep(surgery.bars$Barrier[i],barcalc$surgery_none_reason[barcalc$gender=="female" & barcalc$bilateral_operable_cataract==1])))
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
