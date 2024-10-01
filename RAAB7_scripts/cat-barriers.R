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

barcalc<-raab[,c('surgery_none_reason_1','surgery_none_reason_2','bilateral_operable_cataract','gender')]
barcalc$surgery_none_reason<-paste0("first_",barcalc$surgery_none_reason_1," second_",barcalc$surgery_none_reason_2)

names(surgery.bars)<-"Barrier"
for (i in 1:nrow(surgery.bars))
{
  surgery.bars$total_n[i]<-sum(table(grep(surgery.bars$Barrier[i],barcalc$surgery_none_reason[barcalc$bilateral_operable_cataract==1])))
  surgery.bars$male_n[i]<-sum(table(grep(surgery.bars$Barrier[i],barcalc$surgery_none_reason[barcalc$gender=="male" & barcalc$bilateral_operable_cataract==1])))
  surgery.bars$female_n[i]<-sum(table(grep(surgery.bars$Barrier[i],barcalc$surgery_none_reason[barcalc$gender=="female" & barcalc$bilateral_operable_cataract==1])))
}

surgery.bars$total_pct<-surgery.bars$total_n/sum(surgery.bars$total_n,na.rm=T)
surgery.bars$male_pct<-surgery.bars$male_n/sum(surgery.bars$male_n,na.rm=T)
surgery.bars$female_pct<-surgery.bars$female_n/sum(surgery.bars$female_n,na.rm=T)

surgery.bars[nrow(surgery.bars)+1,c(2:4)]<-colSums(surgery.bars[,c(2:4)])
surgery.bars[nrow(surgery.bars),c(5:7)]<-1
surgery.bars$Barrier[nrow(surgery.bars)]<-"Total"

pcts <- grep("pct",names(surgery.bars))
surgery.bars[,pcts]<-round( surgery.bars[,pcts] * 100, 1 )
surgery.bars[,pcts]<-format( surgery.bars[,pcts], nsmall=1 )
