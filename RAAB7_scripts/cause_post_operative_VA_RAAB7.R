#Causes of post-operative presenting VA <6/12 in cataract operated eyes with borderline and poor outcomes 
#AH 15.10.24

causes.PO.VA<-data.frame(oc.tab$oc.levels[c(1,2)])
causes.PO.VA[,2:11] <- NA

names(causes.PO.VA) <- c("oc.levels",
                        
                        "Case.selection.n",
                        "Case.selection.pct",
                        
                        "Intraoperative.complications.n",
                        "Intraoperative.complications.pct",
                        
                        "PCO.n",
                        "PCO.pct",
                        
                        "Other.sequelae.n",
                        "Other.sequelae.pct",
                        
                        "Refractive.error.n",
                        "Refractive.error.pct")


for (i in 1:length(causes.PO.VA$oc.levels)) 
  
{
  
  causes.PO.VA$Case.selection.n[i]<-sum(raab$postop.eyes.right.denom[raab$right.oc.levels==oc.tab$right.oc.levels[i] & raab$surgery_ocular_comorbidity.re==1],na.rm=T) +sum(raab$postop.eyes.left.denom[raab$left.oc.levels==oc.tab$left.oc.levels[i] & raab$surgery_ocular_comorbidity.le==1],na.rm=T)
  causes.PO.VA$Case.selection.pct[i]<-(causes.PO.VA$Case.selection.n[i] / (sum(raab$postop.eyes.right.denom[raab$surgery_ocular_comorbidity.re==1], na.rm=T)+sum(raab$postop.eyes.left.denom[raab$surgery_ocular_comorbidity.le==1], na.rm=T)))
  
  causes.PO.VA$Intraoperative.complications.n[i]<-sum(raab$postop.eyes.right.denom[raab$right.oc.levels==oc.tab$right.oc.levels[i] & raab$surgery_op_comp.re==1],na.rm=T) +sum(raab$postop.eyes.left.denom[raab$left.oc.levels==oc.tab$left.oc.levels[i] & raab$surgery_op_comp.le==1],na.rm=T)
  causes.PO.VA$Intraoperative.complications.pct[i]<-(causes.PO.VA$Intraoperative.complications.n[i] / (sum(raab$postop.eyes.right.denom[raab$surgery_op_comp.re==1], na.rm=T)+sum(raab$postop.eyes.left.denom[raab$surgery_op_comp.le==1], na.rm=T)))
  
  causes.PO.VA$PCO.n[i]<-sum(raab$postop.eyes.right.denom[raab$right.oc.levels==oc.tab$right.oc.levels[i] & raab$surgery_PCO.re==1],na.rm=T) +sum(raab$postop.eyes.left.denom[raab$left.oc.levels==oc.tab$left.oc.levels[i] & raab$surgery_PCO.le==1],na.rm=T)
  causes.PO.VA$PCO.pct[i]<-(causes.PO.VA$PCO.n[i] / (sum(raab$postop.eyes.right.denom[raab$surgery_PCO.re==1], na.rm=T)+sum(raab$postop.eyes.left.denom[raab$surgery_PCO.le==1], na.rm=T)))
  
  causes.PO.VA$Other.sequelae.n[i]<-sum(raab$postop.eyes.right.denom[raab$right.oc.levels==oc.tab$right.oc.levels[i] & raab$surgery_other_seq.re==1],na.rm=T) +sum(raab$postop.eyes.left.denom[raab$left.oc.levels==oc.tab$left.oc.levels[i] & raab$surgery_other_seq.le==1],na.rm=T)
  causes.PO.VA$Other.sequelae.pct[i]<-(causes.PO.VA$Other.sequelae.n[i] / (sum(raab$postop.eyes.right.denom[raab$surgery_other_seq.re==1], na.rm=T)+sum(raab$postop.eyes.left.denom[raab$surgery_other_seq.le==1], na.rm=T)))
  
  causes.PO.VA$Refractive.error.n[i]<-sum(raab$postop.eyes.right.denom[raab$right.oc.levels==oc.tab$right.oc.levels[i] & raab$surgery_ref_err.re==1],na.rm=T) +sum(raab$postop.eyes.left.denom[raab$left.oc.levels==oc.tab$left.oc.levels[i] & raab$surgery_ref_err.le==1],na.rm=T)
  causes.PO.VA$Refractive.error.pct[i]<-(causes.PO.VA$Refractive.error.n[i] / (sum(raab$postop.eyes.right.denom[raab$surgery_ref_err.re==1], na.rm=T)+sum(raab$postop.eyes.left.denom[raab$surgery_ref_err.le==1], na.rm=T)))
}


causes.PO.VA[nrow(causes.PO.VA)+1,c(2,4,6,8,10)]<-colSums(causes.PO.VA[,c(2,4,6,8,10)])
causes.PO.VA[nrow(causes.PO.VA),c(3,5,7,9,11)]<-1
causes.PO.VA$oc.levels[nrow(causes.PO.VA)]<-"Total"

nt4.pcts<-grep("pct",names(causes.PO.VA))
causes.PO.VA[,nt4.pcts]<-round(causes.PO.VA[,nt4.pcts] * 100,1)
causes.PO.VA[,nt4.pcts]<-format(causes.PO.VA[,nt4.pcts], nsmall=1)
