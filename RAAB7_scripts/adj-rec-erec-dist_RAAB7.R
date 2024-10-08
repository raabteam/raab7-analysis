#RAAB7

#v1 03/08/21 - IM
#v2 01/09/21 - RB
#v3 17/01/22 - IM

# eREC for distance: Crude calculation using the gold standard definition

# aa = Individuals who present with spectacles or contact lenses for distance and whose UCVA is <6/12 in the better eye and CVA is 6/12 in the better eye (Met Need);
# bb = Individuals who present with spectacles or contact lenses for distance and whose UCVA is <6/12 in the better eye and whose CVA is <6/12 in the better eye, but who improve to 6/12 on PinVA (Undermet Need);
# cc = Individuals who present without spectacles and whose UCVA is <6/12 in the better eye and whose PinVA is 6/12 in the better eye (Unmet Need)
# dd = Individuals who are not aa, bb or cc cases (no need)

raab$erec_dist_num <-(raab$aa_case==1)+0
raab$rec_dist_num <-(raab$aa_case==1 | raab$bb_case==1)+0
raab$erec_rec_dist_denom <-(raab$aa_case==1 | raab$bb_case==1 | raab$cc_case==1)+0

erec_output <- c("dist_erec","dist_rec")

newtab5<-data.frame(erec_output)
newtab5[,2:19] <- NA
names(newtab5) <- c("rec_metric",
                    
                    "female.pct",
                    "female.pct.lci",
                    "female.pct.uci",
                    "female.adj.pct",
                    "female.adj.pct.lci",
                    "female.adj.pct.uci",
                    
                    "male.pct",
                    "male.pct.lci",
                    "male.pct.uci",
                    "male.adj.pct",
                    "male.adj.pct.lci",
                    "male.adj.pct.uci",
                    
                    "total.pct",
                    "total.pct.lci",
                    "total.pct.uci",
                    "total.adj.pct",
                    "total.adj.pct.lci",
                    "total.adj.pct.uci")

#eREC and REC crude proportion estimates

newtab5$female.pct[newtab5$rec_metric=="dist_erec"] <- sum(raab$erec_dist_num[raab$gender=="female"],na.rm=T)/sum(raab$erec_rec_dist_denom[raab$gender=="female"],na.rm=T)
newtab5$male.pct[newtab5$rec_metric=="dist_erec"] <- sum(raab$erec_dist_num[raab$gender=="male"],na.rm=T)/sum(raab$erec_rec_dist_denom[raab$gender=="male"],na.rm=T)
newtab5$total.pct[newtab5$rec_metric=="dist_erec"] <- sum(raab$erec_dist_num,na.rm=T)/sum(raab$erec_rec_dist_denom,na.rm=T)

newtab5$female.pct[newtab5$rec_metric=="dist_rec"] <- sum(raab$rec_dist_num[raab$gender=="female"],na.rm=T)/sum(raab$erec_rec_dist_denom[raab$gender=="female"],na.rm=T)
newtab5$male.pct[newtab5$rec_metric=="dist_rec"] <- sum(raab$rec_dist_num[raab$gender=="male"],na.rm=T)/sum(raab$erec_rec_dist_denom[raab$gender=="male"],na.rm=T)
newtab5$total.pct[newtab5$rec_metric=="dist_rec"] <- sum(raab$rec_dist_num,na.rm=T)/sum(raab$erec_rec_dist_denom,na.rm=T)

newtab5$female.pct.lci[newtab5$rec_metric=="dist_erec"] <- bennett.lci(newtab5$female.pct[newtab5$rec_metric=="dist_erec"],raab$erec_dist_num[raab$gender=="female"],raab$erec_rec_dist_denom[raab$gender=="female"],raab$clusterId[raab$gender=="female"])
newtab5$male.pct.lci[newtab5$rec_metric=="dist_erec"] <- bennett.lci(newtab5$male.pct[newtab5$rec_metric=="dist_erec"],raab$erec_dist_num[raab$gender=="male"],raab$erec_rec_dist_denom[raab$gender=="male"],raab$clusterId[raab$gender=="male"])
newtab5$total.pct.lci[newtab5$rec_metric=="dist_erec"] <- bennett.lci(newtab5$total.pct[newtab5$rec_metric=="dist_erec"],raab$erec_dist_num,raab$erec_rec_dist_denom,raab$clusterId)

newtab5$female.pct.uci[newtab5$rec_metric=="dist_erec"] <- bennett.uci(newtab5$female.pct[newtab5$rec_metric=="dist_erec"],raab$erec_dist_num[raab$gender=="female"],raab$erec_rec_dist_denom[raab$gender=="female"],raab$clusterId[raab$gender=="female"])
newtab5$male.pct.uci[newtab5$rec_metric=="dist_erec"] <- bennett.uci(newtab5$male.pct[newtab5$rec_metric=="dist_erec"],raab$erec_dist_num[raab$gender=="male"],raab$erec_rec_dist_denom[raab$gender=="male"],raab$clusterId[raab$gender=="male"])
newtab5$total.pct.uci[newtab5$rec_metric=="dist_erec"] <- bennett.uci(newtab5$total.pct[newtab5$rec_metric=="dist_erec"],raab$erec_dist_num,raab$erec_rec_dist_denom,raab$clusterId)

newtab5$female.pct.lci[newtab5$rec_metric=="dist_rec"] <- bennett.lci(newtab5$female.pct[newtab5$rec_metric=="dist_rec"],raab$rec_dist_num[raab$gender=="female"],raab$erec_rec_dist_denom[raab$gender=="female"],raab$clusterId[raab$gender=="female"])
newtab5$male.pct.lci[newtab5$rec_metric=="dist_rec"] <- bennett.lci(newtab5$male.pct[newtab5$rec_metric=="dist_rec"],raab$rec_dist_num[raab$gender=="male"],raab$erec_rec_dist_denom[raab$gender=="male"],raab$clusterId[raab$gender=="male"])
newtab5$total.pct.lci[newtab5$rec_metric=="dist_rec"] <- bennett.lci(newtab5$total.pct[newtab5$rec_metric=="dist_rec"],raab$rec_dist_num,raab$erec_rec_dist_denom,raab$clusterId)

newtab5$female.pct.uci[newtab5$rec_metric=="dist_rec"] <- bennett.uci(newtab5$female.pct[newtab5$rec_metric=="dist_rec"],raab$rec_dist_num[raab$gender=="female"],raab$erec_rec_dist_denom[raab$gender=="female"],raab$clusterId[raab$gender=="female"])
newtab5$male.pct.uci[newtab5$rec_metric=="dist_rec"] <- bennett.uci(newtab5$male.pct[newtab5$rec_metric=="dist_rec"],raab$rec_dist_num[raab$gender=="male"],raab$erec_rec_dist_denom[raab$gender=="male"],raab$clusterId[raab$gender=="male"])
newtab5$total.pct.uci[newtab5$rec_metric=="dist_rec"] <- bennett.uci(newtab5$total.pct[newtab5$rec_metric=="dist_rec"],raab$rec_dist_num,raab$erec_rec_dist_denom,raab$clusterId)

#eREC and REC adjusted proportion estimates

newtab5$female.adj.pct[newtab5$rec_metric=="dist_erec"] <- prop.age.adjust(female.subpop,raab[raab$gender=="female",],raab$erec_dist_num[raab$gender=="female"],raab$erec_rec_dist_denom[raab$gender=="female"])
newtab5$male.adj.pct[newtab5$rec_metric=="dist_erec"] <- prop.age.adjust(male.subpop,raab[raab$gender=="male",], raab$erec_dist_num[raab$gender=="male"], raab$erec_rec_dist_denom[raab$gender=="male"])
newtab5$total.adj.pct[newtab5$rec_metric=="dist_erec"] <- prop.age.sex.adjust(popfives, raab, raab$erec_dist_num, raab$erec_rec_dist_denom)

newtab5$female.adj.pct[newtab5$rec_metric=="dist_rec"] <- prop.age.adjust(female.subpop,raab[raab$gender=="female",],raab$rec_dist_num[raab$gender=="female"],raab$erec_rec_dist_denom[raab$gender=="female"])
newtab5$male.adj.pct[newtab5$rec_metric=="dist_rec"] <- prop.age.adjust(male.subpop,raab[raab$gender=="male",], raab$rec_dist_num[raab$gender=="male"], raab$erec_rec_dist_denom[raab$gender=="male"])
newtab5$total.adj.pct[newtab5$rec_metric=="dist_rec"] <- prop.age.sex.adjust(popfives, raab, raab$rec_dist_num, raab$erec_rec_dist_denom)

newtab5$female.adj.pct.lci[newtab5$rec_metric=="dist_erec"] <- bennett.lci(newtab5$female.adj.pct[newtab5$rec_metric=="dist_erec"],raab$erec_dist_num[raab$gender=="female"],raab$erec_rec_dist_denom[raab$gender=="female"],raab$clusterId[raab$gender=="female"])
newtab5$male.adj.pct.lci[newtab5$rec_metric=="dist_erec"] <- bennett.lci(newtab5$male.adj.pct[newtab5$rec_metric=="dist_erec"],raab$erec_dist_num[raab$gender=="male"],raab$erec_rec_dist_denom[raab$gender=="male"],raab$clusterId[raab$gender=="male"])
newtab5$total.adj.pct.lci[newtab5$rec_metric=="dist_erec"] <- bennett.lci(newtab5$total.adj.pct[newtab5$rec_metric=="dist_erec"],raab$erec_dist_num,raab$erec_rec_dist_denom,raab$clusterId)

newtab5$female.adj.pct.uci[newtab5$rec_metric=="dist_erec"] <- bennett.uci(newtab5$female.adj.pct[newtab5$rec_metric=="dist_erec"],raab$erec_dist_num[raab$gender=="female"],raab$erec_rec_dist_denom[raab$gender=="female"],raab$clusterId[raab$gender=="female"])
newtab5$male.adj.pct.uci[newtab5$rec_metric=="dist_erec"] <- bennett.uci(newtab5$male.adj.pct[newtab5$rec_metric=="dist_erec"],raab$erec_dist_num[raab$gender=="male"],raab$erec_rec_dist_denom[raab$gender=="male"],raab$clusterId[raab$gender=="male"])
newtab5$total.adj.pct.uci[newtab5$rec_metric=="dist_erec"] <- bennett.uci(newtab5$total.adj.pct[newtab5$rec_metric=="dist_erec"],raab$erec_dist_num,raab$erec_rec_dist_denom,raab$clusterId)

newtab5$female.adj.pct.lci[newtab5$rec_metric=="dist_rec"] <- bennett.lci(newtab5$female.adj.pct[newtab5$rec_metric=="dist_rec"],raab$rec_dist_num[raab$gender=="female"],raab$erec_rec_dist_denom[raab$gender=="female"],raab$clusterId[raab$gender=="female"])
newtab5$male.adj.pct.lci[newtab5$rec_metric=="dist_rec"] <- bennett.lci(newtab5$male.adj.pct[newtab5$rec_metric=="dist_rec"],raab$rec_dist_num[raab$gender=="male"],raab$erec_rec_dist_denom[raab$gender=="male"],raab$clusterId[raab$gender=="male"])
newtab5$total.adj.pct.lci[newtab5$rec_metric=="dist_rec"] <- bennett.lci(newtab5$total.adj.pct[newtab5$rec_metric=="dist_rec"],raab$rec_dist_num,raab$erec_rec_dist_denom,raab$clusterId)

newtab5$female.adj.pct.uci[newtab5$rec_metric=="dist_rec"] <- bennett.uci(newtab5$female.adj.pct[newtab5$rec_metric=="dist_rec"],raab$rec_dist_num[raab$gender=="female"],raab$erec_rec_dist_denom[raab$gender=="female"],raab$clusterId[raab$gender=="female"])
newtab5$male.adj.pct.uci[newtab5$rec_metric=="dist_rec"] <- bennett.uci(newtab5$male.adj.pct[newtab5$rec_metric=="dist_rec"],raab$rec_dist_num[raab$gender=="male"],raab$erec_rec_dist_denom[raab$gender=="male"],raab$clusterId[raab$gender=="male"])
newtab5$total.adj.pct.uci[newtab5$rec_metric=="dist_rec"] <- bennett.uci(newtab5$total.adj.pct[newtab5$rec_metric=="dist_rec"],raab$rec_dist_num,raab$erec_rec_dist_denom,raab$clusterId)

tot_num<-newtab5[,c("rec_metric","total.adj.pct")]
tot_num$total.adj.pct<-as.numeric(tot_num$total.adj.pct)

newtab5$quality_gap[2]<-format( round( ((tot_num$total.adj.pct[tot_num$rec_metric=="dist_rec"]-tot_num$total.adj.pct[tot_num$rec_metric=="dist_erec"])/tot_num$total.adj.pct[tot_num$rec_metric=="dist_rec"])*100,1),nsmall=1)

lcis<-grep("lci",names(newtab5))
ucis<-grep("uci",names(newtab5))
newtab5[,lcis][newtab5[,lcis]<0]<-0
newtab5[,ucis][newtab5[,ucis]>1]<-1

pcts<-grep("pct",names(newtab5))
newtab5[,pcts]<-format(round(newtab5[,pcts]*100,1),nsmall=1)

need=c("met","undermet","unmet","none")

dist.re.need<-data.frame(need)
dist.re.need[,c(2:25)]<-NA
names(dist.re.need)<-c(
  "need",

  "male.n",
  "male.pct",
  "male.pct.lci",
  "male.pct.uci",
  "male.adj.pct",
  "male.adj.pct.lci",
  "male.adj.pct.uci",
  "male.extrapolated.n",

  "female.n",
  "female.pct",
  "female.pct.lci",
  "female.pct.uci",
  "female.adj.pct",
  "female.adj.pct.lci",
  "female.adj.pct.uci",
  "female.extrapolated.n",

  "total.n",
  "total.pct",
  "total.pct.lci",
  "total.pct.uci",
  "total.adj.pct",
  "total.adj.pct.lci",
  "total.adj.pct.uci",
  "total.extrapolated.n"
  
)

dist.re.need$male.n[dist.re.need$need=="met"]<-sum(raab$aa_case[raab$gender=="male"],na.rm=T)
dist.re.need$female.n[dist.re.need$need=="met"]<-sum(raab$aa_case[raab$gender=="female"],na.rm=T)
dist.re.need$total.n[dist.re.need$need=="met"]<-sum(raab$aa_case,na.rm=T)

dist.re.need$male.n[dist.re.need$need=="undermet"]<-sum(raab$bb_case[raab$gender=="male"],na.rm=T)
dist.re.need$female.n[dist.re.need$need=="undermet"]<-sum(raab$bb_case[raab$gender=="female"],na.rm=T)
dist.re.need$total.n[dist.re.need$need=="undermet"]<-sum(raab$bb_case,na.rm=T)

dist.re.need$male.n[dist.re.need$need=="unmet"]<-sum(raab$cc_case[raab$gender=="male"],na.rm=T)
dist.re.need$female.n[dist.re.need$need=="unmet"]<-sum(raab$cc_case[raab$gender=="female"],na.rm=T)
dist.re.need$total.n[dist.re.need$need=="unmet"]<-sum(raab$cc_case,na.rm=T)

dist.re.need$male.n[dist.re.need$need=="none"]<-sum(raab$dd_case[raab$gender=="male"],na.rm=T)
dist.re.need$female.n[dist.re.need$need=="none"]<-sum(raab$dd_case[raab$gender=="female"],na.rm=T)
dist.re.need$total.n[dist.re.need$need=="none"]<-sum(raab$dd_case,na.rm=T)

dist.re.need$male.pct[dist.re.need$need=="met"]<-sum(raab$aa_case[raab$gender=="male"],na.rm=T)/sum(raab$exam_status[raab$gender=="male"]=="exam_status_examined",na.rm=T)
dist.re.need$female.pct[dist.re.need$need=="met"]<-sum(raab$aa_case[raab$gender=="female"],na.rm=T)/sum(raab$exam_status[raab$gender=="female"]=="exam_status_examined",na.rm=T)
dist.re.need$total.pct[dist.re.need$need=="met"]<-sum(raab$aa_case,na.rm=T)/sum(raab$exam_status=="exam_status_examined",na.rm=T)

dist.re.need$male.pct[dist.re.need$need=="undermet"]<-sum(raab$bb_case[raab$gender=="male"],na.rm=T)/sum(raab$exam_status[raab$gender=="male"]=="exam_status_examined",na.rm=T)
dist.re.need$female.pct[dist.re.need$need=="undermet"]<-sum(raab$bb_case[raab$gender=="female"],na.rm=T)/sum(raab$exam_status[raab$gender=="female"]=="exam_status_examined",na.rm=T)
dist.re.need$total.pct[dist.re.need$need=="undermet"]<-sum(raab$bb_case,na.rm=T)/sum(raab$exam_status=="exam_status_examined",na.rm=T)

dist.re.need$male.pct[dist.re.need$need=="unmet"]<-sum(raab$cc_case[raab$gender=="male"],na.rm=T)/sum(raab$exam_status[raab$gender=="male"]=="exam_status_examined",na.rm=T)
dist.re.need$female.pct[dist.re.need$need=="unmet"]<-sum(raab$cc_case[raab$gender=="female"],na.rm=T)/sum(raab$exam_status[raab$gender=="female"]=="exam_status_examined",na.rm=T)
dist.re.need$total.pct[dist.re.need$need=="unmet"]<-sum(raab$cc_case,na.rm=T)/sum(raab$exam_status=="exam_status_examined",na.rm=T)

dist.re.need$male.pct[dist.re.need$need=="none"]<-sum(raab$dd_case[raab$gender=="male"],na.rm=T)/sum(raab$exam_status[raab$gender=="male"]=="exam_status_examined",na.rm=T)
dist.re.need$female.pct[dist.re.need$need=="none"]<-sum(raab$dd_case[raab$gender=="female"],na.rm=T)/sum(raab$exam_status[raab$gender=="female"]=="exam_status_examined",na.rm=T)
dist.re.need$total.pct[dist.re.need$need=="none"]<-sum(raab$dd_case,na.rm=T)/sum(raab$exam_status=="exam_status_examined",na.rm=T)

dist.re.need$male.pct.lci[dist.re.need$need=="met"]<-bennett.lci(dist.re.need$male.pct[dist.re.need$need=="met"],raab$aa_case[raab$gender=="male"],raab$exam_status[raab$gender=="male"]=="exam_status_examined",raab$clusterId[raab$gender=="male"])
dist.re.need$female.pct.lci[dist.re.need$need=="met"]<-bennett.lci(dist.re.need$female.pct[dist.re.need$need=="met"],raab$aa_case[raab$gender=="female"],raab$exam_status[raab$gender=="female"]=="exam_status_examined",raab$clusterId[raab$gender=="female"])
dist.re.need$total.pct.lci[dist.re.need$need=="met"]<-bennett.lci(dist.re.need$total.pct[dist.re.need$need=="met"],raab$aa_case,raab$exam_status=="exam_status_examined",raab$clusterId)

dist.re.need$male.pct.uci[dist.re.need$need=="met"]<-bennett.uci(dist.re.need$male.pct[dist.re.need$need=="met"],raab$aa_case[raab$gender=="male"],raab$exam_status[raab$gender=="male"]=="exam_status_examined",raab$clusterId[raab$gender=="male"])
dist.re.need$female.pct.uci[dist.re.need$need=="met"]<-bennett.uci(dist.re.need$female.pct[dist.re.need$need=="met"],raab$aa_case[raab$gender=="female"],raab$exam_status[raab$gender=="female"]=="exam_status_examined",raab$clusterId[raab$gender=="female"])
dist.re.need$total.pct.uci[dist.re.need$need=="met"]<-bennett.uci(dist.re.need$total.pct[dist.re.need$need=="met"],raab$aa_case,raab$exam_status=="exam_status_examined",raab$clusterId)

dist.re.need$male.pct.lci[dist.re.need$need=="undermet"]<-bennett.lci(dist.re.need$male.pct[dist.re.need$need=="undermet"],raab$bb_case[raab$gender=="male"],raab$exam_status[raab$gender=="male"]=="exam_status_examined",raab$clusterId[raab$gender=="male"])
dist.re.need$female.pct.lci[dist.re.need$need=="undermet"]<-bennett.lci(dist.re.need$female.pct[dist.re.need$need=="undermet"],raab$bb_case[raab$gender=="female"],raab$exam_status[raab$gender=="female"]=="exam_status_examined",raab$clusterId[raab$gender=="female"])
dist.re.need$total.pct.lci[dist.re.need$need=="undermet"]<-bennett.lci(dist.re.need$total.pct[dist.re.need$need=="undermet"],raab$bb_case,raab$exam_status=="exam_status_examined",raab$clusterId)

dist.re.need$male.pct.uci[dist.re.need$need=="undermet"]<-bennett.uci(dist.re.need$male.pct[dist.re.need$need=="undermet"],raab$bb_case[raab$gender=="male"],raab$exam_status[raab$gender=="male"]=="exam_status_examined",raab$clusterId[raab$gender=="male"])
dist.re.need$female.pct.uci[dist.re.need$need=="undermet"]<-bennett.uci(dist.re.need$female.pct[dist.re.need$need=="undermet"],raab$bb_case[raab$gender=="female"],raab$exam_status[raab$gender=="female"]=="exam_status_examined",raab$clusterId[raab$gender=="female"])
dist.re.need$total.pct.uci[dist.re.need$need=="undermet"]<-bennett.uci(dist.re.need$total.pct[dist.re.need$need=="undermet"],raab$bb_case,raab$exam_status=="exam_status_examined",raab$clusterId)

dist.re.need$male.pct.lci[dist.re.need$need=="unmet"]<-bennett.lci(dist.re.need$male.pct[dist.re.need$need=="unmet"],raab$cc_case[raab$gender=="male"],raab$exam_status[raab$gender=="male"]=="exam_status_examined",raab$clusterId[raab$gender=="male"])
dist.re.need$female.pct.lci[dist.re.need$need=="unmet"]<-bennett.lci(dist.re.need$female.pct[dist.re.need$need=="unmet"],raab$cc_case[raab$gender=="female"],raab$exam_status[raab$gender=="female"]=="exam_status_examined",raab$clusterId[raab$gender=="female"])
dist.re.need$total.pct.lci[dist.re.need$need=="unmet"]<-bennett.lci(dist.re.need$total.pct[dist.re.need$need=="unmet"],raab$cc_case,raab$exam_status=="exam_status_examined",raab$clusterId)

dist.re.need$male.pct.uci[dist.re.need$need=="unmet"]<-bennett.uci(dist.re.need$male.pct[dist.re.need$need=="unmet"],raab$cc_case[raab$gender=="male"],raab$exam_status[raab$gender=="male"]=="exam_status_examined",raab$clusterId[raab$gender=="male"])
dist.re.need$female.pct.uci[dist.re.need$need=="unmet"]<-bennett.uci(dist.re.need$female.pct[dist.re.need$need=="unmet"],raab$cc_case[raab$gender=="female"],raab$exam_status[raab$gender=="female"]=="exam_status_examined",raab$clusterId[raab$gender=="female"])
dist.re.need$total.pct.uci[dist.re.need$need=="unmet"]<-bennett.uci(dist.re.need$total.pct[dist.re.need$need=="unmet"],raab$cc_case,raab$exam_status=="exam_status_examined",raab$clusterId)

dist.re.need$male.pct.lci[dist.re.need$need=="none"]<-bennett.lci(dist.re.need$male.pct[dist.re.need$need=="none"],raab$dd_case[raab$gender=="male"],raab$exam_status[raab$gender=="male"]=="exam_status_examined",raab$clusterId[raab$gender=="male"])
dist.re.need$female.pct.lci[dist.re.need$need=="none"]<-bennett.lci(dist.re.need$female.pct[dist.re.need$need=="none"],raab$dd_case[raab$gender=="female"],raab$exam_status[raab$gender=="female"]=="exam_status_examined",raab$clusterId[raab$gender=="female"])
dist.re.need$total.pct.lci[dist.re.need$need=="none"]<-bennett.lci(dist.re.need$total.pct[dist.re.need$need=="none"],raab$dd_case,raab$exam_status=="exam_status_examined",raab$clusterId)

dist.re.need$male.pct.uci[dist.re.need$need=="none"]<-bennett.uci(dist.re.need$male.pct[dist.re.need$need=="none"],raab$dd_case[raab$gender=="male"],raab$exam_status[raab$gender=="male"]=="exam_status_examined",raab$clusterId[raab$gender=="male"])
dist.re.need$female.pct.uci[dist.re.need$need=="none"]<-bennett.uci(dist.re.need$female.pct[dist.re.need$need=="none"],raab$dd_case[raab$gender=="female"],raab$exam_status[raab$gender=="female"]=="exam_status_examined",raab$clusterId[raab$gender=="female"])
dist.re.need$total.pct.uci[dist.re.need$need=="none"]<-bennett.uci(dist.re.need$total.pct[dist.re.need$need=="none"],raab$dd_case,raab$exam_status=="exam_status_examined",raab$clusterId)

dist.re.need$male.adj.pct[dist.re.need$need=="met"]<-prop.age.adjust(male.subpop,raab[raab$gender=="male",],raab$aa_case[raab$gender=="male"],raab$exam_status[raab$gender=="male"]=="exam_status_examined")
dist.re.need$female.adj.pct[dist.re.need$need=="met"]<-prop.age.adjust(female.subpop,raab[raab$gender=="female",],raab$aa_case[raab$gender=="female"],raab$exam_status[raab$gender=="female"]=="exam_status_examined")
dist.re.need$total.adj.pct[dist.re.need$need=="met"]<-prop.age.sex.adjust(popfives,raab,raab$aa_case,raab$exam_status=="exam_status_examined")

dist.re.need$male.adj.pct[dist.re.need$need=="undermet"]<-prop.age.adjust(male.subpop,raab[raab$gender=="male",],raab$bb_case[raab$gender=="male"],raab$exam_status[raab$gender=="male"]=="exam_status_examined")
dist.re.need$female.adj.pct[dist.re.need$need=="undermet"]<-prop.age.adjust(female.subpop,raab[raab$gender=="female",],raab$bb_case[raab$gender=="female"],raab$exam_status[raab$gender=="female"]=="exam_status_examined")
dist.re.need$total.adj.pct[dist.re.need$need=="undermet"]<-prop.age.sex.adjust(popfives,raab,raab$bb_case,raab$exam_status=="exam_status_examined")

dist.re.need$male.adj.pct[dist.re.need$need=="unmet"]<-prop.age.adjust(male.subpop,raab[raab$gender=="male",],raab$cc_case[raab$gender=="male"],raab$exam_status[raab$gender=="male"]=="exam_status_examined")
dist.re.need$female.adj.pct[dist.re.need$need=="unmet"]<-prop.age.adjust(female.subpop,raab[raab$gender=="female",],raab$cc_case[raab$gender=="female"],raab$exam_status[raab$gender=="female"]=="exam_status_examined")
dist.re.need$total.adj.pct[dist.re.need$need=="unmet"]<-prop.age.sex.adjust(popfives,raab,raab$cc_case,raab$exam_status=="exam_status_examined")

dist.re.need$male.adj.pct[dist.re.need$need=="none"]<-prop.age.adjust(male.subpop,raab[raab$gender=="male",],raab$dd_case[raab$gender=="male"],raab$exam_status[raab$gender=="male"]=="exam_status_examined")
dist.re.need$female.adj.pct[dist.re.need$need=="none"]<-prop.age.adjust(female.subpop,raab[raab$gender=="female",],raab$dd_case[raab$gender=="female"],raab$exam_status[raab$gender=="female"]=="exam_status_examined")
dist.re.need$total.adj.pct[dist.re.need$need=="none"]<-prop.age.sex.adjust(popfives,raab,raab$dd_case,raab$exam_status=="exam_status_examined")

dist.re.need$male.adj.pct.lci[dist.re.need$need=="met"]<-bennett.lci(dist.re.need$male.adj.pct[dist.re.need$need=="met"],raab$aa_case[raab$gender=="male"],raab$exam_status[raab$gender=="male"]=="exam_status_examined",raab$clusterId[raab$gender=="male"])
dist.re.need$female.adj.pct.lci[dist.re.need$need=="met"]<-bennett.lci(dist.re.need$female.adj.pct[dist.re.need$need=="met"],raab$aa_case[raab$gender=="female"],raab$exam_status[raab$gender=="female"]=="exam_status_examined",raab$clusterId[raab$gender=="female"])
dist.re.need$total.adj.pct.lci[dist.re.need$need=="met"]<-bennett.lci(dist.re.need$total.adj.pct[dist.re.need$need=="met"],raab$aa_case,raab$exam_status=="exam_status_examined",raab$clusterId)

dist.re.need$male.adj.pct.uci[dist.re.need$need=="met"]<-bennett.uci(dist.re.need$male.adj.pct[dist.re.need$need=="met"],raab$aa_case[raab$gender=="male"],raab$exam_status[raab$gender=="male"]=="exam_status_examined",raab$clusterId[raab$gender=="male"])
dist.re.need$female.adj.pct.uci[dist.re.need$need=="met"]<-bennett.uci(dist.re.need$female.adj.pct[dist.re.need$need=="met"],raab$aa_case[raab$gender=="female"],raab$exam_status[raab$gender=="female"]=="exam_status_examined",raab$clusterId[raab$gender=="female"])
dist.re.need$total.adj.pct.uci[dist.re.need$need=="met"]<-bennett.uci(dist.re.need$total.adj.pct[dist.re.need$need=="met"],raab$aa_case,raab$exam_status=="exam_status_examined",raab$clusterId)

dist.re.need$male.adj.pct.lci[dist.re.need$need=="undermet"]<-bennett.lci(dist.re.need$male.adj.pct[dist.re.need$need=="undermet"],raab$bb_case[raab$gender=="male"],raab$exam_status[raab$gender=="male"]=="exam_status_examined",raab$clusterId[raab$gender=="male"])
dist.re.need$female.adj.pct.lci[dist.re.need$need=="undermet"]<-bennett.lci(dist.re.need$female.adj.pct[dist.re.need$need=="undermet"],raab$bb_case[raab$gender=="female"],raab$exam_status[raab$gender=="female"]=="exam_status_examined",raab$clusterId[raab$gender=="female"])
dist.re.need$total.adj.pct.lci[dist.re.need$need=="undermet"]<-bennett.lci(dist.re.need$total.adj.pct[dist.re.need$need=="undermet"],raab$bb_case,raab$exam_status=="exam_status_examined",raab$clusterId)

dist.re.need$male.adj.pct.uci[dist.re.need$need=="undermet"]<-bennett.uci(dist.re.need$male.adj.pct[dist.re.need$need=="undermet"],raab$aa_case[raab$gender=="male"],raab$exam_status[raab$gender=="male"]=="exam_status_examined",raab$clusterId[raab$gender=="male"])
dist.re.need$female.adj.pct.uci[dist.re.need$need=="undermet"]<-bennett.uci(dist.re.need$female.adj.pct[dist.re.need$need=="undermet"],raab$bb_case[raab$gender=="female"],raab$exam_status[raab$gender=="female"]=="exam_status_examined",raab$clusterId[raab$gender=="female"])
dist.re.need$total.adj.pct.uci[dist.re.need$need=="undermet"]<-bennett.uci(dist.re.need$total.adj.pct[dist.re.need$need=="undermet"],raab$bb_case,raab$exam_status=="exam_status_examined",raab$clusterId)

dist.re.need$male.adj.pct.lci[dist.re.need$need=="unmet"]<-bennett.lci(dist.re.need$male.adj.pct[dist.re.need$need=="unmet"],raab$cc_case[raab$gender=="male"],raab$exam_status[raab$gender=="male"]=="exam_status_examined",raab$clusterId[raab$gender=="male"])
dist.re.need$female.adj.pct.lci[dist.re.need$need=="unmet"]<-bennett.lci(dist.re.need$female.adj.pct[dist.re.need$need=="unmet"],raab$cc_case[raab$gender=="female"],raab$exam_status[raab$gender=="female"]=="exam_status_examined",raab$clusterId[raab$gender=="female"])
dist.re.need$total.adj.pct.lci[dist.re.need$need=="unmet"]<-bennett.lci(dist.re.need$total.adj.pct[dist.re.need$need=="unmet"],raab$cc_case,raab$exam_status=="exam_status_examined",raab$clusterId)

dist.re.need$male.adj.pct.uci[dist.re.need$need=="unmet"]<-bennett.uci(dist.re.need$male.adj.pct[dist.re.need$need=="unmet"],raab$cc_case[raab$gender=="male"],raab$exam_status[raab$gender=="male"]=="exam_status_examined",raab$clusterId[raab$gender=="male"])
dist.re.need$female.adj.pct.uci[dist.re.need$need=="unmet"]<-bennett.uci(dist.re.need$female.adj.pct[dist.re.need$need=="unmet"],raab$cc_case[raab$gender=="female"],raab$exam_status[raab$gender=="female"]=="exam_status_examined",raab$clusterId[raab$gender=="female"])
dist.re.need$total.adj.pct.uci[dist.re.need$need=="unmet"]<-bennett.uci(dist.re.need$total.adj.pct[dist.re.need$need=="unmet"],raab$cc_case,raab$exam_status=="exam_status_examined",raab$clusterId)

dist.re.need$male.adj.pct.lci[dist.re.need$need=="none"]<-bennett.lci(dist.re.need$male.adj.pct[dist.re.need$need=="none"],raab$dd_case[raab$gender=="male"],raab$exam_status[raab$gender=="male"]=="exam_status_examined",raab$clusterId[raab$gender=="male"])
dist.re.need$female.adj.pct.lci[dist.re.need$need=="none"]<-bennett.lci(dist.re.need$female.adj.pct[dist.re.need$need=="none"],raab$dd_case[raab$gender=="female"],raab$exam_status[raab$gender=="female"]=="exam_status_examined",raab$clusterId[raab$gender=="female"])
dist.re.need$total.adj.pct.lci[dist.re.need$need=="none"]<-bennett.lci(dist.re.need$total.adj.pct[dist.re.need$need=="none"],raab$dd_case,raab$exam_status=="exam_status_examined",raab$clusterId)

dist.re.need$male.adj.pct.uci[dist.re.need$need=="none"]<-bennett.uci(dist.re.need$male.adj.pct[dist.re.need$need=="none"],raab$dd_case[raab$gender=="male"],raab$exam_status[raab$gender=="male"]=="exam_status_examined",raab$clusterId[raab$gender=="male"])
dist.re.need$female.adj.pct.uci[dist.re.need$need=="none"]<-bennett.uci(dist.re.need$female.adj.pct[dist.re.need$need=="none"],raab$dd_case[raab$gender=="female"],raab$exam_status[raab$gender=="female"]=="exam_status_examined",raab$clusterId[raab$gender=="female"])
dist.re.need$total.adj.pct.uci[dist.re.need$need=="none"]<-bennett.uci(dist.re.need$total.adj.pct[dist.re.need$need=="none"],raab$dd_case,raab$exam_status=="exam_status_examined",raab$clusterId)

dist.re.need$male.extrapolated.n<-round( dist.re.need$male.adj.pct*sum(male.subpop$population,na.rm=T),0)
dist.re.need$female.extrapolated.n<-round( dist.re.need$female.adj.pct*sum(female.subpop$population,na.rm=T),0)
dist.re.need$total.extrapolated.n<-round( dist.re.need$total.adj.pct*sum(popfives$population,na.rm=T),0)

lcis<-grep("lci",names(dist.re.need))
ucis<-grep("uci",names(dist.re.need))
dist.re.need[,lcis][dist.re.need[,lcis]<0]<-0
dist.re.need[,ucis][dist.re.need[,ucis]>1]<-1

pcts<-grep("pct",names(dist.re.need))
dist.re.need[,pcts]<-format(round(dist.re.need[,pcts]*100,1),nsmall=1)

