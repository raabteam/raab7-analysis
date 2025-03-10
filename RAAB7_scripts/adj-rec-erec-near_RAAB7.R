#RAAB7

#v1 231123 IM

# eREC for near: Crude calculation using the gold standard definition

# ee=individuals with UCVA <N6 at 40 cm in the better eye who present with spectacles for near vision and whose PVA is ≥N6 in the better eye (met need) [NB the WHO paper box does not include "distance BCVA ≥6/12* in at least one eye" but the supp material flow chart does]
# ff=individuals with distance BCVA ≥6/12* in at least one eye who present with spectacles for near vision and whose PVA is <N6 in the better eye (undermet need)
# gg=individuals with distance BCVA ≥6/12 in at least one eye who do not have correction for near vision and whose UCVA is <N6 in the better eye (unmet need)

# Only individuals with distance BCVA ≥6/12 will be considered in order to exclude those with reduced near vision not due to other causes.

raab$erec_near_num <-(raab$ee_case==1)+0
raab$rec_near_num <-(raab$ee_case==1 | raab$ff_case==1)+0
raab$erec_rec_near_denom <-(raab$ee_case==1 | raab$ff_case==1 | raab$gg_case==1)+0

erec_output <- c("near_erec","near_rec")

newtab7<-data.frame(erec_output)
newtab7[,2:19] <- NA
names(newtab7) <- c("rec_metric",
                    
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

newtab7$female.pct[newtab7$rec_metric=="near_erec"] <- sum(raab$erec_near_num[raab$gender=="female"],na.rm=T)/sum(raab$erec_rec_near_denom[raab$gender=="female"],na.rm=T)
newtab7$male.pct[newtab7$rec_metric=="near_erec"] <- sum(raab$erec_near_num[raab$gender=="male"],na.rm=T)/sum(raab$erec_rec_near_denom[raab$gender=="male"],na.rm=T)
newtab7$total.pct[newtab7$rec_metric=="near_erec"] <- sum(raab$erec_near_num,na.rm=T)/sum(raab$erec_rec_near_denom,na.rm=T)

newtab7$female.pct[newtab7$rec_metric=="near_rec"] <- sum(raab$rec_near_num[raab$gender=="female"],na.rm=T)/sum(raab$erec_rec_near_denom[raab$gender=="female"],na.rm=T)
newtab7$male.pct[newtab7$rec_metric=="near_rec"] <- sum(raab$rec_near_num[raab$gender=="male"],na.rm=T)/sum(raab$erec_rec_near_denom[raab$gender=="male"],na.rm=T)
newtab7$total.pct[newtab7$rec_metric=="near_rec"] <- sum(raab$rec_near_num,na.rm=T)/sum(raab$erec_rec_near_denom,na.rm=T)

newtab7$female.pct.lci[newtab7$rec_metric=="near_erec"] <- bennett.lci(newtab7$female.pct[newtab7$rec_metric=="near_erec"],raab$erec_near_num[raab$gender=="female"],raab$erec_rec_near_denom[raab$gender=="female"],raab$clusterId[raab$gender=="female"])
newtab7$male.pct.lci[newtab7$rec_metric=="near_erec"] <- bennett.lci(newtab7$male.pct[newtab7$rec_metric=="near_erec"],raab$erec_near_num[raab$gender=="male"],raab$erec_rec_near_denom[raab$gender=="male"],raab$clusterId[raab$gender=="male"])
newtab7$total.pct.lci[newtab7$rec_metric=="near_erec"] <- bennett.lci(newtab7$total.pct[newtab7$rec_metric=="near_erec"],raab$erec_near_num,raab$erec_rec_near_denom,raab$clusterId)

newtab7$female.pct.uci[newtab7$rec_metric=="near_erec"] <- bennett.uci(newtab7$female.pct[newtab7$rec_metric=="near_erec"],raab$erec_near_num[raab$gender=="female"],raab$erec_rec_near_denom[raab$gender=="female"],raab$clusterId[raab$gender=="female"])
newtab7$male.pct.uci[newtab7$rec_metric=="near_erec"] <- bennett.uci(newtab7$male.pct[newtab7$rec_metric=="near_erec"],raab$erec_near_num[raab$gender=="male"],raab$erec_rec_near_denom[raab$gender=="male"],raab$clusterId[raab$gender=="male"])
newtab7$total.pct.uci[newtab7$rec_metric=="near_erec"] <- bennett.uci(newtab7$total.pct[newtab7$rec_metric=="near_erec"],raab$erec_near_num,raab$erec_rec_near_denom,raab$clusterId)

newtab7$female.pct.lci[newtab7$rec_metric=="near_rec"] <- bennett.lci(newtab7$female.pct[newtab7$rec_metric=="near_rec"],raab$rec_near_num[raab$gender=="female"],raab$erec_rec_near_denom[raab$gender=="female"],raab$clusterId[raab$gender=="female"])
newtab7$male.pct.lci[newtab7$rec_metric=="near_rec"] <- bennett.lci(newtab7$male.pct[newtab7$rec_metric=="near_rec"],raab$rec_near_num[raab$gender=="male"],raab$erec_rec_near_denom[raab$gender=="male"],raab$clusterId[raab$gender=="male"])
newtab7$total.pct.lci[newtab7$rec_metric=="near_rec"] <- bennett.lci(newtab7$total.pct[newtab7$rec_metric=="near_rec"],raab$rec_near_num,raab$erec_rec_near_denom,raab$clusterId)

newtab7$female.pct.uci[newtab7$rec_metric=="near_rec"] <- bennett.uci(newtab7$female.pct[newtab7$rec_metric=="near_rec"],raab$rec_near_num[raab$gender=="female"],raab$erec_rec_near_denom[raab$gender=="female"],raab$clusterId[raab$gender=="female"])
newtab7$male.pct.uci[newtab7$rec_metric=="near_rec"] <- bennett.uci(newtab7$male.pct[newtab7$rec_metric=="near_rec"],raab$rec_near_num[raab$gender=="male"],raab$erec_rec_near_denom[raab$gender=="male"],raab$clusterId[raab$gender=="male"])
newtab7$total.pct.uci[newtab7$rec_metric=="near_rec"] <- bennett.uci(newtab7$total.pct[newtab7$rec_metric=="near_rec"],raab$rec_near_num,raab$erec_rec_near_denom,raab$clusterId)

#eREC and REC adjusted proportion estimates

newtab7$female.adj.pct[newtab7$rec_metric=="near_erec"] <- prop.age.adjust(female.subpop,raab[raab$gender=="female",],raab$erec_near_num[raab$gender=="female"],raab$erec_rec_near_denom[raab$gender=="female"])
newtab7$male.adj.pct[newtab7$rec_metric=="near_erec"] <- prop.age.adjust(male.subpop,raab[raab$gender=="male",], raab$erec_near_num[raab$gender=="male"], raab$erec_rec_near_denom[raab$gender=="male"])
newtab7$total.adj.pct[newtab7$rec_metric=="near_erec"] <- prop.age.sex.adjust(popfives, raab, raab$erec_near_num, raab$erec_rec_near_denom)

newtab7$female.adj.pct[newtab7$rec_metric=="near_rec"] <- prop.age.adjust(female.subpop,raab[raab$gender=="female",],raab$rec_near_num[raab$gender=="female"],raab$erec_rec_near_denom[raab$gender=="female"])
newtab7$male.adj.pct[newtab7$rec_metric=="near_rec"] <- prop.age.adjust(male.subpop,raab[raab$gender=="male",], raab$rec_near_num[raab$gender=="male"], raab$erec_rec_near_denom[raab$gender=="male"])
newtab7$total.adj.pct[newtab7$rec_metric=="near_rec"] <- prop.age.sex.adjust(popfives, raab, raab$rec_near_num, raab$erec_rec_near_denom)

newtab7$female.adj.pct.lci[newtab7$rec_metric=="near_erec"] <- bennett.lci(newtab7$female.adj.pct[newtab7$rec_metric=="near_erec"],raab$erec_near_num[raab$gender=="female"],raab$erec_rec_near_denom[raab$gender=="female"],raab$clusterId[raab$gender=="female"])
newtab7$male.adj.pct.lci[newtab7$rec_metric=="near_erec"] <- bennett.lci(newtab7$male.adj.pct[newtab7$rec_metric=="near_erec"],raab$erec_near_num[raab$gender=="male"],raab$erec_rec_near_denom[raab$gender=="male"],raab$clusterId[raab$gender=="male"])
newtab7$total.adj.pct.lci[newtab7$rec_metric=="near_erec"] <- bennett.lci(newtab7$total.adj.pct[newtab7$rec_metric=="near_erec"],raab$erec_near_num,raab$erec_rec_near_denom,raab$clusterId)

newtab7$female.adj.pct.uci[newtab7$rec_metric=="near_erec"] <- bennett.uci(newtab7$female.adj.pct[newtab7$rec_metric=="near_erec"],raab$erec_near_num[raab$gender=="female"],raab$erec_rec_near_denom[raab$gender=="female"],raab$clusterId[raab$gender=="female"])
newtab7$male.adj.pct.uci[newtab7$rec_metric=="near_erec"] <- bennett.uci(newtab7$male.adj.pct[newtab7$rec_metric=="near_erec"],raab$erec_near_num[raab$gender=="male"],raab$erec_rec_near_denom[raab$gender=="male"],raab$clusterId[raab$gender=="male"])
newtab7$total.adj.pct.uci[newtab7$rec_metric=="near_erec"] <- bennett.uci(newtab7$total.adj.pct[newtab7$rec_metric=="near_erec"],raab$erec_near_num,raab$erec_rec_near_denom,raab$clusterId)

newtab7$female.adj.pct.lci[newtab7$rec_metric=="near_rec"] <- bennett.lci(newtab7$female.adj.pct[newtab7$rec_metric=="near_rec"],raab$rec_near_num[raab$gender=="female"],raab$erec_rec_near_denom[raab$gender=="female"],raab$clusterId[raab$gender=="female"])
newtab7$male.adj.pct.lci[newtab7$rec_metric=="near_rec"] <- bennett.lci(newtab7$male.adj.pct[newtab7$rec_metric=="near_rec"],raab$rec_near_num[raab$gender=="male"],raab$erec_rec_near_denom[raab$gender=="male"],raab$clusterId[raab$gender=="male"])
newtab7$total.adj.pct.lci[newtab7$rec_metric=="near_rec"] <- bennett.lci(newtab7$total.adj.pct[newtab7$rec_metric=="near_rec"],raab$rec_near_num,raab$erec_rec_near_denom,raab$clusterId)

newtab7$female.adj.pct.uci[newtab7$rec_metric=="near_rec"] <- bennett.uci(newtab7$female.adj.pct[newtab7$rec_metric=="near_rec"],raab$rec_near_num[raab$gender=="female"],raab$erec_rec_near_denom[raab$gender=="female"],raab$clusterId[raab$gender=="female"])
newtab7$male.adj.pct.uci[newtab7$rec_metric=="near_rec"] <- bennett.uci(newtab7$male.adj.pct[newtab7$rec_metric=="near_rec"],raab$rec_near_num[raab$gender=="male"],raab$erec_rec_near_denom[raab$gender=="male"],raab$clusterId[raab$gender=="male"])
newtab7$total.adj.pct.uci[newtab7$rec_metric=="near_rec"] <- bennett.uci(newtab7$total.adj.pct[newtab7$rec_metric=="near_rec"],raab$rec_near_num,raab$erec_rec_near_denom,raab$clusterId)

tot_num<-newtab7[,c("rec_metric","total.adj.pct")]
tot_num$total.adj.pct<-as.numeric(tot_num$total.adj.pct)

newtab7$quality_gap[2]<-format( round( ((tot_num$total.adj.pct[tot_num$rec_metric=="near_rec"]-tot_num$total.adj.pct[tot_num$rec_metric=="near_erec"])/tot_num$total.adj.pct[tot_num$rec_metric=="near_rec"])*100,1),nsmall=1)

lcis<-grep("lci",names(newtab7))
ucis<-grep("uci",names(newtab7))
newtab7[,lcis][newtab7[,lcis]<0]<-0
newtab7[,ucis][newtab7[,ucis]>1]<-1

pcts<-grep("pct",names(newtab7))
newtab7[,pcts]<-format(round(newtab7[,pcts]*100,1),nsmall=1)

raab$near_need_denom <-(raab$ee_case==1 | raab$ff_case==1 | raab$gg_case==1 | raab$hh_case==1)+0

need=c("met","undermet","unmet","none")

near.re.need<-data.frame(need)
near.re.need[,c(2:25)]<-NA
names(near.re.need)<-c(
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

near.re.need$male.n[near.re.need$need=="met"]<-sum(raab$ee_case[raab$gender=="male"],na.rm=T)
near.re.need$female.n[near.re.need$need=="met"]<-sum(raab$ee_case[raab$gender=="female"],na.rm=T)
near.re.need$total.n[near.re.need$need=="met"]<-sum(raab$ee_case,na.rm=T)

near.re.need$male.n[near.re.need$need=="undermet"]<-sum(raab$ff_case[raab$gender=="male"],na.rm=T)
near.re.need$female.n[near.re.need$need=="undermet"]<-sum(raab$ff_case[raab$gender=="female"],na.rm=T)
near.re.need$total.n[near.re.need$need=="undermet"]<-sum(raab$ff_case,na.rm=T)

near.re.need$male.n[near.re.need$need=="unmet"]<-sum(raab$gg_case[raab$gender=="male"],na.rm=T)
near.re.need$female.n[near.re.need$need=="unmet"]<-sum(raab$gg_case[raab$gender=="female"],na.rm=T)
near.re.need$total.n[near.re.need$need=="unmet"]<-sum(raab$gg_case,na.rm=T)

near.re.need$male.n[near.re.need$need=="none"]<-sum(raab$hh_case[raab$gender=="male"],na.rm=T)
near.re.need$female.n[near.re.need$need=="none"]<-sum(raab$hh_case[raab$gender=="female"],na.rm=T)
near.re.need$total.n[near.re.need$need=="none"]<-sum(raab$hh_case,na.rm=T)

near.re.need$male.pct[near.re.need$need=="met"]<-sum(raab$ee_case[raab$gender=="male"],na.rm=T)/sum(raab$near_need_denom[raab$gender=="male"]==1,na.rm=T)
near.re.need$female.pct[near.re.need$need=="met"]<-sum(raab$ee_case[raab$gender=="female"],na.rm=T)/sum(raab$near_need_denom[raab$gender=="female"]==1,na.rm=T)
near.re.need$total.pct[near.re.need$need=="met"]<-sum(raab$ee_case,na.rm=T)/sum(raab$near_need_denom==1,na.rm=T)

near.re.need$male.pct[near.re.need$need=="undermet"]<-sum(raab$ff_case[raab$gender=="male"],na.rm=T)/sum(raab$near_need_denom[raab$gender=="male"]==1,na.rm=T)
near.re.need$female.pct[near.re.need$need=="undermet"]<-sum(raab$ff_case[raab$gender=="female"],na.rm=T)/sum(raab$near_need_denom[raab$gender=="female"]==1,na.rm=T)
near.re.need$total.pct[near.re.need$need=="undermet"]<-sum(raab$ff_case,na.rm=T)/sum(raab$near_need_denom==1,na.rm=T)

near.re.need$male.pct[near.re.need$need=="unmet"]<-sum(raab$gg_case[raab$gender=="male"],na.rm=T)/sum(raab$near_need_denom[raab$gender=="male"]==1,na.rm=T)
near.re.need$female.pct[near.re.need$need=="unmet"]<-sum(raab$gg_case[raab$gender=="female"],na.rm=T)/sum(raab$near_need_denom[raab$gender=="female"]==1,na.rm=T)
near.re.need$total.pct[near.re.need$need=="unmet"]<-sum(raab$gg_case,na.rm=T)/sum(raab$near_need_denom==1,na.rm=T)

near.re.need$male.pct[near.re.need$need=="none"]<-sum(raab$hh_case[raab$gender=="male"],na.rm=T)/sum(raab$near_need_denom[raab$gender=="male"]==1,na.rm=T)
near.re.need$female.pct[near.re.need$need=="none"]<-sum(raab$hh_case[raab$gender=="female"],na.rm=T)/sum(raab$near_need_denom[raab$gender=="female"]==1,na.rm=T)
near.re.need$total.pct[near.re.need$need=="none"]<-sum(raab$hh_case,na.rm=T)/sum(raab$near_need_denom==1,na.rm=T)

near.re.need$male.pct.lci[near.re.need$need=="met"]<-bennett.lci(near.re.need$male.pct[near.re.need$need=="met"],raab$ee_case[raab$gender=="male"],raab$near_need_denom[raab$gender=="male"]==1,raab$clusterId[raab$gender=="male"])
near.re.need$female.pct.lci[near.re.need$need=="met"]<-bennett.lci(near.re.need$female.pct[near.re.need$need=="met"],raab$ee_case[raab$gender=="female"],raab$near_need_denom[raab$gender=="female"]==1,raab$clusterId[raab$gender=="female"])
near.re.need$total.pct.lci[near.re.need$need=="met"]<-bennett.lci(near.re.need$total.pct[near.re.need$need=="met"],raab$ee_case,raab$near_need_denom==1,raab$clusterId)

near.re.need$male.pct.uci[near.re.need$need=="met"]<-bennett.uci(near.re.need$male.pct[near.re.need$need=="met"],raab$ee_case[raab$gender=="male"],raab$near_need_denom[raab$gender=="male"]==1,raab$clusterId[raab$gender=="male"])
near.re.need$female.pct.uci[near.re.need$need=="met"]<-bennett.uci(near.re.need$female.pct[near.re.need$need=="met"],raab$ee_case[raab$gender=="female"],raab$near_need_denom[raab$gender=="female"]==1,raab$clusterId[raab$gender=="female"])
near.re.need$total.pct.uci[near.re.need$need=="met"]<-bennett.uci(near.re.need$total.pct[near.re.need$need=="met"],raab$ee_case,raab$near_need_denom==1,raab$clusterId)

near.re.need$male.pct.lci[near.re.need$need=="undermet"]<-bennett.lci(near.re.need$male.pct[near.re.need$need=="undermet"],raab$ff_case[raab$gender=="male"],raab$near_need_denom[raab$gender=="male"]==1,raab$clusterId[raab$gender=="male"])
near.re.need$female.pct.lci[near.re.need$need=="undermet"]<-bennett.lci(near.re.need$female.pct[near.re.need$need=="undermet"],raab$ff_case[raab$gender=="female"],raab$near_need_denom[raab$gender=="female"]==1,raab$clusterId[raab$gender=="female"])
near.re.need$total.pct.lci[near.re.need$need=="undermet"]<-bennett.lci(near.re.need$total.pct[near.re.need$need=="undermet"],raab$ff_case,raab$near_need_denom==1,raab$clusterId)

near.re.need$male.pct.uci[near.re.need$need=="undermet"]<-bennett.uci(near.re.need$male.pct[near.re.need$need=="undermet"],raab$ff_case[raab$gender=="male"],raab$near_need_denom[raab$gender=="male"]==1,raab$clusterId[raab$gender=="male"])
near.re.need$female.pct.uci[near.re.need$need=="undermet"]<-bennett.uci(near.re.need$female.pct[near.re.need$need=="undermet"],raab$ff_case[raab$gender=="female"],raab$near_need_denom[raab$gender=="female"]==1,raab$clusterId[raab$gender=="female"])
near.re.need$total.pct.uci[near.re.need$need=="undermet"]<-bennett.uci(near.re.need$total.pct[near.re.need$need=="undermet"],raab$ff_case,raab$near_need_denom==1,raab$clusterId)

near.re.need$male.pct.lci[near.re.need$need=="unmet"]<-bennett.lci(near.re.need$male.pct[near.re.need$need=="unmet"],raab$gg_case[raab$gender=="male"],raab$near_need_denom[raab$gender=="male"]==1,raab$clusterId[raab$gender=="male"])
near.re.need$female.pct.lci[near.re.need$need=="unmet"]<-bennett.lci(near.re.need$female.pct[near.re.need$need=="unmet"],raab$gg_case[raab$gender=="female"],raab$near_need_denom[raab$gender=="female"]==1,raab$clusterId[raab$gender=="female"])
near.re.need$total.pct.lci[near.re.need$need=="unmet"]<-bennett.lci(near.re.need$total.pct[near.re.need$need=="unmet"],raab$gg_case,raab$near_need_denom==1,raab$clusterId)

near.re.need$male.pct.uci[near.re.need$need=="unmet"]<-bennett.uci(near.re.need$male.pct[near.re.need$need=="unmet"],raab$gg_case[raab$gender=="male"],raab$near_need_denom[raab$gender=="male"]==1,raab$clusterId[raab$gender=="male"])
near.re.need$female.pct.uci[near.re.need$need=="unmet"]<-bennett.uci(near.re.need$female.pct[near.re.need$need=="unmet"],raab$gg_case[raab$gender=="female"],raab$near_need_denom[raab$gender=="female"]==1,raab$clusterId[raab$gender=="female"])
near.re.need$total.pct.uci[near.re.need$need=="unmet"]<-bennett.uci(near.re.need$total.pct[near.re.need$need=="unmet"],raab$gg_case,raab$near_need_denom==1,raab$clusterId)

near.re.need$male.pct.lci[near.re.need$need=="none"]<-bennett.lci(near.re.need$male.pct[near.re.need$need=="none"],raab$hh_case[raab$gender=="male"],raab$near_need_denom[raab$gender=="male"]==1,raab$clusterId[raab$gender=="male"])
near.re.need$female.pct.lci[near.re.need$need=="none"]<-bennett.lci(near.re.need$female.pct[near.re.need$need=="none"],raab$hh_case[raab$gender=="female"],raab$near_need_denom[raab$gender=="female"]==1,raab$clusterId[raab$gender=="female"])
near.re.need$total.pct.lci[near.re.need$need=="none"]<-bennett.lci(near.re.need$total.pct[near.re.need$need=="none"],raab$hh_case,raab$near_need_denom==1,raab$clusterId)

near.re.need$male.pct.uci[near.re.need$need=="none"]<-bennett.uci(near.re.need$male.pct[near.re.need$need=="none"],raab$hh_case[raab$gender=="male"],raab$near_need_denom[raab$gender=="male"]==1,raab$clusterId[raab$gender=="male"])
near.re.need$female.pct.uci[near.re.need$need=="none"]<-bennett.uci(near.re.need$female.pct[near.re.need$need=="none"],raab$hh_case[raab$gender=="female"],raab$near_need_denom[raab$gender=="female"]==1,raab$clusterId[raab$gender=="female"])
near.re.need$total.pct.uci[near.re.need$need=="none"]<-bennett.uci(near.re.need$total.pct[near.re.need$need=="none"],raab$hh_case,raab$near_need_denom==1,raab$clusterId)

near.re.need$male.adj.pct[near.re.need$need=="met"]<-prop.age.adjust(male.subpop,raab[raab$gender=="male",],raab$ee_case[raab$gender=="male"],raab$near_need_denom[raab$gender=="male"]==1)
near.re.need$female.adj.pct[near.re.need$need=="met"]<-prop.age.adjust(female.subpop,raab[raab$gender=="female",],raab$ee_case[raab$gender=="female"],raab$near_need_denom[raab$gender=="female"]==1)
near.re.need$total.adj.pct[near.re.need$need=="met"]<-prop.age.sex.adjust(popfives,raab,raab$ee_case,raab$near_need_denom==1)

near.re.need$male.adj.pct[near.re.need$need=="undermet"]<-prop.age.adjust(male.subpop,raab[raab$gender=="male",],raab$ff_case[raab$gender=="male"],raab$near_need_denom[raab$gender=="male"]==1)
near.re.need$female.adj.pct[near.re.need$need=="undermet"]<-prop.age.adjust(female.subpop,raab[raab$gender=="female",],raab$ff_case[raab$gender=="female"],raab$near_need_denom[raab$gender=="female"]==1)
near.re.need$total.adj.pct[near.re.need$need=="undermet"]<-prop.age.sex.adjust(popfives,raab,raab$ff_case,raab$near_need_denom==1)

near.re.need$male.adj.pct[near.re.need$need=="unmet"]<-prop.age.adjust(male.subpop,raab[raab$gender=="male",],raab$gg_case[raab$gender=="male"],raab$near_need_denom[raab$gender=="male"]==1)
near.re.need$female.adj.pct[near.re.need$need=="unmet"]<-prop.age.adjust(female.subpop,raab[raab$gender=="female",],raab$gg_case[raab$gender=="female"],raab$near_need_denom[raab$gender=="female"]==1)
near.re.need$total.adj.pct[near.re.need$need=="unmet"]<-prop.age.sex.adjust(popfives,raab,raab$gg_case,raab$near_need_denom==1)

near.re.need$male.adj.pct[near.re.need$need=="none"]<-prop.age.adjust(male.subpop,raab[raab$gender=="male",],raab$hh_case[raab$gender=="male"],raab$near_need_denom[raab$gender=="male"]==1)
near.re.need$female.adj.pct[near.re.need$need=="none"]<-prop.age.adjust(female.subpop,raab[raab$gender=="female",],raab$hh_case[raab$gender=="female"],raab$near_need_denom[raab$gender=="female"]==1)
near.re.need$total.adj.pct[near.re.need$need=="none"]<-prop.age.sex.adjust(popfives,raab,raab$hh_case,raab$near_need_denom==1)

near.re.need$male.adj.pct.lci[near.re.need$need=="met"]<-bennett.lci(near.re.need$male.adj.pct[near.re.need$need=="met"],raab$ee_case[raab$gender=="male"],raab$near_need_denom[raab$gender=="male"]==1,raab$clusterId[raab$gender=="male"])
near.re.need$female.adj.pct.lci[near.re.need$need=="met"]<-bennett.lci(near.re.need$female.adj.pct[near.re.need$need=="met"],raab$ee_case[raab$gender=="female"],raab$near_need_denom[raab$gender=="female"]==1,raab$clusterId[raab$gender=="female"])
near.re.need$total.adj.pct.lci[near.re.need$need=="met"]<-bennett.lci(near.re.need$total.adj.pct[near.re.need$need=="met"],raab$ee_case,raab$near_need_denom==1,raab$clusterId)

near.re.need$male.adj.pct.uci[near.re.need$need=="met"]<-bennett.uci(near.re.need$male.adj.pct[near.re.need$need=="met"],raab$ee_case[raab$gender=="male"],raab$near_need_denom[raab$gender=="male"]==1,raab$clusterId[raab$gender=="male"])
near.re.need$female.adj.pct.uci[near.re.need$need=="met"]<-bennett.uci(near.re.need$female.adj.pct[near.re.need$need=="met"],raab$ee_case[raab$gender=="female"],raab$near_need_denom[raab$gender=="female"]==1,raab$clusterId[raab$gender=="female"])
near.re.need$total.adj.pct.uci[near.re.need$need=="met"]<-bennett.uci(near.re.need$total.adj.pct[near.re.need$need=="met"],raab$ee_case,raab$near_need_denom==1,raab$clusterId)

near.re.need$male.adj.pct.lci[near.re.need$need=="undermet"]<-bennett.lci(near.re.need$male.adj.pct[near.re.need$need=="undermet"],raab$ff_case[raab$gender=="male"],raab$near_need_denom[raab$gender=="male"]==1,raab$clusterId[raab$gender=="male"])
near.re.need$female.adj.pct.lci[near.re.need$need=="undermet"]<-bennett.lci(near.re.need$female.adj.pct[near.re.need$need=="undermet"],raab$ff_case[raab$gender=="female"],raab$near_need_denom[raab$gender=="female"]==1,raab$clusterId[raab$gender=="female"])
near.re.need$total.adj.pct.lci[near.re.need$need=="undermet"]<-bennett.lci(near.re.need$total.adj.pct[near.re.need$need=="undermet"],raab$ff_case,raab$near_need_denom==1,raab$clusterId)

near.re.need$male.adj.pct.uci[near.re.need$need=="undermet"]<-bennett.uci(near.re.need$male.adj.pct[near.re.need$need=="undermet"],raab$ff_case[raab$gender=="male"],raab$near_need_denom[raab$gender=="male"]==1,raab$clusterId[raab$gender=="male"])
near.re.need$female.adj.pct.uci[near.re.need$need=="undermet"]<-bennett.uci(near.re.need$female.adj.pct[near.re.need$need=="undermet"],raab$ff_case[raab$gender=="female"],raab$near_need_denom[raab$gender=="female"]==1,raab$clusterId[raab$gender=="female"])
near.re.need$total.adj.pct.uci[near.re.need$need=="undermet"]<-bennett.uci(near.re.need$total.adj.pct[near.re.need$need=="undermet"],raab$ff_case,raab$near_need_denom==1,raab$clusterId)

near.re.need$male.adj.pct.lci[near.re.need$need=="unmet"]<-bennett.lci(near.re.need$male.adj.pct[near.re.need$need=="unmet"],raab$gg_case[raab$gender=="male"],raab$near_need_denom[raab$gender=="male"]==1,raab$clusterId[raab$gender=="male"])
near.re.need$female.adj.pct.lci[near.re.need$need=="unmet"]<-bennett.lci(near.re.need$female.adj.pct[near.re.need$need=="unmet"],raab$gg_case[raab$gender=="female"],raab$near_need_denom[raab$gender=="female"]==1,raab$clusterId[raab$gender=="female"])
near.re.need$total.adj.pct.lci[near.re.need$need=="unmet"]<-bennett.lci(near.re.need$total.adj.pct[near.re.need$need=="unmet"],raab$gg_case,raab$near_need_denom==1,raab$clusterId)

near.re.need$male.adj.pct.uci[near.re.need$need=="unmet"]<-bennett.uci(near.re.need$male.adj.pct[near.re.need$need=="unmet"],raab$gg_case[raab$gender=="male"],raab$near_need_denom[raab$gender=="male"]==1,raab$clusterId[raab$gender=="male"])
near.re.need$female.adj.pct.uci[near.re.need$need=="unmet"]<-bennett.uci(near.re.need$female.adj.pct[near.re.need$need=="unmet"],raab$gg_case[raab$gender=="female"],raab$near_need_denom[raab$gender=="female"]==1,raab$clusterId[raab$gender=="female"])
near.re.need$total.adj.pct.uci[near.re.need$need=="unmet"]<-bennett.uci(near.re.need$total.adj.pct[near.re.need$need=="unmet"],raab$gg_case,raab$near_need_denom==1,raab$clusterId)

near.re.need$male.adj.pct.lci[near.re.need$need=="none"]<-bennett.lci(near.re.need$male.adj.pct[near.re.need$need=="none"],raab$hh_case[raab$gender=="male"],raab$near_need_denom[raab$gender=="male"]==1,raab$clusterId[raab$gender=="male"])
near.re.need$female.adj.pct.lci[near.re.need$need=="none"]<-bennett.lci(near.re.need$female.adj.pct[near.re.need$need=="none"],raab$hh_case[raab$gender=="female"],raab$near_need_denom[raab$gender=="female"]==1,raab$clusterId[raab$gender=="female"])
near.re.need$total.adj.pct.lci[near.re.need$need=="none"]<-bennett.lci(near.re.need$total.adj.pct[near.re.need$need=="none"],raab$hh_case,raab$near_need_denom==1,raab$clusterId)

near.re.need$male.adj.pct.uci[near.re.need$need=="none"]<-bennett.uci(near.re.need$male.adj.pct[near.re.need$need=="none"],raab$hh_case[raab$gender=="male"],raab$near_need_denom[raab$gender=="male"]==1,raab$clusterId[raab$gender=="male"])
near.re.need$female.adj.pct.uci[near.re.need$need=="none"]<-bennett.uci(near.re.need$female.adj.pct[near.re.need$need=="none"],raab$hh_case[raab$gender=="female"],raab$near_need_denom[raab$gender=="female"]==1,raab$clusterId[raab$gender=="female"])
near.re.need$total.adj.pct.uci[near.re.need$need=="none"]<-bennett.uci(near.re.need$total.adj.pct[near.re.need$need=="none"],raab$hh_case,raab$near_need_denom==1,raab$clusterId)

near.re.need$male.extrapolated.n<-round( near.re.need$male.adj.pct*sum(male.subpop$population,na.rm=T),0)
near.re.need$female.extrapolated.n<-round( near.re.need$female.adj.pct*sum(female.subpop$population,na.rm=T),0)
near.re.need$total.extrapolated.n<-round( near.re.need$total.adj.pct*sum(popfives$population,na.rm=T),0)

lcis<-grep("lci",names(near.re.need))
ucis<-grep("uci",names(near.re.need))
near.re.need[,lcis][near.re.need[,lcis]<0]<-0
near.re.need[,ucis][near.re.need[,ucis]>1]<-1

pcts<-grep("pct",names(near.re.need))
near.re.need[,pcts]<-format(round(near.re.need[,pcts]*100,1),nsmall=1)
