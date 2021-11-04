#v1 03/08/21 - IM
#v2 01/09/21 - RB

# eREC: Crude calculation using the gold standard definition

# aa = Individuals who present with spectacles or contact lenses for distance and whose UCVA is <6/12 in the better eye and CVA is 6/12 in the better eye (Met Need);
# bb = Individuals who present with spectacles or contact lenses for distance and whose UCVA is <6/12 in the better eye and whose CVA is <6/12 in the better eye, but who improve to 6/12 on PinVA (Undermet Need);
# cc = Individuals who present without spectacles and whose UCVA is <6/12 in the better eye and whose PinVA is 6/12 in the better eye (Unmet Need)

# Does b_Case need !=a_case or was that only in the alternative calc done for WHO last year??

raab$erec_num <-(raab$aa_case==1)+0
raab$rec_num <-(raab$aa_case==1 | raab$bb_case==1)+0
raab$erec_rec_denom <-(raab$aa_case==1 | raab$bb_case==1 | raab$cc_case==1)+0


# erec_qual_gap_eq <- 
# ignore this 'quality gap' between REC and eREC concept for now
  
erec_output <- c("erec","rec")

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

newtab5$female.pct[newtab5$rec_metric=="erec"] <- sum(raab$erec_num[raab$gender=="female"],na.rm=T)/sum(raab$erec_rec_denom[raab$gender=="female"],na.rm=T)
newtab5$male.pct[newtab5$rec_metric=="erec"] <- sum(raab$erec_num[raab$gender=="male"],na.rm=T)/sum(raab$erec_rec_denom[raab$gender=="male"],na.rm=T)
newtab5$total.pct[newtab5$rec_metric=="erec"] <- sum(raab$erec_num,na.rm=T)/sum(raab$erec_rec_denom,na.rm=T)

newtab5$female.pct[newtab5$rec_metric=="rec"] <- sum(raab$rec_num[raab$gender=="female"],na.rm=T)/sum(raab$erec_rec_denom[raab$gender=="female"],na.rm=T)
newtab5$male.pct[newtab5$rec_metric=="rec"] <- sum(raab$rec_num[raab$gender=="male"],na.rm=T)/sum(raab$erec_rec_denom[raab$gender=="male"],na.rm=T)
newtab5$total.pct[newtab5$rec_metric=="rec"] <- sum(raab$rec_num,na.rm=T)/sum(raab$erec_rec_denom,na.rm=T)

newtab5$female.pct.lci[newtab5$rec_metric=="erec"] <- bennett.lci(newtab5$female.pct[newtab5$rec_metric=="erec"],raab$erec_num[raab$gender=="female"],raab$erec_rec_denom[raab$gender=="female"],raab$clusterNumber[raab$gender=="female"])
newtab5$male.pct.lci[newtab5$rec_metric=="erec"] <- bennett.lci(newtab5$male.pct[newtab5$rec_metric=="erec"],raab$erec_num[raab$gender=="male"],raab$erec_rec_denom[raab$gender=="male"],raab$clusterNumber[raab$gender=="male"])
newtab5$total.pct.lci[newtab5$rec_metric=="erec"] <- bennett.lci(newtab5$total.pct[newtab5$rec_metric=="erec"],raab$erec_num,raab$erec_rec_denom,raab$clusterNumber)

newtab5$female.pct.uci[newtab5$rec_metric=="erec"] <- bennett.uci(newtab5$female.pct[newtab5$rec_metric=="erec"],raab$erec_num[raab$gender=="female"],raab$erec_rec_denom[raab$gender=="female"],raab$clusterNumber[raab$gender=="female"])
newtab5$male.pct.uci[newtab5$rec_metric=="erec"] <- bennett.uci(newtab5$male.pct[newtab5$rec_metric=="erec"],raab$erec_num[raab$gender=="male"],raab$erec_rec_denom[raab$gender=="male"],raab$clusterNumber[raab$gender=="male"])
newtab5$total.pct.uci[newtab5$rec_metric=="erec"] <- bennett.uci(newtab5$total.pct[newtab5$rec_metric=="erec"],raab$erec_num,raab$erec_rec_denom,raab$clusterNumber)

newtab5$female.pct.lci[newtab5$rec_metric=="rec"] <- bennett.lci(newtab5$female.pct[newtab5$rec_metric=="rec"],raab$rec_num[raab$gender=="female"],raab$erec_rec_denom[raab$gender=="female"],raab$clusterNumber[raab$gender=="female"])
newtab5$male.pct.lci[newtab5$rec_metric=="rec"] <- bennett.lci(newtab5$male.pct[newtab5$rec_metric=="rec"],raab$rec_num[raab$gender=="male"],raab$erec_rec_denom[raab$gender=="male"],raab$clusterNumber[raab$gender=="male"])
newtab5$total.pct.lci[newtab5$rec_metric=="rec"] <- bennett.lci(newtab5$total.pct[newtab5$rec_metric=="rec"],raab$rec_num,raab$erec_rec_denom,raab$clusterNumber)

newtab5$female.pct.uci[newtab5$rec_metric=="rec"] <- bennett.uci(newtab5$female.pct[newtab5$rec_metric=="rec"],raab$rec_num[raab$gender=="female"],raab$erec_rec_denom[raab$gender=="female"],raab$clusterNumber[raab$gender=="female"])
newtab5$male.pct.uci[newtab5$rec_metric=="rec"] <- bennett.uci(newtab5$male.pct[newtab5$rec_metric=="rec"],raab$rec_num[raab$gender=="male"],raab$erec_rec_denom[raab$gender=="male"],raab$clusterNumber[raab$gender=="male"])
newtab5$total.pct.uci[newtab5$rec_metric=="rec"] <- bennett.uci(newtab5$total.pct[newtab5$rec_metric=="rec"],raab$rec_num,raab$erec_rec_denom,raab$clusterNumber)

#eREC and REC adjusted proportion estimates

newtab5$female.adj.pct[newtab5$rec_metric=="erec"] <- prop.age.adjust(female.subpop,raab[raab$gender=="female",],raab$erec_num[raab$gender=="female"],raab$erec_rec_denom[raab$gender=="female"])
newtab5$male.adj.pct[newtab5$rec_metric=="erec"] <- prop.age.adjust(male.subpop,raab[raab$gender=="male",], raab$erec_num[raab$gender=="male"], raab$erec_rec_denom[raab$gender=="male"])
newtab5$total.adj.pct[newtab5$rec_metric=="erec"] <- prop.age.sex.adjust(popfives, raab, raab$erec_num, raab$erec_rec_denom)

newtab5$female.adj.pct[newtab5$rec_metric=="rec"] <- prop.age.adjust(female.subpop,raab[raab$gender=="female",],raab$rec_num[raab$gender=="female"],raab$erec_rec_denom[raab$gender=="female"])
newtab5$male.adj.pct[newtab5$rec_metric=="rec"] <- prop.age.adjust(male.subpop,raab[raab$gender=="male",], raab$rec_num[raab$gender=="male"], raab$erec_rec_denom[raab$gender=="male"])
newtab5$total.adj.pct[newtab5$rec_metric=="rec"] <- prop.age.sex.adjust(popfives, raab, raab$rec_num, raab$erec_rec_denom)

newtab5$female.adj.pct.lci[newtab5$rec_metric=="erec"] <- bennett.lci(newtab5$female.adj.pct[newtab5$rec_metric=="erec"],raab$erec_num[raab$gender=="female"],raab$erec_rec_denom[raab$gender=="female"],raab$clusterNumber[raab$gender=="female"])
newtab5$male.adj.pct.lci[newtab5$rec_metric=="erec"] <- bennett.lci(newtab5$male.adj.pct[newtab5$rec_metric=="erec"],raab$erec_num[raab$gender=="male"],raab$erec_rec_denom[raab$gender=="male"],raab$clusterNumber[raab$gender=="male"])
newtab5$total.adj.pct.lci[newtab5$rec_metric=="erec"] <- bennett.lci(newtab5$total.adj.pct[newtab5$rec_metric=="erec"],raab$erec_num,raab$erec_rec_denom,raab$clusterNumber)

newtab5$female.adj.pct.uci[newtab5$rec_metric=="erec"] <- bennett.uci(newtab5$female.adj.pct[newtab5$rec_metric=="erec"],raab$erec_num[raab$gender=="female"],raab$erec_rec_denom[raab$gender=="female"],raab$clusterNumber[raab$gender=="female"])
newtab5$male.adj.pct.uci[newtab5$rec_metric=="erec"] <- bennett.uci(newtab5$male.adj.pct[newtab5$rec_metric=="erec"],raab$erec_num[raab$gender=="male"],raab$erec_rec_denom[raab$gender=="male"],raab$clusterNumber[raab$gender=="male"])
newtab5$total.adj.pct.uci[newtab5$rec_metric=="erec"] <- bennett.uci(newtab5$total.adj.pct[newtab5$rec_metric=="erec"],raab$erec_num,raab$erec_rec_denom,raab$clusterNumber)

newtab5$female.adj.pct.lci[newtab5$rec_metric=="rec"] <- bennett.lci(newtab5$female.adj.pct[newtab5$rec_metric=="rec"],raab$rec_num[raab$gender=="female"],raab$erec_rec_denom[raab$gender=="female"],raab$clusterNumber[raab$gender=="female"])
newtab5$male.adj.pct.lci[newtab5$rec_metric=="rec"] <- bennett.lci(newtab5$male.adj.pct[newtab5$rec_metric=="rec"],raab$rec_num[raab$gender=="male"],raab$erec_rec_denom[raab$gender=="male"],raab$clusterNumber[raab$gender=="male"])
newtab5$total.adj.pct.lci[newtab5$rec_metric=="rec"] <- bennett.lci(newtab5$total.adj.pct[newtab5$rec_metric=="rec"],raab$rec_num,raab$erec_rec_denom,raab$clusterNumber)

newtab5$female.adj.pct.uci[newtab5$rec_metric=="rec"] <- bennett.uci(newtab5$female.adj.pct[newtab5$rec_metric=="rec"],raab$rec_num[raab$gender=="female"],raab$erec_rec_denom[raab$gender=="female"],raab$clusterNumber[raab$gender=="female"])
newtab5$male.adj.pct.uci[newtab5$rec_metric=="rec"] <- bennett.uci(newtab5$male.adj.pct[newtab5$rec_metric=="rec"],raab$rec_num[raab$gender=="male"],raab$erec_rec_denom[raab$gender=="male"],raab$clusterNumber[raab$gender=="male"])
newtab5$total.adj.pct.uci[newtab5$rec_metric=="rec"] <- bennett.uci(newtab5$total.adj.pct[newtab5$rec_metric=="rec"],raab$rec_num,raab$erec_rec_denom,raab$clusterNumber)

pcts<-grep("pct",names(newtab5))
newtab5[,pcts]<-round(newtab5[,pcts] * 100, 1)
lcis<-grep("lci",names(newtab5))
newtab5[,lcis][newtab5[,lcis]<0]<-0.0
ucis<-grep("uci",names(newtab5))
newtab5[,ucis][newtab5[,ucis]>100]<-100.0
