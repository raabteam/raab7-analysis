# table for near eREC and REC disaggregated by disability status

# ee=individuals with UCVA <N6 at 40 cm in the better eye who present with spectacles for near vision and whose PVA is ≥N6 in the better eye (met need) [NB the WHO paper box does not include "distance BCVA ≥6/12* in at least one eye" but the supp material flow chart does]
# ff=individuals with distance BCVA ≥6/12* in at least one eye who present with spectacles for near vision and whose PVA is <N6 in the better eye (undermet need)
# gg=individuals with distance BCVA ≥6/12 in at least one eye who do not have correction for near vision and whose UCVA is <N6 in the better eye (unmet need)
# hh_case = no need

# Only individuals with distance BCVA ≥6/12 will be considered in order to exclude those with reduced near vision not due to other causes.

raab$erec_near_num <-(raab$ee_case==1)+0
raab$rec_near_num <-(raab$ee_case==1 | raab$ff_case==1)+0
raab$erec_rec_near_denom <-(raab$ee_case==1 | raab$ff_case==1 | raab$gg_case==1)+0

erec_output_near <- c("near_erec","near_rec")

erec.near.dis<-data.frame(erec_output_near)
erec.near.dis[,2:19] <- NA
names(erec.near.dis) <- c("rec_metric",
                          
                          "any.dis.pct",
                          "any.dis.pct.lci",
                          "any.dis.pct.uci",
                          "any.dis.adj.pct",
                          "any.dis.adj.pct.lci",
                          "any.dis.adj.pct.uci",
                          
                          "any.non.vi.dis.pct",
                          "any.non.vi.dis.pct.lci",
                          "any.non.vi.dis.pct.uci",
                          "any.non.vi.dis.adj.pct",
                          "any.non.vi.dis.adj.pct.lci",
                          "any.non.vi.dis.adj.pct.uci",
                          
                          "no.dis.pct",
                          "no.dis.pct.lci",
                          "no.dis.pct.uci",
                          "no.dis.adj.pct",
                          "no.dis.adj.pct.lci",
                          "no.dis.adj.pct.uci")


#eREC and REC crude and adjusted estimates


# Crude results - erec
erec.near.dis$any.dis.pct[erec.near.dis$rec_metric=="near_erec"]        <- sum(raab$erec_near_num[raab$wgq.dis.any==1],na.rm=T)   / sum(raab$erec_rec_near_denom[raab$wgq.dis.any==1],na.rm=T)
erec.near.dis$any.non.vi.dis.pct[erec.near.dis$rec_metric=="near_erec"] <- sum(raab$erec_near_num[raab$wgq.dis.nonvi==1],na.rm=T) / sum(raab$erec_rec_near_denom[raab$wgq.dis.nonvi==1],na.rm=T)
erec.near.dis$no.dis.pct[erec.near.dis$rec_metric=="near_erec"]         <- sum(raab$erec_near_num[raab$wgq.dis.any==0],na.rm=T)   / sum(raab$erec_rec_near_denom[raab$wgq.dis.any==0],na.rm=T)

erec.near.dis$any.dis.pct.lci[erec.near.dis$rec_metric=="near_erec"]     <- bennett.lci(erec.near.dis$any.dis.pct[erec.near.dis$rec_metric=="near_erec"],
                                                                                        raab$erec_near_num[raab$wgq.dis.any==1],
                                                                                        raab$erec_rec_near_denom[raab$wgq.dis.any==1],
                                                                                        raab$clusterId[raab$wgq.dis.any==1])

erec.near.dis$any.non.vi.dis.pct.lci[erec.near.dis$rec_metric=="near_erec"]       <- bennett.lci(erec.near.dis$any.non.vi.dis.pct[erec.near.dis$rec_metric=="near_erec"],
                                                                                                 raab$erec_near_num[raab$wgq.dis.nonvi==1],
                                                                                                 raab$erec_rec_near_denom[raab$wgq.dis.nonvi==1],
                                                                                                 raab$clusterId[raab$wgq.dis.nonvi==1])

erec.near.dis$no.dis.pct.lci[erec.near.dis$rec_metric=="near_erec"]      <- bennett.lci(erec.near.dis$no.dis.pct[erec.near.dis$rec_metric=="near_erec"],
                                                                                        raab$erec_near_num[raab$wgq.dis.any==0],
                                                                                        raab$erec_rec_near_denom[raab$wgq.dis.any==0],
                                                                                        raab$clusterId[raab$wgq.dis.any==0])

erec.near.dis$any.dis.pct.uci[erec.near.dis$rec_metric=="near_erec"]     <- bennett.uci(erec.near.dis$any.dis.pct[erec.near.dis$rec_metric=="near_erec"],
                                                                                        raab$erec_near_num[raab$wgq.dis.any==1],
                                                                                        raab$erec_rec_near_denom[raab$wgq.dis.any==1],
                                                                                        raab$clusterId[raab$wgq.dis.any==1])

erec.near.dis$any.non.vi.dis.pct.uci[erec.near.dis$rec_metric=="near_erec"]       <- bennett.uci(erec.near.dis$any.non.vi.dis.pct[erec.near.dis$rec_metric=="near_erec"],
                                                                                                 raab$erec_near_num[raab$wgq.dis.nonvi==1],
                                                                                                 raab$erec_rec_near_denom[raab$wgq.dis.nonvi==1],
                                                                                                 raab$clusterId[raab$wgq.dis.nonvi==1])

erec.near.dis$no.dis.pct.uci[erec.near.dis$rec_metric=="near_erec"]      <- bennett.uci(erec.near.dis$no.dis.pct[erec.near.dis$rec_metric=="near_erec"],
                                                                                        raab$erec_near_num[raab$wgq.dis.any==0],
                                                                                        raab$erec_rec_near_denom[raab$wgq.dis.any==0],
                                                                                        raab$clusterId[raab$wgq.dis.any==0])
# Crude results - rec
erec.near.dis$any.dis.pct[erec.near.dis$rec_metric=="near_rec"]          <- sum(raab$rec_near_num[raab$wgq.dis.any==1],na.rm=T)   / sum(raab$erec_rec_near_denom[raab$wgq.dis.any==1],na.rm=T)
erec.near.dis$any.non.vi.dis.pct[erec.near.dis$rec_metric=="near_rec"]   <- sum(raab$rec_near_num[raab$wgq.dis.nonvi==1],na.rm=T) / sum(raab$erec_rec_near_denom[raab$wgq.dis.nonvi==1],na.rm=T)
erec.near.dis$no.dis.pct[erec.near.dis$rec_metric=="near_rec"]           <- sum(raab$rec_near_num[raab$wgq.dis.any==0],na.rm=T)   / sum(raab$erec_rec_near_denom[raab$wgq.dis.any==0],na.rm=T)

erec.near.dis$any.dis.pct.lci[erec.near.dis$rec_metric=="near_rec"]     <- bennett.lci(erec.near.dis$any.dis.pct[erec.near.dis$rec_metric=="near_rec"],
                                                                                       raab$erec_near_num[raab$wgq.dis.any==1],
                                                                                       raab$erec_rec_near_denom[raab$wgq.dis.any==1],
                                                                                       raab$clusterId[raab$wgq.dis.any==1])

erec.near.dis$any.non.vi.dis.pct.lci[erec.near.dis$rec_metric=="near_rec"]       <- bennett.lci(erec.near.dis$any.non.vi.dis.pct[erec.near.dis$rec_metric=="near_rec"],
                                                                                                raab$erec_near_num[raab$wgq.dis.nonvi==1],
                                                                                                raab$erec_rec_near_denom[raab$wgq.dis.nonvi==1],
                                                                                                raab$clusterId[raab$wgq.dis.nonvi==1])

erec.near.dis$no.dis.pct.lci[erec.near.dis$rec_metric=="near_rec"]      <- bennett.lci(erec.near.dis$no.dis.pct[erec.near.dis$rec_metric=="near_rec"],
                                                                                       raab$erec_near_num[raab$wgq.dis.any==0],
                                                                                       raab$erec_rec_near_denom[raab$wgq.dis.any==0],
                                                                                       raab$clusterId[raab$wgq.dis.any==0])

erec.near.dis$any.dis.pct.uci[erec.near.dis$rec_metric=="near_rec"]     <- bennett.uci(erec.near.dis$any.dis.pct[erec.near.dis$rec_metric=="near_rec"],
                                                                                       raab$erec_near_num[raab$wgq.dis.any==1],
                                                                                       raab$erec_rec_near_denom[raab$wgq.dis.any==1],
                                                                                       raab$clusterId[raab$wgq.dis.any==1])

erec.near.dis$any.non.vi.dis.pct.uci[erec.near.dis$rec_metric=="near_rec"]       <- bennett.uci(erec.near.dis$any.non.vi.dis.pct[erec.near.dis$rec_metric=="near_rec"],
                                                                                                raab$erec_near_num[raab$wgq.dis.nonvi==1],
                                                                                                raab$erec_rec_near_denom[raab$wgq.dis.nonvi==1],
                                                                                                raab$clusterId[raab$wgq.dis.nonvi==1])

erec.near.dis$no.dis.pct.uci[erec.near.dis$rec_metric=="near_rec"]      <- bennett.uci(erec.near.dis$no.dis.pct[erec.near.dis$rec_metric=="near_rec"],
                                                                                       raab$erec_near_num[raab$wgq.dis.any==0],
                                                                                       raab$erec_rec_near_denom[raab$wgq.dis.any==0],
                                                                                       raab$clusterId[raab$wgq.dis.any==0])

# Adjusted results - erec
erec.near.dis$any.dis.adj.pct[erec.near.dis$rec_metric=="near_erec"]        <- prop.age.sex.adjust(popfives,
                                                                                                   raab,
                                                                                                   raab$erec_near_num[raab$wgq.dis.any==1],
                                                                                                   raab$erec_rec_near_denom[raab$wgq.dis.any==1])

erec.near.dis$any.non.vi.dis.adj.pct[erec.near.dis$rec_metric=="near_erec"] <- prop.age.sex.adjust(popfives,
                                                                                                   raab,
                                                                                                   raab$erec_near_num[raab$wgq.dis.nonvi==1],
                                                                                                   raab$erec_rec_near_denom[raab$wgq.dis.nonvi==1])

erec.near.dis$no.dis.adj.pct[erec.near.dis$rec_metric=="near_erec"]         <- prop.age.sex.adjust(popfives,
                                                                                                   raab,
                                                                                                   raab$erec_near_num[raab$wgq.dis.any==0],
                                                                                                   raab$erec_rec_near_denom[raab$wgq.dis.any==0])

erec.near.dis$any.dis.adj.pct.lci[erec.near.dis$rec_metric=="near_erec"]     <- bennett.lci(erec.near.dis$any.dis.adj.pct[erec.near.dis$rec_metric=="near_erec"],
                                                                                            raab$erec_near_num[raab$wgq.dis.any==1],
                                                                                            raab$erec_rec_near_denom[raab$wgq.dis.any==1],
                                                                                            raab$clusterId[raab$wgq.dis.any==1])

erec.near.dis$any.non.vi.dis.adj.pct.lci[erec.near.dis$rec_metric=="near_erec"]       <- bennett.lci(erec.near.dis$any.non.vi.dis.adj.pct[erec.near.dis$rec_metric=="near_erec"],
                                                                                                     raab$erec_near_num[raab$wgq.dis.nonvi==1],
                                                                                                     raab$erec_rec_near_denom[raab$wgq.dis.nonvi==1],
                                                                                                     raab$clusterId[raab$wgq.dis.nonvi==1])

erec.near.dis$no.dis.adj.pct.lci[erec.near.dis$rec_metric=="near_erec"]      <- bennett.lci(erec.near.dis$no.dis.adj.pct[erec.near.dis$rec_metric=="near_erec"],
                                                                                            raab$erec_near_num[raab$wgq.dis.any==0],
                                                                                            raab$erec_rec_near_denom[raab$wgq.dis.any==0],
                                                                                            raab$clusterId[raab$wgq.dis.any==0])

erec.near.dis$any.dis.adj.pct.uci[erec.near.dis$rec_metric=="near_erec"]     <- bennett.uci(erec.near.dis$any.dis.adj.pct[erec.near.dis$rec_metric=="near_erec"],
                                                                                            raab$erec_near_num[raab$wgq.dis.any==1],
                                                                                            raab$erec_rec_near_denom[raab$wgq.dis.any==1],
                                                                                            raab$clusterId[raab$wgq.dis.any==1])

erec.near.dis$any.non.vi.dis.adj.pct.uci[erec.near.dis$rec_metric=="near_erec"]       <- bennett.uci(erec.near.dis$any.non.vi.dis.adj.pct[erec.near.dis$rec_metric=="near_erec"],
                                                                                                     raab$erec_near_num[raab$wgq.dis.nonvi==1],
                                                                                                     raab$erec_rec_near_denom[raab$wgq.dis.nonvi==1],
                                                                                                     raab$clusterId[raab$wgq.dis.nonvi==1])

erec.near.dis$no.dis.adj.pct.uci[erec.near.dis$rec_metric=="near_erec"]      <- bennett.uci(erec.near.dis$no.dis.adj.pct[erec.near.dis$rec_metric=="near_erec"],
                                                                                            raab$erec_near_num[raab$wgq.dis.any==0],
                                                                                            raab$erec_rec_near_denom[raab$wgq.dis.any==0],
                                                                                            raab$clusterId[raab$wgq.dis.any==0])
# Adjusted results - rec
erec.near.dis$any.dis.adj.pct[erec.near.dis$rec_metric=="near_rec"]        <- prop.age.sex.adjust(popfives,
                                                                                                  raab,
                                                                                                  raab$rec_near_num[raab$wgq.dis.any==1],
                                                                                                  raab$erec_rec_near_denom[raab$wgq.dis.any==1])

erec.near.dis$any.non.vi.dis.adj.pct[erec.near.dis$rec_metric=="near_rec"] <- prop.age.sex.adjust(popfives,
                                                                                                  raab,
                                                                                                  raab$rec_near_num[raab$wgq.dis.nonvi==1],
                                                                                                  raab$erec_rec_near_denom[raab$wgq.dis.nonvi==1])

erec.near.dis$no.dis.adj.pct[erec.near.dis$rec_metric=="near_rec"]         <- prop.age.sex.adjust(popfives,
                                                                                                  raab,
                                                                                                  raab$rec_near_num[raab$wgq.dis.any==0],
                                                                                                  raab$erec_rec_near_denom[raab$wgq.dis.any==0])

erec.near.dis$any.dis.adj.pct.lci[erec.near.dis$rec_metric=="near_rec"]     <- bennett.lci(erec.near.dis$any.dis.adj.pct[erec.near.dis$rec_metric=="near_rec"],
                                                                                           raab$erec_near_num[raab$wgq.dis.any==1],
                                                                                           raab$erec_rec_near_denom[raab$wgq.dis.any==1],
                                                                                           raab$clusterId[raab$wgq.dis.any==1])

erec.near.dis$any.non.vi.dis.adj.pct.lci[erec.near.dis$rec_metric=="near_rec"]       <- bennett.lci(erec.near.dis$any.non.vi.dis.adj.pct[erec.near.dis$rec_metric=="near_rec"],
                                                                                                    raab$erec_near_num[raab$wgq.dis.nonvi==1],
                                                                                                    raab$erec_rec_near_denom[raab$wgq.dis.nonvi==1],
                                                                                                    raab$clusterId[raab$wgq.dis.nonvi==1])

erec.near.dis$no.dis.adj.pct.lci[erec.near.dis$rec_metric=="near_rec"]      <- bennett.lci(erec.near.dis$no.dis.adj.pct[erec.near.dis$rec_metric=="near_rec"],
                                                                                           raab$erec_near_num[raab$wgq.dis.any==0],
                                                                                           raab$erec_rec_near_denom[raab$wgq.dis.any==0],
                                                                                           raab$clusterId[raab$wgq.dis.any==0])

erec.near.dis$any.dis.adj.pct.uci[erec.near.dis$rec_metric=="near_rec"]     <- bennett.uci(erec.near.dis$any.dis.adj.pct[erec.near.dis$rec_metric=="near_rec"],
                                                                                           raab$erec_near_num[raab$wgq.dis.any==1],
                                                                                           raab$erec_rec_near_denom[raab$wgq.dis.any==1],
                                                                                           raab$clusterId[raab$wgq.dis.any==1])

erec.near.dis$any.non.vi.dis.adj.pct.uci[erec.near.dis$rec_metric=="near_rec"]       <- bennett.uci(erec.near.dis$any.non.vi.dis.adj.pct[erec.near.dis$rec_metric=="near_rec"],
                                                                                                    raab$erec_near_num[raab$wgq.dis.nonvi==1],
                                                                                                    raab$erec_rec_near_denom[raab$wgq.dis.nonvi==1],
                                                                                                    raab$clusterId[raab$wgq.dis.nonvi==1])

erec.near.dis$no.dis.adj.pct.uci[erec.near.dis$rec_metric=="near_rec"]      <- bennett.uci(erec.near.dis$no.dis.adj.pct[erec.near.dis$rec_metric=="near_rec"],
                                                                                           raab$erec_near_num[raab$wgq.dis.any==0],
                                                                                           raab$erec_rec_near_denom[raab$wgq.dis.any==0],
                                                                                           raab$clusterId[raab$wgq.dis.any==0])

lcis<-grep("lci",names(erec.near.dis))
ucis<-grep("uci",names(erec.near.dis))
erec.near.dis[,lcis][erec.near.dis[,lcis]<0]<-0
erec.near.dis[,ucis][erec.near.dis[,ucis]>1]<-1

pcts<-grep("pct",names(erec.near.dis))
erec.near.dis[,pcts]<-round(erec.near.dis[,pcts]*100,1)
erec.near.dis[,pcts]<-format(erec.near.dis[,pcts], nsmall=1)

