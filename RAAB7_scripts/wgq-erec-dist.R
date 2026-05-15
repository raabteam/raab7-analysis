# table for distance eREC and REC disaggregated by disability status

# aa = Individuals who present with spectacles or contact lenses for distance and whose UCVA is <6/12 in the better eye and CVA is 6/12 in the better eye (Met Need);
# bb = Individuals who present with spectacles or contact lenses for distance and whose UCVA is <6/12 in the better eye and whose CVA is <6/12 in the better eye, but who improve to 6/12 on PinVA (Undermet Need);
# cc = Individuals who present without spectacles and whose UCVA is <6/12 in the better eye and whose PinVA is 6/12 in the better eye (Unmet Need)
# dd = Individuals who are not aa, bb or cc cases (no need)

raab$erec_dist_num <-(raab$aa_case==1)+0
raab$rec_dist_num <-(raab$aa_case==1 | raab$bb_case==1)+0
raab$erec_rec_dist_denom <-(raab$aa_case==1 | raab$bb_case==1 | raab$cc_case==1)+0

erec_output_dist <- c("dist_erec","dist_rec")

erec.dist.dis<-data.frame(erec_output_dist)
erec.dist.dis[,2:19] <- NA
names(erec.dist.dis) <- c("rec_metric",
                         
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
  erec.dist.dis$any.dis.pct[erec.dist.dis$rec_metric=="dist_erec"]        <- sum(raab$erec_dist_num[raab$wgq.dis.any==1],na.rm=T)   / sum(raab$erec_rec_dist_denom[raab$wgq.dis.any==1],na.rm=T)
  erec.dist.dis$any.non.vi.dis.pct[erec.dist.dis$rec_metric=="dist_erec"] <- sum(raab$erec_dist_num[raab$wgq.dis.nonvi==1],na.rm=T) / sum(raab$erec_rec_dist_denom[raab$wgq.dis.nonvi==1],na.rm=T)
  erec.dist.dis$no.dis.pct[erec.dist.dis$rec_metric=="dist_erec"]         <- sum(raab$erec_dist_num[raab$wgq.dis.any==0],na.rm=T)   / sum(raab$erec_rec_dist_denom[raab$wgq.dis.any==0],na.rm=T)
  
  erec.dist.dis$any.dis.pct.lci[erec.dist.dis$rec_metric=="dist_erec"]     <- bennett.lci(erec.dist.dis$any.dis.pct[erec.dist.dis$rec_metric=="dist_erec"],
                                                                                         raab$erec_dist_num[raab$wgq.dis.any==1],
                                                                                         raab$erec_rec_dist_denom[raab$wgq.dis.any==1],
                                                                                         raab$clusterId[raab$wgq.dis.any==1])
  
  erec.dist.dis$any.non.vi.dis.pct.lci[erec.dist.dis$rec_metric=="dist_erec"]       <- bennett.lci(erec.dist.dis$any.non.vi.dis.pct[erec.dist.dis$rec_metric=="dist_erec"],
                                                                                         raab$erec_dist_num[raab$wgq.dis.nonvi==1],
                                                                                         raab$erec_rec_dist_denom[raab$wgq.dis.nonvi==1],
                                                                                         raab$clusterId[raab$wgq.dis.nonvi==1])
  
  erec.dist.dis$no.dis.pct.lci[erec.dist.dis$rec_metric=="dist_erec"]      <- bennett.lci(erec.dist.dis$no.dis.pct[erec.dist.dis$rec_metric=="dist_erec"],
                                                                                         raab$erec_dist_num[raab$wgq.dis.any==0],
                                                                                         raab$erec_rec_dist_denom[raab$wgq.dis.any==0],
                                                                                         raab$clusterId[raab$wgq.dis.any==0])
  
  erec.dist.dis$any.dis.pct.uci[erec.dist.dis$rec_metric=="dist_erec"]     <- bennett.uci(erec.dist.dis$any.dis.pct[erec.dist.dis$rec_metric=="dist_erec"],
                                                                                         raab$erec_dist_num[raab$wgq.dis.any==1],
                                                                                         raab$erec_rec_dist_denom[raab$wgq.dis.any==1],
                                                                                         raab$clusterId[raab$wgq.dis.any==1])
  
  erec.dist.dis$any.non.vi.dis.pct.uci[erec.dist.dis$rec_metric=="dist_erec"]       <- bennett.uci(erec.dist.dis$any.non.vi.dis.pct[erec.dist.dis$rec_metric=="dist_erec"],
                                                                                         raab$erec_dist_num[raab$wgq.dis.nonvi==1],
                                                                                         raab$erec_rec_dist_denom[raab$wgq.dis.nonvi==1],
                                                                                         raab$clusterId[raab$wgq.dis.nonvi==1])
  
  erec.dist.dis$no.dis.pct.uci[erec.dist.dis$rec_metric=="dist_erec"]      <- bennett.uci(erec.dist.dis$no.dis.pct[erec.dist.dis$rec_metric=="dist_erec"],
                                                                                         raab$erec_dist_num[raab$wgq.dis.any==0],
                                                                                         raab$erec_rec_dist_denom[raab$wgq.dis.any==0],
                                                                                         raab$clusterId[raab$wgq.dis.any==0])
  # Crude results - rec
  erec.dist.dis$any.dis.pct[erec.dist.dis$rec_metric=="dist_rec"]          <- sum(raab$rec_dist_num[raab$wgq.dis.any==1],na.rm=T)   / sum(raab$erec_rec_dist_denom[raab$wgq.dis.any==1],na.rm=T)
  erec.dist.dis$any.non.vi.dis.pct[erec.dist.dis$rec_metric=="dist_rec"]   <- sum(raab$rec_dist_num[raab$wgq.dis.nonvi==1],na.rm=T) / sum(raab$erec_rec_dist_denom[raab$wgq.dis.nonvi==1],na.rm=T)
  erec.dist.dis$no.dis.pct[erec.dist.dis$rec_metric=="dist_rec"]           <- sum(raab$rec_dist_num[raab$wgq.dis.any==0],na.rm=T)   / sum(raab$erec_rec_dist_denom[raab$wgq.dis.any==0],na.rm=T)
  
  erec.dist.dis$any.dis.pct.lci[erec.dist.dis$rec_metric=="dist_rec"]     <- bennett.lci(erec.dist.dis$any.dis.pct[erec.dist.dis$rec_metric=="dist_rec"],
                                                                                         raab$erec_dist_num[raab$wgq.dis.any==1],
                                                                                         raab$erec_rec_dist_denom[raab$wgq.dis.any==1],
                                                                                         raab$clusterId[raab$wgq.dis.any==1])
  
  erec.dist.dis$any.non.vi.dis.pct.lci[erec.dist.dis$rec_metric=="dist_rec"]       <- bennett.lci(erec.dist.dis$any.non.vi.dis.pct[erec.dist.dis$rec_metric=="dist_rec"],
                                                                                         raab$erec_dist_num[raab$wgq.dis.nonvi==1],
                                                                                         raab$erec_rec_dist_denom[raab$wgq.dis.nonvi==1],
                                                                                         raab$clusterId[raab$wgq.dis.nonvi==1])
  
  erec.dist.dis$no.dis.pct.lci[erec.dist.dis$rec_metric=="dist_rec"]      <- bennett.lci(erec.dist.dis$no.dis.pct[erec.dist.dis$rec_metric=="dist_rec"],
                                                                                         raab$erec_dist_num[raab$wgq.dis.any==0],
                                                                                         raab$erec_rec_dist_denom[raab$wgq.dis.any==0],
                                                                                         raab$clusterId[raab$wgq.dis.any==0])
  
  erec.dist.dis$any.dis.pct.uci[erec.dist.dis$rec_metric=="dist_rec"]     <- bennett.uci(erec.dist.dis$any.dis.pct[erec.dist.dis$rec_metric=="dist_rec"],
                                                                                         raab$erec_dist_num[raab$wgq.dis.any==1],
                                                                                         raab$erec_rec_dist_denom[raab$wgq.dis.any==1],
                                                                                         raab$clusterId[raab$wgq.dis.any==1])
  
  erec.dist.dis$any.non.vi.dis.pct.uci[erec.dist.dis$rec_metric=="dist_rec"]       <- bennett.uci(erec.dist.dis$any.non.vi.dis.pct[erec.dist.dis$rec_metric=="dist_rec"],
                                                                                         raab$erec_dist_num[raab$wgq.dis.nonvi==1],
                                                                                         raab$erec_rec_dist_denom[raab$wgq.dis.nonvi==1],
                                                                                         raab$clusterId[raab$wgq.dis.nonvi==1])
  
  erec.dist.dis$no.dis.pct.uci[erec.dist.dis$rec_metric=="dist_rec"]      <- bennett.uci(erec.dist.dis$no.dis.pct[erec.dist.dis$rec_metric=="dist_rec"],
                                                                                         raab$erec_dist_num[raab$wgq.dis.any==0],
                                                                                         raab$erec_rec_dist_denom[raab$wgq.dis.any==0],
                                                                                         raab$clusterId[raab$wgq.dis.any==0])
  
  # Adjusted results - erec
  erec.dist.dis$any.dis.adj.pct[erec.dist.dis$rec_metric=="dist_erec"]        <- prop.age.sex.adjust(popfives,
                                                                                                     raab,
                                                                                                     raab$erec_dist_num[raab$wgq.dis.any==1],
                                                                                                     raab$erec_rec_dist_denom[raab$wgq.dis.any==1])
    
  erec.dist.dis$any.non.vi.dis.adj.pct[erec.dist.dis$rec_metric=="dist_erec"] <- prop.age.sex.adjust(popfives,
                                                                                                     raab,
                                                                                                     raab$erec_dist_num[raab$wgq.dis.nonvi==1],
                                                                                                     raab$erec_rec_dist_denom[raab$wgq.dis.nonvi==1])
  
  erec.dist.dis$no.dis.adj.pct[erec.dist.dis$rec_metric=="dist_erec"]         <- prop.age.sex.adjust(popfives,
                                                                                                     raab,
                                                                                                     raab$erec_dist_num[raab$wgq.dis.any==0],
                                                                                                     raab$erec_rec_dist_denom[raab$wgq.dis.any==0])
  
  erec.dist.dis$any.dis.adj.pct.lci[erec.dist.dis$rec_metric=="dist_erec"]     <- bennett.lci(erec.dist.dis$any.dis.adj.pct[erec.dist.dis$rec_metric=="dist_erec"],
                                                                                          raab$erec_dist_num[raab$wgq.dis.any==1],
                                                                                          raab$erec_rec_dist_denom[raab$wgq.dis.any==1],
                                                                                          raab$clusterId[raab$wgq.dis.any==1])
  
  erec.dist.dis$any.non.vi.dis.adj.pct.lci[erec.dist.dis$rec_metric=="dist_erec"]       <- bennett.lci(erec.dist.dis$any.non.vi.dis.adj.pct[erec.dist.dis$rec_metric=="dist_erec"],
                                                                                                   raab$erec_dist_num[raab$wgq.dis.nonvi==1],
                                                                                                   raab$erec_rec_dist_denom[raab$wgq.dis.nonvi==1],
                                                                                                   raab$clusterId[raab$wgq.dis.nonvi==1])
  
  erec.dist.dis$no.dis.adj.pct.lci[erec.dist.dis$rec_metric=="dist_erec"]      <- bennett.lci(erec.dist.dis$no.dis.adj.pct[erec.dist.dis$rec_metric=="dist_erec"],
                                                                                          raab$erec_dist_num[raab$wgq.dis.any==0],
                                                                                          raab$erec_rec_dist_denom[raab$wgq.dis.any==0],
                                                                                          raab$clusterId[raab$wgq.dis.any==0])
  
  erec.dist.dis$any.dis.adj.pct.uci[erec.dist.dis$rec_metric=="dist_erec"]     <- bennett.uci(erec.dist.dis$any.dis.adj.pct[erec.dist.dis$rec_metric=="dist_erec"],
                                                                                          raab$erec_dist_num[raab$wgq.dis.any==1],
                                                                                          raab$erec_rec_dist_denom[raab$wgq.dis.any==1],
                                                                                          raab$clusterId[raab$wgq.dis.any==1])
  
  erec.dist.dis$any.non.vi.dis.adj.pct.uci[erec.dist.dis$rec_metric=="dist_erec"]       <- bennett.uci(erec.dist.dis$any.non.vi.dis.adj.pct[erec.dist.dis$rec_metric=="dist_erec"],
                                                                                                   raab$erec_dist_num[raab$wgq.dis.nonvi==1],
                                                                                                   raab$erec_rec_dist_denom[raab$wgq.dis.nonvi==1],
                                                                                                   raab$clusterId[raab$wgq.dis.nonvi==1])
  
  erec.dist.dis$no.dis.adj.pct.uci[erec.dist.dis$rec_metric=="dist_erec"]      <- bennett.uci(erec.dist.dis$no.dis.adj.pct[erec.dist.dis$rec_metric=="dist_erec"],
                                                                                          raab$erec_dist_num[raab$wgq.dis.any==0],
                                                                                          raab$erec_rec_dist_denom[raab$wgq.dis.any==0],
                                                                                          raab$clusterId[raab$wgq.dis.any==0])
  # Adjusted results - rec
  erec.dist.dis$any.dis.adj.pct[erec.dist.dis$rec_metric=="dist_rec"]        <- prop.age.sex.adjust(popfives,
                                                                                                     raab,
                                                                                                     raab$rec_dist_num[raab$wgq.dis.any==1],
                                                                                                     raab$erec_rec_dist_denom[raab$wgq.dis.any==1])
  
  erec.dist.dis$any.non.vi.dis.adj.pct[erec.dist.dis$rec_metric=="dist_rec"] <- prop.age.sex.adjust(popfives,
                                                                                                     raab,
                                                                                                     raab$rec_dist_num[raab$wgq.dis.nonvi==1],
                                                                                                     raab$erec_rec_dist_denom[raab$wgq.dis.nonvi==1])
  
  erec.dist.dis$no.dis.adj.pct[erec.dist.dis$rec_metric=="dist_rec"]         <- prop.age.sex.adjust(popfives,
                                                                                                     raab,
                                                                                                     raab$rec_dist_num[raab$wgq.dis.any==0],
                                                                                                     raab$erec_rec_dist_denom[raab$wgq.dis.any==0])
  
  erec.dist.dis$any.dis.adj.pct.lci[erec.dist.dis$rec_metric=="dist_rec"]     <- bennett.lci(erec.dist.dis$any.dis.adj.pct[erec.dist.dis$rec_metric=="dist_rec"],
                                                                                              raab$erec_dist_num[raab$wgq.dis.any==1],
                                                                                              raab$erec_rec_dist_denom[raab$wgq.dis.any==1],
                                                                                              raab$clusterId[raab$wgq.dis.any==1])
  
  erec.dist.dis$any.non.vi.dis.adj.pct.lci[erec.dist.dis$rec_metric=="dist_rec"]       <- bennett.lci(erec.dist.dis$any.non.vi.dis.adj.pct[erec.dist.dis$rec_metric=="dist_rec"],
                                                                                                       raab$erec_dist_num[raab$wgq.dis.nonvi==1],
                                                                                                       raab$erec_rec_dist_denom[raab$wgq.dis.nonvi==1],
                                                                                                       raab$clusterId[raab$wgq.dis.nonvi==1])
  
  erec.dist.dis$no.dis.adj.pct.lci[erec.dist.dis$rec_metric=="dist_rec"]      <- bennett.lci(erec.dist.dis$no.dis.adj.pct[erec.dist.dis$rec_metric=="dist_rec"],
                                                                                              raab$erec_dist_num[raab$wgq.dis.any==0],
                                                                                              raab$erec_rec_dist_denom[raab$wgq.dis.any==0],
                                                                                              raab$clusterId[raab$wgq.dis.any==0])
  
  erec.dist.dis$any.dis.adj.pct.uci[erec.dist.dis$rec_metric=="dist_rec"]     <- bennett.uci(erec.dist.dis$any.dis.adj.pct[erec.dist.dis$rec_metric=="dist_rec"],
                                                                                              raab$erec_dist_num[raab$wgq.dis.any==1],
                                                                                              raab$erec_rec_dist_denom[raab$wgq.dis.any==1],
                                                                                              raab$clusterId[raab$wgq.dis.any==1])
  
  erec.dist.dis$any.non.vi.dis.adj.pct.uci[erec.dist.dis$rec_metric=="dist_rec"]       <- bennett.uci(erec.dist.dis$any.non.vi.dis.adj.pct[erec.dist.dis$rec_metric=="dist_rec"],
                                                                                                       raab$erec_dist_num[raab$wgq.dis.nonvi==1],
                                                                                                       raab$erec_rec_dist_denom[raab$wgq.dis.nonvi==1],
                                                                                                       raab$clusterId[raab$wgq.dis.nonvi==1])
  
  erec.dist.dis$no.dis.adj.pct.uci[erec.dist.dis$rec_metric=="dist_rec"]      <- bennett.uci(erec.dist.dis$no.dis.adj.pct[erec.dist.dis$rec_metric=="dist_rec"],
                                                                                              raab$erec_dist_num[raab$wgq.dis.any==0],
                                                                                              raab$erec_rec_dist_denom[raab$wgq.dis.any==0],
                                                                                              raab$clusterId[raab$wgq.dis.any==0])

  lcis <- grep("lci", names(erec.dist.dis))
  ucis <- grep("uci", names(erec.dist.dis))
  pcts <- grep("pct", names(erec.dist.dis))
  
  # empty subgroups (0/0) produce NaN — set non-finite to NA before formatting
  erec.dist.dis[, pcts][!is.finite(as.matrix(erec.dist.dis[, pcts]))] <- NA
  
  erec.dist.dis[, lcis][erec.dist.dis[, lcis] < 0] <- 0
  erec.dist.dis[, ucis][erec.dist.dis[, ucis] > 1] <- 1
  
  erec.dist.dis[, pcts] <- round(erec.dist.dis[, pcts] * 100, 1)
  erec.dist.dis[, pcts] <- format(erec.dist.dis[, pcts], nsmall = 1)
  
  # format() stringifies NA as "NA" — convert back so kable(na=...) catches them
  erec.dist.dis[, pcts] <- lapply(erec.dist.dis[, pcts], function(x) {
    x <- trimws(x)
    x[x %in% c("NA", "NaN")] <- NA
    x
  })
