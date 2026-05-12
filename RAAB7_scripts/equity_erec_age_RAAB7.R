# output required in summary_out.csv for equity figure on raab.world

# sex-adjusted eREC by age groups (age_50_64 or age_65_plus)

# distance erec
raab$erec_dist_num <-(raab$aa_case==1)+0
raab$rec_dist_num <-(raab$aa_case==1 | raab$bb_case==1)+0
raab$erec_rec_dist_denom <-(raab$aa_case==1 | raab$bb_case==1 | raab$cc_case==1)+0


dist_rec_metric <- c("dist_rec","dist_erec")
equity.erec.dist.age <- as.data.frame(cbind(dist_rec_metric))

equity.erec.dist.age$age_50_64.denom[equity.erec.dist.age$dist_rec_metric=="dist_rec"]     <- sum(raab$erec_rec_dist_denom[raab$age.groups.working=="age_50_64"],    na.rm=T)
equity.erec.dist.age$age_50_64.denom[equity.erec.dist.age$dist_rec_metric=="dist_erec"]    <- sum(raab$erec_rec_dist_denom[raab$age.groups.working=="age_50_64"],   na.rm=T)
equity.erec.dist.age$age_65_plus.denom[equity.erec.dist.age$dist_rec_metric=="dist_rec"]   <- sum(raab$erec_rec_dist_denom[raab$age.groups.working=="age_65_plus"],  na.rm=T)
equity.erec.dist.age$age_65_plus.denom[equity.erec.dist.age$dist_rec_metric=="dist_erec"]  <- sum(raab$erec_rec_dist_denom[raab$age.groups.working=="age_65_plus"], na.rm=T)
equity.erec.dist.age$total.denom[equity.erec.dist.age$dist_rec_metric=="dist_rec"]         <- sum(raab$erec_rec_dist_denom,   na.rm=T)
equity.erec.dist.age$total.denom[equity.erec.dist.age$dist_rec_metric=="dist_erec"]        <- sum(raab$erec_rec_dist_denom,  na.rm=T)

equity.erec.dist.age$age_50_64.num[equity.erec.dist.age$dist_rec_metric=="dist_rec"]       <- sum(raab$rec_dist_num[raab$age.groups.working=="age_50_64"],      na.rm=T)
equity.erec.dist.age$age_50_64.num[equity.erec.dist.age$dist_rec_metric=="dist_erec"]      <- sum(raab$erec_dist_num[raab$age.groups.working=="age_50_64"],     na.rm=T)
equity.erec.dist.age$age_65_plus.num[equity.erec.dist.age$dist_rec_metric=="dist_rec"]     <- sum(raab$rec_dist_num[raab$age.groups.working=="age_65_plus"],    na.rm=T)
equity.erec.dist.age$age_65_plus.num[equity.erec.dist.age$dist_rec_metric=="dist_erec"]    <- sum(raab$erec_dist_num[raab$age.groups.working=="age_65_plus"],   na.rm=T)
equity.erec.dist.age$total.num[equity.erec.dist.age$dist_rec_metric=="dist_rec"]           <- sum(raab$rec_dist_num,     na.rm=T)
equity.erec.dist.age$total.num[equity.erec.dist.age$dist_rec_metric=="dist_erec"]          <- sum(raab$erec_dist_num,    na.rm=T)

# crude
equity.erec.dist.age$age_50_64.crude   <- equity.erec.dist.age$age_50_64.num   / equity.erec.dist.age$age_50_64.denom
equity.erec.dist.age$age_65_plus.crude <- equity.erec.dist.age$age_65_plus.num / equity.erec.dist.age$age_65_plus.denom
equity.erec.dist.age$total.crude       <- equity.erec.dist.age$total.num       / equity.erec.dist.age$total.denom

# adjusted
equity.erec.dist.age$age_50_64.adjusted[equity.erec.dist.age$dist_rec_metric=="dist_rec"]    <- prop.sex.adjust(age.5064.subpop, raab[raab$age.groups.working=="age_50_64",],  raab$rec_dist_num[raab$age.groups.working=="age_50_64"],    raab$erec_rec_dist_denom[raab$age.groups.working=="age_50_64"])
equity.erec.dist.age$age_50_64.adjusted[equity.erec.dist.age$dist_rec_metric=="dist_erec"]   <- prop.sex.adjust(age.5064.subpop, raab[raab$age.groups.working=="age_50_64",],  raab$erec_dist_num[raab$age.groups.working=="age_50_64"],   raab$erec_rec_dist_denom[raab$age.groups.working=="age_50_64"])
equity.erec.dist.age$age_65_plus.adjusted[equity.erec.dist.age$dist_rec_metric=="dist_rec"]  <- prop.sex.adjust(age.65p.subpop,  raab[raab$age.groups.working=="age_65_plus",], raab$rec_dist_num[raab$age.groups.working=="age_65_plus"],  raab$erec_rec_dist_denom[raab$age.groups.working=="age_65_plus"])
equity.erec.dist.age$age_65_plus.adjusted[equity.erec.dist.age$dist_rec_metric=="dist_erec"] <- prop.sex.adjust(age.65p.subpop,  raab[raab$age.groups.working=="age_65_plus",], raab$erec_dist_num[raab$age.groups.working=="age_65_plus"], raab$erec_rec_dist_denom[raab$age.groups.working=="age_65_plus"])
equity.erec.dist.age$total.adjusted[equity.erec.dist.age$dist_rec_metric=="dist_rec"]        <- prop.age.sex.adjust(popfives, raab, raab$rec_dist_num,   raab$erec_rec_dist_denom)
equity.erec.dist.age$total.adjusted[equity.erec.dist.age$dist_rec_metric=="dist_erec"]       <- prop.age.sex.adjust(popfives, raab, raab$erec_dist_num,  raab$erec_rec_dist_denom)

# CIs
equity.erec.dist.age$age_50_64.adjusted.lci[equity.erec.dist.age$dist_rec_metric=="dist_rec"]    <- bennett.lci(equity.erec.dist.age$age_50_64.adjusted[equity.erec.dist.age$dist_rec_metric=="dist_rec"],    raab$rec_dist_num[raab$age.groups.working=="age_50_64"],   raab$erec_rec_dist_denom[raab$age.groups.working=="age_50_64"],   raab$clusterId[raab$age.groups.working=="age_50_64"])
equity.erec.dist.age$age_50_64.adjusted.lci[equity.erec.dist.age$dist_rec_metric=="dist_erec"]   <- bennett.lci(equity.erec.dist.age$age_50_64.adjusted[equity.erec.dist.age$dist_rec_metric=="dist_erec"],   raab$erec_dist_num[raab$age.groups.working=="age_50_64"],  raab$erec_rec_dist_denom[raab$age.groups.working=="age_50_64"],  raab$clusterId[raab$age.groups.working=="age_50_64"])
equity.erec.dist.age$age_50_64.adjusted.uci[equity.erec.dist.age$dist_rec_metric=="dist_rec"]    <- bennett.uci(equity.erec.dist.age$age_50_64.adjusted[equity.erec.dist.age$dist_rec_metric=="dist_rec"],    raab$rec_dist_num[raab$age.groups.working=="age_50_64"],   raab$erec_rec_dist_denom[raab$age.groups.working=="age_50_64"],   raab$clusterId[raab$age.groups.working=="age_50_64"])
equity.erec.dist.age$age_50_64.adjusted.uci[equity.erec.dist.age$dist_rec_metric=="dist_erec"]   <- bennett.uci(equity.erec.dist.age$age_50_64.adjusted[equity.erec.dist.age$dist_rec_metric=="dist_erec"],   raab$erec_dist_num[raab$age.groups.working=="age_50_64"],  raab$erec_rec_dist_denom[raab$age.groups.working=="age_50_64"],  raab$clusterId[raab$age.groups.working=="age_50_64"])
equity.erec.dist.age$age_65_plus.adjusted.lci[equity.erec.dist.age$dist_rec_metric=="dist_rec"]  <- bennett.lci(equity.erec.dist.age$age_65_plus.adjusted[equity.erec.dist.age$dist_rec_metric=="dist_rec"],  raab$rec_dist_num[raab$age.groups.working=="age_65_plus"], raab$erec_rec_dist_denom[raab$age.groups.working=="age_65_plus"], raab$clusterId[raab$age.groups.working=="age_65_plus"])
equity.erec.dist.age$age_65_plus.adjusted.lci[equity.erec.dist.age$dist_rec_metric=="dist_erec"] <- bennett.lci(equity.erec.dist.age$age_65_plus.adjusted[equity.erec.dist.age$dist_rec_metric=="dist_erec"], raab$erec_dist_num[raab$age.groups.working=="age_65_plus"],raab$erec_rec_dist_denom[raab$age.groups.working=="age_65_plus"],raab$clusterId[raab$age.groups.working=="age_65_plus"])
equity.erec.dist.age$age_65_plus.adjusted.uci[equity.erec.dist.age$dist_rec_metric=="dist_rec"]  <- bennett.uci(equity.erec.dist.age$age_65_plus.adjusted[equity.erec.dist.age$dist_rec_metric=="dist_rec"],  raab$rec_dist_num[raab$age.groups.working=="age_65_plus"], raab$erec_rec_dist_denom[raab$age.groups.working=="age_65_plus"], raab$clusterId[raab$age.groups.working=="age_65_plus"])
equity.erec.dist.age$age_65_plus.adjusted.uci[equity.erec.dist.age$dist_rec_metric=="dist_erec"] <- bennett.uci(equity.erec.dist.age$age_65_plus.adjusted[equity.erec.dist.age$dist_rec_metric=="dist_erec"], raab$erec_dist_num[raab$age.groups.working=="age_65_plus"],raab$erec_rec_dist_denom[raab$age.groups.working=="age_65_plus"],raab$clusterId[raab$age.groups.working=="age_65_plus"])
equity.erec.dist.age$total.adjusted.lci[equity.erec.dist.age$dist_rec_metric=="dist_rec"]        <- bennett.lci(equity.erec.dist.age$total.adjusted[equity.erec.dist.age$dist_rec_metric=="dist_rec"],        raab$rec_dist_num,   raab$erec_rec_dist_denom,   raab$clusterId)
equity.erec.dist.age$total.adjusted.lci[equity.erec.dist.age$dist_rec_metric=="dist_erec"]       <- bennett.lci(equity.erec.dist.age$total.adjusted[equity.erec.dist.age$dist_rec_metric=="dist_erec"],       raab$erec_dist_num,  raab$erec_rec_dist_denom,  raab$clusterId)
equity.erec.dist.age$total.adjusted.uci[equity.erec.dist.age$dist_rec_metric=="dist_rec"]        <- bennett.uci(equity.erec.dist.age$total.adjusted[equity.erec.dist.age$dist_rec_metric=="dist_rec"],        raab$rec_dist_num,   raab$erec_rec_dist_denom,   raab$clusterId)
equity.erec.dist.age$total.adjusted.uci[equity.erec.dist.age$dist_rec_metric=="dist_erec"]       <- bennett.uci(equity.erec.dist.age$total.adjusted[equity.erec.dist.age$dist_rec_metric=="dist_erec"],       raab$erec_dist_num,  raab$erec_rec_dist_denom,  raab$clusterId)

# quality gap = (rec - erec) / rec
equity.erec.dist.age$quality_gap <- NA
equity.erec.dist.age$quality_gap[equity.erec.dist.age$dist_rec_metric=="dist_erec"] <- (equity.erec.dist.age$total.adjusted[equity.erec.dist.age$dist_rec_metric=="dist_rec"] - equity.erec.dist.age$total.adjusted[equity.erec.dist.age$dist_rec_metric=="dist_erec"]) / equity.erec.dist.age$total.adjusted[equity.erec.dist.age$dist_rec_metric=="dist_rec"]

# format
lcis <- grep("lci", names(equity.erec.dist.age))
ucis <- grep("uci", names(equity.erec.dist.age))
equity.erec.dist.age[, lcis][equity.erec.dist.age[, lcis] < 0] <- 0
equity.erec.dist.age[, ucis][equity.erec.dist.age[, ucis] > 1] <- 1

apcts <- grep("adjusted", names(equity.erec.dist.age))
equity.erec.dist.age[, apcts] <- round(equity.erec.dist.age[, apcts] * 100, 1)
equity.erec.dist.age[, apcts] <- format(equity.erec.dist.age[, apcts], nsmall = 1)

cpcts <- grep("crude", names(equity.erec.dist.age))
equity.erec.dist.age[, cpcts] <- round(equity.erec.dist.age[, cpcts] * 100, 1)
equity.erec.dist.age[, cpcts] <- format(equity.erec.dist.age[, cpcts], nsmall = 1)

equity.erec.dist.age$quality_gap <- round(equity.erec.dist.age$quality_gap * 100, 1)
equity.erec.dist.age$quality_gap[is.na(equity.erec.dist.age$quality_gap)] <- ""
equity.erec.dist.age$quality_gap <- format(equity.erec.dist.age$quality_gap, nsmall = 1)


# near erec conditional on near va testing done
if(sum(!is.na(NV_check$binocular_near_corrected_result)==TRUE)>0){
  
  raab$erec_near_num <-(raab$ee_case==1)+0
  raab$rec_near_num <-(raab$ee_case==1 | raab$ff_case==1)+0
  raab$erec_rec_near_denom <-(raab$ee_case==1 | raab$ff_case==1 | raab$gg_case==1)+0
  
  near_rec_metric <- c("near_rec","near_erec")
  equity.erec.near.age <- as.data.frame(cbind(near_rec_metric))
  
  equity.erec.near.age$age_50_64.denom[equity.erec.near.age$near_rec_metric=="near_rec"]     <- sum(raab$erec_rec_near_denom[raab$age.groups.working=="age_50_64"],    na.rm=T)
  equity.erec.near.age$age_50_64.denom[equity.erec.near.age$near_rec_metric=="near_erec"]    <- sum(raab$erec_rec_near_denom[raab$age.groups.working=="age_50_64"],   na.rm=T)
  equity.erec.near.age$age_65_plus.denom[equity.erec.near.age$near_rec_metric=="near_rec"]   <- sum(raab$erec_rec_near_denom[raab$age.groups.working=="age_65_plus"],  na.rm=T)
  equity.erec.near.age$age_65_plus.denom[equity.erec.near.age$near_rec_metric=="near_erec"]  <- sum(raab$erec_rec_near_denom[raab$age.groups.working=="age_65_plus"], na.rm=T)
  equity.erec.near.age$total.denom[equity.erec.near.age$near_rec_metric=="near_rec"]         <- sum(raab$erec_rec_near_denom,   na.rm=T)
  equity.erec.near.age$total.denom[equity.erec.near.age$near_rec_metric=="near_erec"]        <- sum(raab$erec_rec_near_denom,  na.rm=T)
  
  equity.erec.near.age$age_50_64.num[equity.erec.near.age$near_rec_metric=="near_rec"]       <- sum(raab$rec_near_num[raab$age.groups.working=="age_50_64"],      na.rm=T)
  equity.erec.near.age$age_50_64.num[equity.erec.near.age$near_rec_metric=="near_erec"]      <- sum(raab$erec_near_num[raab$age.groups.working=="age_50_64"],     na.rm=T)
  equity.erec.near.age$age_65_plus.num[equity.erec.near.age$near_rec_metric=="near_rec"]     <- sum(raab$rec_near_num[raab$age.groups.working=="age_65_plus"],    na.rm=T)
  equity.erec.near.age$age_65_plus.num[equity.erec.near.age$near_rec_metric=="near_erec"]    <- sum(raab$erec_near_num[raab$age.groups.working=="age_65_plus"],   na.rm=T)
  equity.erec.near.age$total.num[equity.erec.near.age$near_rec_metric=="near_rec"]           <- sum(raab$rec_near_num,     na.rm=T)
  equity.erec.near.age$total.num[equity.erec.near.age$near_rec_metric=="near_erec"]          <- sum(raab$erec_near_num,    na.rm=T)
  
  # crude
  equity.erec.near.age$age_50_64.crude   <- equity.erec.near.age$age_50_64.num   / equity.erec.near.age$age_50_64.denom
  equity.erec.near.age$age_65_plus.crude <- equity.erec.near.age$age_65_plus.num / equity.erec.near.age$age_65_plus.denom
  equity.erec.near.age$total.crude       <- equity.erec.near.age$total.num       / equity.erec.near.age$total.denom
  
  # adjusted
  equity.erec.near.age$age_50_64.adjusted[equity.erec.near.age$near_rec_metric=="near_rec"]    <- prop.sex.adjust(age.5064.subpop, raab[raab$age.groups.working=="age_50_64",],  raab$rec_near_num[raab$age.groups.working=="age_50_64"],    raab$erec_rec_near_denom[raab$age.groups.working=="age_50_64"])
  equity.erec.near.age$age_50_64.adjusted[equity.erec.near.age$near_rec_metric=="near_erec"]   <- prop.sex.adjust(age.5064.subpop, raab[raab$age.groups.working=="age_50_64",],  raab$erec_near_num[raab$age.groups.working=="age_50_64"],   raab$erec_rec_near_denom[raab$age.groups.working=="age_50_64"])
  equity.erec.near.age$age_65_plus.adjusted[equity.erec.near.age$near_rec_metric=="near_rec"]  <- prop.sex.adjust(age.65p.subpop,  raab[raab$age.groups.working=="age_65_plus",], raab$rec_near_num[raab$age.groups.working=="age_65_plus"],  raab$erec_rec_near_denom[raab$age.groups.working=="age_65_plus"])
  equity.erec.near.age$age_65_plus.adjusted[equity.erec.near.age$near_rec_metric=="near_erec"] <- prop.sex.adjust(age.65p.subpop,  raab[raab$age.groups.working=="age_65_plus",], raab$erec_near_num[raab$age.groups.working=="age_65_plus"], raab$erec_rec_near_denom[raab$age.groups.working=="age_65_plus"])
  equity.erec.near.age$total.adjusted[equity.erec.near.age$near_rec_metric=="near_rec"]        <- prop.age.sex.adjust(popfives, raab, raab$rec_near_num,   raab$erec_rec_near_denom)
  equity.erec.near.age$total.adjusted[equity.erec.near.age$near_rec_metric=="near_erec"]       <- prop.age.sex.adjust(popfives, raab, raab$erec_near_num,  raab$erec_rec_near_denom)
  
  # CIs
  equity.erec.near.age$age_50_64.adjusted.lci[equity.erec.near.age$near_rec_metric=="near_rec"]    <- bennett.lci(equity.erec.near.age$age_50_64.adjusted[equity.erec.near.age$near_rec_metric=="near_rec"],    raab$rec_near_num[raab$age.groups.working=="age_50_64"],   raab$erec_rec_near_denom[raab$age.groups.working=="age_50_64"],   raab$clusterId[raab$age.groups.working=="age_50_64"])
  equity.erec.near.age$age_50_64.adjusted.lci[equity.erec.near.age$near_rec_metric=="near_erec"]   <- bennett.lci(equity.erec.near.age$age_50_64.adjusted[equity.erec.near.age$near_rec_metric=="near_erec"],   raab$erec_near_num[raab$age.groups.working=="age_50_64"],  raab$erec_rec_near_denom[raab$age.groups.working=="age_50_64"],  raab$clusterId[raab$age.groups.working=="age_50_64"])
  equity.erec.near.age$age_50_64.adjusted.uci[equity.erec.near.age$near_rec_metric=="near_rec"]    <- bennett.uci(equity.erec.near.age$age_50_64.adjusted[equity.erec.near.age$near_rec_metric=="near_rec"],    raab$rec_near_num[raab$age.groups.working=="age_50_64"],   raab$erec_rec_near_denom[raab$age.groups.working=="age_50_64"],   raab$clusterId[raab$age.groups.working=="age_50_64"])
  equity.erec.near.age$age_50_64.adjusted.uci[equity.erec.near.age$near_rec_metric=="near_erec"]   <- bennett.uci(equity.erec.near.age$age_50_64.adjusted[equity.erec.near.age$near_rec_metric=="near_erec"],   raab$erec_near_num[raab$age.groups.working=="age_50_64"],  raab$erec_rec_near_denom[raab$age.groups.working=="age_50_64"],  raab$clusterId[raab$age.groups.working=="age_50_64"])
  equity.erec.near.age$age_65_plus.adjusted.lci[equity.erec.near.age$near_rec_metric=="near_rec"]  <- bennett.lci(equity.erec.near.age$age_65_plus.adjusted[equity.erec.near.age$near_rec_metric=="near_rec"],  raab$rec_near_num[raab$age.groups.working=="age_65_plus"], raab$erec_rec_near_denom[raab$age.groups.working=="age_65_plus"], raab$clusterId[raab$age.groups.working=="age_65_plus"])
  equity.erec.near.age$age_65_plus.adjusted.lci[equity.erec.near.age$near_rec_metric=="near_erec"] <- bennett.lci(equity.erec.near.age$age_65_plus.adjusted[equity.erec.near.age$near_rec_metric=="near_erec"], raab$erec_near_num[raab$age.groups.working=="age_65_plus"],raab$erec_rec_near_denom[raab$age.groups.working=="age_65_plus"],raab$clusterId[raab$age.groups.working=="age_65_plus"])
  equity.erec.near.age$age_65_plus.adjusted.uci[equity.erec.near.age$near_rec_metric=="near_rec"]  <- bennett.uci(equity.erec.near.age$age_65_plus.adjusted[equity.erec.near.age$near_rec_metric=="near_rec"],  raab$rec_near_num[raab$age.groups.working=="age_65_plus"], raab$erec_rec_near_denom[raab$age.groups.working=="age_65_plus"], raab$clusterId[raab$age.groups.working=="age_65_plus"])
  equity.erec.near.age$age_65_plus.adjusted.uci[equity.erec.near.age$near_rec_metric=="near_erec"] <- bennett.uci(equity.erec.near.age$age_65_plus.adjusted[equity.erec.near.age$near_rec_metric=="near_erec"], raab$erec_near_num[raab$age.groups.working=="age_65_plus"],raab$erec_rec_near_denom[raab$age.groups.working=="age_65_plus"],raab$clusterId[raab$age.groups.working=="age_65_plus"])
  equity.erec.near.age$total.adjusted.lci[equity.erec.near.age$near_rec_metric=="near_rec"]        <- bennett.lci(equity.erec.near.age$total.adjusted[equity.erec.near.age$near_rec_metric=="near_rec"],        raab$rec_near_num,   raab$erec_rec_near_denom,   raab$clusterId)
  equity.erec.near.age$total.adjusted.lci[equity.erec.near.age$near_rec_metric=="near_erec"]       <- bennett.lci(equity.erec.near.age$total.adjusted[equity.erec.near.age$near_rec_metric=="near_erec"],       raab$erec_near_num,  raab$erec_rec_near_denom,  raab$clusterId)
  equity.erec.near.age$total.adjusted.uci[equity.erec.near.age$near_rec_metric=="near_rec"]        <- bennett.uci(equity.erec.near.age$total.adjusted[equity.erec.near.age$near_rec_metric=="near_rec"],        raab$rec_near_num,   raab$erec_rec_near_denom,   raab$clusterId)
  equity.erec.near.age$total.adjusted.uci[equity.erec.near.age$near_rec_metric=="near_erec"]       <- bennett.uci(equity.erec.near.age$total.adjusted[equity.erec.near.age$near_rec_metric=="near_erec"],       raab$erec_near_num,  raab$erec_rec_near_denom,  raab$clusterId)
  
  # quality gap = (rec - erec) / rec
  equity.erec.near.age$quality_gap <- NA
  equity.erec.near.age$quality_gap[equity.erec.near.age$near_rec_metric=="near_erec"] <- (equity.erec.near.age$total.adjusted[equity.erec.near.age$near_rec_metric=="near_rec"] - equity.erec.near.age$total.adjusted[equity.erec.near.age$near_rec_metric=="near_erec"]) / equity.erec.near.age$total.adjusted[equity.erec.near.age$near_rec_metric=="near_rec"]
  
  # format
  lcis <- grep("lci", names(equity.erec.near.age))
  ucis <- grep("uci", names(equity.erec.near.age))
  equity.erec.near.age[, lcis][equity.erec.near.age[, lcis] < 0] <- 0
  equity.erec.near.age[, ucis][equity.erec.near.age[, ucis] > 1] <- 1
  
  apcts <- grep("adjusted", names(equity.erec.near.age))
  equity.erec.near.age[, apcts] <- round(equity.erec.near.age[, apcts] * 100, 1)
  equity.erec.near.age[, apcts] <- format(equity.erec.near.age[, apcts], nsmall = 1)
  
  cpcts <- grep("crude", names(equity.erec.near.age))
  equity.erec.near.age[, cpcts] <- round(equity.erec.near.age[, cpcts] * 100, 1)
  equity.erec.near.age[, cpcts] <- format(equity.erec.near.age[, cpcts], nsmall = 1)
  
  equity.erec.near.age$quality_gap <- round(equity.erec.near.age$quality_gap * 100, 1)
  equity.erec.near.age$quality_gap[is.na(equity.erec.near.age$quality_gap)] <- ""
  equity.erec.near.age$quality_gap <- format(equity.erec.near.age$quality_gap, nsmall = 1)
  
}
