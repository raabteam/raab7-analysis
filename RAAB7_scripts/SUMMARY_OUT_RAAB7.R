# RAAB7 output for main results on www.raab.world 
# From RAAB7s with distance UCVA and optional near VA

# Vision impairment
prebigboi1<-dcast(melt(sum3[,c("vi.level","female.adj.pct","female.adj.pct.lci","female.adj.pct.uci","male.adj.pct","male.adj.pct.lci","male.adj.pct.uci","total.adj.pct","total.adj.pct.lci","total.adj.pct.uci","extrapolated.female.n","extrapolated.male.n","extrapolated.total.n")], id.var="vi.level"), 1~variable+vi.level)

# eCSC/CSC
prev14_360<-prev14[prev14$denom.thresh==360 & (prev14$num.thresh=="csc" | prev14$num.thresh=="ecsc_612"),]
prev14_660<-prev14[prev14$denom.thresh==660 & (prev14$num.thresh=="csc" | prev14$num.thresh=="ecsc_612"),]
prev14_618<-prev14[prev14$denom.thresh==618 & (prev14$num.thresh=="csc" | prev14$num.thresh=="ecsc_612"),]
prev14_612<-prev14[prev14$denom.thresh==612 & (prev14$num.thresh=="csc" | prev14$num.thresh=="ecsc_612"),]

prebigboi2<-dcast(melt(prev14_360[,2:20], id.var="num.thresh"), 1~variable+num.thresh)
names(prebigboi2)<-paste0(names(prebigboi2),"_operable_thresh_360")
prebigboi3<-dcast(melt(prev14_660[,2:20], id.var="num.thresh"), 1~variable+num.thresh)
names(prebigboi3)<-paste0(names(prebigboi3),"_operable_thresh_660")
prebigboi4<-dcast(melt(prev14_618[,2:20], id.var="num.thresh"), 1~variable+num.thresh)
names(prebigboi4)<-paste0(names(prebigboi4),"_operable_thresh_618")
prebigboi5<-dcast(melt(prev14_612[,2:20], id.var="num.thresh"), 1~variable+num.thresh)
names(prebigboi5)<-paste0(names(prebigboi5),"_operable_thresh_612")

# Unmet need for cataract surgery
prebigboi6<-dcast(melt(asa7[,c("out.names","male.adj.pct","male.adj.pct.lci","male.adj.pct.uci","female.adj.pct","female.adj.pct.lci","female.adj.pct.uci","total.adj.pct","total.adj.pct.lci","total.adj.pct.uci","extrapolated.male.n","extrapolated.female.n","extrapolated.total.n")], id.var="out.names"), 1~variable+out.names)
names(prebigboi6)<-gsub("\\ <","_",names(prebigboi6))
names(prebigboi6)<-gsub("/","",names(prebigboi6))

# Top three causes of blindness
causes<-sum6[1:13,]
causes<-causes[order(causes$blind.pct,decreasing=T),]
top_causes<-causes[1:3,c("principal.cause","blind.n","blind.pct")]
prebigboi7<-dcast(melt(top_causes,id.var="principal.cause"),1~variable+principal.cause)

# Refractive error prevalence
prebigboi8<-dcast(melt(dist.re.prev.final,id.vars="age.groups.tens"), 1~variable+age.groups.tens)
names(prebigboi8)<-paste0(names(prebigboi8),"_dist_re_ucva")

# Distance eREC/REC
prebigboi9<-dcast(melt(newtab5,id.vars="rec_metric"), 1~variable+rec_metric)
names(prebigboi9)<-paste0(names(prebigboi9),"_ucva")

if("gbd_reg" %in% names(meta) & ("gbd_superreg" %in% names(meta))){
    loc_vars<-data.frame(iso_2=raab_meta$iso_2,year_end=raab_meta$year_end,gbd_reg=raab_meta$gbd_reg,gbd_superreg=raab_meta$gbd_superreg)
  }else{
    loc_vars<-data.frame(iso_2=raab_meta$iso_2,year_end=raab_meta$year_end)
  }

# Near VI/ near eREC if used
if(sum(!is.na(NV_check$binocular_near_corrected_result)==TRUE)>0){
  
  prebigboi10<-dcast(melt(near.vi.prev.final,id_vars="age.groups.tens"), 1~variable+age.groups.tens)
  names(prebigboi10)<-paste0(names(prebigboi10),"_near_re_ucva")
  
  prebigboi11<-dcast(melt(newtab7,id.vars="rec_metric"), 1~variable+rec_metric)
  names(prebigboi11)<-paste0(names(prebigboi11),"_ucva")
  
}

# Equity dashboard
# blindness by age
prebigboi_age_blind <- data.frame(
  age.working.adj.pct_blind = equity.blindness.age$age_50_64.adj.pct[1],
  age.retired.adj.pct_blind = equity.blindness.age$age_65_plus.adj.pct[1]
)

# ecsc by age
ecsc612_612_age_row <- equity.ecsc.age[equity.ecsc.age$num.thresh=="ecsc_612" & equity.ecsc.age$denom.thresh=="612", ]
ecsc612_660_age_row <- equity.ecsc.age[equity.ecsc.age$num.thresh=="ecsc_612" & equity.ecsc.age$denom.thresh=="660", ]

prebigboi_age_ecsc <- data.frame(
  age.working.adjusted_ecsc_612_operable_thresh_612 = ecsc612_612_age_row$age_50_64.adjusted,
  age.working.adjusted_ecsc_612_operable_thresh_660 = ecsc612_660_age_row$age_50_64.adjusted,
  age.retired.adjusted_ecsc_612_operable_thresh_612 = ecsc612_612_age_row$age_65_plus.adjusted,
  age.retired.adjusted_ecsc_612_operable_thresh_660 = ecsc612_660_age_row$age_65_plus.adjusted,
  stringsAsFactors = FALSE
)

# erec distance by age
prebigboi_age_erec_dist <- data.frame(
  age.working.adj.pct_dist_erec_ucva = equity.erec.dist.age$age_50_64.adjusted[equity.erec.dist.age$dist_rec_metric == "dist_erec"],
  age.retired.adj.pct_dist_erec_ucva = equity.erec.dist.age$age_65_plus.adjusted[equity.erec.dist.age$dist_rec_metric == "dist_erec"]
)

# erec near by age
if(sum(!is.na(NV_check$binocular_near_corrected_result)==TRUE)>0){
  
  prebigboi_age_erec_near <- data.frame(
    age.working.adj.pct_near_erec_ucva = equity.erec.near.age$age_50_64.adjusted[equity.erec.near.age$near_rec_metric == "near_erec"],
    age.retired.adj.pct_near_erec_ucva = equity.erec.near.age$age_65_plus.adjusted[equity.erec.near.age$near_rec_metric == "near_erec"]
  )
}

# blindness by disability if WGQ used
if(sum(!is.na(WQ_check$wg_difficulty_seeing)) + sum(!is.na(WQ_check$wg_difficulty_hearing)) + sum(!is.na(WQ_check$wg_difficulty_memory)) + sum(!is.na(WQ_check$wg_difficulty_mobility)) + sum(!is.na(WQ_check$wg_difficulty_selfcare)) > 0) {
  
  prebigboi_dis_blind <- data.frame(
    any.dis.adj.pct_blind = wgq.vi.table$any.dis.adj.pct[wgq.vi.table$vi.level == "blind"],
    no.dis.adj.pct_blind  = wgq.vi.table$no.dis.adj.pct[wgq.vi.table$vi.level == "blind"]
  )
  }

# ecsc by disability if WGQ used
if(sum(!is.na(WQ_check$wg_difficulty_seeing)) + sum(!is.na(WQ_check$wg_difficulty_hearing)) + sum(!is.na(WQ_check$wg_difficulty_memory)) + sum(!is.na(WQ_check$wg_difficulty_mobility)) + sum(!is.na(WQ_check$wg_difficulty_selfcare)) > 0) {

  ecsc612_612_dis_row <- wgq.ecsc[wgq.ecsc$num.thresh=="ecsc_612" & wgq.ecsc$denom.thresh=="612", ]
  ecsc612_660_dis_row <- wgq.ecsc[wgq.ecsc$num.thresh=="ecsc_612" & wgq.ecsc$denom.thresh=="660", ]
  
  prebigboi_dis_ecsc <- data.frame(
    any.dis.adjusted_ecsc_612_operable_thresh_612 = ecsc612_612_dis_row$any.dis.adjusted,
    no.dis.adjusted_ecsc_612_operable_thresh_612  = ecsc612_612_dis_row$no.dis.adjusted,
    any.dis.adjusted_ecsc_612_operable_thresh_660 = ecsc612_660_dis_row$any.dis.adjusted,
    no.dis.adjusted_ecsc_612_operable_thresh_660  = ecsc612_660_dis_row$no.dis.adjusted,
    stringsAsFactors = FALSE
  )
  }

# erec distance by disability if WGQ used
if(sum(!is.na(WQ_check$wg_difficulty_seeing)) + sum(!is.na(WQ_check$wg_difficulty_hearing)) + sum(!is.na(WQ_check$wg_difficulty_memory)) + sum(!is.na(WQ_check$wg_difficulty_mobility)) + sum(!is.na(WQ_check$wg_difficulty_selfcare)) > 0) {
  
  erec_dist_dis_row <- erec.dist.dis[erec.dist.dis$rec_metric=="dist_erec",]
  
  any.dis.adj.pct_dist_erec_ucva <- erec_dist_dis_row$any.dis.adj.pct
  no.dis.adj.pct_dist_erec_ucva <- erec_dist_dis_row$no.dis.adj.pct
  
  prebigboi_dist_erec_dis <- data.frame(
    any.dis.adj.pct_dist_erec_ucva = erec_dist_dis_row$any.dis.adj.pct,
    no.dis.adj.pct_dist_erec_ucva = erec_dist_dis_row$no.dis.adj.pct
  )
}

# erec near by disability if near va and WGQ used
if((sum(!is.na(NV_check$binocular_near_corrected_result)) >0) && (sum(!is.na(WQ_check$wg_difficulty_seeing)) + sum(!is.na(WQ_check$wg_difficulty_hearing)) + sum(!is.na(WQ_check$wg_difficulty_memory)) + sum(!is.na(WQ_check$wg_difficulty_mobility)) + sum(!is.na(WQ_check$wg_difficulty_selfcare)) > 0)) {

  erec_near_dis_row <- erec.near.dis[erec.near.dis$rec_metric=="near_erec",]
  
  prebigboi_near_erec_dis <- data.frame(
    any.dis.adj.pct_near_erec_ucva = erec_near_dis_row$any.dis.adj.pct,
    no.dis.adj.pct_near_erec_ucva = erec_near_dis_row$no.dis.adj.pct
  )
}

#DR if used
if(sum(!is.na(DR_check$dr_diabetes_blood_consent)==TRUE)>0){
  
  dr.last.exam.table_12<-dr.last.exam.table[dr.last.exam.table$last.dr.exam=="dr_diabetic_last_exam_0_12_months",]
  prebigboi12<-dcast(melt(dr.last.exam.table_12, id.vars="last.dr.exam"), 1~variable+last.dr.exam)
  
}

# Dynamic inclusion of all prebigboi* objects including optional ones
bigboiTMP <- as.data.frame(Reduce("cbind", mget(ls(pattern = "prebigboi*"))))
bigboi    <- as.data.frame(cbind(raab_id = ID, bigboiTMP, loc_vars))

bigboi[bigboi == "*"] <- NA

spots <- grep("^\\.", names(bigboi))
if (length(spots)) bigboi <- bigboi[, -spots]


