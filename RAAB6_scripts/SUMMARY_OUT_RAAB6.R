#RAAB6

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
prebigboi6<-dcast(melt(catcovdenom[,c("out.names","male.adj.pct","male.adj.pct.lci","male.adj.pct.uci","female.adj.pct","female.adj.pct.lci","female.adj.pct.uci","total.adj.pct","total.adj.pct.lci","total.adj.pct.uci","extrapolated.male.n","extrapolated.female.n","extrapolated.total.n")], id.var="out.names"), 1~variable+out.names)
names(prebigboi6)<-gsub("\\ <","_",names(prebigboi6))
names(prebigboi6)<-gsub("/","",names(prebigboi6))

# Top three causes of blindness
causes<-sum6[1:13,]
causes<-causes[order(causes$blind.pct,decreasing=T),]
top_causes<-causes[1:3,c("principal.cause","blind.n","blind.pct")]
prebigboi7<-dcast(melt(top_causes,id.var="principal.cause"),1~variable+principal.cause)

# Refractive error prevalence
prebigboi8<-dcast(melt(newtab4[,c("vi.level","male.adj.pct","male.adj.pct.lci","male.adj.pct.uci","female.adj.pct","female.adj.pct.lci","female.adj.pct.uci","total.adj.pct","total.adj.pct.lci","total.adj.pct.uci","extrapolated.male.n","extrapolated.female.n","extrapolated.total.n")], id.var="vi.level"), 1~variable+vi.level)

# Distance eREC/REC
prebigboi9<-dcast(melt(newtab5,id.vars="rec_metric"), 1~variable+rec_metric)
names(prebigboi9)<-paste0(names(prebigboi9),"_pva")

# Equity dashboard
# blindness by age
blind_row <- equity.blindness.age[1, ]
age.working.adj.pct_blind<-blind_row$age_50_64.adj.pct
age.retired.adj.pct_blind<-blind_row$age_65_plus.adj.pct
prebigboi_age_blind <- data.frame(
  age.working.adj.pct_blind = age.working.adj.pct_blind,
  age.retired.adj.pct_blind = age.retired.adj.pct_blind
)
# ecsc by age
ecsc612_612_row <- equity.ecsc.age[equity.ecsc.age$num.thresh=="ecsc_612" & equity.ecsc.age$denom.thresh=="612",]
ecsc612_660_row <- equity.ecsc.age[equity.ecsc.age$num.thresh=="ecsc_612" & equity.ecsc.age$denom.thresh=="660",]
age.working.adjusted_ecsc_612_operable_thresh_612 <- ecsc612_612_row$age_50_64.adjusted
age.working.adjusted_ecsc_612_operable_thresh_660 <- ecsc612_660_row$age_50_64.adjusted
age.retired.adjusted_ecsc_612_operable_thresh_612 <- ecsc612_612_row$age_65_plus.adjusted
age.retired.adjusted_ecsc_612_operable_thresh_660 <- ecsc612_660_row$age_65_plus.adjusted
prebigboi_age_ecsc <- data.frame(
  age.working.adjusted_ecsc_612_operable_thresh_612 = age.working.adjusted_ecsc_612_operable_thresh_612,
  age.working.adjusted_ecsc_612_operable_thresh_660 = age.working.adjusted_ecsc_612_operable_thresh_660,
  age.retired.adjusted_ecsc_612_operable_thresh_612 = age.retired.adjusted_ecsc_612_operable_thresh_612,
  age.retired.adjusted_ecsc_612_operable_thresh_660 = age.retired.adjusted_ecsc_612_operable_thresh_660,
  stringsAsFactors = FALSE
)

#DR if used
if(sum(!is.na(DR_check$dr_diabetes_blood_consent)==TRUE)>0){
  
  dr.last.exam.table_12<-dr.last.exam.table[dr.last.exam.table$last.dr.exam=="dr_diabetic_last_exam_0_12_months",]
  prebigboi12<-dcast(melt(dr.last.exam.table_12, id.vars="last.dr.exam"), 1~variable+last.dr.exam)
  
}

# Dynamic inclusion of all prebigboi* objects including optional ones
bigboiTMP <- as.data.frame(Reduce("cbind", mget(ls(pattern = "^prebigboi"))))
loc_vars<-data.frame(iso_2=raab_meta$iso_2,year_end=raab_meta$year_end,gbd_reg=raab_meta$gbd_reg,gbd_superreg=raab_meta$gbd_superreg)
bigboi <- as.data.frame(cbind(bigboiTMP, loc_vars))

bigboi[bigboi=="*"]<-NA

bigboi <- cbind(
  raab_id = ID,
  bigboi
)

spots<-grep("^\\.",names(bigboi))
bigboi<-bigboi[,-spots]
