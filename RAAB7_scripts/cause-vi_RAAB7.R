#RAAB7

#v1 - 16/09/21 - RB
#v2 - 09/08/22 - IM - adding myopic degeneration as a cause of VI to list of causes

sum6<-data.frame(raab.cause)
sum6[,2:9] <- NA
names(sum6) <- c("principal.cause",
                             
                             "blind.n",
                             "blind.pct",
                             
                             "severe.vi.n",
                             "severe.vi.pct",
                             
                             "moderate.vi.n",
                             "moderate.vi.pct",
                             
                             "mild.vi.n",
                             "mild.vi.pct"
)

for(i in seq_along(raab.cause)) 
  
  {
  
  sum6$blind.n[i]<-sum(raab$poor_vision_cause_principle[raab$blind==1]==raab.cause[i],na.rm=T)
  sum6$blind.pct[i]<-sum6$blind.n[i]/sum(raab$blind==1,na.rm=T)
  
  sum6$severe.vi.n[i]<-sum(raab$poor_vision_cause_principle[raab$severe.vi==1]==raab.cause[i],na.rm=T)
  sum6$severe.vi.pct[i]<-sum6$severe.vi.n[i]/sum(raab$severe.vi==1,na.rm=T)
  
  sum6$moderate.vi.n[i]<-sum(raab$poor_vision_cause_principle[raab$moderate.vi==1]==raab.cause[i],na.rm=T)
  sum6$moderate.vi.pct[i]<-sum6$moderate.vi.n[i]/sum(raab$moderate.vi==1,na.rm=T)
  
  sum6$mild.vi.n[i]<-sum(raab$poor_vision_cause_principle[raab$mild.vi==1]==raab.cause[i],na.rm=T)
  sum6$mild.vi.pct[i]<-sum6$mild.vi.n[i]/sum(raab$mild.vi==1,na.rm=T)
  
}

sum6[nrow(sum6)+1,2:9]<-colSums(sum6[,2:9])
sum6$principal.cause[16] <- "Total"

sum6$cause_group_1[sum6$principal.cause=="poor_vision_cause_uncorrected_refractive_error" | sum6$principal.cause=="poor_vision_cause_aphakia_uncorrected" | sum6$principal.cause== "poor_vision_cause_cataract_untreated"]<-"A. Treatable (1, 2, 3)"
sum6$cause_group_1[sum6$principal.cause=="poor_vision_cause_trachomatous_corneal_opacity" | sum6$principal.cause=="poor_vision_cause_other_corneal_opacity" |  sum6$principal.cause=="poor_vision_cause_pterygium" | sum6$principal.cause=="poor_vision_cause_phthisis" | sum6$principal.cause=="poor_vision_cause_onchocerciasis"]<-"B. Preventable (PHC/PEC services) (5, 6, 7, 8, 9)"
sum6$cause_group_1[sum6$principal.cause=="poor_vision_cause_cataract_surgical_complications" | sum6$principal.cause=="poor_vision_cause_glaucoma" | sum6$principal.cause=="poor_vision_cause_diabetic_retinopathy"]<-"C. Preventable (Ophthalmic services) (4, 10, 11)"
sum6$cause_group_2[sum6$principal.cause=="poor_vision_cause_onchocerciasis" | sum6$principal.cause=="poor_vision_cause_glaucoma" | sum6$principal.cause=="poor_vision_cause_diabetic_retinopathy" | sum6$principal.cause=="poor_vision_cause_age_related_macular_degeneration" | sum6$principal.cause=="poor_vision_cause_other_posterior_segment_disease" | sum6$principal.cause=="poor_vision_cause_myopic_degeneration"]<-"E. Posterior segment disease (9, 10, 11, 12, 13, 14)"

pcts<-grep("pct",names(sum6))
sum6[,pcts]<-round( sum6[,pcts] * 100, 1 )
sum6.numeric<-sum6
sum6[,pcts]<-format( sum6[,pcts], nsmall=1 )
