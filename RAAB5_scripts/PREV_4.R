#RAAB5

#v1 22 July 21 - IM
#v2 23 Aug 21 - RB

prev4<-data.frame(raab.cause)
prev4[,2:7] <- NA
names(prev4) <- c("principal.cause",
                             
                             "female.n",
                             "female.pct",
                             
                             "male.n",
                             "male.pct",
                             
                             "total.n",
                             "total.pct"
)


for (i in seq_along(raab.cause)) {
  
  prev4$female.n[i] <- sum(raab$poor_vision_cause_principle[raab$gender=='female' & raab$blind==1]==raab.cause[i]) 
  prev4$female.pct[i] <- sum(raab$poor_vision_cause_principle[raab$gender=='female' & raab$blind==1]==raab.cause[i]) / sum(raab$blind[raab$gender=='female'])

  prev4$male.n[i] <- sum(raab$poor_vision_cause_principle[raab$gender=='male' & raab$blind==1]==raab.cause[i])
  prev4$male.pct[i] <- sum(raab$poor_vision_cause_principle[raab$gender=='male' & raab$blind==1]==raab.cause[i]) / sum(raab$blind[raab$gender=='male'])
  
  prev4$total.n[i] <- sum(raab$poor_vision_cause_principle[raab$blind==1]==raab.cause[i])
  prev4$total.pct[i] <- sum(raab$poor_vision_cause_principle[raab$blind==1]==raab.cause[i]) / sum(raab$blind==1)
  
}

prev4[nrow(prev4)+1,2:7]<-colSums(prev4[,2:7])
prev4$principal.cause[14]<-"Total"

prev4$cause_group_1[prev4$principal.cause=="poor_vision_cause_uncorrected_refractive_error" | prev4$principal.cause=="poor_vision_cause_aphakia_uncorrected" | prev4$principal.cause== "poor_vision_cause_cataract_untreated"]<-"A. Treatable (1, 2, 3)"
prev4$cause_group_1[prev4$principal.cause=="poor_vision_cause_trachomatous_corneal_opacity" | prev4$principal.cause=="poor_vision_cause_other_corneal_opacity" | prev4$principal.cause=="poor_vision_cause_phthisis" | prev4$principal.cause=="poor_vision_cause_onchocerciasis"]<-"B. Preventable (PHC/PEC services) (5, 6, 7, 8)"
prev4$cause_group_1[prev4$principal.cause=="poor_vision_cause_cataract_surgical_complications" | prev4$principal.cause=="poor_vision_cause_glaucoma" | prev4$principal.cause=="poor_vision_cause_diabetic_retinopathy"]<-"C. Preventable (Ophthalmic services) (4, 9, 10)"
prev4$cause_group_2[prev4$principal.cause=="poor_vision_cause_onchocerciasis" | prev4$principal.cause=="poor_vision_cause_glaucoma" | prev4$principal.cause=="poor_vision_cause_diabetic_retinopathy" | prev4$principal.cause=="poor_vision_cause_age_related_macular_degeneration" | prev4$principal.cause=="poor_vision_cause_other_posterior_segment_disease"]<-"E. Posterior segment disease (8, 9, 10, 11, 12)"


pcts<-grep("pct",names(prev4))
prev4[,pcts]<-round( prev4[,pcts] * 100, 1 )
