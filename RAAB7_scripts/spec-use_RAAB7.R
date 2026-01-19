#v1 24/08/21 - IM
#v2 02/09/21 - RB

specs <- c("distance.total", "distance.under.2", "distance.2.to.5", "distance.over.5", "near")

newtab6<-data.frame(specs)
newtab6[,2:7] <- NA
names(newtab6) <- c("specs",
                             
                     "female.n",
                     "female.pct",
                             
                     "male.n",
                     "male.pct",
                    
                     "total.n",
                     "total.pct")

newtab6$female.n[newtab6$specs=="near"] <- sum(raab$spectacles_used_near[raab$gender=='female']==TRUE, na.rm = T)
newtab6$male.n[newtab6$specs=="near"] <- sum(raab$spectacles_used_near[raab$gender=='male']==TRUE, na.rm = T)
newtab6$total.n[newtab6$specs=="near"] <- sum(raab$spectacles_used_near==TRUE, na.rm = T)

newtab6$female.pct[newtab6$specs=="near"] <- round( sum(raab$spectacles_used_near[raab$gender=='female']==TRUE, na.rm = TRUE) / sum(raab$exam_status[raab$gender=="female"]=="exam_status_examined", na.rm = T) * 100,1)
newtab6$male.pct[newtab6$specs=="near"] <- round( sum(raab$spectacles_used_near[raab$gender=='male']==TRUE, na.rm = TRUE) / sum(raab$exam_status[raab$gender=="male"]=="exam_status_examined", na.rm = T) * 100,1)
newtab6$total.pct[newtab6$specs=="near"] <- round( sum(raab$spectacles_used_near==TRUE, na.rm = TRUE) / sum(raab$exam_status=="exam_status_examined", na.rm = T) * 100,1)

newtab6$female.n[newtab6$specs=="distance.total"] <- sum(raab$spectacles_used_distance[raab$gender=='female']==TRUE, na.rm = T)
newtab6$male.n[newtab6$specs=="distance.total"] <- sum(raab$spectacles_used_distance[raab$gender=='male']==TRUE, na.rm = T)
newtab6$total.n[newtab6$specs=="distance.total"] <- sum(raab$spectacles_used_distance==TRUE, na.rm = T)

newtab6$female.pct[newtab6$specs=="distance.total"] <- round( sum(raab$spectacles_used_distance[raab$gender=='female']==TRUE, na.rm = TRUE) / sum(raab$exam_status[raab$gender=="female"]=="exam_status_examined", na.rm = T) * 100,1)
newtab6$male.pct[newtab6$specs=="distance.total"] <- round( sum(raab$spectacles_used_distance[raab$gender=='male']==TRUE, na.rm = TRUE) / sum(raab$exam_status[raab$gender=="male"]=="exam_status_examined", na.rm = T) * 100,1)
newtab6$total.pct[newtab6$specs=="distance.total"] <- round( sum(raab$spectacles_used_distance==TRUE, na.rm = TRUE) / sum(raab$exam_status=="exam_status_examined", na.rm = T) * 100,1)

newtab6$female.n[newtab6$specs=="distance.under.2"] <- sum(raab$spectacles_age_distance[raab$gender=='female']=="spectacles_age_under_2", na.rm = T)
newtab6$male.n[newtab6$specs=="distance.under.2"] <- sum(raab$spectacles_age_distance[raab$gender=='male']=="spectacles_age_under_2", na.rm = T)
newtab6$total.n[newtab6$specs=="distance.under.2"] <- sum(raab$spectacles_age_distance=="spectacles_age_under_2", na.rm = T)

newtab6$female.pct[newtab6$specs=="distance.under.2"] <- round( sum(raab$spectacles_age_distance[raab$gender=='female']=="spectacles_age_under_2", na.rm = TRUE) / sum(raab$spectacles_used_distance[raab$gender=="female"]==TRUE, na.rm=T) * 100,1)
newtab6$male.pct[newtab6$specs=="distance.under.2"] <- round( sum(raab$spectacles_age_distance[raab$gender=='male']=="spectacles_age_under_2", na.rm = TRUE) / sum(raab$spectacles_used_distance[raab$gender=="male"]==TRUE, na.rm=T) * 100,1)
newtab6$total.pct[newtab6$specs=="distance.under.2"] <- round( sum(raab$spectacles_age_distance=="spectacles_age_under_2", na.rm = TRUE) / sum(raab$spectacles_used_distance==TRUE, na.rm=T) * 100,1)

newtab6$female.n[newtab6$specs=="distance.2.to.5"] <- sum(raab$spectacles_age_distance[raab$gender=='female']=="spectacles_age_2_5", na.rm = T)
newtab6$male.n[newtab6$specs=="distance.2.to.5"] <- sum(raab$spectacles_age_distance[raab$gender=='male']=="spectacles_age_2_5", na.rm = T)
newtab6$total.n[newtab6$specs=="distance.2.to.5"] <- sum(raab$spectacles_age_distance=="spectacles_age_2_5", na.rm = T)

newtab6$female.pct[newtab6$specs=="distance.2.to.5"] <- round( sum(raab$spectacles_age_distance[raab$gender=='female']=="spectacles_age_2_5", na.rm = TRUE) / sum(raab$spectacles_used_distance[raab$gender=="female"]==TRUE, na.rm=T) * 100,1)
newtab6$male.pct[newtab6$specs=="distance.2.to.5"] <- round( sum(raab$spectacles_age_distance[raab$gender=='male']=="spectacles_age_2_5", na.rm = TRUE) / sum(raab$spectacles_used_distance[raab$gender=="male"]==TRUE, na.rm=T) * 100,1)
newtab6$total.pct[newtab6$specs=="distance.2.to.5"] <- round( sum(raab$spectacles_age_distance=="spectacles_age_2_5", na.rm = TRUE) / sum(raab$spectacles_used_distance==TRUE, na.rm=T) * 100,1)

newtab6$female.n[newtab6$specs=="distance.over.5"] <- sum(raab$spectacles_age_distance[raab$gender=='female']=="spectacles_age_over_5", na.rm = T)
newtab6$male.n[newtab6$specs=="distance.over.5"] <- sum(raab$spectacles_age_distance[raab$gender=='male']=="spectacles_age_over_5", na.rm = T)
newtab6$total.n[newtab6$specs=="distance.over.5"] <- sum(raab$spectacles_age_distance=="spectacles_age_over_5", na.rm = T)

newtab6$female.pct[newtab6$specs=="distance.over.5"] <- round( sum(raab$spectacles_age_distance[raab$gender=='female']=="spectacles_age_over_5", na.rm = TRUE) / sum(raab$spectacles_used_distance[raab$gender=="female"]==TRUE, na.rm=T) * 100,1)
newtab6$male.pct[newtab6$specs=="distance.over.5"] <- round( sum(raab$spectacles_age_distance[raab$gender=='male']=="spectacles_age_over_5", na.rm = TRUE) / sum(raab$spectacles_used_distance[raab$gender=="male"]==TRUE, na.rm=T) * 100,1)
newtab6$total.pct[newtab6$specs=="distance.over.5"] <- round( sum(raab$spectacles_age_distance=="spectacles_age_over_5", na.rm = TRUE) / sum(raab$spectacles_used_distance==TRUE, na.rm=T) * 100,1)
