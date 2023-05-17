# Crude number and proportion of participants using distance vision spectacles by time since prescription spectacles received and number and proportion using near vision spectacles

# Sample prevalence of distance and near spectacle use
specs <- c("distance", "near")

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

newtab6$female.pct[newtab6$specs=="near"] <- round( sum(raab$spectacles_used_near[raab$gender=='female']==TRUE, na.rm = TRUE) / sum(raab$exam_status[raab$gender=="female"]=="exam_status_examined") * 100,1)
newtab6$male.pct[newtab6$specs=="near"] <- round( sum(raab$spectacles_used_near[raab$gender=='male']==TRUE, na.rm = TRUE) / sum(raab$exam_status[raab$gender=="male"]=="exam_status_examined") * 100,1)
newtab6$total.pct[newtab6$specs=="near"] <- round( sum(raab$spectacles_used_near==TRUE, na.rm = TRUE) / sum(raab$exam_status=="exam_status_examined") * 100,1)

newtab6$female.n[newtab6$specs=="distance"] <- sum(raab$spectacles_used_distance[raab$gender=='female']==TRUE, na.rm = T)
newtab6$male.n[newtab6$specs=="distance"] <- sum(raab$spectacles_used_distance[raab$gender=='male']==TRUE, na.rm = T)
newtab6$total.n[newtab6$specs=="distance"] <- sum(raab$spectacles_used_distance==TRUE, na.rm = T)

newtab6$female.pct[newtab6$specs=="distance"] <- round( sum(raab$spectacles_used_distance[raab$gender=='female']==TRUE, na.rm = TRUE) / sum(raab$exam_status[raab$gender=="female"]=="exam_status_examined") * 100,1)
newtab6$male.pct[newtab6$specs=="distance"] <- round( sum(raab$spectacles_used_distance[raab$gender=='male']==TRUE, na.rm = TRUE) / sum(raab$exam_status[raab$gender=="male"]=="exam_status_examined") * 100,1)
newtab6$total.pct[newtab6$specs=="distance"] <- round( sum(raab$spectacles_used_distance==TRUE, na.rm = TRUE) / sum(raab$exam_status=="exam_status_examined") * 100,1)

# Add total column
tots<-colSums(newtab6[,2:7])
newtab6[3,]<-NA
newtab6[3,1]<-"any"
newtab6[3,2:7]<-tots

# Disaggregation of distance spectacle use by time since prescription spectacles received
newtab7<-data.frame(dspecs.age)
newtab7[,2:7] <- NA
names(newtab7) <- c("dspecs.age",
                    
                    "female.n",
                    "female.pct",
                    
                    "male.n",
                    "male.pct",
                    
                    "total.n",
                    "total.pct")

for (i in 1:length(dspecs.age)) 
  
{
  
  newtab7$female.n[i] <- sum(raab$spectacles_age_distance[raab$gender=='female']==dspecs.age[i])
  newtab7$female.pct[i] <- (sum(raab$spectacles_age_distance[raab$gender=='female']==dspecs.age[i]) / sum(raab$dspecs.denom[raab$gender=='female']==1))
  
  newtab7$male.n[i] <- sum(raab$spectacles_age_distance[raab$gender=='male']==dspecs.age[i])
  newtab7$male.pct[i] <- (sum(raab$spectacles_age_distance[raab$gender=='male']==dspecs.age[i]) / sum(raab$dspecs.denom[raab$gender=='male']==1))
  
  newtab7$total.n[i] <- sum(raab$spectacles_age_distance==dspecs.age[i])
  newtab7$total.pct[i] <- (sum(raab$spectacles_age_distance==dspecs.age[i]) / sum(raab$dspecs.denom==1))
  
}

# # Calculate totals
# tots <- colSums(newtab7[,2:7])
# 
# # Create totals row
# totals_row <- data.frame(distance_spectacles = "distance_spectacles", t(tots))
# names(totals_row) <- names(newtab7)
# 
# # Add totals row to top of table
# newtab7 <- rbind(totals_row, newtab7)

# Round pct values
pcts <- grep("pct",names(newtab7))
newtab7[,pcts] <- round(newtab7[,pcts] * 100, 1)
newtab7[,pcts] <- format(newtab7[,pcts], nsmall=1)

# Create a new dataframe to combine newtab6 and newtab7 in single output
specs.use<-c('distance', 'spectacles_age_under_2', 'spectacles_age_2_5', 'spectacles_age_over_5', 'near')

spec.use.tab<-as.data.frame(specs.use)
spec.use.tab[,2:7]<-NA
names(spec.use.tab) <- c("",
                    
                    "female.n",
                    "female.pct",
                    
                    "male.n",
                    "male.pct",
                    
                    "total.n",
                    "total.pct")

spec.use.tab[1,2:7]<-newtab6[1,2:7]
spec.use.tab[2,2:7]<-newtab7[1,2:7]
spec.use.tab[3,2:7]<-newtab7[2,2:7]
spec.use.tab[4,2:7]<-newtab7[3,2:7]
spec.use.tab[5,2:7]<-newtab6[2,2:7]
