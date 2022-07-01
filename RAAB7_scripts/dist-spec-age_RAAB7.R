# create a denominator for proportions of distance specs by time since received
#raab$dspecs.denom <- case_when(raab$spectacles_age_distance=="spectacles_age_under_2" | raab$spectacles_age_distance=="spectacles_age_2_5" | raab$spectacles_age_distance=="spectacles_age_over_5" ~ 1, TRUE ~ 0)

# create a dummy denom to test script - must remove!
raab$dspecs.denom <- 0
raab$dspecs.denom[sample(nrow(raab),size=200)] <- 1


# Number of participants using distance vision correction by time since prescription received

dspecs.age <- c('spectacles_age_under_2', 'spectacles_age_2_5', 'spectacles_age_over_5')

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
  newtab7$female.pct[i] <- (sum(raab$spectacles_age_distance[raab$gender=='female']==dspecs.age[i]) / sum(raab$dspecs.denom[raab$gender=='female']==1)) *100
  
  newtab7$male.n[i] <- sum(raab$spectacles_age_distance[raab$gender=='male']==dspecs.age[i])
  newtab7$male.pct[i] <- (sum(raab$spectacles_age_distance[raab$gender=='male']==dspecs.age[i]) / sum(raab$dspecs.denom[raab$gender=='male']==1)) *100
  
  newtab7$total.n[i] <- sum(raab$spectacles_age_distance==dspecs.age[i])
  newtab7$total.pct[i] <- (sum(raab$spectacles_age_distance==dspecs.age[i]) / sum(raab$dspecs.denom==1)) *100
  
}

#Add totals row to bottom of table (for total count of female, male, all)

tots<-colSums(newtab7[,2:7])
newtab7[4,]<-NA
newtab7[4,1]<-"Total"
newtab7[4,2:7]<-tots

