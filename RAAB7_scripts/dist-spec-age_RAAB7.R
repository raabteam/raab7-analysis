# Crude number and proportion of participants using distance vision correction by time since prescription spectacles received

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

# Add totals row to bottom of table
tots<-colSums(newtab7[,2:7])
newtab7[4,]<-NA
newtab7[4,1]<-"Total"
newtab7[4,2:7]<-tots

# Round pct values
pcts <- grep("pct",names(newtab7))
newtab7[,pcts] <- round(newtab7[,pcts] * 100, 1)
newtab7[,pcts] <- format(newtab7[,pcts], nsmall=1)