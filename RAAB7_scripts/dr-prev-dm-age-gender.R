# DR module: Prevalence of known and newly suspected diabetes by age group and by gender
# 04.07.22 IM

dm.prev<-data.frame(age.groups.tens)
dm.prev[,2:13] <- NA
names(dm.prev) <- c("age.groups.tens",
                    
                    "female.n",
                    "female.pct",
                    "female.pct.lci",
                    "female.pct.uci",
                    
                    "male.n",
                    "male.pct",
                    "male.pct.lci",
                    "male.pct.uci",
                    
                    "total.n",
                    "total.pct",
                    "total.pct.lci",
                    "total.pct.uci")

for (i in 1:length(age.groups.tens)) {
  
  dm.prev$female.n[i] <- sum(raab$diabetes.known.susp[raab$gender=="female" & raab$age.groups.tens==age.groups.tens[i]]==1,na.rm=T) 
  dm.prev$female.pct[i] <- sum(raab$diabetes.known.susp[raab$gender=="female" & raab$age.groups.tens==age.groups.tens[i]]==1,na.rm=T) / sum(raab$diabetes.denom[raab$gender=="female" & raab$age.groups.tens==age.groups.tens[i]]==1,na.rm=T) 
  # dm.prev$female.pct.lci[i] <- bennett.lci(dm.prev$female.pct[i], raab$diabetes.known.susp[raab$gender=="female",age.groups.tens[i]], raab$diabetes.denom[raab$gender=="female",age.groups.tens[i]], raab$clusterNumber[raab$gender=="female"])
  # dm.prev$female.pct.uci[i] <- 
  
  dm.prev$male.n[i] <- sum(raab$diabetes.known.susp[raab$gender=="male" & raab$age.groups.tens==age.groups.tens[i]]==1,na.rm=T) 
  dm.prev$male.pct[i] <- sum(raab$diabetes.known.susp[raab$gender=="male" & raab$age.groups.tens==age.groups.tens[i]]==1,na.rm=T) / sum(raab$diabetes.denom[raab$gender=="male" & raab$age.groups.tens==age.groups.tens[i]]==1,na.rm=T)
  # dm.prev$male.pct.lci[i] <-
  # dm.prev$male.pct.uci[i] <-
    
  dm.prev$total.n[i] <- sum(raab$diabetes.known.susp[raab$age.groups.tens==age.groups.tens[i]]==1,na.rm=T) 
  dm.prev$total.pct[i] <- sum(raab$diabetes.known.susp[raab$age.groups.tens==age.groups.tens[i]]==1,na.rm=T) / sum(raab$diabetes.denom[raab$age.groups.tens==age.groups.tens[i]]==1,na.rm=T)
  # dm.prev$total.pct.lci[i] <-
  # dm.prev$total.pct.uci[i] <-
    
}

# Create a separate row for the overall prevalence of diabetes
dm.prev.total <- data.frame('age.groups.tens')
dm.prev.total[,2:13] <- NA
names(dm.prev.total) <- c("age.groups.tens",
                          
                          "female.n",
                          "female.pct",
                          "female.pct.lci",
                          "female.pct.uci",
                          
                          "male.n",
                          "male.pct",
                          "male.pct.lci",
                          "male.pct.uci",
                          
                          "total.n",
                          "total.pct",
                          "total.pct.lci",
                          "total.pct.uci")

dm.prev.total$female.n <- sum(raab$diabetes.known.susp[raab$gender=="female"]==1,na.rm=T)
dm.prev.total$female.pct <- sum(raab$diabetes.known.susp[raab$gender=="female"]==1,na.rm=T) / sum(raab$diabetes.denom[raab$gender=="female"]==1,na.rm=T)
# dm.prev.total$female.pct.lci[i] <- bennett.lci(dm.prev.total$female.pct[i], raab$diabetes.known.susp[raab$gender=="female",age.groups.tens[i]], raab$diabetes.denom[raab$gender=="female",age.groups.tens[i]], raab$clusterNumber[raab$gender=="female"])
# dm.prev.total$female.pct.uci[i] <- 

dm.prev.total$male.n <- sum(raab$diabetes.known.susp[raab$gender=="male"]==1,na.rm=T) 
dm.prev.total$male.pct <- sum(raab$diabetes.known.susp[raab$gender=="male"]==1,na.rm=T) / sum(raab$diabetes.denom[raab$gender=="male"]==1,na.rm=T)
# dm.prev.total$male.pct.lci[i] <-
# dm.prev.total$male.pct.uci[i] <-

dm.prev.total$total.n <- sum(raab$diabetes.known.susp==1,na.rm=T) 
dm.prev.total$total.pct <- sum(raab$diabetes.known.susp==1,na.rm=T) / sum(raab$diabetes.denom==1,na.rm=T)
# dm.prev.total$total.pct.lci[i] <-
# dm.prev.total$total.pct.uci[i] <-

# Combine age group prev table with overall prev table
dm.prev.final <- rbind(dm.prev, dm.prev.total)
dm.prev.final[5,1] <- "Total"

pcts <- grep("pct",names(dm.prev.final))
dm.prev.final[,pcts] <- round(dm.prev.final[,pcts] * 100, 1)

write.csv(dm.prev.final, here('outputs', 'dm.prev.csv'), row.names = FALSE)
