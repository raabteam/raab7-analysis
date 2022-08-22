# DR module: Prevalence of known and newly suspected diabetes by age group and by gender
# 04.07.22 IM

dm.prev<-data.frame(age.groups.tens)
dm.prev[,2:7] <- NA
names(dm.prev) <- c("age.groups.tens",
                    
                    "female.n",
                    "female.pct",
                    
                    "male.n",
                    "male.pct",
                    
                    "total.n",
                    "total.pct")

for (i in 1:length(age.groups.tens)) {
  
  dm.prev$female.n[i] <- sum(raab$diabetes.known.susp[raab$gender=="female" & raab$age.groups.tens==age.groups.tens[i]]==1,na.rm=T) 
  dm.prev$female.pct[i] <- sum(raab$diabetes.known.susp[raab$gender=="female" & raab$age.groups.tens==age.groups.tens[i]]==1,na.rm=T) / sum(raab$diabetes.denom[raab$gender=="female" & raab$age.groups.tens==age.groups.tens[i]]==1,na.rm=T) 
  
  dm.prev$male.n[i] <- sum(raab$diabetes.known.susp[raab$gender=="male" & raab$age.groups.tens==age.groups.tens[i]]==1,na.rm=T) 
  dm.prev$male.pct[i] <- sum(raab$diabetes.known.susp[raab$gender=="male" & raab$age.groups.tens==age.groups.tens[i]]==1,na.rm=T) / sum(raab$diabetes.denom[raab$gender=="male" & raab$age.groups.tens==age.groups.tens[i]]==1,na.rm=T)
  
  dm.prev$total.n[i] <- sum(raab$diabetes.known.susp[raab$age.groups.tens==age.groups.tens[i]]==1,na.rm=T) 
  dm.prev$total.pct[i] <- sum(raab$diabetes.known.susp[raab$age.groups.tens==age.groups.tens[i]]==1,na.rm=T) / sum(raab$diabetes.denom[raab$age.groups.tens==age.groups.tens[i]]==1,na.rm=T)
  
}

# NEEDS A NEW TOTAL ROW
dm.prev[nrow(dm.prev)+1,2:7]<-colSums(dm.prev[,2:7])
dm.prev$age.groups.tens[5]<-"Total"

pcts <- grep("pct",names(dm.prev))
dm.prev[,pcts] <- round( dm.prev[,pcts] * 100, 1)

write.csv(dm.prev, here('outputs', 'dm.prev.csv'))
