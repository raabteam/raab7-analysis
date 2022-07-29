# DR module: Prevalence of DR in diabetics
# 04.07.22 IM

# Retinopathy table
dr.ret.prev<-data.frame(retinopathy.grade)
dr.ret.prev[,2:7] <- NA
names(dr.ret.prev) <- c("retinopathy.grade",
                        
                        "female.n",
                        "female.pct",
                        
                        "male.n",
                        "male.pct",
                        
                        "total.n",
                        "total.pct")

for (i in 1:length(dr.ret.grade.person)) {
  
  dr.ret.prev$female.n[i] <- sum(raab$dr.exam.denom[raab$gender=="female" & raab$dr.ret.grade.person==dr.ret.grade.person[i]]==1,na.rm=T) 
  dr.ret.prev$female.pct[i] <- sum(raab$dr.exam.denom[raab$gender=="female" & raab$dr.ret.grade.person==dr.ret.grade.person[i]]==1,na.rm=T) / sum(raab$dr.exam.denom[raab$gender=="female"]==1,na.rm=T) 
  
  dr.ret.prev$male.n[i] <- sum(raab$dr.exam.denom[raab$gender=="male" & raab$dr.ret.grade.person==dr.ret.grade.person[i]]==1,na.rm=T) 
  dr.ret.prev$male.pct[i] <- sum(raab$dr.exam.denom[raab$gender=="male" & raab$dr.ret.grade.person==dr.ret.grade.person[i]]==1,na.rm=T) / sum(raab$dr.exam.denom[raab$gender=="male"]==1,na.rm=T)
  
  dr.ret.prev$total.n[i] <- sum(raab$dr.exam.denom[raab$dr.ret.grade.person==dr.ret.grade.person[i]]==1,na.rm=T) 
  dr.ret.prev$total.pct[i] <- sum(raab$dr.exam.denom[raab$dr.ret.grade.person==dr.ret.grade.person[i]]==1,na.rm=T) / sum(raab$dr.exam.denom==1,na.rm=T)
  
  dr.ret.prev$sample.n[i] <- 
  dr.ret.prev$sample.pct[i] <- 
}

# row for any retinopathy  
dr.ret.any.person <- data.frame("retinopathy.grade")
dr.ret.any.person[,2:7] <- NA
names(dr.ret.any.person) <- c("retinopathy.grade",
                              
                              "female.n",
                              "female.pct",
                              
                              "male.n",
                              "male.pct",
                              
                              "total.n",
                              "total.pct")

dr.ret.any.person$female.n <- sum(raab$dr.exam.denom[raab$gender=="female" & raab$dr.ret.any.person==1],na.rm=T) 
dr.ret.any.person$female.pct <- sum(raab$dr.exam.denom[raab$gender=="female" & raab$dr.ret.any.person==1],na.rm=T) / sum(raab$dr.exam.denom[raab$gender=="female"]==1,na.rm=T) 

dr.ret.any.person$male.n <- sum(raab$dr.exam.denom[raab$gender=="male" & raab$dr.ret.any.person==1],na.rm=T) 
dr.ret.any.person$male.pct <- sum(raab$dr.exam.denom[raab$gender=="male" & raab$dr.ret.any.person==1],na.rm=T) / sum(raab$dr.exam.denom[raab$gender=="male"]==1,na.rm=T) 

dr.ret.any.person$total.n <- sum(raab$dr.exam.denom[raab$dr.ret.any.person==1],na.rm=T) 
dr.ret.any.person$total.pct <- sum(raab$dr.exam.denom[raab$dr.ret.any.person==1],na.rm=T) / sum(raab$dr.exam.denom==1,na.rm=T)

# Bind grades and any DR row
retinopathy.grade.prev <- rbind(dr.ret.prev, dr.ret.any.person)
pcts <- grep("pct",names(retinopathy.grade.prev))
retinopathy.grade.prev[,pcts] <- round(retinopathy.grade.prev[,pcts] * 100, 1)
retinopathy.grade.prev$retinopathy.grade[7]<-"Any retinopathy"


# maculopathy table
maculopathy.grade <- c("dr_maculopathy_grade_none", "dr_maculopathy_grade_observable", "dr_maculopathy_grade_referable", "dr_maculopathy_grade_not_visualised")
dr.mac.grade.person <- c(1,2,3,0)

dr.mac.prev<-data.frame(maculopathy.grade)
dr.mac.prev[,2:7] <- NA
names(dr.mac.prev) <- c("maculopathy.grade",
                        
                        "female.n",
                        "female.pct",
                        
                        "male.n",
                        "male.pct",
                        
                        "total.n",
                        "total.pct")

for (i in 1:length(dr.mac.grade.person)) {
  
  dr.mac.prev$female.n[i] <- sum(raab$dr.exam.denom[raab$gender=="female" & raab$dr.mac.grade.person==dr.mac.grade.person[i]]==1,na.rm=T) 
  dr.mac.prev$female.pct[i] <- sum(raab$dr.exam.denom[raab$gender=="female" & raab$dr.mac.grade.person==dr.mac.grade.person[i]]==1,na.rm=T) / sum(raab$dr.exam.denom[raab$gender=="female"]==1,na.rm=T) 
  
  dr.mac.prev$male.n[i] <- sum(raab$dr.exam.denom[raab$gender=="male" & raab$dr.mac.grade.person==dr.mac.grade.person[i]]==1,na.rm=T) 
  dr.mac.prev$male.pct[i] <- sum(raab$dr.exam.denom[raab$gender=="male" & raab$dr.mac.grade.person==dr.mac.grade.person[i]]==1,na.rm=T) / sum(raab$dr.exam.denom[raab$gender=="male"]==1,na.rm=T)
  
  dr.mac.prev$total.n[i] <- sum(raab$dr.exam.denom[raab$dr.mac.grade.person==dr.mac.grade.person[i]]==1,na.rm=T) 
  dr.mac.prev$total.pct[i] <- sum(raab$dr.exam.denom[raab$dr.mac.grade.person==dr.mac.grade.person[i]]==1,na.rm=T) / sum(raab$dr.exam.denom==1,na.rm=T)
  
}
# row for any maculaopathy
dr.mac.any.person <- data.frame("maculopathy.grade")
dr.mac.any.person[,2:7] <- NA
names(dr.mac.any.person) <- c("maculopathy.grade",
                              
                              "female.n",
                              "female.pct",
                              
                              "male.n",
                              "male.pct",
                              
                              "total.n",
                              "total.pct")

dr.mac.any.person$female.n <- sum(raab$dr.exam.denom[raab$gender=="female" & raab$dr.mac.any.person==1],na.rm=T) 
dr.mac.any.person$female.pct <- sum(raab$dr.exam.denom[raab$gender=="female" & raab$dr.mac.any.person==1],na.rm=T) / sum(raab$dr.exam.denom[raab$gender=="female"]==1,na.rm=T) 

dr.mac.any.person$male.n <- sum(raab$dr.exam.denom[raab$gender=="male" & raab$dr.mac.any.person==1],na.rm=T) 
dr.mac.any.person$male.pct <- sum(raab$dr.exam.denom[raab$gender=="male" & raab$dr.mac.any.person==1],na.rm=T) / sum(raab$dr.exam.denom[raab$gender=="male"]==1,na.rm=T) 

dr.mac.any.person$total.n <- sum(raab$dr.exam.denom[raab$dr.mac.any.person==1],na.rm=T) 
dr.mac.any.person$total.pct <- sum(raab$dr.exam.denom[raab$dr.mac.any.person==1],na.rm=T) / sum(raab$dr.exam.denom==1,na.rm=T)

# Bind mac grades and any maculopathy row
maculopathy.grade.prev <- rbind(dr.mac.prev, dr.mac.any.person)
pcts <- grep("pct",names(maculopathy.grade.prev))
maculopathy.grade.prev[,pcts] <- round(maculopathy.grade.prev[,pcts] * 100, 1)
maculopathy.grade.prev$maculopathy.grade[5]<-"Any maculopathy"

## summary DR prev info

# any retinopathy and/or maculopathy
any.dr.mac.person <- data.frame("any.dr.mac")
any.dr.mac.person[,2:7] <- NA
names(any.dr.mac.person) <- c("dr.summary",
                              
                              "female.n",
                              "female.pct",
                              
                              "male.n",
                              "male.pct",
                              
                              "total.n",
                              "total.pct")

any.dr.mac.person$female.n <- sum(raab$dr.exam.denom[raab$gender=="female" & raab$dr.ret.mac.any.person==1],na.rm=T) 
any.dr.mac.person$female.pct <- sum(raab$dr.exam.denom[raab$gender=="female" & raab$dr.ret.mac.any.person==1],na.rm=T) / sum(raab$dr.exam.denom[raab$gender=="female"]==1,na.rm=T) 

any.dr.mac.person$male.n <- sum(raab$dr.exam.denom[raab$gender=="male" & raab$dr.ret.mac.any.person==1],na.rm=T) 
any.dr.mac.person$male.pct <- sum(raab$dr.exam.denom[raab$gender=="male" & raab$dr.ret.mac.any.person==1],na.rm=T) / sum(raab$dr.exam.denom[raab$gender=="male"]==1,na.rm=T) 

any.dr.mac.person$total.n <- sum(raab$dr.exam.denom[raab$dr.ret.mac.any.person==1],na.rm=T) 
any.dr.mac.person$total.pct <- sum(raab$dr.exam.denom[raab$dr.ret.mac.any.person==1],na.rm=T) / sum(raab$dr.exam.denom==1,na.rm=T)

# any STDR
any.stdr.person <- data.frame("any.stdr")
any.stdr.person[,2:7] <- NA
names(any.stdr.person) <- c("dr.summary",
                            
                            "female.n",
                            "female.pct",
                            
                            "male.n",
                            "male.pct",
                            
                            "total.n",
                            "total.pct")

any.stdr.person$female.n <- sum(raab$dr.exam.denom[raab$gender=="female" & raab$dr.stdr.any.person==1],na.rm=T) 
any.stdr.person$female.pct <- sum(raab$dr.exam.denom[raab$gender=="female" & raab$dr.stdr.any.person==1],na.rm=T) / sum(raab$dr.exam.denom[raab$gender=="female"]==1,na.rm=T) 

any.stdr.person$male.n <- sum(raab$dr.exam.denom[raab$gender=="male" & raab$dr.stdr.any.person==1],na.rm=T) 
any.stdr.person$male.pct <- sum(raab$dr.exam.denom[raab$gender=="male" & raab$dr.stdr.any.person==1],na.rm=T) / sum(raab$dr.exam.denom[raab$gender=="male"]==1,na.rm=T) 

any.stdr.person$total.n <- sum(raab$dr.exam.denom[raab$dr.stdr.any.person==1],na.rm=T) 
any.stdr.person$total.pct <- sum(raab$dr.exam.denom[raab$dr.stdr.any.person==1],na.rm=T) / sum(raab$dr.exam.denom==1,na.rm=T)

# any laser scars
any.laser.person <- data.frame("any.laser")
any.laser.person[,2:7] <- NA
names(any.laser.person) <- c("dr.summary",
                             
                             "female.n",
                             "female.pct",
                             
                             "male.n",
                             "male.pct",
                             
                             "total.n",
                             "total.pct")

any.laser.person$female.n <- sum(raab$dr.exam.denom[raab$gender=="female" & raab$dr.laser.person==1],na.rm=T) 
any.laser.person$female.pct <- sum(raab$dr.exam.denom[raab$gender=="female" & raab$dr.laser.person==1],na.rm=T) / sum(raab$dr.exam.denom[raab$gender=="female"]==1,na.rm=T) 

any.laser.person$male.n <- sum(raab$dr.exam.denom[raab$gender=="male" & raab$dr.laser.person==1],na.rm=T) 
any.laser.person$male.pct <- sum(raab$dr.exam.denom[raab$gender=="male" & raab$dr.laser.person==1],na.rm=T) / sum(raab$dr.exam.denom[raab$gender=="male"]==1,na.rm=T) 

any.laser.person$total.n <- sum(raab$dr.exam.denom[raab$dr.laser.person==1],na.rm=T) 
any.laser.person$total.pct <- sum(raab$dr.exam.denom[raab$dr.laser.person==1],na.rm=T) / sum(raab$dr.exam.denom==1,na.rm=T)

# Bind any ret/mac, STDR and laser tx counts
dr.summary <- rbind(any.dr.mac.person, any.stdr.person, any.laser.person)
pcts <- grep("pct",names(dr.summary))
dr.summary[,pcts] <- round(dr.summary[,pcts] * 100, 1)
