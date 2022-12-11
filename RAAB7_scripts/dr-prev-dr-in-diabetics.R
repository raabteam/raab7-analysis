# DR module: Prevalence of DR in diabetics
# 04.07.22 IM

# Retinopathy table
dr.ret.prev<-data.frame(retinopathy.grade)
dr.ret.prev[,2:13] <- NA
names(dr.ret.prev) <- c("grade",
                        
                        "female.n",
                        "female.pct",

                        "male.n",
                        "male.pct",

                        "total.n",
                        "total.pct")

for (i in 1:length(dr.ret.grade.person)) {
  
  dr.ret.prev$female.n[i] <- sum(raab$dr.exam.denom[raab$gender=="female" & raab$dr.ret.grade.person==dr.ret.grade.person[i]],na.rm=T) 
  dr.ret.prev$female.pct[i] <- sum(raab$dr.exam.denom[raab$gender=="female" & raab$dr.ret.grade.person==dr.ret.grade.person[i]],na.rm=T) / sum(raab$dr.exam.denom[raab$gender=="female"],na.rm=T) 

  dr.ret.prev$male.n[i] <- sum(raab$dr.exam.denom[raab$gender=="male" & raab$dr.ret.grade.person==dr.ret.grade.person[i]],na.rm=T) 
  dr.ret.prev$male.pct[i] <- sum(raab$dr.exam.denom[raab$gender=="male" & raab$dr.ret.grade.person==dr.ret.grade.person[i]],na.rm=T) / sum(raab$dr.exam.denom[raab$gender=="male"],na.rm=T)

  dr.ret.prev$total.n[i] <- sum(raab$dr.exam.denom[raab$dr.ret.grade.person==dr.ret.grade.person[i]]==1,na.rm=T) 
  dr.ret.prev$total.pct[i] <- sum(raab$dr.exam.denom[raab$dr.ret.grade.person==dr.ret.grade.person[i]]==1,na.rm=T) / sum(raab$dr.exam.denom,na.rm=T)

}

# row for any retinopathy  
dr.ret.any.person <- data.frame("retinopathy.grade")
dr.ret.any.person[,2:13] <- NA
names(dr.ret.any.person) <- c("condition",
                              
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

dr.ret.any.person$female.n <- sum(raab$dr.ret.any.person[raab$gender=="female"],na.rm=T) 
dr.ret.any.person$female.pct <- sum(raab$dr.ret.any.person[raab$gender=="female"],na.rm=T) / sum(raab$dr.exam.denom[raab$gender=="female"],na.rm=T) 
dr.ret.any.person$female.pct.lci <- bennett.lci(dr.ret.any.person$female.pct,raab$dr.ret.any.person[raab$gender=="female"],raab$dr.exam.denom[raab$gender=="female"],raab$clusterId[raab$gender=="female"])
dr.ret.any.person$female.pct.uci <- bennett.uci(dr.ret.any.person$female.pct,raab$dr.ret.any.person[raab$gender=="female"],raab$dr.exam.denom[raab$gender=="female"],raab$clusterId[raab$gender=="female"])

dr.ret.any.person$male.n <- sum(raab$dr.ret.any.person[raab$gender=="male" & raab$dr.ret.any.person],na.rm=T) 
dr.ret.any.person$male.pct <- sum(raab$dr.ret.any.person[raab$gender=="male" & raab$dr.ret.any.person],na.rm=T) / sum(raab$dr.exam.denom[raab$gender=="male"],na.rm=T) 
dr.ret.any.person$male.pct.lci <- bennett.lci(dr.ret.any.person$male.pct,raab$dr.ret.any.person[raab$gender=="male"],raab$dr.exam.denom[raab$gender=="male"],raab$clusterId[raab$gender=="male"])
dr.ret.any.person$male.pct.uci <- bennett.uci(dr.ret.any.person$male.pct,raab$dr.ret.any.person[raab$gender=="male"],raab$dr.exam.denom[raab$gender=="male"],raab$clusterId[raab$gender=="male"])

dr.ret.any.person$total.n <- sum(raab$dr.ret.any.person,na.rm=T) 
dr.ret.any.person$total.pct <- sum(raab$dr.ret.any.person,na.rm=T) / sum(raab$dr.exam.denom,na.rm=T)
dr.ret.any.person$total.pct.lci <- bennett.lci(dr.ret.any.person$total.pct,raab$dr.ret.any.person,raab$dr.exam.denom,raab$clusterId)
dr.ret.any.person$total.pct.uci <- bennett.uci(dr.ret.any.person$total.pct,raab$dr.ret.any.person,raab$dr.exam.denom,raab$clusterId)

# maculopathy table
maculopathy.grade <- c("dr_maculopathy_grade_none", "dr_maculopathy_grade_observable", "dr_maculopathy_grade_referable", "dr_maculopathy_grade_not_visualised")
dr.mac.grade.person <- c(1,2,3,0)

dr.mac.prev<-data.frame(maculopathy.grade)
dr.mac.prev[,2:13] <- NA
names(dr.mac.prev) <- c("grade",
                        
                        "female.n",
                        "female.pct",

                        "male.n",
                        "male.pct",

                        "total.n",
                        "total.pct")

for (i in 1:length(dr.mac.grade.person)) {
  
  dr.mac.prev$female.n[i] <- sum(raab$dr.exam.denom[raab$gender=="female" & raab$dr.mac.grade.person==dr.mac.grade.person[i]],na.rm=T) 
  dr.mac.prev$female.pct[i] <- sum(raab$dr.exam.denom[raab$gender=="female" & raab$dr.mac.grade.person==dr.mac.grade.person[i]],na.rm=T) / sum(raab$dr.exam.denom[raab$gender=="female"],na.rm=T) 

  dr.mac.prev$male.n[i] <- sum(raab$dr.exam.denom[raab$gender=="male" & raab$dr.mac.grade.person==dr.mac.grade.person[i]],na.rm=T) 
  dr.mac.prev$male.pct[i] <- sum(raab$dr.exam.denom[raab$gender=="male" & raab$dr.mac.grade.person==dr.mac.grade.person[i]],na.rm=T) / sum(raab$dr.exam.denom[raab$gender=="male"],na.rm=T)

  dr.mac.prev$total.n[i] <- sum(raab$dr.exam.denom[raab$dr.mac.grade.person==dr.mac.grade.person[i]],na.rm=T) 
  dr.mac.prev$total.pct[i] <- sum(raab$dr.exam.denom[raab$dr.mac.grade.person==dr.mac.grade.person[i]],na.rm=T) / sum(raab$dr.exam.denom,na.rm=T)

}

# row for any maculaopathy
dr.mac.any.person <- data.frame("maculopathy.grade")
dr.mac.any.person[,2:13] <- NA
names(dr.mac.any.person) <- c("condition",
                              
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

dr.mac.any.person$female.n <- sum(raab$dr.mac.any.person[raab$gender=="female"],na.rm=T) 
dr.mac.any.person$female.pct <- sum(raab$dr.mac.any.person[raab$gender=="female"],na.rm=T) / sum(raab$dr.exam.denom[raab$gender=="female"],na.rm=T) 
dr.mac.any.person$female.pct.lci <- bennett.lci(dr.mac.any.person$female.pct,raab$dr.mac.any.person[raab$gender=="female"],raab$dr.exam.denom[raab$gender=="female"],raab$clusterId[raab$gender=="female"])
dr.mac.any.person$female.pct.uci <- bennett.uci(dr.mac.any.person$female.pct,raab$dr.mac.any.person[raab$gender=="female"],raab$dr.exam.denom[raab$gender=="female"],raab$clusterId[raab$gender=="female"])

dr.mac.any.person$male.n <- sum(raab$dr.mac.any.person[raab$gender=="male" & raab$dr.mac.any.person==1],na.rm=T) 
dr.mac.any.person$male.pct <- sum(raab$dr.mac.any.person[raab$gender=="male" & raab$dr.mac.any.person==1],na.rm=T) / sum(raab$dr.exam.denom[raab$gender=="male"],na.rm=T) 
dr.mac.any.person$male.pct.lci <- bennett.lci(dr.mac.any.person$male.pct,raab$dr.mac.any.person[raab$gender=="male"],raab$dr.exam.denom[raab$gender=="male"],raab$clusterId[raab$gender=="male"])
dr.mac.any.person$male.pct.uci <- bennett.uci(dr.mac.any.person$male.pct,raab$dr.mac.any.person[raab$gender=="male"],raab$dr.exam.denom[raab$gender=="male"],raab$clusterId[raab$gender=="male"])

dr.mac.any.person$total.n <- sum(raab$dr.mac.any.person,na.rm=T) 
dr.mac.any.person$total.pct <- sum(raab$dr.mac.any.person,na.rm=T) / sum(raab$dr.exam.denom,na.rm=T)
dr.mac.any.person$total.pct.lci <- bennett.lci(dr.mac.any.person$total.pct,raab$dr.mac.any.person,raab$dr.exam.denom,raab$clusterId)
dr.mac.any.person$total.pct.uci <- bennett.uci(dr.mac.any.person$total.pct,raab$dr.mac.any.person,raab$dr.exam.denom,raab$clusterId)

# any retinopathy and/or maculopathy
any.dr.mac.person <- data.frame("any.dr.mac")
any.dr.mac.person[,2:13] <- NA
names(any.dr.mac.person) <- c("condition",
                              
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

any.dr.mac.person$female.n <- sum(raab$dr.ret.mac.any.person[raab$gender=="female"],na.rm=T) 
any.dr.mac.person$female.pct <- sum(raab$dr.ret.mac.any.person[raab$gender=="female"],na.rm=T) / sum(raab$dr.exam.denom[raab$gender=="female"],na.rm=T) 
any.dr.mac.person$female.pct.lci <- bennett.lci(any.dr.mac.person$female.pct,raab$dr.ret.mac.any.person[raab$gender=="female"],raab$dr.exam.denom[raab$gender=="female"],raab$clusterId[raab$gender=="female"])
any.dr.mac.person$female.pct.uci <- bennett.uci(any.dr.mac.person$female.pct,raab$dr.ret.mac.any.person[raab$gender=="female"],raab$dr.exam.denom[raab$gender=="female"],raab$clusterId[raab$gender=="female"])

any.dr.mac.person$male.n <- sum(raab$dr.ret.mac.any.person[raab$gender=="male"],na.rm=T) 
any.dr.mac.person$male.pct <- sum(raab$dr.ret.mac.any.person[raab$gender=="male"],na.rm=T) / sum(raab$dr.exam.denom[raab$gender=="male"],na.rm=T) 
any.dr.mac.person$male.pct.lci <- bennett.lci(any.dr.mac.person$male.pct,raab$dr.ret.mac.any.person[raab$gender=="male"],raab$dr.exam.denom[raab$gender=="male"],raab$clusterId[raab$gender=="male"])
any.dr.mac.person$male.pct.uci <- bennett.uci(any.dr.mac.person$male.pct,raab$dr.ret.mac.any.person[raab$gender=="male"],raab$dr.exam.denom[raab$gender=="male"],raab$clusterId[raab$gender=="male"])

any.dr.mac.person$total.n <- sum(raab$dr.ret.mac.any.person, na.rm=T) 
any.dr.mac.person$total.pct <- sum(raab$dr.ret.mac.any.person, na.rm=T) / sum(raab$dr.exam.denom, na.rm=T)
any.dr.mac.person$total.pct.lci <- bennett.lci(any.dr.mac.person$total.pct,raab$dr.ret.mac.any.person,raab$dr.exam.denom,raab$clusterId)
any.dr.mac.person$total.pct.uci <- bennett.uci(any.dr.mac.person$total.pct,raab$dr.ret.mac.any.person,raab$dr.exam.denom,raab$clusterId)

# any STDR
any.stdr.person <- data.frame("any.stdr")
any.stdr.person[,2:13] <- NA
names(any.stdr.person) <- c("condition",
                            
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

any.stdr.person$female.n <- sum(raab$dr.stdr.any.person[raab$gender=="female"],na.rm=T) 
any.stdr.person$female.pct <- sum(raab$dr.stdr.any.person[raab$gender=="female"],na.rm=T) / sum(raab$dr.exam.denom[raab$gender=="female"],na.rm=T) 
any.stdr.person$female.pct.lci <- bennett.lci(any.stdr.person$female.pct,raab$dr.stdr.any.person[raab$gender=="female"],raab$dr.exam.denom[raab$gender=="female"],raab$clusterId[raab$gender=="female"])
any.stdr.person$female.pct.uci <- bennett.uci(any.stdr.person$female.pct,raab$dr.stdr.any.person[raab$gender=="female"],raab$dr.exam.denom[raab$gender=="female"],raab$clusterId[raab$gender=="female"])

any.stdr.person$male.n <- sum(raab$dr.stdr.any.person[raab$gender=="male"],na.rm=T) 
any.stdr.person$male.pct <- sum(raab$dr.stdr.any.person[raab$gender=="male"],na.rm=T) / sum(raab$dr.exam.denom[raab$gender=="male"],na.rm=T) 
any.stdr.person$male.pct.lci <- bennett.lci(any.stdr.person$male.pct,raab$dr.stdr.any.person[raab$gender=="male"],raab$dr.exam.denom[raab$gender=="male"],raab$clusterId[raab$gender=="male"])
any.stdr.person$male.pct.uci <- bennett.uci(any.stdr.person$male.pct,raab$dr.stdr.any.person[raab$gender=="male"],raab$dr.exam.denom[raab$gender=="male"],raab$clusterId[raab$gender=="male"])

any.stdr.person$total.n <- sum(raab$dr.stdr.any.person,na.rm=T) 
any.stdr.person$total.pct <- sum(raab$dr.stdr.any.person,na.rm=T) / sum(raab$dr.exam.denom,na.rm=T)
any.stdr.person$total.pct.lci <- bennett.lci(any.stdr.person$total.pct,raab$dr.stdr.any.person,raab$dr.exam.denom,raab$clusterId)
any.stdr.person$total.pct.uci <- bennett.uci(any.stdr.person$total.pct,raab$dr.stdr.any.person,raab$dr.exam.denom,raab$clusterId)

# any laser scars
any.laser.person <- data.frame("any.laser")
any.laser.person[,2:13] <- NA
names(any.laser.person) <- c("condition",
                             
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

any.laser.person$female.n <- sum(raab$dr.laser.person[raab$gender=="female"],na.rm=T) 
any.laser.person$female.pct <- sum(raab$dr.laser.person[raab$gender=="female"],na.rm=T) / sum(raab$dr.exam.denom[raab$gender=="female"],na.rm=T) 
any.laser.person$female.pct.lci <- bennett.lci(any.laser.person$female.pct,raab$dr.laser.person[raab$gender=="female"],raab$dr.exam.denom[raab$gender=="female"],raab$clusterId[raab$gender=="female"])
any.laser.person$female.pct.uci <- bennett.uci(any.laser.person$female.pct,raab$dr.laser.person[raab$gender=="female"],raab$dr.exam.denom[raab$gender=="female"],raab$clusterId[raab$gender=="female"])

any.laser.person$male.n <- sum(raab$dr.laser.person[raab$gender=="male"],na.rm=T) 
any.laser.person$male.pct <- sum(raab$dr.laser.person[raab$gender=="male"],na.rm=T) / sum(raab$dr.exam.denom[raab$gender=="male"],na.rm=T) 
any.laser.person$male.pct.lci <- bennett.lci(any.laser.person$male.pct,raab$dr.laser.person[raab$gender=="male"],raab$dr.exam.denom[raab$gender=="male"],raab$clusterId[raab$gender=="male"])
any.laser.person$male.pct.uci <- bennett.uci(any.laser.person$male.pct,raab$dr.laser.person[raab$gender=="male"],raab$dr.exam.denom[raab$gender=="male"],raab$clusterId[raab$gender=="male"])

any.laser.person$total.n <- sum(raab$dr.laser.person,na.rm=T) 
any.laser.person$total.pct <- sum(raab$dr.laser.person,na.rm=T) / sum(raab$dr.exam.denom,na.rm=T)
any.laser.person$total.pct.lci <- bennett.lci(any.laser.person$total.pct,raab$dr.laser.person,raab$dr.exam.denom,raab$clusterId)
any.laser.person$total.pct.uci <- bennett.uci(any.laser.person$total.pct,raab$dr.laser.person,raab$dr.exam.denom,raab$clusterId)

# Bind any ret/mac, STDR and laser tx counts
any.dr.mac.laser.prev <- rbind(dr.ret.any.person, dr.mac.any.person, any.dr.mac.person, any.stdr.person, any.laser.person)

lcis<-grep("lci",names(any.dr.mac.laser.prev))
ucis<-grep("uci",names(any.dr.mac.laser.prev))
any.dr.mac.laser.prev[,lcis][any.dr.mac.laser.prev[,lcis]<0]<-0
any.dr.mac.laser.prev[,ucis][any.dr.mac.laser.prev[,ucis]>1]<-1

pcts <- grep("pct",names(any.dr.mac.laser.prev))
any.dr.mac.laser.prev[,pcts] <- round(any.dr.mac.laser.prev[,pcts] * 100, 1)

ret.mac.prev <- rbind(dr.ret.prev, dr.mac.prev)

pcts <- grep("pct",names(ret.mac.prev))
ret.mac.prev[,pcts] <- round(ret.mac.prev[,pcts] * 100, 1)
