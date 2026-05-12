# crude prevalence of all near vi by age group and gender
# near vi defined using presenting near VA (only one level, binary screening test at N6 at 40cm threshold)

near.vi.prev<-data.frame(age.groups.tens)
near.vi.prev[,2:13] <- NA
names(near.vi.prev) <- c("age.groups.tens",
                    
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
  
  near.vi.prev$female.n[i] <- sum(raab$near.vi[raab$gender=="female" & raab$age.groups.tens==age.groups.tens[i]],na.rm=T) 
  near.vi.prev$female.pct[i] <- sum(raab$near.vi[raab$gender=="female" & raab$age.groups.tens==age.groups.tens[i]],na.rm=T) / sum(raab$vi.denom[raab$gender=="female" & raab$age.groups.tens==age.groups.tens[i]],na.rm=T) 
  near.vi.prev$female.pct.lci[i] <- bennett.lci(near.vi.prev$female.pct[i], raab$near.vi[raab$gender=="female" & raab$age.groups.tens==age.groups.tens[i]], raab$vi.denom[raab$gender=="female" & raab$age.groups.tens==age.groups.tens[i]], raab$clusterId[raab$gender=="female" & raab$age.groups.tens==age.groups.tens[i]])
  near.vi.prev$female.pct.uci[i] <- bennett.uci(near.vi.prev$female.pct[i], raab$near.vi[raab$gender=="female" & raab$age.groups.tens==age.groups.tens[i]], raab$vi.denom[raab$gender=="female" & raab$age.groups.tens==age.groups.tens[i]], raab$clusterId[raab$gender=="female" & raab$age.groups.tens==age.groups.tens[i]])  

  near.vi.prev$male.n[i] <- sum(raab$near.vi[raab$gender=="male" & raab$age.groups.tens==age.groups.tens[i]],na.rm=T) 
  near.vi.prev$male.pct[i] <- sum(raab$near.vi[raab$gender=="male" & raab$age.groups.tens==age.groups.tens[i]],na.rm=T) / sum(raab$vi.denom[raab$gender=="male" & raab$age.groups.tens==age.groups.tens[i]],na.rm=T)
  near.vi.prev$male.pct.lci[i] <- bennett.lci(near.vi.prev$male.pct[i], raab$near.vi[raab$gender=="male" & raab$age.groups.tens==age.groups.tens[i]], raab$vi.denom[raab$gender=="male" & raab$age.groups.tens==age.groups.tens[i]], raab$clusterId[raab$gender=="male" & raab$age.groups.tens==age.groups.tens[i]])
  near.vi.prev$male.pct.uci[i] <- bennett.uci(near.vi.prev$male.pct[i], raab$near.vi[raab$gender=="male" & raab$age.groups.tens==age.groups.tens[i]], raab$vi.denom[raab$gender=="male" & raab$age.groups.tens==age.groups.tens[i]], raab$clusterId[raab$gender=="male" & raab$age.groups.tens==age.groups.tens[i]])  
  
  near.vi.prev$total.n[i] <- sum(raab$near.vi[raab$age.groups.tens==age.groups.tens[i]]==1,na.rm=T) 
  near.vi.prev$total.pct[i] <- sum(raab$near.vi[raab$age.groups.tens==age.groups.tens[i]]==1,na.rm=T) / sum(raab$vi.denom[raab$age.groups.tens==age.groups.tens[i]]==1,na.rm=T)
  near.vi.prev$total.pct.lci[i] <- bennett.lci(near.vi.prev$total.pct[i], raab$near.vi[raab$age.groups.tens==age.groups.tens[i]], raab$vi.denom[raab$age.groups.tens==age.groups.tens[i]], raab$clusterId[raab$age.groups.tens==age.groups.tens[i]])
  near.vi.prev$total.pct.uci[i] <- bennett.uci(near.vi.prev$total.pct[i], raab$near.vi[raab$age.groups.tens==age.groups.tens[i]], raab$vi.denom[raab$age.groups.tens==age.groups.tens[i]], raab$clusterId[raab$age.groups.tens==age.groups.tens[i]])  

}

# Create a separate row for the overall prevalence of near vi

near.vi.prev.total<-data.frame('near.vi')
near.vi.prev.total[,2:25] <- NA
names(near.vi.prev.total) <- c("age.groups.tens",
                    
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
                    "total.pct.uci",
                    
                    "female.adj.pct",
                    "female.adj.pct.lci",
                    "female.adj.pct.uci",
                    "female.extrapolated.n",
                    
                    "male.adj.pct",
                    "male.adj.pct.lci",
                    "male.adj.pct.uci",
                    "male.extrapolated.n",
                    
                    "total.adj.pct",
                    "total.adj.pct.lci",
                    "total.adj.pct.uci",
                    "total.extrapolated.n")

near.vi.prev.total$female.n <- sum(raab$near.vi[raab$gender=="female"],na.rm=T) 
near.vi.prev.total$female.pct <- sum(raab$near.vi[raab$gender=="female"],na.rm=T) / sum(raab$vi.denom[raab$gender=="female"],na.rm=T) 
near.vi.prev.total$female.pct.lci <- bennett.lci(near.vi.prev.total$female.pct, raab$near.vi[raab$gender=="female"], raab$vi.denom[raab$gender=="female"], raab$clusterId[raab$gender=="female"])
near.vi.prev.total$female.pct.uci <- bennett.uci(near.vi.prev.total$female.pct, raab$near.vi[raab$gender=="female"], raab$vi.denom[raab$gender=="female"], raab$clusterId[raab$gender=="female"])  

near.vi.prev.total$male.n <- sum(raab$near.vi[raab$gender=="male"],na.rm=T) 
near.vi.prev.total$male.pct <- sum(raab$near.vi[raab$gender=="male"],na.rm=T) / sum(raab$vi.denom[raab$gender=="male"],na.rm=T)
near.vi.prev.total$male.pct.lci <- bennett.lci(near.vi.prev.total$male.pct, raab$near.vi[raab$gender=="male"], raab$vi.denom[raab$gender=="male"], raab$clusterId[raab$gender=="male"])
near.vi.prev.total$male.pct.uci <- bennett.uci(near.vi.prev.total$male.pct, raab$near.vi[raab$gender=="male"], raab$vi.denom[raab$gender=="male"], raab$clusterId[raab$gender=="male"])  

near.vi.prev.total$total.n <- sum(raab$near.vi,na.rm=T) 
near.vi.prev.total$total.pct <- sum(raab$near.vi,na.rm=T) / sum(raab$vi.denom,na.rm=T)
near.vi.prev.total$total.pct.lci <- bennett.lci(near.vi.prev.total$total.pct, raab$near.vi, raab$vi.denom, raab$clusterId)
near.vi.prev.total$total.pct.uci <- bennett.uci(near.vi.prev.total$total.pct, raab$near.vi, raab$vi.denom, raab$clusterId)  

near.vi.prev.total$female.adj.pct <- prop.age.adjust(female.subpop,raab[raab$gender=="female",],raab$near.vi[raab$gender=="female"],raab$vi.denom[raab$gender=="female"]) 
near.vi.prev.total$female.adj.pct.lci <- bennett.lci(near.vi.prev.total$female.adj.pct, raab$near.vi[raab$gender=="female"], raab$vi.denom[raab$gender=="female"], raab$clusterId[raab$gender=="female"])
near.vi.prev.total$female.adj.pct.uci <- bennett.uci(near.vi.prev.total$female.adj.pct, raab$near.vi[raab$gender=="female"], raab$vi.denom[raab$gender=="female"], raab$clusterId[raab$gender=="female"])  
near.vi.prev.total$female.extrapolated.n <- round( near.vi.prev.total$female.adj.pct * sum(female.subpop$population,na.rm=T), 0) 

near.vi.prev.total$male.adj.pct <- prop.age.adjust(male.subpop, raab[raab$gender=="male",],raab$near.vi[raab$gender=="male"],raab$vi.denom[raab$gender=="male"])
near.vi.prev.total$male.adj.pct.lci <- bennett.lci(near.vi.prev.total$male.adj.pct, raab$near.vi[raab$gender=="male"], raab$vi.denom[raab$gender=="male"], raab$clusterId[raab$gender=="male"])
near.vi.prev.total$male.adj.pct.uci <- bennett.uci(near.vi.prev.total$male.adj.pct, raab$near.vi[raab$gender=="male"], raab$vi.denom[raab$gender=="male"], raab$clusterId[raab$gender=="male"])  
near.vi.prev.total$male.extrapolated.n <- round( near.vi.prev.total$male.adj.pct * sum(male.subpop$population,na.rm=T), 0)

near.vi.prev.total$total.adj.pct <- prop.age.sex.adjust(popfives, raab, raab$near.vi, raab$vi.denom)
near.vi.prev.total$total.adj.pct.lci <- bennett.lci(near.vi.prev.total$total.adj.pct, raab$near.vi, raab$vi.denom, raab$clusterId)
near.vi.prev.total$total.adj.pct.uci <- bennett.uci(near.vi.prev.total$total.adj.pct, raab$near.vi, raab$vi.denom, raab$clusterId)  
near.vi.prev.total$total.extrapolated.n <- round( near.vi.prev.total$total.adj.pct * sum(popfives$population,na.rm=T), 0)

# Combine age group nvi table with overall nvi table
near.vi.prev.final <- rbind(near.vi.prev, near.vi.prev.total[,c(1:13)])
near.vi.prev.final[5,1] <- "total"

lcis<-grep("lci",names(near.vi.prev.final))
ucis<-grep("uci",names(near.vi.prev.final))
near.vi.prev.final[,lcis][near.vi.prev.final[,lcis]<0]<-0
near.vi.prev.final[,ucis][near.vi.prev.final[,ucis]>1]<-1

# Round pct values
pcts <- grep("pct",names(near.vi.prev.final))

near.vi.prev.final[,pcts] <- round(near.vi.prev.final[,pcts] * 100, 1)
near.vi.prev.final[,pcts] <- format(near.vi.prev.final[,pcts], nsmall=1)

#Fix crude and adjusted rows

lcis<-grep("lci",names(near.vi.prev.total))
ucis<-grep("uci",names(near.vi.prev.total))
near.vi.prev.total[,lcis][near.vi.prev.total[,lcis]<0]<-0
near.vi.prev.total[,ucis][near.vi.prev.total[,ucis]>1]<-1

# Round pct values
pcts <- grep("pct",names(near.vi.prev.total))

near.vi.prev.total[,pcts] <- round(near.vi.prev.total[,pcts] * 100, 1)
near.vi.prev.total[,pcts] <- format(near.vi.prev.total[,pcts], nsmall=1)

# ---- Presbyopic near VI: overall prevalence (ie not by age groups) ----
near.vi.presb.prev.total <- data.frame('near.vi.presb')
near.vi.presb.prev.total[,2:25] <- NA
names(near.vi.presb.prev.total) <- names(near.vi.prev.total)

near.vi.presb.prev.total$female.n        <- sum(raab$near.vi.presb[raab$gender=="female"], na.rm=T)
near.vi.presb.prev.total$female.pct      <- sum(raab$near.vi.presb[raab$gender=="female"], na.rm=T) / sum(raab$vi.denom.presb[raab$gender=="female"], na.rm=T)
near.vi.presb.prev.total$female.pct.lci  <- bennett.lci(near.vi.presb.prev.total$female.pct, raab$near.vi.presb[raab$gender=="female"], raab$vi.denom.presb[raab$gender=="female"], raab$clusterId[raab$gender=="female"])
near.vi.presb.prev.total$female.pct.uci  <- bennett.uci(near.vi.presb.prev.total$female.pct, raab$near.vi.presb[raab$gender=="female"], raab$vi.denom.presb[raab$gender=="female"], raab$clusterId[raab$gender=="female"])

near.vi.presb.prev.total$male.n          <- sum(raab$near.vi.presb[raab$gender=="male"], na.rm=T)
near.vi.presb.prev.total$male.pct        <- sum(raab$near.vi.presb[raab$gender=="male"], na.rm=T) / sum(raab$vi.denom.presb[raab$gender=="male"], na.rm=T)
near.vi.presb.prev.total$male.pct.lci    <- bennett.lci(near.vi.presb.prev.total$male.pct, raab$near.vi.presb[raab$gender=="male"], raab$vi.denom.presb[raab$gender=="male"], raab$clusterId[raab$gender=="male"])
near.vi.presb.prev.total$male.pct.uci    <- bennett.uci(near.vi.presb.prev.total$male.pct, raab$near.vi.presb[raab$gender=="male"], raab$vi.denom.presb[raab$gender=="male"], raab$clusterId[raab$gender=="male"])

near.vi.presb.prev.total$total.n         <- sum(raab$near.vi.presb, na.rm=T)
near.vi.presb.prev.total$total.pct       <- sum(raab$near.vi.presb, na.rm=T) / sum(raab$vi.denom.presb, na.rm=T)
near.vi.presb.prev.total$total.pct.lci   <- bennett.lci(near.vi.presb.prev.total$total.pct, raab$near.vi.presb, raab$vi.denom.presb, raab$clusterId)
near.vi.presb.prev.total$total.pct.uci   <- bennett.uci(near.vi.presb.prev.total$total.pct, raab$near.vi.presb, raab$vi.denom.presb, raab$clusterId)

# Share of each sex's sample that is presbyopia-eligible (distance PinVA ≥6/12)
presb.frac.f     <- sum(raab$vi.denom.presb[raab$gender=="female"], na.rm=T) / sum(raab$vi.denom[raab$gender=="female"], na.rm=T)
presb.frac.m     <- sum(raab$vi.denom.presb[raab$gender=="male"],   na.rm=T) / sum(raab$vi.denom[raab$gender=="male"],   na.rm=T)
presb.frac.total <- sum(raab$vi.denom.presb, na.rm=T) / sum(raab$vi.denom, na.rm=T)

near.vi.presb.prev.total$female.adj.pct        <- prop.age.adjust(female.subpop, raab[raab$gender=="female",], raab$near.vi.presb[raab$gender=="female"], raab$vi.denom.presb[raab$gender=="female"])
near.vi.presb.prev.total$female.adj.pct.lci    <- bennett.lci(near.vi.presb.prev.total$female.adj.pct, raab$near.vi.presb[raab$gender=="female"], raab$vi.denom.presb[raab$gender=="female"], raab$clusterId[raab$gender=="female"])
near.vi.presb.prev.total$female.adj.pct.uci    <- bennett.uci(near.vi.presb.prev.total$female.adj.pct, raab$near.vi.presb[raab$gender=="female"], raab$vi.denom.presb[raab$gender=="female"], raab$clusterId[raab$gender=="female"])
near.vi.presb.prev.total$female.extrapolated.n <- round(near.vi.presb.prev.total$female.adj.pct * sum(female.subpop$population, na.rm=T) * presb.frac.f, 0)

near.vi.presb.prev.total$male.adj.pct        <- prop.age.adjust(male.subpop, raab[raab$gender=="male",], raab$near.vi.presb[raab$gender=="male"], raab$vi.denom.presb[raab$gender=="male"])
near.vi.presb.prev.total$male.adj.pct.lci    <- bennett.lci(near.vi.presb.prev.total$male.adj.pct, raab$near.vi.presb[raab$gender=="male"], raab$vi.denom.presb[raab$gender=="male"], raab$clusterId[raab$gender=="male"])
near.vi.presb.prev.total$male.adj.pct.uci    <- bennett.uci(near.vi.presb.prev.total$male.adj.pct, raab$near.vi.presb[raab$gender=="male"], raab$vi.denom.presb[raab$gender=="male"], raab$clusterId[raab$gender=="male"])
near.vi.presb.prev.total$male.extrapolated.n <- round(near.vi.presb.prev.total$male.adj.pct * sum(male.subpop$population, na.rm=T) * presb.frac.m, 0)

near.vi.presb.prev.total$total.adj.pct        <- prop.age.sex.adjust(popfives, raab, raab$near.vi.presb, raab$vi.denom.presb)
near.vi.presb.prev.total$total.adj.pct.lci    <- bennett.lci(near.vi.presb.prev.total$total.adj.pct, raab$near.vi.presb, raab$vi.denom.presb, raab$clusterId)
near.vi.presb.prev.total$total.adj.pct.uci    <- bennett.uci(near.vi.presb.prev.total$total.adj.pct, raab$near.vi.presb, raab$vi.denom.presb, raab$clusterId)
near.vi.presb.prev.total$total.extrapolated.n <- round(near.vi.presb.prev.total$total.adj.pct * sum(popfives$population, na.rm=T) * presb.frac.total, 0)

lcis <- grep("lci", names(near.vi.presb.prev.total))
ucis <- grep("uci", names(near.vi.presb.prev.total))
near.vi.presb.prev.total[,lcis][near.vi.presb.prev.total[,lcis] < 0] <- 0
near.vi.presb.prev.total[,ucis][near.vi.presb.prev.total[,ucis] > 1] <- 1

pcts <- grep("pct", names(near.vi.presb.prev.total))
near.vi.presb.prev.total[,pcts] <- round(near.vi.presb.prev.total[,pcts] * 100, 1)
near.vi.presb.prev.total[,pcts] <- format(near.vi.presb.prev.total[,pcts], nsmall=1)
