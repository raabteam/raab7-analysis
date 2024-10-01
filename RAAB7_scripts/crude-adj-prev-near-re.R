# crude prevalence of refractive error by age group and gender
# near refractive error defined as presenting near VA (only one level, binary screening test at N6 threshold)

near.re.prev<-data.frame(age.groups.tens)
near.re.prev[,2:13] <- NA
names(near.re.prev) <- c("age.groups.tens",
                    
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
  
  near.re.prev$female.n[i] <- sum(raab$near.vi[raab$gender=="female" & raab$age.groups.tens==age.groups.tens[i]],na.rm=T) 
  near.re.prev$female.pct[i] <- sum(raab$near.vi[raab$gender=="female" & raab$age.groups.tens==age.groups.tens[i]],na.rm=T) / sum(raab$vi.denom[raab$gender=="female" & raab$age.groups.tens==age.groups.tens[i]],na.rm=T) 
  near.re.prev$female.pct.lci[i] <- bennett.lci(near.re.prev$female.pct[i], raab$near.vi[raab$gender=="female" & raab$age.groups.tens==age.groups.tens[i]], raab$vi.denom[raab$gender=="female" & raab$age.groups.tens==age.groups.tens[i]], raab$clusterId[raab$gender=="female" & raab$age.groups.tens==age.groups.tens[i]])
  near.re.prev$female.pct.uci[i] <- bennett.uci(near.re.prev$female.pct[i], raab$near.vi[raab$gender=="female" & raab$age.groups.tens==age.groups.tens[i]], raab$vi.denom[raab$gender=="female" & raab$age.groups.tens==age.groups.tens[i]], raab$clusterId[raab$gender=="female" & raab$age.groups.tens==age.groups.tens[i]])  

  near.re.prev$male.n[i] <- sum(raab$near.vi[raab$gender=="male" & raab$age.groups.tens==age.groups.tens[i]],na.rm=T) 
  near.re.prev$male.pct[i] <- sum(raab$near.vi[raab$gender=="male" & raab$age.groups.tens==age.groups.tens[i]],na.rm=T) / sum(raab$vi.denom[raab$gender=="male" & raab$age.groups.tens==age.groups.tens[i]],na.rm=T)
  near.re.prev$male.pct.lci[i] <- bennett.lci(near.re.prev$male.pct[i], raab$near.vi[raab$gender=="male" & raab$age.groups.tens==age.groups.tens[i]], raab$vi.denom[raab$gender=="male" & raab$age.groups.tens==age.groups.tens[i]], raab$clusterId[raab$gender=="male" & raab$age.groups.tens==age.groups.tens[i]])
  near.re.prev$male.pct.uci[i] <- bennett.uci(near.re.prev$male.pct[i], raab$near.vi[raab$gender=="male" & raab$age.groups.tens==age.groups.tens[i]], raab$vi.denom[raab$gender=="male" & raab$age.groups.tens==age.groups.tens[i]], raab$clusterId[raab$gender=="male" & raab$age.groups.tens==age.groups.tens[i]])  
  
  near.re.prev$total.n[i] <- sum(raab$near.vi[raab$age.groups.tens==age.groups.tens[i]]==1,na.rm=T) 
  near.re.prev$total.pct[i] <- sum(raab$near.vi[raab$age.groups.tens==age.groups.tens[i]]==1,na.rm=T) / sum(raab$vi.denom[raab$age.groups.tens==age.groups.tens[i]]==1,na.rm=T)
  near.re.prev$total.pct.lci[i] <- bennett.lci(near.re.prev$total.pct[i], raab$near.vi[raab$age.groups.tens==age.groups.tens[i]], raab$vi.denom[raab$age.groups.tens==age.groups.tens[i]], raab$clusterId[raab$age.groups.tens==age.groups.tens[i]])
  near.re.prev$total.pct.uci[i] <- bennett.uci(near.re.prev$total.pct[i], raab$near.vi[raab$age.groups.tens==age.groups.tens[i]], raab$vi.denom[raab$age.groups.tens==age.groups.tens[i]], raab$clusterId[raab$age.groups.tens==age.groups.tens[i]])  

}

# Create a separate row for the overall prevalence of refractive error

near.re.prev.total<-data.frame('near.vi')
near.re.prev.total[,2:25] <- NA
names(near.re.prev.total) <- c("age.groups.tens",
                    
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

near.re.prev.total$female.n <- sum(raab$near.vi[raab$gender=="female"],na.rm=T) 
near.re.prev.total$female.pct <- sum(raab$near.vi[raab$gender=="female"],na.rm=T) / sum(raab$vi.denom[raab$gender=="female"],na.rm=T) 
near.re.prev.total$female.pct.lci <- bennett.lci(near.re.prev.total$female.pct, raab$near.vi[raab$gender=="female"], raab$vi.denom[raab$gender=="female"], raab$clusterId[raab$gender=="female"])
near.re.prev.total$female.pct.uci <- bennett.uci(near.re.prev.total$female.pct, raab$near.vi[raab$gender=="female"], raab$vi.denom[raab$gender=="female"], raab$clusterId[raab$gender=="female"])  

near.re.prev.total$male.n <- sum(raab$near.vi[raab$gender=="male"],na.rm=T) 
near.re.prev.total$male.pct <- sum(raab$near.vi[raab$gender=="male"],na.rm=T) / sum(raab$vi.denom[raab$gender=="male"],na.rm=T)
near.re.prev.total$male.pct.lci <- bennett.lci(near.re.prev.total$male.pct, raab$near.vi[raab$gender=="male"], raab$vi.denom[raab$gender=="male"], raab$clusterId[raab$gender=="male"])
near.re.prev.total$male.pct.uci <- bennett.uci(near.re.prev.total$male.pct, raab$near.vi[raab$gender=="male"], raab$vi.denom[raab$gender=="male"], raab$clusterId[raab$gender=="male"])  

near.re.prev.total$total.n <- sum(raab$near.vi,na.rm=T) 
near.re.prev.total$total.pct <- sum(raab$near.vi,na.rm=T) / sum(raab$vi.denom,na.rm=T)
near.re.prev.total$total.pct.lci <- bennett.lci(near.re.prev.total$total.pct, raab$near.vi, raab$vi.denom, raab$clusterId)
near.re.prev.total$total.pct.uci <- bennett.uci(near.re.prev.total$total.pct, raab$near.vi, raab$vi.denom, raab$clusterId)  

near.re.prev.total$female.adj.pct <- prop.age.adjust(female.subpop,raab[raab$gender=="female",],raab$near.vi[raab$gender=="female"],raab$vi.denom[raab$gender=="female"]) 
near.re.prev.total$female.adj.pct.lci <- bennett.lci(near.re.prev.total$female.adj.pct, raab$near.vi[raab$gender=="female"], raab$vi.denom[raab$gender=="female"], raab$clusterId[raab$gender=="female"])
near.re.prev.total$female.adj.pct.uci <- bennett.uci(near.re.prev.total$female.adj.pct, raab$near.vi[raab$gender=="female"], raab$vi.denom[raab$gender=="female"], raab$clusterId[raab$gender=="female"])  
near.re.prev.total$female.extrapolated.n <- round( near.re.prev.total$female.adj.pct * sum(female.subpop$population,na.rm=T), 0) 

near.re.prev.total$male.adj.pct <- prop.age.adjust(male.subpop, raab[raab$gender=="male",],raab$near.vi[raab$gender=="male"],raab$vi.denom[raab$gender=="male"])
near.re.prev.total$male.adj.pct.lci <- bennett.lci(near.re.prev.total$male.adj.pct, raab$near.vi[raab$gender=="male"], raab$vi.denom[raab$gender=="male"], raab$clusterId[raab$gender=="male"])
near.re.prev.total$male.adj.pct.uci <- bennett.uci(near.re.prev.total$male.adj.pct, raab$near.vi[raab$gender=="male"], raab$vi.denom[raab$gender=="male"], raab$clusterId[raab$gender=="male"])  
near.re.prev.total$male.extrapolated.n <- round( near.re.prev.total$male.adj.pct * sum(male.subpop$population,na.rm=T), 0)

near.re.prev.total$total.adj.pct <- prop.age.sex.adjust(popfives, raab, raab$near.vi, raab$vi.denom)
near.re.prev.total$total.adj.pct.lci <- bennett.lci(near.re.prev.total$total.adj.pct, raab$near.vi, raab$vi.denom, raab$clusterId)
near.re.prev.total$total.adj.pct.uci <- bennett.uci(near.re.prev.total$total.adj.pct, raab$near.vi, raab$vi.denom, raab$clusterId)  
near.re.prev.total$total.extrapolated.n <- round( near.re.prev.total$total.adj.pct * sum(popfives$population,na.rm=T), 0)

# Combine age group prev table with overall prev table
near.re.prev.final <- rbind(near.re.prev, near.re.prev.total[,c(1:13)])
near.re.prev.final[5,1] <- "Total"

lcis<-grep("lci",names(near.re.prev.final))
ucis<-grep("uci",names(near.re.prev.final))
near.re.prev.final[,lcis][near.re.prev.final[,lcis]<0]<-0
near.re.prev.final[,ucis][near.re.prev.final[,ucis]>1]<-1

# Round pct values
pcts <- grep("pct",names(near.re.prev.final))

near.re.prev.final[,pcts] <- round(near.re.prev.final[,pcts] * 100, 1)
near.re.prev.final[,pcts] <- format(near.re.prev.final[,pcts], nsmall=1)

#Fix crude and adjusted rows

lcis<-grep("lci",names(near.re.prev.total))
ucis<-grep("uci",names(near.re.prev.total))
near.re.prev.total[,lcis][near.re.prev.total[,lcis]<0]<-0
near.re.prev.total[,ucis][near.re.prev.total[,ucis]>1]<-1

# Round pct values
pcts <- grep("pct",names(near.re.prev.total))

near.re.prev.total[,pcts] <- round(near.re.prev.total[,pcts] * 100, 1)
near.re.prev.total[,pcts] <- format(near.re.prev.total[,pcts], nsmall=1)
