# crude prevalence of refractive error by age group and gender
# refractive error defined as UCVA less than 6/12 improving to 6/12 with correction or pinhole (i.e., includes met and unmet re need)

dist.re.prev<-data.frame(age.groups.tens)
dist.re.prev[,2:13] <- NA
names(dist.re.prev) <- c("age.groups.tens",
                    
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
  
  dist.re.prev$female.n[i] <- sum(raab$ref.error[raab$gender=="female" & raab$age.groups.tens==age.groups.tens[i]],na.rm=T) 
  dist.re.prev$female.pct[i] <- sum(raab$ref.error[raab$gender=="female" & raab$age.groups.tens==age.groups.tens[i]],na.rm=T) / sum(raab$vi.denom[raab$gender=="female" & raab$age.groups.tens==age.groups.tens[i]],na.rm=T) 
  dist.re.prev$female.pct.lci[i] <- bennett.lci(dist.re.prev$female.pct[i], raab$ref.error[raab$gender=="female" & raab$age.groups.tens==age.groups.tens[i]], raab$vi.denom[raab$gender=="female" & raab$age.groups.tens==age.groups.tens[i]], raab$clusterId[raab$gender=="female" & raab$age.groups.tens==age.groups.tens[i]])
  dist.re.prev$female.pct.uci[i] <- bennett.uci(dist.re.prev$female.pct[i], raab$ref.error[raab$gender=="female" & raab$age.groups.tens==age.groups.tens[i]], raab$vi.denom[raab$gender=="female" & raab$age.groups.tens==age.groups.tens[i]], raab$clusterId[raab$gender=="female" & raab$age.groups.tens==age.groups.tens[i]])  

  dist.re.prev$male.n[i] <- sum(raab$ref.error[raab$gender=="male" & raab$age.groups.tens==age.groups.tens[i]],na.rm=T) 
  dist.re.prev$male.pct[i] <- sum(raab$ref.error[raab$gender=="male" & raab$age.groups.tens==age.groups.tens[i]],na.rm=T) / sum(raab$vi.denom[raab$gender=="male" & raab$age.groups.tens==age.groups.tens[i]],na.rm=T)
  dist.re.prev$male.pct.lci[i] <- bennett.lci(dist.re.prev$male.pct[i], raab$ref.error[raab$gender=="male" & raab$age.groups.tens==age.groups.tens[i]], raab$vi.denom[raab$gender=="male" & raab$age.groups.tens==age.groups.tens[i]], raab$clusterId[raab$gender=="male" & raab$age.groups.tens==age.groups.tens[i]])
  dist.re.prev$male.pct.uci[i] <- bennett.uci(dist.re.prev$male.pct[i], raab$ref.error[raab$gender=="male" & raab$age.groups.tens==age.groups.tens[i]], raab$vi.denom[raab$gender=="male" & raab$age.groups.tens==age.groups.tens[i]], raab$clusterId[raab$gender=="male" & raab$age.groups.tens==age.groups.tens[i]])  
  
  dist.re.prev$total.n[i] <- sum(raab$ref.error[raab$age.groups.tens==age.groups.tens[i]]==1,na.rm=T) 
  dist.re.prev$total.pct[i] <- sum(raab$ref.error[raab$age.groups.tens==age.groups.tens[i]]==1,na.rm=T) / sum(raab$vi.denom[raab$age.groups.tens==age.groups.tens[i]]==1,na.rm=T)
  dist.re.prev$total.pct.lci[i] <- bennett.lci(dist.re.prev$total.pct[i], raab$ref.error[raab$age.groups.tens==age.groups.tens[i]], raab$vi.denom[raab$age.groups.tens==age.groups.tens[i]], raab$clusterId[raab$age.groups.tens==age.groups.tens[i]])
  dist.re.prev$total.pct.uci[i] <- bennett.uci(dist.re.prev$total.pct[i], raab$ref.error[raab$age.groups.tens==age.groups.tens[i]], raab$vi.denom[raab$age.groups.tens==age.groups.tens[i]], raab$clusterId[raab$age.groups.tens==age.groups.tens[i]])  

}

# Create a separate row for the overall prevalence of refractive error
dist.re.prev.total<-data.frame('Total')
dist.re.prev.total[,2:13] <- NA
names(dist.re.prev.total) <- c("age.groups.tens",
                    
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

dist.re.prev.total$female.n <- sum(raab$ref.error[raab$gender=="female"],na.rm=T) 
dist.re.prev.total$female.pct <- sum(raab$ref.error[raab$gender=="female"],na.rm=T) / sum(raab$vi.denom[raab$gender=="female"],na.rm=T) 
dist.re.prev.total$female.pct.lci <- bennett.lci(dist.re.prev.total$female.pct, raab$ref.error[raab$gender=="female"], raab$vi.denom[raab$gender=="female"], raab$clusterId[raab$gender=="female"])
dist.re.prev.total$female.pct.uci <- bennett.uci(dist.re.prev.total$female.pct, raab$ref.error[raab$gender=="female"], raab$vi.denom[raab$gender=="female"], raab$clusterId[raab$gender=="female"])  

dist.re.prev.total$male.n <- sum(raab$ref.error[raab$gender=="male"],na.rm=T) 
dist.re.prev.total$male.pct <- sum(raab$ref.error[raab$gender=="male"],na.rm=T) / sum(raab$vi.denom[raab$gender=="male"],na.rm=T)
dist.re.prev.total$male.pct.lci <- bennett.lci(dist.re.prev.total$male.pct, raab$ref.error[raab$gender=="male"], raab$vi.denom[raab$gender=="male"], raab$clusterId[raab$gender=="male"])
dist.re.prev.total$male.pct.uci <- bennett.uci(dist.re.prev.total$male.pct, raab$ref.error[raab$gender=="male"], raab$vi.denom[raab$gender=="male"], raab$clusterId[raab$gender=="male"])  

dist.re.prev.total$total.n <- sum(raab$ref.error,na.rm=T) 
dist.re.prev.total$total.pct <- sum(raab$ref.error,na.rm=T) / sum(raab$vi.denom,na.rm=T)
dist.re.prev.total$total.pct.lci <- bennett.lci(dist.re.prev.total$total.pct, raab$ref.error, raab$vi.denom, raab$clusterId)
dist.re.prev.total$total.pct.uci <- bennett.uci(dist.re.prev.total$total.pct, raab$ref.error, raab$vi.denom, raab$clusterId)  

# Combine age group prev table with overall prev table
dist.re.prev.final <- rbind(dist.re.prev, dist.re.prev.total)
dist.re.prev.final[5,1] <- "Total"

lcis<-grep("lci",names(dist.re.prev.final))
ucis<-grep("uci",names(dist.re.prev.final))
dist.re.prev.final[,lcis][dist.re.prev.final[,lcis]<0]<-0
dist.re.prev.final[,ucis][dist.re.prev.final[,ucis]>1]<-1

# Round pct values
pcts <- grep("pct",names(dist.re.prev.final))
dist.re.prev.final[,pcts] <- round(dist.re.prev.final[,pcts] * 100, 1)
dist.re.prev.final[,pcts] <- format(dist.re.prev.final[,pcts], nsmall=1)

