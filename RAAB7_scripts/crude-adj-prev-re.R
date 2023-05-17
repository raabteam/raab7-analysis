# crude and adjusted prevalence of refractive error by age group and gender
# refractive error defined as UCVA less than 6/12 improving to 6/12 with correction or pinhole (i.e., includes met and unmet re need)

re.prev<-data.frame(age.groups.tens)
re.prev[,2:22] <- NA
names(re.prev) <- c("age.groups.tens",
                    
                    "female.n",
                    "female.pct",
                    "female.pct.lci",
                    "female.pct.uci",
                    "female.adj.pct",
                    "female.adj.pct.lci",
                    "female.adj.pct.uci",
                    
                    "male.n",
                    "male.pct",
                    "male.pct.lci",
                    "male.pct.uci",
                    "male.adj.pct",
                    "male.adj.pct.lci",
                    "male.adj.pct.uci",
                    
                    "total.n",
                    "total.pct",
                    "total.pct.lci",
                    "total.pct.uci",
                    "total.adj.pct",
                    "total.adj.pct.lci",
                    "total.adj.pct.uci")

for (i in 1:length(age.groups.tens)) {
  
  re.prev$female.n[i] <- sum(raab$ref.error[raab$gender=="female" & raab$age.groups.tens==age.groups.tens[i]],na.rm=T) 
  re.prev$female.pct[i] <- sum(raab$ref.error[raab$gender=="female" & raab$age.groups.tens==age.groups.tens[i]],na.rm=T) / sum(raab$vi.denom[raab$gender=="female" & raab$age.groups.tens==age.groups.tens[i]],na.rm=T) 
  re.prev$female.pct.lci[i] <- bennett.lci(re.prev$female.pct[i], raab$ref.error[raab$gender=="female" & raab$age.groups.tens==age.groups.tens[i]], raab$vi.denom[raab$gender=="female" & raab$age.groups.tens==age.groups.tens[i]], raab$clusterId[raab$gender=="female" & raab$age.groups.tens==age.groups.tens[i]])
  re.prev$female.pct.uci[i] <- bennett.uci(re.prev$female.pct[i], raab$ref.error[raab$gender=="female" & raab$age.groups.tens==age.groups.tens[i]], raab$vi.denom[raab$gender=="female" & raab$age.groups.tens==age.groups.tens[i]], raab$clusterId[raab$gender=="female" & raab$age.groups.tens==age.groups.tens[i]])  
  
  # re.prev$female.adj.pct[i] <- prop.age.adjust()
  # re.prev$female.adj.pct.lci[i] <- bennett.lci()
  # re.prev$female.adj.pct.uci[i] <- bennett.uci()
  
  re.prev$male.n[i] <- sum(raab$ref.error[raab$gender=="male" & raab$age.groups.tens==age.groups.tens[i]],na.rm=T) 
  re.prev$male.pct[i] <- sum(raab$ref.error[raab$gender=="male" & raab$age.groups.tens==age.groups.tens[i]],na.rm=T) / sum(raab$vi.denom[raab$gender=="male" & raab$age.groups.tens==age.groups.tens[i]],na.rm=T)
  re.prev$male.pct.lci[i] <- bennett.lci(re.prev$male.pct[i], raab$ref.error[raab$gender=="male" & raab$age.groups.tens==age.groups.tens[i]], raab$vi.denom[raab$gender=="male" & raab$age.groups.tens==age.groups.tens[i]], raab$clusterId[raab$gender=="male" & raab$age.groups.tens==age.groups.tens[i]])
  re.prev$male.pct.uci[i] <- bennett.uci(re.prev$male.pct[i], raab$ref.error[raab$gender=="male" & raab$age.groups.tens==age.groups.tens[i]], raab$vi.denom[raab$gender=="male" & raab$age.groups.tens==age.groups.tens[i]], raab$clusterId[raab$gender=="male" & raab$age.groups.tens==age.groups.tens[i]])  
  
  # re.prev$male.adj.pct[i] <- prop.age.adjust()
  # re.prev$male.adj.pct.lci[i] <- bennett.lci()
  # re.prev$male.adj.pct.uci[i] <- bennett.uci()
  
  re.prev$total.n[i] <- sum(raab$ref.error[raab$age.groups.tens==age.groups.tens[i]]==1,na.rm=T) 
  re.prev$total.pct[i] <- sum(raab$ref.error[raab$age.groups.tens==age.groups.tens[i]]==1,na.rm=T) / sum(raab$vi.denom[raab$age.groups.tens==age.groups.tens[i]]==1,na.rm=T)
  re.prev$total.pct.lci[i] <- bennett.lci(re.prev$total.pct[i], raab$ref.error[raab$age.groups.tens==age.groups.tens[i]], raab$vi.denom[raab$age.groups.tens==age.groups.tens[i]], raab$clusterId[raab$age.groups.tens==age.groups.tens[i]])
  re.prev$total.pct.uci[i] <- bennett.uci(re.prev$total.pct[i], raab$ref.error[raab$age.groups.tens==age.groups.tens[i]], raab$vi.denom[raab$age.groups.tens==age.groups.tens[i]], raab$clusterId[raab$age.groups.tens==age.groups.tens[i]])  
  
  # re.prev$total.adj.pct[i] <- prop.age.sex.adjust()
  # re.prev$total.adj.pct.lci[i] <- bennett.lci()
  # re.prev$total.adj.pct.uci[i] <- bennett.uci()
    
}

# Create a separate row for the overall prevalence of refractive error
re.prev.total<-data.frame('Total')
re.prev.total[,2:22] <- NA
names(re.prev.total) <- c("age.groups.tens",
                    
                    "female.n",
                    "female.pct",
                    "female.pct.lci",
                    "female.pct.uci",
                    "female.adj.pct",
                    "female.adj.pct.lci",
                    "female.adj.pct.uci",
                    
                    "male.n",
                    "male.pct",
                    "male.pct.lci",
                    "male.pct.uci",
                    "male.adj.pct",
                    "male.adj.pct.lci",
                    "male.adj.pct.uci",
                    
                    "total.n",
                    "total.pct",
                    "total.pct.lci",
                    "total.pct.uci",
                    "total.adj.pct",
                    "total.adj.pct.lci",
                    "total.adj.pct.uci")

re.prev.total$female.n <- sum(raab$ref.error[raab$gender=="female"],na.rm=T) 
re.prev.total$female.pct <- sum(raab$ref.error[raab$gender=="female"],na.rm=T) / sum(raab$vi.denom[raab$gender=="female"],na.rm=T) 
re.prev.total$female.pct.lci <- bennett.lci(re.prev.total$female.pct, raab$ref.error[raab$gender=="female"], raab$vi.denom[raab$gender=="female"], raab$clusterId[raab$gender=="female"])
re.prev.total$female.pct.uci <- bennett.uci(re.prev.total$female.pct, raab$ref.error[raab$gender=="female"], raab$vi.denom[raab$gender=="female"], raab$clusterId[raab$gender=="female"])  

# re.prev.total$female.adj.pct <- prop.age.adjust()
# re.prev.total$female.adj.pct.lci <- bennett.lci()
# re.prev.total$female.adj.pct.uci <- bennett.uci()

re.prev.total$male.n <- sum(raab$ref.error[raab$gender=="male"],na.rm=T) 
re.prev.total$male.pct <- sum(raab$ref.error[raab$gender=="male"],na.rm=T) / sum(raab$vi.denom[raab$gender=="male"],na.rm=T)
re.prev.total$male.pct.lci <- bennett.lci(re.prev.total$male.pct, raab$ref.error[raab$gender=="male"], raab$vi.denom[raab$gender=="male"], raab$clusterId[raab$gender=="male"])
re.prev.total$male.pct.uci <- bennett.uci(re.prev.total$male.pct, raab$ref.error[raab$gender=="male"], raab$vi.denom[raab$gender=="male"], raab$clusterId[raab$gender=="male"])  

# re.prev.total$male.adj.pct <- prop.age.adjust()
# re.prev.total$male.adj.pct.lci <- bennett.lci()
# re.prev.total$male.adj.pct.uci <- bennett.uci()

re.prev.total$total.n <- sum(raab$ref.error,na.rm=T) 
re.prev.total$total.pct <- sum(raab$ref.error,na.rm=T) / sum(raab$vi.denom,na.rm=T)
re.prev.total$total.pct.lci <- bennett.lci(re.prev.total$total.pct, raab$ref.error, raab$vi.denom, raab$clusterId)
re.prev.total$total.pct.uci <- bennett.uci(re.prev.total$total.pct, raab$ref.error, raab$vi.denom, raab$clusterId)  

# re.prev.total$total.adj.pct <- prop.age.sex.adjust()
# re.prev.total$total.adj.pct.lci <- bennett.lci()
# re.prev.total$total.adj.pct.uci <- bennett.uci()



# Combine age group prev table with overall prev table
re.prev.final <- rbind(re.prev, re.prev.total)
re.prev.final[5,1] <- "Total"

# Round pct values
pcts <- grep("pct",names(re.prev.final))
re.prev.final[,pcts] <- round(re.prev.final[,pcts] * 100, 1)
re.prev.final[,pcts] <- format(re.prev.final[,pcts], nsmall=1)