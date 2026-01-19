# output required in summary_out.csv for equity figure on raab.world

# sex-adjusted blindness prevalence by age groups (age_50_64 or age_65_plus)

equity.blindness.age<-data.frame(vi.levels)
equity.blindness.age[,2:22] <- NA
names(equity.blindness.age) <- c("vi.level",
                 
                 "age_50_64.n",
                 "age_50_64.pct",
                 "age_50_64.pct.lci",
                 "age_50_64.pct.uci",
                 "age_50_64.adj.pct",
                 "age_50_64.adj.pct.lci",
                 "age_50_64.adj.pct.uci",
                 
                 "age_65_plus.n",
                 "age_65_plus.pct",
                 "age_65_plus.pct.lci",
                 "age_65_plus.pct.uci",
                 "age_65_plus.adj.pct",
                 "age_65_plus.adj.pct.lci",
                 "age_65_plus.adj.pct.uci",
                 
                 "total.n",
                 "total.pct",
                 "total.pct.lci",
                 "total.pct.uci",
                 "total.adj.pct",
                 "total.adj.pct.lci",
                 "total.adj.pct.uci")


for (i in 1:length(vi.levels))
{
  
  equity.blindness.age$age_50_64.n[i]<-sum(raab[raab$age.groups.working=="age_50_64",vi.levels[i]],na.rm=T)
  equity.blindness.age$age_65_plus.n[i]<-sum(raab[raab$age.groups.working=="age_65_plus",vi.levels[i]],na.rm=T)
  equity.blindness.age$total.n[i]<-sum(raab[,vi.levels[i]],na.rm=T)
  
  equity.blindness.age$age_50_64.pct[i]<-sum(raab[raab$age.groups.working=="age_50_64",vi.levels[i]],na.rm=T)/sum(raab$vi.denom[raab$age.groups.working=="age_50_64"],na.rm=T)
  equity.blindness.age$age_65_plus.pct[i]<-sum(raab[raab$age.groups.working=="age_65_plus",vi.levels[i]],na.rm=T)/sum(raab$vi.denom[raab$age.groups.working=="age_65_plus"],na.rm=T)
  equity.blindness.age$total.pct[i]<-sum(raab[,vi.levels[i]],na.rm=T)/sum(raab$vi.denom,na.rm=T)
  
  equity.blindness.age$age_50_64.pct.lci[i]<-bennett.lci(equity.blindness.age$age_50_64.pct[i],raab[raab$age.groups.working=="age_50_64",vi.levels[i]],raab$vi.denom[raab$age.groups.working=="age_50_64"],raab$clusterId[raab$age.groups.working=="age_50_64"])
  equity.blindness.age$age_65_plus.pct.lci[i]<-bennett.lci(equity.blindness.age$age_65_plus.pct[i],raab[raab$age.groups.working=="age_65_plus",vi.levels[i]],raab$vi.denom[raab$age.groups.working=="age_65_plus"],raab$clusterId[raab$age.groups.working=="age_65_plus"])
  equity.blindness.age$total.pct.lci[i]<-bennett.lci(equity.blindness.age$total.pct[i],raab[,vi.levels[i]],raab$vi.denom,raab$clusterId)
  
  equity.blindness.age$age_50_64.pct.uci[i]<-bennett.uci(equity.blindness.age$age_50_64.pct[i],raab[raab$age.groups.working=="age_50_64",vi.levels[i]],raab$vi.denom[raab$age.groups.working=="age_50_64"],raab$clusterId[raab$age.groups.working=="age_50_64"])
  equity.blindness.age$age_65_plus.pct.uci[i]<-bennett.uci(equity.blindness.age$age_65_plus.pct[i],raab[raab$age.groups.working=="age_65_plus",vi.levels[i]],raab$vi.denom[raab$age.groups.working=="age_65_plus"],raab$clusterId[raab$age.groups.working=="age_65_plus"])
  equity.blindness.age$total.pct.uci[i]<-bennett.uci(equity.blindness.age$total.pct[i],raab[,vi.levels[i]],raab$vi.denom,raab$clusterId)
  
  equity.blindness.age$age_50_64.adj.pct[i]<-prop.sex.adjust(age.5064.subpop, raab[raab$age.groups.working=="age_50_64",], raab[raab$age.groups.working=="age_50_64",vi.levels[i]], raab$vi.denom[raab$age.groups.working=="age_50_64"])
  equity.blindness.age$age_65_plus.adj.pct[i]<-prop.sex.adjust(age.65p.subpop, raab[raab$age.groups.working=="age_65_plus",], raab[raab$age.groups.working=="age_65_plus",vi.levels[i]], raab$vi.denom[raab$age.groups.working=="age_65_plus"])
  equity.blindness.age$total.adj.pct[i]<-prop.age.sex.adjust(popfives, raab, raab[,vi.levels[i]], raab$vi.denom)
  
  equity.blindness.age$age_50_64.adj.pct.lci[i]<-bennett.lci(equity.blindness.age$age_50_64.adj.pct[i],raab[raab$age.groups.working=="age_50_64",vi.levels[i]],raab$vi.denom[raab$age.groups.working=="age_50_64"],raab$clusterId[raab$age.groups.working=="age_50_64"])
  equity.blindness.age$age_65_plus.adj.pct.lci[i]<-bennett.lci(equity.blindness.age$age_65_plus.adj.pct[i],raab[raab$age.groups.working=="age_65_plus",vi.levels[i]],raab$vi.denom[raab$age.groups.working=="age_65_plus"],raab$clusterId[raab$age.groups.working=="age_65_plus"])
  equity.blindness.age$total.adj.pct.lci[i]<-bennett.lci(equity.blindness.age$total.adj.pct[i],raab[,vi.levels[i]],raab$vi.denom,raab$clusterId)
  
  equity.blindness.age$age_50_64.adj.pct.uci[i]<-bennett.uci(equity.blindness.age$age_50_64.adj.pct[i],raab[raab$age.groups.working=="age_50_64",vi.levels[i]],raab$vi.denom[raab$age.groups.working=="age_50_64"],raab$clusterId[raab$age.groups.working=="age_50_64"])
  equity.blindness.age$age_65_plus.adj.pct.uci[i]<-bennett.uci(equity.blindness.age$age_65_plus.adj.pct[i],raab[raab$age.groups.working=="age_65_plus",vi.levels[i]],raab$vi.denom[raab$age.groups.working=="age_65_plus"],raab$clusterId[raab$age.groups.working=="age_65_plus"])
  equity.blindness.age$total.adj.pct.uci[i]<-bennett.uci(equity.blindness.age$total.adj.pct[i],raab[,vi.levels[i]],raab$vi.denom,raab$clusterId)
  
}


lcis<-grep("lci",names(equity.blindness.age))
ucis<-grep("uci",names(equity.blindness.age))
equity.blindness.age[,lcis][equity.blindness.age[,lcis]<0]<-0
equity.blindness.age[,ucis][equity.blindness.age[,ucis]>1]<-1

pcts<-grep("pct",names(equity.blindness.age))
equity.blindness.age[,pcts]<-round(equity.blindness.age[,pcts] * 100, 1 )
equity.blindness.age[,pcts]<-format(equity.blindness.age[,pcts], nsmall=1)


