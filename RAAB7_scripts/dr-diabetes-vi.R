# DR module: VI by diabetes status
# 05.07.22 IM

dm.vi.table<-data.frame(vi.levels)
dm.vi.table[,2:9] <- NA
names(dm.vi.table) <- c("vi.level",
                         
                         "diabetic.n",
                         "diabetic.pct",
                         "diabetic.pct.lci",
                         "diabetic.pct.uci",
                         # "diabetic.adj.pct",
                         # "diabetic.adj.pct.lci",
                         # "diabetic.adj.pct.uci",
                         
                         "non.diabetic.n",
                         "non.diabetic.pct",
                         "non.diabetic.pct.lci",
                         "non.diabetic.pct.uci"
                         # "non.diabetic.adj.pct",
                         # "non.diabetic.adj.pct.lci",
                         # "non.diabetic.adj.pct.uci"
                         )

for (i in 1:length(vi.levels))
{
  
  dm.vi.table$diabetic.n[i]<-sum(raab[raab$diabetes.known.susp==1,vi.levels[i]],na.rm=T)
  dm.vi.table$diabetic.pct[i]<-sum(raab[raab$diabetes.known.susp==1,vi.levels[i]])/sum(raab$diabetes.known.susp==1,na.rm=T)
  
  dm.vi.table$diabetic.pct.lci[i]<-bennett.lci(dm.vi.table$diabetic.pct[i],raab[raab$diabetes.known.susp==1,vi.levels[i]],raab$diabetes.denom[raab$diabetes.known.susp==1],raab$clusterNumber[raab$diabetes.known.susp==1])
  dm.vi.table$diabetic.pct.uci[i]<-bennett.uci(dm.vi.table$diabetic.pct[i],raab[raab$diabetes.known.susp==1,vi.levels[i]],raab$diabetes.denom[raab$diabetes.known.susp==1],raab$clusterNumber[raab$diabetes.known.susp==1])
  
  dm.vi.table$non.diabetic.n[i]<-sum(raab[raab$diabetes.no==1,vi.levels[i]],na.rm=T)
  dm.vi.table$non.diabetic.pct[i]<-sum(raab[raab$diabetes.no==1,vi.levels[i]])/sum(raab$diabetes.no==1,na.rm=T)
  
  dm.vi.table$non.diabetic.pct.lci[i]<-bennett.lci(dm.vi.table$non.diabetic.pct[i],raab[raab$diabetes.no==1,vi.levels[i]],raab$vi.denom[raab$diabetes.no==1],raab$clusterNumber[raab$diabetes.no==1])
  dm.vi.table$non.diabetic.pct.uci[i]<-bennett.uci(dm.vi.table$non.diabetic.pct[i],raab[raab$diabetes.no==1,vi.levels[i]],raab$vi.denom[raab$diabetes.no==1],raab$clusterNumber[raab$diabetes.no==1])

  # No ASA included here
}  

lcis<-grep("lci",names(dm.vi.table))
ucis<-grep("uci",names(dm.vi.table))
dm.vi.table[,lcis][dm.vi.table[,lcis]<0]<-0
dm.vi.table[,ucis][dm.vi.table[,ucis]>1]<-1

pcts<-grep("pct",names(dm.vi.table))
dm.vi.table[,pcts]<-round(dm.vi.table[,pcts] * 100, 1 )

write.csv(dm.vi.table, here('outputs', 'dm.vi.table.csv'))
