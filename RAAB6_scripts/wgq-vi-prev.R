# Washington Group Questions (Short Set) disability prevalence by gender
# v1 24.05.22 IM

# four levels: none, some, a lot, cannot so
# a lot and cannot do = disability status for binary var

# VI by disability status (any disability, no disability, non-vi disability, seeing disability)

wgq.vi.table<-data.frame(vi.levels)
wgq.vi.table[,2:22] <- NA
names(wgq.vi.table) <- c("vi.level",
                             
                             "any.dis.n",
                             "any.dis.pct",
                             "any.dis.pct.lci",
                             "any.dis.pct.uci",
                             "any.dis.adj.pct",
                             "any.dis.adj.pct.lci",
                             "any.dis.adj.pct.uci",
                             
                             "any.non.vi.dis.n",
                             "any.non.vi.dis.pct",
                             "any.non.vi.dis.pct.lci",
                             "any.non.vi.dis.pct.uci",
                             "any.non.vi.dis.adj.pct",
                             "any.non.vi.dis.adj.pct.lci",
                             "any.non.vi.dis.adj.pct.uci",
                             
                             "no.dis.n",
                             "no.dis.pct",
                             "no.dis.pct.lci",
                             "no.dis.pct.uci",
                             "no.dis.adj.pct",
                             "no.dis.adj.pct.lci",
                             "no.dis.adj.pct.uci")

for (i in 1:length(vi.levels))
{
  
  wgq.vi.table$any.dis.n[i]<-sum(raab[raab$wgq.dis.any==1,vi.levels[i]])
  wgq.vi.table$any.non.vi.dis.n[i]<-sum(raab[raab$wgq.dis.nonvi==1,vi.levels[i]])
  wgq.vi.table$no.dis.n[i]<-sum(raab[raab$wgq.dis.any==0,vi.levels[i]])
  
  wgq.vi.table$any.dis.pct[i]<-sum(raab[raab$wgq.dis.any==1,vi.levels[i]])/sum(raab$wgq.dis.any==1)
  wgq.vi.table$any.non.vi.dis.pct[i]<-sum(raab[raab$wgq.dis.nonvi==1,vi.levels[i]])/sum(raab$wgq.dis.nonvi==1)
  wgq.vi.table$no.dis.pct[i]<-sum(raab[raab$wgq.dis.any==0,vi.levels[i]])/sum(raab$wgq.dis.any==0)

  wgq.vi.table$any.dis.pct.lci[i]<-bennett.lci(wgq.vi.table$any.dis.pct[i],raab[raab$wgq.dis.any==1,vi.levels[i]],raab$vi.denom[raab$wgq.dis.any==1],raab$clusterId[raab$wgq.dis.any==1])
  wgq.vi.table$any.dis.pct.uci[i]<-bennett.uci(wgq.vi.table$any.dis.pct[i],raab[raab$wgq.dis.any==1,vi.levels[i]],raab$vi.denom[raab$wgq.dis.any==1],raab$clusterId[raab$wgq.dis.any==1])
  wgq.vi.table$any.non.vi.dis.pct.lci[i]<-bennett.lci(wgq.vi.table$any.non.vi.dis.pct[i],raab[raab$wgq.dis.nonvi==1,vi.levels[i]],raab$vi.denom[raab$wgq.dis.nonvi==1],raab$clusterId[raab$wgq.dis.nonvi==1])
  wgq.vi.table$any.non.vi.dis.pct.uci[i]<-bennett.uci(wgq.vi.table$any.non.vi.dis.pct[i],raab[raab$wgq.dis.nonvi==1,vi.levels[i]],raab$vi.denom[raab$wgq.dis.nonvi==1],raab$clusterId[raab$wgq.dis.nonvi==1])
  wgq.vi.table$no.dis.pct.lci[i]<-bennett.lci(wgq.vi.table$no.dis.pct[i],raab[raab$wgq.dis.any==0,vi.levels[i]],raab$vi.denom[raab$wgq.dis.any==0],raab$clusterId[raab$wgq.dis.any==0])
  wgq.vi.table$no.dis.pct.uci[i]<-bennett.uci(wgq.vi.table$no.dis.pct[i],raab[raab$wgq.dis.any==0,vi.levels[i]],raab$vi.denom[raab$wgq.dis.any==0],raab$clusterId[raab$wgq.dis.any==0])
  
  wgq.vi.table$any.dis.adj.pct[i]<-prop.age.sex.adjust(popfives,raab,raab[raab$wgq.dis.any==1,vi.levels[i]],raab$vi.denom[raab$wgq.dis.any==1])
  wgq.vi.table$any.non.vi.dis.adj.pct[i]<-prop.age.sex.adjust(popfives,raab,raab[raab$wgq.dis.nonvi==1,vi.levels[i]],raab$vi.denom[raab$wgq.dis.nonvi==1])
  wgq.vi.table$no.dis.adj.pct[i]<-prop.age.sex.adjust(popfives,raab,raab[raab$wgq.dis.any==0,vi.levels[i]],raab$vi.denom[raab$wgq.dis.any==0])
  
  wgq.vi.table$any.dis.adj.pct.lci[i]<-bennett.lci(wgq.vi.table$any.dis.adj.pct[i],raab[raab$wgq.dis.any==1,vi.levels[i]],raab$vi.denom[raab$wgq.dis.any==1],raab$clusterId[raab$wgq.dis.any==1])
  wgq.vi.table$any.dis.adj.pct.uci[i]<-bennett.uci(wgq.vi.table$any.dis.adj.pct[i],raab[raab$wgq.dis.any==1,vi.levels[i]],raab$vi.denom[raab$wgq.dis.any==1],raab$clusterId[raab$wgq.dis.any==1])
  wgq.vi.table$any.non.vi.dis.adj.pct.lci[i]<-bennett.lci(wgq.vi.table$any.non.vi.dis.adj.pct[i],raab[raab$wgq.dis.nonvi==1,vi.levels[i]],raab$vi.denom[raab$wgq.dis.nonvi==1],raab$clusterId[raab$wgq.dis.nonvi==1])
  wgq.vi.table$any.non.vi.dis.adj.pct.uci[i]<-bennett.uci(wgq.vi.table$any.non.vi.dis.adj.pct[i],raab[raab$wgq.dis.nonvi==1,vi.levels[i]],raab$vi.denom[raab$wgq.dis.nonvi==1],raab$clusterId[raab$wgq.dis.nonvi==1])
  wgq.vi.table$no.dis.adj.pct.lci[i]<-bennett.lci(wgq.vi.table$no.dis.adj.pct[i],raab[raab$wgq.dis.any==0,vi.levels[i]],raab$vi.denom[raab$wgq.dis.any==0],raab$clusterId[raab$wgq.dis.any==0])
  wgq.vi.table$no.dis.adj.pct.uci[i]<-bennett.uci(wgq.vi.table$no.dis.adj.pct[i],raab[raab$wgq.dis.any==0,vi.levels[i]],raab$vi.denom[raab$wgq.dis.any==0],raab$clusterId[raab$wgq.dis.any==0])
  
}  

wgq.vi.table[nrow(wgq.vi.table)+1,]<-NA
wgq.vi.table$vi.level[nrow(wgq.vi.table)]<-"moderate.severe.vi"

wgq.vi.table$any.dis.n[wgq.vi.table$vi.level=="moderate.severe.vi"]<-sum(raab$msvi[raab$wgq.dis.any==1])
wgq.vi.table$any.non.vi.dis.n[wgq.vi.table$vi.level=="moderate.severe.vi"]<-sum(raab$msvi[raab$wgq.dis.nonvi==1])
wgq.vi.table$no.dis.n[wgq.vi.table$vi.level=="moderate.severe.vi"]<-sum(raab$msvi[raab$wgq.dis.any==0])

wgq.vi.table$any.dis.pct[wgq.vi.table$vi.level=="moderate.severe.vi"]<-sum(raab$msvi[raab$wgq.dis.any==1])/sum(raab$wgq.dis.any==1)
wgq.vi.table$any.non.vi.dis.pct[wgq.vi.table$vi.level=="moderate.severe.vi"]<-sum(raab$msvi[raab$wgq.dis.nonvi==1])/sum(raab$wgq.dis.nonvi==1)
wgq.vi.table$no.dis.pct[wgq.vi.table$vi.level=="moderate.severe.vi"]<-sum(raab$msvi[raab$wgq.dis.any==0])/sum(raab$wgq.dis.any==0)

wgq.vi.table$any.dis.pct.lci[wgq.vi.table$vi.level=="moderate.severe.vi"]<-bennett.lci(wgq.vi.table$any.dis.pct[wgq.vi.table$vi.level=="moderate.severe.vi"],raab$msvi[raab$wgq.dis.any==1],raab$vi.denom[raab$wgq.dis.any==1],raab$clusterId[raab$wgq.dis.any==1])
wgq.vi.table$any.dis.pct.uci[wgq.vi.table$vi.level=="moderate.severe.vi"]<-bennett.uci(wgq.vi.table$any.dis.pct[wgq.vi.table$vi.level=="moderate.severe.vi"],raab$msvi[raab$wgq.dis.any==1],raab$vi.denom[raab$wgq.dis.any==1],raab$clusterId[raab$wgq.dis.any==1])
wgq.vi.table$any.non.vi.dis.pct.lci[wgq.vi.table$vi.level=="moderate.severe.vi"]<-bennett.lci(wgq.vi.table$any.non.vi.dis.pct[wgq.vi.table$vi.level=="moderate.severe.vi"],raab$msvi[raab$wgq.dis.nonvi==1],raab$vi.denom[raab$wgq.dis.nonvi==1],raab$clusterId[raab$wgq.dis.nonvi==1])
wgq.vi.table$any.non.vi.dis.pct.uci[wgq.vi.table$vi.level=="moderate.severe.vi"]<-bennett.uci(wgq.vi.table$any.non.vi.dis.pct[wgq.vi.table$vi.level=="moderate.severe.vi"],raab$msvi[raab$wgq.dis.nonvi==1],raab$vi.denom[raab$wgq.dis.nonvi==1],raab$clusterId[raab$wgq.dis.nonvi==1])
wgq.vi.table$no.dis.pct.lci[wgq.vi.table$vi.level=="moderate.severe.vi"]<-bennett.lci(wgq.vi.table$no.dis.pct[wgq.vi.table$vi.level=="moderate.severe.vi"],raab$msvi[raab$wgq.dis.any==0],raab$vi.denom[raab$wgq.dis.any==0],raab$clusterId[raab$wgq.dis.any==0])
wgq.vi.table$no.dis.pct.uci[wgq.vi.table$vi.level=="moderate.severe.vi"]<-bennett.uci(wgq.vi.table$no.dis.pct[wgq.vi.table$vi.level=="moderate.severe.vi"],raab$msvi[raab$wgq.dis.any==0],raab$vi.denom[raab$wgq.dis.any==0],raab$clusterId[raab$wgq.dis.any==0])

wgq.vi.table$any.dis.adj.pct[wgq.vi.table$vi.level=="moderate.severe.vi"]<-prop.age.sex.adjust(popfives,raab,raab$msvi[raab$wgq.dis.any==1],raab$vi.denom[raab$wgq.dis.any==1])
wgq.vi.table$any.non.vi.dis.adj.pct[wgq.vi.table$vi.level=="moderate.severe.vi"]<-prop.age.sex.adjust(popfives,raab,raab$msvi[raab$wgq.dis.nonvi==1],raab$vi.denom[raab$wgq.dis.nonvi==1])
wgq.vi.table$no.dis.adj.pct[wgq.vi.table$vi.level=="moderate.severe.vi"]<-prop.age.sex.adjust(popfives,raab,raab$msvi[raab$wgq.dis.any==0],raab$vi.denom[raab$wgq.dis.any==0])

wgq.vi.table$any.dis.adj.pct.lci[wgq.vi.table$vi.level=="moderate.severe.vi"]<-bennett.lci(wgq.vi.table$any.dis.adj.pct[wgq.vi.table$vi.level=="moderate.severe.vi"],raab$msvi[raab$wgq.dis.any==1],raab$vi.denom[raab$wgq.dis.any==1],raab$clusterId[raab$wgq.dis.any==1])
wgq.vi.table$any.dis.adj.pct.uci[wgq.vi.table$vi.level=="moderate.severe.vi"]<-bennett.uci(wgq.vi.table$any.dis.adj.pct[wgq.vi.table$vi.level=="moderate.severe.vi"],raab$msvi[raab$wgq.dis.any==1],raab$vi.denom[raab$wgq.dis.any==1],raab$clusterId[raab$wgq.dis.any==1])
wgq.vi.table$any.non.vi.dis.adj.pct.lci[wgq.vi.table$vi.level=="moderate.severe.vi"]<-bennett.lci(wgq.vi.table$any.non.vi.dis.adj.pct[wgq.vi.table$vi.level=="moderate.severe.vi"],raab$msvi[raab$wgq.dis.nonvi==1],raab$vi.denom[raab$wgq.dis.nonvi==1],raab$clusterId[raab$wgq.dis.nonvi==1])
wgq.vi.table$any.non.vi.dis.adj.pct.uci[wgq.vi.table$vi.level=="moderate.severe.vi"]<-bennett.uci(wgq.vi.table$any.non.vi.dis.adj.pct[wgq.vi.table$vi.level=="moderate.severe.vi"],raab$msvi[raab$wgq.dis.nonvi==1],raab$vi.denom[raab$wgq.dis.nonvi==1],raab$clusterId[raab$wgq.dis.nonvi==1])
wgq.vi.table$no.dis.adj.pct.lci[wgq.vi.table$vi.level=="moderate.severe.vi"]<-bennett.lci(wgq.vi.table$no.dis.adj.pct[wgq.vi.table$vi.level=="moderate.severe.vi"],raab$msvi[raab$wgq.dis.any==0],raab$vi.denom[raab$wgq.dis.any==0],raab$clusterId[raab$wgq.dis.any==0])
wgq.vi.table$no.dis.adj.pct.uci[wgq.vi.table$vi.level=="moderate.severe.vi"]<-bennett.uci(wgq.vi.table$no.dis.adj.pct[wgq.vi.table$vi.level=="moderate.severe.vi"],raab$msvi[raab$wgq.dis.any==0],raab$vi.denom[raab$wgq.dis.any==0],raab$clusterId[raab$wgq.dis.any==0])

row.order<-c("blind","severe.vi","moderate.vi","moderate.severe.vi","mild.vi")
wgq.vi.table<-wgq.vi.table %>% arrange(match(wgq.vi.table$vi.level,row.order))

pcts<-grep("pct",names(wgq.vi.table))
wgq.vi.table[,pcts]<-round(wgq.vi.table[,pcts]*100,1)
wgq.vi.table[,pcts]<-format(wgq.vi.table[,pcts], nsmall=1)
