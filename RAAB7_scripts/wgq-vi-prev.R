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

  wgq.vi.table$any.dis.pct[i]<-sum(raab[raab$wgq.dis.any==1,vi.levels[i]])/sum(raab$wgq.dis.any==1)*100
  wgq.vi.table$any.non.vi.dis.pct[i]<-sum(raab[raab$wgq.dis.nonvi==1,vi.levels[i]])/sum(raab$wgq.dis.nonvi==1)*100
  wgq.vi.table$no.dis.pct[i]<-sum(raab[raab$wgq.dis.any==0,vi.levels[i]])/sum(raab$wgq.dis.any==0)*100
  
  wgq.vi.table$any.dis.pct.lci[i]<-bennett.lci(wgq.vi.table$any.dis.pct[i],raab[raab$wgq.dis.any==1,vi.levels[i]],raab$vi.denom[raab$wgq.dis.any==1],raab$clusterId[raab$wgq.dis.any==1])
  wgq.vi.table$any.non.vi.dis.pct.lci[i]<-bennett.lci(wgq.vi.table$any.non.vi.dis.pct[i],raab[raab$wgq.dis.nonvi==1,vi.levels[i]],raab$vi.denom[raab$wgq.dis.nonvi==1],raab$clusterId[raab$wgq.dis.nonvi==1])
  wgq.vi.table$no.dis.pct.lci[i]<-bennett.lci(wgq.vi.table$no.dis.pct[i],raab[raab$wgq.dis.any==0,vi.levels[i]],raab$vi.denom[raab$wgq.dis.any==0],raab$clusterId[raab$wgq.dis.any==0])
  
  wgq.vi.table$any.dis.pct.uci[i]<-bennett.uci(wgq.vi.table$any.dis.pct[i],raab[raab$wgq.dis.any==1,vi.levels[i]],raab$vi.denom[raab$wgq.dis.any==1],raab$clusterId[raab$wgq.dis.any==1])
  wgq.vi.table$any.non.vi.dis.pct.uci[i]<-bennett.uci(wgq.vi.table$any.non.vi.dis.pct[i],raab[raab$wgq.dis.nonvi==1,vi.levels[i]],raab$vi.denom[raab$wgq.dis.nonvi==1],raab$clusterId[raab$wgq.dis.nonvi==1])
  wgq.vi.table$no.dis.pct.uci[i]<-bennett.uci(wgq.vi.table$no.dis.pct[i],raab[raab$wgq.dis.any==0,vi.levels[i]],raab$vi.denom[raab$wgq.dis.any==0],raab$clusterId[raab$wgq.dis.any==0])
  
  # No ASA included here
}  
