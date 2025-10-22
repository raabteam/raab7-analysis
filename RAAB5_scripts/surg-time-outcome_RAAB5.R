# Cataract surgical outcomes in sample by time since surgery
# 20251022 IM updated RAAB6/7 code for RAAB5


cat.oc.time<-data.frame(oc.tab$oc.levels)
cat.oc.time[,2:11] <- NA
names(cat.oc.time) <- c("oc.levels",
                     
                     "less2.n.eyes",
                     "less2.pct.eyes",
                     
                     "more2.n.eyes",
                     "more2.pct.eyes",
                     
                     "more4.n.eyes",
                     "more4.pct.eyes",
                     
                     "more6.n.eyes",
                     "more6.pct.eyes",
                     
                     "more8.n.eyes",
                     "more8.pct.eyes"
)

for (i in 1:length(oc.tab$oc.levels)) 
  
{
  
  cat.oc.time$less2.n.eyes[i] <- sum(raab$right.operated.eyes.denom[raab$right.oc.levels==oc.tab$right.oc.levels[i] & raab$time.cat.op.re.0==1],na.rm=T) + sum(raab$left.operated.eyes.denom[raab$left.oc.levels==oc.tab$left.oc.levels[i] & raab$time.cat.op.le.0==1],na.rm=T)
  cat.oc.time$less2.pct.eyes[i] <- (cat.oc.time$less2.n.eyes[i] / (sum(raab$right.operated.eyes.denom[raab$time.cat.op.re.0==1],na.rm=T) + sum(raab$left.operated.eyes.denom[raab$time.cat.op.le.0==1],na.rm=T)))

  cat.oc.time$more2.n.eyes[i] <- sum(raab$right.operated.eyes.denom[raab$right.oc.levels==oc.tab$right.oc.levels[i] & raab$time.cat.op.re.2==1],na.rm=T) + sum(raab$left.operated.eyes.denom[raab$left.oc.levels==oc.tab$left.oc.levels[i] & raab$time.cat.op.le.2==1],na.rm=T)
  cat.oc.time$more2.pct.eyes[i] <- (cat.oc.time$more2.n.eyes[i] / (sum(raab$right.operated.eyes.denom[raab$time.cat.op.re.2==1],na.rm=T) + sum(raab$left.operated.eyes.denom[raab$time.cat.op.le.2==1],na.rm=T)))

  cat.oc.time$more4.n.eyes[i] <- sum(raab$right.operated.eyes.denom[raab$right.oc.levels==oc.tab$right.oc.levels[i] & raab$time.cat.op.re.4==1],na.rm=T) + sum(raab$left.operated.eyes.denom[raab$left.oc.levels==oc.tab$left.oc.levels[i] & raab$time.cat.op.le.4==1],na.rm=T)
  cat.oc.time$more4.pct.eyes[i] <- (cat.oc.time$more4.n.eyes[i] / (sum(raab$right.operated.eyes.denom[raab$time.cat.op.re.4==1],na.rm=T) + sum(raab$left.operated.eyes.denom[raab$time.cat.op.le.4==1],na.rm=T)))

  cat.oc.time$more6.n.eyes[i] <- sum(raab$right.operated.eyes.denom[raab$right.oc.levels==oc.tab$right.oc.levels[i] & raab$time.cat.op.re.6==1],na.rm=T) + sum(raab$left.operated.eyes.denom[raab$left.oc.levels==oc.tab$left.oc.levels[i] & raab$time.cat.op.le.6==1],na.rm=T)
  cat.oc.time$more6.pct.eyes[i] <- (cat.oc.time$more6.n.eyes[i] / (sum(raab$right.operated.eyes.denom[raab$time.cat.op.re.6==1],na.rm=T) + sum(raab$left.operated.eyes.denom[raab$time.cat.op.le.6==1],na.rm=T)))

  cat.oc.time$more8.n.eyes[i] <- sum(raab$right.operated.eyes.denom[raab$right.oc.levels==oc.tab$right.oc.levels[i] & raab$time.cat.op.re.8==1],na.rm=T) + sum(raab$left.operated.eyes.denom[raab$left.oc.levels==oc.tab$left.oc.levels[i] & raab$time.cat.op.le.8==1],na.rm=T)
  cat.oc.time$more8.pct.eyes[i] <- (cat.oc.time$more8.n.eyes[i] / (sum(raab$right.operated.eyes.denom[raab$time.cat.op.re.8==1],na.rm=T) + sum(raab$left.operated.eyes.denom[raab$time.cat.op.le.8==1],na.rm=T)))
  
}

cat.oc.time[nrow(cat.oc.time)+1,c(2,4,6,8,10)]<-colSums(cat.oc.time[,c(2,4,6,8,10)])
cat.oc.time[nrow(cat.oc.time),c(3,5,7,9,11)]<-1
cat.oc.time$oc.levels[nrow(cat.oc.time)]<-"Total"

nt3a.pcts<-grep("pct",names(cat.oc.time))
cat.oc.time[,nt3a.pcts]<-round(cat.oc.time[,nt3a.pcts] * 100,1)
cat.oc.time[,nt3a.pcts]<-format(cat.oc.time[,nt3a.pcts], nsmall=1)

