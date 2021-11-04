#RAAB6

#v3 20th July 2021 IM
#v4 12th Aug 2021 RB

#Priority table 1
#SUM 1

# https://stats.idre.ucla.edu/r/modules/subsetting-data/

Exam.Status <- c("exam_status_examined","exam_status_incapable","exam_status_refused","exam_status_unavailable") 

sum1<-data.frame(Exam.Status)
sum1[,2:7] <- NA
names(sum1) <- c("Exam.Status",
                             
                  "female.n",
                  "female.pct",
                             
                  "male.n",
                  "male.pct",
                             
                  "total.n",
                  "total.pct"
                )

for (i in 1:length(Exam.Status)) {
  
  sum1$female.n[i] <- sum(raab$exam_status[raab$gender=='female']==Exam.Status[i],na.rm=T) 
  sum1$female.pct[i] <- sum(raab$exam_status[raab$gender=='female']==Exam.Status[i],na.rm=T) / sum(raab$exam_status[raab$gender=='female']!=is.na(Exam.Status[i]))
   
  sum1$male.n[i] <- sum(raab$exam_status[raab$gender=='male']==Exam.Status[i],na.rm=T) 
  sum1$male.pct[i] <- sum(raab$exam_status[raab$gender=='male']==Exam.Status[i],na.rm=T) / sum(raab$exam_status[raab$gender=='male']!=is.na(Exam.Status[i]),na.rm=T)
  
  sum1$total.n[i] <- sum(raab$exam_status==Exam.Status[i],na.rm=T)  
  sum1$total.pct[i] <- sum(raab$exam_status==Exam.Status[i],na.rm=T) / sum(raab$exam_status!=is.na(Exam.Status[i]),na.rm=T)
   
}
    

#Add totals row to bottom of table (for total count of female, male, all)

sum1[nrow(sum1)+1,2:7]<-colSums(sum1[,2:7])
sum1$Exam.Status<-as.character(c('Examined*', 'Refused', 'Incapable', 'Unavailable','Total'))

pcts<-grep("pct",names(sum1))
sum1[,pcts]<-round( sum1[,pcts] * 100, 1 )

cnts<-grep("n",names(sum1))
sum1[,cnts]<-format( sum1[,cnts], digits=0, big.interval = 3L, big.mark = " ", scientific=F )
