# DR module: Last fundus examination for DR among self-reported/known diabetics
# 04.07.22 IM

dr.last.exam.table <- data.frame(dr.last.exam)
dr.last.exam.table[,2:7] <- NA
names(dr.last.exam.table) <- c("last.dr.exam",
                        
                        "female.n",
                        "female.pct",
                        
                        "male.n",
                        "male.pct",
                        
                        "total.n",
                        "total.pct"
)

for (i in 1:length(dr.last.exam)) {
  
  dr.last.exam.table$female.n[i] <- sum(raab$dr_diabetic_last_exam[raab$gender=="female"]==dr.last.exam[i],na.rm=T) 
  dr.last.exam.table$female.pct[i] <- sum(raab$dr_diabetic_last_exam[raab$gender=="female"]==dr.last.exam[i],na.rm=T) / sum(raab$dr_diabetes_known[raab$gender=="female"]==TRUE,na.rm=T) 
  
  dr.last.exam.table$male.n[i] <- sum(raab$dr_diabetic_last_exam[raab$gender=="male"]==dr.last.exam[i],na.rm=T)  
  dr.last.exam.table$male.pct[i] <- sum(raab$dr_diabetic_last_exam[raab$gender=="male"]==dr.last.exam[i],na.rm=T) / sum(raab$dr_diabetes_known[raab$gender=="male"]==TRUE,na.rm=T)
  
  dr.last.exam.table$total.n[i] <- sum(raab$dr_diabetic_last_exam==dr.last.exam[i],na.rm=T) 
  dr.last.exam.table$total.pct[i] <- sum(raab$dr_diabetic_last_exam==dr.last.exam[i],na.rm=T) / sum(raab$dr_diabetes_known==TRUE,na.rm=T)
  
}

dr.last.exam.table[nrow(dr.last.exam.table)+1,2:7]<-colSums(dr.last.exam.table[,2:7])
dr.last.exam.table$last.dr.exam[5]<-"Total"

pcts <- grep("pct",names(dr.last.exam.table))
dr.last.exam.table[,pcts] <- format( round(dr.last.exam.table[,pcts] * 100, 1), nsmall=1)