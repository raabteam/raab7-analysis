#v1 24/08/21 - IM
#v2 02/09/21 - RB

specs <- c("near","distance")

newtab6<-data.frame(specs)
newtab6[,2:7] <- NA
names(newtab6) <- c("specs",
                             
                     "female.n",
                     "female.pct",
                             
                     "male.n",
                     "male.pct",
                             
                     "total.n",
                     "total.pct")

newtab6$female.n[newtab6$specs=="near"] <- sum(raab$spectacles_used_near[raab$gender=='female']=="true", na.rm = T)
newtab6$male.n[newtab6$specs=="near"] <- sum(raab$spectacles_used_near[raab$gender=='male']=="true", na.rm = T)
newtab6$total.n[newtab6$specs=="near"] <- sum(raab$spectacles_used_near=="true", na.rm = T)

newtab6$female.pct[newtab6$specs=="near"] <- round( sum(raab$spectacles_used_near[raab$gender=='female']=="true", na.rm = TRUE) / sum(raab$exam_status[raab$gender=="female"]=="exam_status_examined") * 100,1)
newtab6$male.pct[newtab6$specs=="near"] <- round( sum(raab$spectacles_used_near[raab$gender=='male']=="true", na.rm = TRUE) / sum(raab$exam_status[raab$gender=="male"]=="exam_status_examined") * 100,1)
newtab6$total.pct[newtab6$specs=="near"] <- round( sum(raab$spectacles_used_near=="true", na.rm = TRUE) / sum(raab$exam_status=="exam_status_examined") * 100,1)

newtab6$female.n[newtab6$specs=="distance"] <- sum(raab$spectacles_used_distance[raab$gender=='female']=="true", na.rm = T)
newtab6$male.n[newtab6$specs=="distance"] <- sum(raab$spectacles_used_distance[raab$gender=='male']=="true", na.rm = T)
newtab6$total.n[newtab6$specs=="distance"] <- sum(raab$spectacles_used_distance=="true", na.rm = T)

newtab6$female.pct[newtab6$specs=="distance"] <- round( sum(raab$spectacles_used_distance[raab$gender=='female']=="true", na.rm = TRUE) / sum(raab$exam_status[raab$gender=="female"]=="exam_status_examined") * 100,1)
newtab6$male.pct[newtab6$specs=="distance"] <- round( sum(raab$spectacles_used_distance[raab$gender=='male']=="true", na.rm = TRUE) / sum(raab$exam_status[raab$gender=="male"]=="exam_status_examined") * 100,1)
newtab6$total.pct[newtab6$specs=="distance"] <- round( sum(raab$spectacles_used_distance=="true", na.rm = TRUE) / sum(raab$exam_status=="exam_status_examined") * 100,1)


tots<-colSums(newtab6[,2:7])
newtab6[3,]<-NA
newtab6[3,1]<-"any"
newtab6[3,2:7]<-tots
