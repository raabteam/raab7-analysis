# DR module: Response rate
# 04.07.22 IM

dr.response <- data.frame(dr.response.cascade)
dr.response[,2:7] <- NA
names(dr.response) <- c("Exam.Status",
                        
                        "female.n",
                        "female.pct",
                        
                        "male.n",
                        "male.pct",
                        
                        "total.n",
                        "total.pct"
)

dr.response$female.n[dr.response$Exam.Status=="Enrolled"] <- sum(raab$gender=="female", na.rm=T)
dr.response$female.pct[dr.response$Exam.Status=="Enrolled"] <- 100
dr.response$male.n[dr.response$Exam.Status=="Enrolled"] <- sum(raab$gender=="male", na.rm=T)
dr.response$male.pct[dr.response$Exam.Status=="Enrolled"] <- 100
dr.response$total.n[dr.response$Exam.Status=="Enrolled"] <- sum((raab$gender=="female" | raab$gender=="male"),na.rm=T)
dr.response$total.pct[dr.response$Exam.Status=="Enrolled"] <- 100

dr.response$female.n[dr.response$Exam.Status=="Examined"] <- sum(raab$exam_status[raab$gender=='female']=="exam_status_examined", na.rm=T) 
dr.response$female.pct[dr.response$Exam.Status=="Examined"] <- sum(raab$exam_status[raab$gender=='female']=="exam_status_examined",na.rm=T) / sum(raab$gender=="female",na.rm=T) *100
dr.response$male.n[dr.response$Exam.Status=="Examined"] <- sum(raab$exam_status[raab$gender=='male']=="exam_status_examined",na.rm=T) 
dr.response$male.pct[dr.response$Exam.Status=="Examined"] <- sum(raab$exam_status[raab$gender=='male']=="exam_status_examined",na.rm=T) / sum(raab$gender=="male",na.rm=T) *100
dr.response$total.n[dr.response$Exam.Status=="Examined"] <- sum(raab$exam_status=="exam_status_examined",na.rm=T)  
dr.response$total.pct[dr.response$Exam.Status=="Examined"] <- sum(raab$exam_status=="exam_status_examined",na.rm=T) / sum((raab$gender=="female" | raab$gender=="male"),na.rm=T) *100

# dr.response$female.n[dr.response$Exam.Status=="Consented blood test"] <- sum(raab$dr_diabetes_blood_consent[raab$gender=='female']=="true", na.rm=T) 
# dr.response$female.pct[dr.response$Exam.Status=="Consented blood test"] <- sum(raab$dr_diabetes_blood_consent[raab$gender=='female']=="true",na.rm=T) / sum(raab$exam_status[raab$gender=='female']=="exam_status_examined",na.rm=T) *100
# dr.response$male.n[dr.response$Exam.Status=="Consented blood test"] <- sum(raab$dr_diabetes_blood_consent[raab$gender=='male']=="true",na.rm=T) 
# dr.response$male.pct[dr.response$Exam.Status=="Consented blood test"] <- sum(raab$dr_diabetes_blood_consent[raab$gender=='male']=="true",na.rm=T) / sum(raab$exam_status[raab$gender=='male']=="exam_status_examined",na.rm=T) *100
# dr.response$total.n[dr.response$Exam.Status=="Consented blood test"] <- sum(raab$dr_diabetes_blood_consent=="true",na.rm=T)  
# dr.response$total.pct[dr.response$Exam.Status=="Consented blood test"] <- sum(raab$dr_diabetes_blood_consent=="true",na.rm=T) / sum(raab$exam_status=="exam_status_examined",na.rm=T) *100

dr.response$female.n[dr.response$Exam.Status=="Self-reported diabetes or consented to blood test"] <- sum(raab$diabetes.denom[raab$gender=='female']==1, na.rm=T)
dr.response$female.pct[dr.response$Exam.Status=="Self-reported diabetes or consented to blood test"] <- sum(raab$diabetes.denom[raab$gender=='female']==1,na.rm=T) / sum(raab$exam_status[raab$gender=='female']=="exam_status_examined",na.rm=T) *100
dr.response$male.n[dr.response$Exam.Status=="Self-reported diabetes or consented to blood test"] <- sum(raab$diabetes.denom[raab$gender=='male']==1,na.rm=T)
dr.response$male.pct[dr.response$Exam.Status=="Self-reported diabetes or consented to blood test"] <- sum(raab$diabetes.denom[raab$gender=='male']==1,na.rm=T) / sum(raab$exam_status[raab$gender=='male']=="exam_status_examined",na.rm=T) *100
dr.response$total.n[dr.response$Exam.Status=="Self-reported diabetes or consented to blood test"] <- sum(raab$diabetes.denom==1,na.rm=T)
dr.response$total.pct[dr.response$Exam.Status=="Self-reported diabetes or consented to blood test"] <- sum(raab$diabetes.denom==1, na.rm=T) / sum(raab$exam_status=="exam_status_examined",na.rm=T) *100

dr.response$female.n[dr.response$Exam.Status=="Known or suspected diabetes"] <- sum(raab$diabetes.known.susp[raab$gender=='female']==1, na.rm=T) 
dr.response$female.pct[dr.response$Exam.Status=="Known or suspected diabetes"] <- sum(raab$diabetes.known.susp[raab$gender=='female']==1,na.rm=T) / sum(raab$exam_status[raab$gender=='female']=="exam_status_examined",na.rm=T) *100
dr.response$male.n[dr.response$Exam.Status=="Known or suspected diabetes"] <- sum(raab$diabetes.known.susp[raab$gender=='male']==1,na.rm=T) 
dr.response$male.pct[dr.response$Exam.Status=="Known or suspected diabetes"] <- sum(raab$diabetes.known.susp[raab$gender=='male']==1,na.rm=T) / sum(raab$exam_status[raab$gender=='male']=="exam_status_examined",na.rm=T) *100
dr.response$total.n[dr.response$Exam.Status=="Known or suspected diabetes"] <- sum(raab$diabetes.known.susp==1,na.rm=T)  
dr.response$total.pct[dr.response$Exam.Status=="Known or suspected diabetes"] <- sum(raab$diabetes.known.susp==1,na.rm=T) / sum(raab$exam_status=="exam_status_examined",na.rm=T) *100

dr.response$female.n[dr.response$Exam.Status=="Consented dilated examination"] <- sum(raab$dr_retinopathy_method_right[raab$gender=='female']=="dr_retinopathy_method_dilatation_fundoscopy" | raab$dr_retinopathy_method_right[raab$gender=='female']=="dr_retinopathy_method_fundus_camera", na.rm=T) 
dr.response$female.pct[dr.response$Exam.Status=="Consented dilated examination"] <- sum(raab$dr_retinopathy_method_right[raab$gender=='female']=="dr_retinopathy_method_dilatation_fundoscopy" | raab$dr_retinopathy_method_right[raab$gender=='female']=="dr_retinopathy_method_fundus_camera",na.rm=T) / sum(raab$diabetes.known.susp[raab$gender=='female']==1,na.rm=T) *100
dr.response$male.n[dr.response$Exam.Status=="Consented dilated examination"] <- sum(raab$dr_retinopathy_method_right[raab$gender=='male']=="dr_retinopathy_method_dilatation_fundoscopy" | raab$dr_retinopathy_method_right[raab$gender=='male']=="dr_retinopathy_method_fundus_camera",na.rm=T) 
dr.response$male.pct[dr.response$Exam.Status=="Consented dilated examination"] <- sum(raab$dr_retinopathy_method_right[raab$gender=='male']=="dr_retinopathy_method_dilatation_fundoscopy" | raab$dr_retinopathy_method_right[raab$gender=='male']=="dr_retinopathy_method_fundus_camera",na.rm=T) / sum(raab$diabetes.known.susp[raab$gender=='male']==1,na.rm=T) *100
dr.response$total.n[dr.response$Exam.Status=="Consented dilated examination"] <- sum(raab$dr_retinopathy_method_right=="dr_retinopathy_method_dilatation_fundoscopy" | raab$dr_retinopathy_method_right=="dr_retinopathy_method_fundus_camera",na.rm=T)  
dr.response$total.pct[dr.response$Exam.Status=="Consented dilated examination"] <- sum(raab$dr_retinopathy_method_right=="dr_retinopathy_method_dilatation_fundoscopy" | raab$dr_retinopathy_method_right=="dr_retinopathy_method_fundus_camera",na.rm=T) / sum(raab$diabetes.known.susp==1,na.rm=T) *100

# Round pct values in table
pcts<-grep("pct",names(dr.response))
dr.response[,pcts]<-round(dr.response[,pcts], 1)

write.csv(dr.response, here('outputs', 'dr.response.csv'))
