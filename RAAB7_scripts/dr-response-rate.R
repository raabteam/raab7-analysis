# DR module: Response rate
# v1 04.07.22 IM
# v2 22.08.22 IM - split original table in to two, includes new proportions for known and suspected diabetes

dr.response.a <- data.frame(dr.response.cascade)
dr.response.a[,2:7] <- NA
names(dr.response.a) <- c("Exam.Status",
                        
                        "female.n",
                        "female.pct",
                        
                        "male.n",
                        "male.pct",
                        
                        "total.n",
                        "total.pct"
)

dr.response.a$female.n[dr.response.a$Exam.Status=="Enrolled"] <- sum(raab$gender=="female", na.rm=T)
dr.response.a$female.pct[dr.response.a$Exam.Status=="Enrolled"] <- 100
dr.response.a$male.n[dr.response.a$Exam.Status=="Enrolled"] <- sum(raab$gender=="male", na.rm=T)
dr.response.a$male.pct[dr.response.a$Exam.Status=="Enrolled"] <- 100
dr.response.a$total.n[dr.response.a$Exam.Status=="Enrolled"] <- sum((raab$gender=="female" | raab$gender=="male"),na.rm=T)
dr.response.a$total.pct[dr.response.a$Exam.Status=="Enrolled"] <- 100

dr.response.a$female.n[dr.response.a$Exam.Status=="Examined"] <- sum(raab$exam_status[raab$gender=='female']=="exam_status_examined", na.rm=T) 
dr.response.a$female.pct[dr.response.a$Exam.Status=="Examined"] <- sum(raab$exam_status[raab$gender=='female']=="exam_status_examined",na.rm=T) / sum(raab$gender=="female",na.rm=T) *100
dr.response.a$male.n[dr.response.a$Exam.Status=="Examined"] <- sum(raab$exam_status[raab$gender=='male']=="exam_status_examined",na.rm=T) 
dr.response.a$male.pct[dr.response.a$Exam.Status=="Examined"] <- sum(raab$exam_status[raab$gender=='male']=="exam_status_examined",na.rm=T) / sum(raab$gender=="male",na.rm=T) *100
dr.response.a$total.n[dr.response.a$Exam.Status=="Examined"] <- sum(raab$exam_status=="exam_status_examined",na.rm=T)  
dr.response.a$total.pct[dr.response.a$Exam.Status=="Examined"] <- sum(raab$exam_status=="exam_status_examined",na.rm=T) / sum((raab$gender=="female" | raab$gender=="male"),na.rm=T) *100

# dr.response.a$female.n[dr.response.a$Exam.Status=="Consented blood test"] <- sum(raab$dr_diabetes_blood_consent[raab$gender=='female']=="true", na.rm=T) 
# dr.response.a$female.pct[dr.response.a$Exam.Status=="Consented blood test"] <- sum(raab$dr_diabetes_blood_consent[raab$gender=='female']=="true",na.rm=T) / sum(raab$exam_status[raab$gender=='female']=="exam_status_examined",na.rm=T) *100
# dr.response.a$male.n[dr.response.a$Exam.Status=="Consented blood test"] <- sum(raab$dr_diabetes_blood_consent[raab$gender=='male']=="true",na.rm=T) 
# dr.response.a$male.pct[dr.response.a$Exam.Status=="Consented blood test"] <- sum(raab$dr_diabetes_blood_consent[raab$gender=='male']=="true",na.rm=T) / sum(raab$exam_status[raab$gender=='male']=="exam_status_examined",na.rm=T) *100
# dr.response.a$total.n[dr.response.a$Exam.Status=="Consented blood test"] <- sum(raab$dr_diabetes_blood_consent=="true",na.rm=T)  
# dr.response.a$total.pct[dr.response.a$Exam.Status=="Consented blood test"] <- sum(raab$dr_diabetes_blood_consent=="true",na.rm=T) / sum(raab$exam_status=="exam_status_examined",na.rm=T) *100

dr.response.a$female.n[dr.response.a$Exam.Status=="Diabetes status assessed"] <- sum(raab$diabetes.denom[raab$gender=='female']==1, na.rm=T)
dr.response.a$female.pct[dr.response.a$Exam.Status=="Diabetes status assessed"] <- sum(raab$diabetes.denom[raab$gender=='female']==1,na.rm=T) / sum(raab$gender=="female", na.rm=T) *100
dr.response.a$male.n[dr.response.a$Exam.Status=="Diabetes status assessed"] <- sum(raab$diabetes.denom[raab$gender=='male']==1,na.rm=T)
dr.response.a$male.pct[dr.response.a$Exam.Status=="Diabetes status assessed"] <- sum(raab$diabetes.denom[raab$gender=='male']==1,na.rm=T) / sum(raab$gender=="male", na.rm=T) *100
dr.response.a$total.n[dr.response.a$Exam.Status=="Diabetes status assessed"] <- sum(raab$diabetes.denom==1,na.rm=T)
dr.response.a$total.pct[dr.response.a$Exam.Status=="Diabetes status assessed"] <- sum(raab$diabetes.denom==1, na.rm=T) / sum((raab$gender=="female" | raab$gender=="male"),na.rm=T) *100

# dr.response.a$female.n[dr.response.a$Exam.Status=="Known or suspected diabetes"] <- sum(raab$diabetes.known.susp[raab$gender=='female']==1, na.rm=T) 
# dr.response.a$female.pct[dr.response.a$Exam.Status=="Known or suspected diabetes"] <- sum(raab$diabetes.known.susp[raab$gender=='female']==1,na.rm=T) / sum(raab$diabetes.denom[raab$gender=='female']==1,na.rm=T) *100
# dr.response.a$male.n[dr.response.a$Exam.Status=="Known or suspected diabetes"] <- sum(raab$diabetes.known.susp[raab$gender=='male']==1,na.rm=T) 
# dr.response.a$male.pct[dr.response.a$Exam.Status=="Known or suspected diabetes"] <- sum(raab$diabetes.known.susp[raab$gender=='male']==1,na.rm=T) / sum(raab$diabetes.denom[raab$gender=='male']==1,na.rm=T) *100
# dr.response.a$total.n[dr.response.a$Exam.Status=="Known or suspected diabetes"] <- sum(raab$diabetes.known.susp==1,na.rm=T)  
# dr.response.a$total.pct[dr.response.a$Exam.Status=="Known or suspected diabetes"] <- sum(raab$diabetes.known.susp==1,na.rm=T) / sum(raab$diabetes.denom==1,na.rm=T) *100
# 
# dr.response.a$female.n[dr.response.a$Exam.Status=="Consented dilated examination"] <- sum(raab$dr_retinopathy_method_right[raab$gender=='female']=="dr_retinopathy_method_dilatation_fundoscopy" | raab$dr_retinopathy_method_right[raab$gender=='female']=="dr_retinopathy_method_fundus_camera", na.rm=T) 
# dr.response.a$female.pct[dr.response.a$Exam.Status=="Consented dilated examination"] <- sum(raab$dr_retinopathy_method_right[raab$gender=='female']=="dr_retinopathy_method_dilatation_fundoscopy" | raab$dr_retinopathy_method_right[raab$gender=='female']=="dr_retinopathy_method_fundus_camera",na.rm=T) / sum(raab$diabetes.known.susp[raab$gender=='female']==1,na.rm=T) *100
# dr.response.a$male.n[dr.response.a$Exam.Status=="Consented dilated examination"] <- sum(raab$dr_retinopathy_method_right[raab$gender=='male']=="dr_retinopathy_method_dilatation_fundoscopy" | raab$dr_retinopathy_method_right[raab$gender=='male']=="dr_retinopathy_method_fundus_camera",na.rm=T) 
# dr.response.a$male.pct[dr.response.a$Exam.Status=="Consented dilated examination"] <- sum(raab$dr_retinopathy_method_right[raab$gender=='male']=="dr_retinopathy_method_dilatation_fundoscopy" | raab$dr_retinopathy_method_right[raab$gender=='male']=="dr_retinopathy_method_fundus_camera",na.rm=T) / sum(raab$diabetes.known.susp[raab$gender=='male']==1,na.rm=T) *100
# dr.response.a$total.n[dr.response.a$Exam.Status=="Consented dilated examination"] <- sum(raab$dr_retinopathy_method_right=="dr_retinopathy_method_dilatation_fundoscopy" | raab$dr_retinopathy_method_right=="dr_retinopathy_method_fundus_camera",na.rm=T)  
# dr.response.a$total.pct[dr.response.a$Exam.Status=="Consented dilated examination"] <- sum(raab$dr_retinopathy_method_right=="dr_retinopathy_method_dilatation_fundoscopy" | raab$dr_retinopathy_method_right=="dr_retinopathy_method_fundus_camera",na.rm=T) / sum(raab$diabetes.known.susp==1,na.rm=T) *100

# Round pct values in table
pcts<-grep("pct",names(dr.response.a))
dr.response.a[,pcts]<-round(dr.response.a[,pcts], 1)

write.csv(dr.response.a, here('outputs', 'dr.response.a.csv'), row.names = FALSE)

## Splitting response table in two and adding row for known vs suspected

dr.response.cascade.b <- (c("Known or suspected diabetes", "Known", "Suspected", "Consented dilated examination"))

dr.response.b <- data.frame(dr.response.cascade.b)
dr.response.b[,2:7] <- NA
names(dr.response.b) <- c("Exam.Status",
                        
                        "female.n",
                        "female.pct",
                        
                        "male.n",
                        "male.pct",
                        
                        "total.n",
                        "total.pct"
)

dr.response.b$female.n[dr.response.b$Exam.Status=="Known or suspected diabetes"] <- sum(raab$diabetes.known.susp[raab$gender=='female']==1, na.rm=T) 
dr.response.b$female.pct[dr.response.b$Exam.Status=="Known or suspected diabetes"] <- sum(raab$diabetes.known.susp[raab$gender=='female']==1,na.rm=T) / sum(raab$diabetes.denom[raab$gender=='female']==1,na.rm=T) *100
dr.response.b$male.n[dr.response.b$Exam.Status=="Known or suspected diabetes"] <- sum(raab$diabetes.known.susp[raab$gender=='male']==1,na.rm=T) 
dr.response.b$male.pct[dr.response.b$Exam.Status=="Known or suspected diabetes"] <- sum(raab$diabetes.known.susp[raab$gender=='male']==1,na.rm=T) / sum(raab$diabetes.denom[raab$gender=='male']==1,na.rm=T) *100
dr.response.b$total.n[dr.response.b$Exam.Status=="Known or suspected diabetes"] <- sum(raab$diabetes.known.susp==1,na.rm=T)  
dr.response.b$total.pct[dr.response.b$Exam.Status=="Known or suspected diabetes"] <- sum(raab$diabetes.known.susp==1,na.rm=T) / sum(raab$diabetes.denom==1,na.rm=T) *100

dr.response.b$female.n[dr.response.b$Exam.Status=="Known"]   <- sum(raab$diabetes.known[raab$gender=='female']==1, na.rm=T) 
dr.response.b$female.pct[dr.response.b$Exam.Status=="Known"] <- sum(raab$diabetes.known[raab$gender=='female']==1, na.rm=T) / sum(raab$diabetes.known.susp[raab$gender=='female']==1,na.rm=T) *100
dr.response.b$male.n[dr.response.b$Exam.Status=="Known"]     <- sum(raab$diabetes.known[raab$gender=='male']==1, na.rm=T)
dr.response.b$male.pct[dr.response.b$Exam.Status=="Known"]   <- sum(raab$diabetes.known[raab$gender=='male']==1, na.rm=T) / sum(raab$diabetes.known.susp[raab$gender=='male']==1,na.rm=T) *100
dr.response.b$total.n[dr.response.b$Exam.Status=="Known"]    <- sum(raab$diabetes.known==1,na.rm=T)
dr.response.b$total.pct[dr.response.b$Exam.Status=="Known"]  <- sum(raab$diabetes.known==1,na.rm=T) / sum(raab$diabetes.known.susp==1,na.rm=T) *100

dr.response.b$female.n[dr.response.b$Exam.Status=="Suspected"] <- sum(raab$diabetes.new[raab$gender=='female']==1, na.rm=T) 
dr.response.b$female.pct[dr.response.b$Exam.Status=="Suspected"]<- sum(raab$diabetes.new[raab$gender=='female']==1, na.rm=T) / sum(raab$diabetes.known.susp[raab$gender=='female']==1,na.rm=T) *100
dr.response.b$male.n[dr.response.b$Exam.Status=="Suspected"] <- sum(raab$diabetes.new[raab$gender=='male']==1, na.rm=T)
dr.response.b$male.pct[dr.response.b$Exam.Status=="Suspected"] <- sum(raab$diabetes.new[raab$gender=='male']==1, na.rm=T) / sum(raab$diabetes.known.susp[raab$gender=='male']==1,na.rm=T) *100
dr.response.b$total.n[dr.response.b$Exam.Status=="Suspected"] <- sum(raab$diabetes.new==1,na.rm=T)
dr.response.b$total.pct[dr.response.b$Exam.Status=="Suspected"]  <- sum(raab$diabetes.new==1,na.rm=T) / sum(raab$diabetes.known.susp==1,na.rm=T) *100

dr.response.b$female.n[dr.response.b$Exam.Status=="Consented dilated examination"] <- sum(raab$dr_retinopathy_method_right[raab$gender=='female']=="dr_retinopathy_method_dilatation_fundoscopy" | raab$dr_retinopathy_method_right[raab$gender=='female']=="dr_retinopathy_method_fundus_camera", na.rm=T) 
dr.response.b$female.pct[dr.response.b$Exam.Status=="Consented dilated examination"] <- sum(raab$dr_retinopathy_method_right[raab$gender=='female']=="dr_retinopathy_method_dilatation_fundoscopy" | raab$dr_retinopathy_method_right[raab$gender=='female']=="dr_retinopathy_method_fundus_camera",na.rm=T) / sum(raab$diabetes.known.susp[raab$gender=='female']==1,na.rm=T) *100
dr.response.b$male.n[dr.response.b$Exam.Status=="Consented dilated examination"] <- sum(raab$dr_retinopathy_method_right[raab$gender=='male']=="dr_retinopathy_method_dilatation_fundoscopy" | raab$dr_retinopathy_method_right[raab$gender=='male']=="dr_retinopathy_method_fundus_camera",na.rm=T) 
dr.response.b$male.pct[dr.response.b$Exam.Status=="Consented dilated examination"] <- sum(raab$dr_retinopathy_method_right[raab$gender=='male']=="dr_retinopathy_method_dilatation_fundoscopy" | raab$dr_retinopathy_method_right[raab$gender=='male']=="dr_retinopathy_method_fundus_camera",na.rm=T) / sum(raab$diabetes.known.susp[raab$gender=='male']==1,na.rm=T) *100
dr.response.b$total.n[dr.response.b$Exam.Status=="Consented dilated examination"] <- sum(raab$dr_retinopathy_method_right=="dr_retinopathy_method_dilatation_fundoscopy" | raab$dr_retinopathy_method_right=="dr_retinopathy_method_fundus_camera",na.rm=T)  
dr.response.b$total.pct[dr.response.b$Exam.Status=="Consented dilated examination"] <- sum(raab$dr_retinopathy_method_right=="dr_retinopathy_method_dilatation_fundoscopy" | raab$dr_retinopathy_method_right=="dr_retinopathy_method_fundus_camera",na.rm=T) / sum(raab$diabetes.known.susp==1,na.rm=T) *100

# Round pct values in table
pcts<-grep("pct",names(dr.response.b))
dr.response.b[,pcts]<-round(dr.response.b[,pcts], 1)

write.csv(dr.response.b, here('outputs', 'dr.response.b.csv'), row.names = FALSE)

  