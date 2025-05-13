## near spectacles history

# question set for near spectacle owners (can be met need, undermet need or no need but with near specs anyway)

# first pair near spectacles
specs.first.pair.near <- c(TRUE, FALSE)
specs.first.pair.near.table <- data.frame(specs.first.pair.near)
specs.first.pair.near.table[,2:7] <- NA
names(specs.first.pair.near.table) <- c("first.pair.near",
                               
                               "female.n",
                               "female.pct",
                               
                               "male.n",
                               "male.pct",
                               
                               "total.n",
                               "total.pct"
)

for (i in 1:length(specs.first.pair.near)) {
  
  specs.first.pair.near.table$female.n[i] <- sum(raab$spectacles_first_pair_near[raab$gender=="female"]==specs.first.pair.near[i],na.rm=T) 
  specs.first.pair.near.table$female.pct[i] <- sum(raab$spectacles_first_pair_near[raab$gender=="female"]==specs.first.pair.near[i],na.rm=T) / sum(raab$spectacles_used_near[raab$gender=="female"]==TRUE,na.rm=T) 
  
  specs.first.pair.near.table$male.n[i] <- sum(raab$spectacles_first_pair_near[raab$gender=="male"]==specs.first.pair.near[i],na.rm=T)  
  specs.first.pair.near.table$male.pct[i] <- sum(raab$spectacles_first_pair_near[raab$gender=="male"]==specs.first.pair.near[i],na.rm=T) / sum(raab$spectacles_used_near[raab$gender=="male"]==TRUE,na.rm=T)
  
  specs.first.pair.near.table$total.n[i] <- sum(raab$spectacles_first_pair_near==specs.first.pair.near[i],na.rm=T) 
  specs.first.pair.near.table$total.pct[i] <- sum(raab$spectacles_first_pair_near==specs.first.pair.near[i],na.rm=T) / sum(raab$spectacles_used_near==TRUE,na.rm=T)
  
}

pcts <- grep("pct",names(specs.first.pair.near.table))
specs.first.pair.near.table[,pcts] <- format(round(specs.first.pair.near.table[,pcts] * 100, 1), nsmall=1)

# near spectacle age
specs.age.near <- c("spectacles_age_under_2", "spectacles_age_2_5", "spectacles_age_over_5")
specs.age.near.table<-data.frame(specs.age.near)
specs.age.near.table[,2:7] <- NA
names(specs.age.near.table) <- c("specs.age.near",
                    
                    "female.n",
                    "female.pct",
                    
                    "male.n",
                    "male.pct",
                    
                    "total.n",
                    "total.pct")

for (i in 1:length(specs.age.near)) 
  
{
  
  specs.age.near.table$female.n[i] <- sum(raab$spectacles_age_near[raab$gender=='female']==specs.age.near[i])
  specs.age.near.table$female.pct[i] <- (sum(raab$spectacles_age_near[raab$gender=='female']==specs.age.near[i]) / sum(raab$spectacles_used_near[raab$gender=='female']==TRUE))
  
  specs.age.near.table$male.n[i] <- sum(raab$spectacles_age_near[raab$gender=='male']==specs.age.near[i])
  specs.age.near.table$male.pct[i] <- (sum(raab$spectacles_age_near[raab$gender=='male']==specs.age.near[i]) / sum(raab$spectacles_used_near[raab$gender=='male']==TRUE))
  
  specs.age.near.table$total.n[i] <- sum(raab$spectacles_age_near==specs.age.near[i])
  specs.age.near.table$total.pct[i] <- (sum(raab$spectacles_age_near==specs.age.near[i]) / sum(raab$spectacles_used_near==TRUE))
  
}

pcts <- grep("pct",names(specs.age.near.table))
specs.age.near.table[,pcts] <- format(round(specs.age.near.table[,pcts] * 100, 1), nsmall=1)

# type of current near specs
specs.type.near <- c("spectacles_type_custom", "spectacles_type_readymade")
specs.type.near.table<-data.frame(specs.type.near)
specs.type.near.table[,2:7] <- NA
names(specs.type.near.table) <- c("specs.type.near",
                                 
                                 "female.n",
                                 "female.pct",
                                 
                                 "male.n",
                                 "male.pct",
                                 
                                 "total.n",
                                 "total.pct")

for (i in 1:length(specs.type.near)) 
  
{
  
  specs.type.near.table$female.n[i] <- sum(raab$spectacles_type_near[raab$gender=='female']==specs.type.near[i])
  specs.type.near.table$female.pct[i] <- (sum(raab$spectacles_type_near[raab$gender=='female']==specs.type.near[i]) / sum(raab$spectacles_used_near[raab$gender=='female']==TRUE))
  
  specs.type.near.table$male.n[i] <- sum(raab$spectacles_type_near[raab$gender=='male']==specs.type.near[i])
  specs.type.near.table$male.pct[i] <- (sum(raab$spectacles_type_near[raab$gender=='male']==specs.type.near[i]) / sum(raab$spectacles_used_near[raab$gender=='male']==TRUE))
  
  specs.type.near.table$total.n[i] <- sum(raab$spectacles_type_near==specs.type.near[i])
  specs.type.near.table$total.pct[i] <- (sum(raab$spectacles_type_near==specs.type.near[i]) / sum(raab$spectacles_used_near==TRUE))
  
}

pcts <- grep("pct",names(specs.type.near.table))
specs.type.near.table[,pcts] <- format(round(specs.type.near.table[,pcts] * 100, 1), nsmall=1)

# near specs (refraction) assessment
specs.assess.near <- c("spectacles_assessed_eye_health", "spectacles_assessed_other_health", "spectacles_assessed_self")
specs.assess.near.table<-data.frame(specs.assess.near)
specs.assess.near.table[,2:7] <- NA
names(specs.assess.near.table) <- c("specs.assess.near",
                                  
                                  "female.n",
                                  "female.pct",
                                  
                                  "male.n",
                                  "male.pct",
                                  
                                  "total.n",
                                  "total.pct")

for (i in 1:length(specs.assess.near)) 
  
{
  
  specs.assess.near.table$female.n[i] <- sum(raab$spectacles_assessed_near[raab$gender=='female']==specs.assess.near[i])
  specs.assess.near.table$female.pct[i] <- (sum(raab$spectacles_assessed_near[raab$gender=='female']==specs.assess.near[i]) / sum(raab$spectacles_used_near[raab$gender=='female']==TRUE))
  
  specs.assess.near.table$male.n[i] <- sum(raab$spectacles_assessed_near[raab$gender=='male']==specs.assess.near[i])
  specs.assess.near.table$male.pct[i] <- (sum(raab$spectacles_assessed_near[raab$gender=='male']==specs.assess.near[i]) / sum(raab$spectacles_used_near[raab$gender=='male']==TRUE))
  
  specs.assess.near.table$total.n[i] <- sum(raab$spectacles_assessed_near==specs.assess.near[i])
  specs.assess.near.table$total.pct[i] <- (sum(raab$spectacles_assessed_near==specs.assess.near[i]) / sum(raab$spectacles_used_near==TRUE))
  
}

pcts <- grep("pct",names(specs.assess.near.table))
specs.assess.near.table[,pcts] <- format(round(specs.assess.near.table[,pcts] * 100, 1), nsmall=1)

# were near specs free?
specs.free.near <- c(TRUE, FALSE)
specs.free.near.table<-data.frame(specs.free.near)
specs.free.near.table[,2:7] <- NA
names(specs.free.near.table) <- c("specs.free.near",
                                    
                                    "female.n",
                                    "female.pct",
                                    
                                    "male.n",
                                    "male.pct",
                                    
                                    "total.n",
                                    "total.pct")

for (i in 1:length(specs.free.near)) 
  
{
  
  specs.free.near.table$female.n[i] <- sum(raab$spectacles_free_near[raab$gender=='female']==specs.free.near[i])
  specs.free.near.table$female.pct[i] <- (sum(raab$spectacles_free_near[raab$gender=='female']==specs.free.near[i]) / sum(raab$spectacles_used_near[raab$gender=='female']==TRUE))
  
  specs.free.near.table$male.n[i] <- sum(raab$spectacles_free_near[raab$gender=='male']==specs.free.near[i])
  specs.free.near.table$male.pct[i] <- (sum(raab$spectacles_free_near[raab$gender=='male']==specs.free.near[i]) / sum(raab$spectacles_used_near[raab$gender=='male']==TRUE))
  
  specs.free.near.table$total.n[i] <- sum(raab$spectacles_free_near==specs.free.near[i])
  specs.free.near.table$total.pct[i] <- (sum(raab$spectacles_free_near==specs.free.near[i]) / sum(raab$spectacles_used_near==TRUE))
  
}

pcts <- grep("pct",names(specs.free.near.table))
specs.free.near.table[,pcts] <- format(round(specs.free.near.table[,pcts] * 100, 1), nsmall=1)

# would you replace near specs if lost/ broken?
specs.replace.near <-c(TRUE, FALSE)
specs.replace.near.table<-data.frame(specs.replace.near)
specs.replace.near.table[,2:7] <- NA
names(specs.replace.near.table) <- c("specs.replace.near",
                                  
                                  "female.n",
                                  "female.pct",
                                  
                                  "male.n",
                                  "male.pct",
                                  
                                  "total.n",
                                  "total.pct")

for (i in 1:length(specs.replace.near)) 
  
{
  
  specs.replace.near.table$female.n[i] <- sum(raab$spectacles_purchase_replacement_near[raab$gender=='female']==specs.replace.near[i])
  specs.replace.near.table$female.pct[i] <- (sum(raab$spectacles_purchase_replacement_near[raab$gender=='female']==specs.replace.near[i]) / sum(raab$spectacles_used_near[raab$gender=='female']==TRUE))
  
  specs.replace.near.table$male.n[i] <- sum(raab$spectacles_purchase_replacement_near[raab$gender=='male']==specs.replace.near[i])
  specs.replace.near.table$male.pct[i] <- (sum(raab$spectacles_purchase_replacement_near[raab$gender=='male']==specs.replace.near[i]) / sum(raab$spectacles_used_near[raab$gender=='male']==TRUE))
  
  specs.replace.near.table$total.n[i] <- sum(raab$spectacles_purchase_replacement_near==specs.replace.near[i])
  specs.replace.near.table$total.pct[i] <- (sum(raab$spectacles_purchase_replacement_near==specs.replace.near[i]) / sum(raab$spectacles_used_near==TRUE))
  
}

pcts <- grep("pct",names(specs.replace.near.table))
specs.replace.near.table[,pcts] <- format(round(specs.replace.near.table[,pcts] * 100, 1), nsmall=1)

# question set for unmet need for near specs (no specs & cannot see N6 but can see 6/12 pinhole distance)

# have you ever used near specs?
specs.ever.near <-c(TRUE, FALSE)
specs.ever.near.table<-data.frame(specs.ever.near)
specs.ever.near.table[,2:7] <- NA
names(specs.ever.near.table) <- c("specs.ever.near",
                                     
                                     "female.n",
                                     "female.pct",
                                     
                                     "male.n",
                                     "male.pct",
                                     
                                     "total.n",
                                     "total.pct")

for (i in 1:length(specs.ever.near)) 
  
{
  
  specs.ever.near.table$female.n[i] <- sum(raab$spectacles_used_ever_near[raab$gender=='female']==specs.ever.near[i])
  specs.ever.near.table$female.pct[i] <- (sum(raab$spectacles_used_ever_near[raab$gender=='female']==specs.ever.near[i]) / sum(raab$gg_case[raab$gender=='female']==1))
  
  specs.ever.near.table$male.n[i] <- sum(raab$spectacles_used_ever_near[raab$gender=='male']==specs.ever.near[i])
  specs.ever.near.table$male.pct[i] <- (sum(raab$spectacles_used_ever_near[raab$gender=='male']==specs.ever.near[i]) / sum(raab$gg_case[raab$gender=='male']==1))
  
  specs.ever.near.table$total.n[i] <- sum(raab$spectacles_used_ever_near==specs.ever.near[i])
  specs.ever.near.table$total.pct[i] <- (sum(raab$spectacles_used_ever_near==specs.ever.near[i]) / sum(raab$gg_case==1))
  
}

pcts <- grep("pct",names(specs.ever.near.table))
specs.ever.near.table[,pcts] <- format(round(specs.ever.near.table[,pcts] * 100, 1), nsmall=1)

# if yes, why not using near specs now?
specs.not.now.near <-c("spectacles_not_using_no_need", "spectacles_not_using_ineffective", "spectacles_not_using_lost", "spectacles_not_using_disliked")
specs.not.now.near.table<-data.frame(specs.not.now.near)
specs.not.now.near.table[,2:7] <- NA
names(specs.not.now.near.table) <- c("specs.not.now.near",
                                  
                                  "female.n",
                                  "female.pct",
                                  
                                  "male.n",
                                  "male.pct",
                                  
                                  "total.n",
                                  "total.pct")

for (i in 1:length(specs.not.now.near)) 
  
{
  
  specs.not.now.near.table$female.n[i] <- sum(raab$spectacles_not_using_near[raab$gender=='female']==specs.not.now.near[i])
  specs.not.now.near.table$female.pct[i] <- (sum(raab$spectacles_not_using_near[raab$gender=='female']==specs.not.now.near[i]) / sum(raab$specs.ever.used.true.denom[raab$gender=='female']==1))
  
  specs.not.now.near.table$male.n[i] <- sum(raab$spectacles_not_using_near[raab$gender=='male']==specs.not.now.near[i])
  specs.not.now.near.table$male.pct[i] <- (sum(raab$spectacles_not_using_near[raab$gender=='male']==specs.not.now.near[i]) / sum(raab$specs.ever.used.true.denom[raab$gender=='male']==1))
  
  specs.not.now.near.table$total.n[i] <- sum(raab$spectacles_not_using_near==specs.not.now.near[i])
  specs.not.now.near.table$total.pct[i] <- (sum(raab$spectacles_not_using_near==specs.not.now.near[i]) / sum(raab$specs.ever.used.true.denom==1))
  
}

pcts <- grep("pct",names(specs.not.now.near.table))
specs.not.now.near.table[,pcts] <- format(round(specs.not.now.near.table[,pcts] * 100, 1), nsmall=1)

# if yes, why near specs not replaced? (only if response to specs.not.now.near is not "spectacles_not_using_no_need")
specs.not.replaced.near <-c("spectacles_not_replaced_unavailable", "spectacles_not_replaced_unaffordable", "spectacles_not_replaced_other")
specs.not.replaced.near.table<-data.frame(specs.not.replaced.near)
specs.not.replaced.near.table[,2:7] <- NA
names(specs.not.replaced.near.table) <- c("specs.not.replaced.near",
                                     
                                     "female.n",
                                     "female.pct",
                                     
                                     "male.n",
                                     "male.pct",
                                     
                                     "total.n",
                                     "total.pct")

for (i in 1:length(specs.not.replaced.near)) 
  
{
  
  specs.not.replaced.near.table$female.n[i] <- sum(raab$spectacles_not_replaced_near[raab$gender=='female']==specs.not.replaced.near[i])
  specs.not.replaced.near.table$female.pct[i] <- (sum(raab$spectacles_not_replaced_near[raab$gender=='female']==specs.not.replaced.near[i]) / sum(raab$specs.not.replaced.near.denom[raab$gender=='female']==1))
  
  specs.not.replaced.near.table$male.n[i] <- sum(raab$spectacles_not_replaced_near[raab$gender=='male']==specs.not.replaced.near[i])
  specs.not.replaced.near.table$male.pct[i] <- (sum(raab$spectacles_not_replaced_near[raab$gender=='male']==specs.not.replaced.near[i]) / sum(raab$specs.not.replaced.near.denom[raab$gender=='male']==1))
  
  specs.not.replaced.near.table$total.n[i] <- sum(raab$spectacles_not_replaced_near==specs.not.replaced.near[i])
  specs.not.replaced.near.table$total.pct[i] <- (sum(raab$spectacles_not_replaced_near==specs.not.replaced.near[i]) / sum(raab$specs.not.replaced.near.denom==1))
  
}

pcts <- grep("pct",names(specs.not.replaced.near.table))
specs.not.replaced.near.table[,pcts] <- format(round(specs.not.replaced.near.table[,pcts] * 100, 1), nsmall=1)

# if no, why near specs never used before?
specs.never.near <-c("spectacles_not_used_no_need", "spectacles_not_used_unavailable", "spectacles_not_used_unaffordable", "spectacles_not_used_other")
specs.never.near.table<-data.frame(specs.never.near)
specs.never.near.table[,2:7] <- NA
names(specs.never.near.table) <- c("specs.never.near",
                                          
                                          "female.n",
                                          "female.pct",
                                          
                                          "male.n",
                                          "male.pct",
                                          
                                          "total.n",
                                          "total.pct")

for (i in 1:length(specs.never.near)) 
  
{
  
  specs.never.near.table$female.n[i] <- sum(raab$spectacles_not_used_near[raab$gender=='female']==specs.never.near[i])
  specs.never.near.table$female.pct[i] <- (sum(raab$spectacles_not_used_near[raab$gender=='female']==specs.never.near[i]) / sum(raab$specs.ever.used.false.denom[raab$gender=='female']==1))
  
  specs.never.near.table$male.n[i] <- sum(raab$spectacles_not_used_near[raab$gender=='male']==specs.never.near[i])
  specs.never.near.table$male.pct[i] <- (sum(raab$spectacles_not_used_near[raab$gender=='male']==specs.never.near[i]) / sum(raab$specs.ever.used.false.denom[raab$gender=='male']==1))
  
  specs.never.near.table$total.n[i] <- sum(raab$spectacles_not_used_near==specs.never.near[i])
  specs.never.near.table$total.pct[i] <- (sum(raab$spectacles_not_used_near==specs.never.near[i]) / sum(raab$specs.ever.used.false.denom==1))
  
}

pcts <- grep("pct",names(specs.never.near.table))
specs.never.near.table[,pcts] <- format(round(specs.never.near.table[,pcts] * 100, 1), nsmall=1)


## create two new tables combining all questions for with and without near specs

# with near specs
specs.near.owns <- c("First near spectacles", 
                     "Yes", "No", 
                     "Age of current near spectacles", 
                     "<2 years", "2-5 years", ">5 years", 
                     "Type of current near spectacles",
                     "Custom", "Ready-made",
                     "How refraction was assessed",
                     "Eye health worker", "Other health worker", "Self-assessed",
                     "Were near spectacles free",
                     "Yes", "No",
                     "If lost/broken, would you replace",
                     "Yes", "No")

specs.near.owns.table<-as.data.frame(specs.near.owns)
specs.near.owns.table[,2:7]<-NA
names(specs.near.owns.table) <- c("",
                         
                         "female.n",
                         "female.pct",
                         
                         "male.n",
                         "male.pct",
                         
                         "total.n",
                         "total.pct")

specs.near.owns.table[2:3,2:7]<-specs.first.pair.near.table[1:2,2:7]
specs.near.owns.table[5:7,2:7]<-specs.age.near.table[1:3,2:7]
specs.near.owns.table[9:10,2:7]<-specs.type.near.table[1:2,2:7]
specs.near.owns.table[12:14,2:7]<-specs.assess.near.table[1:3,2:7]
specs.near.owns.table[16:17,2:7]<-specs.free.near.table[1:2,2:7]
specs.near.owns.table[19:20,2:7]<-specs.replace.near.table[1:2,2:7]

# without near specs
specs.near.none <- c("Have you ever used near spectacles",
                     "Yes", "No",
                     "If yes, why not currently using",
                     "Need not felt", "No longer effective", "Lost/broken", "Disliked appearance",
                     "Why have you not replaced them",
                     "Not available", "Not affordable", "Other reason",
                     "If no, why not used before",
                     "Need not felt", "Not available", "Not affordable", "Other reason")

specs.near.none.table<-as.data.frame(specs.near.none)
specs.near.none.table[,2:7]<-NA
names(specs.near.none.table) <- c("",
                                  
                                  "female.n",
                                  "female.pct",
                                  
                                  "male.n",
                                  "male.pct",
                                  
                                  "total.n",
                                  "total.pct")

specs.near.none.table[2:3,2:7]<-specs.ever.near.table[1:2,2:7]
specs.near.none.table[5:8,2:7]<-specs.not.now.near.table[1:4,2:7]
specs.near.none.table[10:12,2:7]<-specs.not.replaced.near.table[1:3,2:7]
specs.near.none.table[14:17,2:7]<-specs.never.near.table[1:4,2:7]

