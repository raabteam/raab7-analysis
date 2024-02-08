# Washington Group Questions (Short Set)
# IM 24.05.22 v1 ss
# IM 07.02.24 v2 ss-e

# four levels: none, some, a lot, cannot so
# a lot and cannot do = disability status for binary var
# different response options for anxiety and depression - binary disability status var based on (daily AND (medium OR a lot)) AND (weekly AND a lot)

# Disability (all domains) crude prevalence by gender

wgq.domains.table <- data.frame(dis.domains)
wgq.domains.table[,c(2:7)]<-NA
names(wgq.domains.table)<-c("disability",
               
               "female.n",
               "female.pct",
               
               "male.n",
               "male.pct",
               
               "total.n",
               "total.pct")

wgq.domains.table$female.n[wgq.domains.table$disability=="wgq.dis.see"]<-sum(raab$wgq.dis.see[raab$gender=="female"],na.rm=T)
wgq.domains.table$female.n[wgq.domains.table$disability=="wgq.dis.hear"]<-sum(raab$wgq.dis.hear[raab$gender=="female"],na.rm=T)
wgq.domains.table$female.n[wgq.domains.table$disability=="wgq.dis.mob"]<-sum(raab$wgq.dis.mob[raab$gender=="female"],na.rm=T)
wgq.domains.table$female.n[wgq.domains.table$disability=="wgq.dis.mem"]<-sum(raab$wgq.dis.mem[raab$gender=="female"],na.rm=T)
wgq.domains.table$female.n[wgq.domains.table$disability=="wgq.dis.comm"]<-sum(raab$wgq.dis.comm[raab$gender=="female"],na.rm=T)
wgq.domains.table$female.n[wgq.domains.table$disability=="wgq.dis.self"]<-sum(raab$wgq.dis.self[raab$gender=="female"],na.rm=T)

wgq.domains.table$female.n[wgq.domains.table$disability=="wgq.dis.upbod.str"]<-sum(raab$wgq.dis.upbod.str[raab$gender=="female"],na.rm=T)
wgq.domains.table$female.n[wgq.domains.table$disability=="wgq.dis.upbod.dex"]<-sum(raab$wgq.dis.upbod.dex[raab$gender=="female"],na.rm=T)
wgq.domains.table$female.n[wgq.domains.table$disability=="wgq.dis.anx"]<-sum(raab$wgq.dis.anx[raab$gender=="female"],na.rm=T)
wgq.domains.table$female.n[wgq.domains.table$disability=="wgq.dis.dep"]<-sum(raab$wgq.dis.dep[raab$gender=="female"],na.rm=T)

wgq.domains.table$female.n[wgq.domains.table$disability=="wgq.dis.any"]<-sum(raab$wgq.dis.any[raab$gender=="female"],na.rm=T)
wgq.domains.table$female.n[wgq.domains.table$disability=="wgq.dis.nonvi"]<-sum(raab$wgq.dis.nonvi[raab$gender=="female"],na.rm=T)

wgq.domains.table$male.n[wgq.domains.table$disability=="wgq.dis.see"]<-sum(raab$wgq.dis.see[raab$gender=="male"],na.rm=T)
wgq.domains.table$male.n[wgq.domains.table$disability=="wgq.dis.hear"]<-sum(raab$wgq.dis.hear[raab$gender=="male"],na.rm=T)
wgq.domains.table$male.n[wgq.domains.table$disability=="wgq.dis.mob"]<-sum(raab$wgq.dis.mob[raab$gender=="male"],na.rm=T)
wgq.domains.table$male.n[wgq.domains.table$disability=="wgq.dis.mem"]<-sum(raab$wgq.dis.mem[raab$gender=="male"],na.rm=T)
wgq.domains.table$male.n[wgq.domains.table$disability=="wgq.dis.comm"]<-sum(raab$wgq.dis.comm[raab$gender=="male"],na.rm=T)
wgq.domains.table$male.n[wgq.domains.table$disability=="wgq.dis.self"]<-sum(raab$wgq.dis.self[raab$gender=="male"],na.rm=T)

wgq.domains.table$male.n[wgq.domains.table$disability=="wgq.dis.upbod.str"]<-sum(raab$wgq.dis.upbod.str[raab$gender=="male"],na.rm=T)
wgq.domains.table$male.n[wgq.domains.table$disability=="wgq.dis.upbod.dex"]<-sum(raab$wgq.dis.upbod.dex[raab$gender=="male"],na.rm=T)
wgq.domains.table$male.n[wgq.domains.table$disability=="wgq.dis.anx"]<-sum(raab$wgq.dis.anx[raab$gender=="male"],na.rm=T)
wgq.domains.table$male.n[wgq.domains.table$disability=="wgq.dis.dep"]<-sum(raab$wgq.dis.dep[raab$gender=="male"],na.rm=T)

wgq.domains.table$male.n[wgq.domains.table$disability=="wgq.dis.any"]<-sum(raab$wgq.dis.any[raab$gender=="male"],na.rm=T)
wgq.domains.table$male.n[wgq.domains.table$disability=="wgq.dis.nonvi"]<-sum(raab$wgq.dis.nonvi[raab$gender=="male"],na.rm=T)

wgq.domains.table$total.n[wgq.domains.table$disability=="wgq.dis.see"]<-sum(raab$wgq.dis.see,na.rm=T)
wgq.domains.table$total.n[wgq.domains.table$disability=="wgq.dis.hear"]<-sum(raab$wgq.dis.hear,na.rm=T)
wgq.domains.table$total.n[wgq.domains.table$disability=="wgq.dis.mob"]<-sum(raab$wgq.dis.mob,na.rm=T)
wgq.domains.table$total.n[wgq.domains.table$disability=="wgq.dis.mem"]<-sum(raab$wgq.dis.mem,na.rm=T)
wgq.domains.table$total.n[wgq.domains.table$disability=="wgq.dis.comm"]<-sum(raab$wgq.dis.comm,na.rm=T)
wgq.domains.table$total.n[wgq.domains.table$disability=="wgq.dis.self"]<-sum(raab$wgq.dis.self,na.rm=T)

wgq.domains.table$total.n[wgq.domains.table$disability=="wgq.dis.upbod.str"]<-sum(raab$wgq.dis.upbod.str,na.rm=T)
wgq.domains.table$total.n[wgq.domains.table$disability=="wgq.dis.upbod.dex"]<-sum(raab$wgq.dis.upbod.dex,na.rm=T)
wgq.domains.table$total.n[wgq.domains.table$disability=="wgq.dis.anx"]<-sum(raab$wgq.dis.anx,na.rm=T)
wgq.domains.table$total.n[wgq.domains.table$disability=="wgq.dis.dep"]<-sum(raab$wgq.dis.dep,na.rm=T)

wgq.domains.table$total.n[wgq.domains.table$disability=="wgq.dis.any"]<-sum(raab$wgq.dis.any,na.rm=T)
wgq.domains.table$total.n[wgq.domains.table$disability=="wgq.dis.nonvi"]<-sum(raab$wgq.dis.nonvi,na.rm=T)

wgq.domains.table$female.pct[wgq.domains.table$disability=="wgq.dis.see"]<-sum(raab$wgq.dis.see[raab$gender=="female"],na.rm=T)/sum(raab$vi.denom[raab$gender=="female"],na.rm=T)*100
wgq.domains.table$female.pct[wgq.domains.table$disability=="wgq.dis.hear"]<-sum(raab$wgq.dis.hear[raab$gender=="female"],na.rm=T)/sum(raab$vi.denom[raab$gender=="female"],na.rm=T)*100
wgq.domains.table$female.pct[wgq.domains.table$disability=="wgq.dis.mob"]<-sum(raab$wgq.dis.mob[raab$gender=="female"],na.rm=T)/sum(raab$vi.denom[raab$gender=="female"],na.rm=T)*100
wgq.domains.table$female.pct[wgq.domains.table$disability=="wgq.dis.mem"]<-sum(raab$wgq.dis.mem[raab$gender=="female"],na.rm=T)/sum(raab$vi.denom[raab$gender=="female"],na.rm=T)*100
wgq.domains.table$female.pct[wgq.domains.table$disability=="wgq.dis.comm"]<-sum(raab$wgq.dis.comm[raab$gender=="female"],na.rm=T)/sum(raab$vi.denom[raab$gender=="female"],na.rm=T)*100
wgq.domains.table$female.pct[wgq.domains.table$disability=="wgq.dis.self"]<-sum(raab$wgq.dis.self[raab$gender=="female"],na.rm=T)/sum(raab$vi.denom[raab$gender=="female"],na.rm=T)*100

wgq.domains.table$female.pct[wgq.domains.table$disability=="wgq.dis.upbod.str"]<-sum(raab$wgq.dis.upbod.str[raab$gender=="female"],na.rm=T)/sum(raab$vi.denom[raab$gender=="female"],na.rm=T)*100
wgq.domains.table$female.pct[wgq.domains.table$disability=="wgq.dis.upbod.dex"]<-sum(raab$wgq.dis.upbod.dex[raab$gender=="female"],na.rm=T)/sum(raab$vi.denom[raab$gender=="female"],na.rm=T)*100
wgq.domains.table$female.pct[wgq.domains.table$disability=="wgq.dis.anx"]<-sum(raab$wgq.dis.anx[raab$gender=="female"],na.rm=T)/sum(raab$vi.denom[raab$gender=="female"],na.rm=T)*100
wgq.domains.table$female.pct[wgq.domains.table$disability=="wgq.dis.dep"]<-sum(raab$wgq.dis.dep[raab$gender=="female"],na.rm=T)/sum(raab$vi.denom[raab$gender=="female"],na.rm=T)*100

wgq.domains.table$female.pct[wgq.domains.table$disability=="wgq.dis.any"]<-sum(raab$wgq.dis.any[raab$gender=="female"],na.rm=T)/sum(raab$vi.denom[raab$gender=="female"],na.rm=T)*100
wgq.domains.table$female.pct[wgq.domains.table$disability=="wgq.dis.nonvi"]<-sum(raab$wgq.dis.nonvi[raab$gender=="female"],na.rm=T)/sum(raab$vi.denom[raab$gender=="female"],na.rm=T)*100

wgq.domains.table$male.pct[wgq.domains.table$disability=="wgq.dis.see"]<-sum(raab$wgq.dis.see[raab$gender=="male"],na.rm=T)/sum(raab$vi.denom[raab$gender=="male"],na.rm=T)*100
wgq.domains.table$male.pct[wgq.domains.table$disability=="wgq.dis.hear"]<-sum(raab$wgq.dis.hear[raab$gender=="male"],na.rm=T)/sum(raab$vi.denom[raab$gender=="male"],na.rm=T)*100
wgq.domains.table$male.pct[wgq.domains.table$disability=="wgq.dis.mob"]<-sum(raab$wgq.dis.mob[raab$gender=="male"],na.rm=T)/sum(raab$vi.denom[raab$gender=="male"],na.rm=T)*100
wgq.domains.table$male.pct[wgq.domains.table$disability=="wgq.dis.mem"]<-sum(raab$wgq.dis.mem[raab$gender=="male"],na.rm=T)/sum(raab$vi.denom[raab$gender=="male"],na.rm=T)*100
wgq.domains.table$male.pct[wgq.domains.table$disability=="wgq.dis.comm"]<-sum(raab$wgq.dis.comm[raab$gender=="male"],na.rm=T)/sum(raab$vi.denom[raab$gender=="male"],na.rm=T)*100
wgq.domains.table$male.pct[wgq.domains.table$disability=="wgq.dis.self"]<-sum(raab$wgq.dis.self[raab$gender=="male"],na.rm=T)/sum(raab$vi.denom[raab$gender=="male"],na.rm=T)*100

wgq.domains.table$male.pct[wgq.domains.table$disability=="wgq.dis.upbod.str"]<-sum(raab$wgq.dis.upbod.str[raab$gender=="male"],na.rm=T)/sum(raab$vi.denom[raab$gender=="male"],na.rm=T)*100
wgq.domains.table$male.pct[wgq.domains.table$disability=="wgq.dis.upbod.dex"]<-sum(raab$wgq.dis.upbod.dex[raab$gender=="male"],na.rm=T)/sum(raab$vi.denom[raab$gender=="male"],na.rm=T)*100
wgq.domains.table$male.pct[wgq.domains.table$disability=="wgq.dis.anx"]<-sum(raab$wgq.dis.anx[raab$gender=="male"],na.rm=T)/sum(raab$vi.denom[raab$gender=="male"],na.rm=T)*100
wgq.domains.table$male.pct[wgq.domains.table$disability=="wgq.dis.dep"]<-sum(raab$wgq.dis.dep[raab$gender=="male"],na.rm=T)/sum(raab$vi.denom[raab$gender=="male"],na.rm=T)*100

wgq.domains.table$male.pct[wgq.domains.table$disability=="wgq.dis.any"]<-sum(raab$wgq.dis.any[raab$gender=="male"],na.rm=T)/sum(raab$vi.denom[raab$gender=="male"],na.rm=T)*100
wgq.domains.table$male.pct[wgq.domains.table$disability=="wgq.dis.nonvi"]<-sum(raab$wgq.dis.nonvi[raab$gender=="male"],na.rm=T)/sum(raab$vi.denom[raab$gender=="male"],na.rm=T)*100

wgq.domains.table$total.pct[wgq.domains.table$disability=="wgq.dis.see"]<-sum(raab$wgq.dis.see,na.rm=T)/sum(raab$vi.denom,na.rm=T)*100
wgq.domains.table$total.pct[wgq.domains.table$disability=="wgq.dis.hear"]<-sum(raab$wgq.dis.hear,na.rm=T)/sum(raab$vi.denom,na.rm=T)*100
wgq.domains.table$total.pct[wgq.domains.table$disability=="wgq.dis.mob"]<-sum(raab$wgq.dis.mob,na.rm=T)/sum(raab$vi.denom,na.rm=T)*100
wgq.domains.table$total.pct[wgq.domains.table$disability=="wgq.dis.mem"]<-sum(raab$wgq.dis.mem,na.rm=T)/sum(raab$vi.denom,na.rm=T)*100
wgq.domains.table$total.pct[wgq.domains.table$disability=="wgq.dis.comm"]<-sum(raab$wgq.dis.comm,na.rm=T)/sum(raab$vi.denom,na.rm=T)*100
wgq.domains.table$total.pct[wgq.domains.table$disability=="wgq.dis.self"]<-sum(raab$wgq.dis.self,na.rm=T)/sum(raab$vi.denom,na.rm=T)*100

wgq.domains.table$total.pct[wgq.domains.table$disability=="wgq.dis.upbod.str"]<-sum(raab$wgq.dis.upbod.str,na.rm=T)/sum(raab$vi.denom,na.rm=T)*100
wgq.domains.table$total.pct[wgq.domains.table$disability=="wgq.dis.upbod.dex"]<-sum(raab$wgq.dis.upbod.dex,na.rm=T)/sum(raab$vi.denom,na.rm=T)*100
wgq.domains.table$total.pct[wgq.domains.table$disability=="wgq.dis.anx"]<-sum(raab$wgq.dis.anx,na.rm=T)/sum(raab$vi.denom,na.rm=T)*100
wgq.domains.table$total.pct[wgq.domains.table$disability=="wgq.dis.dep"]<-sum(raab$wgq.dis.dep,na.rm=T)/sum(raab$vi.denom,na.rm=T)*100

wgq.domains.table$total.pct[wgq.domains.table$disability=="wgq.dis.any"]<-sum(raab$wgq.dis.any,na.rm=T)/sum(raab$vi.denom,na.rm=T)*100
wgq.domains.table$total.pct[wgq.domains.table$disability=="wgq.dis.nonvi"]<-sum(raab$wgq.dis.nonvi,na.rm=T)/sum(raab$vi.denom,na.rm=T)*100

wgq.domains.table$disability<-as.character(c("Seeing", "Hearing", "Mobility", "Memory", "Communication", "Self care", "Upper body strength", "Upper body dexterity", "Anxiety", "Depression", "Any domain", "Any non-seeing domain"))
is.num <- sapply(wgq.domains.table, is.numeric)
wgq.domains.table[is.num] <- lapply(wgq.domains.table[is.num], round, 1)

wgq.domains.agegroups<-data.frame(age.groups.tens)
wgq.domains.agegroups[,2:21] <- NA
names(wgq.domains.agegroups) <- c("age.groups.tens",

                 "seeing.n",
                 "seeing.pct",
                 
                 "hearing.n",
                 "hearing.pct",
                 
                 "mobility.n",
                 "mobility.pct",
                 
                 "memory.n",
                 "memory.pct",
                 
                 "communication.n",
                 "communication.pct",
                 
                 "selfcare.n",
                 "selfcare.pct",
                 
                 "upperbodystrength.n",
                 "upperbodystrength.pct",
                 
                 "upperbodydexterity.n",
                 "upperbodydexterity.pct",
                 
                 "anxiety.n",
                 "anxiety.pct",
                 
                 "depression.n",
                 "depression.pct")

for (i in 1:length(age.groups.tens)) {
  
  wgq.domains.agegroups$seeing.n[i] <- sum(raab$wgq.dis.see[raab$age.groups.tens==age.groups.tens[i]],na.rm=T) 
  wgq.domains.agegroups$seeing.pct[i] <- (sum(raab$wgq.dis.see[raab$age.groups.tens==age.groups.tens[i]],na.rm=T)/sum(raab$vi.denom[raab$age.groups.tens==age.groups.tens[i]],na.rm=T))
  
  wgq.domains.agegroups$hearing.n[i] <- sum(raab$wgq.dis.hear[raab$age.groups.tens==age.groups.tens[i]],na.rm=T) 
  wgq.domains.agegroups$hearing.pct[i] <- (sum(raab$wgq.dis.hear[raab$age.groups.tens==age.groups.tens[i]],na.rm=T)/sum(raab$vi.denom[raab$age.groups.tens==age.groups.tens[i]],na.rm=T))

  wgq.domains.agegroups$mobility.n[i] <- sum(raab$wgq.dis.mob[raab$age.groups.tens==age.groups.tens[i]],na.rm=T) 
  wgq.domains.agegroups$mobility.pct[i] <- (sum(raab$wgq.dis.mob[raab$age.groups.tens==age.groups.tens[i]],na.rm=T)/sum(raab$vi.denom[raab$age.groups.tens==age.groups.tens[i]],na.rm=T))

  wgq.domains.agegroups$memory.n[i] <- sum(raab$wgq.dis.mem[raab$age.groups.tens==age.groups.tens[i]],na.rm=T) 
  wgq.domains.agegroups$memory.pct[i] <- (sum(raab$wgq.dis.mem[raab$age.groups.tens==age.groups.tens[i]],na.rm=T)/sum(raab$vi.denom[raab$age.groups.tens==age.groups.tens[i]],na.rm=T))

  wgq.domains.agegroups$communication.n[i] <- sum(raab$wgq.dis.comm[raab$age.groups.tens==age.groups.tens[i]],na.rm=T) 
  wgq.domains.agegroups$communication.pct[i] <- (sum(raab$wgq.dis.comm[raab$age.groups.tens==age.groups.tens[i]],na.rm=T)/sum(raab$vi.denom[raab$age.groups.tens==age.groups.tens[i]],na.rm=T))

  wgq.domains.agegroups$selfcare.n[i] <- sum(raab$wgq.dis.self[raab$age.groups.tens==age.groups.tens[i]],na.rm=T) 
  wgq.domains.agegroups$selfcare.pct[i] <- (sum(raab$wgq.dis.self[raab$age.groups.tens==age.groups.tens[i]],na.rm=T)/sum(raab$vi.denom[raab$age.groups.tens==age.groups.tens[i]],na.rm=T))
  
  wgq.domains.agegroups$upperbodystrength.n[i] <- sum(raab$wgq.dis.upbod.str[raab$age.groups.tens==age.groups.tens[i]],na.rm=T)
  wgq.domains.agegroups$upperbodystrength.pct[i] <- (sum(raab$wgq.dis.upbod.str[raab$age.groups.tens==age.groups.tens[i]],na.rm=T)/sum(raab$vi.denom[raab$age.groups.tens==age.groups.tens[i]],na.rm=T))
    
  wgq.domains.agegroups$upperbodydexterity.n[i] <- sum(raab$wgq.dis.upbod.dex[raab$age.groups.tens==age.groups.tens[i]],na.rm=T)
  wgq.domains.agegroups$upperbodydexterity.pct[i] <- (sum(raab$wgq.dis.upbod.dex[raab$age.groups.tens==age.groups.tens[i]],na.rm=T)/sum(raab$vi.denom[raab$age.groups.tens==age.groups.tens[i]],na.rm=T))
    
  wgq.domains.agegroups$anxiety.n[i] <- sum(raab$wgq.dis.anx[raab$age.groups.tens==age.groups.tens[i]],na.rm=T)
  wgq.domains.agegroups$anxiety.pct[i] <- (sum(raab$wgq.dis.anx[raab$age.groups.tens==age.groups.tens[i]],na.rm=T)/sum(raab$vi.denom[raab$age.groups.tens==age.groups.tens[i]],na.rm=T))
  
  wgq.domains.agegroups$depression.n[i] <- sum(raab$wgq.dis.dep[raab$age.groups.tens==age.groups.tens[i]],na.rm=T)
  wgq.domains.agegroups$depression.pct[i] <- (sum(raab$wgq.dis.dep[raab$age.groups.tens==age.groups.tens[i]],na.rm=T)/sum(raab$vi.denom[raab$age.groups.tens==age.groups.tens[i]],na.rm=T))
    
}

#Add totals row to bottom of table (for total count of female, male, all)

wgq.domains.agegroups[nrow(wgq.domains.agegroups)+1,2:21]<-colSums(wgq.domains.agegroups[,2:21])
wgq.domains.agegroups[5,1]<-"Total"

pcts <- grep("pct",names(wgq.domains.agegroups))
wgq.domains.agegroups[,pcts] <- round( wgq.domains.agegroups[,pcts] * 100, 1)

cnts<-grep("\\.n",names(wgq.domains.agegroups))
wgq.domains.agegroups[,cnts]<-format( wgq.domains.agegroups[,cnts], big.interval = 3L, big.mark = " ", scientific=F )

