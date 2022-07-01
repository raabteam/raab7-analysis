# Washington Group Questions (Short Set)
# IM 24.05.22

# four levels: none, some, a lot, cannot so
# a lot and cannot do = disability status for binary var

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

wgq.domains.table$female.n[wgq.domains.table$disability=="wgq.dis.any"]<-sum(raab$wgq.dis.any[raab$gender=="female"],na.rm=T)
wgq.domains.table$female.n[wgq.domains.table$disability=="wgq.dis.nonvi"]<-sum(raab$wgq.dis.nonvi[raab$gender=="female"],na.rm=T)

wgq.domains.table$male.n[wgq.domains.table$disability=="wgq.dis.see"]<-sum(raab$wgq.dis.see[raab$gender=="male"],na.rm=T)
wgq.domains.table$male.n[wgq.domains.table$disability=="wgq.dis.hear"]<-sum(raab$wgq.dis.hear[raab$gender=="male"],na.rm=T)
wgq.domains.table$male.n[wgq.domains.table$disability=="wgq.dis.mob"]<-sum(raab$wgq.dis.mob[raab$gender=="male"],na.rm=T)
wgq.domains.table$male.n[wgq.domains.table$disability=="wgq.dis.mem"]<-sum(raab$wgq.dis.mem[raab$gender=="male"],na.rm=T)
wgq.domains.table$male.n[wgq.domains.table$disability=="wgq.dis.comm"]<-sum(raab$wgq.dis.comm[raab$gender=="male"],na.rm=T)
wgq.domains.table$male.n[wgq.domains.table$disability=="wgq.dis.self"]<-sum(raab$wgq.dis.self[raab$gender=="male"],na.rm=T)

wgq.domains.table$male.n[wgq.domains.table$disability=="wgq.dis.any"]<-sum(raab$wgq.dis.any[raab$gender=="male"],na.rm=T)
wgq.domains.table$male.n[wgq.domains.table$disability=="wgq.dis.nonvi"]<-sum(raab$wgq.dis.nonvi[raab$gender=="male"],na.rm=T)

wgq.domains.table$total.n[wgq.domains.table$disability=="wgq.dis.see"]<-sum(raab$wgq.dis.see,na.rm=T)
wgq.domains.table$total.n[wgq.domains.table$disability=="wgq.dis.hear"]<-sum(raab$wgq.dis.hear,na.rm=T)
wgq.domains.table$total.n[wgq.domains.table$disability=="wgq.dis.mob"]<-sum(raab$wgq.dis.mob,na.rm=T)
wgq.domains.table$total.n[wgq.domains.table$disability=="wgq.dis.mem"]<-sum(raab$wgq.dis.mem,na.rm=T)
wgq.domains.table$total.n[wgq.domains.table$disability=="wgq.dis.comm"]<-sum(raab$wgq.dis.comm,na.rm=T)
wgq.domains.table$total.n[wgq.domains.table$disability=="wgq.dis.self"]<-sum(raab$wgq.dis.self,na.rm=T)

wgq.domains.table$total.n[wgq.domains.table$disability=="wgq.dis.any"]<-sum(raab$wgq.dis.any,na.rm=T)
wgq.domains.table$total.n[wgq.domains.table$disability=="wgq.dis.nonvi"]<-sum(raab$wgq.dis.nonvi,na.rm=T)

wgq.domains.table$female.pct[wgq.domains.table$disability=="wgq.dis.see"]<-sum(raab$wgq.dis.see[raab$gender=="female"],na.rm=T)/sum(raab$vi.denom[raab$gender=="female"],na.rm=T)*100
wgq.domains.table$female.pct[wgq.domains.table$disability=="wgq.dis.hear"]<-sum(raab$wgq.dis.hear[raab$gender=="female"],na.rm=T)/sum(raab$vi.denom[raab$gender=="female"],na.rm=T)*100
wgq.domains.table$female.pct[wgq.domains.table$disability=="wgq.dis.mob"]<-sum(raab$wgq.dis.mob[raab$gender=="female"],na.rm=T)/sum(raab$vi.denom[raab$gender=="female"],na.rm=T)*100
wgq.domains.table$female.pct[wgq.domains.table$disability=="wgq.dis.mem"]<-sum(raab$wgq.dis.mem[raab$gender=="female"],na.rm=T)/sum(raab$vi.denom[raab$gender=="female"],na.rm=T)*100
wgq.domains.table$female.pct[wgq.domains.table$disability=="wgq.dis.comm"]<-sum(raab$wgq.dis.comm[raab$gender=="female"],na.rm=T)/sum(raab$vi.denom[raab$gender=="female"],na.rm=T)*100
wgq.domains.table$female.pct[wgq.domains.table$disability=="wgq.dis.self"]<-sum(raab$wgq.dis.self[raab$gender=="female"],na.rm=T)/sum(raab$vi.denom[raab$gender=="female"],na.rm=T)*100

wgq.domains.table$female.pct[wgq.domains.table$disability=="wgq.dis.any"]<-sum(raab$wgq.dis.any[raab$gender=="female"],na.rm=T)/sum(raab$vi.denom[raab$gender=="female"],na.rm=T)*100
wgq.domains.table$female.pct[wgq.domains.table$disability=="wgq.dis.nonvi"]<-sum(raab$wgq.dis.nonvi[raab$gender=="female"],na.rm=T)/sum(raab$vi.denom[raab$gender=="female"],na.rm=T)*100

wgq.domains.table$male.pct[wgq.domains.table$disability=="wgq.dis.see"]<-sum(raab$wgq.dis.see[raab$gender=="male"],na.rm=T)/sum(raab$vi.denom[raab$gender=="male"],na.rm=T)*100
wgq.domains.table$male.pct[wgq.domains.table$disability=="wgq.dis.hear"]<-sum(raab$wgq.dis.hear[raab$gender=="male"],na.rm=T)/sum(raab$vi.denom[raab$gender=="male"],na.rm=T)*100
wgq.domains.table$male.pct[wgq.domains.table$disability=="wgq.dis.mob"]<-sum(raab$wgq.dis.mob[raab$gender=="male"],na.rm=T)/sum(raab$vi.denom[raab$gender=="male"],na.rm=T)*100
wgq.domains.table$male.pct[wgq.domains.table$disability=="wgq.dis.mem"]<-sum(raab$wgq.dis.mem[raab$gender=="male"],na.rm=T)/sum(raab$vi.denom[raab$gender=="male"],na.rm=T)*100
wgq.domains.table$male.pct[wgq.domains.table$disability=="wgq.dis.comm"]<-sum(raab$wgq.dis.comm[raab$gender=="male"],na.rm=T)/sum(raab$vi.denom[raab$gender=="male"],na.rm=T)*100
wgq.domains.table$male.pct[wgq.domains.table$disability=="wgq.dis.self"]<-sum(raab$wgq.dis.self[raab$gender=="male"],na.rm=T)/sum(raab$vi.denom[raab$gender=="male"],na.rm=T)*100

wgq.domains.table$male.pct[wgq.domains.table$disability=="wgq.dis.any"]<-sum(raab$wgq.dis.any[raab$gender=="male"],na.rm=T)/sum(raab$vi.denom[raab$gender=="male"],na.rm=T)*100
wgq.domains.table$male.pct[wgq.domains.table$disability=="wgq.dis.nonvi"]<-sum(raab$wgq.dis.nonvi[raab$gender=="male"],na.rm=T)/sum(raab$vi.denom[raab$gender=="male"],na.rm=T)*100

wgq.domains.table$total.pct[wgq.domains.table$disability=="wgq.dis.see"]<-sum(raab$wgq.dis.see,na.rm=T)/sum(raab$vi.denom,na.rm=T)*100
wgq.domains.table$total.pct[wgq.domains.table$disability=="wgq.dis.hear"]<-sum(raab$wgq.dis.hear,na.rm=T)/sum(raab$vi.denom,na.rm=T)*100
wgq.domains.table$total.pct[wgq.domains.table$disability=="wgq.dis.mob"]<-sum(raab$wgq.dis.mob,na.rm=T)/sum(raab$vi.denom,na.rm=T)*100
wgq.domains.table$total.pct[wgq.domains.table$disability=="wgq.dis.mem"]<-sum(raab$wgq.dis.mem,na.rm=T)/sum(raab$vi.denom,na.rm=T)*100
wgq.domains.table$total.pct[wgq.domains.table$disability=="wgq.dis.comm"]<-sum(raab$wgq.dis.comm,na.rm=T)/sum(raab$vi.denom,na.rm=T)*100
wgq.domains.table$total.pct[wgq.domains.table$disability=="wgq.dis.self"]<-sum(raab$wgq.dis.self,na.rm=T)/sum(raab$vi.denom,na.rm=T)*100

wgq.domains.table$total.pct[wgq.domains.table$disability=="wgq.dis.any"]<-sum(raab$wgq.dis.any,na.rm=T)/sum(raab$vi.denom,na.rm=T)*100
wgq.domains.table$total.pct[wgq.domains.table$disability=="wgq.dis.nonvi"]<-sum(raab$wgq.dis.nonvi,na.rm=T)/sum(raab$vi.denom,na.rm=T)*100

wgq.domains.table$disability<-as.character(c("Seeing", "Hearing", "Mobility", "Memory", "Communication", "Self care", "Any domain", "Any non-seeing domain"))
is.num <- sapply(wgq.domains.table, is.numeric)
wgq.domains.table[is.num] <- lapply(wgq.domains.table[is.num], round, 1)
