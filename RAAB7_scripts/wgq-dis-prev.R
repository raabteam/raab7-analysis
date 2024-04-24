# Washington Group Questions (Short Set)
# IM 24.05.22 v1 ss
# IM 07.02.24 v2 ss-e

# four levels: none, some, a lot, cannot so
# a lot and cannot do = disability status for binary var
# different response options for anxiety and depression - binary disability status var based on (daily AND (medium OR a lot)) AND (weekly AND a lot)

# Disability (all domains) crude prevalence by gender

wgq.domains.table <- data.frame(dis.domains)
wgq.domains.table[,c(2:22)]<-NA
names(wgq.domains.table)<-c("disability",
                            
                            "female.n",
                            "female.pct",
                            "female.pct.lci",
                            "female.pct.uci",
                            "female.adj.pct",
                            "female.adj.pct.lci",
                            "female.adj.pct.uci",
                            
                            "male.n",
                            "male.pct",
                            "male.pct.lci",
                            "male.pct.uci",
                            "male.adj.pct",
                            "male.adj.pct.lci",
                            "male.adj.pct.uci",
                            
                            "total.n",
                            "total.pct",
                            "total.pct.lci",
                            "total.pct.uci",
                            "total.adj.pct",
                            "total.adj.pct.lci",
                            "total.adj.pct.uci")

wgq.domains.table$female.n[wgq.domains.table$disability=="wgq.dis.see"]<-sum(raab$wgq.dis.see[raab$gender=="female"],na.rm=T)
wgq.domains.table$female.n[wgq.domains.table$disability=="wgq.dis.hear"]<-sum(raab$wgq.dis.hear[raab$gender=="female"],na.rm=T)
wgq.domains.table$female.n[wgq.domains.table$disability=="wgq.dis.mob"]<-sum(raab$wgq.dis.mob[raab$gender=="female"],na.rm=T)
wgq.domains.table$female.n[wgq.domains.table$disability=="wgq.dis.mem"]<-sum(raab$wgq.dis.mem[raab$gender=="female"],na.rm=T)
wgq.domains.table$female.n[wgq.domains.table$disability=="wgq.dis.comm"]<-sum(raab$wgq.dis.comm[raab$gender=="female"],na.rm=T)
wgq.domains.table$female.n[wgq.domains.table$disability=="wgq.dis.self"]<-sum(raab$wgq.dis.self[raab$gender=="female"],na.rm=T)

if (!all(is.na(raab$wg_difficulty_upperbody_strength))){
wgq.domains.table$female.n[wgq.domains.table$disability=="wgq.dis.upbod.str"]<-sum(raab$wgq.dis.upbod.str[raab$gender=="female"],na.rm=T)
wgq.domains.table$female.n[wgq.domains.table$disability=="wgq.dis.upbod.dex"]<-sum(raab$wgq.dis.upbod.dex[raab$gender=="female"],na.rm=T)
wgq.domains.table$female.n[wgq.domains.table$disability=="wgq.dis.anx"]<-sum(raab$wgq.dis.anx[raab$gender=="female"],na.rm=T)
wgq.domains.table$female.n[wgq.domains.table$disability=="wgq.dis.dep"]<-sum(raab$wgq.dis.dep[raab$gender=="female"],na.rm=T)
}

wgq.domains.table$female.n[wgq.domains.table$disability=="wgq.dis.any"]<-sum(raab$wgq.dis.any[raab$gender=="female"],na.rm=T)
wgq.domains.table$female.n[wgq.domains.table$disability=="wgq.dis.nonvi"]<-sum(raab$wgq.dis.nonvi[raab$gender=="female"],na.rm=T)

wgq.domains.table$male.n[wgq.domains.table$disability=="wgq.dis.see"]<-sum(raab$wgq.dis.see[raab$gender=="male"],na.rm=T)
wgq.domains.table$male.n[wgq.domains.table$disability=="wgq.dis.hear"]<-sum(raab$wgq.dis.hear[raab$gender=="male"],na.rm=T)
wgq.domains.table$male.n[wgq.domains.table$disability=="wgq.dis.mob"]<-sum(raab$wgq.dis.mob[raab$gender=="male"],na.rm=T)
wgq.domains.table$male.n[wgq.domains.table$disability=="wgq.dis.mem"]<-sum(raab$wgq.dis.mem[raab$gender=="male"],na.rm=T)
wgq.domains.table$male.n[wgq.domains.table$disability=="wgq.dis.comm"]<-sum(raab$wgq.dis.comm[raab$gender=="male"],na.rm=T)
wgq.domains.table$male.n[wgq.domains.table$disability=="wgq.dis.self"]<-sum(raab$wgq.dis.self[raab$gender=="male"],na.rm=T)

if (!all(is.na(raab$wg_difficulty_upperbody_strength))){
wgq.domains.table$male.n[wgq.domains.table$disability=="wgq.dis.upbod.str"]<-sum(raab$wgq.dis.upbod.str[raab$gender=="male"],na.rm=T)
wgq.domains.table$male.n[wgq.domains.table$disability=="wgq.dis.upbod.dex"]<-sum(raab$wgq.dis.upbod.dex[raab$gender=="male"],na.rm=T)
wgq.domains.table$male.n[wgq.domains.table$disability=="wgq.dis.anx"]<-sum(raab$wgq.dis.anx[raab$gender=="male"],na.rm=T)
wgq.domains.table$male.n[wgq.domains.table$disability=="wgq.dis.dep"]<-sum(raab$wgq.dis.dep[raab$gender=="male"],na.rm=T)
}

wgq.domains.table$male.n[wgq.domains.table$disability=="wgq.dis.any"]<-sum(raab$wgq.dis.any[raab$gender=="male"],na.rm=T)
wgq.domains.table$male.n[wgq.domains.table$disability=="wgq.dis.nonvi"]<-sum(raab$wgq.dis.nonvi[raab$gender=="male"],na.rm=T)

wgq.domains.table$total.n[wgq.domains.table$disability=="wgq.dis.see"]<-sum(raab$wgq.dis.see,na.rm=T)
wgq.domains.table$total.n[wgq.domains.table$disability=="wgq.dis.hear"]<-sum(raab$wgq.dis.hear,na.rm=T)
wgq.domains.table$total.n[wgq.domains.table$disability=="wgq.dis.mob"]<-sum(raab$wgq.dis.mob,na.rm=T)
wgq.domains.table$total.n[wgq.domains.table$disability=="wgq.dis.mem"]<-sum(raab$wgq.dis.mem,na.rm=T)
wgq.domains.table$total.n[wgq.domains.table$disability=="wgq.dis.comm"]<-sum(raab$wgq.dis.comm,na.rm=T)
wgq.domains.table$total.n[wgq.domains.table$disability=="wgq.dis.self"]<-sum(raab$wgq.dis.self,na.rm=T)

if (!all(is.na(raab$wg_difficulty_upperbody_strength))){
wgq.domains.table$total.n[wgq.domains.table$disability=="wgq.dis.upbod.str"]<-sum(raab$wgq.dis.upbod.str,na.rm=T)
wgq.domains.table$total.n[wgq.domains.table$disability=="wgq.dis.upbod.dex"]<-sum(raab$wgq.dis.upbod.dex,na.rm=T)
wgq.domains.table$total.n[wgq.domains.table$disability=="wgq.dis.anx"]<-sum(raab$wgq.dis.anx,na.rm=T)
wgq.domains.table$total.n[wgq.domains.table$disability=="wgq.dis.dep"]<-sum(raab$wgq.dis.dep,na.rm=T)
}

wgq.domains.table$total.n[wgq.domains.table$disability=="wgq.dis.any"]<-sum(raab$wgq.dis.any,na.rm=T)
wgq.domains.table$total.n[wgq.domains.table$disability=="wgq.dis.nonvi"]<-sum(raab$wgq.dis.nonvi,na.rm=T)

wgq.domains.table$female.pct[wgq.domains.table$disability=="wgq.dis.see"]<-sum(raab$wgq.dis.see[raab$gender=="female"],na.rm=T)/sum(raab$vi.denom[raab$gender=="female"],na.rm=T)
wgq.domains.table$female.pct[wgq.domains.table$disability=="wgq.dis.hear"]<-sum(raab$wgq.dis.hear[raab$gender=="female"],na.rm=T)/sum(raab$vi.denom[raab$gender=="female"],na.rm=T)
wgq.domains.table$female.pct[wgq.domains.table$disability=="wgq.dis.mob"]<-sum(raab$wgq.dis.mob[raab$gender=="female"],na.rm=T)/sum(raab$vi.denom[raab$gender=="female"],na.rm=T)
wgq.domains.table$female.pct[wgq.domains.table$disability=="wgq.dis.mem"]<-sum(raab$wgq.dis.mem[raab$gender=="female"],na.rm=T)/sum(raab$vi.denom[raab$gender=="female"],na.rm=T)
wgq.domains.table$female.pct[wgq.domains.table$disability=="wgq.dis.comm"]<-sum(raab$wgq.dis.comm[raab$gender=="female"],na.rm=T)/sum(raab$vi.denom[raab$gender=="female"],na.rm=T)
wgq.domains.table$female.pct[wgq.domains.table$disability=="wgq.dis.self"]<-sum(raab$wgq.dis.self[raab$gender=="female"],na.rm=T)/sum(raab$vi.denom[raab$gender=="female"],na.rm=T)

if (!all(is.na(raab$wg_difficulty_upperbody_strength))){
wgq.domains.table$female.pct[wgq.domains.table$disability=="wgq.dis.upbod.str"]<-sum(raab$wgq.dis.upbod.str[raab$gender=="female"],na.rm=T)/sum(raab$vi.denom[raab$gender=="female"],na.rm=T)
wgq.domains.table$female.pct[wgq.domains.table$disability=="wgq.dis.upbod.dex"]<-sum(raab$wgq.dis.upbod.dex[raab$gender=="female"],na.rm=T)/sum(raab$vi.denom[raab$gender=="female"],na.rm=T)
wgq.domains.table$female.pct[wgq.domains.table$disability=="wgq.dis.anx"]<-sum(raab$wgq.dis.anx[raab$gender=="female"],na.rm=T)/sum(raab$vi.denom[raab$gender=="female"],na.rm=T)
wgq.domains.table$female.pct[wgq.domains.table$disability=="wgq.dis.dep"]<-sum(raab$wgq.dis.dep[raab$gender=="female"],na.rm=T)/sum(raab$vi.denom[raab$gender=="female"],na.rm=T)
}

wgq.domains.table$female.pct[wgq.domains.table$disability=="wgq.dis.any"]<-sum(raab$wgq.dis.any[raab$gender=="female"],na.rm=T)/sum(raab$vi.denom[raab$gender=="female"],na.rm=T)
wgq.domains.table$female.pct[wgq.domains.table$disability=="wgq.dis.nonvi"]<-sum(raab$wgq.dis.nonvi[raab$gender=="female"],na.rm=T)/sum(raab$vi.denom[raab$gender=="female"],na.rm=T)

wgq.domains.table$male.pct[wgq.domains.table$disability=="wgq.dis.see"]<-sum(raab$wgq.dis.see[raab$gender=="male"],na.rm=T)/sum(raab$vi.denom[raab$gender=="male"],na.rm=T)
wgq.domains.table$male.pct[wgq.domains.table$disability=="wgq.dis.hear"]<-sum(raab$wgq.dis.hear[raab$gender=="male"],na.rm=T)/sum(raab$vi.denom[raab$gender=="male"],na.rm=T)
wgq.domains.table$male.pct[wgq.domains.table$disability=="wgq.dis.mob"]<-sum(raab$wgq.dis.mob[raab$gender=="male"],na.rm=T)/sum(raab$vi.denom[raab$gender=="male"],na.rm=T)
wgq.domains.table$male.pct[wgq.domains.table$disability=="wgq.dis.mem"]<-sum(raab$wgq.dis.mem[raab$gender=="male"],na.rm=T)/sum(raab$vi.denom[raab$gender=="male"],na.rm=T)
wgq.domains.table$male.pct[wgq.domains.table$disability=="wgq.dis.comm"]<-sum(raab$wgq.dis.comm[raab$gender=="male"],na.rm=T)/sum(raab$vi.denom[raab$gender=="male"],na.rm=T)
wgq.domains.table$male.pct[wgq.domains.table$disability=="wgq.dis.self"]<-sum(raab$wgq.dis.self[raab$gender=="male"],na.rm=T)/sum(raab$vi.denom[raab$gender=="male"],na.rm=T)

if (!all(is.na(raab$wg_difficulty_upperbody_strength))){
wgq.domains.table$male.pct[wgq.domains.table$disability=="wgq.dis.upbod.str"]<-sum(raab$wgq.dis.upbod.str[raab$gender=="male"],na.rm=T)/sum(raab$vi.denom[raab$gender=="male"],na.rm=T)
wgq.domains.table$male.pct[wgq.domains.table$disability=="wgq.dis.upbod.dex"]<-sum(raab$wgq.dis.upbod.dex[raab$gender=="male"],na.rm=T)/sum(raab$vi.denom[raab$gender=="male"],na.rm=T)
wgq.domains.table$male.pct[wgq.domains.table$disability=="wgq.dis.anx"]<-sum(raab$wgq.dis.anx[raab$gender=="male"],na.rm=T)/sum(raab$vi.denom[raab$gender=="male"],na.rm=T)
wgq.domains.table$male.pct[wgq.domains.table$disability=="wgq.dis.dep"]<-sum(raab$wgq.dis.dep[raab$gender=="male"],na.rm=T)/sum(raab$vi.denom[raab$gender=="male"],na.rm=T)
}

wgq.domains.table$male.pct[wgq.domains.table$disability=="wgq.dis.any"]<-sum(raab$wgq.dis.any[raab$gender=="male"],na.rm=T)/sum(raab$vi.denom[raab$gender=="male"],na.rm=T)
wgq.domains.table$male.pct[wgq.domains.table$disability=="wgq.dis.nonvi"]<-sum(raab$wgq.dis.nonvi[raab$gender=="male"],na.rm=T)/sum(raab$vi.denom[raab$gender=="male"],na.rm=T)

wgq.domains.table$total.pct[wgq.domains.table$disability=="wgq.dis.see"]<-sum(raab$wgq.dis.see,na.rm=T)/sum(raab$vi.denom,na.rm=T)
wgq.domains.table$total.pct[wgq.domains.table$disability=="wgq.dis.hear"]<-sum(raab$wgq.dis.hear,na.rm=T)/sum(raab$vi.denom,na.rm=T)
wgq.domains.table$total.pct[wgq.domains.table$disability=="wgq.dis.mob"]<-sum(raab$wgq.dis.mob,na.rm=T)/sum(raab$vi.denom,na.rm=T)
wgq.domains.table$total.pct[wgq.domains.table$disability=="wgq.dis.mem"]<-sum(raab$wgq.dis.mem,na.rm=T)/sum(raab$vi.denom,na.rm=T)
wgq.domains.table$total.pct[wgq.domains.table$disability=="wgq.dis.comm"]<-sum(raab$wgq.dis.comm,na.rm=T)/sum(raab$vi.denom,na.rm=T)
wgq.domains.table$total.pct[wgq.domains.table$disability=="wgq.dis.self"]<-sum(raab$wgq.dis.self,na.rm=T)/sum(raab$vi.denom,na.rm=T)

if (!all(is.na(raab$wg_difficulty_upperbody_strength))){
wgq.domains.table$total.pct[wgq.domains.table$disability=="wgq.dis.upbod.str"]<-sum(raab$wgq.dis.upbod.str,na.rm=T)/sum(raab$vi.denom,na.rm=T)
wgq.domains.table$total.pct[wgq.domains.table$disability=="wgq.dis.upbod.dex"]<-sum(raab$wgq.dis.upbod.dex,na.rm=T)/sum(raab$vi.denom,na.rm=T)
wgq.domains.table$total.pct[wgq.domains.table$disability=="wgq.dis.anx"]<-sum(raab$wgq.dis.anx,na.rm=T)/sum(raab$vi.denom,na.rm=T)
wgq.domains.table$total.pct[wgq.domains.table$disability=="wgq.dis.dep"]<-sum(raab$wgq.dis.dep,na.rm=T)/sum(raab$vi.denom,na.rm=T)
}

wgq.domains.table$total.pct[wgq.domains.table$disability=="wgq.dis.any"]<-sum(raab$wgq.dis.any,na.rm=T)/sum(raab$vi.denom,na.rm=T)
wgq.domains.table$total.pct[wgq.domains.table$disability=="wgq.dis.nonvi"]<-sum(raab$wgq.dis.nonvi,na.rm=T)/sum(raab$vi.denom,na.rm=T)

wgq.domains.table$female.pct.lci[wgq.domains.table$disability=="wgq.dis.see"]<-bennett.lci(wgq.domains.table$female.pct[wgq.domains.table$disability=="wgq.dis.see"],raab$wgq.dis.see[raab$gender=="female"],raab$vi.denom[raab$gender=="female"],raab$clusterId[raab$gender=="female"])
wgq.domains.table$female.pct.lci[wgq.domains.table$disability=="wgq.dis.hear"]<-bennett.lci(wgq.domains.table$female.pct[wgq.domains.table$disability=="wgq.dis.hear"],raab$wgq.dis.hear[raab$gender=="female"],raab$vi.denom[raab$gender=="female"],raab$clusterId[raab$gender=="female"])
wgq.domains.table$female.pct.lci[wgq.domains.table$disability=="wgq.dis.mob"]<-bennett.lci(wgq.domains.table$female.pct[wgq.domains.table$disability=="wgq.dis.mob"],raab$wgq.dis.mob[raab$gender=="female"],raab$vi.denom[raab$gender=="female"],raab$clusterId[raab$gender=="female"])
wgq.domains.table$female.pct.lci[wgq.domains.table$disability=="wgq.dis.mem"]<-bennett.lci(wgq.domains.table$female.pct[wgq.domains.table$disability=="wgq.dis.mem"],raab$wgq.dis.mem[raab$gender=="female"],raab$vi.denom[raab$gender=="female"],raab$clusterId[raab$gender=="female"])
wgq.domains.table$female.pct.lci[wgq.domains.table$disability=="wgq.dis.comm"]<-bennett.lci(wgq.domains.table$female.pct[wgq.domains.table$disability=="wgq.dis.comm"],raab$wgq.dis.comm[raab$gender=="female"],raab$vi.denom[raab$gender=="female"],raab$clusterId[raab$gender=="female"])
wgq.domains.table$female.pct.lci[wgq.domains.table$disability=="wgq.dis.self"]<-bennett.lci(wgq.domains.table$female.pct[wgq.domains.table$disability=="wgq.dis.self"],raab$wgq.dis.self[raab$gender=="female"],raab$vi.denom[raab$gender=="female"],raab$clusterId[raab$gender=="female"])

if (!all(is.na(raab$wg_difficulty_upperbody_strength))){
wgq.domains.table$female.pct.lci[wgq.domains.table$disability=="wgq.dis.upbod.str"]<-bennett.lci(wgq.domains.table$female.pct[wgq.domains.table$disability=="wgq.dis.upbod.str"],raab$wgq.dis.mob[raab$gender=="female"],raab$vi.denom[raab$gender=="female"],raab$clusterId[raab$gender=="female"])
wgq.domains.table$female.pct.lci[wgq.domains.table$disability=="wgq.dis.upbod.dex"]<-bennett.lci(wgq.domains.table$female.pct[wgq.domains.table$disability=="wgq.dis.upbod.dex"],raab$wgq.dis.mem[raab$gender=="female"],raab$vi.denom[raab$gender=="female"],raab$clusterId[raab$gender=="female"])
wgq.domains.table$female.pct.lci[wgq.domains.table$disability=="wgq.dis.anx"]<-bennett.lci(wgq.domains.table$female.pct[wgq.domains.table$disability=="wgq.dis.anx"],raab$wgq.dis.comm[raab$gender=="female"],raab$vi.denom[raab$gender=="female"],raab$clusterId[raab$gender=="female"])
wgq.domains.table$female.pct.lci[wgq.domains.table$disability=="wgq.dis.dep"]<-bennett.lci(wgq.domains.table$female.pct[wgq.domains.table$disability=="wgq.dis.dep"],raab$wgq.dis.self[raab$gender=="female"],raab$vi.denom[raab$gender=="female"],raab$clusterId[raab$gender=="female"])
}

wgq.domains.table$female.pct.lci[wgq.domains.table$disability=="wgq.dis.any"]<-bennett.lci(wgq.domains.table$female.pct[wgq.domains.table$disability=="wgq.dis.any"],raab$wgq.dis.any[raab$gender=="female"],raab$vi.denom[raab$gender=="female"],raab$clusterId[raab$gender=="female"])
wgq.domains.table$female.pct.lci[wgq.domains.table$disability=="wgq.dis.nonvi"]<-bennett.lci(wgq.domains.table$female.pct[wgq.domains.table$disability=="wgq.dis.nonvi"],raab$wgq.dis.nonvi[raab$gender=="female"],raab$vi.denom[raab$gender=="female"],raab$clusterId[raab$gender=="female"])

wgq.domains.table$female.pct.uci[wgq.domains.table$disability=="wgq.dis.see"]<-bennett.uci(wgq.domains.table$female.pct[wgq.domains.table$disability=="wgq.dis.see"],raab$wgq.dis.see[raab$gender=="female"],raab$vi.denom[raab$gender=="female"],raab$clusterId[raab$gender=="female"])
wgq.domains.table$female.pct.uci[wgq.domains.table$disability=="wgq.dis.hear"]<-bennett.uci(wgq.domains.table$female.pct[wgq.domains.table$disability=="wgq.dis.hear"],raab$wgq.dis.hear[raab$gender=="female"],raab$vi.denom[raab$gender=="female"],raab$clusterId[raab$gender=="female"])
wgq.domains.table$female.pct.uci[wgq.domains.table$disability=="wgq.dis.mob"]<-bennett.uci(wgq.domains.table$female.pct[wgq.domains.table$disability=="wgq.dis.mob"],raab$wgq.dis.mob[raab$gender=="female"],raab$vi.denom[raab$gender=="female"],raab$clusterId[raab$gender=="female"])
wgq.domains.table$female.pct.uci[wgq.domains.table$disability=="wgq.dis.mem"]<-bennett.uci(wgq.domains.table$female.pct[wgq.domains.table$disability=="wgq.dis.mem"],raab$wgq.dis.mem[raab$gender=="female"],raab$vi.denom[raab$gender=="female"],raab$clusterId[raab$gender=="female"])
wgq.domains.table$female.pct.uci[wgq.domains.table$disability=="wgq.dis.comm"]<-bennett.uci(wgq.domains.table$female.pct[wgq.domains.table$disability=="wgq.dis.comm"],raab$wgq.dis.comm[raab$gender=="female"],raab$vi.denom[raab$gender=="female"],raab$clusterId[raab$gender=="female"])
wgq.domains.table$female.pct.uci[wgq.domains.table$disability=="wgq.dis.self"]<-bennett.uci(wgq.domains.table$female.pct[wgq.domains.table$disability=="wgq.dis.self"],raab$wgq.dis.self[raab$gender=="female"],raab$vi.denom[raab$gender=="female"],raab$clusterId[raab$gender=="female"])

if (!all(is.na(raab$wg_difficulty_upperbody_strength))){
wgq.domains.table$female.pct.uci[wgq.domains.table$disability=="wgq.dis.upbod.str"]<-bennett.uci(wgq.domains.table$female.pct[wgq.domains.table$disability=="wgq.dis.upbod.str"],raab$wgq.dis.mob[raab$gender=="female"],raab$vi.denom[raab$gender=="female"],raab$clusterId[raab$gender=="female"])
wgq.domains.table$female.pct.uci[wgq.domains.table$disability=="wgq.dis.upbod.dex"]<-bennett.uci(wgq.domains.table$female.pct[wgq.domains.table$disability=="wgq.dis.upbod.dex"],raab$wgq.dis.mem[raab$gender=="female"],raab$vi.denom[raab$gender=="female"],raab$clusterId[raab$gender=="female"])
wgq.domains.table$female.pct.uci[wgq.domains.table$disability=="wgq.dis.anx"]<-bennett.uci(wgq.domains.table$female.pct[wgq.domains.table$disability=="wgq.dis.anx"],raab$wgq.dis.comm[raab$gender=="female"],raab$vi.denom[raab$gender=="female"],raab$clusterId[raab$gender=="female"])
wgq.domains.table$female.pct.uci[wgq.domains.table$disability=="wgq.dis.dep"]<-bennett.uci(wgq.domains.table$female.pct[wgq.domains.table$disability=="wgq.dis.dep"],raab$wgq.dis.self[raab$gender=="female"],raab$vi.denom[raab$gender=="female"],raab$clusterId[raab$gender=="female"])
}

wgq.domains.table$female.pct.uci[wgq.domains.table$disability=="wgq.dis.any"]<-bennett.uci(wgq.domains.table$female.pct[wgq.domains.table$disability=="wgq.dis.any"],raab$wgq.dis.any[raab$gender=="female"],raab$vi.denom[raab$gender=="female"],raab$clusterId[raab$gender=="female"])
wgq.domains.table$female.pct.uci[wgq.domains.table$disability=="wgq.dis.nonvi"]<-bennett.uci(wgq.domains.table$female.pct[wgq.domains.table$disability=="wgq.dis.nonvi"],raab$wgq.dis.nonvi[raab$gender=="female"],raab$vi.denom[raab$gender=="female"],raab$clusterId[raab$gender=="female"])

wgq.domains.table$male.pct.lci[wgq.domains.table$disability=="wgq.dis.see"]<-bennett.lci(wgq.domains.table$male.pct[wgq.domains.table$disability=="wgq.dis.see"],raab$wgq.dis.see[raab$gender=="male"],raab$vi.denom[raab$gender=="male"],raab$clusterId[raab$gender=="male"])
wgq.domains.table$male.pct.lci[wgq.domains.table$disability=="wgq.dis.hear"]<-bennett.lci(wgq.domains.table$male.pct[wgq.domains.table$disability=="wgq.dis.hear"],raab$wgq.dis.hear[raab$gender=="male"],raab$vi.denom[raab$gender=="male"],raab$clusterId[raab$gender=="male"])
wgq.domains.table$male.pct.lci[wgq.domains.table$disability=="wgq.dis.mob"]<-bennett.lci(wgq.domains.table$male.pct[wgq.domains.table$disability=="wgq.dis.mob"],raab$wgq.dis.mob[raab$gender=="male"],raab$vi.denom[raab$gender=="male"],raab$clusterId[raab$gender=="male"])
wgq.domains.table$male.pct.lci[wgq.domains.table$disability=="wgq.dis.mem"]<-bennett.lci(wgq.domains.table$male.pct[wgq.domains.table$disability=="wgq.dis.mem"],raab$wgq.dis.mem[raab$gender=="male"],raab$vi.denom[raab$gender=="male"],raab$clusterId[raab$gender=="male"])
wgq.domains.table$male.pct.lci[wgq.domains.table$disability=="wgq.dis.comm"]<-bennett.lci(wgq.domains.table$male.pct[wgq.domains.table$disability=="wgq.dis.comm"],raab$wgq.dis.comm[raab$gender=="male"],raab$vi.denom[raab$gender=="male"],raab$clusterId[raab$gender=="male"])
wgq.domains.table$male.pct.lci[wgq.domains.table$disability=="wgq.dis.self"]<-bennett.lci(wgq.domains.table$male.pct[wgq.domains.table$disability=="wgq.dis.self"],raab$wgq.dis.self[raab$gender=="male"],raab$vi.denom[raab$gender=="male"],raab$clusterId[raab$gender=="male"])

if (!all(is.na(raab$wg_difficulty_upperbody_strength))){
wgq.domains.table$male.pct.lci[wgq.domains.table$disability=="wgq.dis.upbod.str"]<-bennett.lci(wgq.domains.table$male.pct[wgq.domains.table$disability=="wgq.dis.upbod.str"],raab$wgq.dis.mob[raab$gender=="male"],raab$vi.denom[raab$gender=="male"],raab$clusterId[raab$gender=="male"])
wgq.domains.table$male.pct.lci[wgq.domains.table$disability=="wgq.dis.upbod.dex"]<-bennett.lci(wgq.domains.table$male.pct[wgq.domains.table$disability=="wgq.dis.upbod.dex"],raab$wgq.dis.mem[raab$gender=="male"],raab$vi.denom[raab$gender=="male"],raab$clusterId[raab$gender=="male"])
wgq.domains.table$male.pct.lci[wgq.domains.table$disability=="wgq.dis.anx"]<-bennett.lci(wgq.domains.table$male.pct[wgq.domains.table$disability=="wgq.dis.anx"],raab$wgq.dis.comm[raab$gender=="male"],raab$vi.denom[raab$gender=="male"],raab$clusterId[raab$gender=="male"])
wgq.domains.table$male.pct.lci[wgq.domains.table$disability=="wgq.dis.dep"]<-bennett.lci(wgq.domains.table$male.pct[wgq.domains.table$disability=="wgq.dis.dep"],raab$wgq.dis.self[raab$gender=="male"],raab$vi.denom[raab$gender=="male"],raab$clusterId[raab$gender=="male"])
}

wgq.domains.table$male.pct.lci[wgq.domains.table$disability=="wgq.dis.any"]<-bennett.lci(wgq.domains.table$male.pct[wgq.domains.table$disability=="wgq.dis.any"],raab$wgq.dis.any[raab$gender=="male"],raab$vi.denom[raab$gender=="male"],raab$clusterId[raab$gender=="male"])
wgq.domains.table$male.pct.lci[wgq.domains.table$disability=="wgq.dis.nonvi"]<-bennett.lci(wgq.domains.table$male.pct[wgq.domains.table$disability=="wgq.dis.nonvi"],raab$wgq.dis.nonvi[raab$gender=="male"],raab$vi.denom[raab$gender=="male"],raab$clusterId[raab$gender=="male"])

wgq.domains.table$male.pct.uci[wgq.domains.table$disability=="wgq.dis.see"]<-bennett.uci(wgq.domains.table$male.pct[wgq.domains.table$disability=="wgq.dis.see"],raab$wgq.dis.see[raab$gender=="male"],raab$vi.denom[raab$gender=="male"],raab$clusterId[raab$gender=="male"])
wgq.domains.table$male.pct.uci[wgq.domains.table$disability=="wgq.dis.hear"]<-bennett.uci(wgq.domains.table$male.pct[wgq.domains.table$disability=="wgq.dis.hear"],raab$wgq.dis.hear[raab$gender=="male"],raab$vi.denom[raab$gender=="male"],raab$clusterId[raab$gender=="male"])
wgq.domains.table$male.pct.uci[wgq.domains.table$disability=="wgq.dis.mob"]<-bennett.uci(wgq.domains.table$male.pct[wgq.domains.table$disability=="wgq.dis.mob"],raab$wgq.dis.mob[raab$gender=="male"],raab$vi.denom[raab$gender=="male"],raab$clusterId[raab$gender=="male"])
wgq.domains.table$male.pct.uci[wgq.domains.table$disability=="wgq.dis.mem"]<-bennett.uci(wgq.domains.table$male.pct[wgq.domains.table$disability=="wgq.dis.mem"],raab$wgq.dis.mem[raab$gender=="male"],raab$vi.denom[raab$gender=="male"],raab$clusterId[raab$gender=="male"])
wgq.domains.table$male.pct.uci[wgq.domains.table$disability=="wgq.dis.comm"]<-bennett.uci(wgq.domains.table$male.pct[wgq.domains.table$disability=="wgq.dis.comm"],raab$wgq.dis.comm[raab$gender=="male"],raab$vi.denom[raab$gender=="male"],raab$clusterId[raab$gender=="male"])
wgq.domains.table$male.pct.uci[wgq.domains.table$disability=="wgq.dis.self"]<-bennett.uci(wgq.domains.table$male.pct[wgq.domains.table$disability=="wgq.dis.self"],raab$wgq.dis.self[raab$gender=="male"],raab$vi.denom[raab$gender=="male"],raab$clusterId[raab$gender=="male"])

if (!all(is.na(raab$wg_difficulty_upperbody_strength))){
wgq.domains.table$male.pct.uci[wgq.domains.table$disability=="wgq.dis.upbod.str"]<-bennett.uci(wgq.domains.table$male.pct[wgq.domains.table$disability=="wgq.dis.upbod.str"],raab$wgq.dis.mob[raab$gender=="male"],raab$vi.denom[raab$gender=="male"],raab$clusterId[raab$gender=="male"])
wgq.domains.table$male.pct.uci[wgq.domains.table$disability=="wgq.dis.upbod.dex"]<-bennett.uci(wgq.domains.table$male.pct[wgq.domains.table$disability=="wgq.dis.upbod.dex"],raab$wgq.dis.mem[raab$gender=="male"],raab$vi.denom[raab$gender=="male"],raab$clusterId[raab$gender=="male"])
wgq.domains.table$male.pct.uci[wgq.domains.table$disability=="wgq.dis.anx"]<-bennett.uci(wgq.domains.table$male.pct[wgq.domains.table$disability=="wgq.dis.anx"],raab$wgq.dis.comm[raab$gender=="male"],raab$vi.denom[raab$gender=="male"],raab$clusterId[raab$gender=="male"])
wgq.domains.table$male.pct.uci[wgq.domains.table$disability=="wgq.dis.dep"]<-bennett.uci(wgq.domains.table$male.pct[wgq.domains.table$disability=="wgq.dis.dep"],raab$wgq.dis.self[raab$gender=="male"],raab$vi.denom[raab$gender=="male"],raab$clusterId[raab$gender=="male"])
}

wgq.domains.table$male.pct.uci[wgq.domains.table$disability=="wgq.dis.any"]<-bennett.uci(wgq.domains.table$male.pct[wgq.domains.table$disability=="wgq.dis.any"],raab$wgq.dis.any[raab$gender=="male"],raab$vi.denom[raab$gender=="male"],raab$clusterId[raab$gender=="male"])
wgq.domains.table$male.pct.uci[wgq.domains.table$disability=="wgq.dis.nonvi"]<-bennett.uci(wgq.domains.table$male.pct[wgq.domains.table$disability=="wgq.dis.nonvi"],raab$wgq.dis.nonvi[raab$gender=="male"],raab$vi.denom[raab$gender=="male"],raab$clusterId[raab$gender=="male"])

wgq.domains.table$total.pct.lci[wgq.domains.table$disability=="wgq.dis.see"]<-bennett.lci(wgq.domains.table$total.pct[wgq.domains.table$disability=="wgq.dis.see"],raab$wgq.dis.see,raab$vi.denom,raab$clusterId)
wgq.domains.table$total.pct.lci[wgq.domains.table$disability=="wgq.dis.hear"]<-bennett.lci(wgq.domains.table$total.pct[wgq.domains.table$disability=="wgq.dis.hear"],raab$wgq.dis.hear,raab$vi.denom,raab$clusterId)
wgq.domains.table$total.pct.lci[wgq.domains.table$disability=="wgq.dis.mob"]<-bennett.lci(wgq.domains.table$total.pct[wgq.domains.table$disability=="wgq.dis.mob"],raab$wgq.dis.mob,raab$vi.denom,raab$clusterId)
wgq.domains.table$total.pct.lci[wgq.domains.table$disability=="wgq.dis.mem"]<-bennett.lci(wgq.domains.table$total.pct[wgq.domains.table$disability=="wgq.dis.mem"],raab$wgq.dis.mem,raab$vi.denom,raab$clusterId)
wgq.domains.table$total.pct.lci[wgq.domains.table$disability=="wgq.dis.comm"]<-bennett.lci(wgq.domains.table$total.pct[wgq.domains.table$disability=="wgq.dis.comm"],raab$wgq.dis.comm,raab$vi.denom,raab$clusterId)
wgq.domains.table$total.pct.lci[wgq.domains.table$disability=="wgq.dis.self"]<-bennett.lci(wgq.domains.table$total.pct[wgq.domains.table$disability=="wgq.dis.self"],raab$wgq.dis.self,raab$vi.denom,raab$clusterId)

if (!all(is.na(raab$wg_difficulty_upperbody_strength))){
wgq.domains.table$total.pct.lci[wgq.domains.table$disability=="wgq.dis.upbod.str"]<-bennett.lci(wgq.domains.table$total.pct[wgq.domains.table$disability=="wgq.dis.upbod.str"],raab$wgq.dis.mob,raab$vi.denom,raab$clusterId)
wgq.domains.table$total.pct.lci[wgq.domains.table$disability=="wgq.dis.upbod.dex"]<-bennett.lci(wgq.domains.table$total.pct[wgq.domains.table$disability=="wgq.dis.upbod.dex"],raab$wgq.dis.mem,raab$vi.denom,raab$clusterId)
wgq.domains.table$total.pct.lci[wgq.domains.table$disability=="wgq.dis.anx"]<-bennett.lci(wgq.domains.table$total.pct[wgq.domains.table$disability=="wgq.dis.anx"],raab$wgq.dis.comm,raab$vi.denom,raab$clusterId)
wgq.domains.table$total.pct.lci[wgq.domains.table$disability=="wgq.dis.dep"]<-bennett.lci(wgq.domains.table$total.pct[wgq.domains.table$disability=="wgq.dis.dep"],raab$wgq.dis.self,raab$vi.denom,raab$clusterId)
}

wgq.domains.table$total.pct.lci[wgq.domains.table$disability=="wgq.dis.any"]<-bennett.lci(wgq.domains.table$total.pct[wgq.domains.table$disability=="wgq.dis.any"],raab$wgq.dis.any,raab$vi.denom,raab$clusterId)
wgq.domains.table$total.pct.lci[wgq.domains.table$disability=="wgq.dis.nonvi"]<-bennett.lci(wgq.domains.table$total.pct[wgq.domains.table$disability=="wgq.dis.nonvi"],raab$wgq.dis.nonvi,raab$vi.denom,raab$clusterId)

wgq.domains.table$total.pct.uci[wgq.domains.table$disability=="wgq.dis.see"]<-bennett.uci(wgq.domains.table$total.pct[wgq.domains.table$disability=="wgq.dis.see"],raab$wgq.dis.see,raab$vi.denom,raab$clusterId)
wgq.domains.table$total.pct.uci[wgq.domains.table$disability=="wgq.dis.hear"]<-bennett.uci(wgq.domains.table$total.pct[wgq.domains.table$disability=="wgq.dis.hear"],raab$wgq.dis.hear,raab$vi.denom,raab$clusterId)
wgq.domains.table$total.pct.uci[wgq.domains.table$disability=="wgq.dis.mob"]<-bennett.uci(wgq.domains.table$total.pct[wgq.domains.table$disability=="wgq.dis.mob"],raab$wgq.dis.mob,raab$vi.denom,raab$clusterId)
wgq.domains.table$total.pct.uci[wgq.domains.table$disability=="wgq.dis.mem"]<-bennett.uci(wgq.domains.table$total.pct[wgq.domains.table$disability=="wgq.dis.mem"],raab$wgq.dis.mem,raab$vi.denom,raab$clusterId)
wgq.domains.table$total.pct.uci[wgq.domains.table$disability=="wgq.dis.comm"]<-bennett.uci(wgq.domains.table$total.pct[wgq.domains.table$disability=="wgq.dis.comm"],raab$wgq.dis.comm,raab$vi.denom,raab$clusterId)
wgq.domains.table$total.pct.uci[wgq.domains.table$disability=="wgq.dis.self"]<-bennett.uci(wgq.domains.table$total.pct[wgq.domains.table$disability=="wgq.dis.self"],raab$wgq.dis.self,raab$vi.denom,raab$clusterId)

if (!all(is.na(raab$wg_difficulty_upperbody_strength))){
wgq.domains.table$total.pct.uci[wgq.domains.table$disability=="wgq.dis.upbod.str"]<-bennett.uci(wgq.domains.table$total.pct[wgq.domains.table$disability=="wgq.dis.upbod.str"],raab$wgq.dis.mob,raab$vi.denom,raab$clusterId)
wgq.domains.table$total.pct.uci[wgq.domains.table$disability=="wgq.dis.upbod.dex"]<-bennett.uci(wgq.domains.table$total.pct[wgq.domains.table$disability=="wgq.dis.upbod.dex"],raab$wgq.dis.mem,raab$vi.denom,raab$clusterId)
wgq.domains.table$total.pct.uci[wgq.domains.table$disability=="wgq.dis.anx"]<-bennett.uci(wgq.domains.table$total.pct[wgq.domains.table$disability=="wgq.dis.anx"],raab$wgq.dis.comm,raab$vi.denom,raab$clusterId)
wgq.domains.table$total.pct.uci[wgq.domains.table$disability=="wgq.dis.dep"]<-bennett.uci(wgq.domains.table$total.pct[wgq.domains.table$disability=="wgq.dis.dep"],raab$wgq.dis.self,raab$vi.denom,raab$clusterId)
}

wgq.domains.table$total.pct.uci[wgq.domains.table$disability=="wgq.dis.any"]<-bennett.uci(wgq.domains.table$total.pct[wgq.domains.table$disability=="wgq.dis.any"],raab$wgq.dis.any,raab$vi.denom,raab$clusterId)
wgq.domains.table$total.pct.uci[wgq.domains.table$disability=="wgq.dis.nonvi"]<-bennett.uci(wgq.domains.table$total.pct[wgq.domains.table$disability=="wgq.dis.nonvi"],raab$wgq.dis.nonvi,raab$vi.denom,raab$clusterId)

wgq.domains.table$female.adj.pct[wgq.domains.table$disability=="wgq.dis.see"]<-prop.age.adjust(female.subpop,raab[raab$gender=="female",],raab$wgq.dis.see[raab$gender=="female"],raab$vi.denom[raab$gender=="female"])
wgq.domains.table$female.adj.pct[wgq.domains.table$disability=="wgq.dis.hear"]<-prop.age.adjust(female.subpop,raab[raab$gender=="female",],raab$wgq.dis.hear[raab$gender=="female"],raab$vi.denom[raab$gender=="female"])
wgq.domains.table$female.adj.pct[wgq.domains.table$disability=="wgq.dis.mob"]<-prop.age.adjust(female.subpop,raab[raab$gender=="female",],raab$wgq.dis.mob[raab$gender=="female"],raab$vi.denom[raab$gender=="female"])
wgq.domains.table$female.adj.pct[wgq.domains.table$disability=="wgq.dis.mem"]<-prop.age.adjust(female.subpop,raab[raab$gender=="female",],raab$wgq.dis.mem[raab$gender=="female"],raab$vi.denom[raab$gender=="female"])
wgq.domains.table$female.adj.pct[wgq.domains.table$disability=="wgq.dis.comm"]<-prop.age.adjust(female.subpop,raab[raab$gender=="female",],raab$wgq.dis.comm[raab$gender=="female"],raab$vi.denom[raab$gender=="female"])
wgq.domains.table$female.adj.pct[wgq.domains.table$disability=="wgq.dis.self"]<-prop.age.adjust(female.subpop,raab[raab$gender=="female",],raab$wgq.dis.self[raab$gender=="female"],raab$vi.denom[raab$gender=="female"])

if (!all(is.na(raab$wg_difficulty_upperbody_strength))){
wgq.domains.table$female.adj.pct[wgq.domains.table$disability=="wgq.dis.upbod.str"]<-prop.age.adjust(female.subpop,raab[raab$gender=="female",],raab$wgq.dis.upbod.str[raab$gender=="female"],raab$vi.denom[raab$gender=="female"])
wgq.domains.table$female.adj.pct[wgq.domains.table$disability=="wgq.dis.upbod.dex"]<-prop.age.adjust(female.subpop,raab[raab$gender=="female",],raab$wgq.dis.upbod.dex[raab$gender=="female"],raab$vi.denom[raab$gender=="female"])
wgq.domains.table$female.adj.pct[wgq.domains.table$disability=="wgq.dis.anx"]<-prop.age.adjust(female.subpop,raab[raab$gender=="female",],raab$wgq.dis.anx[raab$gender=="female"],raab$vi.denom[raab$gender=="female"])
wgq.domains.table$female.adj.pct[wgq.domains.table$disability=="wgq.dis.dep"]<-prop.age.adjust(female.subpop,raab[raab$gender=="female",],raab$wgq.dis.dep[raab$gender=="female"],raab$vi.denom[raab$gender=="female"])
}

wgq.domains.table$female.adj.pct[wgq.domains.table$disability=="wgq.dis.any"]<-prop.age.adjust(female.subpop,raab[raab$gender=="female",],raab$wgq.dis.any[raab$gender=="female"],raab$vi.denom[raab$gender=="female"])
wgq.domains.table$female.adj.pct[wgq.domains.table$disability=="wgq.dis.nonvi"]<-prop.age.adjust(female.subpop,raab[raab$gender=="female",],raab$wgq.dis.nonvi[raab$gender=="female"],raab$vi.denom[raab$gender=="female"])

wgq.domains.table$male.adj.pct[wgq.domains.table$disability=="wgq.dis.see"]<-prop.age.adjust(male.subpop,raab[raab$gender=="male",],raab$wgq.dis.see[raab$gender=="male"],raab$vi.denom[raab$gender=="male"])
wgq.domains.table$male.adj.pct[wgq.domains.table$disability=="wgq.dis.hear"]<-prop.age.adjust(male.subpop,raab[raab$gender=="male",],raab$wgq.dis.hear[raab$gender=="male"],raab$vi.denom[raab$gender=="male"])
wgq.domains.table$male.adj.pct[wgq.domains.table$disability=="wgq.dis.mob"]<-prop.age.adjust(male.subpop,raab[raab$gender=="male",],raab$wgq.dis.mob[raab$gender=="male"],raab$vi.denom[raab$gender=="male"])
wgq.domains.table$male.adj.pct[wgq.domains.table$disability=="wgq.dis.mem"]<-prop.age.adjust(male.subpop,raab[raab$gender=="male",],raab$wgq.dis.mem[raab$gender=="male"],raab$vi.denom[raab$gender=="male"])
wgq.domains.table$male.adj.pct[wgq.domains.table$disability=="wgq.dis.comm"]<-prop.age.adjust(male.subpop,raab[raab$gender=="male",],raab$wgq.dis.comm[raab$gender=="male"],raab$vi.denom[raab$gender=="male"])
wgq.domains.table$male.adj.pct[wgq.domains.table$disability=="wgq.dis.self"]<-prop.age.adjust(male.subpop,raab[raab$gender=="male",],raab$wgq.dis.self[raab$gender=="male"],raab$vi.denom[raab$gender=="male"])

if (!all(is.na(raab$wg_difficulty_upperbody_strength))){
wgq.domains.table$male.adj.pct[wgq.domains.table$disability=="wgq.dis.upbod.str"]<-prop.age.adjust(male.subpop,raab[raab$gender=="male",],raab$wgq.dis.upbod.str[raab$gender=="male"],raab$vi.denom[raab$gender=="male"])
wgq.domains.table$male.adj.pct[wgq.domains.table$disability=="wgq.dis.upbod.dex"]<-prop.age.adjust(male.subpop,raab[raab$gender=="male",],raab$wgq.dis.upbod.dex[raab$gender=="male"],raab$vi.denom[raab$gender=="male"])
wgq.domains.table$male.adj.pct[wgq.domains.table$disability=="wgq.dis.anx"]<-prop.age.adjust(male.subpop,raab[raab$gender=="male",],raab$wgq.dis.anx[raab$gender=="male"],raab$vi.denom[raab$gender=="male"])
wgq.domains.table$male.adj.pct[wgq.domains.table$disability=="wgq.dis.dep"]<-prop.age.adjust(male.subpop,raab[raab$gender=="male",],raab$wgq.dis.dep[raab$gender=="male"],raab$vi.denom[raab$gender=="male"])
}

wgq.domains.table$male.adj.pct[wgq.domains.table$disability=="wgq.dis.any"]<-prop.age.adjust(male.subpop,raab[raab$gender=="male",],raab$wgq.dis.any[raab$gender=="male"],raab$vi.denom[raab$gender=="male"])
wgq.domains.table$male.adj.pct[wgq.domains.table$disability=="wgq.dis.nonvi"]<-prop.age.adjust(male.subpop,raab[raab$gender=="male",],raab$wgq.dis.nonvi[raab$gender=="male"],raab$vi.denom[raab$gender=="male"])

wgq.domains.table$total.adj.pct[wgq.domains.table$disability=="wgq.dis.see"]<-prop.age.sex.adjust(popfives,raab,raab$wgq.dis.see,raab$vi.denom)
wgq.domains.table$total.adj.pct[wgq.domains.table$disability=="wgq.dis.hear"]<-prop.age.sex.adjust(popfives,raab,raab$wgq.dis.hear,raab$vi.denom)
wgq.domains.table$total.adj.pct[wgq.domains.table$disability=="wgq.dis.mob"]<-prop.age.sex.adjust(popfives,raab,raab$wgq.dis.mob,raab$vi.denom)
wgq.domains.table$total.adj.pct[wgq.domains.table$disability=="wgq.dis.mem"]<-prop.age.sex.adjust(popfives,raab,raab$wgq.dis.mem,raab$vi.denom)
wgq.domains.table$total.adj.pct[wgq.domains.table$disability=="wgq.dis.comm"]<-prop.age.sex.adjust(popfives,raab,raab$wgq.dis.comm,raab$vi.denom)
wgq.domains.table$total.adj.pct[wgq.domains.table$disability=="wgq.dis.self"]<-prop.age.sex.adjust(popfives,raab,raab$wgq.dis.self,raab$vi.denom)

if (!all(is.na(raab$wg_difficulty_upperbody_strength))){
wgq.domains.table$total.adj.pct[wgq.domains.table$disability=="wgq.dis.upbod.str"]<-prop.age.sex.adjust(popfives,raab,raab$wgq.dis.upbod.str,raab$vi.denom)
wgq.domains.table$total.adj.pct[wgq.domains.table$disability=="wgq.dis.upbod.dex"]<-prop.age.sex.adjust(popfives,raab,raab$wgq.dis.upbod.dex,raab$vi.denom)
wgq.domains.table$total.adj.pct[wgq.domains.table$disability=="wgq.dis.anx"]<-prop.age.sex.adjust(popfives,raab,raab$wgq.dis.anx,raab$vi.denom)
wgq.domains.table$total.adj.pct[wgq.domains.table$disability=="wgq.dis.dep"]<-prop.age.sex.adjust(popfives,raab,raab$wgq.dis.dep,raab$vi.denom)
}

wgq.domains.table$total.adj.pct[wgq.domains.table$disability=="wgq.dis.any"]<-prop.age.sex.adjust(popfives,raab,raab$wgq.dis.any,raab$vi.denom)
wgq.domains.table$total.adj.pct[wgq.domains.table$disability=="wgq.dis.nonvi"]<-prop.age.sex.adjust(popfives,raab,raab$wgq.dis.nonvi,raab$vi.denom)

wgq.domains.table$female.adj.pct.lci[wgq.domains.table$disability=="wgq.dis.see"]<-bennett.lci(wgq.domains.table$total.adj.pct[wgq.domains.table$disability=="wgq.dis.see"],raab$wgq.dis.see[raab$gender=="female"],raab$vi.denom[raab$gender=="female"],raab$clusterId[raab$gender=="female"])
wgq.domains.table$female.adj.pct.lci[wgq.domains.table$disability=="wgq.dis.hear"]<-bennett.lci(wgq.domains.table$total.adj.pct[wgq.domains.table$disability=="wgq.dis.hear"],raab$wgq.dis.hear[raab$gender=="female"],raab$vi.denom[raab$gender=="female"],raab$clusterId[raab$gender=="female"])
wgq.domains.table$female.adj.pct.lci[wgq.domains.table$disability=="wgq.dis.mob"]<-bennett.lci(wgq.domains.table$total.adj.pct[wgq.domains.table$disability=="wgq.dis.mob"],raab$wgq.dis.mob[raab$gender=="female"],raab$vi.denom[raab$gender=="female"],raab$clusterId[raab$gender=="female"])
wgq.domains.table$female.adj.pct.lci[wgq.domains.table$disability=="wgq.dis.mem"]<-bennett.lci(wgq.domains.table$total.adj.pct[wgq.domains.table$disability=="wgq.dis.mem"],raab$wgq.dis.mem[raab$gender=="female"],raab$vi.denom[raab$gender=="female"],raab$clusterId[raab$gender=="female"])
wgq.domains.table$female.adj.pct.lci[wgq.domains.table$disability=="wgq.dis.comm"]<-bennett.lci(wgq.domains.table$total.adj.pct[wgq.domains.table$disability=="wgq.dis.comm"],raab$wgq.dis.comm[raab$gender=="female"],raab$vi.denom[raab$gender=="female"],raab$clusterId[raab$gender=="female"])
wgq.domains.table$female.adj.pct.lci[wgq.domains.table$disability=="wgq.dis.self"]<-bennett.lci(wgq.domains.table$total.adj.pct[wgq.domains.table$disability=="wgq.dis.self"],raab$wgq.dis.self[raab$gender=="female"],raab$vi.denom[raab$gender=="female"],raab$clusterId[raab$gender=="female"])

if (!all(is.na(raab$wg_difficulty_upperbody_strength))){
wgq.domains.table$female.adj.pct.lci[wgq.domains.table$disability=="wgq.dis.upbod.str"]<-bennett.lci(wgq.domains.table$total.adj.pct[wgq.domains.table$disability=="wgq.dis.upbod.str"],raab$wgq.dis.mob[raab$gender=="female"],raab$vi.denom[raab$gender=="female"],raab$clusterId[raab$gender=="female"])
wgq.domains.table$female.adj.pct.lci[wgq.domains.table$disability=="wgq.dis.upbod.dex"]<-bennett.lci(wgq.domains.table$total.adj.pct[wgq.domains.table$disability=="wgq.dis.upbod.dex"],raab$wgq.dis.mem[raab$gender=="female"],raab$vi.denom[raab$gender=="female"],raab$clusterId[raab$gender=="female"])
wgq.domains.table$female.adj.pct.lci[wgq.domains.table$disability=="wgq.dis.anx"]<-bennett.lci(wgq.domains.table$total.adj.pct[wgq.domains.table$disability=="wgq.dis.anx"],raab$wgq.dis.comm[raab$gender=="female"],raab$vi.denom[raab$gender=="female"],raab$clusterId[raab$gender=="female"])
wgq.domains.table$female.adj.pct.lci[wgq.domains.table$disability=="wgq.dis.dep"]<-bennett.lci(wgq.domains.table$total.adj.pct[wgq.domains.table$disability=="wgq.dis.dep"],raab$wgq.dis.self[raab$gender=="female"],raab$vi.denom[raab$gender=="female"],raab$clusterId[raab$gender=="female"])
}

wgq.domains.table$female.adj.pct.lci[wgq.domains.table$disability=="wgq.dis.any"]<-bennett.lci(wgq.domains.table$total.adj.pct[wgq.domains.table$disability=="wgq.dis.any"],raab$wgq.dis.any[raab$gender=="female"],raab$vi.denom[raab$gender=="female"],raab$clusterId[raab$gender=="female"])
wgq.domains.table$female.adj.pct.lci[wgq.domains.table$disability=="wgq.dis.nonvi"]<-bennett.lci(wgq.domains.table$total.adj.pct[wgq.domains.table$disability=="wgq.dis.nonvi"],raab$wgq.dis.nonvi[raab$gender=="female"],raab$vi.denom[raab$gender=="female"],raab$clusterId[raab$gender=="female"])

wgq.domains.table$female.adj.pct.uci[wgq.domains.table$disability=="wgq.dis.see"]<-bennett.uci(wgq.domains.table$total.adj.pct[wgq.domains.table$disability=="wgq.dis.see"],raab$wgq.dis.see[raab$gender=="female"],raab$vi.denom[raab$gender=="female"],raab$clusterId[raab$gender=="female"])
wgq.domains.table$female.adj.pct.uci[wgq.domains.table$disability=="wgq.dis.hear"]<-bennett.uci(wgq.domains.table$total.adj.pct[wgq.domains.table$disability=="wgq.dis.hear"],raab$wgq.dis.hear[raab$gender=="female"],raab$vi.denom[raab$gender=="female"],raab$clusterId[raab$gender=="female"])
wgq.domains.table$female.adj.pct.uci[wgq.domains.table$disability=="wgq.dis.mob"]<-bennett.uci(wgq.domains.table$total.adj.pct[wgq.domains.table$disability=="wgq.dis.mob"],raab$wgq.dis.mob[raab$gender=="female"],raab$vi.denom[raab$gender=="female"],raab$clusterId[raab$gender=="female"])
wgq.domains.table$female.adj.pct.uci[wgq.domains.table$disability=="wgq.dis.mem"]<-bennett.uci(wgq.domains.table$total.adj.pct[wgq.domains.table$disability=="wgq.dis.mem"],raab$wgq.dis.mem[raab$gender=="female"],raab$vi.denom[raab$gender=="female"],raab$clusterId[raab$gender=="female"])
wgq.domains.table$female.adj.pct.uci[wgq.domains.table$disability=="wgq.dis.comm"]<-bennett.uci(wgq.domains.table$total.adj.pct[wgq.domains.table$disability=="wgq.dis.comm"],raab$wgq.dis.comm[raab$gender=="female"],raab$vi.denom[raab$gender=="female"],raab$clusterId[raab$gender=="female"])
wgq.domains.table$female.adj.pct.uci[wgq.domains.table$disability=="wgq.dis.self"]<-bennett.uci(wgq.domains.table$total.adj.pct[wgq.domains.table$disability=="wgq.dis.self"],raab$wgq.dis.self[raab$gender=="female"],raab$vi.denom[raab$gender=="female"],raab$clusterId[raab$gender=="female"])

if (!all(is.na(raab$wg_difficulty_upperbody_strength))){
wgq.domains.table$female.adj.pct.uci[wgq.domains.table$disability=="wgq.dis.upbod.str"]<-bennett.uci(wgq.domains.table$total.adj.pct[wgq.domains.table$disability=="wgq.dis.upbod.str"],raab$wgq.dis.mob[raab$gender=="female"],raab$vi.denom[raab$gender=="female"],raab$clusterId[raab$gender=="female"])
wgq.domains.table$female.adj.pct.uci[wgq.domains.table$disability=="wgq.dis.upbod.dex"]<-bennett.uci(wgq.domains.table$total.adj.pct[wgq.domains.table$disability=="wgq.dis.upbod.dex"],raab$wgq.dis.mem[raab$gender=="female"],raab$vi.denom[raab$gender=="female"],raab$clusterId[raab$gender=="female"])
wgq.domains.table$female.adj.pct.uci[wgq.domains.table$disability=="wgq.dis.anx"]<-bennett.uci(wgq.domains.table$total.adj.pct[wgq.domains.table$disability=="wgq.dis.anx"],raab$wgq.dis.comm[raab$gender=="female"],raab$vi.denom[raab$gender=="female"],raab$clusterId[raab$gender=="female"])
wgq.domains.table$female.adj.pct.uci[wgq.domains.table$disability=="wgq.dis.dep"]<-bennett.uci(wgq.domains.table$total.adj.pct[wgq.domains.table$disability=="wgq.dis.dep"],raab$wgq.dis.self[raab$gender=="female"],raab$vi.denom[raab$gender=="female"],raab$clusterId[raab$gender=="female"])
}

wgq.domains.table$female.adj.pct.uci[wgq.domains.table$disability=="wgq.dis.any"]<-bennett.uci(wgq.domains.table$total.adj.pct[wgq.domains.table$disability=="wgq.dis.any"],raab$wgq.dis.any[raab$gender=="female"],raab$vi.denom[raab$gender=="female"],raab$clusterId[raab$gender=="female"])
wgq.domains.table$female.adj.pct.uci[wgq.domains.table$disability=="wgq.dis.nonvi"]<-bennett.uci(wgq.domains.table$total.adj.pct[wgq.domains.table$disability=="wgq.dis.nonvi"],raab$wgq.dis.nonvi[raab$gender=="female"],raab$vi.denom[raab$gender=="female"],raab$clusterId[raab$gender=="female"])

wgq.domains.table$male.adj.pct.lci[wgq.domains.table$disability=="wgq.dis.see"]<-bennett.lci(wgq.domains.table$total.adj.pct[wgq.domains.table$disability=="wgq.dis.see"],raab$wgq.dis.see[raab$gender=="male"],raab$vi.denom[raab$gender=="male"],raab$clusterId[raab$gender=="male"])
wgq.domains.table$male.adj.pct.lci[wgq.domains.table$disability=="wgq.dis.hear"]<-bennett.lci(wgq.domains.table$total.adj.pct[wgq.domains.table$disability=="wgq.dis.hear"],raab$wgq.dis.hear[raab$gender=="male"],raab$vi.denom[raab$gender=="male"],raab$clusterId[raab$gender=="male"])
wgq.domains.table$male.adj.pct.lci[wgq.domains.table$disability=="wgq.dis.mob"]<-bennett.lci(wgq.domains.table$total.adj.pct[wgq.domains.table$disability=="wgq.dis.mob"],raab$wgq.dis.mob[raab$gender=="male"],raab$vi.denom[raab$gender=="male"],raab$clusterId[raab$gender=="male"])
wgq.domains.table$male.adj.pct.lci[wgq.domains.table$disability=="wgq.dis.mem"]<-bennett.lci(wgq.domains.table$total.adj.pct[wgq.domains.table$disability=="wgq.dis.mem"],raab$wgq.dis.mem[raab$gender=="male"],raab$vi.denom[raab$gender=="male"],raab$clusterId[raab$gender=="male"])
wgq.domains.table$male.adj.pct.lci[wgq.domains.table$disability=="wgq.dis.comm"]<-bennett.lci(wgq.domains.table$total.adj.pct[wgq.domains.table$disability=="wgq.dis.comm"],raab$wgq.dis.comm[raab$gender=="male"],raab$vi.denom[raab$gender=="male"],raab$clusterId[raab$gender=="male"])
wgq.domains.table$male.adj.pct.lci[wgq.domains.table$disability=="wgq.dis.self"]<-bennett.lci(wgq.domains.table$total.adj.pct[wgq.domains.table$disability=="wgq.dis.self"],raab$wgq.dis.self[raab$gender=="male"],raab$vi.denom[raab$gender=="male"],raab$clusterId[raab$gender=="male"])

if (!all(is.na(raab$wg_difficulty_upperbody_strength))){
wgq.domains.table$male.adj.pct.lci[wgq.domains.table$disability=="wgq.dis.upbod.str"]<-bennett.lci(wgq.domains.table$total.adj.pct[wgq.domains.table$disability=="wgq.dis.upbod.str"],raab$wgq.dis.mob[raab$gender=="male"],raab$vi.denom[raab$gender=="male"],raab$clusterId[raab$gender=="male"])
wgq.domains.table$male.adj.pct.lci[wgq.domains.table$disability=="wgq.dis.upbod.dex"]<-bennett.lci(wgq.domains.table$total.adj.pct[wgq.domains.table$disability=="wgq.dis.upbod.dex"],raab$wgq.dis.mem[raab$gender=="male"],raab$vi.denom[raab$gender=="male"],raab$clusterId[raab$gender=="male"])
wgq.domains.table$male.adj.pct.lci[wgq.domains.table$disability=="wgq.dis.anx"]<-bennett.lci(wgq.domains.table$total.adj.pct[wgq.domains.table$disability=="wgq.dis.anx"],raab$wgq.dis.comm[raab$gender=="male"],raab$vi.denom[raab$gender=="male"],raab$clusterId[raab$gender=="male"])
wgq.domains.table$male.adj.pct.lci[wgq.domains.table$disability=="wgq.dis.dep"]<-bennett.lci(wgq.domains.table$total.adj.pct[wgq.domains.table$disability=="wgq.dis.dep"],raab$wgq.dis.self[raab$gender=="male"],raab$vi.denom[raab$gender=="male"],raab$clusterId[raab$gender=="male"])
}

wgq.domains.table$male.adj.pct.lci[wgq.domains.table$disability=="wgq.dis.any"]<-bennett.lci(wgq.domains.table$total.adj.pct[wgq.domains.table$disability=="wgq.dis.any"],raab$wgq.dis.any[raab$gender=="male"],raab$vi.denom[raab$gender=="male"],raab$clusterId[raab$gender=="male"])
wgq.domains.table$male.adj.pct.lci[wgq.domains.table$disability=="wgq.dis.nonvi"]<-bennett.lci(wgq.domains.table$total.adj.pct[wgq.domains.table$disability=="wgq.dis.nonvi"],raab$wgq.dis.nonvi[raab$gender=="male"],raab$vi.denom[raab$gender=="male"],raab$clusterId[raab$gender=="male"])

wgq.domains.table$male.adj.pct.uci[wgq.domains.table$disability=="wgq.dis.see"]<-bennett.uci(wgq.domains.table$total.adj.pct[wgq.domains.table$disability=="wgq.dis.see"],raab$wgq.dis.see[raab$gender=="male"],raab$vi.denom[raab$gender=="male"],raab$clusterId[raab$gender=="male"])
wgq.domains.table$male.adj.pct.uci[wgq.domains.table$disability=="wgq.dis.hear"]<-bennett.uci(wgq.domains.table$total.adj.pct[wgq.domains.table$disability=="wgq.dis.hear"],raab$wgq.dis.hear[raab$gender=="male"],raab$vi.denom[raab$gender=="male"],raab$clusterId[raab$gender=="male"])
wgq.domains.table$male.adj.pct.uci[wgq.domains.table$disability=="wgq.dis.mob"]<-bennett.uci(wgq.domains.table$total.adj.pct[wgq.domains.table$disability=="wgq.dis.mob"],raab$wgq.dis.mob[raab$gender=="male"],raab$vi.denom[raab$gender=="male"],raab$clusterId[raab$gender=="male"])
wgq.domains.table$male.adj.pct.uci[wgq.domains.table$disability=="wgq.dis.mem"]<-bennett.uci(wgq.domains.table$total.adj.pct[wgq.domains.table$disability=="wgq.dis.mem"],raab$wgq.dis.mem[raab$gender=="male"],raab$vi.denom[raab$gender=="male"],raab$clusterId[raab$gender=="male"])
wgq.domains.table$male.adj.pct.uci[wgq.domains.table$disability=="wgq.dis.comm"]<-bennett.uci(wgq.domains.table$total.adj.pct[wgq.domains.table$disability=="wgq.dis.comm"],raab$wgq.dis.comm[raab$gender=="male"],raab$vi.denom[raab$gender=="male"],raab$clusterId[raab$gender=="male"])
wgq.domains.table$male.adj.pct.uci[wgq.domains.table$disability=="wgq.dis.self"]<-bennett.uci(wgq.domains.table$total.adj.pct[wgq.domains.table$disability=="wgq.dis.self"],raab$wgq.dis.self[raab$gender=="male"],raab$vi.denom[raab$gender=="male"],raab$clusterId[raab$gender=="male"])

if (!all(is.na(raab$wg_difficulty_upperbody_strength))){
wgq.domains.table$male.adj.pct.uci[wgq.domains.table$disability=="wgq.dis.upbod.str"]<-bennett.uci(wgq.domains.table$total.adj.pct[wgq.domains.table$disability=="wgq.dis.upbod.str"],raab$wgq.dis.mob[raab$gender=="male"],raab$vi.denom[raab$gender=="male"],raab$clusterId[raab$gender=="male"])
wgq.domains.table$male.adj.pct.uci[wgq.domains.table$disability=="wgq.dis.upbod.dex"]<-bennett.uci(wgq.domains.table$total.adj.pct[wgq.domains.table$disability=="wgq.dis.upbod.dex"],raab$wgq.dis.mem[raab$gender=="male"],raab$vi.denom[raab$gender=="male"],raab$clusterId[raab$gender=="male"])
wgq.domains.table$male.adj.pct.uci[wgq.domains.table$disability=="wgq.dis.anx"]<-bennett.uci(wgq.domains.table$total.adj.pct[wgq.domains.table$disability=="wgq.dis.anx"],raab$wgq.dis.comm[raab$gender=="male"],raab$vi.denom[raab$gender=="male"],raab$clusterId[raab$gender=="male"])
wgq.domains.table$male.adj.pct.uci[wgq.domains.table$disability=="wgq.dis.dep"]<-bennett.uci(wgq.domains.table$total.adj.pct[wgq.domains.table$disability=="wgq.dis.dep"],raab$wgq.dis.self[raab$gender=="male"],raab$vi.denom[raab$gender=="male"],raab$clusterId[raab$gender=="male"])
}

wgq.domains.table$male.adj.pct.uci[wgq.domains.table$disability=="wgq.dis.any"]<-bennett.uci(wgq.domains.table$total.adj.pct[wgq.domains.table$disability=="wgq.dis.any"],raab$wgq.dis.any[raab$gender=="male"],raab$vi.denom[raab$gender=="male"],raab$clusterId[raab$gender=="male"])
wgq.domains.table$male.adj.pct.uci[wgq.domains.table$disability=="wgq.dis.nonvi"]<-bennett.uci(wgq.domains.table$total.adj.pct[wgq.domains.table$disability=="wgq.dis.nonvi"],raab$wgq.dis.nonvi[raab$gender=="male"],raab$vi.denom[raab$gender=="male"],raab$clusterId[raab$gender=="male"])

wgq.domains.table$total.adj.pct.lci[wgq.domains.table$disability=="wgq.dis.see"]<-bennett.lci(wgq.domains.table$total.adj.pct[wgq.domains.table$disability=="wgq.dis.see"],raab$wgq.dis.see,raab$vi.denom,raab$clusterId)
wgq.domains.table$total.adj.pct.lci[wgq.domains.table$disability=="wgq.dis.hear"]<-bennett.lci(wgq.domains.table$total.adj.pct[wgq.domains.table$disability=="wgq.dis.hear"],raab$wgq.dis.hear,raab$vi.denom,raab$clusterId)
wgq.domains.table$total.adj.pct.lci[wgq.domains.table$disability=="wgq.dis.mob"]<-bennett.lci(wgq.domains.table$total.adj.pct[wgq.domains.table$disability=="wgq.dis.mob"],raab$wgq.dis.mob,raab$vi.denom,raab$clusterId)
wgq.domains.table$total.adj.pct.lci[wgq.domains.table$disability=="wgq.dis.mem"]<-bennett.lci(wgq.domains.table$total.adj.pct[wgq.domains.table$disability=="wgq.dis.mem"],raab$wgq.dis.mem,raab$vi.denom,raab$clusterId)
wgq.domains.table$total.adj.pct.lci[wgq.domains.table$disability=="wgq.dis.comm"]<-bennett.lci(wgq.domains.table$total.adj.pct[wgq.domains.table$disability=="wgq.dis.comm"],raab$wgq.dis.comm,raab$vi.denom,raab$clusterId)
wgq.domains.table$total.adj.pct.lci[wgq.domains.table$disability=="wgq.dis.self"]<-bennett.lci(wgq.domains.table$total.adj.pct[wgq.domains.table$disability=="wgq.dis.self"],raab$wgq.dis.self,raab$vi.denom,raab$clusterId)

if (!all(is.na(raab$wg_difficulty_upperbody_strength))){
wgq.domains.table$total.adj.pct.lci[wgq.domains.table$disability=="wgq.dis.upbod.str"]<-bennett.lci(wgq.domains.table$total.adj.pct[wgq.domains.table$disability=="wgq.dis.upbod.str"],raab$wgq.dis.mob,raab$vi.denom,raab$clusterId)
wgq.domains.table$total.adj.pct.lci[wgq.domains.table$disability=="wgq.dis.upbod.dex"]<-bennett.lci(wgq.domains.table$total.adj.pct[wgq.domains.table$disability=="wgq.dis.upbod.dex"],raab$wgq.dis.mem,raab$vi.denom,raab$clusterId)
wgq.domains.table$total.adj.pct.lci[wgq.domains.table$disability=="wgq.dis.anx"]<-bennett.lci(wgq.domains.table$total.adj.pct[wgq.domains.table$disability=="wgq.dis.anx"],raab$wgq.dis.comm,raab$vi.denom,raab$clusterId)
wgq.domains.table$total.adj.pct.lci[wgq.domains.table$disability=="wgq.dis.dep"]<-bennett.lci(wgq.domains.table$total.adj.pct[wgq.domains.table$disability=="wgq.dis.dep"],raab$wgq.dis.self,raab$vi.denom,raab$clusterId)
}

wgq.domains.table$total.adj.pct.lci[wgq.domains.table$disability=="wgq.dis.any"]<-bennett.lci(wgq.domains.table$total.adj.pct[wgq.domains.table$disability=="wgq.dis.any"],raab$wgq.dis.any,raab$vi.denom,raab$clusterId)
wgq.domains.table$total.adj.pct.lci[wgq.domains.table$disability=="wgq.dis.nonvi"]<-bennett.lci(wgq.domains.table$total.adj.pct[wgq.domains.table$disability=="wgq.dis.nonvi"],raab$wgq.dis.nonvi,raab$vi.denom,raab$clusterId)

wgq.domains.table$total.adj.pct.uci[wgq.domains.table$disability=="wgq.dis.see"]<-bennett.uci(wgq.domains.table$total.adj.pct[wgq.domains.table$disability=="wgq.dis.see"],raab$wgq.dis.see,raab$vi.denom,raab$clusterId)
wgq.domains.table$total.adj.pct.uci[wgq.domains.table$disability=="wgq.dis.hear"]<-bennett.uci(wgq.domains.table$total.adj.pct[wgq.domains.table$disability=="wgq.dis.hear"],raab$wgq.dis.hear,raab$vi.denom,raab$clusterId)
wgq.domains.table$total.adj.pct.uci[wgq.domains.table$disability=="wgq.dis.mob"]<-bennett.uci(wgq.domains.table$total.adj.pct[wgq.domains.table$disability=="wgq.dis.mob"],raab$wgq.dis.mob,raab$vi.denom,raab$clusterId)
wgq.domains.table$total.adj.pct.uci[wgq.domains.table$disability=="wgq.dis.mem"]<-bennett.uci(wgq.domains.table$total.adj.pct[wgq.domains.table$disability=="wgq.dis.mem"],raab$wgq.dis.mem,raab$vi.denom,raab$clusterId)
wgq.domains.table$total.adj.pct.uci[wgq.domains.table$disability=="wgq.dis.comm"]<-bennett.uci(wgq.domains.table$total.adj.pct[wgq.domains.table$disability=="wgq.dis.comm"],raab$wgq.dis.comm,raab$vi.denom,raab$clusterId)
wgq.domains.table$total.adj.pct.uci[wgq.domains.table$disability=="wgq.dis.self"]<-bennett.uci(wgq.domains.table$total.adj.pct[wgq.domains.table$disability=="wgq.dis.self"],raab$wgq.dis.self,raab$vi.denom,raab$clusterId)

if (!all(is.na(raab$wg_difficulty_upperbody_strength))){
wgq.domains.table$total.adj.pct.uci[wgq.domains.table$disability=="wgq.dis.upbod.str"]<-bennett.uci(wgq.domains.table$total.adj.pct[wgq.domains.table$disability=="wgq.dis.upbod.str"],raab$wgq.dis.mob,raab$vi.denom,raab$clusterId)
wgq.domains.table$total.adj.pct.uci[wgq.domains.table$disability=="wgq.dis.upbod.dex"]<-bennett.uci(wgq.domains.table$total.adj.pct[wgq.domains.table$disability=="wgq.dis.upbod.dex"],raab$wgq.dis.mem,raab$vi.denom,raab$clusterId)
wgq.domains.table$total.adj.pct.uci[wgq.domains.table$disability=="wgq.dis.anx"]<-bennett.uci(wgq.domains.table$total.adj.pct[wgq.domains.table$disability=="wgq.dis.anx"],raab$wgq.dis.comm,raab$vi.denom,raab$clusterId)
wgq.domains.table$total.adj.pct.uci[wgq.domains.table$disability=="wgq.dis.dep"]<-bennett.uci(wgq.domains.table$total.adj.pct[wgq.domains.table$disability=="wgq.dis.dep"],raab$wgq.dis.self,raab$vi.denom,raab$clusterId)
}

wgq.domains.table$total.adj.pct.uci[wgq.domains.table$disability=="wgq.dis.any"]<-bennett.uci(wgq.domains.table$total.adj.pct[wgq.domains.table$disability=="wgq.dis.any"],raab$wgq.dis.any,raab$vi.denom,raab$clusterId)
wgq.domains.table$total.adj.pct.uci[wgq.domains.table$disability=="wgq.dis.nonvi"]<-bennett.uci(wgq.domains.table$total.adj.pct[wgq.domains.table$disability=="wgq.dis.nonvi"],raab$wgq.dis.nonvi,raab$vi.denom,raab$clusterId)

if (!all(is.na(raab$wg_difficulty_upperbody_strength))){
wgq.domains.table$disability<-as.character(c("Seeing", "Hearing", "Mobility", "Memory", "Communication", "Self care", "Upper body strength", "Upper body dexterity", "Anxiety", "Depression", "Any domain", "Any non-seeing domain"))
} else {
wgq.domains.table$disability<-as.character(c("Seeing", "Hearing", "Mobility", "Memory", "Communication", "Self care", "Any domain", "Any non-seeing domain"))
}

lcis<-grep("lci",names(wgq.domains.table))
ucis<-grep("uci",names(wgq.domains.table))
wgq.domains.table[,lcis][wgq.domains.table[,lcis]<0]<-0
wgq.domains.table[,ucis][wgq.domains.table[,ucis]>1]<-1

cnts <- grep("n",names(wgq.domains.table))
pcts <- grep("pct",names(wgq.domains.table))
wgq.domains.table[,pcts] <- round(wgq.domains.table[pcts]*100,1)
wgq.domains.table[,pcts] <- format(wgq.domains.table[,pcts], nsmall=1)
wgq.domains.table.c<-wgq.domains.table[,c("disability","female.n","female.pct","female.pct.lci","female.pct.uci","male.n","male.pct","male.pct.lci","male.pct.uci","total.n","total.pct","total.pct.lci","total.pct.uci")]
wgq.domains.table.a<-wgq.domains.table[,c("disability","female.adj.pct","female.adj.pct.lci","female.adj.pct.uci","male.adj.pct","male.adj.pct.lci","male.adj.pct.uci","total.adj.pct","total.adj.pct.lci","total.adj.pct.uci")]

wgq.domains.agegroups<-data.frame(age.groups.tens)
wgq.domains.agegroups[,2:21] <- NA

if (!all(is.na(raab$wg_difficulty_upperbody_strength))){
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
} else {
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
                                    "selfcare.pct")
  
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
  }
}  

#Add totals row to bottom of table (for total count of female, male, all)

wgq.domains.agegroups[nrow(wgq.domains.agegroups)+1,2:21]<-colSums(wgq.domains.agegroups[,2:21])
wgq.domains.agegroups[5,1]<-"Total"

pcts <- grep("pct",names(wgq.domains.agegroups))
wgq.domains.agegroups[,pcts] <- round( wgq.domains.agegroups[,pcts] * 100, 1)

cnts<-grep("\\.n",names(wgq.domains.agegroups))
wgq.domains.agegroups[,cnts]<-format( wgq.domains.agegroups[,cnts], big.interval = 3L, big.mark = " ", scientific=F )

