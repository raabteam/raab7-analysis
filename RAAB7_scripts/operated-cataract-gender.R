# New cataract table - operated people (bilateral or unilateral operated, by gender)
# 04.07.22 IM - need to ask Bert how to apply bennett function to this table

# Operated cataract

operated.cat <- c("bilateral.operated", "unilateral.operated")
operated.cat.table <- data.frame(operated.cat)
operated.cat.table[,c(2:13)]<-NA
names(operated.cat.table)<-c("one.or.two",
                            
                            "female.n",
                            "female.pct",
                            "female.pct.lci",
                            "female.pct.uci",
                            
                            "male.n",
                            "male.pct",
                            "male.pct.lci",
                            "male.pct.uci",
                            
                            "total.n",
                            "total.pct",
                            "total.pct.lci",
                            "total.pct.uci")

operated.cat.table$female.n[operated.cat.table$one.or.two=="bilateral.operated"]<-sum(raab$bilat.operated[raab$gender=="female"],na.rm=T)
operated.cat.table$female.n[operated.cat.table$one.or.two=="unilateral.operated"]<-sum(raab$unilat.operated[raab$gender=="female"],na.rm=T)

operated.cat.table$female.pct[operated.cat.table$one.or.two=="bilateral.operated"]<-sum(raab$bilat.operated[raab$gender=="female"],na.rm=T)/sum(raab$vi.denom[raab$gender=="female"],na.rm=T)
operated.cat.table$female.pct[operated.cat.table$one.or.two=="unilateral.operated"]<-sum(raab$unilat.operated[raab$gender=="female"],na.rm=T)/sum(raab$vi.denom[raab$gender=="female"],na.rm=T)

operated.cat.table$male.n[operated.cat.table$one.or.two=="bilateral.operated"]<-sum(raab$bilat.operated[raab$gender=="male"],na.rm=T)
operated.cat.table$male.n[operated.cat.table$one.or.two=="unilateral.operated"]<-sum(raab$unilat.operated[raab$gender=="male"],na.rm=T)

operated.cat.table$male.pct[operated.cat.table$one.or.two=="bilateral.operated"]<-sum(raab$bilat.operated[raab$gender=="male"],na.rm=T)/sum(raab$vi.denom[raab$gender=="male"],na.rm=T)
operated.cat.table$male.pct[operated.cat.table$one.or.two=="unilateral.operated"]<-sum(raab$unilat.operated[raab$gender=="male"],na.rm=T)/sum(raab$vi.denom[raab$gender=="male"],na.rm=T)

operated.cat.table$total.n[operated.cat.table$one.or.two=="bilateral.operated"]<-sum(raab$bilat.operated,na.rm=T)
operated.cat.table$total.n[operated.cat.table$one.or.two=="unilateral.operated"]<-sum(raab$unilat.operated,na.rm=T)

operated.cat.table$total.pct[operated.cat.table$one.or.two=="bilateral.operated"]<-sum(raab$bilat.operated,na.rm=T)/sum(raab$vi.denom,na.rm=T)
operated.cat.table$total.pct[operated.cat.table$one.or.two=="unilateral.operated"]<-sum(raab$unilat.operated,na.rm=T)/sum(raab$vi.denom,na.rm=T)

operated.cat.table[nrow(operated.cat.table)+1,2:7]<-colSums(operated.cat.table[,2:7])
operated.cat.table$one.or.two[3]<-"operated.people"

pcts <- grep("pct",names(operated.cat.table))
operated.cat.table[,pcts] <- round(operated.cat.table[,pcts] * 100, 1)


