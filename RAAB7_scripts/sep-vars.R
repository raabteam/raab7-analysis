# SEP analysis - pilot variables
# 09.08.2022 IM

library(here)
library(tidyverse)

# Add variable creation to RAAB7_report_setup file when necessary
raab <- raab %>% mutate(
  income = case_when(
    (sep_income_sufficiency=="sep_income_sufficiency_borrow" | sep_income_sufficiency=="sep_income_sufficiency_savings") ~1,
    sep_income_sufficiency=="sep_income_sufficiency_enough_just" ~2,
    (sep_income_sufficiency=="sep_income_sufficiency_enough_save" | sep_income_sufficiency=="sep_income_sufficiency_enough_building_savings") ~3, TRUE~0)
                        )

# Sample proportions for subjective food adequacy by gender
Food.Status <- c("sep_food_adequacy_less","sep_food_adequacy_adequate","sep_food_adequacy_more") 

sample.food<-data.frame(Food.Status)
sample.food[,2:7] <- NA
names(sample.food) <- c("Food.Status",
                 
                 "female.n",
                 "female.pct",
                 
                 "male.n",
                 "male.pct",
                 
                 "total.n",
                 "total.pct"
)

for (i in 1:length(Food.Status)) {
  
  sample.food$female.n[i] <- sum(raab$sep_food_adequacy[raab$gender=='female']==Food.Status[i],na.rm=T) 
  sample.food$female.pct[i] <- sum(raab$sep_food_adequacy[raab$gender=='female']==Food.Status[i],na.rm=T) / sum(raab$exam_status[raab$gender=='female']=="exam_status_examined",na.rm=T)
  
  sample.food$male.n[i] <- sum(raab$sep_food_adequacy[raab$gender=='male']==Food.Status[i],na.rm=T) 
  sample.food$male.pct[i] <- sum(raab$sep_food_adequacy[raab$gender=='male']==Food.Status[i],na.rm=T) / sum(raab$exam_status[raab$gender=='male']=="exam_status_examined",na.rm=T)
  
  sample.food$total.n[i] <- sum(raab$sep_food_adequacy==Food.Status[i],na.rm=T)  
  sample.food$total.pct[i] <- sum(raab$sep_food_adequacy==Food.Status[i],na.rm=T) / sum(raab$exam_status=="exam_status_examined",na.rm=T)
  
}

sample.food[nrow(sample.food)+1,2:7]<-colSums(sample.food[,2:7])
sample.food$Food.Status<-as.character(c('Less than adequate', 'Adquate', 'More than adequate', 'Total'))

pcts<-grep("pct",names(sample.food))
sample.food[,pcts]<-round( sample.food[,pcts] * 100, 1 )

cnts<-grep("n",names(sample.food))

# Sample proportions for subjective income sufficiency by gender
Income.Status <- c(1,2,3) 

sample.income<-data.frame(Income.Status)
sample.income[,2:7] <- NA
names(sample.income) <- c("Income.Status",
                        
                        "female.n",
                        "female.pct",
                        
                        "male.n",
                        "male.pct",
                        
                        "total.n",
                        "total.pct"
)

for (i in 1:length(Income.Status)) {
  
  sample.income$female.n[i] <- sum(raab$income[raab$gender=='female']==Income.Status[i],na.rm=T) 
  sample.income$female.pct[i] <- sum(raab$income[raab$gender=='female']==Income.Status[i],na.rm=T) / sum(raab$exam_status[raab$gender=='female']=="exam_status_examined",na.rm=T)
  
  sample.income$male.n[i] <- sum(raab$income[raab$gender=='male']==Income.Status[i],na.rm=T) 
  sample.income$male.pct[i] <- sum(raab$income[raab$gender=='male']==Income.Status[i],na.rm=T) / sum(raab$exam_status[raab$gender=='male']=="exam_status_examined",na.rm=T)
  
  sample.income$total.n[i] <- sum(raab$income==Income.Status[i],na.rm=T)  
  sample.income$total.pct[i] <- sum(raab$income==Income.Status[i],na.rm=T) / sum(raab$exam_status=="exam_status_examined",na.rm=T)
  
}

sample.income[nrow(sample.income)+1,2:7]<-colSums(sample.income[,2:7])
sample.income$Income.Status<-as.character(c('Not enough income', 'Just enough income', 'More than enough income', 'Total'))

pcts<-grep("pct",names(sample.income))
sample.income[,pcts]<-round( sample.income[,pcts] * 100, 1 )

cnts<-grep("n",names(sample.income))

# VI by three levels of subjective income sufficiency
# income var created in setup

income.vi<-data.frame(vi.levels)
income.vi[,2:22] <- NA
names(income.vi) <- c("vi.level",
                 
                 "not.enough.n",
                 "not.enough.pct",
                 "not.enough.pct.lci",
                 "not.enough.pct.uci",
                 "not.enough.adj.pct",
                 "not.enough.adj.pct.lci",
                 "not.enough.adj.pct.uci",
                 
                 "just.enough.n",
                 "just.enough.pct",
                 "just.enough.pct.lci",
                 "just.enough.pct.uci",
                 "just.enough.adj.pct",
                 "just.enough.adj.pct.lci",
                 "just.enough.adj.pct.uci",
                 
                 "more.enough.n",
                 "more.enough.pct",
                 "more.enough.pct.lci",
                 "more.enough.pct.uci",
                 "more.enough.adj.pct",
                 "more.enough.adj.pct.lci",
                 "more.enough.adj.pct.uci")


for (i in 1:length(vi.levels))
{
  
  income.vi$not.enough.n[i]<-sum(raab[raab$income==1,vi.levels[i]],na.rm=T)
  income.vi$just.enough.n[i]<-sum(raab[raab$income==2,vi.levels[i]],na.rm=T)
  income.vi$more.enough.n[i]<-sum(raab[raab$income==3,vi.levels[i]],na.rm=T)
  
  income.vi$not.enough.pct[i]<-sum(raab[raab$income==1,vi.levels[i]],na.rm=T)/sum(raab$vi.denom[raab$income==1],na.rm=T)
  income.vi$just.enough.pct[i]<-sum(raab[raab$income==2,vi.levels[i]],na.rm=T)/sum(raab$vi.denom[raab$income==2],na.rm=T)
  income.vi$more.enough.pct[i]<-sum(raab[raab$income==3,vi.levels[i]],na.rm=T)/sum(raab$vi.denom[raab$income==3],na.rm=T)

  income.vi$not.enough.pct.lci[i]<-bennett.lci(income.vi$not.enough.pct[i],raab[raab$income==1,vi.levels[i]],raab$vi.denom[raab$income==1],raab$clusterNumber[raab$income==1])
  income.vi$just.enough.pct.lci[i]<-bennett.lci(income.vi$just.enough.pct[i],raab[raab$income==2,vi.levels[i]],raab$vi.denom[raab$income==2],raab$clusterNumber[raab$income==2])
  income.vi$more.enough.pct.lci[i]<-bennett.lci(income.vi$more.enough.pct[i],raab[raab$income==3,vi.levels[i]],raab$vi.denom[raab$income==3],raab$clusterNumber[raab$income==3])
  
  income.vi$not.enough.pct.uci[i]<-bennett.uci(income.vi$not.enough.pct[i],raab[raab$income==1,vi.levels[i]],raab$vi.denom[raab$income==1],raab$clusterNumber[raab$income==1])
  income.vi$just.enough.pct.uci[i]<-bennett.uci(income.vi$just.enough.pct[i],raab[raab$income==2,vi.levels[i]],raab$vi.denom[raab$income==2],raab$clusterNumber[raab$income==2])
  income.vi$more.enough.pct.uci[i]<-bennett.uci(income.vi$more.enough.pct[i],raab[raab$income==3,vi.levels[i]],raab$vi.denom[raab$income==3],raab$clusterNumber[raab$income==3])

  income.vi$not.enough.adj.pct[i]<-prop.age.sex.adjust(popfives, raab[raab$income==1,], raab[raab$income==1,vi.levels[i]], raab$vi.denom[raab$income==1])
  income.vi$just.enough.adj.pct[i]<-prop.age.sex.adjust(popfives, raab[raab$income==2,], raab[raab$income==2,vi.levels[i]], raab$vi.denom[raab$income==2])
  income.vi$more.enough.adj.pct[i]<-prop.age.sex.adjust(popfives, raab[raab$income==3,], raab[raab$income==3,vi.levels[i]], raab$vi.denom[raab$income==3])
  
  income.vi$not.enough.adj.pct.lci[i]<-bennett.lci(income.vi$not.enough.adj.pct[i],raab[raab$income==1,vi.levels[i]],raab$vi.denom[raab$income==1],raab$clusterNumber[raab$income==1])
  income.vi$just.enough.adj.pct.lci[i]<-bennett.lci(income.vi$just.enough.adj.pct[i],raab[raab$income==2,vi.levels[i]],raab$vi.denom[raab$income==2],raab$clusterNumber[raab$income==2])
  income.vi$more.enough.adj.pct.lci[i]<-bennett.lci(income.vi$more.enough.adj.pct[i],raab[raab$income==3,vi.levels[i]],raab$vi.denom[raab$income==3],raab$clusterNumber[raab$income==3])
  
  income.vi$not.enough.adj.pct.uci[i]<-bennett.uci(income.vi$not.enough.adj.pct[i],raab[raab$income==1,vi.levels[i]],raab$vi.denom[raab$income==1],raab$clusterNumber[raab$income==1])
  income.vi$just.enough.adj.pct.uci[i]<-bennett.uci(income.vi$just.enough.adj.pct[i],raab[raab$income==2,vi.levels[i]],raab$vi.denom[raab$income==2],raab$clusterNumber[raab$income==2])
  income.vi$more.enough.adj.pct.uci[i]<-bennett.uci(income.vi$more.enough.adj.pct[i],raab[raab$income==3,vi.levels[i]],raab$vi.denom[raab$income==3],raab$clusterNumber[raab$income==3])
  
}

lcis<-grep("lci",names(income.vi))
ucis<-grep("uci",names(income.vi))
income.vi[,lcis][income.vi[,lcis]<0]<-0
income.vi[,ucis][income.vi[,ucis]>1]<-1

pcts<-grep("pct",names(income.vi))
income.vi[,pcts]<-round( income.vi[,pcts] * 100, 1 )
income.vi[,pcts]<-format(income.vi[,pcts], nsmall=1)

# VI by three levels of subjective income sufficiency

food.vi<-data.frame(vi.levels)
food.vi[,2:22] <- NA
names(food.vi) <- c("vi.level",
                      
                      "not.enough.n",
                      "not.enough.pct",
                      "not.enough.pct.lci",
                      "not.enough.pct.uci",
                      "not.enough.adj.pct",
                      "not.enough.adj.pct.lci",
                      "not.enough.adj.pct.uci",
                      
                      "just.enough.n",
                      "just.enough.pct",
                      "just.enough.pct.lci",
                      "just.enough.pct.uci",
                      "just.enough.adj.pct",
                      "just.enough.adj.pct.lci",
                      "just.enough.adj.pct.uci",
                      
                      "more.enough.n",
                      "more.enough.pct",
                      "more.enough.pct.lci",
                      "more.enough.pct.uci",
                      "more.enough.adj.pct",
                      "more.enough.adj.pct.lci",
                      "more.enough.adj.pct.uci")


for (i in 1:length(vi.levels))
{
  
  food.vi$not.enough.n[i]<-sum(raab[raab$sep_food_adequacy=="sep_food_adequacy_less",vi.levels[i]],na.rm=T)
  food.vi$just.enough.n[i]<-sum(raab[raab$sep_food_adequacy=="sep_food_adequacy_adequate",vi.levels[i]],na.rm=T)
  food.vi$more.enough.n[i]<-sum(raab[raab$sep_food_adequacy=="sep_food_adequacy_more",vi.levels[i]],na.rm=T)
  
  food.vi$not.enough.pct[i]<-sum(raab[raab$sep_food_adequacy=="sep_food_adequacy_less",vi.levels[i]],na.rm=T)/sum(raab$vi.denom[raab$sep_food_adequacy=="sep_food_adequacy_less"],na.rm=T)
  food.vi$just.enough.pct[i]<-sum(raab[raab$sep_food_adequacy=="sep_food_adequacy_adequate",vi.levels[i]],na.rm=T)/sum(raab$vi.denom[raab$sep_food_adequacy=="sep_food_adequacy_adequate"],na.rm=T)
  food.vi$more.enough.pct[i]<-sum(raab[raab$sep_food_adequacy=="sep_food_adequacy_more",vi.levels[i]],na.rm=T)/sum(raab$vi.denom[raab$sep_food_adequacy=="sep_food_adequacy_more"],na.rm=T)
  
  food.vi$not.enough.pct.lci[i]<-bennett.lci(food.vi$not.enough.pct[i],raab[raab$sep_food_adequacy=="sep_food_adequacy_less",vi.levels[i]],raab$vi.denom[raab$sep_food_adequacy=="sep_food_adequacy_less"],raab$clusterNumber[raab$sep_food_adequacy=="sep_food_adequacy_less"])
  food.vi$just.enough.pct.lci[i]<-bennett.lci(food.vi$just.enough.pct[i],raab[raab$sep_food_adequacy=="sep_food_adequacy_adequate",vi.levels[i]],raab$vi.denom[raab$sep_food_adequacy=="sep_food_adequacy_adequate"],raab$clusterNumber[raab$sep_food_adequacy=="sep_food_adequacy_adequate"])
  food.vi$more.enough.pct.lci[i]<-bennett.lci(food.vi$more.enough.pct[i],raab[raab$sep_food_adequacy=="sep_food_adequacy_more",vi.levels[i]],raab$vi.denom[raab$sep_food_adequacy=="sep_food_adequacy_more"],raab$clusterNumber[raab$sep_food_adequacy=="sep_food_adequacy_more"])
  
  food.vi$not.enough.pct.uci[i]<-bennett.uci(food.vi$not.enough.pct[i],raab[raab$sep_food_adequacy=="sep_food_adequacy_less",vi.levels[i]],raab$vi.denom[raab$sep_food_adequacy=="sep_food_adequacy_less"],raab$clusterNumber[raab$sep_food_adequacy=="sep_food_adequacy_less"])
  food.vi$just.enough.pct.uci[i]<-bennett.uci(food.vi$just.enough.pct[i],raab[raab$sep_food_adequacy=="sep_food_adequacy_adequate",vi.levels[i]],raab$vi.denom[raab$sep_food_adequacy=="sep_food_adequacy_adequate"],raab$clusterNumber[raab$sep_food_adequacy=="sep_food_adequacy_adequate"])
  food.vi$more.enough.pct.uci[i]<-bennett.uci(food.vi$more.enough.pct[i],raab[raab$sep_food_adequacy=="sep_food_adequacy_more",vi.levels[i]],raab$vi.denom[raab$sep_food_adequacy=="sep_food_adequacy_more"],raab$clusterNumber[raab$sep_food_adequacy=="sep_food_adequacy_more"])
  
  food.vi$not.enough.adj.pct[i]<-prop.age.sex.adjust(popfives, raab[raab$sep_food_adequacy=="sep_food_adequacy_less",], raab[raab$sep_food_adequacy=="sep_food_adequacy_less",vi.levels[i]], raab$vi.denom[raab$sep_food_adequacy=="sep_food_adequacy_less"])
  food.vi$just.enough.adj.pct[i]<-prop.age.sex.adjust(popfives, raab[raab$sep_food_adequacy=="sep_food_adequacy_adequate",], raab[raab$sep_food_adequacy=="sep_food_adequacy_adequate",vi.levels[i]], raab$vi.denom[raab$sep_food_adequacy=="sep_food_adequacy_adequate"])
  food.vi$more.enough.adj.pct[i]<-prop.age.sex.adjust(popfives, raab[raab$sep_food_adequacy=="sep_food_adequacy_more",], raab[raab$sep_food_adequacy=="sep_food_adequacy_more",vi.levels[i]], raab$vi.denom[raab$sep_food_adequacy=="sep_food_adequacy_more"])
  
  food.vi$not.enough.adj.pct.lci[i]<-bennett.lci(food.vi$not.enough.adj.pct[i],raab[raab$sep_food_adequacy=="sep_food_adequacy_less",vi.levels[i]],raab$vi.denom[raab$sep_food_adequacy=="sep_food_adequacy_less"],raab$clusterNumber[raab$sep_food_adequacy=="sep_food_adequacy_less"])
  food.vi$just.enough.adj.pct.lci[i]<-bennett.lci(food.vi$just.enough.adj.pct[i],raab[raab$sep_food_adequacy=="sep_food_adequacy_adequate",vi.levels[i]],raab$vi.denom[raab$sep_food_adequacy=="sep_food_adequacy_adequate"],raab$clusterNumber[raab$sep_food_adequacy=="sep_food_adequacy_adequate"])
  food.vi$more.enough.adj.pct.lci[i]<-bennett.lci(food.vi$more.enough.adj.pct[i],raab[raab$sep_food_adequacy=="sep_food_adequacy_more",vi.levels[i]],raab$vi.denom[raab$sep_food_adequacy=="sep_food_adequacy_more"],raab$clusterNumber[raab$sep_food_adequacy=="sep_food_adequacy_more"])
  
  food.vi$not.enough.adj.pct.uci[i]<-bennett.uci(food.vi$not.enough.adj.pct[i],raab[raab$sep_food_adequacy=="sep_food_adequacy_less",vi.levels[i]],raab$vi.denom[raab$sep_food_adequacy=="sep_food_adequacy_less"],raab$clusterNumber[raab$sep_food_adequacy=="sep_food_adequacy_less"])
  food.vi$just.enough.adj.pct.uci[i]<-bennett.uci(food.vi$just.enough.adj.pct[i],raab[raab$sep_food_adequacy=="sep_food_adequacy_adequate",vi.levels[i]],raab$vi.denom[raab$sep_food_adequacy=="sep_food_adequacy_adequate"],raab$clusterNumber[raab$sep_food_adequacy=="sep_food_adequacy_adequate"])
  food.vi$more.enough.adj.pct.uci[i]<-bennett.uci(food.vi$more.enough.adj.pct[i],raab[raab$sep_food_adequacy=="sep_food_adequacy_more",vi.levels[i]],raab$vi.denom[raab$sep_food_adequacy=="sep_food_adequacy_more"],raab$clusterNumber[raab$sep_food_adequacy=="sep_food_adequacy_more"])
  
}

lcis<-grep("lci",names(food.vi))
ucis<-grep("uci",names(food.vi))
food.vi[,lcis][food.vi[,lcis]<0]<-0
food.vi[,ucis][food.vi[,ucis]>1]<-1

pcts<-grep("pct",names(food.vi))
food.vi[,pcts]<-round( food.vi[,pcts] * 100, 1 )
food.vi[,pcts]<-format(food.vi[,pcts], nsmall=1)


# create output as separate tables while pilot testing
write.csv(sample.food, here('outputs', 'sample-food.csv'), row.names=FALSE)
write.csv(sample.income, here('outputs', 'sample-income.csv'), row.names=FALSE)
write.csv(food.vi, here('outputs', 'food-vi.csv'), row.names=FALSE)
write.csv(income.vi, here('outputs', 'income-vi.csv'), row.names=FALSE)
