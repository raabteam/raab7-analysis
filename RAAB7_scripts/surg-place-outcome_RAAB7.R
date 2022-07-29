#RAAB6

#v1 23 Aug 21 - RB

right.male<-as.data.frame.matrix(table(raab$right.oc.levels[raab$gender=="male"],raab$surgery_place_right[raab$gender=="male"]))

for(i in 1:length(surgery_places))
    { 
      if(!(surgery_places[i] %in% names(right.male)))
        {
          new.col.i<-surgery_places[i]
          right.male[,new.col.i]<-0
        } 
      else 
        {
          right.male<-right.male
        }
    }

right.female<-as.data.frame.matrix(table(raab$right.oc.levels[raab$gender=="female"],raab$surgery_place_right[raab$gender=="female"]))

for(i in 1:length(surgery_places))
  { 
    if(!(surgery_places[i] %in% names(right.female)))
      {
        new.col.i<-surgery_places[i]
        right.female[,new.col.i]<-0
      } 
    else 
      {
        right.female<-right.female
      }
  }

right.total<-as.data.frame.matrix(table(raab$right.oc.levels,raab$surgery_place_right))

for(i in 1:length(surgery_places))
{ 
  if(!(surgery_places[i] %in% names(right.total)))
  {
    new.col.i<-surgery_places[i]
    right.total[,new.col.i]<-0
  } 
  else 
  {
    right.total<-right.total
  }
}

left.male<-as.data.frame.matrix(table(raab$left.oc.levels[raab$gender=="male"],raab$surgery_place_left[raab$gender=="male"]))

for(i in 1:length(surgery_places))
{ 
  if(!(surgery_places[i] %in% names(left.male)))
  {
    new.col.i<-surgery_places[i]
    left.male[,new.col.i]<-0
  } 
  else 
  {
    left.male<-left.male
  }
}

left.female<-as.data.frame.matrix(table(raab$left.oc.levels[raab$gender=="female"],raab$surgery_place_left[raab$gender=="female"]))

for(i in 1:length(surgery_places))
{ 
  if(!(surgery_places[i] %in% names(left.female)))
  {
    new.col.i<-surgery_places[i]
    left.female[,new.col.i]<-0
  } 
  else 
  {
    left.female<-left.female
  }
}

left.total<-as.data.frame.matrix(table(raab$left.oc.levels,raab$surgery_place_left))

for(i in 1:length(surgery_places))
{ 
  if(!(surgery_places[i] %in% names(left.total)))
  {
    new.col.i<-surgery_places[i]
    left.total[,new.col.i]<-0
  } 
  else 
  {
    left.total<-left.total
  }
}



male_surgery_place_camp_improvised_n<-right.male$surgery_place_camp_improvised+left.male$surgery_place_camp_improvised
male_surgery_place_gov_hospital_n<-right.male$surgery_place_gov_hospital+left.male$surgery_place_gov_hospital
male_surgery_place_private_hospital_n<-right.male$surgery_place_private_hospital+left.male$surgery_place_private_hospital
male_surgery_place_traditional_n<-right.male$surgery_place_traditional+left.male$surgery_place_traditional
male_surgery_place_voluntary_hospital_n<-right.male$surgery_place_voluntary_hospital+left.male$surgery_place_voluntary_hospital

male<-data.frame(cbind(male_surgery_place_camp_improvised_n,male_surgery_place_gov_hospital_n,male_surgery_place_private_hospital_n,male_surgery_place_traditional_n,male_surgery_place_voluntary_hospital_n))
male$oc<-row.names(right.male)
male$oc<-gsub("right.","",male$oc)

ifelse(sum(male$male_surgery_place_camp_improvised_n)>0,male$male_surgery_place_camp_improvised_percent<-round((male$male_surgery_place_camp_improvised_n/sum(male$male_surgery_place_camp_improvised_n))*100,1),male$male_surgery_place_camp_improvised_percent<-round(0,1))
ifelse(sum(male$male_surgery_place_gov_hospital_n)>0,male$male_surgery_place_gov_hospital_percent<-round((male$male_surgery_place_gov_hospital_n/sum(male$male_surgery_place_gov_hospital_n))*100,1),male$male_surgery_place_gov_hospital_percent<-round(0,1))
ifelse(sum(male$male_surgery_place_private_hospital_n)>0,male$male_surgery_place_private_hospital_percent<-round((male$male_surgery_place_private_hospital_n/sum(male$male_surgery_place_private_hospital_n))*100,1),male$male_surgery_place_private_hospital_percent<-round(0,1))
ifelse(sum(male$male_surgery_place_traditional_n)>0,male$male_surgery_place_traditional_percent<-round((male$male_surgery_place_traditional_n/sum(male$male_surgery_place_traditional_n))*100,1),male$male_surgery_place_traditional_percent<-round(0,1))
ifelse(sum(male$male_surgery_place_voluntary_hospital_n)>0,male$male_surgery_place_voluntary_hospital_percent<-round((male$male_surgery_place_voluntary_hospital_n/sum(male$male_surgery_place_voluntary_hospital_n))*100,1),male$male_surgery_place_voluntary_hospital_percent<-round(0,1))

female_surgery_place_camp_improvised_n<-right.female$surgery_place_camp_improvised+left.female$surgery_place_camp_improvised
female_surgery_place_gov_hospital_n<-right.female$surgery_place_gov_hospital+left.female$surgery_place_gov_hospital
female_surgery_place_private_hospital_n<-right.female$surgery_place_private_hospital+left.female$surgery_place_private_hospital
female_surgery_place_traditional_n<-right.female$surgery_place_traditional+left.female$surgery_place_traditional
female_surgery_place_voluntary_hospital_n<-right.female$surgery_place_voluntary_hospital+left.female$surgery_place_voluntary_hospital

female<-data.frame(cbind(female_surgery_place_camp_improvised_n,female_surgery_place_gov_hospital_n,female_surgery_place_private_hospital_n,female_surgery_place_traditional_n,female_surgery_place_voluntary_hospital_n))
female$oc<-row.names(right.female)
female$oc<-gsub("right.","",female$oc)

ifelse(sum(female$female_surgery_place_camp_improvised_n)>0,female$female_surgery_place_camp_improvised_percent<-round((female$female_surgery_place_camp_improvised_n/sum(female$female_surgery_place_camp_improvised_n))*100,1),female$female_surgery_place_camp_improvised_percent<-round(0,1))
ifelse(sum(female$female_surgery_place_gov_hospital_n)>0,female$female_surgery_place_gov_hospital_percent<-round((female$female_surgery_place_gov_hospital_n/sum(female$female_surgery_place_gov_hospital_n))*100,1),female$female_surgery_place_gov_hospital_percent<-round(0,1))
ifelse(sum(female$female_surgery_place_private_hospital_n)>0,female$female_surgery_place_private_hospital_percent<-round((female$female_surgery_place_private_hospital_n/sum(female$female_surgery_place_private_hospital_n))*100,1),female$female_surgery_place_private_hospital_percent<-round(0,1))
ifelse(sum(female$female_surgery_place_traditional_n)>0,female$female_surgery_place_traditional_percent<-round((female$female_surgery_place_traditional_n/sum(female$female_surgery_place_traditional_n))*100,1),female$female_surgery_place_traditional_percent<-round(0,1))
ifelse(sum(female$female_surgery_place_voluntary_hospital_n)>0,female$female_surgery_place_voluntary_hospital_percent<-round((female$female_surgery_place_voluntary_hospital_n/sum(female$female_surgery_place_voluntary_hospital_n))*100,1),female$female_surgery_place_voluntary_hospital_percent<-round(0,1))

total_surgery_place_camp_improvised_n<-right.total$surgery_place_camp_improvised+left.total$surgery_place_camp_improvised
total_surgery_place_gov_hospital_n<-right.total$surgery_place_gov_hospital+left.total$surgery_place_gov_hospital
total_surgery_place_private_hospital_n<-right.total$surgery_place_private_hospital+left.total$surgery_place_private_hospital
total_surgery_place_traditional_n<-right.total$surgery_place_traditional+left.total$surgery_place_traditional
total_surgery_place_voluntary_hospital_n<-right.total$surgery_place_voluntary_hospital+left.total$surgery_place_voluntary_hospital

total<-data.frame(cbind(total_surgery_place_camp_improvised_n,total_surgery_place_gov_hospital_n,total_surgery_place_private_hospital_n,total_surgery_place_traditional_n,total_surgery_place_voluntary_hospital_n))
total$oc<-row.names(right.total)
total$oc<-gsub("right.","",total$oc)

ifelse(sum(total$total_surgery_place_camp_improvised_n)>0,total$total_surgery_place_camp_improvised_percent<-round((total$total_surgery_place_camp_improvised_n/sum(total$total_surgery_place_camp_improvised_n))*100,1),total$total_surgery_place_camp_improvised_percent<-round(0,1))
ifelse(sum(total$total_surgery_place_gov_hospital_n)>0,total$total_surgery_place_gov_hospital_percent<-round((total$total_surgery_place_gov_hospital_n/sum(total$total_surgery_place_gov_hospital_n))*100,1),total$total_surgery_place_gov_hospital_percent<-round(0,1))
ifelse(sum(total$total_surgery_place_private_hospital_n)>0,total$total_surgery_place_private_hospital_percent<-round((total$total_surgery_place_private_hospital_n/sum(total$total_surgery_place_private_hospital_n))*100,1),total$total_surgery_place_private_hospital_percent<-round(0,1))
ifelse(sum(total$total_surgery_place_traditional_n)>0,total$total_surgery_place_traditional_percent<-round((total$total_surgery_place_traditional_n/sum(total$total_surgery_place_traditional_n))*100,1),total$total_surgery_place_traditional_percent<-round(0,1))
ifelse(sum(total$total_surgery_place_voluntary_hospital_n)>0,total$total_surgery_place_voluntary_hospital_percent<-round((total$total_surgery_place_voluntary_hospital_n/sum(total$total_surgery_place_voluntary_hospital_n))*100,1),total$total_surgery_place_voluntary_hospital_percent<-round(0,1))

total<-total[,c(6,1:5,7:11)]
total[nrow(total)+1,2:11]<-colSums(total[,2:11])
total[nrow(total),1]<-"Total"
t.pcts<-grep("percent",names(total))
total[,t.pcts]<-format(total[,t.pcts],nsmall=1)


male<-male[,c(6,1:5,7:11)]
male[nrow(male)+1,2:11]<-colSums(male[,2:11])
male[nrow(male),1]<-"Total"
m.pcts<-grep("percent",names(male))
male[,m.pcts]<-format(male[,m.pcts],nsmall=1)


female<-female[,c(6,1:5,7:11)]
female[nrow(female)+1,2:11]<-colSums(female[,2:11])
female[nrow(female),1]<-"Total"
f.pcts<-grep("percent",names(female))
female[,f.pcts]<-format(female[,f.pcts],nsmall=1)