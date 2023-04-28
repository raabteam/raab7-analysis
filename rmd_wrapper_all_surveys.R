###############################################################################
#                               RAAB report wrapper                           #
#                                  v. 10 Nov 21                               #
#                                  v. 5 Aug 22                                #
#                                  v. 24 Apr 23                               #
###############################################################################

#DANGER: THIS SCRIPT WILL OVERWRITE PREVIOUS ANALYSES OF THE SAME RAAB DATA IN THE DATA OUTPUTS FOLDER
#MOVE PREVIOUS ANALYSES TO A SAFE FOLDER IF YOU WANT TO KEEP THEM

#This script should be run in a folder where raw RAAB data are contained in data subfolder

rm(list = ls())
setwd("path/to/folder/")

library(rmarkdown)
library(here)

#Read in meta file and trim unused rows
meta<-read.csv(here("data", "historic_meta.csv"))
meta[meta=="NA"]<-NA
meta<-meta[!is.na(meta$raab_id),]

#Split meta file into RAABs with permission to report (fulls) and no permission to report (empties)
fulls<-meta[meta$repo_meta==TRUE & meta$repo_data==TRUE,]
fulls<-fulls[!is.na(fulls$raab_id),]

empties<-meta[meta$repo_meta==TRUE & meta$repo_data==FALSE,]
empties<-empties[!is.na(empties$raab_id),]

#Create empty folders for RAABs with no permission

emptids<-unique(empties$raab_id)

for (i in 1:3)
#for (i in 1:length(unique(empties$raab_id)))
{
ID<-emptids[i]
unlink(here("outputs",ID),recursive = T)
dir.create(here("outputs",ID))
outdir<-ID
dir.create(here("outputs",ID,"/summary"))
dir.create(here("outputs",ID,"/summary/data"))
dir.create(here("outputs",ID,"/raw"))
dir.create(here("outputs",ID,"/raw/data"))
empty_meta<-empties[empties$raab_id==ID,]
write.table(empty_meta,file=here("outputs",ID,"/raw/meta.csv"),row.names=F,col.names=T,sep=",",na="")
}

# Create reports for RAABs with permission

#RAAB5

raab5<-read.csv(here("data","raabs_618.csv"))
raab5ids_all<-as.data.frame(unique(raab5$raab_id))
raab5ids<-raab5ids_all[raab5ids_all$`unique(raab5$raab_id)` %in% fulls$raab_id,]

#First, run the loop on the first three RAABs to make sure everything works
for (k in 1:3)
#If it does work, run through all the RAABs
#for (k in 4:length(raab5ids))
{
  ID<-raab5ids[k]
  render(here("RAAB5_scripts","RAAB5_reporter.Rmd"), output_file = paste0(ID,"_report"), output_dir = here("outputs", ID, "summary"))
  unlink(here("outputs",ID,"/summary/*_files"),recursive=T)
  print(paste0(raab5ids[k],": done!"))
}

remove(raab5)

#RAAB6

raab6<-read.csv(here("data","raabs_612.csv"))
checker6<-raab6
raab6ids_all<-as.data.frame(unique(raab6$raab_id))
raab6ids<-raab6ids_all[raab6ids_all$`unique(raab6$raab_id)` %in% fulls$raab_id,]


#First, run the loop on the first three RAABs to make sure everything works
for (k in 1:3)
#If it does work, run through all the RAABs
#for (k in 4:length(raab6ids))
{
  ID<-raab6ids[k]
  DR_check<-checker6[checker6$raab_id==ID,c('raab_id','dr_diabetes_blood_consent')]
  WQ_check<-checker6[checker6$raab_id==ID,c('raab_id',"wg_difficulty_seeing","wg_difficulty_hearing","wg_difficulty_memory","wg_difficulty_mobility","wg_difficulty_selfcare","wg_difficulty_communication")]
  render(here("RAAB6_scripts","RAAB6_reporter.Rmd"), output_file = paste0(ID,"_report"), output_dir = here("outputs", ID, "summary"))
  unlink(here("outputs",ID,"/summary/*_files"),recursive=T)
  print(paste0(raab6ids[k],": done!"))
}

remove(raab6,checker6)

raabids<-c(raab5ids,raab6ids)
