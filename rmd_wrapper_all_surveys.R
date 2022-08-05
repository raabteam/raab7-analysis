###############################################################################
#                               RAAB report wrapper                           #
#                                  v. 10 Nov 21                               #
#                                  v. 5 Aug 22                                #
###############################################################################

#DANGER: THIS SCRIPT WILL OVERWRITE PREVIOUS ANALYSES OF THE SAME RAAB DATA IN THE DATA OUTPUTS FOLDER
#MOVE PREVIOUS ANALYSES TO A SAFE FOLDER IF YOU WANT TO KEEP THEM

#Before first run, install required packages

#install.packages("rmarkdown")
#install.packages("readxl")
#install.packages("knitr")
#install.packages("tinytex")
#install.packages("kableExtra")
#install.packages("float")
#install.packages("RColorBrewer")
#install_tinytex()
#library(tinytex)
#install.packages("tidyverse")
#install.packages("stringr")
#install.packages("treemap")
#install.packages("maditr")

#1. Download all RAAB7 scripts and save in a folder. Download most recent version of raab_logfile from sharepoint

#2. Navigate to downloaded folder (replace path with)
rm(list = ls())
setwd("X:/path/to/folder")


#3. Load packages needed for wrapper script - others will be loaded in reporter script
require(rmarkdown)
require(readxl)
require(here)

#4. Read in meta file and trim unused rows
meta<-read_xlsx(here('data', "raab-log_v5.xlsx"))
meta[meta=="NA"]<-NA
meta<-meta[!is.na(meta$raab_id),]

#5. Split meta file into RAABs with permission to report (fulls) and no permission to report (empties)
fulls<-meta[meta$repo_meta==TRUE & meta$repo_data==TRUE,]
fulls<-fulls[!is.na(fulls$raab_id),]

empties<-meta[meta$repo_meta==TRUE & meta$repo_data==FALSE,]
empties<-empties[!is.na(empties$raab_id),]

mrm_vars<-c("raab_id","survey_title","year_end","iso_2","level1","level2","osm_id","iso3166-1","iso3166-2","admin_level","national","sample_size","response_rate","raab_version","va_threshold","raab_dr","raab_wgq","iceh_data","pi_name","pi_institution","pi_email1","trainer_name","trainer_email1","implementing_org","facilitating_org","funder","pub_doi1","pub_doi2","pub_doi3")

#6. Create empty folders for RAABs with no permission

emptids<-unique(empties$raab_id)

for (i in 1:length(unique(empties$raab_id)))
{
ID<-emptids[i]
unlink(here("outputs",ID),recursive = T)
dir.create(here("outputs",ID))
outdir<-ID
dir.create(here("outputs",ID,"/summary"))
dir.create(here("outputs",ID,"/summary/data"))
dir.create(here("outputs",ID,"/raw"))
dir.create(here("outputs",ID,"/raw/data"))
empty_meta<-empties[empties$raab_id==ID,mrm_vars]
write.table(empty_meta,file=here("outputs",ID,"/raw/meta.csv"),row.names=F,col.names=T,sep=",",na="")
}

#7. Create reports for RAABs with permission

#RAAB5

raab5<-read.csv(here("data","raabs_618.csv"))
raab5ids_all<-as.data.frame(unique(raab5$raab_id))
raab5ids<-raab5ids_all[raab5ids_all$`unique(raab5$raab_id)` %in% fulls$raab_id,]

#for (k in 1:length(raab5ids))
for (k in 1:3)
{
  ID<-raab5ids[k]
  render(here("RAAB5_scripts","RAAB5_reporter.Rmd"), output_file = paste0(ID,"_report"), output_dir = here("outputs", ID, "summary"))
  unlink(here("outputs",ID,"/summary/*_files"),recursive=T)
  print(paste0(raab5ids[k],": done!"))
}

remove(raab5)

#RAAB6

raab6<-read.csv(here("data","raabs_612.csv"))
raab6ids_all<-as.data.frame(unique(raab6$raab_id))
raab6ids<-raab6ids_all[raab6ids_all$`unique(raab6$raab_id)` %in% fulls$raab_id,]

#for (k in 1:length(raab6ids))
for (k in 1:3)
{
  ID<-raab6ids[k]
  render(here("RAAB6_scripts","RAAB6_reporter.Rmd"), output_file = paste0(ID,"_report"), output_dir = here("outputs", ID, "summary"))
  unlink(here("outputs",ID,"/summary/*_files"),recursive=T)
  print(paste0(raab6ids[k],": done!"))
}

remove(raab6)

#RAAB7

#TBC

raabids<-c(raab5ids,raab6ids,raab7ids)
