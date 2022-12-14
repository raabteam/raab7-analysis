###############################################################################
#                  RAAB report wrapper - single survey                        #
#                             v1. 17 May 2022                                 #
#                             v2. 01 Jul 2022 IM (uses here())                #
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

#1. Download all RAAB7 scripts and save in a folder. Download most recent version of meta_data_logfile from sharepoint


#2. Navigate to downloaded folder (replace path with)
rm(list = ls())
setwd("../raab7")


#3. Load packages needed for wrapper script - others will be loaded in reporter script
require(rmarkdown)
require(here)

#4. Set data file names and retrieve RAAB ID

#For RAAB5 and 6 [no alphanumeric region ID]
#raab_id_hr<-<HUMAN READABLE SUVEY ID>
#raab_id<-<HUMAN READABLE SUVEY ID>
#resident.data<-read.csv(here("data","raabs_618.csv"))
#raab<-resident.data[resident.data$raab_id==raab_id,]
#population.data<-read.csv(here("data","raabs_pop_618_repo.csv"))
#pop.data<-population.data[population.data$raab_id==raab_id,]
#ID<-raab_id

#For RAAB7 [alphanumeric region ID]
ID<-<surveyID>

DR_check<-read.csv(here("data", "surveys.csv"))
DR_check<-DR_check[DR_check$raab_id==ID,c('raab_id','dr_diabetes_blood_consent')]

#5. Run appropriate reporter script
#render(here("RAAB5_scripts","RAAB5_reporter.Rmd"), output_file = here("outputs", paste0(raab_id_hr,"_report")), output_dir = here("outputs", ID, "summary"))
#render(here("RAAB6_scripts","RAAB6_reporter.Rmd"), output_file = here("outputs", paste0(raab_id_hr,"_report")), output_dir = here("outputs", ID, "summary"))

if(sum(!is.na(DR_check$dr_diabetes_blood_consent)==TRUE)>0)
{
  render(here("RAAB7_scripts","DR_RAAB7_reporter.Rmd"), output_file = here("outputs", paste0(ID,"_report")), output_dir = here("outputs", ID, "summary"))
} else {
  render(here("RAAB7_scripts","RAAB7_reporter.Rmd"), output_file = here("outputs", paste0(ID,"_report")), output_dir = here("outputs", ID, "summary"))
}

unlink(here("outputs", "summary", "*_files"),recursive=T)
unlink(here("RAAB7_scripts", "*.log"))

#6. Delete intermediate files
unlink(here("outputs", "summary", "*_files"),recursive=T)





