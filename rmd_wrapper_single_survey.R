#######################################################################
#                  RAAB report wrapper - single report                #
#                         v. 11 Jan 2023                              #
#                         v. 24 Apr 2023                              #
#######################################################################

#This script should be run in a folder where raw RAAB data are contained in data subfolder

rm(list = ls())
setwd("path/to/folder/")

library(rmarkdown)
library(here)

#Sample raab5 ID
ID<-"insert sample ID here"
#Sample raab6 ID
ID<-"insert sample ID here"
#Sample early raab7 ID
ID<-"insert sample ID here"
#Sample recent raab7 ID
ID<-"insert sample ID here"

#RAAB5 report

render(here("RAAB5_scripts","RAAB5_reporter.Rmd"), output_file = here("outputs", paste0(ID,"_report")), output_dir = here("outputs", ID, "summary"))

unlink(here("outputs", "summary", "*_files"),recursive=T)
unlink(here("RAAB5_scripts", "*.log"))

#RAAB6 report

checker6<-read.csv(here("data", "raabs_612.csv"))
DR_check<-checker6[checker6$raab_id==ID,c('raab_id','dr_diabetes_blood_consent')]
WQ_check<-checker6[checker6$raab_id==ID,c('raab_id',"wg_difficulty_seeing","wg_difficulty_hearing","wg_difficulty_memory","wg_difficulty_mobility","wg_difficulty_selfcare","wg_difficulty_communication")]
remove(checker6)

render(here("RAAB6_scripts","RAAB6_reporter.Rmd"), output_file = here("outputs", paste0(ID,"_report")), output_dir = here("outputs", ID, "summary"))

unlink(here("outputs", "summary", "*_files"),recursive=T)
unlink(here("RAAB6_scripts", "*.log"))

#RAAB7 without UCVA/CVA

checker_early_r7<-read.csv(here("data", "surveys.csv"))
DR_check_early_r7<-checker_early_r7[checker_early_r7$raab_id==ID,c('raab_id','dr_diabetes_blood_consent')]
WQ_check<-checker_early_r7[checker_early_r7$raab_id==ID,c('raab_id',"wg_difficulty_seeing","wg_difficulty_hearing","wg_difficulty_memory","wg_difficulty_mobility","wg_difficulty_selfcare","wg_difficulty_communication")]
remove(checker_early_r7)

render(here("RAAB6_scripts","RAAB6_reporter.Rmd"), output_file = here("outputs", paste0(ID,"_report")), output_dir = here("outputs", ID, "summary"))

unlink(here("outputs", "summary", "*_files"),recursive=T)
unlink(here("RAAB6_scripts", "*.log"))


#RAAB7 report

checker7<-read.csv(here("data", "surveys.csv"))
DR_check<-checker7[checker7$raab_id==ID,c('raab_id','dr_diabetes_blood_consent')]
WQ_check<-checker7[checker7$raab_id==ID,c('raab_id',"wg_difficulty_seeing","wg_difficulty_hearing","wg_difficulty_memory","wg_difficulty_mobility","wg_difficulty_selfcare","wg_difficulty_communication")]
remove(checker7)

render(here("RAAB7_scripts","RAAB7_reporter.Rmd"), output_file = here("outputs", paste0(ID,"_report")), output_dir = here("outputs", ID, "summary"))

unlink(here("outputs", "summary", "*_files"),recursive=T)
unlink(here("RAAB7_scripts", "*.log"))
