#######################################################################
#                  RAAB report wrapper - single report                #
#                         v. 11 Jan 2023                              #
#######################################################################

#This script should be run in a folder where raw RAAB data are contained in data subfolder

rm(list = ls())
setwd("GBR/RAAB/github_repo_sync/")

library(rmarkdown)
library(knitr)
library(tinytex)
library(kableExtra)
library(float)
library(here)

#Sample raab5 ID
ID<-"2000_Vietnam_Ha-Tay"
#Sample raab6 ID
ID<-"2017_Papua-New-Guinea_Islands"
#Sample raab7 ID
ID<-"15e50c7a-d2b4-426a-8a3a-f4234e88e8c8"

#RAAB5 report

checker_r5<-read.csv(here("data", "raabs_618.csv"))
DR_check_r5<-checker_r5[checker_r5$raab_id==ID,c('raab_id','dr_diabetes_blood_consent')]
remove(checker_r5)

render(here("RAAB5_scripts","RAAB5_reporter.Rmd"), output_file = here("outputs", paste0(ID,"_report")), output_dir = here("outputs", ID, "summary"))

unlink(here("outputs", "summary", "*_files"),recursive=T)
unlink(here("RAAB5_scripts", "*.log"))

#RAAB6 report

checker_r6<-read.csv(here("data", "raabs_612.csv"))
DR_check_r6<-checker_r6[checker_r6$raab_id==ID,c('raab_id','dr_diabetes_blood_consent')]
remove(checker_r6)

render(here("RAAB6_scripts","RAAB6_reporter.Rmd"), output_file = here("outputs", paste0(ID,"_report")), output_dir = here("outputs", ID, "summary"))

unlink(here("outputs", "summary", "*_files"),recursive=T)
unlink(here("RAAB6_scripts", "*.log"))

#RAAB7 without UCVA/CVA

checker_early_r7<-read.csv(here("data", "surveys.csv"))
DR_check_early_r7<-checker_early_r7[checker_early_r7$raab_id==ID,c('raab_id','dr_diabetes_blood_consent')]
remove(checker_early_r7)

render(here("RAAB6_scripts","early_RAAB7_reporter.Rmd"), output_file = here("outputs", paste0(ID,"_report")), output_dir = here("outputs", ID, "summary"))

unlink(here("outputs", "summary", "*_files"),recursive=T)
unlink(here("RAAB6_scripts", "*.log"))


#RAAB7 report

checker<-read.csv(here("data", "surveys.csv"))
DR_check<-checker[checker$raab_id==ID,c('raab_id','dr_diabetes_blood_consent')]
WQ_check<-checker[checker$raab_id==ID,c('raab_id',"wg_difficulty_seeing","wg_difficulty_hearing","wg_difficulty_memory","wg_difficulty_mobility","wg_difficulty_selfcare","wg_difficulty_communication")]
remove(checker)

render(here("RAAB7_scripts","RAAB7_reporter.Rmd"), output_file = here("outputs", paste0(ID,"_report")), output_dir = here("outputs", ID, "summary"))

unlink(here("outputs", "summary", "*_files"),recursive=T)
unlink(here("RAAB7_scripts", "*.log"))
