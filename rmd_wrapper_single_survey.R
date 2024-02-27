#######################################################################
#                  RAAB report wrapper - single report                #
#                         v. 11 Jan 2023                              #
#                         v. 24 Apr 2023                              #
#######################################################################

#This script should be run in a folder where raw RAAB data are contained in data subfolder

#For running on command line in Docker Image

rm(list = ls())

library(rmarkdown)
library(here)

arguments <- commandArgs(trailingOnly = TRUE)
ID <- arguments[1]

# For running manually on Windows computer

#rm(list = ls())
#setwd("GBR/RAAB/github_repo_sync/")

#library(rmarkdown)
#library(here)

#Sample raab5 ID
#ID<-"sample RAAB5 ID"
#Sample raab6 or pva raab7 ID
#ID<-"sample RAAB6 ID"
#Sample ucva raab7 ID
#ID<-"alpha-numeric Peek ID"

#RAAB5 report

#render(here("RAAB5_scripts","RAAB5_reporter.Rmd"), output_file = here("outputs", paste0(ID,"_report")), output_dir = here("outputs", ID, "summary"))

#unlink(here("outputs", "summary", "*_files"),recursive=T)
#unlink(here("RAAB5_scripts", "*.log"))

#RAAB6 & RAAB7 without UCVA report

checker6<-read.csv(here("data", "raabs_612_pva.csv"))
DR_check<-checker6[checker6$raab_id==ID,c('raab_id','dr_diabetes_blood_consent')]
WQ_check<-checker6[checker6$raab_id==ID,c('raab_id',"wg_difficulty_seeing","wg_difficulty_hearing","wg_difficulty_memory","wg_difficulty_mobility","wg_difficulty_selfcare","wg_difficulty_communication")]
remove(checker6)

render(here("RAAB6_scripts","RAAB6_reporter.Rmd"), output_file = here("outputs", paste0(ID,"_report")), output_dir = here("outputs", ID, "summary"))

unlink(here("outputs", "summary", "*_files"),recursive=T)
unlink(here("RAAB6_scripts", "*.log"))

#RAAB7 with UCVA report

#checker7<-read.csv(here("data", "surveys.csv"))
#DR_check<-checker7[checker7$raab_id==ID,c('raab_id','dr_diabetes_blood_consent')]
#WQ_check<-checker7[checker7$raab_id==ID,c('raab_id',"wg_difficulty_seeing","wg_difficulty_hearing","wg_difficulty_memory","wg_difficulty_mobility","wg_difficulty_selfcare","wg_difficulty_communication")]
#if(exists('binocular_near_corrected_result',where=checker7)){
#  NV_check<-checker7[checker7$raab_id==ID,c('raab_id','binocular_near_corrected_result')]
#}else{
#  NV_check<-data.frame(raab_id=rep(ID, nrow(checker7)), binocular_near_corrected_result=NA)}

#remove(checker7)

#render(here("RAAB7_scripts","RAAB7_reporter.Rmd"), output_file = here("outputs", paste0(ID,"_report")), output_dir = here("outputs", ID, "summary"))

#unlink(here("outputs", "summary", "*_files"),recursive=T)
#unlink(here("RAAB7_scripts", "*.log"))
