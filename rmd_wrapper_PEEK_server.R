#######################################################################
#                  RAAB report wrapper - PEEK server                  #
#                         v. 11 Jan 2023                              #
#######################################################################

#This script must be run in a docker image with the relevant packages enabled.
#This script should be run in a folder where raw RAAB data are contained in data subfolder

rm(list = ls())

library(rmarkdown)
library(knitr)
library(tinytex)
library(kableExtra)
library(float)
library(here)

arguments <- commandArgs(trailingOnly = TRUE)
ID <- arguments[1]

checker<-read.csv(here("data", "surveys.csv"))
DR_check<-checker[checker$raab_id==ID,c('raab_id','dr_diabetes_blood_consent')]
WQ_check<-checker[checker$raab_id==ID,c('raab_id',"wg_difficulty_seeing","wg_difficulty_hearing","wg_difficulty_memory","wg_difficulty_mobility","wg_difficulty_selfcare","wg_difficulty_communication")]
remove(checker)

#Running core analyses on everything


render(here("RAAB7_scripts","RAAB7_reporter.Rmd"), output_file = here("outputs", paste0(ID,"_report")), output_dir = here("outputs", ID, "summary"))

unlink(here("outputs", "summary", "*_files"),recursive=T)
unlink(here("RAAB7_scripts", "*.log"))



