#######################################################################
#                  RAAB report wrapper - PEEK server                  #
#                         v. 24 Apr 2023                              #
#######################################################################

#This script must be run in a docker image
#This script should be run in a folder where raw RAAB data are contained in data subfolder

#library(tinytex)
#install_tinytex(repository="https://ftp.tu-chemnitz.de/pub/tug/historic/systems/texlive/2022/tlnet-final",version="2022.12",force=T,extra_packages = c("fancyhdr","tabu","float"))

rm(list = ls())

library(rmarkdown)
library(here)

arguments <- commandArgs(trailingOnly = TRUE)
ID <- arguments[1]

checker7<-read.csv(here("data", "surveys.csv"))
DR_check<-checker7[checker7$raab_id==ID,c('raab_id','dr_diabetes_blood_consent')]
WQ_check<-checker7[checker7$raab_id==ID,c('raab_id',"wg_difficulty_seeing","wg_difficulty_hearing","wg_difficulty_memory","wg_difficulty_mobility","wg_difficulty_selfcare","wg_difficulty_communication")]
remove(checker7)

render(here("RAAB7_scripts","RAAB7_reporter.Rmd"), output_file = here("outputs", paste0(ID,"_report")), output_dir = here("outputs", ID, "summary"))

unlink(here("outputs", "summary", "*_files"),recursive=T)
unlink(here("RAAB7_scripts", "*.log"))



