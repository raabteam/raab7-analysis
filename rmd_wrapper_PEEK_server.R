#######################################################################
#                  RAAB report wrapper - PEEK server                  #
#                         v1. 11 November 2022                        #
#######################################################################

#This script must be run in a docker image with the following packages enabled.
#This script should be run in a folder where raw RAAB data are contained in data subfolder

rm(list = ls())
setwd("path/to/folder")
# setwd("/raab7")
require(rmarkdown)
require(here)
arguments <- commandArgs(trailingOnly = TRUE)
ID <- arguments[1]
ID<-"b871319b-08ee-4508-bd41-413733c51075"
DR_check<-read.csv(here("data", "surveys.csv"))
DR_check<-DR_check[DR_check$raab_id==ID,c('raab_id','dr_diabetes_blood_consent')]

#if(sum(!is.na(DR_check$dr_diabetes_blood_consent)==TRUE)>0)
#  {
    render(here("RAAB7_scripts","DR_RAAB7_reporter.Rmd"), output_file = here("outputs", paste0(ID,"_report")), output_dir = here("outputs", ID, "summary"))
#  }
#else
#  {
#    render(here("RAAB7_scripts","RAAB7_reporter.Rmd"), output_file = here("outputs", paste0(ID,"_report")), output_dir = here("outputs", ID, "summary"))
#  }

render(here("RAAB7_scripts","RAAB7_reporter.Rmd"), output_file = here("outputs", paste0(ID,"_report")), output_dir = here("outputs", ID, "summary"))
unlink(here("outputs", "summary", "*_files"),recursive=T)
unlink(here("RAAB7_scripts", "*.log"))


