#######################################################################
#                  RAAB report wrapper - PEEK server                  #
#                         v1. 11 November 2022                        #
#######################################################################

#This script must be run in a docker image with the following packages enabled.
#This script should be run in a folder where raw RAAB data are contained in data subfolder

rm(list = ls())
#setwd("path/to/folder")
require(rmarkdown)
require(here)
ID<-<SURVEY ID>
render(here("RAAB7_scripts","RAAB7_reporter.Rmd"), output_file = here("outputs", paste0(ID,"_report")), output_dir = here("outputs", ID, "summary"))
unlink(here("outputs", "summary", "*_files"),recursive=T)
unlink(here("RAAB7_scripts", "*.log"))


