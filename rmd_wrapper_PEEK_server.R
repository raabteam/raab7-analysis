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

#Running core analyses on everything

render(here("RAAB7_scripts","RAAB7_reporter.Rmd"), output_file = here("outputs", paste0(ID,"_report")), output_dir = here("outputs", ID, "summary"))

unlink(here("outputs", "summary", "*_files"),recursive=T)
unlink(here("RAAB7_scripts", "*.log"))



