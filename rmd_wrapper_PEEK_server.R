#######################################################################
#                  RAAB report wrapper - PEEK server                  #
#                         v1. 11 November 2022                        #
#######################################################################

#This script must be run in a docker image with the relevant packages enabled.
#This script should be run in a folder where raw RAAB data are contained in data subfolder

rm(list = ls())
require(rmarkdown)
require(here)
arguments <- commandArgs(trailingOnly = TRUE)
ID <- arguments[1]
checker<-read.csv(here("data", "surveys.csv"))
DR_check<-checker[checker$raab_id==ID,c('raab_id','dr_diabetes_blood_consent')]
WQ_check<-checker[checker$raab_id==ID,c('raab_id',"wg_difficulty_seeing","wg_difficulty_hearing","wg_difficulty_memory","wg_difficulty_mobility","wg_difficulty_selfcare","wg_difficulty_communication")]
remove(checker)

#Running core analyses on everything

render(here("RAAB7_scripts","RAAB7_reporter.Rmd"), output_file = here("outputs", paste0(ID,"_report")), output_dir = here("outputs", ID, "summary"))

#Check for DR data and run DR analyses

if(sum(!is.na(DR_check$dr_diabetes_blood_consent)==TRUE)>0)
  {
    render(here("RAAB7_scripts","RAAB7_DR_reporter.Rmd"), output_file = here("outputs", paste0(ID,"_DR_subreport")), output_dir = here("outputs", ID, "summary"))
  } else {
print("No DR data found")
  }

#Check for WGQ data and run WGQ analyses

  if(sum(!is.na(WQ_check$wg_difficulty_seeing)==TRUE) + sum(!is.na(WQ_check$wg_difficulty_hearing)==TRUE) + sum(!is.na(WQ_check$wg_difficulty_memory)==TRUE) + sum(!is.na(WQ_check$wg_difficulty_mobility)==TRUE) + sum(!is.na(WQ_check$wg_difficulty_selfcare)==TRUE) + sum(!is.na(WQ_check$wg_difficulty_selfcare)==TRUE) >0)
    {
      render(here("RAAB7_scripts","RAAB7_WGQ_reporter.Rmd"), output_file = here("outputs", paste0(ID,"_WGQ_subreport")), output_dir = here("outputs", ID, "summary"))
    } else {
      print("No WGQ data found")
    }



unlink(here("outputs", "summary", "*_files"),recursive=T)
unlink(here("RAAB7_scripts", "*.log"))



