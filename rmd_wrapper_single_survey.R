###############################################################################
#                  RAAB report wrapper - single survey                        #
#                             v1. 17 May 2022 
#                             v2. 01 Jul 2022 IM (uses here())  
###############################################################################

#1. Download all RAAB7 scripts and save in a folder. Download most recent version of raab_logfile from sharepoint

#2. Install required packages (first run only)

install.packages("rmarkdown")
install.packages("readxl")
install.packages("knitr")
install.packages("tinytex")
install.packages("kableExtra")
install.packages("float")
install.packages("RColorBrewer")
library(tinytex)
install.packages("tidyverse")
install.packages("stringr")
install.packages("treemap")
install.packages("maditr")

#3. Load packages needed for wrapper script - others will be loaded in reporter script
require(rmarkdown)
require(readxl)
require(here)

#4. Read in meta file and trim unused rows
meta<-read_xlsx(here('data', "raab-log_v5.xlsx"))
meta[meta=="NA"]<-NA
meta<-meta[!is.na(meta$raab_id),]

#5. Set data file names and retrieve RAAB ID
raab_id_hr<-"2022_Ethiopia_Amhara_West-Gojam"
resident.data<-"ethiopia.csv"
raab<-read.csv(here("data", resident.data))
pop.data<-"ethiopia_pop.csv"
raab_id<-raab$regionId[1]

#6. Make folders for output (will override any previous outputs in the same folder)
ID<-raab_id
if (file.exists(here("outputs",ID))) {outdir<-ID} else {dir.create(here("outputs",ID)); outdir<-ID}
dir.create(here("outputs",ID,"/summary"))
dir.create(here("outputs",ID,"/summary/data"))
dir.create(here("outputs",ID,"/raw"))
dir.create(here("outputs",ID,"/raw/data"))

#7. Run reporter script
render(here("RAAB7_scripts","RAAB7_reporter_new-script-names.Rmd"), output_file = here("outputs", paste0(ID,"_","report")), output_dir = here("outputs", ID, "summary"))

#8. Delete intermediate files
unlink(here("outputs", "summary", "*_files"),recursive=T)

