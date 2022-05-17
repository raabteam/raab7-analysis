###############################################################################
#                  RAAB report wrapper - single survey                        #
#                             v. 17 May 2022                                  #
###############################################################################

#1. Install required packages (first run only)

install.packages(rmarkdown)
install.packages(readxl)
install.packages(knitr)
install.packages(tinytex)
install.packages(kableExtra)
install.packages(float)
install.packages(RColorBrewer)
install_tinytex()

install.packages(tidyverse)
install.packages(stringr)
install.packages(treemap)
install.packages(maditr)

#2. Load packages needed for wrapper script - others will be loaded in reporter script
require(rmarkdown)
require(readxl)

#3. Clear R environment
rm(list = ls())

#4. Set directory for where you want to report to be saved
setwd("C:/Users/icrurbut/Dropbox/Bert/GBR/RAAB/RAAB7_eg/")

#5. Set paths to data and scripts
path.to.raab<-"C:/Users/icrurbut/Dropbox/Bert/GBR/RAAB/RAAB7_eg/"
path.to.pop<-"C:/Users/icrurbut/Dropbox/Bert/GBR/RAAB/RAAB7_eg/"
path.to.scripts<-"C:/Users/icrurbut/Dropbox/Bert/GBR/RAAB/github_repo_sync/RAAB7_scripts/"

#6. Read in meta file and trim unused rows
meta<-read_xlsx("../raab-log_v5.xlsx",sheet=1)
meta[meta=="NA"]<-NA
meta<-meta[!is.na(meta$raab_id),]

#7. Set data file names and retrieve RAAB ID
raab_id_hr<-"2022_Ethiopia_Amhara_West-Gojam"
resident.data<-"ethiopia.csv"
raab<-read.csv(paste0(path.to.raab,resident.data))
pop.data<-"ethiopia_pop.csv"
raab_id<-raab$regionId[1]


#9. Make folders for output (will override any previous outputs in the same folder)
ID<-raab_id
if(file.exists(ID)){outdir<-ID}else{dir.create(ID);outdir<-ID}
dir.create(paste0(ID,"/summary"))
dir.create(paste0(ID,"/summary/data"))
dir.create(paste0(ID,"/raw"))
dir.create(paste0(ID,"/raw/data"))

#10. Run reporter script
render(paste0(path.to.scripts,"RAAB7_reporter.Rmd"), output_file = paste0(ID,"_","report"), output_dir = paste0(outdir,"/summary"))

#11. Delete intermediate files
unlink(paste0(ID,"/summary/*_files"),recursive=T)

