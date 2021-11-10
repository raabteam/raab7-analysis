###############################################################################
#                               RAAB report wrapper                           #
#                                  v. 10 Nov 21                               #
###############################################################################

require(rmarkdown)
require(readxl)

#navigate to path where RAABX_reporter.Rmd is stored

rm(list = ls())

setwd("C:/Users/icrurbut/Dropbox/Bert/GBR/RAAB/")

path.to.raab<-"C:/Users/icrurbut/Dropbox/Bert/GBR/RAAB/"
path.to.pop<-"C:/Users/icrurbut/Dropbox/Bert/GBR/RAAB/"


meta<-read_xlsx(paste0(path.to.raab,"raab-log_v3.xlsx"),sheet=1)

fulls<-meta[meta$repo_meta==TRUE & meta$repo_data==TRUE,]

empties<-meta[meta$repo_meta==TRUE & meta$repo_data==FALSE,]

emptids<-unique(empties$raab_id)

for (i in 1:length(unique(empties$raab_id)))
{
ID<-emptids[i]
if(file.exists(ID)){outdir<-ID}else{dir.create(ID);outdir<-ID}
dir.create(paste0(ID,"/summary"))
dir.create(paste0(ID,"/summary/data"))
dir.create(paste0(ID,"/raw"))
dir.create(paste0(ID,"/raw/data"))
empty_meta<-empties[empties$raab_id==ID,]
write.table(empty_meta,file=paste0(outdir,"/raw/meta.csv"),row.names=F,col.names=T,sep=",",na="")
}

#RAAB5

setwd("C:/Users/icrurbut/Dropbox/Bert/GBR/RAAB/RAAB5_scripts/")

path.to.scripts<-"C:/Users/icrurbut/Dropbox/Bert/GBR/RAAB/RAAB5_scripts/"

raab5<-read.csv(paste0(path.to.raab,"raabs_618_repo.csv"))
raab5ids<-as.data.frame(unique(raab5$raab_id))
raab5ids<-raab5ids[raab5ids$`unique(raab5$raab_id)` %in% fulls$raab_id,]
remove(raab5)

for (k in 1:length(raab5ids))
  {
  ID<-raab5ids[k]
  if(file.exists(ID)){outdir<-ID}else{dir.create(ID);outdir<-ID}
  dir.create(paste0(ID,"/summary"))
  dir.create(paste0(ID,"/summary/data"))
  dir.create(paste0(ID,"/raw"))
  dir.create(paste0(ID,"/raw/data"))
  render(paste0(path.to.scripts,"RAAB5_reporter.Rmd"), output_file = paste0(ID,"_","report"), output_dir = paste0(outdir,"/summary"))
  unlink(paste0(ID,"/summary/*_files"),recursive=T)
  print(paste0(raab5ids[k],": done!"))
  }

#RAAB6

setwd("C:/Users/icrurbut/Dropbox/Bert/GBR/RAAB/RAAB6_scripts/")

raab<-read.csv(paste0(path.to.raab,"raabs_612_repo.csv"))

path.to.scripts<-"C:/Users/icrurbut/Dropbox/Bert/GBR/RAAB/RAAB6_scripts/"
raab6<-read.csv(paste0(path.to.raab,"raabs_612_repo.csv"))
raab6ids<-as.data.frame(unique(raab6$raab_id))
raab6ids<-raab6ids[raab6ids$`unique(raab6$raab_id)` %in% fulls$raab_id,]
remove(raab6)

for (k in 1:length(raab6ids))
  {
    ID<-raab6ids[k]
    if(file.exists(ID)){outdir<-ID}else{dir.create(ID);outdir<-ID}
    dir.create(paste0(ID,"/summary"))
    dir.create(paste0(ID,"/summary/data"))
    dir.create(paste0(ID,"/raw"))
    dir.create(paste0(ID,"/raw/data"))
    render(paste0(path.to.scripts,"RAAB6_reporter.Rmd"), output_file = paste0(ID,"_","report", output_dir = paste0(outdir,"/summary"))
    unlink(paste0(ID,"/summary/*_files"),recursive=T)
    print(paste0(raab6ids[k],": done!"))
  }

raabids<-c(raab5ids,raab6ids)
missing<-fulls[!(fulls$raab_id %in% raabids),]
