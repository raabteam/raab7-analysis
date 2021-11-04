#RAAB report wrapper

require(rmarkdown)
require(readxl)

#these objects need to be defined manually until we know where this will all happen
#navigate to path where RAABX_reporter.Rmd is stored

rm(list = ls())

setwd("C:/Users/icrurbut/Dropbox/Bert/GBR/RAAB/")

path.to.raab<-"C:/Users/icrurbut/Dropbox/Bert/GBR/RAAB/"
path.to.pop<-"C:/Users/icrurbut/Dropbox/Bert/GBR/RAAB/"

#pop5<-read.table(paste0(path.to.pop,"raabs_pop_618_repo.csv"),sep=",",header=T)
#pop5$raab_id<-gsub("\\.","_",pop5$raab_id)
#pop5$raab_id<-gsub("\\ ","-",pop5$raab_id)
#pop5$raab_id<-gsub("\\_$","",pop5$raab_id)
#pop5$raab_id<-gsub("\\_$","",pop5$raab_id)
#pop5$gender<-gsub("m","male",pop5$gender)
#pop5$gender<-gsub("f","female",pop5$gender)
#write.table(pop5,paste0(path.to.pop,"raabs_pop_618_repo.csv"),quote=F,col.names=T,row.names=F,sep=",")
#pop6<-read.table(paste0(path.to.pop,"raabs_pop_612_repo.csv"),sep=",",header=T)
#pop6$raab_id<-gsub("\\.","_",pop6$raab_id)
#pop6$raab_id<-gsub("\\ ","-",pop6$raab_id)
#pop6$raab_id<-gsub("\\_$","",pop6$raab_id)
#pop6$raab_id<-gsub("\\_$","",pop6$raab_id)
#pop6$gender<-gsub("m","male",pop6$gender)
#pop6$gender<-gsub("f","female",pop6$gender)
#write.table(pop6,paste0(path.to.pop,"raabs_pop_612_repo.csv"),quote=F,col.names=T,row.names=F,sep=",")

meta<-read_xlsx(paste0(path.to.raab,"raab-log_v3.xlsx"),sheet=1)
meta$raab_id<-gsub("\\.","_",meta$raab_id)
meta$raab_id<-gsub("\\ ","-",meta$raab_id)
meta$raab_id<-gsub("\\_$","",meta$raab_id)
meta$raab_id<-gsub("\\_$","",meta$raab_id)

fulls<-meta[meta$repo_meta==TRUE & meta$repo_data==TRUE,]

empties<-meta[meta$repo_meta==TRUE & meta$repo_data==FALSE,]
emptids<-unique(empties$raab_id)

#emptids[149] 2019_Paksitan skipped because it has 3 ?s in the names

for (i in 1:length(unique(empties$raab_id)))
{
ID<-emptids[i]
if(file.exists(ID)){outdir<-ID}else{dir.create(ID);outdir<-ID}
dir.create(paste0(ID,"/summary"))
dir.create(paste0(ID,"/summary/data"))
dir.create(paste0(ID,"/raw"))
dir.create(paste0(ID,"/raw/data"))
empty_meta<-empties[empties$raab_id==ID,]
write.table(empty_meta,file=paste0(outdir,"/raw/meta.csv"),row.names=F,col.names=T,sep=",")
}

#RAAB5

setwd("C:/Users/icrurbut/Dropbox/Bert/GBR/RAAB/RAAB5_scripts/")

path.to.scripts<-"C:/Users/icrurbut/Dropbox/Bert/GBR/RAAB/RAAB5_scripts/"
#raab<-read.csv(paste0(path.to.raab,"raabs_618_repo.csv"))
#raab$raab_id<-gsub("\\.","_",raab$raab_id)
#raab$raab_id<-gsub("\\ ","-",raab$raab_id)
#raab$raab_id<-gsub("\\_$","",raab$raab_id)
#raab$raab_id<-gsub("\\_$","",raab$raab_id)
#raab$surgery_none_reason[raab$surgery_none_reason=="8"]<-"surgery_none_reason_unnecessary"
#raab$surgery_none_reason[raab$surgery_none_reason=="9"]<-"surgery_none_reason_unnecessary"
#raab$surgery_none_reason[raab$surgery_none_reason=="10"]<-"surgery_none_reason_unnecessary"
#raab$surgery_none_reason[raab$surgery_none_reason=="11"]<-"surgery_none_reason_fear"
#raab$surgery_none_reason[raab$surgery_none_reason=="12"]<-"surgery_none_reason_fear"
#raab$surgery_none_reason[raab$surgery_none_reason=="13"]<-"surgery_none_reason_denied"
#raab$surgery_none_reason2[raab$surgery_none_reason2=="9"]<-"surgery_none_reason_unnecessary"
#raab$surgery_none_reason2[raab$surgery_none_reason2=="10"]<-"surgery_none_reason_unnecessary"
#raab$surgery_none_reason2[raab$surgery_none_reason2=="11"]<-"surgery_none_reason_fear"
#raab$surgery_none_reason2[raab$surgery_none_reason2=="12"]<-"surgery_none_reason_fear"
#raab$surgery_none_reason2[raab$surgery_none_reason2=="13"]<-"surgery_none_reason_denied"
#write.table(raab,paste0(path.to.raab,"raabs_618_repo.csv"),sep=",",quote=F,row.names=F)

raab5<-read.csv(paste0(path.to.raab,"raabs_618_repo.csv"))
raab5ids<-as.data.frame(unique(raab5$raab_id))
raab5ids<-raab5ids[raab5ids$`unique(raab5$raab_id)` %in% fulls$raab_id,]
remove(raab5)

#NB some issues with running in a loop
#Malawi 2009 raab id has a comma in so removed from pop and raab file. Also leads to a very long file name
#Mexico 2010 raab id has a comma in so removed from pop and raab file
#Tanzania Singida 2017 is not in the population file so not run
#56 folders in total

for (k in 1:length(raab5ids))
  {
  ID<-raab5ids[k]
  if(file.exists(ID)){outdir<-ID}else{dir.create(ID);outdir<-ID}
  dir.create(paste0(ID,"/summary"))
  dir.create(paste0(ID,"/summary/data"))
  dir.create(paste0(ID,"/raw"))
  dir.create(paste0(ID,"/raw/data"))
# render(paste0(path.to.scripts,"RAAB5_reporter.Rmd"), output_file = paste0(ID,"_","report"), output_dir = paste0(outdir,"/summary"))
  render(paste0(path.to.scripts,"RAAB5_reporter_IM.Rmd"), output_file = paste0(ID,"_","report"), output_dir = paste0(outdir,"/summary"))
  unlink(paste0(ID,"/summary/*_files"),recursive=T)
  print(paste0(raab5ids[k],": done!"))
  }

#RAAB6
#RAAB IDs for three surveys from 2014 Malaysia and 2017 Kyrgyzstan have commas in so not run

setwd("C:/Users/icrurbut/Dropbox/Bert/GBR/RAAB/RAAB6_scripts/")

raab<-read.csv(paste0(path.to.raab,"raabs_612_repo.csv"))
#raab$raab_id<-gsub("\\.","_",raab$raab_id)
#raab$raab_id<-gsub("\\ ","-",raab$raab_id)
#raab$raab_id<-gsub("\\_$","",raab$raab_id)
#raab$raab_id<-gsub("\\_$","",raab$raab_id)
#raab$surgery_none_reason[raab$surgery_none_reason=="8"]<-"surgery_none_reason_unnecessary"
#raab$surgery_none_reason[raab$surgery_none_reason=="9"]<-"surgery_none_reason_unnecessary"
#raab$surgery_none_reason[raab$surgery_none_reason=="10"]<-"surgery_none_reason_unnecessary"
#raab$surgery_none_reason[raab$surgery_none_reason=="11"]<-"surgery_none_reason_fear"
#raab$surgery_none_reason[raab$surgery_none_reason=="12"]<-"surgery_none_reason_fear"
#raab$surgery_none_reason[raab$surgery_none_reason=="13"]<-"surgery_none_reason_denied"
#raab$surgery_none_reason2[raab$surgery_none_reason2=="8"]<-"surgery_none_reason_unnecessary"
#raab$surgery_none_reason2[raab$surgery_none_reason2=="9"]<-"surgery_none_reason_unnecessary"
#raab$surgery_none_reason2[raab$surgery_none_reason2=="10"]<-"surgery_none_reason_unnecessary"
#raab$surgery_none_reason2[raab$surgery_none_reason2=="11"]<-"surgery_none_reason_fear"
#raab$surgery_none_reason2[raab$surgery_none_reason2=="12"]<-"surgery_none_reason_fear"
#raab$surgery_none_reason2[raab$surgery_none_reason2=="13"]<-"surgery_none_reason_denied"
#write.table(raab,paste0(path.to.raab,"raabs_612_repo.csv"),sep=",",quote=F,row.names=F)

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
#   render(paste0(path.to.scripts,"RAAB6_reporter.Rmd"), output_file = paste0(ID,"_","report", output_dir = paste0(outdir,"/summary"))
    render(paste0(path.to.scripts,"RAAB6_reporter_IM.Rmd"), output_file = paste0(ID,"_","report"), output_dir = paste0(outdir,"/summary"))
    unlink(paste0(ID,"/summary/*_files"),recursive=T)
    print(paste0(raab6ids[k],": done!"))
  }

raabids<-c(raab5ids,raab6ids)
missing<-fulls[!(fulls$raab_id %in% raabids),]
