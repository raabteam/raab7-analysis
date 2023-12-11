# RAAB7 output for main results on www.raab.world 
# From RAAB7s with distance UCVA and optional near VA

# Vision impairment
prebigboi1<-dcast(melt(sum3[,c("vi.level","female.adj.pct","female.adj.pct.lci","female.adj.pct.uci","male.adj.pct","male.adj.pct.lci","male.adj.pct.uci","total.adj.pct","total.adj.pct.lci","total.adj.pct.uci","extrapolated.female.n","extrapolated.male.n","extrapolated.total.n")], id.var="vi.level"), 1~variable+vi.level)

# eCSC/CSC
prev14_360<-prev14[prev14$denom.thresh==360 & (prev14$num.thresh=="csc" | prev14$num.thresh=="ecsc_612"),]
prev14_660<-prev14[prev14$denom.thresh==660 & (prev14$num.thresh=="csc" | prev14$num.thresh=="ecsc_612"),]
prev14_618<-prev14[prev14$denom.thresh==618 & (prev14$num.thresh=="csc" | prev14$num.thresh=="ecsc_612"),]
prev14_612<-prev14[prev14$denom.thresh==612 & (prev14$num.thresh=="csc" | prev14$num.thresh=="ecsc_612"),]

prebigboi2<-dcast(melt(prev14_360[,2:20], id.var="num.thresh"), 1~variable+num.thresh)
names(prebigboi2)<-paste0(names(prebigboi2),"_operable_thresh_360")
prebigboi3<-dcast(melt(prev14_660[,2:20], id.var="num.thresh"), 1~variable+num.thresh)
names(prebigboi3)<-paste0(names(prebigboi3),"_operable_thresh_660")
prebigboi4<-dcast(melt(prev14_618[,2:20], id.var="num.thresh"), 1~variable+num.thresh)
names(prebigboi4)<-paste0(names(prebigboi4),"_operable_thresh_618")
prebigboi5<-dcast(melt(prev14_612[,2:20], id.var="num.thresh"), 1~variable+num.thresh)
names(prebigboi5)<-paste0(names(prebigboi5),"_operable_thresh_612")

# Unmet need for cataract surgery
prebigboi6<-dcast(melt(asa7[,c("out.names","male.adj.pct","male.adj.pct.lci","male.adj.pct.uci","female.adj.pct","female.adj.pct.lci","female.adj.pct.uci","total.adj.pct","total.adj.pct.lci","total.adj.pct.uci","extrapolated.male.n","extrapolated.female.n","extrapolated.total.n")], id.var="out.names"), 1~variable+out.names)
names(prebigboi6)<-gsub("\\ <","_",names(prebigboi6))
names(prebigboi6)<-gsub("/","",names(prebigboi6))

# Top three causes of blindness
causes<-sum6[1:13,]
causes<-causes[order(causes$blind.pct,decreasing=T),]
top_causes<-causes[1:3,c("principal.cause","blind.n","blind.pct")]
prebigboi7<-dcast(melt(top_causes,id.var="principal.cause"),1~variable+principal.cause)

# Refractive error prevalence
prebigboi8<-dist.re.prev.final[,-1]
names(prebigboi8)<-paste0(names(prebigboi8),"_dist_re")

# Distance eREC/REC
prebigboi9<-dcast(melt(newtab5,id.vars="rec_metric"), 1~variable+rec_metric)
prebigboi9<-dcast(melt(newtab5,id.vars="rec_metric"), 1~variable+rec_metric)

# Near VI/ near eREC if used
if(sum(!is.na(NV_check$binocular_near_corrected_result)==TRUE)>0){
  
  prebigboi10<-near.re.prev.final[,-1]
  names(prebigboi10)<-paste0(names(prebigboi10),"_near_re")
  
  prebigboi11<-dcast(melt(newtab7,id.vars="rec_metric"), 1~variable+rec_metric)
  prebigboi11<-dcast(melt(newtab7,id.vars="rec_metric"), 1~variable+rec_metric)
  
  bigboi<-as.data.frame(cbind(prebigboi1,prebigboi2,prebigboi3,prebigboi4,prebigboi5,prebigboi6,prebigboi7,prebigboi8,prebigboi9,prebigboi10,prebigboi11))
  
  }else{
  
  bigboi<-as.data.frame(cbind(prebigboi1,prebigboi2,prebigboi3,prebigboi4,prebigboi5,prebigboi6,prebigboi7,prebigboi8,prebigboi9))}

bigboi[bigboi=="*"]<-NA

bigboi[1,1]<-ID
names(bigboi)[1]<-"raab_id"

spots<-grep("^\\.",names(bigboi))
bigboi<-bigboi[,-spots]