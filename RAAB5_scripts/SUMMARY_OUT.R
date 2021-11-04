#RAAB5

prebigboi1<-dcast(melt(sum3[,c("vi.level","female.adj.pct","male.adj.pct","total.adj.pct","extrapolated.female.n","extrapolated.male.n","extrapolated.total.n")], id.var="vi.level"), 1~variable+vi.level)

prev14_360<-prev14[prev14$denom.thresh==360,]
prev14_660<-prev14[prev14$denom.thresh==660,]
prev14_618<-prev14[prev14$denom.thresh==618,]

prebigboi2<-dcast(melt(prev14_360[,2:20], id.var="num.thresh"), 1~variable+num.thresh)
names(prebigboi2)<-paste0(names(prebigboi2),"_operable_thresh_360")
prebigboi3<-dcast(melt(prev14_660[,2:20], id.var="num.thresh"), 1~variable+num.thresh)
names(prebigboi3)<-paste0(names(prebigboi3),"_operable_thresh_660")
prebigboi4<-dcast(melt(prev14_618[,2:20], id.var="num.thresh"), 1~variable+num.thresh)
names(prebigboi4)<-paste0(names(prebigboi4),"_operable_thresh_618")

prebigboi5<-dcast(melt(asa7[,c("out.names","male.adj.pct","female.adj.pct","total.adj.pct","extrapolated.male.n","extrapolated.female.n","extrapolated.total.n")], id.var="out.names"), 1~variable+out.names)

causes<-sum6[1:13,]
causes<-causes[order(causes$blind.pct,decreasing=T),]
top_causes<-causes[1:3,c("principal.cause","blind.n","blind.pct")]
prebigboi6<-dcast(melt(top_causes,id.var="principal.cause"),1~variable+principal.cause)

bigboi<-cbind(prebigboi1,prebigboi2,prebigboi3,prebigboi4,prebigboi5,prebigboi6)
bigboi[1,1]<-ID
names(bigboi)[1]<-"raab_id"