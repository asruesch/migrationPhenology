#the first function just creates a vector of days with each number representing a fish and takes the median of that

median.day<-function(a){median(rep(a$Day,a$TotalFrequency))}

#I can't remember why I created this alternate method for calculating the median
median.day2<-function(data){
a<-na.omit(melt(lapply(split(data,list(data$Year,data$STREAM_NAME)),median.day)))

a$Year<-as.numeric(unlist(lapply(strsplit(a[,2],"\\."),"[",1)))

a$X1<-unlist(lapply(strsplit(a[,2],"\\."),"[",2))
a$X2<-unlist(lapply(strsplit(a[,2],"\\."),"[",3))
a$X3<-unlist(lapply(strsplit(a[,2],"\\."),"[",4))
a$X4<-unlist(lapply(strsplit(a[,2],"\\."),"[",5))
a$X5<-unlist(lapply(strsplit(a[,2],"\\."),"[",6))
a$Stream<-paste(a$X1,a$X2,sep=".")
a[!is.na(a$X3),]$Stream<-paste(a[!is.na(a$X3),"Stream"],
                               a[!is.na(a$X3),"X3"],sep=".")
a[!is.na(a$X4),]$Stream<-paste(a[!is.na(a$X4),"Stream"],
                               a[!is.na(a$X4),"X4"],sep=".")
a[!is.na(a$X5),]$Stream<-paste(a[!is.na(a$X5),"Stream"],
                               a[!is.na(a$X5),"X5"],sep=".")
a<-a[,c("value","Year","Stream")]
a<-a[order(a$Year,a$Stream),]
return(a$value)
}
