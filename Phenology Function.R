phenology<-function(Species.num){
  setwd("C:/Users/Evan/Desktop/Suckers/USFW Data/")
  unique.length<-function(x){length(unique(x))}
  require(reshape)
  source("median.day.r")
  data<-read.csv("compiled data.csv")
  
  #This is a clunky way of dealing with the variable number of IDs for each fish species.
  l<-length(Species.num)
  if(l==1){data<-data[data$SPECIES==Species.num,]}
  if(l==2){data<-data[data$SPECIES==Species.num[1]|data$SPECIES==Species.num[2],]}
  if(l==3){data<-data[data$SPECIES==Species.num[1]|data$SPECIES==Species.num[2]|
                         data$SPECIES==Species.num[3],]}
  if(l==4){data<-data[data$SPECIES==Species.num[1]|data$SPECIES==Species.num[2]|
                         data$SPECIES==Species.num[3]|data$SPECIES==Species.num[4],]}
  
  max<-data.frame(melt(tapply(data$TotalFrequency,list(data$STREAM_NAME,data$Year),max)))
  
  #this identifies the maximum caught on a single day
  #and then gives the day of the year for each stream/year combo
  max<-na.omit(max)
  peak<-NULL
  for(i in c(1:length(max$X1))){peak<-c(peak,
    subset(subset(data,Year==max$X2[i]),STREAM_NAME==max$X1[i])$Day[match(max$value[i],
    subset(subset(data,Year==max$X2[i]),STREAM_NAME==max$X1[i])$TotalFrequency)])}
  
  #gives the total number of fish caught in a given year/stream
  num<-melt(tapply(data$TotalFrequency,list(data$STREAM_NAME,data$Year),sum),varnames=c("Stream","Year"))
  
  #calculates the date the average fish migrated in each year/stream
  data$X<-data$Day*data$TotalFrequency
  mean<-melt(tapply(data$X,list(data$STREAM_NAME,data$Year),sum))
  mean$value<-mean$value/num$value
  
  median2<-median.day2(data)
  
  #caculates the day the median fish migrated in each year/stream
  median<-melt(tapply(data$Day,list(data$STREAM_NAME,data$Year),median))
  
  #gives the first day a fish was observed
  min<-melt(tapply(data$Day,list(data$STREAM_NAME,data$Year),min))
  
  #gives the number of days on which fish were observed in each year/stream
  days<-melt(tapply(data$Day,list(data$STREAM_NAME,data$Year),unique.length))
  
  
  #creates a data frame of the outputs
  phenology<-na.omit(data.frame(cbind(num,mean[,3],median[,3],min[,3],days[,3])))
  names(phenology)<-c("Stream","Year","num","mean","median","min","days")
  phenology$median2<-median2
  phenology$peak<-peak
  
  #adds lat long to the data frame
  coor<-read.csv("C:/Users/Evan/Desktop/Suckers/USFW Data/Lat_Long.csv")
  for(i in c(1:length(phenology$min))){phenology$Lat[i]<-coor$Lat[match(phenology$Stream[i],coor$Stream)]}
  for(i in c(1:length(phenology$min))){phenology$Long[i]<-coor$Long[match(phenology$Stream[i],coor$Stream)]}
  
return(phenology)

} 
