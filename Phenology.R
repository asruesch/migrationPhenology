#this code creates data frames that summarize the migration data for a given species
#there is a data frame created for each species listed below named (e.g., WS.phenology)


setwd("C:/Users/Evan/Desktop/Suckers/USFW Data/")
source("Phenology Function.r")
source("Phenology Regression Function.r")

#These numbers are the species id numbers from the database
WS<-c(51,52,53)
WS.big<-52
WS.big.unk<-c(51,52)
Silver.Lamprey<-4
Northern.Brook.Lamprey<-6
American.Brook.Lamprey<-11
Native.Lamprey.unk<-23
All.native.lamprey<-c(4,6,11,23)
Brown.trout<-c(24,25,26)
Brown.trout.big<-25
Brown.trout.small<-26
Coho<-c(36,205,39)
Coho.small<-36
Coho.big<-39
Rainbow.smelt<-50
Redhorse<-c(57,58,59,60)
Silver.redhorse<-58
Shorthead.redhorse<-59
Golden.redhorse<-60
Northern.pike<-105
Burbot<-110
Walleye<-122
Steel<-c(27,28,29)
Steel.big<-28
Steel.small<-29
Alewife<-20
Sea.lamprey<-1
Longnose<-c(54,55,56)

species<-c("WS","WS.big","WS.big.unk","Silver.Lamprey","Northern.Brook.Lamprey","American.Brook.Lamprey",
           "Native.Lamprey.unk","All.native.lamprey","Brown.trout","Brown.trout.big","Brown.trout.small",
           "Coho","Coho.small","Rainbow.smelt","Redhorse","Silver.redhorse","Shorthead.redhorse",
           "Golden.redhorse","Northern.pike","Burbot","Walleye","Steel","Steel.big","Steel.small","Alewife",
           "Sea.lamprey","Longnose")


for(i in species){assign(paste(i,"phenology",sep="."),read.csv(paste("Phenology files/",i,".csv",sep='')))}


#Everything below this runs the regressions for each species with different thresholds for inclusion
 #and spits out a data frame summarizing the results
results.2<-data.frame("mean.P"=NA,"mean.R2"=NA,"mean.coef"=NA,
                      "median.P"=NA, "median.R2"=NA,"median.coef"=NA,
                      "median2.P"=NA, "median2.R2"=NA,"median2.coef"=NA,"n"=NA)

for(i in species){results.2[match(i,species),]<-phenology.lm(get(paste(i,
                                                ".phenology",sep="")),2,peak=T)}
rownames(results.2)<-species

results.10<-data.frame("mean.P"=NA,"mean.R2"=NA,"mean.coef"=NA,
                       "median.P"=NA, "median.R2"=NA,"median.coef"=NA,
                       "median2.P"=NA, "median2.R2"=NA,"median2.coef"=NA,"n"=NA,
                       "min.year"=NA,"max.year"=NA)
for(i in species){if(nrow(get(paste(i,".phenology",sep=""))[get(paste(i,".phenology",sep=""))$num>10,])==0) 
  results.10[match(i,species),]<-NA else 
  results.10[match(i,species),]<-phenology.lm(get(paste(i,".phenology",sep="")),10,peak=T)
  results.10[match(i,species),c("min.year","max.year")]<-range(get(paste(i,".phenology",sep=""))[
    get(paste(i,".phenology",sep=""))$num>10&!is.na(get(paste(i,".phenology",sep=""))$Lat),"Year"])
}

rownames(results.10)<-species

results.25<-data.frame("mean.P"=NA,"mean.R2"=NA,"mean.coef"=NA,
                       "median.P"=NA, "median.R2"=NA,"median.coef"=NA,
                       "median2.P"=NA, "median2.R2"=NA,"median2.coef"=NA,"n"=NA,"min.year"=NA,"max.year"=NA)
for(i in species){if(nrow(get(paste(i,".phenology",sep=""))[get(paste(i,".phenology",sep=""))$num>25,])<=1|
                       length(unique(get(paste(i,".phenology",sep=""))[
                         get(paste(i,".phenology",sep=""))$num>25&!is.na(
                           get(paste(i,".phenology",sep=""))$Lat),"Lat"]))==1) 
                    results.25[match(i,species),]<-NA else 
                      results.25[match(i,species),]<-phenology.lm(get(paste(i,".phenology",sep="")),25)
                  results.25[match(i,species),c("min.year","max.year")]<-range(get(paste(i,".phenology",sep=""))[
                    get(paste(i,".phenology",sep=""))$num>25&!is.na(get(paste(i,".phenology",sep=""))$Lat),"Year"])}
rownames(results.25)<-species

results.peak<-data.frame("mean.P"=NA,"mean.R2"=NA,"mean.coef"=NA,
                         "median.P"=NA, "median.R2"=NA,"median.coef"=NA,
                         "median2.P"=NA, "median2.R2"=NA,"median2.coef"=NA,"n"=NA)
for(i in species){if(nrow(get(paste(i,".phenology",sep=""))[get(paste(i,".phenology",sep=""))$peak>
                                                              get(paste(i,".phenology",sep=""))$min,])<2|
                       length(unique(get(paste(i,".phenology",sep=""))[
                         get(paste(i,".phenology",sep=""))$peak>
                           get(paste(i,".phenology",sep=""))$min&!is.na(
                           get(paste(i,".phenology",sep=""))$Lat),"Lat"]))==1) 
  results.peak[match(i,species),]<-NA else 
    results.peak[match(i,species),]<-phenology.lm(get(paste(i,".phenology",sep="")),1,peak=T)}
rownames(results.peak)<-species


results.50<-data.frame("mean.P"=NA,"mean.R2"=NA,"mean.coef"=NA,
                       "median.P"=NA, "median.R2"=NA,"median.coef"=NA,
                       "median2.P"=NA, "median2.R2"=NA,"median2.coef"=NA,"n"=NA,"min.year"=NA,"max.year"=NA)
for(i in species){if(nrow(get(paste(i,".phenology",sep=""))[get(paste(i,".phenology",sep=""))$num>50,])<=1|
                       length(unique(get(paste(i,".phenology",sep=""))[
                         get(paste(i,".phenology",sep=""))$num>50&!is.na(
                           get(paste(i,".phenology",sep=""))$Lat),"Lat"]))==1) 
  results.50[match(i,species),]<-NA else 
    results.50[match(i,species),]<-phenology.lm(get(paste(i,".phenology",sep="")),50)
                  results.50[match(i,species),c("min.year","max.year")]<-range(get(paste(i,".phenology",sep=""))[
                    get(paste(i,".phenology",sep=""))$num>50&!is.na(get(paste(i,".phenology",sep=""))$Lat),"Year"])}
rownames(results.50)<-species


