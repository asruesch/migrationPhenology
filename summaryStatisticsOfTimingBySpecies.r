if (grep("windows", tolower(Sys.getenv()["OS"])) == 1) {
    home  = Sys.getenv()["USERPROFILE"]
} else {
    home = "~"
}

d = read.csv(paste(home
                   , "/Dropbox/mcintyreMigrationProject/data/compiled data.csv"
                   , sep = "")
)
d$ASSESSMENT_DATE = strptime(d$ASSESSMENT_DATE, format="%m/%d/%Y")

combos = unique(cbind(d$STREAM, d$SPECIES, d$Year))
  
STREAM = rep(NA, nrow(combos))
SPECIES = rep(NA, nrow(combos))
Year = rep(NA, nrow(combos))
nObs = rep(NA, nrow(combos))
nFish = rep(NA, nrow(combos))
dateOfFirst = rep("", nrow(combos))
dateOfLast = rep("", nrow(combos))
dateOfQ10 = rep("", nrow(combos))
dateOfQ25 = rep("", nrow(combos))
dateOfQ50 = rep("", nrow(combos))
dateOfQ75 = rep("", nrow(combos))
dateOfQ90 = rep("", nrow(combos))

for (r in 1:nrow(combos)) {
    print(r)
    comboData = d[d$STREAM == combos[r,1] & d$SPECIES == combos[r,2] & d$Year == combos[r,3],]
    comboData = comboData[which(!is.na(comboData$ASSESSMENT_DATE)),]
    if (dim(comboData)[1] == 0) {
        next
    }
    comboData = comboData[order(comboData$ASSESSMENT_DATE),]
    n = nrow(comboData)
    N = sum(comboData$TotalFrequency)
    first = min(comboData$ASSESSMENT_DATE, na.rm=T)
    last = max(comboData$ASSESSMENT_DATE, na.rm=T)
    cumsumFreq = cumsum(comboData$TotalFrequency)
    quants = N * c(0.1, 0.25, 0.5, 0.75, 0.9)
    inds = sapply(quants, function(x) {which.min(abs(x - cumsumFreq))})
    dates = comboData$ASSESSMENT_DATE[inds]
    STREAM[r] = as.integer(combos[r,1])
    SPECIES[r] = as.integer(combos[r,2])
    Year[r] = as.integer(combos[r,3])
    nObs[r] = as.integer(n)
    nFish[r] = as.integer(N)
    dateOfFirst[r] = as.character(first)
    dateOfLast[r] = as.character(last)
    dateOfQ10[r] = as.character(dates[1])
    dateOfQ25[r] = as.character(dates[2])
    dateOfQ50[r] = as.character(dates[3])
    dateOfQ75[r] = as.character(dates[4])
    dateOfQ90[r] = as.character(dates[5])
}
outDF = data.frame(
    STREAM = STREAM
    , SPECIES = SPECIES
    , Year = Year
    , nObs = nObs
    , nFish = nFish
    , dateOfFirst = dateOfFirst
    , dateOfLast = dateOfLast
    , dateOfQ10 = dateOfQ10
    , dateOfQ25 = dateOfQ25
    , dateOfQ50 = dateOfQ50
    , dateOfQ75 = dateOfQ75
    , dateOfQ90 = dateOfQ90
)
write.csv(outDF
          , file=paste(home 
                       , "/Dropbox/mcintyreMigrationProject/stats/summaryStatisticsOfTimingBySpeciesSiteYear.csv"
                       , sep="")
          , row.names = F
)