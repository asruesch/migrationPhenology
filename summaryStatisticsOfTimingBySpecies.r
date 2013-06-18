d = read.csv("~/Dropbox/mcintyreMigrationProject/data/compiled data.csv")
d$ASSESSMENT_DATE = strptime(d$ASSESSMENT_DATE, format="%m/%d/%Y")

combos = unique(cbind(d$STREAM, d$SPECIES, d$Year))

summStats = data.frame(        
    STREAM = rep(NA, nrow(combos))
    , SPECIES = rep(NA, nrow(combos))
    , Year = rep(NA, nrow(combos))
    , nObs = rep(NA, nrow(combos))
    , nFreq = rep(NA, nrow(combos))
    , dateOfFirst = rep(NA, nrow(combos))
    , dateOfLast = rep(NA, nrow(combos))
    , dateOfQ10 = rep(NA, nrow(combos))
    , dateOfQ25 = rep(NA, nrow(combos))
    , dateOfQ50 = rep(NA, nrow(combos))
    , dateOfQ75 = rep(NA, nrow(combos))
    , dateOfQ90 = rep(NA, nrow(combos))
)
for (r in 1:nrow(combos)) {
    print(r)
    comboData = d[d$STREAM == combos[r,1] & d$SPECIES == combos[r,2] & d$Year == combos[r,3],]
    comboData = comboData[order(comboData$ASSESSMENT_DATE),]
    n = nrow(comboData)
    N = sum(comboData$TotalFrequency)
    first = max(comboData$ASSESSMENT_DATE)
    last = min(comboData$ASSESSMENT_DATE)
    cumsumFreq = cumsum(comboData$TotalFrequency)
    quants = N * c(0.1, 0.25, 0.5, 0.75, 0.9)
    inds = sapply(quants, function(x) {which.min(abs(x - cumsumFreq))})
    dates = d$ASSESSMENT_DATE[inds]
    outRow = data.frame(
        STREAM = as.integer(combos[row,1])
        , SPECIES = as.integer(combos[row,2])
        , Year = as.integer(combos[row,3])
        , nObs = as.integer(n)
        , nFreq = as.integer(N)
        , dateOfFirst = as.character(first)
        , dateOfLast = as.character(last)
        , dateOfQ10 = as.character(dates[1])
        , dateOfQ25 = as.character(dates[2])
        , dateOfQ50 = as.character(dates[3])
        , dateOfQ75 = as.character(dates[4])
        , dateOfQ90 = as.character(dates[5])
    )
    summStats[row,] = outRow
    
}
write.csv(summStats, file="~/Dropbox/mcintyreMigrationProject/stats/summaryStatisticsOfTimingBySpecies.csv", row.names = F)