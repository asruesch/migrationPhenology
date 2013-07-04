library(animation)
library(maps)
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
loc = read.csv(paste(home
                     , "/Dropbox/mcintyreMigrationProject/data/Lat_Long.csv"
                     , sep = "")
)
d = merge(x=d, y=loc, by.x="STREAM_NAME", by.y="Stream", all.x=T)

species = 52
sp_data = d[d$SPECIES == species,]

minDate = min(sp_data$ASSESSMENT_DATE, na.rm=T)
maxDate = max(sp_data$ASSESSMENT_DATE, na.rm=T)

ts = minDate:maxDate
timeSeries = seq(as.Date(minDate), as.Date(maxDate), 1)

for (date in timeSeries) {
	map('state'
		, region=c('michigan', 'wisconsin', 'minnesota', 'illinois', 'indiana'
		, 'pennsylvania', 'new york', 'ohio')
	)
    sp_t_data = sp_data[sp_data$ASSESSMENT_DATE == date]
}