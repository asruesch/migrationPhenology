
# loc1 : Lake Superior – 46.9 N; 90.4 W; October 1- Nov 15
# loc3 : Lake Michigan – 42.55; 87.45; October 1 – November 15
# loc3 : Lake Michigan – 42.55; 87.45; May 1 – June 30
# loc2 : Lake Michigan, - 44.38; 87.58; Mar 15 – May 25

import os, datetime, arcpy, time
from arcpy.sa import *
from arcpy import env
arcpy.CheckOutExtension('Spatial')
env.overwriteOutput = True

if os.environ['OS'] == 'Windows_NT':
	home  = os.environ["USERPROFILE"]
else:
	home = "~"

dataDir = 'G:/glSST/temp_data_PeteM/geodatabase'
locDir = 'G:/glSST/samplingLocations'
outDir = home + '/Dropbox/mcintyreMigrationProject/stats/SST'
years = range(1994, 2013)
jdLists = {
	'loc1' : {}
	, 'loc2' : {}
	, 'loc3a' : {}
	, 'loc3b' : {}
}
dateRanges = {
	'loc1' : [[10,1],[11,16]]
	, 'loc2' : [[3,15],[5,26]]
	, 'loc3a' : [[10,1],[11,16]]
	, 'loc3b' : [[4,1],[7,1]]
}
lakes = {
	'loc1' : 'ls'
	, 'loc2' : 'lm'
	, 'loc3a' : 'lm'
	, 'loc3b' : 'lm'
}

tempDir = os.environ['TEMP']
tempGdb = tempDir + '/temp.gdb'
if not os.path.exists(tempGdb):
	arcpy.CreateFileGDB_management(tempDir, "temp.gdb", "CURRENT")

env.workspace = tempGdb
env.scratchWorkspace = tempGdb

def julianDay(startDate, nDays, x):
	date = startDate + datetime.timedelta(days=x)
	jd = date.timetuple().tm_yday
	jd = "%03d" % jd
	return [date,jd]

def buildDateDict(jdLists, years, dateRanges):
	for l,loc in enumerate(jdLists.keys()):
		for year in years:
			startDate = datetime.date(year, dateRanges[loc][0][0], dateRanges[loc][0][1])
			endDate = datetime.date(year, dateRanges[loc][1][0], dateRanges[loc][1][1])
			nDays = endDate - startDate
			jdList = [julianDay(startDate, nDays, x) for x in range(0,nDays.days)]
			jdLists[loc].update({str(year) : jdList})
	return jdLists

jdLists = buildDateDict(jdLists, years, dateRanges)

for loc in jdLists.keys():
	locRasterFile = locDir + '/' + loc[0:4] + '.tif'
	locRaster = Raster(locRasterFile)
	env.extent = locRasterFile
	outFile = outDir + '/' + loc + '.csv'
	f = open(outFile, 'w')
	f.write('year,month,day,tmean\n')
	for year in years:
		for date in jdLists[loc][str(year)]:
			tic = time.clock()
			jd = date[1]
			mo = date[0].month
			day = date[0].day
			print loc + ': ' + str(year) + '/' + str(mo) + '/' + str(day)
			if (int(jd) < 182) and (year <= 1994):
				continue
			lake = lakes[loc]
			gdb = dataDir + '/' + lake + '/' + lake + '_daily_SST_' + str(year) + '.gdb'
			if year <= 2005:
				sstRaster = gdb + '/' + lake + '_' + str(year)[2:4] + str(jd) + '_noaa'
			else:
				sstRaster = gdb + '/' + lake + '_' + str(year) + str(jd)
			table = tempGdb + '/' + lake + '_' + str(year) + str(jd)
			ZonalStatisticsAsTable(locRaster, 'Value', sstRaster\
				, table, 'DATA', 'MEAN')
			rows = arcpy.SearchCursor(table)
			for row in rows:
				tmean = row.MEAN
			del row, rows
			f.write(str(year) + ',' + str(mo) + ',' + str(day) + ',' + str(tmean) + '\n')
			toc = time.clock()
			print(toc - tic)
	f.close()
