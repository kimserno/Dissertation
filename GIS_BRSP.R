

library(rgdal)
library(raster)
library(sp)
library(prism)
library(rgeos)
#setwd("C:/Users/kim_serno1/Dropbox/Baylor/PhD/Dissertation")
setwd("~/Dropbox/Baylor/PhD/Dissertation")

#load in gap lc rasters to mosaic together for gaplc west raster -----
# R2<-raster("C:/Users/kim_serno1/Dropbox/Baylor/PhD/Dissertation/GIS/unzipped/gap_landcover/Region2_California/gaplf2011lc_v30_lcc_2.tif")
# R11<-raster("C:/Users/kim_serno1/Dropbox/Baylor/PhD/Dissertation/GIS/unzipped/gap_landcover/Region11_NorthPacific/gaplf2011lc_v30_lcc_11.tif")
# R13<-raster("C:/Users/kim_serno1/Dropbox/Baylor/PhD/Dissertation/GIS/unzipped/gap_landcover/Region13_PlainsPrairiePotholes/gaplf2011lc_v30_lcc_13.tif")
# R15<-raster("C:/Users/kim_serno1/Dropbox/Baylor/PhD/Dissertation/GIS/unzipped/gap_landcover/Region15_SouthernRockies/gaplf2011lc_v30_lcc_15.tif")
# R3<-raster("C:/Users/kim_serno1/Dropbox/Baylor/PhD/Dissertation/GIS/unzipped/gap_landcover/Region3_Desert/gaplf2011lc_v30_lcc_3.tif")
# R5<-raster("C:/Users/kim_serno1/Dropbox/Baylor/PhD/Dissertation/GIS/unzipped/gap_landcover/Region5_GreatBasin/gaplf2011lc_v30_lcc_5.tif")
# R6<-raster("C:/Users/kim_serno1/Dropbox/Baylor/PhD/Dissertation/GIS/unzipped/gap_landcover/Region6_GreatNorthern/gaplf2011lc_v30_lcc_6.tif")
# R7<-raster("C:/Users/kim_serno1/Dropbox/Baylor/PhD/Dissertation/GIS/unzipped/gap_landcover/Region7_GreatPlains/gaplf2011lc_v30_lcc_7.tif")

# R2<-raster("~/Dropbox/Baylor/PhD/GIS/unzipped/gap_landcover/Region2_California/gaplf2011lc_v30_lcc_2.tif")
# R11<-raster("~/Dropbox/Baylor/PhD/GIS/unzipped/gap_landcover/Region11_NorthPacific/gaplf2011lc_v30_lcc_11.tif")
# R13<-raster("~/Dropbox/Baylor/PhD/GIS/unzipped/gap_landcover/Region13_PlainsPrairiePotholes/gaplf2011lc_v30_lcc_13.tif")
# R15<-raster("~/Dropbox/Baylor/PhD/GIS/unzipped/gap_landcover/Region15_SouthernRockies/gaplf2011lc_v30_lcc_15.tif")
# R3<-raster("~/Dropbox/Baylor/PhD/GIS/unzipped/gap_landcover/Region3_Desert/gaplf2011lc_v30_lcc_3.tif")
# R5<-raster("~/Dropbox/Baylor/PhD/GIS/unzipped/gap_landcover/Region5_GreatBasin/gaplf2011lc_v30_lcc_5.tif")
# R6<-raster("~/Dropbox/Baylor/PhD/GIS/unzipped/gap_landcover/Region6_GreatNorthern/gaplf2011lc_v30_lcc_6.tif")
# R7<-raster("~/Dropbox/Baylor/PhD/GIS/unzipped/gap_landcover/Region7_GreatPlains/gaplf2011lc_v30_lcc_7.tif")
# plot(R2)
# proj4string(R2)
# proj4string(R11)
# proj4string(R13)
# proj4string(R15)
# proj4string(R3)
# proj4string(R5)
# proj4string(R6)
# proj4string(R7)

#Creation of gaplc_west raster and timing
# start.time = Sys.time()
# Gaplc_West_raster<-mosaic(R2, R11, R13, R15, R3, R5, R6, R7, fun = mean,  filename = "~/Dropbox/Baylor/PhD/GIS/unzipped/gap_landcover/Gaplc_West_raster.tif")
# end.time = Sys.time(); elapsed.time = round(difftime(end.time,start.time, units='mins'), dig = 2)
# paste('Elapsed time was ', elapsed.time, 'minutes or ', round(elapsed.time/60,3), 'hours', sep = '')
# r2r11<-mosaic(R2, R11, fun = mean)
#brsp_GDB<-"C:/Users/kim_serno1/Documents/GIS_projects/Dissertation/data/BRSP.gdb"

#import shapefiles PC: ----
BRSP_breeding<-readOGR(dsn="C:/Users/kim_serno1/Documents/PhD/GIS/Dissertation/data/unzipped/Shapefiles", layer = "BRSP_BreedingUS")
plot(BRSP_breeding)
#summary(BRSP_breeding)

us<-readOGR(dsn="C:/Users/kim_serno1/Documents/PhD/GIS/Dissertation/data/unzipped/Shapefiles", layer = "US_48States")
plot(us, add=TRUE)
#summary(us)


BRSP_routes<-readOGR(dsn="C:/Users/kim_serno1/Documents/PhD/GIS/Dissertation/data/unzipped/Shapefiles", layer = "BBS_routes_BRSP")
plot(BRSP_routes, add = TRUE)
#summary(BRSP_routes)

# BRSP_mincenter<-readOGR(dsn="C:/Users/kim_serno1/Documents/PhD/GIS/Dissertation/data/unzipped/Shapefiles", layer = "BRSP_MinimumBound_center")
# plot(BRSP_mincenter, add = TRUE)
# summary(BRSP_mincenter)

Gaplc_west<-raster("C:/Users/kim_serno1/Documents/PhD/GIS/Dissertation/data/unzipped/gap_landcover/Gaplc_West_raster.tif")

#import shapefiles Mac: ----
brsp_GDB<-"~/Dropbox/Baylor/PhD/GIS/BRSP.gdb"

subset(ogrDrivers(), grepl("GDB", name))
fc_list<-ogrListLayers(brsp_GDB)
print(fc_list)

BRSP_breeding<-readOGR(dsn=brsp_GDB, layer = "BRSP_BreedingUS")
plot(BRSP_breeding)
#summary(BRSP_breeding)

us<-readOGR(dsn=brsp_GDB, layer = "US_48States")
plot(us, add=TRUE)
#summary(us)


BRSP_routes<-readOGR(dsn=brsp_GDB, layer = "BBS_routes_BRSP")
plot(BRSP_routes, add = TRUE)
#summary(BRSP_routes)

BRSP_mincenter<-readOGR(dsn=brsp_GDB, layer = "BRSP_MinimumBound_center")
# plot(BRSP_mincenter, add = TRUE)
# summary(BRSP_mincenter)


# Load in Gap Landcover west raster (creation code above)
Gaplc_west<-raster("~/Dropbox/Baylor/PhD/GIS/unzipped/gap_landcover/Gaplc_West_raster.tif")


# Get projection information for gap landcover raster: ----
CRS_gap<-"+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
CRS_gap

US<-spTransform(us, CRS = CRS_gap)
BRSP_Breeding<-spTransform(BRSP_breeding, CRS = CRS_gap)
BRSP_Routes<-spTransform(BRSP_routes, CRS = CRS_gap)
BRSP_Mincenter<-spTransform(BRSP_mincenter, CRS = CRS_gap)
proj4string(US)
proj4string(BRSP_Breeding)
proj4string(BRSP_Routes)
proj4string(BRSP_Mincenter)

# plot land cover raster with BRSP breeding range and BBS routes overlay
plot(Gaplc_west)
plot(BRSP_Breeding, add = TRUE)
plot(BRSP_Routes, add = TRUE)


#buffers around routes ------
BRSP_1km<-gBuffer(BRSP_Routes, width = 1000, byid = TRUE, id=BRSP_Routes@data$RouteID)
plot(BRSP_1km, add = TRUE)
summary(BRSP_1km)

BRSP_400m<-gBuffer(BRSP_Routes, width = 400, byid = TRUE, id=BRSP_Routes@data$RouteID)
plot(BRSP_400m, add = TRUE)
summary(BRSP_400m)

BRSP_5km<-gBuffer(BRSP_Routes, width = 5000, byid = TRUE, id=BRSP_Routes@data$RouteID)
plot(BRSP_5km, add = TRUE)
summary(BRSP_5km)

BRSP_4km<-gBuffer(BRSP_Routes, width = 4000, byid = TRUE, id=BRSP_Routes@data$RouteID)
plot(BRSP_4km, add = TRUE)
summary(BRSP_4km)

BRSP_8km<-gBuffer(BRSP_Routes, width = 8000, byid = TRUE, id=BRSP_Routes@data$RouteID)
plot(BRSP_8km, add = TRUE)
summary(BRSP_8km)

# Buffers around minimum bounding rectangle center (20km) 
BRSP_20km<-gBuffer(BRSP_Mincenter, width = 20000, byid = TRUE, id=BRSP_Mincenter@data$RouteID)
plot(BRSP_20km, add = TRUE)
summary(BRSP_20km)


#extract gap lc values for buffer: -----
start.time = Sys.time()
e400m<-extract(Gaplc_west, BRSP_400m)
e400mTable<-lapply(e400m, table)
end.time = Sys.time(); elapsed.time = round(difftime(end.time,start.time, units='mins'), dig = 2)
paste('Elapsed time was ', elapsed.time, 'minutes or ', round(elapsed.time/60,3), 'hours', sep = '')

start.time = Sys.time()
e1km<-extract(Gaplc_west, BRSP_1km)
e1kmTable<-lapply(e1km, table)
end.time = Sys.time(); elapsed.time = round(difftime(end.time,start.time, units='mins'), dig = 2)
paste('Elapsed time was ', elapsed.time, 'minutes or ', round(elapsed.time/60,3), 'hours', sep = '')
start.time = Sys.time()
e5km<-extract(Gaplc_west, BRSP_5km)
e5kmTable<-lapply(e5km, table)
end.time = Sys.time(); elapsed.time = round(difftime(end.time,start.time, units='mins'), dig = 2)
paste('Elapsed time was ', elapsed.time, 'minutes or ', round(elapsed.time/60,3), 'hours', sep = '')

start.time = Sys.time()
e4km<-extract(Gaplc_west, BRSP_4km)
e4kmTable<-lapply(e4km, table)
end.time = Sys.time(); elapsed.time = round(difftime(end.time,start.time, units='mins'), dig = 2)
paste('Elapsed time was ', elapsed.time, 'minutes or ', round(elapsed.time/60,3), 'hours', sep = '')

start.time = Sys.time()
e8km<-extract(Gaplc_west, BRSP_8km)
e8kmTable<-lapply(e8km, table)
end.time = Sys.time(); elapsed.time = round(difftime(end.time,start.time, units='mins'), dig = 2)
paste('Elapsed time was ', elapsed.time, 'minutes or ', round(elapsed.time/60,3), 'hours', sep = '')

# convert to data frame -------
e400mdf <- data.frame(id = rep(BRSP_Routes$RouteID, lapply(e400mTable, length)),
                   cover = names(unlist(e400mTable)),
                   count = unlist(e400mTable)
)
head(e400mdf)
e1kmdf <- data.frame(id = rep(BRSP_Routes$RouteID, lapply(e1kmTable, length)),
                      cover = names(unlist(e1kmTable)),
                      count = unlist(e1kmTable)
)
head(e1kmdf)

e5kmdf <- data.frame(id = rep(BRSP_Routes$RouteID, lapply(e5kmTable, length)),
                      cover = names(unlist(e5kmTable)),
                      count = unlist(e5kmTable)
)
head(e5kmdf)

e20kmdf <- data.frame(id = rep(BRSP_Routes$RouteID, lapply(e20kmTable, length)),
                     cover = names(unlist(e20kmTable)),
                     count = unlist(e20kmTable)
)
head(e20kmdf)

e4kmdf <- data.frame(id = rep(BRSP_Routes$RouteID, lapply(e4kmTable, length)),
                      cover = names(unlist(e4kmTable)),
                      count = unlist(e4kmTable)
)
head(e4kmdf)

e8kmdf <- data.frame(id = rep(BRSP_Routes$RouteID, lapply(e8kmTable, length)),
                      cover = names(unlist(e8kmTable)),
                      count = unlist(e8kmTable)
)
head(e8kmdf)

# test<-extract(Gaplc_west, BRSP_breeding, method = 'simple')
# test<-as.matrix(table(values(Gaplc_west)))


# VegDRI data read in ---------
#2009
# may1709<-raster("/Volumes/Samsung_USB/VegDRI/2009/vegdri_emodis_week20_051709.tif")
may1709<-raster("C:/Users/kim_serno1/Documents/PhD/GIS/Dissertation/data/unzipped/VegDRI/2009/vegdri_emodis_week20_051709.tif")
# plot(may1709)
proj4string(may1709)
May1709<-projectRaster(may1709, crs = CRS_gap)
proj4string(May1709)
summary(May1709@data@values)
# #check visually
# plot(BRSP_Breeding)
# plot(May1709, add= TRUE)
# plot(BRSP_Routes, add = TRUE)
# plot(BRSP_Breeding, add = TRUE)
May1709[is.na(May1709[])]<-0
summary(May1709@data@values)
dim(May1709)
rcl<-c(252,256,0)
rcl2<-matrix(rcl, ncol=3, byrow=TRUE)
May1709<-reclassify(May1709, rcl2)
# plot(May1709)
summary(May1709@data@values)
May1709[May1709 == 0]<-NA
summary(May1709@data@values)

#Week 25 not available so using week 24:
# june1409<-raster("/Volumes/Samsung_USB/VegDRI/2009/vegdri_emodis_week24_061409.tif")
june1409<-raster("C:/Users/kim_serno1/Documents/PhD/GIS/Dissertation/data/unzipped/VegDRI/2009/vegdri_emodis_week24_061409.tif")
# plot(june1409)
proj4string(june1409)
June1409<-projectRaster(june1409, crs = CRS_gap)
proj4string(June1409)
summary(June1409@data@values)
# #check visually
# plot(BRSP_Breeding)
# plot(June1409, add= TRUE)
# plot(BRSP_Routes, add = TRUE)
# plot(BRSP_Breeding, add = TRUE)
June1409[is.na(June1409)]<-0
summary(June1409@data@values)
dim(June1409)
rcl<-c(252,256,0)
rcl2<-matrix(rcl, ncol=3, byrow=TRUE)
June1409<-reclassify(June1409, rcl2)
# plot(June1409)
summary(June1409@data@values)
June1409[June1409 == 0]<-NA

# july2609<-raster("/Volumes/Samsung_USB/VegDRI/2009/vegdri_emodis_week30_072609.tif")
july2609<-raster("C:/Users/kim_serno1/Documents/PhD/GIS/Dissertation/data/unzipped/VegDRI/2009/vegdri_emodis_week30_072609.tif")
# plot(july2609)
proj4string(july2609)
July2609<-projectRaster(july2609, crs = CRS_gap)
proj4string(July2609)
summary(July2609@data@values)
# # check visually
# plot(BRSP_Breeding)
# plot(July2609, add= TRUE)
# plot(BRSP_Routes, add = TRUE)
# plot(BRSP_Breeding, add = TRUE)
# July2609[is.na(July2609)]<-0
summary(July2609@data@values)
dim(July2609)
rcl<-c(252,256,0)
rcl2<-matrix(rcl, ncol=3, byrow=TRUE)
July2609<-reclassify(July2609, rcl2)
# plot(July2609)
summary(July2609@data@values)
July2609[July2609 == 0]<-NA

#2010
# may1610<-raster("/Volumes/Samsung_USB/VegDRI/2010/vegdri_emodis_week20_051610.tif")
may1610<-raster("C:/Users/kim_serno1/Documents/PhD/GIS/Dissertation/data/unzipped/VegDRI/2010/vegdri_emodis_week20_051610.tif")
# plot(may1610)
proj4string(may1610)
May1610<-projectRaster(may1610, crs = CRS_gap)
proj4string(May1610)
summary(May1610@data@values)
# #check visually
# plot(BRSP_Breeding)
# plot(May1610, add= TRUE)
# plot(BRSP_Routes, add = TRUE)
# plot(BRSP_Breeding, add = TRUE)
May1610[is.na(May1610[])]<-0
summary(May1610@data@values)
dim(May1610)
rcl<-c(252,256,0)
rcl2<-matrix(rcl, ncol=3, byrow=TRUE)
May1610<-reclassify(May1610, rcl2)
# plot(May1610)
summary(May1610@data@values)
May1610[May1610 == 0]<-NA

# june2010<-raster("/Volumes/Samsung_USB/VegDRI/2010/vegdri_emodis_week25_062010.tif")
june2010<-raster("C:/Users/kim_serno1/Documents/PhD/GIS/Dissertation/data/unzipped/VegDRI/2010/vegdri_emodis_week25_062010.tif")
# plot(june2010)
proj4string(june2010)
June2010<-projectRaster(june2010, crs = CRS_gap)
proj4string(June2010)
summary(June2010@data@values)
# #check visually
# plot(BRSP_Breeding)
# plot(June2010, add= TRUE)
# plot(BRSP_Routes, add = TRUE)
# plot(BRSP_Breeding, add = TRUE)
June2010[is.na(June2010)]<-0
summary(June2010@data@values)
dim(June2010)
rcl<-c(252,256,0)
rcl2<-matrix(rcl, ncol=3, byrow=TRUE)
June2010<-reclassify(June2010, rcl2)
# plot(June2010)
summary(June2010@data@values)
June2010[June2010 == 0]<-NA

# july2510<-raster("/Volumes/Samsung_USB/VegDRI/2010/vegdri_emodis_week30_072510.tif")
july2510<-raster("C:/Users/kim_serno1/Documents/PhD/GIS/Dissertation/data/unzipped/VegDRI/2010/vegdri_emodis_week30_072510.tif")
# plot(july2510)
proj4string(july2510)
July2510<-projectRaster(july2510, crs = CRS_gap)
proj4string(July2510)
summary(July2510@data@values)
# #check visually
# plot(BRSP_Breeding)
# plot(July2510, add= TRUE)
# plot(BRSP_Routes, add = TRUE)
# plot(BRSP_Breeding, add = TRUE)
July2510[is.na(July2510)]<-0
summary(July2510@data@values)
dim(July2510)
rcl<-c(252,256,0)
rcl2<-matrix(rcl, ncol=3, byrow=TRUE)
July2510<-reclassify(July2510, rcl2)
# plot(July2510)
summary(July2510@data@values)
July2510[July2510 == 0]<-NA

#2011
# may1511<-raster("/Volumes/Samsung_USB/VegDRI/2011/vegdri_emodis_week20_051511.tif")
may1511<-raster("C:/Users/kim_serno1/Documents/PhD/GIS/Dissertation/data/unzipped/VegDRI/2011/vegdri_emodis_week20_051511.tif")
# plot(may1511)
proj4string(may1511)
May1511<-projectRaster(may1511, crs = CRS_gap)
proj4string(May1511)
summary(May1511@data@values)
# #check visually
# plot(BRSP_Breeding)
# plot(May1511, add= TRUE)
# plot(BRSP_Routes, add = TRUE)
# plot(BRSP_Breeding, add = TRUE)
May1511[is.na(May1511[])]<-0
summary(May1511@data@values)
dim(May1511)
rcl<-c(252,256,0)
rcl2<-matrix(rcl, ncol=3, byrow=TRUE)
May1511<-reclassify(May1511, rcl2)
# plot(May1511)
summary(May1511@data@values)
May1511[May1511 == 0]<-NA

# Week 25 not available, so using week 23:
# june0511<-raster("/Volumes/Samsung_USB/VegDRI/2011/vegdri_emodis_week23_060511.tif")
june0511<-raster("C:/Users/kim_serno1/Documents/PhD/GIS/Dissertation/data/unzipped/VegDRI/2011/vegdri_emodis_week23_060511.tif")
plot(june0511)
proj4string(june0511)
June0511<-projectRaster(june0511, crs = CRS_gap)
proj4string(June0511)
summary(June0511@data@values)
#check visually
plot(BRSP_Breeding)
plot(June0511, add= TRUE)
plot(BRSP_Routes, add = TRUE)
plot(BRSP_Breeding, add = TRUE)
June0511[is.na(June0511)]<-0
summary(June0511@data@values)
dim(June0511)
rcl<-c(252,256,0)
rcl2<-matrix(rcl, ncol=3, byrow=TRUE)
June0511<-reclassify(June0511, rcl2)
plot(June0511)
summary(June0511@data@values)
June0511[June0511 == 0]<-NA

# july2411<-raster("/Volumes/Samsung_USB/VegDRI/2011/vegdri_emodis_week30_072411.tif")
july2411<-raster("C:/Users/kim_serno1/Documents/PhD/GIS/Dissertation/data/unzipped/VegDRI/2011/vegdri_emodis_week30_072411.tif")
# plot(july2411)
proj4string(july2411)
July2411<-projectRaster(july2411, crs = CRS_gap)
proj4string(July2411)
summary(July2411@data@values)
# #check visually
# plot(BRSP_Breeding)
# plot(July2411, add= TRUE)
# plot(BRSP_Routes, add = TRUE)
# plot(BRSP_Breeding, add = TRUE)
July2411[is.na(July2411)]<-0
summary(July2411@data@values)
dim(July2411)
rcl<-c(252,256,0)
rcl2<-matrix(rcl, ncol=3, byrow=TRUE)
July2411<-reclassify(July2411, rcl2)
# plot(July2411)
summary(July2411@data@values)
July2411[July2411 == 0]<-NA

#2012
# may1312<-raster("/Volumes/Samsung_USB/VegDRI/2012/vegdri_emodis_week20_051312.tif")
may1312<-raster("C:/Users/kim_serno1/Documents/PhD/GIS/Dissertation/data/unzipped/VegDRI/2012/vegdri_emodis_week20_051312.tif")
# plot(may1312)
proj4string(may1312)
May1312<-projectRaster(may1312, crs = CRS_gap)
proj4string(May1312)
summary(May1312@data@values)
# #check visually
# plot(BRSP_Breeding)
# plot(May1312, add= TRUE)
# plot(BRSP_Routes, add = TRUE)
# plot(BRSP_Breeding, add = TRUE)
May1312[is.na(May1312[])]<-0
summary(May1312@data@values)
dim(May1312)
rcl<-c(252,256,0)
rcl2<-matrix(rcl, ncol=3, byrow=TRUE)
May1312<-reclassify(May1312, rcl2)
# plot(May1312)
summary(May1312@data@values)
May1312[May1312 == 0]<-NA

# june1712<-raster("/Volumes/Samsung_USB/VegDRI/2012/vegdri_emodis_week25_061712.tif")
june1712<-raster("C:/Users/kim_serno1/Documents/PhD/GIS/Dissertation/data/unzipped/VegDRI/2012/vegdri_emodis_week25_061712.tif")
# plot(june1712)
proj4string(june1712)
June1712<-projectRaster(june1712, crs = CRS_gap)
proj4string(June1712)
summary(June1712@data@values)
# #check visually
# plot(BRSP_Breeding)
# plot(June1712, add= TRUE)
# plot(BRSP_Routes, add = TRUE)
# plot(BRSP_Breeding, add = TRUE)
June1712[is.na(June1712)]<-0
summary(June1712@data@values)
dim(June1712)
rcl<-c(252,256,0)
rcl2<-matrix(rcl, ncol=3, byrow=TRUE)
June1712<-reclassify(June1712, rcl2)
# plot(June1712)
summary(June1712@data@values)
June1712[June1712 == 0]<-NA

# july2212<-raster("/Volumes/Samsung_USB/VegDRI/2012/vegdri_emodis_week30_072212.tif")
july2212<-raster("C:/Users/kim_serno1/Documents/PhD/GIS/Dissertation/data/unzipped/VegDRI/2012/vegdri_emodis_week30_072212.tif")
# plot(july2212)
proj4string(july2212)
July2212<-projectRaster(july2212, crs = CRS_gap)
proj4string(July2212)
summary(July2212@data@values)
# #check visually
# plot(BRSP_Breeding)
# plot(July2212, add= TRUE)
# plot(BRSP_Routes, add = TRUE)
# plot(BRSP_Breeding, add = TRUE)
July2212[is.na(July2212)]<-0
summary(July2212@data@values)
dim(July2212)
rcl<-c(252,256,0)
rcl2<-matrix(rcl, ncol=3, byrow=TRUE)
July2212<-reclassify(July2212, rcl2)
# plot(July2212)
summary(July2212@data@values)
July2212[July2212 == 0]<-NA

#2013
# may1913<-raster("/Volumes/Samsung_USB/VegDRI/2013/vegdri_emodis_week20_051913.tif")
may1913<-raster("C:/Users/kim_serno1/Documents/PhD/GIS/Dissertation/data/unzipped/VegDRI/2013/vegdri_emodis_week20_051913.tif")
# plot(may1913)
proj4string(may1913)
May1913<-projectRaster(may1913, crs = CRS_gap)
proj4string(May1913)
summary(May1913@data@values)
# #check visually
# plot(BRSP_Breeding)
# plot(May1913, add= TRUE)
# plot(BRSP_Routes, add = TRUE)
# plot(BRSP_Breeding, add = TRUE)
May1913[is.na(May1913[])]<-0
summary(May1913@data@values)
dim(May1913)
rcl<-c(252,256,0)
rcl2<-matrix(rcl, ncol=3, byrow=TRUE)
May1913<-reclassify(May1913, rcl2)
# plot(May1913)
summary(May1913@data@values)
May1913[May1913 == 0]<-NA

# june2313<-raster("/Volumes/Samsung_USB/VegDRI/2013/vegdri_emodis_week25_062313.tif")
june2313<-raster("C:/Users/kim_serno1/Documents/PhD/GIS/Dissertation/data/unzipped/VegDRI/2013/vegdri_emodis_week25_062313.tif")
# plot(june2313)
proj4string(june2313)
June2313<-projectRaster(june2313, crs = CRS_gap)
proj4string(June2313)
summary(June2313@data@values)
# #check visually
# plot(BRSP_Breeding)
# plot(June2313, add= TRUE)
# plot(BRSP_Routes, add = TRUE)
# plot(BRSP_Breeding, add = TRUE)
June2313[is.na(June2313)]<-0
summary(June2313@data@values)
dim(June2313)
rcl<-c(252,256,0)
rcl2<-matrix(rcl, ncol=3, byrow=TRUE)
June2313<-reclassify(June2313, rcl2)
# plot(June2313)
summary(June2313@data@values)
June2313[June2313 == 0]<-NA

# july2813<-raster("/Volumes/Samsung_USB/VegDRI/2013/vegdri_emodis_week30_072813.tif")
july2813<-raster("C:/Users/kim_serno1/Documents/PhD/GIS/Dissertation/data/unzipped/VegDRI/2013/vegdri_emodis_week30_072813.tif")
# plot(july2813)
proj4string(july2813)
July2813<-projectRaster(july2813, crs = CRS_gap)
proj4string(July2813)
summary(July2813@data@values)
# #check visually
# plot(BRSP_Breeding)
# plot(July2813, add= TRUE)
# plot(BRSP_Routes, add = TRUE)
# plot(BRSP_Breeding, add = TRUE)
July2813[is.na(July2813)]<-0
summary(July2813@data@values)
dim(July2813)
rcl<-c(252,256,0)
rcl2<-matrix(rcl, ncol=3, byrow=TRUE)
July2813<-reclassify(July2813, rcl2)
# plot(July2813)
summary(July2813@data@values)
July2813[July2813 == 0]<-NA

# VegDRI mean of early, mid, end season rasters =====
earlyvd<-mosaic(May1709, May1610, May1511, May1312, May1913, fun = mean, na.rm = TRUE)

midvd<-mosaic(June1409, June2010, June0511, June1712, June2313, fun = mean, na.rm = TRUE)
latevd<-mosaic(July2609, July2510, July2411, July2212, July2813, fun = mean, na.rm = TRUE)
#VegDRI extract and mean across buffers ----
ROUTES_ID<-BRSP_Routes@data[c(2, 16)]
##Early Season: ----
evd400m<-extract(earlyvd, BRSP_400m, fun = mean, df = TRUE, na.rm = TRUE)
VegEarly<-cbind(ROUTES_ID, evd400m)
VegEarly<-VegEarly[c(1,2,4)]
names(VegEarly)<-c("RTENAME", "RouteID", "evd400m")

evd1km<-extract(earlyvd, BRSP_1km, fun = mean, df = TRUE, na.rm = TRUE)
VegEarly<-cbind(VegEarly, evd1km)
VegEarly<-VegEarly[c(1:3,5)]
names(VegEarly)<-c("RTENAME", "RouteID", "evd400m", "evd1km")

evd4km<-extract(earlyvd, BRSP_4km, fun = mean, df = TRUE, na.rm = TRUE)
VegEarly<-cbind(VegEarly, evd4km)
VegEarly<-VegEarly[c(1:4,6)]
names(VegEarly)<-c("RTENAME", "RouteID", "evd400m", "evd1km", "evd4km")

evd5km<-extract(earlyvd, BRSP_5km, fun = mean, df = TRUE, na.rm = TRUE)
VegEarly<-cbind(VegEarly, evd5km)
VegEarly<-VegEarly[c(1:5,7)]
names(VegEarly)<-c("RTENAME", "RouteID", "evd400m", "evd1km", "evd4km", "evd5km")

evd8km<-extract(earlyvd, BRSP_8km, fun = mean, df = TRUE, na.rm = TRUE)
VegEarly<-cbind(VegEarly, evd8km)
VegEarly<-VegEarly[c(1:6,8)]
names(VegEarly)<-c("RTENAME", "RouteID", "evd400m", "evd1km", "evd4km", "evd5km", "evd8km")

evd20km<-extract(earlyvd, BRSP_20km, fun = mean, df = TRUE, na.rm = TRUE)
VegEarly<-cbind(VegEarly, evd20km)
VegEarly<-VegEarly[c(1:7,9)]
names(VegEarly)<-c("RTENAME", "RouteID", "evd400m", "evd1km", "evd4km", "evd5km", "evd8km", "evd20km")
head(VegEarly)
##Mid Season: ----
mvd400m<-extract(midvd, BRSP_400m, fun = mean, df = TRUE, na.rm = TRUE)
VegMid<-cbind(ROUTES_ID, mvd400m)
VegMid<-VegMid[c(1,2,4)]
names(VegMid)<-c("RTENAME", "RouteID", "mvd400m")

mvd1km<-extract(midvd, BRSP_1km, fun = mean, df = TRUE, na.rm = TRUE)
VegMid<-cbind(VegMid, mvd1km)
VegMid<-VegMid[c(1:3,5)]
names(VegMid)<-c("RTENAME", "RouteID", "mvd400m", "mvd1km")

mvd4km<-extract(midvd, BRSP_4km, fun = mean, df = TRUE, na.rm = TRUE)
VegMid<-cbind(VegMid, mvd4km)
VegMid<-VegMid[c(1:4,6)]
names(VegMid)<-c("RTENAME", "RouteID", "mvd400m", "mvd1km", "mvd4km")

mvd5km<-extract(midvd, BRSP_5km, fun = mean, df = TRUE, na.rm = TRUE)
VegMid<-cbind(VegMid, mvd5km)
VegMid<-VegMid[c(1:5,7)]
names(VegMid)<-c("RTENAME", "RouteID", "mvd400m", "mvd1km", "mvd4km", "mvd5km")

mvd8km<-extract(midvd, BRSP_8km, fun = mean, df = TRUE, na.rm = TRUE)
VegMid<-cbind(VegMid, mvd8km)
VegMid<-VegMid[c(1:6,8)]
names(VegMid)<-c("RTENAME", "RouteID", "mvd400m", "mvd1km", "mvd4km", "mvd5km", "mvd8km")

mvd20km<-extract(midvd, BRSP_20km, fun = mean, df = TRUE, na.rm = TRUE)
VegMid<-cbind(VegMid, mvd20km)
VegMid<-VegMid[c(1:7,9)]
names(VegMid)<-c("RTENAME", "RouteID", "mvd400m", "mvd1km", "mvd4km", "mvd5km", "mvd8km", "mvd20km")
head(VegMid)
##Late Season: ----
lvd400m<-extract(latevd, BRSP_400m, fun = mean, df = TRUE, na.rm = TRUE)
VegLate<-cbind(ROUTES_ID, lvd400m)
VegLate<-VegLate[c(1,2,4)]
names(VegLate)<-c("RTENAME", "RouteID", "lvd400m")

lvd1km<-extract(latevd, BRSP_1km, fun = mean, df = TRUE, na.rm = TRUE)
VegLate<-cbind(VegLate, lvd1km)
VegLate<-VegLate[c(1:3,5)]
names(VegLate)<-c("RTENAME", "RouteID", "lvd400m", "lvd1km")

lvd4km<-extract(latevd, BRSP_4km, fun = mean, df = TRUE, na.rm = TRUE)
VegLate<-cbind(VegLate, lvd4km)
VegLate<-VegLate[c(1:4,6)]
names(VegLate)<-c("RTENAME", "RouteID", "lvd400m", "lvd1km", "lvd4km")

lvd5km<-extract(latevd, BRSP_5km, fun = mean, df = TRUE, na.rm = TRUE)
VegLate<-cbind(VegLate, lvd5km)
VegLate<-VegLate[c(1:5,7)]
names(VegLate)<-c("RTENAME", "RouteID", "lvd400m", "lvd1km", "lvd4km", "lvd5km")

lvd8km<-extract(latevd, BRSP_8km, fun = mean, df = TRUE, na.rm = TRUE)
VegLate<-cbind(VegLate, lvd8km)
VegLate<-VegLate[c(1:6,8)]
names(VegLate)<-c("RTENAME", "RouteID", "lvd400m", "lvd1km", "lvd4km", "lvd5km", "lvd8km")

lvd20km<-extract(latevd, BRSP_20km, fun = mean, df = TRUE, na.rm = TRUE)
VegLate<-cbind(VegLate, lvd20km)
VegLate<-VegLate[c(1:7,9)]
names(VegLate)<-c("RTENAME", "RouteID", "lvd400m", "lvd1km", "lvd4km", "lvd5km", "lvd8km", "lvd20km")
head(VegLate)
# PRISM data read in---------
library(prism)
#set pathway to prism data:
#options(prism.path = "~/Dropbox/Baylor/PhD/GIS/unzipped/PRISM")
options(prism.path = "C:/Users/kim_serno1/Dropbox/Baylor/PhD/GIS/unzipped/PRISM")
## downloaded needed data only do once:
#get_prism_monthlys(type = "ppt", years = 2008:2013, mon = c(10:12, 1:4))

#list prism data and absolute paths to use in raster package:
Plist<-ls_prism_data(absPath = TRUE)
Plist
#set pathways to r objects
oct08<-ls_prism_data(absPath = TRUE)[1,2]
nov08<-ls_prism_data(absPath = TRUE)[2,2]
dec08<-ls_prism_data(absPath = TRUE)[3,2]
jan09<-ls_prism_data(absPath = TRUE)[4,2]
feb09<-ls_prism_data(absPath = TRUE)[5,2]
mar09<-ls_prism_data(absPath = TRUE)[6,2]
apr09<-ls_prism_data(absPath = TRUE)[7,2]
oct09<-ls_prism_data(absPath = TRUE)[8,2]
nov09<-ls_prism_data(absPath = TRUE)[9,2]
dec09<-ls_prism_data(absPath = TRUE)[10,2]
jan10<-ls_prism_data(absPath = TRUE)[11,2]
feb10<-ls_prism_data(absPath = TRUE)[12,2]
mar10<-ls_prism_data(absPath = TRUE)[13,2]
apr10<-ls_prism_data(absPath = TRUE)[14,2]
oct10<-ls_prism_data(absPath = TRUE)[15,2]
nov10<-ls_prism_data(absPath = TRUE)[16,2]
dec10<-ls_prism_data(absPath = TRUE)[17,2]
jan11<-ls_prism_data(absPath = TRUE)[18,2]
feb11<-ls_prism_data(absPath = TRUE)[19,2]
mar11<-ls_prism_data(absPath = TRUE)[20,2]
apr11<-ls_prism_data(absPath = TRUE)[21,2]
oct11<-ls_prism_data(absPath = TRUE)[22,2]
nov11<-ls_prism_data(absPath = TRUE)[23,2]
dec11<-ls_prism_data(absPath = TRUE)[24,2]
jan12<-ls_prism_data(absPath = TRUE)[25,2]
feb12<-ls_prism_data(absPath = TRUE)[26,2]
mar12<-ls_prism_data(absPath = TRUE)[27,2]
apr12<-ls_prism_data(absPath = TRUE)[28,2]
oct12<-ls_prism_data(absPath = TRUE)[29,2]
nov12<-ls_prism_data(absPath = TRUE)[30,2]
dec12<-ls_prism_data(absPath = TRUE)[31,2]
jan13<-ls_prism_data(absPath = TRUE)[32,2]
feb13<-ls_prism_data(absPath = TRUE)[33,2]
mar13<-ls_prism_data(absPath = TRUE)[34,2]
apr13<-ls_prism_data(absPath = TRUE)[35,2]

#precipitation raster for each month of interest:
oct08r<-raster(oct08)
oct09r<-raster(oct09)
oct10r<-raster(oct10)
oct11r<-raster(oct11)
oct12r<-raster(oct12)
nov08r<-raster(nov08)
nov09r<-raster(nov09)
nov10r<-raster(nov10)
nov11r<-raster(nov11)
nov12r<-raster(nov12)
dec08r<-raster(dec08)
dec09r<-raster(dec09)
dec10r<-raster(dec10)
dec11r<-raster(dec11)
dec12r<-raster(dec12)
jan09r<-raster(jan09)
jan10r<-raster(jan10)
jan11r<-raster(jan11)
jan12r<-raster(jan12)
jan13r<-raster(jan13)
feb09r<-raster(feb09)
feb10r<-raster(feb10)
feb11r<-raster(feb11)
feb12r<-raster(feb12)
feb13r<-raster(feb13)
mar09r<-raster(mar09)
mar10r<-raster(mar10)
mar11r<-raster(mar11)
mar12r<-raster(mar12)
mar13r<-raster(mar13)
apr09r<-raster(apr09)
apr10r<-raster(apr10)
apr11r<-raster(apr11)
apr12r<-raster(apr12)
apr13r<-raster(apr13)

## PRISM add rasters for winter precip totals (oct-apr; in millimeters): ----
winter09<-mosaic(oct08r, nov08r, dec08r, jan09r, feb09r, mar09r, apr09r, fun = sum)
winter10<-mosaic(oct09r, nov09r, dec09r, jan10r, feb10r, mar10r, apr10r, fun = sum)
winter11<-mosaic(oct10r, nov10r, dec10r, jan11r, feb11r, mar11r, apr11r, fun = sum)
winter12<-mosaic(oct11r, nov11r, dec11r, jan12r, feb12r, mar12r, apr12r, fun = sum)
winter13<-mosaic(oct12r, nov12r, dec12r, jan13r, feb13r, mar13r, apr13r, fun = sum)
winter_mean<-mosaic(winter09, winter10, winter11, winter12, winter13, fun = mean)
Winter_Means<-projectRaster(winter_mean, crs = CRS_gap)

##PRISM extract raster values and mean across buffer -----
ewm400m<-extract(Winter_Means, BRSP_400m, fun = mean, df = TRUE, na.rm = TRUE)
precipWinter<-cbind(ROUTES_ID, ewm400m)
precipWinter<-precipWinter[c(1,2,4)]
names(precipWinter)<-c("RTENAME", "RouteID", "wm400m")

ewm1km<-extract(Winter_Means, BRSP_1km, fun = mean, df = TRUE, na.rm = TRUE)
precipWinter<-cbind(precipWinter, ewm1km)
precipWinter<-precipWinter[c(1:3,5)]
names(precipWinter)<-c("RTENAME", "RouteID", "pwm400m", "pwm1km")

ewm4km<-extract(Winter_Means, BRSP_4km, fun = mean, df = TRUE, na.rm = TRUE)
precipWinter<-cbind(precipWinter, ewm4km)
precipWinter<-precipWinter[c(1:4,6)]
names(precipWinter)<-c("RTENAME", "RouteID", "pwm400m", "pwm1km", "pwm4km")

ewm5km<-extract(Winter_Means, BRSP_5km, fun = mean, df = TRUE, na.rm = TRUE)
precipWinter<-cbind(precipWinter, ewm5km)
precipWinter<-precipWinter[c(1:5,7)]
names(precipWinter)<-c("RTENAME", "RouteID", "pwm400m", "pwm1km", "pwm4km", "pwm5km")

ewm8km<-extract(Winter_Means, BRSP_8km, fun = mean, df = TRUE, na.rm = TRUE)
precipWinter<-cbind(precipWinter, ewm8km)
precipWinter<-precipWinter[c(1:6,8)]
names(precipWinter)<-c("RTENAME", "RouteID", "pwm400m", "pwm1km", "pwm4km", "pwm5km", "pwm8km")

ewm20km<-extract(Winter_Means, BRSP_20km, fun = mean, df = TRUE, na.rm = TRUE)
precipWinter<-cbind(precipWinter, ewm20km)
precipWinter<-precipWinter[c(1:7,9)]
names(precipWinter)<-c("RTENAME", "RouteID", "pwm400m", "pwm1km", "pwm4km", "pwm5km", "pwm8km", "pwm20km")













