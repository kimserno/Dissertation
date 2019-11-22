

library(rgdal)
library(raster)
library(sp)
library(prism)
library(rgeos)
#setwd("C:/Users/kim_serno1/Dropbox/Baylor/PhD/Dissertation")
setwd("~/Dropbox/Baylor/PhD/Dissertation")

#load in gap lc rasters to mosaic together for gaplc west raster
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

#import shapefiles PC:
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

#import shapefiles Mac:
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

Gaplc_west<-raster("C:/Users/kim_serno1/Documents/PhD/GIS/Dissertation/data/unzipped/gap_landcover/Gaplc_West_raster.tif")

# Load in Gap Landcover west raster (creation code above)
# Gaplc_west<-raster("C:/Users/kim_serno1/Dropbox/Baylor/PhD/GIS/unzipped/gap_landcover/Gaplc_West_raster.tif")
Gaplc_west<-raster("~/Dropbox/Baylor/PhD/GIS/unzipped/gap_landcover/Gaplc_West_raster.tif")


# Get projection information for gap landcover raster:
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

# Buffers around minimum bounding rectangle center (20km, 40km)
BRSP_20km<-gBuffer(BRSP_Mincenter, width = 20000, byid = TRUE, id=BRSP_Mincenter@data$RouteID)
plot(BRSP_20km, add = TRUE)
summary(BRSP_20km)
BRSP_40km<-gBuffer(BRSP_Mincenter, width = 40000, byid = TRUE, id=BRSP_Mincenter@data$RouteID)
plot(BRSP_40km, add = TRUE)
summary(BRSP_40km)

#buffers around routes
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

#extract gap lc values for buffer:
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

# convert to data frame
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


# VegDRI

July2813<-raster("C:/Users/kim_serno1/Documents/PhD/GIS/Dissertation/data/unzipped/VegDRI/2013/vegdri_emodis_week30_072813.tif")
# plot(July2813)
proj4string(July2813)
July28_13<-projectRaster(July2813, crs = CRS_gap)
proj4string(July28_13)
summary(July28_13@data@values)
#check visually
plot(BRSP_Breeding)
plot(July28_13, add= TRUE)
plot(BRSP_Routes, add = TRUE)
plot(BRSP_Breeding, add = TRUE)
July28_13[is.na(July28_13[])]<-0
summary(July28_13@data@values)
dim(July28_13)

test<-July28_13
rcl<-c(252,256,0)
rcl2<-matrix(rcl, ncol=3, byrow=TRUE)
test<-reclassify(test, rcl2)
plot(test)
summary(test@data@values)
test[test == 0]<-NA



# PRISM data
library(prism)
#set pathway to prism data:
options(prism.path = "~/Dropbox/Baylor/PhD/GIS/unzipped/PRISM")
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

#add rasters for winter precip totals (oct-apr; in millimeters):
winter09<-mosaic(oct08r, nov08r, dec08r, jan09r, feb09r, mar09r, apr09r, fun = sum)
winter10<-mosaic(oct09r, nov09r, dec09r, jan10r, feb10r, mar10r, apr10r, fun = sum)
winter11<-mosaic(oct10r, nov10r, dec10r, jan11r, feb11r, mar11r, apr11r, fun = sum)
winter12<-mosaic(oct11r, nov11r, dec11r, jan12r, feb12r, mar12r, apr12r, fun = sum)
winter13<-mosaic(oct12r, nov12r, dec12r, jan13r, feb13r, mar13r, apr13r, fun = sum)
winter_mean<-mosaic(winter09, winter10, winter11, winter12, winter13, fun = mean)
Winter_Means<-projectRaster(winter_mean, crs = CRS_gap)
#extract raster values and mean across buffer
start.time = Sys.time()
ewm400m<-extract(winter_mean, BRSP_400m, fun = mean, df = TRUE)
end.time = Sys.time(); elapsed.time = round(difftime(end.time,start.time, units='mins'), dig = 2)
paste('Elapsed time was ', elapsed.time, 'minutes or ', round(elapsed.time/60,3), 'hours', sep = '')






