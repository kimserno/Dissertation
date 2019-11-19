

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
# brsp_GDB<-"~/Dropbox/Baylor/PhD/GIS/BRSP.gdb"


brsp_GDB<-"C:/Users/kim_serno1/Dropbox/Baylor/PhD/Dissertation/GIS/BRSP.gdb"


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
plot(BRSP_mincenter, add = TRUE)
summary(BRSP_mincenter)

# Load in Gap Landcover west raster (creation code above)
# Gaplc_west<-raster("C:/Users/kim_serno1/Dropbox/Baylor/PhD/GIS/unzipped/gap_landcover/Gaplc_West_raster.tif")
Gaplc_west<-raster("~/Dropbox/Baylor/PhD/GIS/unzipped/gap_landcover/Gaplc_West_raster.tif")

# Get projection information for gap landcover raster:
CRS_gap<-proj4string(Gaplc_west)
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
