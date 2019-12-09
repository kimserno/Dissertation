

library(rgdal)
library(raster)
library(sp)
library(prism)
library(rgeos)
library(matrixStats)
library(data.table)
library(tidyverse)
#setwd("C:/Users/kim_serno1/Dropbox/Baylor/PhD/Dissertation")
# setwd("~/Dropbox/Baylor/PhD/Dissertation")
load("~/Dropbox/Baylor/PhD/GIS/BRSP_shapefile_rasters3.RData")
#load in gap lc rasters to mosaic together for gaplc west raster -----
R2<-raster("C:/Users/kim_serno1/Dropbox/Baylor/PhD/Dissertation/GIS/unzipped/gap_landcover/Region2_California/gaplf2011lc_v30_lcc_2.tif")
R11<-raster("C:/Users/kim_serno1/Dropbox/Baylor/PhD/Dissertation/GIS/unzipped/gap_landcover/Region11_NorthPacific/gaplf2011lc_v30_lcc_11.tif")
R13<-raster("C:/Users/kim_serno1/Dropbox/Baylor/PhD/Dissertation/GIS/unzipped/gap_landcover/Region13_PlainsPrairiePotholes/gaplf2011lc_v30_lcc_13.tif")
R15<-raster("C:/Users/kim_serno1/Dropbox/Baylor/PhD/Dissertation/GIS/unzipped/gap_landcover/Region15_SouthernRockies/gaplf2011lc_v30_lcc_15.tif")
R3<-raster("C:/Users/kim_serno1/Dropbox/Baylor/PhD/Dissertation/GIS/unzipped/gap_landcover/Region3_Desert/gaplf2011lc_v30_lcc_3.tif")
R5<-raster("C:/Users/kim_serno1/Dropbox/Baylor/PhD/Dissertation/GIS/unzipped/gap_landcover/Region5_GreatBasin/gaplf2011lc_v30_lcc_5.tif")
R6<-raster("C:/Users/kim_serno1/Dropbox/Baylor/PhD/Dissertation/GIS/unzipped/gap_landcover/Region6_GreatNorthern/gaplf2011lc_v30_lcc_6.tif")
R7<-raster("C:/Users/kim_serno1/Dropbox/Baylor/PhD/Dissertation/GIS/unzipped/gap_landcover/Region7_GreatPlains/gaplf2011lc_v30_lcc_7.tif")

R2<-raster("~/Dropbox/Baylor/PhD/GIS/unzipped/gap_landcover/Region2_California/gaplf2011lc_v30_lcc_2.tif")
R11<-raster("~/Dropbox/Baylor/PhD/GIS/unzipped/gap_landcover/Region11_NorthPacific/gaplf2011lc_v30_lcc_11.tif")
R13<-raster("~/Dropbox/Baylor/PhD/GIS/unzipped/gap_landcover/Region13_PlainsPrairiePotholes/gaplf2011lc_v30_lcc_13.tif")
R15<-raster("~/Dropbox/Baylor/PhD/GIS/unzipped/gap_landcover/Region15_SouthernRockies/gaplf2011lc_v30_lcc_15.tif")
R3<-raster("~/Dropbox/Baylor/PhD/GIS/unzipped/gap_landcover/Region3_Desert/gaplf2011lc_v30_lcc_3.tif")
R5<-raster("~/Dropbox/Baylor/PhD/GIS/unzipped/gap_landcover/Region5_GreatBasin/gaplf2011lc_v30_lcc_5.tif")
R6<-raster("~/Dropbox/Baylor/PhD/GIS/unzipped/gap_landcover/Region6_GreatNorthern/gaplf2011lc_v30_lcc_6.tif")
R7<-raster("~/Dropbox/Baylor/PhD/GIS/unzipped/gap_landcover/Region7_GreatPlains/gaplf2011lc_v30_lcc_7.tif")
plot(R2)
proj4string(R2)
proj4string(R11)
proj4string(R13)
proj4string(R15)
proj4string(R3)
proj4string(R5)
proj4string(R6)
proj4string(R7)

#Creation of gaplc_west raster and timing
start.time = Sys.time()
Gaplc_West_raster<-mosaic(R2, R11, R13, R15, R3, R5, R6, R7, fun = mean,  filename = "~/Dropbox/Baylor/PhD/GIS/unzipped/gap_landcover/Gaplc_West_raster.tif")
end.time = Sys.time(); elapsed.time = round(difftime(end.time,start.time, units='mins'), dig = 2)
paste('Elapsed time was ', elapsed.time, 'minutes or ', round(elapsed.time/60,3), 'hours', sep = '')
r2r11<-mosaic(R2, R11, fun = mean)
brsp_GDB<-"C:/Users/kim_serno1/Documents/GIS_projects/Dissertation/data/BRSP.gdb"

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
plot(BRSP_breeding, col = "wheat3")
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

#spread the dataframes out:-----
##400m buffer ----
glc400m<-e400mdf %>% 
  spread(key=cover, value=count)
glc400m<-setnames(glc400m, paste0("x",names(glc400m)))
colnames(glc400m)[1]<-"RouteID"
glc400m[is.na(glc400m)]<-0
glc400m<-mutate(.data = glc400m, Sum = rowSums(glc400m[,2:230]))
#subset out landcover of interest values only:
lc400m<-glc400m %>% 
  select(RouteID, x312, x316, x437, x484, x485, x488, x489, x490, x491, x492, x493, x495, x498, x556, x557, x558, Sum)
###subset out broad landcover values: ----
##Agriculture and Developed Vegetation:
ADV400m<-glc400m %>% 
  select(RouteID, x555, x556, x557) %>% 
  mutate(ADV = rowSums(.[,2:4])) %>% 
  select(RouteID, ADV)

##Introduced and Semi-natural Vegetation:
ISNV400m<-glc400m %>% 
  # select(RouteID, x558, x559, x560, x561, x562, x563) %>% 
  select(RouteID, x558, x559, x561, x562, x563) %>% 
  mutate(ISNV = rowSums(.[,2:6])) %>% 
  select(RouteID, ISNV)

## Desert and semi-desert
DSD400m<-glc400m %>% 
  # select(RouteID, x306, x442, x460, x461, x462, x466, x470, x471, x472, x473, x474, x476, x477, x484, x485, x486, x487, x488, x489, x490, x491, x492, x493, x494, x495, x496, x497, x498, x499, x538, x545) %>% 
  select(RouteID, x306, x460, x461, x466, x470, x471, x472, x476, x477, x484, x485, x486, x487, x488, x489, x490, x491, x492, x493, x494, x495, x496, x497, x498, x499,  x545) %>% 
  mutate(DSD = rowSums(.[,2:27])) %>% 
  select(RouteID, DSD)

##Shurb and herb vegetation:
SHV400m<-glc400m %>% 
  # select(RouteID, x117, x266, x269, x274, x275, x282, x296, x297, x298, x299, x301, x302, x303, x304, x305, x307, x308, x309, x310, x311, x312, x313, x314, x315, x316, x317, x318, x319, x321, x322, x323, x324, x325, x326, x327, x328, x329, x331, x358, x359, x360, x361, x381, x382, x383, x384, x385, x396, x397, x398, x422, x424, x425, x426, x427, x429, x430, x432, x433, x434, x436, x437, x438, x439, x440, x441, x443, x444, x445, x455, x456, x457, x458, x459, x508) %>% 
  select(RouteID, x117, x266, x269, x274, x275, x282, x296, x297, x299, x301, x302, x303, x304, x305, x307, x308, x309, x310, x311, x312, x313, x314, x315, x316, x317,  x319, x321, x322, x323, x324, x325, x326, x327, x328, x329, x331, x358, x359, x360, x361, x384, x385, x397, x398, x422, x424, x425, x426, x427, x430, x432, x434, x436, x437, x438, x439, x440, x441, x443, x444, x445, x456, x457, x458, x459) %>% 
  mutate(SHV = rowSums(.[,2:66])) %>% 
  select(RouteID, SHV)

Broadlc400m<-left_join(SHV400m, DSD400m, by = "RouteID")
Broadlc400m<-left_join(Broadlc400m, ADV400m, by = "RouteID")
Broadlc400m<-left_join(Broadlc400m, ISNV400m, by = "RouteID")
head(Broadlc400m)
lc400m<-left_join(lc400m, Broadlc400m, by = "RouteID")
head(lc400m)
###get proportions of landcover of interest: -----
lc400m<-lc400m %>% 
  mutate(p312_400m = (x312/Sum)) %>% 
  mutate(p316_400m = (x316/Sum)) %>% 
  mutate(p437_400m = (x437/Sum)) %>% 
  mutate(p484_400m = (x484/Sum)) %>% 
  mutate(p485_400m = (x485/Sum)) %>% 
  mutate(p488_400m = (x488/Sum)) %>% 
  mutate(p489_400m = (x489/Sum)) %>% 
  mutate(p490_400m = (x490/Sum)) %>% 
  mutate(p491_400m = (x491/Sum)) %>% 
  mutate(p492_400m = (x492/Sum)) %>% 
  mutate(p493_400m = (x493/Sum)) %>% 
  mutate(p495_400m = (x495/Sum)) %>% 
  mutate(p498_400m = (x498/Sum)) %>% 
  mutate(p556_400m = (x556/Sum)) %>% 
  mutate(p557_400m = (x557/Sum)) %>% 
  mutate(p558_400m = (x558/Sum)) %>% 
  mutate(pSHV_400m = (SHV/Sum)) %>% 
  mutate(pDSD_400m = (DSD/Sum)) %>% 
  mutate(pADV_400m = (ADV/Sum)) %>% 
  mutate(pISNV_400m = (ISNV/Sum)) 
  

head(lc400m)

##1km buffer ----
glc1km<-e1kmdf %>% 
  spread(key=cover, value=count)
glc1km<-setnames(glc1km, paste0("x",names(glc1km)))
colnames(glc1km)[1]<-"RouteID"
glc1km[is.na(glc1km)]<-0
glc1km<-mutate(.data = glc1km, Sum = rowSums(glc1km[,2:230]))
#subset out landcover of interest values only:
lc1km<-glc1km %>% 
  select(RouteID, x312, x316, x437, x484, x485, x488, x489, x490, x491, x492, x493, x495, x498, x556, x557, x558, Sum)
###subset out broad landcover values: ----
##Agriculture and Developed Vegetation:
ADV1km<-glc1km %>% 
  select(RouteID, x555, x556, x557) %>% 
  mutate(ADV = rowSums(.[,2:4])) %>% 
  select(RouteID, ADV)

##Introduced and Semi-natural Vegetation:
ISNV1km<-glc1km %>% 
  # select(RouteID, x558, x559, x560, x561, x562, x563) %>% 
  select(RouteID, x558, x559, x561, x562, x563) %>% 
  mutate(ISNV = rowSums(.[,2:6])) %>% 
  select(RouteID, ISNV)

## Desert and semi-desert
DSD1km<-glc1km %>% 
  # select(RouteID, x306, x442, x460, x461, x462, x466, x470, x471, x472, x473, x474, x476, x477, x484, x485, x486, x487, x488, x489, x490, x491, x492, x493, x494, x495, x496, x497, x498, x499, x538, x545) %>%
  select(RouteID, x306, x460, x461, x466, x470, x471, x472, x476, x477, x484, x485, x486, x487, x488, x489, x490, x491, x492, x493, x494, x495, x496, x497, x498, x499, x545) %>%
  
  mutate(DSD = rowSums(.[,2:27])) %>% 
  select(RouteID, DSD)

##Shurb and herb vegetation:
SHV1km<-glc1km %>% 
  # select(RouteID, x117, x266, x269, x274, x275, x282, x296, x297, x298, x299, x301, x302, x303, x304, x305, x307, x308, x309, x310, x311, x312, x313, x314, x315, x316, x317, x318, x319, x321, x322, x323, x324, x325, x326, x327, x328, x329, x331, x358, x359, x360, x361, x381, x382, x383, x384, x385, x396, x397, x398, x422, x424, x425, x426, x427, x429, x430, x432, x433, x434, x436, x437, x438, x439, x440, x441, x443, x444, x445, x455, x456, x457, x458, x459, x508) %>%
  select(RouteID, x117, x266, x269, x274, x275, x282, x296, x297, x299, x301, x302, x303, x304, x305, x307, x308, x309, x310, x311, x312, x313, x314, x315, x316, x317, x319, x321, x322, x323, x324, x325, x326, x327, x328, x329, x331, x358, x359, x360, x361, x381,  x384, x385, x397, x398, x422, x424, x425, x426, x427, x430, x432, x433, x434, x436, x437, x438, x439, x440, x441, x443, x444, x445, x456, x457, x458, x459) %>%
  
  mutate(SHV = rowSums(.[,2:66])) %>% 
  select(RouteID, SHV)

Broadlc1km<-left_join(SHV1km, DSD1km, by = "RouteID")
Broadlc1km<-left_join(Broadlc1km, ADV1km, by = "RouteID")
Broadlc1km<-left_join(Broadlc1km, ISNV1km, by = "RouteID")
head(Broadlc1km)
lc1km<-left_join(lc1km, Broadlc1km, by = "RouteID")
head(lc1km)
###get proportions of landcover of interest: -----
lc1km<-lc1km %>% 
  mutate(p312_1km = (x312/Sum)) %>% 
  mutate(p316_1km = (x316/Sum)) %>% 
  mutate(p437_1km = (x437/Sum)) %>% 
  mutate(p484_1km = (x484/Sum)) %>% 
  mutate(p485_1km = (x485/Sum)) %>% 
  mutate(p488_1km = (x488/Sum)) %>% 
  mutate(p489_1km = (x489/Sum)) %>% 
  mutate(p490_1km = (x490/Sum)) %>% 
  mutate(p491_1km = (x491/Sum)) %>% 
  mutate(p492_1km = (x492/Sum)) %>% 
  mutate(p493_1km = (x493/Sum)) %>% 
  mutate(p495_1km = (x495/Sum)) %>% 
  mutate(p498_1km = (x498/Sum)) %>% 
  mutate(p556_1km = (x556/Sum)) %>% 
  mutate(p557_1km = (x557/Sum)) %>% 
  mutate(p558_1km = (x558/Sum)) %>% 
  mutate(pSHV_1km = (SHV/Sum)) %>% 
  mutate(pDSD_1km = (DSD/Sum)) %>% 
  mutate(pADV_1km = (ADV/Sum)) %>% 
  mutate(pISNV_1km = (ISNV/Sum)) 


head(lc1km)


##4km buffer ----
glc4km<-e4kmdf %>% 
  spread(key=cover, value=count)
glc4km<-setnames(glc4km, paste0("x",names(glc4km)))
colnames(glc4km)[1]<-"RouteID"
glc4km[is.na(glc4km)]<-0
glc4km<-mutate(.data = glc4km, Sum = rowSums(glc4km[,2:230]))
#subset out landcover of interest values only:
lc4km<-glc4km %>% 
  select(RouteID, x312, x316, x437, x484, x485, x488, x489, x490, x491, x492, x493, x495, x498, x556, x557, x558, Sum)
###subset out broad landcover values: ----
##Agriculture and Developed Vegetation:
ADV4km<-glc4km %>% 
  select(RouteID, x555, x556, x557) %>% 
  mutate(ADV = rowSums(.[,2:4])) %>% 
  select(RouteID, ADV)

##Introduced and Semi-natural Vegetation:
ISNV4km<-glc4km %>% 
  # select(RouteID, x558, x559, x560, x561, x562, x563) %>% 
  select(RouteID, x558, x559, x561, x562, x563) %>% 
  mutate(ISNV = rowSums(.[,2:6])) %>% 
  select(RouteID, ISNV)

## Desert and semi-desert
DSD4km<-glc4km %>% 
  # select(RouteID, x306, x442, x460, x461, x462, x466, x470, x471, x472, x473, x474, x476, x477, x484, x485, x486, x487, x488, x489, x490, x491, x492, x493, x494, x495, x496, x497, x498, x499, x538, x545) %>% 
  select(RouteID, x306, x460, x461, x466, x470, x471, x472, x476, x477, x484, x485, x486, x487, x488, x489, x490, x491, x492, x493, x494, x495, x496, x497, x498, x499,  x545) %>% 
  mutate(DSD = rowSums(.[,2:27])) %>% 
  select(RouteID, DSD)

##Shurb and herb vegetation:
SHV4km<-glc4km %>% 
  # select(RouteID, x117, x266, x269, x274, x275, x282, x296, x297, x298, x299, x301, x302, x303, x304, x305, x307, x308, x309, x310, x311, x312, x313, x314, x315, x316, x317, x318, x319, x321, x322, x323, x324, x325, x326, x327, x328, x329, x331, x358, x359, x360, x361, x381, x382, x383, x384, x385, x396, x397, x398, x422, x424, x425, x426, x427, x429, x430, x432, x433, x434, x436, x437, x438, x439, x440, x441, x443, x444, x445, x455, x456, x457, x458, x459, x508) %>% 
  select(RouteID, x117, x266, x269, x274, x275, x282, x296, x297, x299, x301, x302, x303, x304, x305, x307, x308, x309, x310, x311, x312, x313, x314, x315, x316, x317,  x319, x321, x322, x323, x324, x325, x326, x327, x328, x329, x331, x358, x359, x360, x361, x384, x385, x397, x398, x422, x424, x425, x426, x427, x430, x432, x434, x436, x437, x438, x439, x440, x441, x443, x444, x445, x456, x457, x458, x459) %>% 
  mutate(SHV = rowSums(.[,2:66])) %>% 
  select(RouteID, SHV)

Broadlc4km<-left_join(SHV4km, DSD4km, by = "RouteID")
Broadlc4km<-left_join(Broadlc4km, ADV4km, by = "RouteID")
Broadlc4km<-left_join(Broadlc4km, ISNV4km, by = "RouteID")
head(Broadlc4km)
lc4km<-left_join(lc4km, Broadlc4km, by = "RouteID")
head(lc4km)
###get proportions of landcover of interest: -----
lc4km<-lc4km %>% 
  mutate(p312_4km = (x312/Sum)) %>% 
  mutate(p316_4km = (x316/Sum)) %>% 
  mutate(p437_4km = (x437/Sum)) %>% 
  mutate(p484_4km = (x484/Sum)) %>% 
  mutate(p485_4km = (x485/Sum)) %>% 
  mutate(p488_4km = (x488/Sum)) %>% 
  mutate(p489_4km = (x489/Sum)) %>% 
  mutate(p490_4km = (x490/Sum)) %>% 
  mutate(p491_4km = (x491/Sum)) %>% 
  mutate(p492_4km = (x492/Sum)) %>% 
  mutate(p493_4km = (x493/Sum)) %>% 
  mutate(p495_4km = (x495/Sum)) %>% 
  mutate(p498_4km = (x498/Sum)) %>% 
  mutate(p556_4km = (x556/Sum)) %>% 
  mutate(p557_4km = (x557/Sum)) %>% 
  mutate(p558_4km = (x558/Sum)) %>% 
  mutate(pSHV_4km = (SHV/Sum)) %>% 
  mutate(pDSD_4km = (DSD/Sum)) %>% 
  mutate(pADV_4km = (ADV/Sum)) %>% 
  mutate(pISNV_4km = (ISNV/Sum)) 


head(lc4km)


##5km buffer ----
glc5km<-e5kmdf %>% 
  spread(key=cover, value=count)
glc5km<-setnames(glc5km, paste0("x",names(glc5km)))
colnames(glc5km)[1]<-"RouteID"
glc5km[is.na(glc5km)]<-0
glc5km<-mutate(.data = glc5km, Sum = rowSums(glc5km[,2:230]))
#subset out landcover of interest values only:
lc5km<-glc5km %>% 
  select(RouteID, x312, x316, x437, x484, x485, x488, x489, x490, x491, x492, x493, x495, x498, x556, x557, x558, Sum)
###subset out broad landcover values: ----
##Agriculture and Developed Vegetation:
ADV5km<-glc5km %>% 
  select(RouteID, x555, x556, x557) %>% 
  mutate(ADV = rowSums(.[,2:4])) %>% 
  select(RouteID, ADV)

##Introduced and Semi-natural Vegetation:
ISNV5km<-glc5km %>% 
  # select(RouteID, x558, x559, x560, x561, x562, x563) %>% 
  select(RouteID, x558, x559, x561, x562, x563) %>% 
  mutate(ISNV = rowSums(.[,2:6])) %>% 
  select(RouteID, ISNV)

## Desert and semi-desert
DSD5km<-glc5km %>% 
  # select(RouteID, x306, x442, x460, x461, x462, x466, x470, x471, x472, x473, x474, x476, x477, x484, x485, x486, x487, x488, x489, x490, x491, x492, x493, x494, x495, x496, x497, x498, x499, x538, x545) %>% 
  select(RouteID, x306, x460, x461, x466, x470, x471, x472, x476, x477, x484, x485, x486, x487, x488, x489, x490, x491, x492, x493, x494, x495, x496, x497, x498, x499,  x545) %>% 
  mutate(DSD = rowSums(.[,2:27])) %>% 
  select(RouteID, DSD)

##Shurb and herb vegetation:
SHV5km<-glc5km %>% 
  # select(RouteID, x117, x266, x269, x274, x275, x282, x296, x297, x298, x299, x301, x302, x303, x304, x305, x307, x308, x309, x310, x311, x312, x313, x314, x315, x316, x317, x318, x319, x321, x322, x323, x324, x325, x326, x327, x328, x329, x331, x358, x359, x360, x361, x381, x382, x383, x384, x385, x396, x397, x398, x422, x424, x425, x426, x427, x429, x430, x432, x433, x434, x436, x437, x438, x439, x440, x441, x443, x444, x445, x455, x456, x457, x458, x459, x508) %>% 
  select(RouteID, x117, x266, x269, x274, x275, x282, x296, x297, x299, x301, x302, x303, x304, x305, x307, x308, x309, x310, x311, x312, x313, x314, x315, x316, x317,  x319, x321, x322, x323, x324, x325, x326, x327, x328, x329, x331, x358, x359, x360, x361, x384, x385, x397, x398, x422, x424, x425, x426, x427, x430, x432, x434, x436, x437, x438, x439, x440, x441, x443, x444, x445, x456, x457, x458, x459) %>% 
  mutate(SHV = rowSums(.[,2:66])) %>% 
  select(RouteID, SHV)

Broadlc5km<-left_join(SHV5km, DSD5km, by = "RouteID")
Broadlc5km<-left_join(Broadlc5km, ADV5km, by = "RouteID")
Broadlc5km<-left_join(Broadlc5km, ISNV5km, by = "RouteID")
head(Broadlc5km)
lc5km<-left_join(lc5km, Broadlc5km, by = "RouteID")
head(lc5km)
###get proportions of landcover of interest: -----
lc5km<-lc5km %>% 
  mutate(p312_5km = (x312/Sum)) %>% 
  mutate(p316_5km = (x316/Sum)) %>% 
  mutate(p437_5km = (x437/Sum)) %>% 
  mutate(p484_5km = (x484/Sum)) %>% 
  mutate(p485_5km = (x485/Sum)) %>% 
  mutate(p488_5km = (x488/Sum)) %>% 
  mutate(p489_5km = (x489/Sum)) %>% 
  mutate(p490_5km = (x490/Sum)) %>% 
  mutate(p491_5km = (x491/Sum)) %>% 
  mutate(p492_5km = (x492/Sum)) %>% 
  mutate(p493_5km = (x493/Sum)) %>% 
  mutate(p495_5km = (x495/Sum)) %>% 
  mutate(p498_5km = (x498/Sum)) %>% 
  mutate(p556_5km = (x556/Sum)) %>% 
  mutate(p557_5km = (x557/Sum)) %>% 
  mutate(p558_5km = (x558/Sum)) %>% 
  mutate(pSHV_5km = (SHV/Sum)) %>% 
  mutate(pDSD_5km = (DSD/Sum)) %>% 
  mutate(pADV_5km = (ADV/Sum)) %>% 
  mutate(pISNV_5km = (ISNV/Sum)) 


head(lc5km)


##8km buffer ----
glc8km<-e8kmdf %>% 
  spread(key=cover, value=count)
glc8km<-setnames(glc8km, paste0("x",names(glc8km)))
colnames(glc8km)[1]<-"RouteID"
glc8km[is.na(glc8km)]<-0
glc8km<-mutate(.data = glc8km, Sum = rowSums(glc8km[,2:230]))
#subset out landcover of interest values only:
lc8km<-glc8km %>% 
  select(RouteID, x312, x316, x437, x484, x485, x488, x489, x490, x491, x492, x493, x495, x498, x556, x557, x558, Sum)
###subset out broad landcover values: ----
##Agriculture and Developed Vegetation:
ADV8km<-glc8km %>% 
  select(RouteID, x555, x556, x557) %>% 
  mutate(ADV = rowSums(.[,2:4])) %>% 
  select(RouteID, ADV)

##Introduced and Semi-natural Vegetation:
ISNV8km<-glc8km %>% 
  # select(RouteID, x558, x559, x560, x561, x562, x563) %>% 
  select(RouteID, x558, x559, x561, x562, x563) %>% 
  mutate(ISNV = rowSums(.[,2:6])) %>% 
  select(RouteID, ISNV)

## Desert and semi-desert
DSD8km<-glc8km %>% 
  # select(RouteID, x306, x442, x460, x461, x462, x466, x470, x471, x472, x473, x474, x476, x477, x484, x485, x486, x487, x488, x489, x490, x491, x492, x493, x494, x495, x496, x497, x498, x499, x538, x545) %>% 
  select(RouteID, x306, x460, x461, x466, x470, x471, x472, x476, x477, x484, x485, x486, x487, x488, x489, x490, x491, x492, x493, x494, x495, x496, x497, x498, x499,  x545) %>% 
  mutate(DSD = rowSums(.[,2:27])) %>% 
  select(RouteID, DSD)

##Shurb and herb vegetation:
SHV8km<-glc8km %>% 
  # select(RouteID, x117, x266, x269, x274, x275, x282, x296, x297, x298, x299, x301, x302, x303, x304, x305, x307, x308, x309, x310, x311, x312, x313, x314, x315, x316, x317, x318, x319, x321, x322, x323, x324, x325, x326, x327, x328, x329, x331, x358, x359, x360, x361, x381, x382, x383, x384, x385, x396, x397, x398, x422, x424, x425, x426, x427, x429, x430, x432, x433, x434, x436, x437, x438, x439, x440, x441, x443, x444, x445, x455, x456, x457, x458, x459, x508) %>% 
  select(RouteID, x117, x266, x269, x274, x275, x282, x296, x297, x299, x301, x302, x303, x304, x305, x307, x308, x309, x310, x311, x312, x313, x314, x315, x316, x317,  x319, x321, x322, x323, x324, x325, x326, x327, x328, x329, x331, x358, x359, x360, x361, x384, x385, x397, x398, x422, x424, x425, x426, x427, x430, x432, x434, x436, x437, x438, x439, x440, x441, x443, x444, x445, x456, x457, x458, x459) %>% 
  mutate(SHV = rowSums(.[,2:66])) %>% 
  select(RouteID, SHV)

Broadlc8km<-left_join(SHV8km, DSD8km, by = "RouteID")
Broadlc8km<-left_join(Broadlc8km, ADV8km, by = "RouteID")
Broadlc8km<-left_join(Broadlc8km, ISNV8km, by = "RouteID")
head(Broadlc8km)
lc8km<-left_join(lc8km, Broadlc8km, by = "RouteID")
head(lc8km)
###get proportions of landcover of interest: -----
lc8km<-lc8km %>% 
  mutate(p312_8km = (x312/Sum)) %>% 
  mutate(p316_8km = (x316/Sum)) %>% 
  mutate(p437_8km = (x437/Sum)) %>% 
  mutate(p484_8km = (x484/Sum)) %>% 
  mutate(p485_8km = (x485/Sum)) %>% 
  mutate(p488_8km = (x488/Sum)) %>% 
  mutate(p489_8km = (x489/Sum)) %>% 
  mutate(p490_8km = (x490/Sum)) %>% 
  mutate(p491_8km = (x491/Sum)) %>% 
  mutate(p492_8km = (x492/Sum)) %>% 
  mutate(p493_8km = (x493/Sum)) %>% 
  mutate(p495_8km = (x495/Sum)) %>% 
  mutate(p498_8km = (x498/Sum)) %>% 
  mutate(p556_8km = (x556/Sum)) %>% 
  mutate(p557_8km = (x557/Sum)) %>% 
  mutate(p558_8km = (x558/Sum)) %>% 
  mutate(pSHV_8km = (SHV/Sum)) %>% 
  mutate(pDSD_8km = (DSD/Sum)) %>% 
  mutate(pADV_8km = (ADV/Sum)) %>% 
  mutate(pISNV_8km = (ISNV/Sum)) 


head(lc8km)


##20km buffer ----
glc20km<-e20kmdf %>% 
  spread(key=cover, value=count)
glc20km<-setnames(glc20km, paste0("x",names(glc20km)))
colnames(glc20km)[1]<-"RouteID"
glc20km[is.na(glc20km)]<-0
glc20km<-mutate(.data = glc20km, Sum = rowSums(glc20km[,2:230]))
#subset out landcover of interest values only:
lc20km<-glc20km %>% 
  select(RouteID, x312, x316, x437, x484, x485, x488, x489, x490, x491, x492, x493, x495, x498, x556, x557, x558, Sum)
###subset out broad landcover values: ----
##Agriculture and Developed Vegetation:
ADV20km<-glc20km %>% 
  select(RouteID, x555, x556, x557) %>% 
  mutate(ADV = rowSums(.[,2:4])) %>% 
  select(RouteID, ADV)

##Introduced and Semi-natural Vegetation:
ISNV20km<-glc20km %>% 
  # select(RouteID, x558, x559, x560, x561, x562, x563) %>% 
  select(RouteID, x558, x559, x561, x562, x563) %>% 
  mutate(ISNV = rowSums(.[,2:6])) %>% 
  select(RouteID, ISNV)

## Desert and semi-desert
DSD20km<-glc20km %>% 
  # select(RouteID, x306, x442, x460, x461, x462, x466, x470, x471, x472, x473, x474, x476, x477, x484, x485, x486, x487, x488, x489, x490, x491, x492, x493, x494, x495, x496, x497, x498, x499, x538, x545) %>% 
  select(RouteID, x306, x460, x461, x466, x470, x471, x472, x476, x477, x484, x485, x486, x487, x488, x489, x490, x491, x492, x493, x494, x495, x496, x497, x498, x499,  x545) %>% 
  mutate(DSD = rowSums(.[,2:27])) %>% 
  select(RouteID, DSD)

##Shurb and herb vegetation:
SHV20km<-glc20km %>% 
  # select(RouteID, x117, x266, x269, x274, x275, x282, x296, x297, x298, x299, x301, x302, x303, x304, x305, x307, x308, x309, x310, x311, x312, x313, x314, x315, x316, x317, x318, x319, x321, x322, x323, x324, x325, x326, x327, x328, x329, x331, x358, x359, x360, x361, x381, x382, x383, x384, x385, x396, x397, x398, x422, x424, x425, x426, x427, x429, x430, x432, x433, x434, x436, x437, x438, x439, x440, x441, x443, x444, x445, x455, x456, x457, x458, x459, x508) %>% 
  select(RouteID, x117, x266, x269, x274, x275, x282, x296, x297, x299, x301, x302, x303, x304, x305, x307, x308, x309, x310, x311, x312, x313, x314, x315, x316, x317,  x319, x321, x322, x323, x324, x325, x326, x327, x328, x329, x331, x358, x359, x360, x361, x384, x385, x397, x398, x422, x424, x425, x426, x427, x430, x432, x434, x436, x437, x438, x439, x440, x441, x443, x444, x445, x456, x457, x458, x459) %>% 
  mutate(SHV = rowSums(.[,2:66])) %>% 
  select(RouteID, SHV)

Broadlc20km<-left_join(SHV20km, DSD20km, by = "RouteID")
Broadlc20km<-left_join(Broadlc20km, ADV20km, by = "RouteID")
Broadlc20km<-left_join(Broadlc20km, ISNV20km, by = "RouteID")
head(Broadlc20km)
lc20km<-left_join(lc20km, Broadlc20km, by = "RouteID")
head(lc20km)
###get proportions of landcover of interest: -----
lc20km<-lc20km %>% 
  mutate(p312_20km = (x312/Sum)) %>% 
  mutate(p316_20km = (x316/Sum)) %>% 
  mutate(p437_20km = (x437/Sum)) %>% 
  mutate(p484_20km = (x484/Sum)) %>% 
  mutate(p485_20km = (x485/Sum)) %>% 
  mutate(p488_20km = (x488/Sum)) %>% 
  mutate(p489_20km = (x489/Sum)) %>% 
  mutate(p490_20km = (x490/Sum)) %>% 
  mutate(p491_20km = (x491/Sum)) %>% 
  mutate(p492_20km = (x492/Sum)) %>% 
  mutate(p493_20km = (x493/Sum)) %>% 
  mutate(p495_20km = (x495/Sum)) %>% 
  mutate(p498_20km = (x498/Sum)) %>% 
  mutate(p556_20km = (x556/Sum)) %>% 
  mutate(p557_20km = (x557/Sum)) %>% 
  mutate(p558_20km = (x558/Sum)) %>% 
  mutate(pSHV_20km = (SHV/Sum)) %>% 
  mutate(pDSD_20km = (DSD/Sum)) %>% 
  mutate(pADV_20km = (ADV/Sum)) %>% 
  mutate(pISNV_20km = (ISNV/Sum)) 


head(lc20km)



#subset to only proportions:-----
plc400m<-lc400m[,c(1,23:42)]
plc1km<-lc1km[,c(1,23:42)]
plc4km<-lc4km[,c(1,23:42)]
plc5km<-lc5km[,c(1,23:42)]
plc8km<-lc8km[,c(1,23:42)]
plc20km<-lc20km[,c(1,23:42)]
# VegDRI data read in ---------
##2009 ----
may1709<-raster("/Volumes/Samsung_USB/VegDRI/2009/vegdri_emodis_week20_051709.tif")
# may1709<-raster("C:/Users/kim_serno1/Documents/PhD/GIS/Dissertation/data/unzipped/VegDRI/2009/vegdri_emodis_week20_051709.tif")
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


july2609<-raster("/Volumes/Samsung_USB/VegDRI/2009/vegdri_emodis_week30_072609.tif")
# july2609<-raster("C:/Users/kim_serno1/Documents/PhD/GIS/Dissertation/data/unzipped/VegDRI/2009/vegdri_emodis_week30_072609.tif")
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

##2010 -----
may1610<-raster("/Volumes/Samsung_USB/VegDRI/2010/vegdri_emodis_week20_051610.tif")
# may1610<-raster("C:/Users/kim_serno1/Documents/PhD/GIS/Dissertation/data/unzipped/VegDRI/2010/vegdri_emodis_week20_051610.tif")
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


july2510<-raster("/Volumes/Samsung_USB/VegDRI/2010/vegdri_emodis_week30_072510.tif")
# july2510<-raster("C:/Users/kim_serno1/Documents/PhD/GIS/Dissertation/data/unzipped/VegDRI/2010/vegdri_emodis_week30_072510.tif")
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

##2011 -----
may1511<-raster("/Volumes/Samsung_USB/VegDRI/2011/vegdri_emodis_week20_051511.tif")
# may1511<-raster("C:/Users/kim_serno1/Documents/PhD/GIS/Dissertation/data/unzipped/VegDRI/2011/vegdri_emodis_week20_051511.tif")
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

july2411<-raster("/Volumes/Samsung_USB/VegDRI/2011/vegdri_emodis_week30_072411.tif")
# july2411<-raster("C:/Users/kim_serno1/Documents/PhD/GIS/Dissertation/data/unzipped/VegDRI/2011/vegdri_emodis_week30_072411.tif")
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

##2012 ------
may1312<-raster("/Volumes/Samsung_USB/VegDRI/2012/vegdri_emodis_week20_051312.tif")
# may1312<-raster("C:/Users/kim_serno1/Documents/PhD/GIS/Dissertation/data/unzipped/VegDRI/2012/vegdri_emodis_week20_051312.tif")
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

july2212<-raster("/Volumes/Samsung_USB/VegDRI/2012/vegdri_emodis_week30_072212.tif")
# july2212<-raster("C:/Users/kim_serno1/Documents/PhD/GIS/Dissertation/data/unzipped/VegDRI/2012/vegdri_emodis_week30_072212.tif")
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

##2013 -------
may1913<-raster("/Volumes/Samsung_USB/VegDRI/2013/vegdri_emodis_week20_051913.tif")
# may1913<-raster("C:/Users/kim_serno1/Documents/PhD/GIS/Dissertation/data/unzipped/VegDRI/2013/vegdri_emodis_week20_051913.tif")
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

july2813<-raster("/Volumes/Samsung_USB/VegDRI/2013/vegdri_emodis_week30_072813.tif")
# july2813<-raster("C:/Users/kim_serno1/Documents/PhD/GIS/Dissertation/data/unzipped/VegDRI/2013/vegdri_emodis_week30_072813.tif")
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

# VegDRI mean of early, end season rasters =====
earlyvd<-mosaic(May1709, May1610, May1511, May1312, May1913, fun = mean, na.rm = TRUE)
latevd<-mosaic(July2609, July2510, July2411, July2212, July2813, fun = mean, na.rm = TRUE)
#VegDRI extract and mean across buffers ----
ROUTES_ID<-BRSP_Routes@data[c(2, 16)]
##Early Season: ----
evd400m<-raster::extract(earlyvd, BRSP_400m, fun = mean, df = TRUE, na.rm = TRUE)
VegEarly<-cbind(ROUTES_ID, evd400m)
VegEarly<-VegEarly[c(1,2,4)]
names(VegEarly)<-c("RTENAME", "RouteID", "evd400m")

evd1km<-raster::extract(earlyvd, BRSP_1km, fun = mean, df = TRUE, na.rm = TRUE)
VegEarly<-cbind(VegEarly, evd1km)
VegEarly<-VegEarly[c(1:3,5)]
names(VegEarly)<-c("RTENAME", "RouteID", "evd400m", "evd1km")

evd4km<-raster::extract(earlyvd, BRSP_4km, fun = mean, df = TRUE, na.rm = TRUE)
VegEarly<-cbind(VegEarly, evd4km)
VegEarly<-VegEarly[c(1:4,6)]
names(VegEarly)<-c("RTENAME", "RouteID", "evd400m", "evd1km", "evd4km")

evd5km<-raster::extract(earlyvd, BRSP_5km, fun = mean, df = TRUE, na.rm = TRUE)
VegEarly<-cbind(VegEarly, evd5km)
VegEarly<-VegEarly[c(1:5,7)]
names(VegEarly)<-c("RTENAME", "RouteID", "evd400m", "evd1km", "evd4km", "evd5km")

evd8km<-raster::extract(earlyvd, BRSP_8km, fun = mean, df = TRUE, na.rm = TRUE)
VegEarly<-cbind(VegEarly, evd8km)
VegEarly<-VegEarly[c(1:6,8)]
names(VegEarly)<-c("RTENAME", "RouteID", "evd400m", "evd1km", "evd4km", "evd5km", "evd8km")

evd20km<-raster::extract(earlyvd, BRSP_20km, fun = mean, df = TRUE, na.rm = TRUE)
VegEarly<-cbind(VegEarly, evd20km)
VegEarly<-VegEarly[c(1:7,9)]
names(VegEarly)<-c("RTENAME", "RouteID", "evd400m", "evd1km", "evd4km", "evd5km", "evd8km", "evd20km")
head(VegEarly)
summary(VegEarly)

##Late Season: ----
lvd400m<-raster::extract(latevd, BRSP_400m, fun = mean, df = TRUE, na.rm = TRUE)
VegLate<-cbind(ROUTES_ID, lvd400m)
VegLate<-VegLate[c(1,2,4)]
names(VegLate)<-c("RTENAME", "RouteID", "lvd400m")

lvd1km<-raster::extract(latevd, BRSP_1km, fun = mean, df = TRUE, na.rm = TRUE)
VegLate<-cbind(VegLate, lvd1km)
VegLate<-VegLate[c(1:3,5)]
names(VegLate)<-c("RTENAME", "RouteID", "lvd400m", "lvd1km")

lvd4km<-raster::extract(latevd, BRSP_4km, fun = mean, df = TRUE, na.rm = TRUE)
VegLate<-cbind(VegLate, lvd4km)
VegLate<-VegLate[c(1:4,6)]
names(VegLate)<-c("RTENAME", "RouteID", "lvd400m", "lvd1km", "lvd4km")

lvd5km<-raster::extract(latevd, BRSP_5km, fun = mean, df = TRUE, na.rm = TRUE)
VegLate<-cbind(VegLate, lvd5km)
VegLate<-VegLate[c(1:5,7)]
names(VegLate)<-c("RTENAME", "RouteID", "lvd400m", "lvd1km", "lvd4km", "lvd5km")

lvd8km<-raster::extract(latevd, BRSP_8km, fun = mean, df = TRUE, na.rm = TRUE)
VegLate<-cbind(VegLate, lvd8km)
VegLate<-VegLate[c(1:6,8)]
names(VegLate)<-c("RTENAME", "RouteID", "lvd400m", "lvd1km", "lvd4km", "lvd5km", "lvd8km")

lvd20km<-raster::extract(latevd, BRSP_20km, fun = mean, df = TRUE, na.rm = TRUE)
VegLate<-cbind(VegLate, lvd20km)
VegLate<-VegLate[c(1:7,9)]
names(VegLate)<-c("RTENAME", "RouteID", "lvd400m", "lvd1km", "lvd4km", "lvd5km", "lvd8km", "lvd20km")
head(VegLate)
summary(VegLate)
# PRISM data read in---------
library(prism)
#set pathway to prism data:
options(prism.path = "~/Dropbox/Baylor/PhD/GIS/unzipped/PRISM")
# options(prism.path = "C:/Users/kim_serno1/Dropbox/Baylor/PhD/GIS/unzipped/PRISM")
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
###mean across years: ------
winter_mean<-mosaic(winter09, winter10, winter11, winter12, winter13, fun = mean)
Winter_Means<-projectRaster(winter_mean, crs = CRS_gap)
###total across years: -----
winter_tot<-mosaic(winter09, winter10, winter11, winter12, winter13, fun = sum)
Winter_Total<-projectRaster(winter_tot, crs = CRS_gap)

####PRISM extract raster values and mean across buffer (Winter means) -----
ewm400m<-raster::extract(Winter_Means, BRSP_400m, fun = mean, df = TRUE, na.rm = TRUE)
precipWinterM<-cbind(ROUTES_ID, ewm400m)
precipWinterM<-precipWinterM[c(1,2,4)]
names(precipWinterM)<-c("RTENAME", "RouteID", "wm400m")

ewm1km<-raster::extract(Winter_Means, BRSP_1km, fun = mean, df = TRUE, na.rm = TRUE)
precipWinterM<-cbind(precipWinterM, ewm1km)
precipWinterM<-precipWinterM[c(1:3,5)]
names(precipWinterM)<-c("RTENAME", "RouteID", "pwm400m", "pwm1km")

ewm4km<-raster::extract(Winter_Means, BRSP_4km, fun = mean, df = TRUE, na.rm = TRUE)
precipWinterM<-cbind(precipWinterM, ewm4km)
precipWinterM<-precipWinterM[c(1:4,6)]
names(precipWinterM)<-c("RTENAME", "RouteID", "pwm400m", "pwm1km", "pwm4km")

ewm5km<-raster::extract(Winter_Means, BRSP_5km, fun = mean, df = TRUE, na.rm = TRUE)
precipWinterM<-cbind(precipWinterM, ewm5km)
precipWinterM<-precipWinterM[c(1:5,7)]
names(precipWinterM)<-c("RTENAME", "RouteID", "pwm400m", "pwm1km", "pwm4km", "pwm5km")

ewm8km<-raster::extract(Winter_Means, BRSP_8km, fun = mean, df = TRUE, na.rm = TRUE)
precipWinterM<-cbind(precipWinterM, ewm8km)
precipWinterM<-precipWinterM[c(1:6,8)]
names(precipWinterM)<-c("RTENAME", "RouteID", "pwm400m", "pwm1km", "pwm4km", "pwm5km", "pwm8km")

ewm20km<-raster::extract(Winter_Means, BRSP_20km, fun = mean, df = TRUE, na.rm = TRUE)
precipWinterM<-cbind(precipWinterM, ewm20km)
precipWinterM<-precipWinterM[c(1:7,9)]
names(precipWinterM)<-c("RTENAME", "RouteID", "pwm400m", "pwm1km", "pwm4km", "pwm5km", "pwm8km", "pwm20km")















####PRISM extract raster values and mean across buffer (Winter total) -----
ewt400m<-raster::extract(Winter_Total, BRSP_400m, fun = mean, df = TRUE, na.rm = TRUE)
precipWinterT<-cbind(ROUTES_ID, ewt400m)
precipWinterT<-precipWinterT[c(1,2,4)]
names(precipWinterT)<-c("RTENAME", "RouteID", "pwt400m")

ewt1km<-raster::extract(Winter_Total, BRSP_1km, fun = mean, df = TRUE, na.rm = TRUE)
precipWinterT<-cbind(precipWinterT, ewt1km)
precipWinterT<-precipWinterT[c(1:3,5)]
names(precipWinterT)<-c("RTENAME", "RouteID", "pwt400m", "pwt1km")

ewt4km<-raster::extract(Winter_Total, BRSP_4km, fun = mean, df = TRUE, na.rm = TRUE)
precipWinterT<-cbind(precipWinterT, ewt4km)
precipWinterT<-precipWinterT[c(1:4,6)]
names(precipWinterT)<-c("RTENAME", "RouteID", "pwt400m", "pwt1km", "pwt4km")

ewt5km<-raster::extract(Winter_Total, BRSP_5km, fun = mean, df = TRUE, na.rm = TRUE)
precipWinterT<-cbind(precipWinterT, ewt5km)
precipWinterT<-precipWinterT[c(1:5,7)]
names(precipWinterT)<-c("RTENAME", "RouteID", "pwt400m", "pwt1km", "pwt4km", "pwt5km")

ewt8km<-raster::extract(Winter_Total, BRSP_8km, fun = mean, df = TRUE, na.rm = TRUE)
precipWinterT<-cbind(precipWinterT, ewt8km)
precipWinterT<-precipWinterT[c(1:6,8)]
names(precipWinterT)<-c("RTENAME", "RouteID", "pwt400m", "pwt1km", "pwt4km", "pwt5km", "pwt8km")

ewt20km<-raster::extract(Winter_Total, BRSP_20km, fun = mean, df = TRUE, na.rm = TRUE)
precipWinterT<-cbind(precipWinterT, ewt20km)
precipWinterT<-precipWinterT[c(1:7,9)]
names(precipWinterT)<-c("RTENAME", "RouteID", "pwt400m", "pwt1km", "pwt4km", "pwt5km", "pwt8km", "pwt20km")


#Combine datafiles and save out as cvs----

Landcover<-left_join(plc400m, plc1km, by = "RouteID")
Landcover<-left_join(Landcover, plc4km, by = "RouteID")
Landcover<-left_join(Landcover, plc5km, by = "RouteID")
Landcover<-left_join(Landcover, plc8km, by = "RouteID")
Landcover<-left_join(Landcover, plc20km, by = "RouteID")
head(Landcover)
write.csv(Landcover, file="Landcover_all.csv")

Precipitation<-left_join(precipWinterT, precipWinterM, by = c("RouteID", "RTENAME"))

VegStress<-left_join(VegEarly, VegLate, by = c("RouteID", "RTENAME"))

Drought<-left_join(VegStress, Precipitation, by = c("RouteID", "RTENAME"))
write.csv(Drought, file = "Drought_all.csv")






