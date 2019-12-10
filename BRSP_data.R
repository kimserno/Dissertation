

library(tidyverse)
library(matrixStats)
library(plyr)
library(dplyr)
library(lubridate)
# setwd("C:/Users/kim_serno1/Dropbox/Baylor/PhD/Dissertation/BBS_manipulation")


#subset routes to those in Great Plains US
BBSroutes<-read.csv("BBS_manipulation/data_files/Unzipped/routes.csv")
head(BBSroutes)

BBSroutes$RouteID<-((BBSroutes$StateNum*1000)+BBSroutes$Route)
head(BBSroutes)
write.csv(BBSroutes, file = "BBS_manipulation/data_files/Unzipped/BBS_routes.csv")

BRSP_BBSroutes<-read.csv("BRSP_routesGIS.csv")
# BRSP_BBSroutes<-read.csv("BBS_manipulation/data_files/Unzipped/BRSP_BBSroutes2.csv")
head(BRSP_BBSroutes)

BRSProutes<-left_join(x=BRSP_BBSroutes, y=BBSroutes, by ="RouteID")
head(BRSProutes)


#State BBS data:
#import states:
Arizona<-read.csv("BBS_manipulation/data_files/Unzipped/BBS_states/Arizona.csv")
California<-read.csv("BBS_manipulation/data_files/Unzipped/BBS_states/Califor.csv")
Colorado<-read.csv("BBS_manipulation/data_files/Unzipped/BBS_states/Colorad.csv")
Idaho<-read.csv("BBS_manipulation/data_files/Unzipped/BBS_states/Idaho.csv")
Kansas<-read.csv("BBS_manipulation/data_files/Unzipped/BBS_states/Kansas.csv")
Montana<-read.csv("BBS_manipulation/data_files/Unzipped/BBS_states/Montana.csv")
NDakota<-read.csv("BBS_manipulation/data_files/Unzipped/BBS_states/NDakota.csv")
Nebraska<-read.csv("BBS_manipulation/data_files/Unzipped/BBS_states/Nebrask.csv")
Nevada<-read.csv("BBS_manipulation/data_files/Unzipped/BBS_states/Nevada.csv")
NMexico<-read.csv("BBS_manipulation/data_files/Unzipped/BBS_states/NMexico.csv")
Oregon<-read.csv("BBS_manipulation/data_files/Unzipped/BBS_states/Oregon.csv")
SDakota<-read.csv("BBS_manipulation/data_files/Unzipped/BBS_states/SDakota.csv")
Utah<-read.csv("BBS_manipulation/data_files/Unzipped/BBS_states/Utah.csv")
Washington<-read.csv("BBS_manipulation/data_files/Unzipped/BBS_states/Washing.csv")
Wyoming<-read.csv("BBS_manipulation/data_files/Unzipped/BBS_states/Wyoming.csv")

#subset to states of interest
SGstates<-rbind(Arizona, California, Colorado, Idaho, Kansas, Montana, NDakota, Nebraska, Nevada, NMexico, Oregon, SDakota, Utah, Washington, Wyoming)
SGstates$RouteID<-((SGstates$StateNum*1000)+SGstates$Route)
head(SGstates)
BRSP_rangeRoutes<-left_join(BRSProutes, SGstates, by = "RouteID")
length(unique(BRSP_rangeRoutes$RouteID))

SBstates<-subset(BRSP_rangeRoutes, RPID == 101)
length(unique(SBstates$RouteID))

##subset to years of interest 2009-2013:
routes2009<-subset(SBstates, Year == 2009)
routes2010<-subset(SBstates, Year == 2010)
routes2011<-subset(SBstates, Year == 2011)
routes2012<-subset(SBstates, Year == 2012)
routes2013<-subset(SBstates, Year == 2013)
#routes surveyed per year:
length(unique(routes2009$RouteID))
length(unique(routes2010$RouteID))
length(unique(routes2011$RouteID))
length(unique(routes2012$RouteID))
length(unique(routes2013$RouteID))




df<-data.frame(matrix(ncol = 1, nrow = length(unique(SBstates$RouteID))))

df$allroutes<-unique(SBstates$RouteID)
df$r2009<-0
df$r2010<-0
df$r2011<-0
df$r2012<-0
df$r2013<-0
df<-df[,-1]

c2009<-df$allroutes %in% routes2009$RouteID
df$r2009[c2009]<-1
c2010<-df$allroutes %in% routes2010$RouteID
df$r2010[c2010]<-1
c2011<-df$allroutes %in% routes2011$RouteID
df$r2011[c2011]<-1
c2012<-df$allroutes %in% routes2012$RouteID
df$r2012[c2012]<-1
c2013<-df$allroutes %in% routes2013$RouteID
df$r2013[c2013]<-1
df$sum<-rowSums(df[,2:6])
head(df)
table(df$sum)
sum(df$r2009)
sum(df$r2010)
sum(df$r2011)
sum(df$r2012)
sum(df$r2013)

#subset routes with surveys in each of the 5 years:
Sroutes<-subset(df, sum == 5, select = c(allroutes, sum))
#rename allroutes to RouteID to match data files:
names(Sroutes)<-c("RouteID", "sum")

#extract BRSP survey data from each year
brsp09<-subset(routes2009, AOU == 5620, select = c(RouteID, AOU, SpeciesTotal))
brsp10<-subset(routes2010, AOU == 5620, select = c(RouteID, AOU, SpeciesTotal))
brsp11<-subset(routes2011, AOU == 5620, select = c(RouteID, AOU, SpeciesTotal))
brsp12<-subset(routes2012, AOU == 5620, select = c(RouteID, AOU, SpeciesTotal))
brsp13<-subset(routes2013, AOU == 5620, select = c(RouteID, AOU, SpeciesTotal))
names(brsp09)<-c("RouteID", "AOU", "Ab09")
names(brsp10)<-c("RouteID", "AOU", "Ab10")
names(brsp11)<-c("RouteID", "AOU", "Ab11")
names(brsp12)<-c("RouteID", "AOU", "Ab12")
names(brsp13)<-c("RouteID", "AOU", "Ab13")


#join data 
BRSPab<-left_join(Sroutes, brsp09, by = "RouteID")
BRSPab<-left_join(BRSPab, brsp10, by = c("RouteID", "AOU"))
BRSPab<-left_join(BRSPab, brsp11, by = c("RouteID", "AOU"))
BRSPab<-left_join(BRSPab, brsp12, by = c("RouteID", "AOU"))
BRSPab<-left_join(BRSPab, brsp13, by = c("RouteID", "AOU"))
BRSPab[is.na(BRSPab)]<-0
BRSPab<-BRSPab %>% 
  mutate(Totalab = rowSums(.[,4:8])) %>% 
  mutate(Aveab = rowMeans(.[,4:8])) %>% 
  mutate(Stdev = apply(.[,4:8], 1, sd)) %>% 
  mutate(CoV = Stdev/Aveab)
BRSPab$CoV[is.nan(BRSPab$CoV)]<-0
BRSPab$pres <-0
BRSPab$pres<- ifelse(BRSPab$Totalab > 0,1,0)
head(BRSPab)


write.csv(BRSPab, file = "BRSP_Data0913.csv")

#begin creation of covariates file:
BRSP_cov<-BRSPab[,1:2]#get routes of study
head(BRSP_cov)

noiseraw<-read.csv("BBS_manipulation/data_files/Unzipped/VehicleData.csv", stringsAsFactors = FALSE)
head(noiseraw)
noiseraw$RouteID<-((noiseraw$StateNum*1000)+noiseraw$Route)
head(noiseraw)
noiseraw<-subset(noiseraw, RPID == 101)
noiseraw[,58:107]<-as.numeric(unlist(noiseraw[,58:107]))
head(noiseraw)
noiseraw<-noiseraw %>% 
  mutate(Propnoise = rowMeans(.[,58:107]))
head(noiseraw)
noise09<- noiseraw%>% 
  subset(Year == 2009, select = c( RouteID, Propnoise)) %>% 
  dplyr::rename(Propnoise09 = Propnoise)
head(noise09)
noise10<-noiseraw%>% 
  subset(Year == 2010, select = c(RouteID, Propnoise)) %>% 
  dplyr::rename(Propnoise10 = Propnoise)
noise11<-noiseraw%>% 
  subset(Year == 2011, select = c(RouteID, Propnoise)) %>% 
  dplyr::rename(Propnoise11 = Propnoise)
noise12<-noiseraw%>% 
  subset(Year == 2012, select = c(RouteID, Propnoise)) %>% 
  dplyr::rename(Propnoise12 = Propnoise)
noise13<-noiseraw%>% 
  subset(Year == 2013, select = c(RouteID, Propnoise)) %>% 
  dplyr::rename(Propnoise13 = Propnoise)

noise<-left_join(BRSP_cov, noise09, by = "RouteID")[,c(1,3)]
noise<-left_join(noise, noise10, by = "RouteID")
noise<-left_join(noise, noise11, by = "RouteID")
noise<-left_join(noise, noise12, by = "RouteID")
noise<-left_join(noise, noise13, by = "RouteID")
noise<-noise %>% 
  mutate(mean_noise = rowMeans(.[,2:6])) %>% 
  dplyr::select(RouteID, mean_noise)



weatherraw<-read.csv('BBS_manipulation/data_files/Unzipped/weather.csv', stringsAsFactors = FALSE) %>% 
  mutate(RouteID = (StateNum * 1000) + Route) %>% 
  filter(RPID == 101) %>% 
  dplyr::select(RouteID, Year, Month, Day, ObsN, EndTemp, QualityCurrentID) %>%
  mutate(date = as.Date(paste(Year, Month, Day, sep="-"))) %>% 
  mutate(yday = yday(date))
head(weatherraw)

weather09<-weatherraw %>% 
  subset(Year == 2009, select = c(-Year, -Month, -Day)) %>% 
  mutate(sday = yday - yday("2009-04-15")) %>% 
  dplyr::rename(
    ObsN09 = ObsN,
    EndTemp09 = EndTemp,
    QualityCurrentID09 = QualityCurrentID,
    yday09 = yday,
    date09 = date, 
    sday09 = sday
  )

weather10<-weatherraw %>% 
  subset(Year == 2010, select = c(-Year, -Month, -Day)) %>% 
  mutate(sday = yday - yday("2010-04-15")) %>% 
  dplyr::rename(
    ObsN10 = ObsN,
    EndTemp10 = EndTemp,
    QualityCurrentID10 = QualityCurrentID,
    yday10 = yday,
    date10 = date, 
    sday10 = sday
  )

weather11<-weatherraw %>% 
  subset(Year == 2011, select = c(-Year, -Month, -Day)) %>% 
  mutate(sday = yday - yday("2011-04-15")) %>% 
  dplyr::rename(
    ObsN11 = ObsN,
    EndTemp11 = EndTemp,
    QualityCurrentID11 = QualityCurrentID,
    yday11 = yday,
    date11 = date, 
    sday11 = sday
  )

weather12<-weatherraw %>% 
  subset(Year == 2012, select = c(-Year, -Month, -Day)) %>% 
  mutate(sday = yday - yday("2012-04-15")) %>% 
  dplyr::rename(
    ObsN12 = ObsN,
    EndTemp12 = EndTemp,
    QualityCurrentID12 = QualityCurrentID,
    yday12 = yday,
    date12 = date, 
    sday12 = sday
  )

weather13<-weatherraw %>% 
  subset(Year == 2013, select = c(-Year, -Month, -Day)) %>% 
  mutate(sday = yday - yday("2013-04-15")) %>% 
  dplyr::rename(
    ObsN13 = ObsN,
    EndTemp13 = EndTemp,
    QualityCurrentID13 = QualityCurrentID,
    yday13 = yday,
    date13 = date, 
    sday13 = sday
  )

weather<-BRSP_cov %>% 
  left_join(weather09, by = "RouteID") %>% 
  left_join(weather10, by = "RouteID") %>% 
  left_join(weather12, by = "RouteID") %>% 
  left_join(weather13, by = "RouteID") %>% 
  left_join(weather11, by = "RouteID")
weather[,2:32]<-as.numeric(unlist(weather[,2:32])) #change values to numeric
obs<-weather %>% 
  dplyr::select(RouteID, starts_with("ObsN")) %>% 
  mutate(countobs = apply(.[,2:6], 1, function(x)length(unique(x)))) %>% 
  dplyr::select(RouteID, countobs)
temp<-weather %>% 
  dplyr::select(RouteID, starts_with("EndTemp")) %>% 
  mutate(mean_temp = rowMeans(.[,2:6])) %>% 
  dplyr::select(RouteID, mean_temp)
quality<-weather %>% 
  dplyr::select(RouteID, starts_with("Quality")) %>% 
  mutate(prop_quality = rowMeans(.[,2:6])) %>% 
  dplyr::select(RouteID, prop_quality)
sday<-weather %>% 
  dplyr::select(RouteID, starts_with("sday")) %>% 
  mutate(msday = rowMeans(.[,2:6])) %>% 
  dplyr::select(RouteID, msday)
SurveyCov<-obs %>% 
  left_join(temp, by = "RouteID") %>% 
  left_join(quality, by = "RouteID") %>% 
  left_join(sday, by = "RouteID") %>% 
  left_join(noise, by = "RouteID")


write.csv(SurveyCov, file = "SurveyCov.csv") 

#Read in spatial data for spatial optimization:
Landcover<-read.csv("Landcover_all.csv")  
Drought<-read.csv("Drought_all.csv") 

BRSPdata_all<-BRSPab %>% 
  left_join(Landcover, by = "RouteID") %>% 
  left_join(Drought, by = "RouteID") %>% 
  left_join(SurveyCov, by = "RouteID") 

#spatial optimization:
##landcover: ----
#create individual dataframes for each variable
p312<-BRSPdata_all %>% 
  dplyr::select(RouteID, Totalab, Aveab, CoV, pres, starts_with("p312"))
p316<-BRSPdata_all %>% 
  dplyr::select(RouteID, Totalab, Aveab, CoV, pres, starts_with("p316"))
p437<-BRSPdata_all %>% 
  dplyr::select(RouteID, Totalab, Aveab, CoV, pres, starts_with("p437"))
p484<-BRSPdata_all %>% 
  dplyr::select(RouteID, Totalab, Aveab, CoV, pres, starts_with("p484"))
p485<-BRSPdata_all %>% 
  dplyr::select(RouteID, Totalab, Aveab, CoV, pres, starts_with("p485"))
p488<-BRSPdata_all %>% 
  dplyr::select(RouteID, Totalab, Aveab, CoV, pres, starts_with("p488"))
p489<-BRSPdata_all %>% 
  dplyr::select(RouteID, Totalab, Aveab, CoV, pres, starts_with("p489"))
p490<-BRSPdata_all %>% 
  dplyr::select(RouteID, Totalab, Aveab, CoV, pres, starts_with("p490"))
p491<-BRSPdata_all %>% 
  dplyr::select(RouteID, Totalab, Aveab, CoV, pres, starts_with("p491"))
p492<-BRSPdata_all %>% 
  dplyr::select(RouteID, Totalab, Aveab, CoV, pres, starts_with("p492"))
p493<-BRSPdata_all %>% 
  dplyr::select(RouteID, Totalab, Aveab, CoV, pres, starts_with("p493"))
p495<-BRSPdata_all %>% 
  dplyr::select(RouteID, Totalab, Aveab, CoV, pres, starts_with("p495"))
p498<-BRSPdata_all %>% 
  dplyr::select(RouteID, Totalab, Aveab, CoV, pres, starts_with("p498"))
p556<-BRSPdata_all %>% 
  dplyr::select(RouteID, Totalab, Aveab, CoV, pres, starts_with("p556"))
p558<-BRSPdata_all %>% 
  dplyr::select(RouteID, Totalab, Aveab, CoV, pres, starts_with("p558"))
p557<-BRSPdata_all %>% 
  dplyr::select(RouteID, Totalab, Aveab, CoV, pres, starts_with("p557"))
pSHV<-BRSPdata_all %>% 
  dplyr::select(RouteID, Totalab, Aveab, CoV, pres, starts_with("pSHV"))
pDSD<-BRSPdata_all %>% 
  dplyr::select(RouteID, Totalab, Aveab, CoV, pres, starts_with("pDSD"))
pADV<-BRSPdata_all %>% 
  dplyr::select(RouteID, Totalab, Aveab, CoV, pres, starts_with("pADV"))
pISNV<-BRSPdata_all %>% 
  dplyr::select(RouteID, Totalab, Aveab, CoV, pres, starts_with("pISNV"))

###correlation matrices for each variable: ------
r2p312<-as.data.frame(cor(p312, method = "spearman"))
r2p312<-r2p312[c(-1, -3, -6:-11),-1:-5]
z<-apply(r2p312,1,which.max)
n312<-names(r2p312)[z]
n312<-append(n312, "n312", after = 0)

r2p316<-as.data.frame(cor(p316, method = "spearman"))
r2p316<-r2p316[c(-1, -3, -6:-11),-1:-5]
z<-apply(r2p316,1,which.max)
n316<-names(r2p316)[z]
n316<-append(n316, "n316", after = 0)

r2p437<-as.data.frame(cor(p437, method = "spearman"))
r2p437<-r2p437[c(-1, -3, -6:-11),-1:-5]
z<-apply(r2p437,1,which.max)
n437<-names(r2p437)[z]
n437<-append(n437, "n437", after = 0)

r2p484<-as.data.frame(cor(p484, method = "spearman"))
r2p484<-r2p484[c(-1, -3, -6:-11),-1:-5]
z<-apply(r2p484,1,which.max)
n484<-names(r2p484)[z]
n484<-append(n484, "n484", after = 0)

r2p485<-as.data.frame(cor(p485, method = "spearman"))
r2p485<-r2p485[c(-1, -3, -6:-11),-1:-5]
z<-apply(r2p485,1,which.max)
n485<-names(r2p485)[z]
n485<-append(n485, "n485", after = 0)

r2p488<-as.data.frame(cor(p488, method = "spearman"))
r2p488<-r2p488[c(-1, -3, -6:-11),-1:-5]
z<-apply(r2p488,1,which.max)
n488<-names(r2p488)[z]
n488<-append(n488, "n488", after = 0)

r2p489<-as.data.frame(cor(p489, method = "spearman"))
r2p489<-r2p489[c(-1, -3, -6:-11),-1:-5]
z<-apply(r2p489,1,which.max)
n489<-names(r2p489)[z]
n489<-append(n489, "n489", after = 0)

r2p490<-as.data.frame(cor(p490, method = "spearman"))
r2p490<-r2p490[c(-1, -3, -6:-11),-1:-5]
z<-apply(r2p490,1,which.max)
n490<-names(r2p490)[z]
n490<-append(n490, "n490", after = 0)

r2p491<-as.data.frame(cor(p491, method = "spearman"))
r2p491<-r2p491[c(-1, -3, -6:-11),-1:-5]
z<-apply(r2p491,1,which.max)
n491<-names(r2p491)[z]
n491<-append(n491, "n491", after = 0)

r2p492<-as.data.frame(cor(p492, method = "spearman"))
r2p492<-r2p492[c(-1, -3, -6:-11),-1:-5]
z<-apply(r2p492,1,which.max)
n492<-names(r2p492)[z]
n492<-append(n492, "n492", after = 0)

r2p493<-as.data.frame(cor(p493, method = "spearman"))
r2p493<-r2p493[c(-1, -3, -6:-11),-1:-5]
z<-apply(r2p493,1,which.max)
n493<-names(r2p493)[z]
n493<-append(n493, "n493", after = 0)

r2p495<-as.data.frame(cor(p495, method = "spearman"))
r2p495<-r2p495[c(-1, -3, -6:-11),-1:-5]
z<-apply(r2p495,1,which.max)
n495<-names(r2p495)[z]
n495<-append(n495, "n495", after = 0)

r2p498<-as.data.frame(cor(p498, method = "spearman"))
r2p498<-r2p498[c(-1, -3, -6:-11),-1:-5]
z<-apply(r2p498,1,which.max)
n498<-names(r2p498)[z]
n498<-append(n498, "n498", after = 0)

r2p556<-as.data.frame(cor(p556, method = "spearman"))
r2p556<-r2p556[c(-1, -3, -6:-11),-1:-5]
z<-apply(r2p556,1,which.max)
n556<-names(r2p556)[z]
n556<-append(n556, "n556", after = 0)

r2p557<-as.data.frame(cor(p557, method = "spearman"))
r2p557<-r2p557[c(-1, -3, -6:-11),-1:-5]
z<-apply(r2p557,1,which.max)
n557<-names(r2p557)[z]
n557<-append(n557, "n557", after = 0)

r2p558<-as.data.frame(cor(p558, method = "spearman"))
r2p558<-r2p558[c(-1, -3, -6:-11),-1:-5]
z<-apply(r2p558,1,which.max)
n558<-names(r2p558)[z]
n558<-append(n558, "n558", after = 0)

r2pADV<-as.data.frame(cor(pADV, method = "spearman"))
r2pADV<-r2pADV[c(-1, -3, -6:-11),-1:-5]
z<-apply(r2pADV,1,which.max)
nADV<-names(r2pADV)[z]
nADV<-append(nADV, "nADV", after = 0)

r2pDSD<-as.data.frame(cor(pDSD, method = "spearman"))
r2pDSD<-r2pDSD[c(-1, -3, -6:-11),-1:-5]
z<-apply(r2pDSD,1,which.max)
nDSD<-names(r2pDSD)[z]
nDSD<-append(nDSD, "nDSD", after = 0)

r2pISNV<-as.data.frame(cor(pISNV, method = "spearman"))
r2pISNV<-r2pISNV[c(-1, -3, -6:-11),-1:-5]
z<-apply(r2pISNV,1,which.max)
nISNV<-names(r2pISNV)[z]
nISNV<-append(nISNV, "nISNV", after = 0)

r2pSHV<-as.data.frame(cor(pSHV, method = "spearman"))
r2pSHV<-r2pSHV[c(-1, -3, -6:-11),-1:-5]
z<-apply(r2pSHV,1,which.max)
nSHV<-names(r2pSHV)[z]
nSHV<-append(nSHV, "nSHV", after = 0)

####create data frame with spatial extend with highest R2 value related to dependent variable: ------
lcso <- data.frame(matrix(ncol = 4, nrow = 1))
x <- c("landc", "Totalab", "CoV", "pres" )
colnames(lcso) <- x
lcso<-rbind(lcso,n312,n316, n437, n484, n485, n488, n489, n490, n491, n492, n493, n495, n498, n556, n557, n558, nADV, nDSD, nISNV, nSHV)
lcso<-lcso[-1,]
##Drought:------
####create individual dataframes for each variable 
earlyvd<-BRSPdata_all %>% 
  dplyr::select(RouteID, Totalab, Aveab, CoV, pres, starts_with("evd"))
latevd<-BRSPdata_all %>% 
  dplyr::select(RouteID, Totalab, Aveab, CoV, pres, starts_with("lvd"))
meanprecip<-BRSPdata_all %>% 
  dplyr::select(RouteID, Totalab, Aveab, CoV, pres, starts_with("pwm"))
totalprecip<-BRSPdata_all %>% 
  dplyr::select(RouteID, Totalab, Aveab, CoV, pres, starts_with("pwt"))

###correlation matrices for each variable: ------
r2evd<-as.data.frame(cor(earlyvd, method = "spearman"))
r2evd<-r2evd[c(-1, -3, -6:-11),-1:-5]
z<-apply(r2evd,1,which.max)
nevd<-names(r2evd)[z]
nevd<-append(nevd, "nevd", after = 0)

r2lvd<-as.data.frame(cor(latevd, method = "spearman"))
r2lvd<-r2lvd[c(-1, -3, -6:-11),-1:-5]
z<-apply(r2lvd,1,which.max)
nlvd<-names(r2lvd)[z]
nlvd<-append(nlvd, "nlvd", after = 0)

r2pwm<-as.data.frame(cor(meanprecip, method = "spearman"))
r2pwm<-r2pwm[c(-1, -3, -6:-11),-1:-5]
z<-apply(r2pwm,1,which.max)
npwm<-names(r2pwm)[z]
npwm<-append(npwm, "npwm", after = 0)

r2pwt<-as.data.frame(cor(totalprecip, method = "spearman"))
r2pwt<-r2pwt[c(-1, -3, -6:-11),-1:-5]
z<-apply(r2pwt,1,which.max)
npwt<-names(r2pwt)[z]
npwt<-append(npwt, "npwt", after = 0)

####create dataframe ------
dso <- data.frame(matrix(ncol = 4, nrow = 1))
x <- c("drought", "Totalab", "CoV", "pres" )
colnames(dso) <- x
dso<-rbind(dso,nevd, nlvd, npwm, npwt)
dso<-dso[-1,]


#subset out varibles of interest for each dependent variable based on spatial optimization -------
BRSP_totab<-BRSPdata_all %>% 
  dplyr::select(RouteID, Totalab, lcso$Totalab, dso$Totalab, countobs, mean_temp, prop_quality, msday, mean_noise) 
  write.csv(BRSP_totab,file = "BRSP_totab.csv")
BRSP_CoV<-BRSPdata_all %>% 
  dplyr::select(RouteID, CoV, lcso$CoV, dso$CoV, countobs, mean_temp, prop_quality, msday, mean_noise)
write.csv(BRSP_CoV,file = "BRSP_CoV.csv")
BRSP_pres<-BRSPdata_all %>% 
  dplyr::select(RouteID, pres, lcso$pres, dso$pres, countobs, mean_temp, prop_quality, msday, mean_noise) 
write.csv(BRSP_pres,file = "BRSP_pres.csv")

