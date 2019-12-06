

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

BRSP_BBSroutes<-read.csv("BBS_manipulation/data_files/Unzipped/BRSP_BBSroutes2.csv")
head(BRSP_BBSroutes)

BRSProutes<-left_join(x=BRSP_BBSroutes, y=BRSP_BBSroutes, by ="RouteID")
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
length(unique(SGstates$RouteID))

SBstates<-subset(SGstates, RPID == 101)
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




df<-data.frame(matrix(ncol = 1, nrow = 1555))

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

noiseraw<-read.csv("C:/Users/kim_serno1/Documents/PhD/Dissertation/BBS_manipulation/data_files/Unzipped/VehicleData.csv", stringsAsFactors = FALSE)
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



weatherraw<-read.csv('C:/Users/kim_serno1/Documents/PhD/Dissertation/BBS_manipulation/data_files/Unzipped/weather.csv')
weatherraw$RouteID<-((weatherraw$StateNum*1000)+weatherraw$Route)
head(weatherraw)
weatherraw<-subset(weatherraw, RPID == 101, select = c(RouteID, Year, Month, Day, ObsN, EndTemp, QualityCurrentID, RunType))
head(weatherraw)
weatherraw$date<-as.Date(paste(weatherraw$Year,weatherraw$Month,weatherraw$Day, sep="-"))
weatherraw$yday<-yday(weatherraw$date)
head(weatherraw)
weather09<-weatherraw %>% 
  subset(Year == 2009, select = c(-Year, -Month, -Day)) %>% 
  dplyr::rename(
    ObsN09 = ObsN,
    EndTemp09 = EndTemp,
    QualityCurrentID09 = QualityCurrentID,
    RunType09 = RunType,
    yday09 = yday,
    day09 = date
  )
weather10<-weatherraw %>% 
  subset(Year == 2010, select = c(-Year, -Month, -Day)) %>% 
  dplyr::rename(
    ObsN10 = ObsN,
    EndTemp10 = EndTemp,
    QualityCurrentID10 = QualityCurrentID,
    RunType10 = RunType,
    yday10 = yday,
    day10 = date
  )
weather11<-weatherraw %>% 
  subset(Year == 2011, select = c(-Year, -Month, -Day)) %>% 
  dplyr::rename(
    ObsN11 = ObsN,
    EndTemp11 = EndTemp,
    QualityCurrentID11 = QualityCurrentID,
    RunType11 = RunType,
    yday11 = yday,
    day11 = date
  )
weather12<-weatherraw %>% 
  subset(Year == 2012, select = c(-Year, -Month, -Day)) %>% 
  dplyr::rename(
    ObsN12 = ObsN,
    EndTemp12 = EndTemp,
    QualityCurrentID12 = QualityCurrentID,
    RunType12 = RunType,
    yday12 = yday,
    day12 = date
  )
weather13<-weatherraw %>% 
  subset(Year == 2013, select = c(-Year, -Month, -Day)) %>% 
  dplyr::rename(
    ObsN13 = ObsN,
    EndTemp13 = EndTemp,
    QualityCurrentID13 = QualityCurrentID,
    RunType13 = RunType,
    yday13 = yday,
    day13 = date
  )

weather<-left_join(BRSP_cov, weather09, by = "RouteID")
weather<-left_join(weather, weather10, by = "RouteID")
weather<-left_join(weather, weather11, by = "RouteID")
weather<-left_join(weather, weather12, by = "RouteID")
weather<-left_join(weather, weather13, by = "RouteID")
