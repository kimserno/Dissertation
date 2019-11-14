
library(tidyverse)

setwd("C:/Users/kim_serno1/Dropbox/Baylor/PhD/Dissertation/BBS_manipulation")


##subset routes to those in Sagebrush (GRSG range atm)
#Start by adjusting BBS routes numbers to match shapefile routes
BBSroutes<-read.csv("data_files/Unzipped/routes.csv")
head(BBSroutes)

BBSroutes$RouteID<-((BBSroutes$StateNum*1000)+BBSroutes$Route)
head(BBSroutes)


#subset to GRSG range/sagebrush
Sageroutes<-read.csv("data_files/Unzipped/GRSG_BBSroutes.csv")
head(Sageroutes)
Sageroutes$RouteID=Sageroutes$rteno
head(Sageroutes)

SBroutes<-left_join(x=Sageroutes, y=BBSroutes, by ="RouteID")
head(SBroutes)

#State BBS data for 2015 - to investigate number of routes with target species:
#import states:
Arizona<-read.csv("data_files/Unzipped/BBS_states/Arizona.csv")
California<-read.csv("data_files/Unzipped/BBS_states/Califor.csv")
Colorado<-read.csv("data_files/Unzipped/BBS_states/Colorad.csv")
Idaho<-read.csv("data_files/Unzipped/BBS_states/Idaho.csv")
Kansas<-read.csv("data_files/Unzipped/BBS_states/Kansas.csv")
Montana<-read.csv("data_files/Unzipped/BBS_states/Montana.csv")
NDakota<-read.csv("data_files/Unzipped/BBS_states/NDakota.csv")
Nebraska<-read.csv("data_files/Unzipped/BBS_states/Nebrask.csv")
Nevada<-read.csv("data_files/Unzipped/BBS_states/Nevada.csv")
NMexico<-read.csv("data_files/Unzipped/BBS_states/NMexico.csv")
Oregon<-read.csv("data_files/Unzipped/BBS_states/Oregon.csv")
SDakota<-read.csv("data_files/Unzipped/BBS_states/SDakota.csv")
Utah<-read.csv("data_files/Unzipped/BBS_states/Utah.csv")
Washington<-read.csv("data_files/Unzipped/BBS_states/Washing.csv")
Wyoming<-read.csv("data_files/Unzipped/BBS_states/Wyoming.csv")

#subset to states of interest
SGstates<-rbind(Arizona, California, Colorado, Idaho, Kansas, Montana, NDakota, Nebraska, Nevada, NMexico, Oregon, SDakota, Utah, Washington, Wyoming)
SGstates$RouteID<-((SGstates$StateNum*1000)+SGstates$Route)
head(SGstates)
SBstates<-subset(SGstates, RPID == 101)

#Subset by years of interest
SGstates2015<-subset(SBstates, Year == 2015)
SGstates2010<-subset(SBstates, Year == 2010)

BRSPBBSroutes<-read.csv("data_files/Unzipped/BRSP_BBSRoutes.csv")
head(BRSPBBSroutes)
BRSPBBSroutes$RouteID<-((BRSPBBSroutes$StateNum*1000)+BRSPBBSroutes$Route)
head(BRSPBBSroutes)

BRSP2015<-merge(BRSPBBSroutes, SGstates2015, by = "RouteID")
length(unique(BRSP2015$RouteID))
sp2015<-group_by(BRSP2015, AOU) %>% 
  summarize(n=n_distinct(RouteID))
BRSP2010<-merge(BRSPBBSroutes, SGstates2010, by = "RouteID")
length(unique(BRSP2010$RouteID))
sp2015<-group_by(BRSP2010, AOU) %>% 
  summarize(n=n_distinct(RouteID))

SABSBBSroutes<-read.csv("data_files/Unzipped/SABS_BBSRoutes.csv")
head(SABSBBSroutes)
BRSPBBSroutes$RouteID<-((SABSBBSroutes$StateNum*1000)+SABSBBSroutes$Route)
head(SABSBBSroutes)

SABS2015<-merge(SABSBBSroutes, SGstates2015, by = "RouteID")
length(unique(SABS2015$RouteID))
sp2015<-group_by(SABS2015, AOU) %>% 
  summarize(n=n_distinct(RouteID))
SABS2010<-merge(SABSBBSroutes, SGstates2010, by = "RouteID")
length(unique(SABS2010$RouteID))
sp2015<-group_by(SABS2010, AOU) %>% 
  summarize(n=n_distinct(RouteID))

SATHBBSroutes<-read.csv("data_files/Unzipped/SATH_BBSRoutes.csv")
head(SATHBBSroutes)
BRSPBBSroutes$RouteID<-((SATHBBSroutes$StateNum*1000)+SATHBBSroutes$Route)
head(SATHBBSroutes)

SATH2015<-merge(SATHBBSroutes, SGstates2015, by = "RouteID")
length(unique(SATH2015$RouteID))
sp2015<-group_by(SATH2015, AOU) %>% 
  summarize(n=n_distinct(RouteID))
SATH2010<-merge(SATHBBSroutes, SGstates2010, by = "RouteID")
length(unique(SATH2010$RouteID))
sp2015<-group_by(SATH2010, AOU) %>% 
  summarize(n=n_distinct(RouteID))

sgstates15<-merge(SBroutes, SGstates2015, by = "RouteID")
length(unique(sgstates15$RouteID))
sp2015<-group_by(sgstates15, AOU) %>% 
  summarize(n=n_distinct(RouteID))

sgstates10<-merge(SBroutes, SGstates2010, by = "RouteID")
length(unique(sgstates10$RouteID))
sp2010<-group_by(sgstates10, AOU) %>% 
  summarize(nroutes=n_distinct(RouteID))









