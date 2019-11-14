#test 

### Route_manipulation rcode 9/3/2019

library(tidyverse)

setwd("C:/Users/kim_serno1/Box/PhD/Dissertation/Great Plains BBS")


#subset routes to those in Great Plains US
BBSroutes<-read.csv("data_files/Unzipped/routes.csv")
head(BBSroutes)

BBSroutes$RouteID<-((BBSroutes$StateNum*1000)+BBSroutes$Route)
head(BBSroutes)


GPBBSroutes<-read.csv("data_files/Unzipped/BBSroutesGP2.csv")
head(GPBBSroutes)
GPBBSroutes$RouteID=GPBBSroutes$ROUTE_

GProutes<-left_join(x=GPBBSroutes, y=BBSroutes, by ="RouteID")
head(GProutes)


#State BBS data for 2015 - to investigate number of routes with target species:
#import states:
Colorado<-read.csv("data_files/Unzipped/Colorad.csv")
Iowa<-read.csv("data_files/Unzipped/Iowa.csv")
Kansas<-read.csv("data_files/Unzipped/Kansas.csv")
Louisiana<-read.csv("data_files/Unzipped/Louisia.csv")
Minnesota<-read.csv("data_files/Unzipped/Minneso.csv")
Missouri<-read.csv("data_files/Unzipped/Missour.csv")
Montana<-read.csv("data_files/Unzipped/Montana.csv")
NDakota<-read.csv("data_files/Unzipped/NDakota.csv")
Nebraska<-read.csv("data_files/Unzipped/Nebrask.csv")
NMexico<-read.csv("data_files/Unzipped/NMexico.csv")
Oklahoma<-read.csv("data_files/Unzipped/Oklahom.csv")
SDakota<-read.csv("data_files/Unzipped/SDakota.csv")
Texas<-read.csv("data_files/Unzipped/Texas.csv")
Wisconsin<-read.csv("data_files/Unzipped/Wiscons.csv")
Wyoming<-read.csv("data_files/Unzipped/Wyoming.csv")

GPstates<-rbind(Colorado, Iowa, Kansas, Louisiana, Minnesota, Missouri, Montana, NDakota, Nebraska, NMexico, Oklahoma, SDakota, Texas, Wisconsin, Wyoming)
GPstates$RouteID<-((GPstates$StateNum*1000)+GPstates$Route)

GPstates2015<-subset(GPstates, Year == 2015)
GPstates2010<-subset(GPstates, Year == 2010)

gpstates15<-merge(GProutes, GPstates2015, by = "RouteID")
length(unique(gpstates15$RouteID))
sp2015<-group_by(gpstates15, AOU) %>% 
  summarize(n=n_distinct(RouteID))

gpstates10<-merge(GProutes, GPstates2010, by = "RouteID")
length(unique(gpstates10$RouteID))
sp2010<-group_by(gpstates10, AOU) %>% 
  summarize(nroutes=n_distinct(RouteID))

#unique(GPstates2015$Year)

#subset by species:

#Black Tern
BLTE2015<-subset(GPstates2015, AOU == 00770)
GP2015BLTE<-merge(GProutes, BLTE2015, by = "RouteID")
dim(GP2015BLTE)

#Yellow-billed Cuckoo
YBCU2015<-subset(GPstates2015, AOU == 03870)
GP2015YBCU<-merge(GProutes, YBCU2015, by = "RouteID")
dim(GP2015YBCU)

#Short-eared Owl
SEOW2015<-subset(GPstates2015, AOU == 03670)
GP2015SEOW<-merge(GProutes, SEOW2015, by = "RouteID")
dim(GP2015SEOW)

#Loggerhead Shrike
LOSH2015<-subset(GPstates2015, AOU == 06220)
GP2015LOSH<-merge(GProutes, LOSH2015, by = "RouteID")
dim(GP2015LOSH)

#Field Sparrow
FISP2015<-subset(GPstates2015, AOU == 05630)
GP2015FISP<-merge(GProutes, FISP2015, by = "RouteID")
dim(GP2015FISP)

#Grasshopper Sparrow
GRSP2015<-subset(GPstates2015, AOU == 05460)
GP2015GRSP<-merge(GProutes, GRSP2015, by = "RouteID")
dim(GP2015GRSP)

#Lark Bunting
LARB2015<-subset(GPstates2015, AOU == 06050)
GP2015LARB<-merge(GProutes, LARB2015, by = "RouteID")
dim(GP2015LARB)

#Black-billed Cuckoo
BBCU2015<-subset(GPstates2015, AOU == 04750)
GP2015BBCU<-merge(GProutes, BBCU2015, by = "RouteID")
dim(GP2015BBCU)

#Red-headed Woodpecker
RHWO2015<-subset(GPstates2015, AOU == 04060)
GP2015RHWO<-merge(GProutes, RHWO2015, by = "RouteID")
dim(GP2015RHWO)

#Pied-billed Grebe
PBGR2015<-subset(GPstates2015, AOU == 00060)
GP2015PBGR<-merge(GProutes, PBGR2015, by = "RouteID")
dim(GP2015PBGR)

#American Bittern
AMBI2015<-subset(GPstates2015, AOU == 01900)
GP2015AMBI<-merge(GProutes, AMBI2015, by = "RouteID")
dim(GP2015AMBI)

#Black-crowned Night-heron
BCNH2015<-subset(GPstates2015, AOU == 02020)
GP2015BCNH<-merge(GProutes, BCNH2015, by = "RouteID")
dim(GP2015BCNH)

#Swainson's Hawk
SWHA2015<-subset(GPstates2015, AOU == 03420)
GP2015SWHA<-merge(GProutes, SWHA2015, by = "RouteID")
dim(GP2015SWHA)

#Upland Sandpiper
UPSA2015<-subset(GPstates2015, AOU == 02610)
GP2015UPSA<-merge(GProutes, UPSA2015, by = "RouteID")
dim(GP2015UPSA)

#Burrowing OWl
BUOW2015<-subset(GPstates2015, AOU == 03780)
GP2015BUOW<-merge(GProutes, BUOW2015, by = "RouteID")
dim(GP2015BUOW)

#Bell's Vireo
BEVI2015<-subset(GPstates2015, AOU == 06330)
GP2015BEVI<-merge(GProutes, BEVI2015, by = "RouteID")
dim(GP2015BEVI)

#Yellow Warbler
YEWA2015<-subset(GPstates2015, AOU == 06520)
GP2015YEWA<-merge(GProutes, YEWA2015, by = "RouteID")
dim(GP2015YEWA)

#Orchard Oriole
OROR2015<-subset(GPstates2015, AOU == 05060)
GP2015OROR<-merge(GProutes, OROR2015, by = "RouteID")
dim(GP2015OROR)

#Ferruginous Hawk
FEHA2015<-subset(GPstates2015, AOU == 03480)
GP2015FEHA<-merge(GProutes, FEHA2015, by = "RouteID")
dim(GP2015FEHA)

#Golden Eagle
GOEA2015<-subset(GPstates2015, AOU == 03490)
GP2015GOEA<-merge(GProutes, GOEA2015, by = "RouteID")
dim(GP2015GOEA)

#Prairie Falcon
PRFA2015<-subset(GPstates2015, AOU == 03550)
GP2015PRFA<-merge(GProutes, PRFA2015, by = "RouteID")
dim(GP2015PRFA)

#Scissor-tailed Flycatcher
STFL2015<-subset(GPstates2015, AOU == 04430)
GP2015STFL<-merge(GProutes, STFL2015, by = "RouteID")
dim(GP2015STFL)

#Willow Flycatcher
WIFL2015<-subset(GPstates2015, AOU == 04660)
GP2015WIFL<-merge(GProutes, WIFL2015, by = "RouteID")
dim(GP2015WIFL)

#Bewick's Wren
BEWR2015<-subset(GPstates2015, AOU == 07190)
GP2015BEWR<-merge(GProutes, BEWR2015, by = "RouteID")
dim(GP2015BEWR)

#Dickcissel
DICK2015<-subset(GPstates2015, AOU == 06040)
GP2015DICK<-merge(GProutes, DICK2015, by = "RouteID")
dim(GP2015DICK)

#Painted Bunting
PABU2015<-subset(GPstates2015, AOU == 06010)
GP2015PABU<-merge(GProutes, PABU2015, by = "RouteID")
dim(GP2015PABU)

#Long-billed Curlew
LBCU2015<-subset(GPstates2015, AOU == 02640)
GP2015LBCU<-merge(GProutes, LBCU2015, by = "RouteID")
dim(GP2015LBCU)






