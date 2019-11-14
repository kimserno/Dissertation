

library(tidyverse)

setwd("C:/Users/kim_serno1/Dropbox/Baylor/PhD/Dissertation/BBS_manipulation")


#subset routes to those in Great Plains US
BBSroutes<-read.csv("data_files/Unzipped/routes.csv")
head(BBSroutes)

BBSroutes$RouteID<-((BBSroutes$StateNum*1000)+BBSroutes$Route)
head(BBSroutes)
write.csv(BBSroutes, file = "BBS_routes.csv")

GPBBSroutes<-read.csv("data_files/Unzipped/BBSroutesGP2.csv")
head(GPBBSroutes)
GPBBSroutes$RouteID=GPBBSroutes$ROUTE_

GProutes<-left_join(x=GPBBSroutes, y=BBSroutes, by ="RouteID")
head(GProutes)


#State BBS data for 2015 - to investigate number of routes with target species:
#import states:
Colorado<-read.csv("data_files/Unzipped/BBS_states/Colorad.csv")
Iowa<-read.csv("data_files/Unzipped/BBS_states/Iowa.csv")
Kansas<-read.csv("data_files/Unzipped/BBS_states/Kansas.csv")
Louisiana<-read.csv("data_files/Unzipped/BBS_states/Louisia.csv")
Minnesota<-read.csv("data_files/Unzipped/BBS_states/Minneso.csv")
Missouri<-read.csv("data_files/Unzipped/BBS_states/Missour.csv")
Montana<-read.csv("data_files/Unzipped/BBS_states/Montana.csv")
NDakota<-read.csv("data_files/Unzipped/BBS_states/NDakota.csv")
Nebraska<-read.csv("data_files/Unzipped/BBS_states/Nebrask.csv")
NMexico<-read.csv("data_files/Unzipped/BBS_states/NMexico.csv")
Oklahoma<-read.csv("data_files/Unzipped/BBS_states/Oklahom.csv")
SDakota<-read.csv("data_files/Unzipped/BBS_states/SDakota.csv")
Texas<-read.csv("data_files/Unzipped/BBS_states/Texas.csv")
Wisconsin<-read.csv("data_files/Unzipped/BBS_states/Wiscons.csv")
Wyoming<-read.csv("data_files/Unzipped/BBS_states/Wyoming.csv")

GPstates<-rbind(Colorado, Iowa, Kansas, Louisiana, Minnesota, Missouri, Montana, NDakota, Nebraska, NMexico, Oklahoma, SDakota, Texas, Wisconsin, Wyoming)
GPstates$RouteID<-((GPstates$StateNum*1000)+GPstates$Route)
gpstates<-subset(GPstates, RPID == 101)

GPstates2015<-subset(gpstates, Year == 2015)
GPstates2010<-subset(gpstates, Year == 2010)

gpstates15<-merge(GProutes, GPstates2015, by = "RouteID")
length(unique(gpstates15$RouteID))
sp2015<-group_by(gpstates15, AOU) %>% 
  summarize(n=n_distinct(RouteID))

gpstates10<-merge(GProutes, GPstates2010, by = "RouteID")
length(unique(gpstates10$RouteID))
sp2010<-group_by(gpstates10, AOU) %>% 
  summarize(nroutes=n_distinct(RouteID))
#write.csv(sp2010, file = "gpsp2010.csv")
#write.csv(sp2015, file = "gpsp2015.csv")

BBS_SppList<-read.csv("data_files/BBS_SpeciesList.csv")
head(BBS_SppList)

gpsp2010<-left_join(x=sp2010, y=BBS_SppList, by="AOU") %>% 
  mutate(pct_routes = (nroutes/467)*100) %>% 
  subset(pct_routes >= 10)
head(gpsp2010)

write.csv(gpsp2010, file = "gpsp2010_2.csv")
#unique(GPstates2015$Year)

#subset by species:

#Black Tern
BLTE2015<-subset(GPstates2015, AOU == 00770)
GP2015BLTE<-merge(GProutes, BLTE2015, by = "RouteID")
dim(GP2015BLTE)

