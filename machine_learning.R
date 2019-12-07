

library(gmodels)

#read in data files and normalize data:
BRSP_totab<-read.csv("BRSP_totab.csv")
BRSP_CoV<-read.csv("BRSP_CoV.csv")
BRSP_pres<-read.csv("BRSP_pres.csv")                          
#Normalize variables not between 0-1:
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
totab_n <- as.data.frame(lapply(BRSP_totab[2:31], normalize))

#Nearest Neighbor Classification:





