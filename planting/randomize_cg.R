# False Spring Experiment Planting Script - CC
# 9 May 2017
# Randomize Individuals in experiment to sort into plotting order
### Super simple version. Randomize. 

library(dplyr)
library(tidyr)
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# Set own wd as needed
setwd("~/Documents/git/freezingexperiment/planting")

d<-read.csv("freezing_planting.csv", sep=",", header=TRUE)

d<-filter(d, d$exp=="whole" )

d$Species<- substr(d$id, 1, 6)
d$Site <- substr(d$id, 8, 8)
d$Number <- substr(d$id, 9, 10)

d<-transform(d, Number = sample(Number))
df<-d[order(d$Number),]

write.csv(df, "~/Documents/git/freezingexperiment/planting/random_planting.csv", row.names=FALSE)


######## BUDS ###########
setwd("~/Documents/git/freezingexperiment/planting")

dd<-read.csv("freezing_planting.csv", sep=",", header=TRUE)

dd<-dd%>% filter(dd$exp=="buds")

dd$Species<- substr(dd$id, 1, 6)
dd$Site <- substr(dd$id, 8, 8)
dd$Number <- substr(dd$id, 9, 10)

dd<-transform(dd, Number = sample(Number))
df1<-dd[order(dd$Number),]

write.csv(df1, "~/Documents/git/freezingexperiment/planting/random_buds.csv", row.names=FALSE)
