## 12 Sept 2017 - Cat
# Duration of Vegetativ Risk: Experiment data
# Aim: To see if chlorophyll measures are different with experimental groups

# Clear workspace
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
graphics.off()

# Load libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(lme4)
library(arm)


# Set Working Directory
setwd("~/Documents/git/freezingexperiment/analyses")
d <-read.csv("input/buds_traits.csv", header=TRUE)

## Start working on columns
d<-dplyr::select(d, NEW, TX, Bud, Chlorophyll, Conversion, freeze)

dx<-dplyr::select(d, NEW, TX, Chlorophyll)

dx$frz<-ifelse(d$TX=="A", 0, 1)
dx<-na.omit(dx)
dx$spp<-substr(dx$NEW, 0,6)
sp.mean<-aggregate(Chlorophyll~NEW + frz, dx, mean)
sp.mean$species<-substr(sp.mean$NEW, 0, 6)


qplot(spp, Chlorophyll, data = dx, 
      geom = "boxplot", color=TX) + 
  xlab("Species")+ylab("Chlorophyll measurement")


mod<-lmer(Chlorophyll~frz + (1|spp), data=dx)
summary(mod)
hist(dx$Chlorophyll[dx$frz==0])
hist(dx$Chlorophyll[dx$frz==1])
