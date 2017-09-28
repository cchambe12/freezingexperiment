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
bb<-read.csv("output/birches_buddata.csv", header=TRUE)

## Start working on columns
dx<-dplyr::select(d, NEW, TX, Bud, Chlorophyll)
dx$bud <- ave(dx$Bud, dx$NEW, FUN = seq_along)
dx$species<-substr(dx$NEW, 1, 6)
dx<-filter(dx, species !="SAMRAC")
dx<-dx[!is.na(dx$Chlorophyll),]
dx$ID<-paste(dx$NEW, dx$bud, sep="_")
bb$chlorophyll<-NA
for(i in c(1:nrow(bb))){
  for(j in c(1:nrow(dx)))
    if(bb$ID[i]==dx$ID[j])
      bb$chlorophyll[i]<-dx$Chlorophyll[j]
}
chl.mod<-lm(chlorophyll~frost+bud+species+frost*bud, data=bb)
display(chl.mod)

qplot(species, chlorophyll, data = bb, 
      geom = "boxplot", color=tx) + 
  xlab("Species")+ylab("Percent Budburst")

ggplot(bb, aes(x=bud, y=chlorophyll, color=as.factor(frost))) + geom_point() + geom_smooth(method="lm")

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
