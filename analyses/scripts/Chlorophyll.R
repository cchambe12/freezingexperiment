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
area<-read.csv("input/SLA_buds.csv", header=TRUE)
wt<-read.csv("input/SLA_weight.csv", header=TRUE)

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
  xlab("Species")+ylab("Chlorophyll")

ggplot(bb, aes(x=bud, y=chlorophyll, color=as.factor(frost))) + geom_point() + geom_smooth(method="lm")

dx$frz<-ifelse(dx$TX=="A", 0, 1)
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
hist(bb$chlorophyll[bb$frost==0])
hist(bb$chlorophyll[bb$frost==1])

#write.csv(bb, file=("~/Documents/git/freezingexperiment/analyses/output/buds_traits.csv"), row.names=FALSE)

bb.chl<-bb[!is.na(bb$chlorophyll),]
table(bb.chl$individ)

area$ID<-paste(area$ID, area$number, sep=".")
area$dry<-NA
for(i in c(1:nrow(area))){
  for(j in c(1:nrow(wt)))
    if(area$ID[i]==wt$ID[j])
      area$dry[i]<-wt$dry[j]
}
area$sla<-area$Area/area$dry
area$ID<-substr(area$ID, 1, 10)
area$sla <- ave(area$sla, area$ID)
area<-dplyr::select(area, ID, sla)
area<-area[!duplicated(area),]

bb<-read.csv("output/buds_traits.csv", header=TRUE)
bb$sla<-NA
for(i in c(1:nrow(bb))){
  for(j in c(1:nrow(area)))
    if(bb$individ[i]==area$ID[j])
      bb$sla[i]<-area$sla[j]
}

bb$dvr<-as.numeric(bb$dvr)
bb$dvr<-ifelse(is.na(bb$dvr), 0, bb$dvr)
bb$dvr.avg<-ave(bb$dvr, bb$individ)
sla.mod<-lm(sla~tx+dvr.avg, data=bb)
display(sla.mod)
sla.mod2<-lm(sla~tx+dvr.avg+tx*dvr.avg, data=bb)
display(sla.mod2)

qplot(species, sla, data = bb, 
      geom = "boxplot", color=tx) + 
  xlab("Species")+ylab("SLA")

#write.csv(bb, file=("~/Documents/git/freezingexperiment/analyses/output/buds_traits.csv"), row.names=FALSE)
  