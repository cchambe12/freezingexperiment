### WORKING ON BUDS DATA
## Now integrating whole species, look at provenance site effect?
## 29 SEPTEMBER 2017 - CAT


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
library(gridExtra)

# Set Working Directory
setwd("~/Documents/git/freezingexperiment/analyses")

#### Let's add in the other individuals now...
budspp<-read.csv("output/birches_speciesdata.csv", header=TRUE)
whole<-read.csv("input/bbch_data.csv", header=TRUE)
to.keep<-read.csv("..//planting/freezing_planting.csv", header=TRUE)

bb<-budspp[!is.na(budspp$dvr),]
bb$avg.rate<-ave(bb$dvr, bb$individ)
bb<-dplyr::select(bb, individ, species, budburst, leafout, frz, tx, avg.rate)
bb$bb<-ave(bb$budburst, bb$individ)
bb$lo<-ave(bb$leafout, bb$individ)
bb<-dplyr::select(bb, -budburst, -leafout)
bb<-bb[!duplicated(bb),]
# Start of Experiment was: 24 March 2017
start<-yday("2017/03/24")
bb$frz<-bb$frz-start
bb$dvr<-bb$lo-bb$bb

###### Time to clean buds experiment...
dx<-gather(whole, key=doy, value=bbch, -NEW, -TX)
keep<-unique(to.keep$id[to.keep$exp=="whole"])
dx<-filter(dx, NEW%in%keep)
dx$day<-substr(dx$doy, 2, 3)
dx$month<-substr(dx$doy, 5, 6)
dx$year<-substr(dx$doy, 8,11)
x<-paste(dx$year, dx$day, dx$month)
dx$date<-as.Date(strptime(x, format="%Y %d %m"))
dx$doy<-yday(dx$date)


start<-yday("2017/02/09")
dx$doy.adjusted<-dx$doy-start

dx<-dx[!is.na(dx$bbch),]
dx<-dx%>%dplyr::select(NEW, doy.adjusted, bbch)
last<-aggregate(dx$doy.adjusted, by = list(dx$NEW), max)
last<-last%>%rename(individ=Group.1)%>%rename(doy.adjusted=x)
last$leaf<-NA
for(i in c(1:nrow(last))) {
  for(j in c(1:nrow(dx)))
    if(last$individ[i]==dx$NEW[j] & last$doy.adjusted[i]==dx$doy.adjusted[j])
      last$leaf[i]<-dx$bbch[j]
}

first<-aggregate(dx$doy.adjusted, by = list(dx$NEW), min)
first<-first%>%rename(individ=Group.1)%>%rename(doy.adjusted=x)
first$bb<-NA
for(i in c(1:nrow(first))) {
  for(j in c(1:nrow(dx)))
    if(first$individ[i]==dx$NEW[j] & first$doy.adjusted[i]==dx$doy.adjusted[j])
      first$bb[i]<-dx$bbch[j]
}

first<-first%>%rename(budburst=doy.adjusted)%>%rename(bbch.first=bb)
last<-last%>%rename(leafout=doy.adjusted)%>%rename(bbch.last=leaf)
risk<-full_join(first, last)
risk$tx<-NA
for(i in c(1:nrow(risk))){
  for(j in c(i:nrow(whole)))
    if(risk$individ[i]==whole$NEW[j])
      risk$tx[i]<-whole$TX[j]
}

### Issues with loop... will fix last three by hand
bad<-risk[is.na(risk$tx),]
risk$tx<-ifelse(risk$individ=="ACEPEN_W07", "A", risk$tx)
risk$tx<-ifelse(risk$individ=="ACEPEN_W12", "A", risk$tx)
risk$tx<-ifelse(risk$individ=="ACEPEN_W13", "A", risk$tx)

risk$dvr<-risk$leafout-risk$budburst
risk<-risk%>%
  rename(bb=budburst)%>%
  rename(lo=leafout)%>%
  dplyr::select(-bbch.first, -bbch.last)
risk$species<-substr(risk$individ, 1, 6)
risk$site<-substr(risk$individ, 8, 8)
risk$site<-ifelse(risk$site=="W", "WM", risk$site)
risk$site<-ifelse(risk$site=="G", "GR", risk$site)
risk$site<-ifelse(risk$site=="S", "SH", risk$site)

bb$site<-substr(bb$individ, 8, 8)
bb$site<-ifelse(bb$site=="W", "WM", bb$site)
bb$site<-ifelse(bb$site=="G", "GR", bb$site)
bb<-dplyr::select(bb, individ, species, tx, bb, lo, dvr, site)
risk<-dplyr::select(risk, individ, species, tx, bb, lo, dvr, site)
dvr<-rbind(risk, bb)

mod<-lm(dvr~tx+species, data=dvr)
display(mod)

mod1<-lmer(dvr~tx+(1|species), data=dvr)
display(mod1)

qplot(species, dvr, data = dvr, 
      geom = "boxplot", color=tx) + 
  xlab("Species")+ylab("Duration of Vegetative Risk")


#### Check site effect...
vib<-filter(dvr, species=="VIBCAS")
mod3<-lm(dvr~site+tx, data=vib)
display(mod3)

#write.csv(dvr, file=("~/Documents/git/freezingexperiment/analyses/output/whole_dvr.csv"), row.names = FALSE)
