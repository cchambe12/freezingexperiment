## 31 August 2017
# working on weather data to try and evaluate FSI across 4 field sites

# Clear workspace
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
graphics.off()

# Load libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)

# Set Working Directory
setwd("~/Documents/git/freezingexperiment/analyses/input")
d<-read.csv("weather_allsites.csv", header=TRUE)

d<-dplyr::rename(d, doy=Julian.Date)
d$year<-substr(d$Date, 7, 10)

### Harvard Forest #####
hf<-d%>%filter(site=="hf")%>%filter(year>=1990)
hf<-hf %>% filter(doy <=240)
hf$gdd <- hf$AirTMax - 5 # Can be 0 here if want 0 degC as threshold
hf$gdd <-ifelse(hf$gdd>0, hf$gdd, 0)
hf$count <- ave(
  hf$gdd, hf$year, 
  FUN=function(x) cumsum(c(0, head(x, -1)))
)
hf$frz<- ifelse((hf$AirTMin<=-2.2), 1, 0)
hf$fs<- ifelse((hf$count >= 200 & hf$frz == 1 & hf$count<=300), TRUE, NA)
hf.fsi<-hf%>%dplyr::select(year, fs)
hf.fsi<-hf.fsi[!duplicated(hf.fsi),]
hf.fsi<-na.omit(hf.fsi)


### White Mountains #####
wm<-d%>%filter(site=="bart")%>%filter(year>=1990)
wm<-wm %>% filter(doy <=240)
wm$gdd <- wm$AirTMax - 5 # Can be 0 here if want 0 degC as threshold
wm$gdd <-ifelse(wm$gdd>0, wm$gdd, 0)
wm$count <- ave(
  wm$gdd, wm$year, 
  FUN=function(x) cumsum(c(0, head(x, -1)))
)
wm$frz<- ifelse((wm$AirTMin<=-2.2), 1, 0)
wm$fs<- ifelse((wm$count >= 200 & wm$frz == 1 & wm$count<=300), TRUE, NA)
wm.fsi<-wm%>%dplyr::select(year, fs)
wm.fsi<-wm.fsi[!duplicated(wm.fsi),]
wm.fsi<-na.omit(wm.fsi)


### Grant ##### Need to find average because two stations...
gr<-d%>%filter(site=="berlin")%>%filter(year>=1990)
gr<-gr %>% filter(doy <=240)
gr$gdd <- gr$AirTMax - 5 # Can be 0 here if want 0 degC as threshold
gr$gdd <-ifelse(gr$gdd>0, gr$gdd, 0)
gr$count <- ave(
  gr$gdd, gr$year, 
  FUN=function(x) cumsum(c(0, head(x, -1)))
)
gr$frz<- ifelse((gr$AirTMin<=-2.2), 1, 0)
gr$fs<- ifelse((gr$count >= 200 & gr$frz == 1 & gr$count<=300), TRUE, NA)
gr.fsi<-gr%>%dplyr::select(year, fs)
gr.fsi<-gr.fsi[!duplicated(gr.fsi),]
gr.fsi<-na.omit(gr.fsi)

gr1<-d%>%filter(site=="merr")%>%filter(year>=1990)
gr1<-gr1 %>% filter(doy <=240)
gr1$gdd <- gr1$AirTMax - 5 # Can be 0 here if want 0 degC as threshold
gr1$gdd <-ifelse(gr1$gdd>0, gr1$gdd, 0)
gr1$count <- ave(
  gr1$gdd, gr1$year, 
  FUN=function(x) cumsum(c(0, head(x, -1)))
)
gr1$frz<- ifelse((gr1$AirTMin<=-2.2), 1, 0)
gr1$fs<- ifelse((gr1$count >= 200 & gr1$frz == 1 & gr1$count<=300), TRUE, NA)
gr1.fsi<-gr1%>%dplyr::select(year, fs)
gr1.fsi<-gr1.fsi[!duplicated(gr1.fsi),]
gr1.fsi<-na.omit(gr1.fsi)

gr2<-d%>%filter(site=="ct")%>%filter(year>=1990)
gr2<-gr2 %>% filter(doy <=240)
gr2$gdd <- gr2$AirTMax - 5 # Can be 0 here if want 0 degC as threshold
gr2$gdd <-ifelse(gr2$gdd>0, gr2$gdd, 0)
gr2$count <- ave(
  gr2$gdd, gr2$year, 
  FUN=function(x) cumsum(c(0, head(x, -1)))
)
gr2$frz<- ifelse((gr2$AirTMin<=-2.2), 1, 0)
gr2$fs<- ifelse((gr2$count >= 200 & gr2$frz == 1 & gr2$count<=300), TRUE, NA)
gr2.fsi<-gr2%>%dplyr::select(year, fs)
gr2.fsi<-gr2.fsi[!duplicated(gr2.fsi),]
gr2.fsi<-na.omit(gr2.fsi)

### Saint Hipp #####
sh<-d%>%filter(site=="sh")%>%filter(year>=1990)
sh<-sh %>% filter(doy <=240)
sh$gdd <- sh$AirTMax - 5 # Can be 0 here if want 0 degC as threshold
sh$gdd <-ifelse(sh$gdd>0, sh$gdd, 0)
sh$count <- ave(
  sh$gdd, sh$year, 
  FUN=function(x) cumsum(c(0, head(x, -1)))
)
sh$frz<- ifelse((sh$AirTMin<=-2.2), 1, 0)
sh$fs<- ifelse((sh$count >= 200 & sh$frz == 1 & sh$count<=300), TRUE, NA)
sh.fsi<-sh%>%dplyr::select(year, fs)
sh.fsi<-sh.fsi[!duplicated(sh.fsi),]
sh.fsi<-na.omit(sh.fsi)

############### Find Last Freeze Dates for each site.....
## Harvard Forest ##
hf.last<-hf%>%filter(frz>0)
hf.last<-hf.last[order(hf.last$doy,hf.last$year),]
hf.last<-hf.last[!duplicated(hf.last$year, fromLast=TRUE),]
hf.last$last<-hf.last$Date

## White Mountains ##
wm.last<-wm%>%filter(frz>0)
wm.last<-wm.last[order(wm.last$doy,wm.last$year),]
wm.last<-wm.last[!duplicated(wm.last$year, fromLast=TRUE),]
wm.last$last<-wm.last$Date

## Grant(s)...
# 1:
gr.last<-gr%>%filter(frz>0)
gr.last<-gr.last[order(gr.last$doy,gr.last$year),]
gr.last<-gr.last[!duplicated(gr.last$year, fromLast=TRUE),]
gr.last$last<-gr.last$Date
# 2:
gr1.last<-gr1%>%filter(frz>0)
gr1.last<-gr1.last[order(gr1.last$doy,gr1.last$year),]
gr1.last<-gr1.last[!duplicated(gr1.last$year, fromLast=TRUE),]
gr1.last$last1<-gr1.last$Date
# 3:
gr2.last<-gr2%>%filter(frz>0)
gr2.last<-gr2.last[order(gr2.last$doy,gr2.last$year),]
gr2.last<-gr2.last[!duplicated(gr2.last$year, fromLast=TRUE),]
gr2.last$last2<-gr2.last$Date
gr.last<-full_join(gr.last, gr1.last)
gr.last<-full_join(gr.last, gr2.last)

## Saint Hipp ##
sh.last<-sh%>%filter(frz>0)
sh.last<-sh.last[order(sh.last$doy,sh.last$year),]
sh.last<-sh.last[!duplicated(sh.last$year, fromLast=TRUE),]
sh.last$last<-sh.last$Date


