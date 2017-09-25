### WORKING ON BUDS DATA
## 25 SEPTEMBER 2017 - CAT


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
d <-read.csv("input/Buds_Sheet.csv", header=TRUE, check.names=FALSE)

################ Cleaning data ##################################
# remove individuals that were frosted twice
error.inds<-d%>%dplyr::select(NEW,Freeze)
error.inds<-error.inds[!duplicated(error.inds),]
#error.inds$Freeze<-ifelse(error.inds$Freeze=="", NA, error.inds$Freeze)
#error.inds<-na.omit(error.inds)
error.inds<-as.data.frame(table(error.inds$NEW))
error.inds<-filter(error.inds, Freq!=3)
keep<-unique(error.inds$Var1)

## Clean dataframe for analysis
dx<-gather(d, key=doy, value=bbch, -NEW, -TX, -Freeze, -Bud)
dx<-filter(dx, NEW %in% keep)
dx$day<-substr(dx$doy, 1, 2)
dx$month<-substr(dx$doy, 4, 5)
dx$year<-substr(dx$doy, 7,10)
x<-paste(dx$year, dx$day, dx$month)
dx$date<-as.Date(strptime(x, format="%Y %d %m"))
dx$doy<-yday(dx$date)

### DOY should now be adjusted for start of experiment rather than actual calendar doy
# Start of Experiment was: 24 March 2017
start<-yday("2017/03/24")
dx$doy.adjusted<-dx$doy-start
dx$bud <- ave(dx$Bud, dx$NEW, dx$date, FUN = seq_along)


#### Determine Percent Budburst - relationship with treatment? ##################
buds<-subset(dx, bbch==15)
buds<-dplyr::select(buds, NEW, bud, doy.adjusted)
buds$id<-paste(buds$NEW, buds$bud)
buds<-aggregate(doy.adjusted ~ id, data = buds, min)
buds$individ<-substr(buds$id, 1, 10)
buds<-distinct(buds, id,individ)
burst<-as.data.frame(table(buds$individ))
burst<-burst%>%rename(id=Var1)%>%rename(burst=Freq)
burst$id<-as.character(burst$id)

all<-dplyr::select(dx, NEW, TX, bud)
all$bud<-as.numeric(all$bud)
all<-aggregate(all$bud, by = list(all$NEW, all$TX), max)
all<-all%>%rename(id=Group.1)%>%rename(total=x) %>%rename(tx=Group.2)

perc<-full_join(burst, all)
perc$percent.bb<-perc$burst/perc$total
perc$species<-substr(perc$id, 1,6)

mod<-lm(percent.bb~tx + species, data=perc)
display(mod)

qplot(species, percent.bb, data = perc, 
      geom = "boxplot", color=tx) + 
  xlab("Species")+ylab("Percent Budburst")


############## Determine Duration of Vegetative Risk #################
##### Need to clean data a lot - remove early incorrectly entered data 
## and edit errors from when I was away
## then determine dead buds and if any reached budburst and then died
## If so to above, need to recalculate percent budburst











