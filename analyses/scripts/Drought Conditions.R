## Cat - 27 March 2017
# Look at soil moisture for Freezing Experiment
# Aim: to see if drought was significant enough to include in analysis

# Clear workspace
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
graphics.off()

# Load libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(arm)
library(data.table)
# Set Working Directory
setwd("~/Documents/git/freezingexperiment/analyses")
d<-read.csv("input/soil.moisture.csv",header=TRUE)

# clean up dataframe
soil <- d %>%
  dplyr::select(ID, percent, TX)
soil<-na.omit(soil)
soil$mean<-ave(soil$percent, soil$ID)
moisture<- soil %>%
  dplyr::select(-percent)
moisture<-moisture[!duplicated(moisture), ]
moisture<-arrange(moisture, ID)
  
tx<- moisture %>%
  dplyr::select(-ID)%>%
  group_by(TX)%>%
  summarise_each(funs(mean))

mod<-lm(percent~TX, data=soil)
display(mod)
                 
#write.csv(tx, "output/Drought.csv", row.names= FALSE)
