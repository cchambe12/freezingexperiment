## checking soil moisture meter measurements

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
library(car)

# Set Working Directory
setwd("~/Documents/git/freezingexperiment/analyses")
d<-read.csv("input/soil.moisture.csv",header=TRUE)

mod<-lm(reading~percent, data=d) 
summary(mod) # significant
Anova(mod) # significant


small<-dplyr::select(d, reading, percent)
small<-na.omit(small)
pear<-cor(small, method="pearson") # 0.6332

plot(reading~percent, data=small)
abline(mod, col="red")
