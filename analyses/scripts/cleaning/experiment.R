# 3 NOVEMBER 2016 - CC
## Organizing Experimental data/individuals
## Renaming and grouping individuals by treatment

library(dplyr)
library(tidyr)
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# Set Working Directory
setwd("~/Documents/git/freezingexperiment/analyses/input")
experiment<-read.csv("Exp.Individs.csv",header=TRUE)
attach<- experiment

# Get the site by getting the last two characters of the undercomp rownames
experiment <- experiment %>%
  separate(ID, c("Individual", "Site"), by="_")

experiment$sp <- substr(experiment$sp, 0, 6) 

species<-as.data.frame(table(experiment$Individual))
site<-as.data.frame(table(experiment$Site))

location<- as.data.frame(table(experiment$Location))
tx<-as.data.frame(table(experiment$TX))
