## 8 February 2017 - Cat
## Formulating a more functional datasheet for experiment

library(dplyr)
library(tidyr)

# Clear workspace
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
graphics.off()

# Set Working directory
setwd("~/Documents/git/freezingexperiment/analyses/input")
d<-read.csv("TX.csv", header=TRUE)

# Start sorting data
d$species<-substr(d$OLD, 0,6)
d$site<-substr(d$OLD, 10, 11)
d$individ<-substr(d$NEW, 9, 10)

# Determining distribution of individuals
tx<-as.data.frame(table(d$TX))
site<-as.data.frame(table(d$site))
species<-as.data.frame(table(d$species))

# Make a new dataframe
df<-as.data.frame(table(d$species,d$TX, d$site))%>%
  rename(species=Var1)%>%
  rename(tx=Var2)%>%
  rename(site=Var3)
sub<- apply(df, 1, function(row) all(row !=0 ))
df<-df[sub,]

write.csv(df, "~/Documents/git/freezingexperiment/analyses/output/species.tx.site.csv", row.names=FALSE)
