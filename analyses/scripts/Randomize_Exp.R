# False Spring Experiment Script - CC
# 16 January 2017
# Randomize Individuals in experiment to sort into control, drought or freeze treatments
### Super simple version. Randomize. 

library(dplyr)
library(tidyr)
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# Set own wd as needed
setwd("~/Documents/git/freezingexperiment/analyses/input")

d<-read.csv("Exp.Individs.csv", sep=",", header=TRUE)

d$Species<- substr(d$ID, 1, 6)
d$Site <- substr(d$ID, 10, 11)
d$Number <- substr(d$New.ID, 9, 10)

acepen<- d %>%
  dplyr::select(ID, New.ID, Species, Site,Number) %>%
  filter(Species == "ACEPEN") %>%
  transform(ID = sample(ID))

alninc<- d %>%
  dplyr::select(ID,New.ID, Species, Site,Number) %>%
  filter(Species == "ALNINC") %>%
  transform(ID = sample(ID))

betall<- d %>%
  dplyr::select(ID,New.ID, Species, Site,Number) %>%
  filter(Species == "BETALL") %>%
  transform(ID = sample(ID))

betpop<- d %>%
  dplyr::select(ID,New.ID, Species, Site,Number) %>%
  filter(Species == "BETPOP") %>%
  transform(ID = sample(ID))

corcor<- d %>%
  dplyr::select(ID,New.ID, Species, Site,Number) %>%
  filter(Species == "CORCOR") %>%
  transform(ID = sample(ID))

prupen<- d %>%
  dplyr::select(ID,New.ID, Species, Site,Number) %>%
  filter(Species == "PRUPEN") %>%
  transform(ID = sample(ID))

vibcas<- d %>%
  dplyr::select(ID,New.ID, Species, Site,Number) %>%
  filter(Species == "VIBCAS") %>%
  transform(ID = sample(ID))

df2<- rbind(acepen,alninc,betall,betpop,corcor,prupen,vibcas)
df2<- df2 %>%
  unite(New, Species, Number)
df2$site<- substr(df2$ID, 10, 11)
df3<- df2 %>%
  dplyr::select(-Site) 
df3$Tag<-unite(df3, New, site)
df3<-df3%>%
  dplyr::select(ID,New,site)

write.csv(df3,"~/Documents/git/freezingexperiment/analyses/output/new.tags.csv",row.names=FALSE, eol="\r\n")

