#### 18 December 2017 - Cat
## A script to make figures for quals thesis proposal
# Hoping to make a similar set of figures that I used for Danf's portion
# of the Rethinking manuscript

# Clear workspace
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
graphics.off()

# Load libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(rstanarm)

# Set Working Directory
setwd("~/Documents/git/freezingexperiment/analyses")
d <-read.csv("output/budsdvr_all.csv", header=TRUE, check.names=FALSE)

d<-d[!is.na(d$dvr),]
d<-dplyr::select(d, species, individ, budburst, leafout, bud, tx, dvr)
d$risk<-ave(d$dvr, d$species, d$tx)
d$sd<-ave(d$dvr, d$species, d$tx, FUN=sd)
d$tx_sd<-paste(d$tx, "se", sep="_")
dx<-filter(d, species!="SAMRAC")

df<-d%>%dplyr::select(species, tx, risk, sd, tx_sd)
df<-df[!duplicated(df),]
dr<-df%>%dplyr::select(species, tx, risk)%>%spread(tx, risk)
ds<-df%>%dplyr::select(species, tx_sd, sd)%>%spread(tx_sd, sd)

df<-inner_join(dr, ds)
df$diff<- df$B-df$A
df$diff.sd<-sqrt((df$A_se)^2+(df$B_se)^2) 
df<-filter(df, species!="SAMRAC")

df$species<-ifelse(df$species=="BETPOP", "B. populifolia", df$species)
df$species<-ifelse(df$species=="BETPAP", "B. papyrifera", df$species)

df$code<-reorder(df$species, df$A)

diff<-ggplot(df, aes(x=factor(code), y=diff)) + geom_point() + 
  geom_linerange(aes(ymin=diff-diff.sd, ymax=diff+diff.sd), alpha=0.3) + 
  ylab(expression(Delta*" in DVR between treatments")) + coord_cartesian(ylim=c(-10,20)) +
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.title.x=element_blank(),
        axis.text.x = element_text(face = "italic", angle=45, vjust=0.5), axis.text=element_text(size=10))
plot(diff)

species<- ggplot(df, aes(x=B, y=A)) + geom_point(aes(size=diff), shape=21) + 
  geom_linerange(aes(ymin=A-A_se, ymax=A+A_se), alpha=0.3) +
  geom_errorbarh(aes(xmax = B+B_se, xmin = B-B_se, height = 0), alpha=0.3) +
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  geom_text(aes(label=species), vjust=2, fontface="italic") + xlab("DVR with weak treatment effects") + 
  ylab("DVR with strong treatment effects") + 
  scale_size_continuous(name=expression(Delta*" in DVR")) 
plot(species)


mod<-stan_glmer(dvr~tx+species+species:tx + (1|individ), data=dx)
mod
plot(mod, pars="beta")


traits<-read.csv("output/buds_traits.csv", header = TRUE)

ch<-traits%>%dplyr::select(species, individ, tx, dvr, chlorophyll)
ch<-ch[!is.na(ch$chlorophyll),]

mod2<-stan_glmer(chlorophyll~species + tx + dvr + species:tx + (1|individ), data=ch)
mod2
plot(mod2, pars="beta")

sl<-traits%>%dplyr::select(species, individ, tx, dvr, sla)
sl<-sl[!is.na(sl$sla),]
sl<-sl[!duplicated(sl),]
sl$risk<-ave(sl$dvr, sl$individ)
sl<-sl%>%dplyr::select(species, individ, tx, sla, risk)
sl<-sl[!duplicated(sl),]
sl$c.sla<-scale(sl$sla, center=TRUE, scale=FALSE)

mod2<-stan_glmer(sla~species + tx + risk + species:tx + (1|individ), data=sl)
mod2
plot(mod2, pars="beta")
