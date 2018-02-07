## Diff plots for Buds exp - updated from springfreeze danf plots
# Cat - 7 February 2018

# Clear workspace
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
graphics.off()

# Load libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(egg)

# Set Working Directory
setwd("~/Documents/git/freezingexperiment/analyses/")
d<-read.csv("output/perc_clean.csv", header=TRUE)

d$tx<-d$TX
dx<- d %>%
  dplyr::select(individ, tx, leafout, budburst, species)

dx<-na.omit(dx)
dxx<-dx
dxx$risk<-dxx$leafout-dxx$budburst
dxx$dvr<-ave(dxx$risk, dxx$species, dxx$tx)
dxx$dvr.sd<-ave(dxx$risk, dxx$species, dxx$tx, FUN=sd)/sqrt(length(unique(dxx$dvr)))
dxx<-dplyr::select(dxx, species, tx, dvr, dvr.sd)
dxx<-dxx[!duplicated(dxx),]
dxx$tx.sd<-paste(dxx$tx, "se", sep="_")
dm<-dxx%>%dplyr::select(species, tx, dvr)%>%spread(tx, dvr)
ds<-dxx%>%dplyr::select(species, tx.sd, dvr.sd)%>%spread(tx.sd, dvr.sd)
dxx<-inner_join(dm, ds)
dxx$diff<-dxx$B-dxx$A
dxx$diff.sd<-sqrt((dxx$B_se)^2+(dxx$A_se)^2) 
dxx<-filter(dxx, species!="SAMRAC")

dxx$species<-ifelse(dxx$species=="BETPAP", "B. papyrifera", dxx$species)
dxx$species<-ifelse(dxx$species=="BETPOP", "B. populifolia", dxx$species)


#write.csv(dxx, file="~/Documents/git/springfreeze/output/diffplot.csv", row.names = FALSE)

diff<-ggplot(dxx, aes(x=factor(species), y=diff)) + geom_point() + 
  geom_linerange(aes(ymin=diff-diff.sd, ymax=diff+diff.sd), alpha=0.3) + 
  ylab(expression(atop("Model Estimate of Change ", paste("in Duration of Vegetative Risk (days)")))) +
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.title.x=element_blank(),
        axis.text.x = element_text(face = "italic", angle=45, hjust=1), axis.text=element_text(size=10)) +
  geom_hline(yintercept=0, alpha=0.3, linetype=2)
plot(diff)


change<-ggplot(dxx, aes(x=B, y=A)) + geom_point(aes(size=diff), shape=21) + 
  geom_linerange(aes(ymin=A-A_se, ymax=A+A_se), alpha=0.3) +
  geom_errorbarh(aes(xmax = B+B_se, xmin = B-B_se, height=0), alpha=0.3) +
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.key.size = unit(0.05, "cm"), legend.background = element_rect(linetype="solid",
                                                                             color="black"), 
        legend.position=c(0.90, 0.85), legend.text = element_text(size=7), 
        legend.title = element_text(size=8), legend.key=element_rect(fill="white")) +
  geom_text(aes(label=species, fontface="italic", vjust=2)) + xlab("Experiment Group \n Duration of Vegetative Risk (days)") + 
  ylab("Control Group \n Duration of Vegetative Risk (days)") + coord_cartesian(xlim=c(4,18), ylim=c(4,18)) +
  scale_size_continuous(name=expression(atop(Delta*" in Duration of",paste("Vegetive Risk"))))
plot(change)

ggarrange(diff, change, ncol=2)





