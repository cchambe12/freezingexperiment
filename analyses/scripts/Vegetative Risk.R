## 20 March 2017
# Duration of Vegetativ Risk: Experiment data
# Aim: To find if the timing is significant between control and tx groups

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
library(car)


# Set Working Directory
setwd("~/Documents/git/freezingexperiment/analyses")
d <-read.csv("input/bbch_data.csv", header=TRUE, check.names=FALSE)

# Organizing data
df<-gather(d, "date","bbch", -NEW, -TX)
df<-na.omit(df)
df$date<-as.character(as.Date(df$date,"%m/%d/%y"))
df$doy<-yday(df$date)
df$species<-substr(df$NEW, 0,6)
df<-dplyr::select(df, -date)

# Now start evaluating duration of vegetative risk
# 9 is using 9 as budburst, 10 is using 10. Some individuals, I missed 9 so it
# was sometime in between the observation dates
phases.9<-c("9","15")
df9<-filter(df, bbch %in% phases.9)
df9$bbch<- factor(df9$bbch, levels = c(9,15), 
                        labels = c("Budburst","Leaves"))
code<-unite_(df9, "species_TX", c("species","TX"))
df9<-full_join(df9, code)

risk9<- df9%>%
  group_by(NEW, bbch, species_TX)%>%
  arrange(NEW)%>%
  filter(row_number()==1) %>%
  spread(bbch, doy)
risk9<-na.omit(risk9)
risk9$Risk <- risk9$Leaves - risk9$Budburst

# Some graphs
ggplot(risk9, aes(x=species, y=Risk)) + geom_point(aes(col=as.factor(TX))) + 
  geom_smooth(aes(col=as.factor(TX)),method="loess", se=FALSE)

ggplot((risk9), aes(x=Budburst, y=NEW), stat="identity") + geom_point(aes(x= risk9$Budburst)) + 
  geom_segment(aes(y = NEW, yend = NEW, x = Budburst, xend = Leaves, col=TX)) +
  geom_point(aes(x=Leaves, col=TX)) + geom_point(aes(col=TX)) +
  xlab("Budburst to Leaf Out") +
  ylab("Species")

# Means and Standard deviations
risk<-risk9%>%
  ungroup(NEW, Risk)%>%
  dplyr::select(species_TX, Risk)
risk.mean<- risk%>%
  group_by(species_TX) %>% 
  summarise_each(funs(mean))%>%
  rename(mean=Risk)
risk.sd<-risk%>%
  group_by(species_TX) %>% 
  summarise_each(funs(sd)) %>%
  rename(sd=Risk)
risk.count<-as.data.frame(table(risk$species_TX)) %>%
  rename(species_TX=Var1)%>%
  rename(count=Freq)
risk.species<-full_join(risk.mean, risk.sd)
risk.species.tx<-full_join(risk.species,risk.count)

tx.count<-as.data.frame(table(risk9$TX))%>%
  rename(TX=Var1)%>%
  rename(count=Freq)
tx<-risk9%>%
  ungroup(NEW, Risk)%>%
  dplyr::select(TX, Risk)
tx.mean<- tx%>%
  group_by(TX) %>% 
  summarise_each(funs(mean))%>%
  rename(mean=Risk)
tx.sd<-tx%>%
  group_by(TX) %>% 
  summarise_each(funs(sd)) %>%
  rename(sd=Risk)
tx.species<-full_join(tx.mean, tx.sd)
tx.species.count<-full_join(tx.species,tx.count)

#write.csv(tx.species.count, "output/tx_count.csv", row.names = FALSE)

mod<-lm(Risk~TX, data=risk9)
lmer<-lmer(Risk~TX + (1|species),data=risk9)
display(mod);display(lmer)

ggplot((risk9), aes(x=Budburst, y=Risk)) + xlab("Budburst") + ylab("Duration of Vegetative Risk") +
  geom_point(aes(col=as.factor(TX))) + 
  geom_smooth(aes(col=as.factor(TX)),method="lm", se=FALSE) 

qplot(species, Risk, data = risk9, 
      geom = "boxplot", color=TX) + 
  xlab("Species")+ylab("Duration of Vegetative Risk")

# Anova
model<-lm(Risk~TX+Budburst+species,data=risk9)
Anova(model)
# ALNINC
alninc<-risk9 %>% filter(species=="ALNINC")
aln.mod<-lm(Risk~TX,data=alninc)
Anova(aln.mod)

# BETPOP
betpop<-risk9 %>% filter(species=="BETPOP")
bet.mod<-lm(Risk~TX,data=betpop)
Anova(bet.mod)

# PRUPEN
prupen<-risk9 %>% filter(species=="PRUPEN")
pru.mod<-lm(Risk~TX,data=prupen)
Anova(pru.mod)

# VIBCAS
vibcas<-risk9 %>% filter(species=="VIBCAS")
vib.mod<-lm(Risk~TX,data=vibcas)
Anova(vib.mod)

#write.csv(risk.species.tx, "output/exp.species.count.csv", row.names = FALSE)
