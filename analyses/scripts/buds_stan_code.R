## Started 27 September 2017 ##
## By Cat ##

## Try to run REAL data ##
## With Stan! ##

############################################
## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# dostan = TRUE

library(rstan)
#install.packages("rstan", repos = "https://cloud.r-project.org/", dependencies=TRUE)
library(ggplot2)
library(shinystan)
library(bayesplot)
library(rstanarm)

# Setting working directory. Add in your own path in an if statement for your file structure
setwd("~/Documents/git/freezingexperiment/analyses/")
source('stan/savestan.R')

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())


########################
#### get the data

# make sure this is the correct file (we're still cleaning as I write this!) 
bb <- read.csv("output/birches_buddata.csv", header=TRUE)
bb <- subset(bb, bb$species=="BETPOP")

## make a bunch of things numeric 
bb$tx <- as.numeric(bb$frost)
bb$bud <- as.numeric(bb$bud)
bb$resp <- as.numeric(bb$dvr)


## subsetting data, preparing genus variable, removing NAs
ospr.prepdata <- subset(bb, select=c("resp", "tx", "bud", "individ"))
dim(subset(bb, is.na(tx)==FALSE & is.na(bud)==FALSE))
ospr.stan <- ospr.prepdata[complete.cases(ospr.prepdata),]


# Fairly strict rules of inclusion in this analysis:

## remove NAs individually .... (not needed currently)
#ospr.stan$bud<-ospr.stan[which(is.na(ospr.stan$bud)==FALSE),]
#ospr.stan$tx<-ospr.stan[which(is.na(ospr.stan$tx)==FALSE),]
ospr.stan$individ <- as.numeric(as.factor(ospr.stan$individ))


y = ospr.stan$resp
tx = ospr.stan$tx
ind = ospr.stan$individ
bud = ospr.stan$bud
N = length(y)
n_ind = length(unique(ospr.stan$individ))


# making a list out of the processed data. It will be input for the model
datalist.td <- list(y=y,tx=tx, bud=bud,ind=ind,N=N,n_ind=n_ind)



##############################
###### real data all chilling
osp.td4 = stan('scripts/Buds_individLevel.stan', data = datalist.td,
               iter = 2000,warmup=1500,control=list(adapt_delta=0.99)) 

betas <- as.matrix(osp.td4, pars = c("mu_b_tx_ind","mu_b_bud_ind",
"b_tx", "b_bud"))
mcmc_intervals(betas[,1:4])

launch_shinystan(osp.td4)
load("/Users/CatherineChamberlain/Downloads/shinystan-multiparam-gg.RData")
shinystan_multiparam_gg


td4 <- summary(osp.td4)$summary
preds.4<-td4[grep("yhat", rownames(td4)),]

save(td4, file="output/Buds_individLevel.Rda")



######################################
###### real data all chilling sigmoid
osp.td5 = stan('stan/bb/M1_daysBBnointer_2level_interceptonly_sigmoid.stan', data = datalist.td,
               iter = 2000,warmup=1500,control=list(adapt_delta=0.95)) 

betas.td5 <- as.matrix(osp.td5, pars = c("b_force", "b_photo","a_chill", "b_chill"))
mcmc_intervals(betas[,1:5])

#td5 <- summary(osp.td5)$summary
#preds.5<-td5[grep("yhat", rownames(td5)),]



########### Running the models with fake data
#setwd("~/Documents/git/projects/treegarden/budreview/ospree/analyses/bb_analysis")
source("bb_analysis/bb_testdata_generate.R")

# lme version
summary(lme1 <- lmer(bb ~ chill+force+photo + (1|sp), data = testdat)) 
ranef(lme1)
fixef(lme1)
#head(testdat)
#head(list.coeffs)

##
# try the model
datalist.td <- with(testdat, 
                    list(y = bb, 
                         chill = as.numeric(chill), 
                         force = as.numeric(force), 
                         photo = as.numeric(photo),
                         sp = as.numeric(sp),
                         N = nrow(testdat),
                         n_sp = length(unique(sp))
                    )
)



## running model with fake data
osp.td2 = stan('stan/bb/M1_daysBBnointer_2level.stan', data = datalist.td, 
             iter = 2000,warmup=1500,control=list(adapt_delta=0.90)) 

