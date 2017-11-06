###Build fake data for Buds analysis - start with BETPOP individuals
## Cat - 28 September 2017 ##

# Clear workspace
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
graphics.off()

# Load libraries
library(lme4)

# Setting working directory. Add in your own path in an if statement for your file structure
setwd("~/Documents/git/freezingexperiment/analyses/")
source('stan/savestan.R')

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())


########################
#### get the data

# make sure this is the correct file (we're still cleaning as I write this!) 
bb <- read.csv("output/birches_buddata.csv", header=TRUE)

## Visualize Actual Data and pull population means from range
bb$dvr<-as.numeric(bb$dvr)
hist(bb$dvr,xlab = "Duration of Vegetative Risk", main = "Actual DVRs" )
mean(bb$dvr,na.rm = TRUE) #10.65094
sd(bb$dvr,na.rm = TRUE) # 7.191461
table(bb$individ)
bpap<-subset(bb, bb$species=="BETPAP")
hist(bpap$dvr)
mean(bpap$dvr, na.rm =TRUE) #10.40449
sd(bpap$dvr, na.rm=TRUE) # 9.570693
bpop<-subset(bb, bb$species=="BETPOP")
mean(bpop$dvr, na.rm=TRUE) #10.82927
sd(bpop$dvr, na.rm=TRUE) #4.800012

nrep <- 22
nind <- 7; ind_sd<-0.1
nsp <- 2; sp_sd<-0.3
ntx <- 2; tx_sd<-1


## Generate Random Y Response Data that follows studies nesting structure
rep_means <- rnorm(nrep, mean = 11, sd = 3)
ind_means <- lapply(rep_means, function(x) rnorm(nind, mean = x, sd = ind_sd))
sp_means <- lapply(ind_means, function(x) rnorm(nsp, mean = x, sd = sp_sd) )
tx_means <- lapply(unlist(sp_means), function(x) rnorm(ntx, mean = x, sd = tx_sd))


## Put Together X Predictor Matrix
ntot <-  nsp * ntx * nind * nrep
x_mat <- data.frame(buds = rep(1:(nrep), each = ntot/nrep),
                    species = rep(1:(nsp), each = nrep*nind*nsp),
                    tx =   rep( 1:(ntx), each=nsp*nind),
                    ind =  rep(1:(nind), each=nrep))

## Add in response for full fake dataset
x_mat <- x_mat[order(x_mat$species, x_mat$tx, x_mat$ind),]
#x_mat$dvr<-rnorm(n_pops, mean = 11, sd = 5)
x_mat$dvr <- unlist(tx_means)
fake_data <- x_mat

# now fix the levels to 0/1 (not 1/2) as R does
fake_data$tx <- as.numeric(fake_data$tx)
fake_data$tx[fake_data$tx==1] <- 0
fake_data$tx[fake_data$tx==2] <- 1


mean(fake_data$dvr)
sd(fake_data$dvr)

save(list=c("fake_data"), file = "FakeBuds_apply.RData")
#write.csv(fake_data, file="~/Documents/git/freezingexperiment/analyses/output/FakeBuds_apply.csv", row.names = FALSE)
