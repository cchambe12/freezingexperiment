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
source('scripts/savestan.R')

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())


########################
#### get the data
bb<-read.csv("output/birches_clean.csv", header=TRUE)
bb<-read.csv("output/fakedata_exp.csv", header=TRUE)

## make a bunch of things numeric 
bb$tx<-ifelse(bb$tx=="A", 0, 1)
bb$sp <- as.numeric(as.factor(bb$sp))
bb$dvr <- as.numeric(bb$dvr)
bb$ind<-substr(bb$individ, 9,10)


## subsetting data, preparing genus variable, removing NAs
dvr.prepdata <- subset(bb, select=c("dvr", "tx", "ind", "sp")) # removed "sp" when doing just one species
dvr.stan <- dvr.prepdata[complete.cases(dvr.prepdata),]

dvr.stan$ind <- as.numeric(as.factor(dvr.stan$ind))

dvr = dvr.stan$dvr
tx = dvr.stan$tx
ind = dvr.stan$ind
sp = dvr.stan$sp
N = length(dvr)
n_ind = length(unique(dvr.stan$ind))
n_sp = length(unique(dvr.stan$sp))


# making a list out of the processed data. It will be input for the model
datalist.td <- list(dvr=dvr,tx=tx,sp=sp, ind=ind,N=N,n_ind=n_ind, n_sp=n_sp) # removed sp=sp and n_sp=s_sp for one species



##############################
###### real data rstanarm first

fit1<-stan_glmer(dvr~tx+sp+(1|ind), data=dvr.stan)
fit1
plot(fit1, pars="beta")
pp_check(fit1)

### Another posterior predictive check
yrep <- posterior_predict(fit1)
all.equal(ncol(yrep), nobs(fit1)) # TRUE
nd <- data.frame(dvr = mean(dvr.stan$dvr), tx, sp, ind)
ytilde <- posterior_predict(fit1, newdata = nd)
all.equal(ncol(ytilde), nrow(nd)) # TRUE

#### Now using rstan model
# Had divergent transitions and the number would vary each time, I increased the warmup and now there are 4
# divergent transitions
dvr.td4 = stan('scripts/buds_sp_pred_poola.stan', data = datalist.td,
               iter = 8000,warmup=6000,control=list(adapt_delta=0.99), chains=4) 
betas <- as.matrix(dvr.td4, pars = c("mu_b_tx", "mu_b_sp"))
mcmc_intervals(betas[,1:2])


posterior<-extract(dvr.td4, 'y_hat')
y_pred <- as.matrix(unlist(y_pred, use.names=FALSE))
color_scheme_set("brightblue")
#pp<-mcmc_trace(posterior, pars=c("mu_b_tx", "mu_b_sp"), n_warmup=6000, facet_args = list(nrow = 2,
                                                                                #labeller=label_parsed))
#pp+facet_text(size = 15)
mcmc_areas(posterior,
           pars = c("mu_b_tx", "mu_b_sp"),
           prob = 0.8) 

ppc_intervals(
  y = dvr.stan$dvr,
  yrep = posterior_predict(fit1),
  x = dvr.stan$tx,
  prob = 0.5
) +
  panel_bg(fill="gray95", color=NA) +
  grid_lines(color="white") +
  labs(x = "Treatment", y = "Duration of Vegetative Risk")

#launch_shinystan(dvr.td4) # use for posterior predictive checks

td4 <- summary(dvr.td4)$summary # yhats around 1! double yay!
preds.4<-td4[grep("yhat", rownames(td4)),]

#save(td4, file="output/Buds_individLevel.Rda")
#save(dvr.td4, file="~/Documents/git/freezingexperiment/analyses/output/buds_2level_real.Rda")


