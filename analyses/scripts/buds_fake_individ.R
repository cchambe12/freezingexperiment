###Build fake data for Buds analysis - start with BETPOP individuals
## Cat - 28 September 2017 ##

# Clear workspace
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
graphics.off()

# Load libraries
library(lme4)

nind = 15 # number of individuals

ntot = 25 # numbers of obs per individuals 

#  with individuals
baseinter <- 12 # baseline intercept (DVR) across all individuals
ind_int <- baseinter + c(1:nind)-mean(1:nind) # different intercepts by individuals

# now start building ...
testdat <- vector()

# assumptions:
# (a) predictors are centered
# (b) predictors are not correlated
# (c) no interactions, linear effects of tx + bud number only
# (d) each obs is a different set of treatments

# and some important points ...
# (z) the below draws treatments from distribution in such a way that there is a lot more variation than we have


for(i in 1:nind){ # loop over species. i = 1
  
  # continuous predictors, generate level (if you will) for each observation
  tx = rnorm(ntot, 0, 1)
  bud = rnorm(ntot, 0, 8)
  
  # set up effect sizes
  txcoef = 3 # steep slope for freeze
  budcoef = -1 # less steep for bud number
  
  # SD for each treatment
  txcoef.sd = 1
  budcoef.sd = 0.5
  
  # build model matrix 
  mm <- model.matrix(~tx+bud, data.frame(tx, bud))
  
  # coefficients need to match the order of the colums in the model matrix (mm)
  # so here, that's intercept, chill, force, photo
  coeff <- c(ind_int[i], 
             rnorm(1, txcoef, txcoef.sd),
             rnorm(1, budcoef, budcoef.sd)
  )
  
  bb <- rnorm(n = ntot, mean = mm %*% coeff, sd = 0.1)
  
  testdatx <- data.frame(bb, ind = i, 
                         tx, bud)
  
  testdat <- rbind(testdat, testdatx)  
}

setwd("~/Documents/git/freezingexperiment/analyses")
source('scripts/savestan.R')

lme1 <- lmer(bb ~ tx+bud+ (1|ind), data = testdat)
summary(lme1)
ranef(lme1)
fixef(lme1)
#head(testdat)
#head(list.coeffs)

##
# try the model
datalist.td <- with(testdat, 
                    list(y = bb, 
                         tx = as.numeric(tx), 
                         bud = as.numeric(bud),
                         ind = as.numeric(ind),
                         N = nrow(testdat),
                         n_ind = length(unique(ind))
                    )
)



## running model with fake data, centered no interactions
library(rstan)
library(shinystan)
osp.td2.fake = stan("scripts/Buds_individLevel.stan", data = datalist.td, 
                    iter = 2000,warmup=1500,control=list(adapt_delta=0.90)) 
launch_shinystan(osp.td2.fake)

### try with interactions
datalist.td2 <- with(testdat3, 
                     list(y = bb, 
                          chill = as.numeric(chill), 
                          force = as.numeric(force), 
                          photo = as.numeric(photo),
                          lat= as.numeric(lat),
                          sp = as.numeric(sp),
                          N = nrow(testdat3),
                          n_sp = length(unique(sp))
                     )
)

###### fake data- uncentered. interactions
osp.td4.fake = stan('stan/lat/LAT_daysBBwinter_2level.stan', data = datalist.td2,
                    iter = 4000,warmup=3000,control=list(adapt_delta=0.99)) 
