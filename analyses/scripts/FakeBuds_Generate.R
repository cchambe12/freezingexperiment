## Stan model with experiment data and duration of vegetative risk
# Based on script by Dan Flynn and Lizzie from Buds repo 

# Basic housekeeping
rm(list=ls()) 
options(stringsAsFactors=FALSE)

# Fake data for buburst stan work #
library(dplyr)

setwd("~/Documents/git/freezingexperiment/analyses/scripts")
bb <- read.csv("..//output/birches_buddata.csv", header=TRUE)

# <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <>
# Set up: same as experiment, with two sites, 28 species, two levels each of warming and photoperiod, and three levels of chilling. 2016-04-01 adding interactions. This ends up generating expected differences, but variation in effect sizes across species is minimal currently.
# <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <>

nsp = 2

ntx = 2
#nbud = 22
nind = 7
rep = 22 # within each combination of treatments. 

(ntot = nsp*ntx*rep*nind) # 792 rows; 22k rows across species

# Build up the data frame
sp = gl(nsp, rep, length=ntot)
ind<-gl(rep, rep*nsp, length=ntot)
tx = gl(ntx, rep*nsp*nind, length = ntot)
#bud = gl(nbud, rep*nsp*ntx, length = ntot)


(d <- data.frame(sp, tx, ind)) # critical coding error here!

###### Set up differences for each level
spdiff = 2
inddiff = 0
txdiff = 3
#buddiff = -1

######## SD for each treatment
spdiff.sd = 1
#buddiff.sd = 1
txdiff.sd = 1 
inddiff.sd = 0.5

#mm <- model.matrix(~(tx+ind), data.frame(tx, ind))
#mm <- mm[,-grep("tx", colnames(mm))]
#coeff <- c(txdiff, inddiff)


risk <- rnorm(n = length(bud), mean = 11, sd = 3) # should be able to do sd = mm %*% sd.coeff as well, with a different sd for each parameter.

(fake <- data_frame(risk, tx, bud))

summary(lm(risk ~ (tx+bud)^2, data = fake)) # sanity check 

##### Again, now with species now.

baseinter = 11 # baseline intercept across all individuals for DVR
spint <- baseinter + c(1:nind)-mean(1:nind) # different intercepts by species

fake <- vector()

for(i in 1:nind){ # loop over species, as these are the random effect modeled
  
  # Give species different difference values, drawn from normal. Could make dataframe of diffs and diff.sds, and use apply..
  
  coeff <- c(spint[i], 
             rnorm(1, spdiff, spdiff.sd),
             rnorm(1, txdiff, txdiff.sd),
             rnorm(1, inddiff, inddiff.sd)
  )
  
  risk <- rnorm(n = length(tx), mean = mm %*% coeff, sd = 3)
  
  fakex <- data.frame(risk, sp, tx, ind)
      
  fake <- rbind(fake, fakex)  
  }

summary(lm(risk ~ (tx+sp)^2, data = fake)) # sanity check 

# now fix the levels to 0/1 (not 1/2) as R does
fake$tx <- as.numeric(fake$tx)
fake$tx[fake$tx==1] <- 0
fake$tx[fake$tx==2] <- 1

summary(lm(risk ~ (tx+bud)^2, data = fake)) # double sanity check 

summary(lmer(risk ~ (tx|sp) + (bud|sp), data = fake)) # too hard for lmer.

save(list=c("fake"), file = "Fake Buds.RData")
#write.csv(fake, file="~/Documents/git/springfreeze/output/fakedata_exp.csv", row.names = FALSE)

mean(fake$risk)
sd(fake$risk)
hist(fake$risk)
length(fake$sp[fake$sp==1])
