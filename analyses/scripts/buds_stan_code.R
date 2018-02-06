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
library(dplyr)
library(brms)
library(ggstance)

# Setting working directory. Add in your own path in an if statement for your file structure
setwd("~/Documents/git/freezingexperiment/analyses/")
source('scripts/savestan.R')

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())


########################
#### get the data
bb<-read.csv("output/birches_clean.csv", header=TRUE)
bb<-read.csv("output/fakedata_exp.csv", header=TRUE)
bb<-read.csv("output/buds_traits.csv", header=TRUE)

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

cl<-bb%>%dplyr::select(ID, species, individ, tx, chlorophyll)
cl<-cl[!is.na(cl$chlorophyll),]
cl$chloro<-ave(cl$chlorophyll, cl$individ)
cl$tx<-ifelse(cl$tx=="A", 0, 1)
mod<-stan_glmer(chloro~tx+species+(1|individ), data=cl)
mod1<-stan_glmer(chloro~tx+species+tx:species+(1|individ), data=cl)
plot(mod1, pars="beta") + xlab(expression(Delta*" in Chlorophyll Content"~(nmol/cm^2))) + ylab("Parameter Effects")

sla<-bb%>%dplyr::select(species, individ, tx, sla)
sla<-sla[!duplicated(sla),]
sla$tx<-ifelse(sla$tx=="A", 0, 1)
mod2<-stan_glm(sla~tx+species+ tx:species, data=sla)
mod3<-stan_glmer(sla~tx+species+tx:species+(1|individ), data=sla)

fit.brm<-brm(dvr~tx+(1|sp)+(tx-1|sp), data=dvr.stan)

m<-fit.brm
m.int<-posterior_interval(m)
sum.m<-summary(m)
cri.f<-as.data.frame(sum.m$fixed[,c("Estimate", "l-95% CI", "u-95% CI")])
cri.f<-cri.f[-1,] #removing the intercept 
fdf1<-as.data.frame(rbind(as.vector(cri.f[,1]), as.vector(cri.f[,2]), as.vector(cri.f[,3])))
fdf2<-cbind(fdf1, c(0, 0, 0) , c("Estimate", "2.5%", "95%"))
names(fdf2)<-c(rownames(cri.f), "sp", "perc")

cri.r<-(ranef(m, summary = TRUE, robust = FALSE,
              probs = c(0.025, 0.975)))$sp
cri.r2<-cri.r[, ,-1]
cri.r2<-cri.r2[,-2,]
dims<-dim(cri.r2)
twoDimMat <- matrix(cri.r2, prod(dims[1:2]), dims[3])
mat2<-cbind(twoDimMat, c(rep(1:2, length.out=6)), rep(c("Estimate", "2.5%", "95%"), each=2))
df<-as.data.frame(mat2)
names(df)<-c(rownames(cri.f), "sp", "perc")
dftot<-rbind(fdf2, df)
dflong<- tidyr::gather(dftot, var, value, tx, factor_key=TRUE)

#adding the coef estiamtes to the random effect values 
for (i in seq(from=1,to=nrow(dflong), by=9)) {
  for (j in seq(from=3, to=8, by=1)) {
    dflong$value[i+j]<- as.numeric(dflong$value[i+j]) + as.numeric(dflong$value[i])
  }
}
dflong$rndm<-ifelse(dftot$ind>0, 2, 1)
dfwide<-tidyr::spread(dflong, perc, value)
dfwide[,4:6] <- as.data.frame(lapply(c(dfwide[,4:6]), as.numeric ))
dfwide$ind<-as.factor(dfwide$ind)
## plotting

pd <- position_dodgev(height = -0.5)


fig1 <-ggplot(dfwide, aes(x=Estimate, y=var, color=factor(ind), size=factor(rndm), alpha=factor(rndm)))+
  geom_point(position =pd, size=4)+
  geom_errorbarh(aes(xmin=(`2.5%`), xmax=(`95%`)), position=pd, size=.5, height =0)+
  geom_vline(xintercept=0)+
  scale_colour_manual(labels = expression("Fixed effects",italic("B. papyrifera"), italic("B. populifolia")),
                      values=c("blue", "red", "orangered1"))+
  scale_shape_manual(labels="", values=c("1"=16,"2"=16))+
  scale_alpha_manual(values=c(1, 0.5))+
  guides(alpha=FALSE) + 
  scale_y_discrete(limits = rev(unique(sort(dfwide$var)))) + ylab("") + 
  labs(col="Effects") + theme(legend.text=element_text(size=10))
fig1


fit1<-stan_glmer(dvr~tx+sp+tx:sp+(1|ind), data=dvr.stan)
fit1
plot(fit1, pars="beta") + xlab(expression(Delta*" in Rate of Budburst (Days)")) + ylab("Parameter Effects")
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


