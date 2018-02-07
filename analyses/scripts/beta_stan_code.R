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
library(betareg)

# Setting working directory. Add in your own path in an if statement for your file structure
setwd("~/Documents/git/freezingexperiment/analyses/")
source('scripts/savestan.R')

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())


########################
#### get the data
bperc<-read.csv("output/percentBB_betula.csv", header=TRUE)
bb<-read.csv("output/perc_clean.csv", header=TRUE)
bb<-read.csv("output/fakebeta.csv", header=TRUE)

## make a bunch of things numeric 
bb<-filter(bb, species!="SAMRAC")
bb$tx<-ifelse(bb$TX=="A", 0, 1)
bb$sp <- as.numeric(as.factor(bb$sp))
bb$sp[bb$sp==1] <- 0
bb$sp[bb$sp==2] <- 1
bb$perc <- as.numeric(bb$perc)
bb$perc<-bb$perc*100
bb$ind<-as.numeric(as.factor(substr(bb$individ, 9,10)))


## subsetting data, preparing genus variable, removing NAs
pp.prepdata <- subset(bb, select=c("perc", "tx", "sp", "ind")) # removed "sp" when doing just one species
pp.stan <- pp.prepdata[complete.cases(pp.prepdata),]
pp.stan<-pp.stan[!duplicated(pp.stan),]


perc = pp.stan$perc
tx = pp.stan$tx
sp = pp.stan$sp
N = length(perc)


# making a list out of the processed data. It will be input for the model
datalist.td <- list(perc=perc,tx=tx,sp=sp,N=N) # removed sp=sp and n_sp=s_sp for one species



##############################
###### real data rstanarm first

perc.brm<-brm(perc~tx+sp+tx:sp+(1|ind)+(tx-1|ind)+(sp-1|ind)+(tx:sp-1|ind), data=pp.stan)
fit1
plot(fit1, pars=c("beta"))
pp_check(fit1)
prior_summary(fit1)

m1<-perc.brm
m.int1<-posterior_interval(m1)
sum.m1<-summary(m1)
cri.f1<-as.data.frame(sum.m1$fixed[,c("Estimate", "l-95% CI", "u-95% CI")])
cri.f1<-cri.f1[-1,] #removing the intercept 
fdf11<-as.data.frame(rbind(as.vector(cri.f1[,1]), as.vector(cri.f1[,2]), as.vector(cri.f1[,3])))
fdf21<-cbind(fdf11, c(0, 0, 0) , c("Estimate", "2.5%", "95%"))
names(fdf21)<-c(rownames(cri.f1), "ind", "perc")

cri.r1<-(ranef(m, summary = TRUE, robust = FALSE,
              probs = c(0.025, 0.975)))$ind
cri.r21<-cri.r1[, ,-1]
cri.r21<-cri.r21[,-2,]
dims1<-dim(cri.r21)
twoDimMat1 <- matrix(cri.r21, prod(dims1[1:2]), dims1[3])
mat21<-cbind(twoDimMat1, c(rep(1:15, length.out=45)), rep(c("Estimate", "2.5%", "95%"), each=15))
df1<-as.data.frame(mat21)
names(df1)<-c(rownames(cri.f1), "ind", "perc")
dftot1<-rbind(fdf21, df1)
dflong1<- tidyr::gather(dftot1, var, value, tx:`tx:sp`, factor_key=TRUE)

#adding the coef estiamtes to the random effect values 
for (i in seq(from=1,to=nrow(dflong1), by=48)) {
  for (j in seq(from=3, to=47, by=1)) {
    dflong1$value[i+j]<- as.numeric(dflong1$value[i+j]) + as.numeric(dflong1$value[i])
  }
}
dflong1$rndm<-ifelse(dftot1$ind>0, 2, 1)
dfwide1<-tidyr::spread(dflong1, perc, value)
dfwide1[,4:6] <- as.data.frame(lapply(c(dfwide1[,4:6]), as.numeric ))
dfwide1$ind<-as.factor(dfwide1$ind)
## plotting

pd <- position_dodgev(height = -0.5)

dfwide1$legend<-factor(dfwide1$ind,
                      labels=c("Overall Effects","1","2","3","4","5","6","7","8","9", "10","11","12","13","14","15"))

fig11 <-ggplot(dfwide1, aes(x=Estimate, y=var, color=legend, size=factor(rndm), alpha=factor(rndm)))+
  geom_point(position =pd)+
  geom_errorbarh(aes(xmin=(`2.5%`), xmax=(`95%`)), position=pd, size=.5, height =0, width=0)+
  geom_vline(xintercept=0)+
  scale_colour_manual(values=c("blue","darkred", "firebrick3","indianred","orangered3", "orangered1","orange3", 
                               "sienna4","sienna2", "green4", "green3","lightseagreen", "purple2","lightslateblue",
                               "mediumorchid2", "magenta3"),
                      breaks=c("Overall Effects"))+
  scale_size_manual(values=c(3, 2, 2, 2, 2, 2, 2, 2, 2, 2)) +
  scale_shape_manual(labels="", values=c("1"=16,"2"=16))+
  scale_alpha_manual(values=c(1, 0.3)) +
  guides(size=FALSE, alpha=FALSE) + #removes the legend 
  ggtitle(label = "B.")+ 
  scale_y_discrete(limits = rev(unique(sort(dfwide$var))), labels=estimates) + ylab("") + 
  labs(col="Effects") + theme(legend.position = "none", legend.box.background = element_rect(), 
                              legend.title=element_blank(), legend.key.size = unit(0.05, "cm")) +
  xlab("Estimate (percent)")
fig11


### Another posterior predictive check
yrep <- posterior_predict(fit1)
all.equal(ncol(yrep), nobs(fit1)) # TRUE
nd <- data.frame(perc = mean(pp.stan$perc), tx, sp)
ytilde <- posterior_predict(fit1, newdata = nd)
all.equal(ncol(ytilde), nrow(nd)) # TRUE

#### Now using rstan model
# Had divergent transitions and the number would vary each time, I increased the warmup and now there are 4
# divergent transitions
pp.td4 = stan('scripts/perc_sp_pred_beta.stan', data = datalist.td,
               iter = 2000,warmup=1500,control=list(adapt_delta=0.99)) 
betas <- as.matrix(pp.td4, pars = c("mu_tx", "mu_sp"))
mcmc_intervals(betas)


posterior<-extract(pp.td4)
y_pred <- as.matrix(unlist(posterior, use.names=FALSE))
color_scheme_set("brightblue")
#pp<-mcmc_trace(posterior, pars=c("mu_b_tx", "mu_b_sp"), n_warmup=6000, facet_args = list(nrow = 2,
                                                                                #labeller=label_parsed))
#pp+facet_text(size = 15)
mcmc_areas(posterior,
           pars = c("mu_tx", "mu_sp"),
           prob = 0.8) 

ppc_intervals(
  y = pp.stan$perc,
  yrep = posterior_predict(fit1),
  x = pp.stan$tx,
  prob = 0.5
) +
  panel_bg(fill="gray95", color=NA) +
  grid_lines(color="white") +
  labs(x = "Treatment", y = "Percent Budburst")

ppc_intervals(
  y = pp.stan$perc,
  yrep = posterior_predict(fit1),
  x = pp.stan$sp,
  prob = 0.5
) +
  panel_bg(fill="gray95", color=NA) +
  grid_lines(color="white") +
  labs(x = "Species", y = "Percent Budburst")

launch_shinystan(pp.td4) # use for posterior predictive checks

td4 <- summary(pp.td4)$summary # yhats around 1! double yay!
preds.4<-td4[grep("yhat", rownames(td4)),]

#save(td4, file="output/Buds_individLevel.Rda")
#save(dvr.td4, file="~/Documents/git/freezingexperiment/analyses/output/buds_2level_real.Rda")


