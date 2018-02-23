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
library(egg)

# Setting working directory. Add in your own path in an if statement for your file structure
setwd("~/Documents/git/freezingexperiment/analyses/")
source('scripts/savestan.R')

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())


########################
#### get the data
bb<-read.csv("output/birches_clean.csv", header=TRUE)
bb<-read.csv("output/fakedata_exp.csv", header=TRUE)
cc<-read.csv("output/buds_traits.csv", header=TRUE)

## make a bunch of things numeric 
bb$tx<-ifelse(bb$tx=="A", 0, 1)
bb$sp <- as.numeric(as.factor(bb$sp))
bb$dvr <- as.numeric(bb$dvr)
bb$ind<-as.numeric(as.factor(bb$individ))
bb$frost<-as.numeric(bb$frost)


## subsetting data, preparing genus variable, removing NAs
dvr.prepdata <- subset(bb, select=c("dvr", "tx", "ind", "sp")) # removed "sp" when doing just one species
dvr.stan <- dvr.prepdata[complete.cases(dvr.prepdata),]

#dvr.stan$ind <- as.numeric(as.factor(dvr.stan$ind))

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

cl<-cc%>%dplyr::select(ID, species, individ, tx, chlorophyll)
cl<-cl[!is.na(cl$chlorophyll),]
cl$chloro<-ave(cl$chlorophyll, cl$individ)
cl$tx<-as.numeric(ifelse(cl$tx=="A", 0, 1))
cl$sp<-as.numeric(as.factor(cl$species))
cl$ind<-as.numeric(as.factor(cl$individ))
cl.prepdata <- subset(cl, select=c("chloro", "tx", "ind", "sp")) # removed "sp" when doing just one species
cl.stan <- cl.prepdata[complete.cases(cl.prepdata),]
cl.stan<-cl.stan[!duplicated(cl.stan),]
cl.brm<-brm(chloro~tx+sp+tx:sp+(1|ind)+(tx-1|ind)+(sp-1|ind)+(tx:sp-1|ind), data=cl.stan)

mc<-cl.brm
m.intc<-posterior_interval(mc)
sum.mc<-summary(mc)
cri.fc<-as.data.frame(sum.mc$fixed[,c("Estimate", "l-95% CI", "u-95% CI")])
cri.fc<-cri.fc[-1,] #removing the intercept 
fdf1c<-as.data.frame(rbind(as.vector(cri.fc[,1]), as.vector(cri.fc[,2]), as.vector(cri.fc[,3])))
fdf2c<-cbind(fdf1c, c(0, 0, 0) , c("Estimate", "2.5%", "95%"))
names(fdf2c)<-c(rownames(cri.fc), "ind", "perc")

cri.rc<-(ranef(mc, summary = TRUE, robust = FALSE,
              probs = c(0.025, 0.975)))$ind
cri.r2c<-cri.rc[, ,-1]
cri.r2c<-cri.r2c[,-2,]
dimsc<-dim(cri.r2c)
twoDimMatc <- matrix(cri.r2c, prod(dimsc[1:2]), dimsc[3])
mat2c<-cbind(twoDimMatc, c(rep(1:29, length.out=87)), rep(c("Estimate", "2.5%", "95%"), each=29))
dfc<-as.data.frame(mat2c)
names(dfc)<-c(rownames(cri.fc), "ind", "perc")
dftotc<-rbind(fdf2c, dfc)
dflongc<- tidyr::gather(dftotc, var, value, tx:`tx:sp`, factor_key=TRUE)

#adding the coef estiamtes to the random effect values 
for (i in seq(from=1,to=nrow(dflongc), by=90)) {
  for (j in seq(from=3, to=89, by=1)) {
    dflongc$value[i+j]<- as.numeric(dflongc$value[i+j]) + as.numeric(dflongc$value[i])
  }
}
dflongc$rndm<-ifelse(dftotc$ind>0, 2, 1)
dfwidec<-tidyr::spread(dflongc, perc, value)
dfwidec[,4:6] <- as.data.frame(lapply(c(dfwidec[,4:6]), as.numeric ))
dfwidec$ind<-as.factor(dfwidec$ind)
## plotting

pd <- position_dodgev(height = -0.5)
bpap<-c(1:14)
bpop<-c(15:29)
dfwidec$col<-NA
dfwidec$col<-ifelse(dfwidec$ind==0, "blue",dfwidec$col)
dfwidec$col<-ifelse(dfwidec$ind%in%bpap, "firebrick4",dfwidec$col)
dfwidec$col<-ifelse(dfwidec$ind%in%bpop, "lightseagreen",dfwidec$col)


estimates<-c("Treatment x Species", "Species", "Treatment")
dfwidec$legend<-factor(dfwidec$col,
                      labels=c("Overall Effects","B. papyrifera", "B. populifolia"))

fig1c <-ggplot(dfwidec, aes(x=Estimate, y=var, color=col, size=factor(rndm), alpha=factor(rndm)))+
  geom_point(position =pd)+
  geom_errorbarh(aes(xmin=(`2.5%`), xmax=(`95%`)), position=pd, size=.5, width=0)+
  geom_vline(xintercept=0)+
  scale_color_manual(values=c("blue","firebrick4","seagreen"),
                     labels=c("Overall Effects","B. papyrifera", "B. populifolia"))+
  scale_size_manual(values=c(3, 2, 2, 2, 2, 2, 2, 2, 2, 2)) +
  scale_shape_manual(labels="", values=c("1"=16,"2"=16))+
  scale_alpha_manual(values=c(1, 0.3)) +
  guides(size=FALSE, alpha=FALSE) + #removes the legend 
  ggtitle(label = "A.")+ 
  scale_y_discrete(limits = rev(unique(sort(dfwide$var))), labels=estimates) + ylab("") + 
  labs(col="Effects") + theme(legend.title = element_blank(), legend.text = element_text(size=8),
                              legend.key.size = unit(0.25,"cm"), legend.position = c(.85,.50),
                              legend.background = element_rect()) +
  xlab(expression(atop("Model Estimate of Change ", paste("in Chlorophyll Content" ~(nmol/cm^2)))))
fig1c


sla<-cc%>%dplyr::select(species, individ, tx, sla)
sla<-sla[!duplicated(sla),]
sla$tx<-as.numeric(ifelse(sla$tx=="A", 0, 1))
sla$sp<-as.numeric(as.factor(sla$species))
sla$sla<-as.numeric(sla$sla)
sla$ind<-as.numeric(as.factor(sla$individ))

sla.prepdata <- subset(sla, select=c("sla", "tx", "ind", "sp")) # removed "sp" when doing just one species
sla.stan <- sla.prepdata[complete.cases(sla.prepdata),]
sla.stan<-sla.stan[!duplicated(sla.stan),]
sla.brm<-brm(sla~tx+sp+tx:sp+(1|ind)+(tx-1|ind)+(sp-1|ind)+(tx:sp-1|ind), data=sla.stan)

ms<-sla.brm
m.ints<-posterior_interval(ms)
sum.ms<-summary(ms)
cri.fs<-as.data.frame(sum.ms$fixed[,c("Estimate", "l-95% CI", "u-95% CI")])
cri.fs<-cri.fs[-1,] #removing the intercept 
fdf1s<-as.data.frame(rbind(as.vector(cri.fs[,1]), as.vector(cri.fs[,2]), as.vector(cri.fs[,3])))
fdf2s<-cbind(fdf1s, c(0, 0, 0) , c("Estimate", "2.5%", "95%"))
names(fdf2s)<-c(rownames(cri.fs), "ind", "perc")

cri.rs<-(ranef(ms, summary = TRUE, robust = FALSE,
               probs = c(0.025, 0.975)))$ind
cri.r2s<-cri.rs[, ,-1]
cri.r2s<-cri.r2s[,-2,]
dimss<-dim(cri.r2s)
twoDimMats <- matrix(cri.r2s, prod(dimss[1:2]), dimss[3])
mat2s<-cbind(twoDimMats, c(rep(1:29, length.out=87)), rep(c("Estimate", "2.5%", "95%"), each=29))
dfs<-as.data.frame(mat2s)
names(dfs)<-c(rownames(cri.fs), "ind", "perc")
dftots<-rbind(fdf2s, dfs)
dflongs<- tidyr::gather(dftots, var, value, tx:`tx:sp`, factor_key=TRUE)

#adding the coef estiamtes to the random effect values 
for (i in seq(from=1,to=nrow(dflongs), by=90)) {
  for (j in seq(from=3, to=89, by=1)) {
    dflongs$value[i+j]<- as.numeric(dflongs$value[i+j]) + as.numeric(dflongs$value[i])
  }
}
dflongs$rndm<-ifelse(dftots$ind>0, 2, 1)
dfwides<-tidyr::spread(dflongs, perc, value)
dfwides[,4:6] <- as.data.frame(lapply(c(dfwides[,4:6]), as.numeric ))
dfwides$ind<-as.factor(dfwides$ind)
## plotting

pd <- position_dodgev(height = -0.5)
dfwides$col<-NA
dfwides$col<-ifelse(dfwides$ind==0, "blue",dfwides$col)
dfwides$col<-ifelse(dfwides$ind%in%bpap, "firebrick4",dfwides$col)
dfwides$col<-ifelse(dfwides$ind%in%bpop, "lightseagreen",dfwides$col)


estimates<-c("Treatment x Species", "Species", "Treatment")
dfwides$legend<-factor(dfwides$col,
                       labels=c("Overall Effects","B. papyrifera", "B. populifolia"))
fig1s <-ggplot(dfwides, aes(x=Estimate, y=var, color=col, size=factor(rndm), alpha=factor(rndm)))+
  geom_point(position =pd)+
  geom_errorbarh(aes(xmin=(`2.5%`), xmax=(`95%`)), position=pd, size=.5, width=0)+
  geom_vline(xintercept=0)+
  scale_color_manual(values=c("blue","firebrick4","seagreen"),
                     labels=c("Overall Effects","B. papyrifera", "B. populifolia"))+
  scale_size_manual(values=c(3, 2, 2, 2, 2, 2, 2, 2, 2, 2)) +
  scale_shape_manual(labels="", values=c("1"=16,"2"=16))+
  scale_alpha_manual(values=c(1, 0.3)) +
  guides(size=FALSE, alpha=FALSE) + #removes the legend 
  ggtitle(label = "B.")+ 
  scale_y_discrete(limits = rev(unique(sort(dfwide$var))), labels=estimates) + ylab("") + 
  labs(col="Effects") + theme(legend.title = element_blank(), legend.text = element_text(size=8),
                              legend.key.size = unit(0.25,"cm"), legend.position = "none",
                              legend.background = element_rect()) +
  xlab(expression(atop("Model Estimate of Change in SLA", paste("(leaf area (cm^2)/leaf mass (g))"))))
fig1s
quartz()
ggarrange(fig1c, fig1s, ncol=2)

fit.brm<-brm(dvr~tx+sp+tx:sp+(1|ind)+(tx-1|ind)+(sp-1|ind)+(tx:sp-1|ind), data=dvr.stan)

m<-fit.brm
m.int<-posterior_interval(m)
sum.m<-summary(m)
cri.f<-as.data.frame(sum.m$fixed[,c("Estimate", "l-95% CI", "u-95% CI")])
cri.f<-cri.f[-1,] #removing the intercept 
fdf1<-as.data.frame(rbind(as.vector(cri.f[,1]), as.vector(cri.f[,2]), as.vector(cri.f[,3])))
fdf2<-cbind(fdf1, c(0, 0, 0) , c("Estimate", "2.5%", "95%"))
names(fdf2)<-c(rownames(cri.f), "ind", "perc")

cri.r<-(ranef(m, summary = TRUE, robust = FALSE,
              probs = c(0.025, 0.975)))$ind
cri.r2<-cri.r[, ,-1]
cri.r2<-cri.r2[,-2,]
dims<-dim(cri.r2)
twoDimMat <- matrix(cri.r2, prod(dims[1:2]), dims[3])
mat2<-cbind(twoDimMat, c(rep(1:29, length.out=87)), rep(c("Estimate", "2.5%", "95%"), each=29))
df<-as.data.frame(mat2)
names(df)<-c(rownames(cri.f), "ind", "perc")
dftot<-rbind(fdf2, df)
dflong<- tidyr::gather(dftot, var, value, tx:`tx:sp`, factor_key=TRUE)

#adding the coef estiamtes to the random effect values 
for (i in seq(from=1,to=nrow(dflong), by=90)) {
  for (j in seq(from=3, to=89, by=1)) {
    dflong$value[i+j]<- as.numeric(dflong$value[i+j]) + as.numeric(dflong$value[i])
  }
}
dflong$rndm<-ifelse(dftot$ind>0, 2, 1)
dfwide<-tidyr::spread(dflong, perc, value)
dfwide[,4:6] <- as.data.frame(lapply(c(dfwide[,4:6]), as.numeric ))
dfwide$ind<-as.factor(dfwide$ind)
## plotting

pd <- position_dodgev(height = -0.5)
#dfwide$ind<-as.numeric(as.factor(dfwide$ind))
bpap<-c(1:14)
bpop<-c(15:29)
dfwide$col<-NA
dfwide$col<-ifelse(dfwide$ind==0, "blue",dfwide$col)
dfwide$col<-ifelse(dfwide$ind%in%bpap, "firebrick4",dfwide$col)
dfwide$col<-ifelse(dfwide$ind%in%bpop, "lightseagreen",dfwide$col)


fig1 <-ggplot(dfwide, aes(x=Estimate, y=var, color=col, size=factor(rndm), alpha=factor(rndm)))+
  geom_point(position =pd, size=4)+
  geom_errorbarh(aes(xmin=(`2.5%`), xmax=(`95%`)), position=pd, size=.5, height =0)+
  geom_vline(xintercept=0)+
  scale_shape_manual(labels="", values=c("1"=16,"2"=16))+
  scale_alpha_manual(values=c(1, 0.5))+
  guides(alpha=FALSE) + 
  scale_y_discrete(limits = rev(unique(sort(dfwide$var)))) + ylab("") + 
  labs(col="Effects") + theme(legend.text=element_text(size=10))
fig1

estimates<-c("Treatment x Species", "Species", "Treatment")
dfwide$legend<-factor(dfwide$col,
                      labels=c("Overall Effects","B. papyrifera", "B. populifolia"))

fig1 <-ggplot(dfwide, aes(x=Estimate, y=var, color=col, size=factor(rndm), alpha=factor(rndm)))+
  geom_point(position=pd)+
  geom_errorbarh(aes(xmin=(`2.5%`), xmax=(`95%`)), position=pd, size=.5, width=0)+
  geom_vline(xintercept=0)+
  scale_color_manual(values=c("blue","firebrick4","seagreen"),
                      labels=c("Overall Effects","B. papyrifera", "B. populifolia"))+
  scale_size_manual(values=c(3, 2, 2, 2, 2, 2, 2, 2, 2, 2)) +
  scale_shape_manual(labels="", values=c("1"=16,"2"=16))+
  scale_alpha_manual(values=c(1, 0.3)) +
  guides(size=FALSE, alpha=FALSE) + #removes the legend 
  ggtitle(label = "A.")+ 
  scale_y_discrete(limits = rev(unique(sort(dfwide$var))), labels=estimates) + ylab("") + 
  labs(col="Effects") + theme(legend.title = element_blank(), legend.text = element_text(size=8),
                              legend.key.size = unit(0.25,"cm"), legend.position = c(.85,.20),
                              legend.background = element_rect()) +
  xlab(expression(atop("Model Estimate of Change", paste("in Duration of Vegetative Risk (days)"))))
fig1


ggarrange(fig1, diff, ncol=2)


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


