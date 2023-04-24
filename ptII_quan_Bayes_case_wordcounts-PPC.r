# file:
# ptII_quan_Bayes_case_wordcounts-PPC.r

# location:
# chap. 6 [6.7.4.6]
# Forshungsbeispiel — Wortproduktion Humor

# load necessary libraries
library(lme4)
library(lattice)
library(bayesplot)
library(BEST)

# library(brms)
# library(ggplot2)

# load necessary helper functions
# from DBDA (Kruschke)
source("DBDA2E-utilities.R")
source("Jags-Ymet-XmetMulti-Mrobust.R")

source("ptall_generalfuncs.r")
source("ptII_quan_Bayes_case_wordcounts-PPC_helpfuncs.r")

	
# get data
diss <- read.csv("LG_school-words-raw.tab", header=TRUE, sep="\t")
str(diss)
head(diss)
tail(diss)
namen <- names(diss)
with(diss, cor(W.noSC,uW.noSC))
with(diss, cor(W.wSC,uW.wSC))
with(diss, xtabs(age ~ schooltype +sex))

par(mfrow=c(2,3))
with(diss, plot(W.noSC,uW.noSC, bty="n", col="violetred3", lty=1, lwd=1, type="p", pre.plot=grid()))
with(diss, lines(lowess(uW.noSC ~ W.noSC), col="blue", lwd=2, lty=2))
with(diss, plot(W.noSC,age, bty="n", col="violetred3", lty=1, lwd=1, type="p", pre.plot=grid()))
with(diss, boxplot(W.noSC ~ sex * schooltype, notch=TRUE, col="violetred3"))
with(diss, interaction.plot(schooltype, sex, W.noSC, bty="n", pre.plot=grid(), col=c("darkred","blue")))
with(diss, interaction.plot(sex, schooltype, W.noSC, bty="n", pre.plot=grid(), col=c("darkred","blue")))

pairs(diss[,c(2,3,10,11)], col="violetred3")

diss$SS <- paste(diss$sex,diss$schooltype,sep="")
summary(lm(W.noSC ~ age + schooltype * sex, data=diss))
summary(glm(W.noSC ~ age + schooltype * sex, data=diss, family=poisson()))

# library 'lme4'
summary(glmer(W.noSC ~ age + schooltype + (age|sex), data=diss, family=poisson()))
summary(glmer(W.noSC ~ age + (1|schooltype) + (1|sex), data=diss, family=poisson()))

# library 'lattice'
bwplot(W.noSC ~ schooltype + sex |age, data=diss)
bwplot(W.noSC ~ SS | age, data=diss)
xyplot(W.noSC ~ age, data=diss)


# using scripts from KRUSCHKE DBDA

# prepare data
diss$W.noSC <- as.integer(diss$W.noSC)
diss$W.noSC.log <- log(diss$W.noSC)
diss$age.log <- log(diss$age)

diss$SS <- factor(diss$SS)
diss$sex <- factor(diss$sex)
diss$schooltype <- factor(diss$schooltype)

diss$SSn <- as.numeric(factor(diss$SS)) #as.numeric(factor(paste(diss$sex,diss$schooltype,sep="")))
diss$sexn <- as.numeric(factor(diss$sex))
diss$stypen <- as.numeric(factor(diss$schooltype))

diss$age.cat <- cut(as.numeric(diss$age), breaks=c(13,16,19,25), include.lowest=TRUE, right=TRUE, labels=c("(14-16]","(17-19]","(20-25]"))
diss$age.cat1 <- cut(as.numeric(diss$age), breaks=c(13,19,25), include.lowest=TRUE, right=TRUE, labels=c("(14-19]","(20-25]"))

str(diss)
head(diss)


# tables
table(diss$age)
with(diss, ftable(sex,stype,age))
with(diss, ftable(sex,schooltype,age))
with(diss, ftable(sex,schooltype))
with(diss, ftable(sex,age))
with(diss, ftable(schooltype,age))

bwplot(W.noSC.log ~ sex | stype, data=diss)
bwplot(W.noSC.log ~ stype | sex, data=diss)

xyplot(W.noSC ~ age, data=diss)
xyplot(W.noSC ~ jitter(age), data=diss)
xyplot(W.noSC.log ~ jitter(age), data=diss)
xyplot(W.noSC.log ~ jitter(age.log), data=diss)
xyplot(jitter(W.noSC.log) ~ jitter(age.log), data=diss)

with(diss, ftable(stype,sex,age))

apply(with(diss, ftable(stype,sex,age)),1,sum)

# interaction plots
colos <- colorRampPalette(c("red","green","orange","blue"))
par(mfrow=c(3,3))
with(diss, interaction.plot(stype, sex, W.noSC, bty="n", pre.plot=grid(), col=colos(2)))
with(diss, interaction.plot(sex, stype, W.noSC, bty="n", pre.plot=grid(), col=colos(3)))
with(diss, interaction.plot(stype, sex, W.noSC.log, bty="n", pre.plot=grid(), col=colos(2)))
with(diss, interaction.plot(sex, stype, W.noSC.log, bty="n", pre.plot=grid(), col=colos(3)))
with(diss, interaction.plot(stype, sex, W.noSC.log, bty="n", pre.plot=grid(), col=colos(3)))
with(diss, interaction.plot(age.cat, stype, W.noSC.log, bty="n", pre.plot=grid(), col=colos(3)))
with(diss, interaction.plot(age.cat, sex, W.noSC.log, bty="n", pre.plot=grid(), col=colos(2)))


# posterior predictive check with R after JAGS

# constants
fileNameRoot <- "Diss-"
numSavedSteps <- 1e4
thinSteps <- 50
numSavedSteps <- 1e3
nChains <- 3

# simple model

# response
yName1 <- "W.noSC.log"

# predictors
xName1 <- c("age.log","sexn","stypen")
diss1 <- diss[,c(yName1,xName1)]
naid <- which(is.na(diss1), arr.ind=TRUE)[,1]
diss.nona <- diss1[-naid,]
head(diss.nona)

# run model
mcmc.dm1 <- genMCMC( data=diss.nona, xName=xName1, yName=yName1, numSavedSteps=numSavedSteps, thinSteps=thinSteps, saveName=fileNameRoot, nChains=nChains)
str(mcmc.dm1)

# more complex model					 
diss.model <- cbind(W.noSC.log=diss.nona$W.noSC.log, with(diss, model.matrix(W.noSC.log ~ age.log + age.cat + stype + sex))[,-1] )
head(diss.model)
yName <- "W.noSC.log"
xName <- colnames(diss.model)[-1]
xName

# run model
mcmc.dm2 <- genMCMC(data=diss.model, xName=xName, yName=yName, numSavedSteps=numSavedSteps, thinSteps=thinSteps, saveName=fileNameRoot, nChains=nChains)
str(mcmc.dm2)


# analysis
# warning... creates a lot of plots
mcmc.diag.kruschke(model=mcmc.dm1, dats=diss.nona, xName=xName1, yName=yName1)
mcmc.diag.kruschke(model=mcmc.dm2, dats=diss.model, xName=xName, yName=yName)
mcmc.diag.kruschke(model=mcmc.dm2, dats=diss.model, xName=xName, yName=yName, PLOTmult=TRUE)
mcmc.diag.kruschke(model=mcmc.dm2, dats=diss.model, xName=xName, yName=yName, PLOThist=TRUE)
#
mcmc.diag.kruschke(model=mcmc.dm2, dats=diss.model, xName=xName1, yName=yName)

# extract infos
mcmc.red <- as.mcmc(lapply(mcmc.dm2, function(i) i[,c(1:8,17)]))
str(mcmc.red)
head(mcmc.red)
coda:::plot.mcmc.list(mcmc.dm2,ask=TRUE)
gelman.plot(mcmc.red)

# library(bayesplot)
color_scheme_set("pink")
mcmc_parcoord(mcmc.red, pars=colnames(mcmc.red[[1]]))
mcmc_parcoord(mcmc.red, pars=colnames(mcmc.red[[1]])[-c(9)])
# https://mc-stan.org/bayesplot/reference/MCMC-parcoord.html
color_scheme_set("brightblue")
# scale before
mcmc_parcoord(mcmc.red, pars=colnames(mcmc.red[[1]]), transform = function(x) {(x - mean(x)) / sd(x)})
mcmc_parcoord(mcmc.red, pars=colnames(mcmc.red[[1]])[-c(9)], transform = function(x) {(x - mean(x)) / sd(x)})
#
mcmc_parcoord(mcmc.red, pars=colnames(mcmc.red[[1]])[-c(9)], alpha=0.05)

# just sigma and nu
mcmc_parcoord(mcmc.red, pars=colnames(mcmc.red[[1]])[c(8,9)])
mcmc_pairs(mcmc.red)
color_scheme_set("darkgray")
# selected beta[1] vs. nu
mcmc_scatter(mcmc.red, pars=c("beta[1]","nu"), transform=list(nu="log"))
color_scheme_set("mix-brightblue-gray")
# traceplot MCMCs
mcmc_trace(mcmc.red)
# autocorrelation MCMCs
mcmc_acf(mcmc.red, lags=10)


# https://cran.r-project.org/web/packages/bayesplot/vignettes/visual-mcmc-diagnostics.html

# without sigma and nu
# https://arxiv.org/abs/1709.01449
# https://arxiv.org/pdf/1709.01449.pdf p.6
# Visualization in Bayesian workflow Jonah Gabry, Daniel Simpson, Aki Vehtari, Michael Betancourt, Andrew Gelman 



# not run  (outtake of full process)
# PPC posterior predictive distribution
dim(diss.model)
mcmc.mat.dm2 <- as.matrix(mcmc.dm2)
d.mcmc.mat.dm2 <- dim(mcmc.mat.dm2)
head(diss.model)
head(mcmc.mat.dm2[,grep("^beta",colnames(mcmc.mat.dm2))])

beta.nams <- grep("^beta",colnames(mcmc.mat.dm2),value=TRUE)
beta.id <- grep("^beta",colnames(mcmc.mat.dm2))
beta.nams
beta.id

# create a random sample (full cases!) from real values
# sample with replacement = bootstrap
seed <- 1432
set.seed(seed)
head(diss.model)
diss.model.d <- dim(diss.model)
diss.model.d #360 7 >>> N=360 persons (rows), k=7 variables (cols)
samp.ids <- sample(1:diss.model.d[1], replace=TRUE)
samp.mat <- diss.model[samp.ids,]
d.samp.mat <- dim(samp.mat)
d.samp.mat

# create y_pred values from posterior values for each of the sample elements
mat.ypred <- matrix(data=NA, ncol=d.samp.mat[1], nrow=d.mcmc.mat.dm2[1])
# cols=360 Persons
# rows=1002 mcmc (=3*334)
# ie. for each person investigated from the sample a full mcmc chain

# we use only the betas
attr(mcmc.mat.dm2,"dimnames")
dim(mat.ypred)
for(i in 1:d.samp.mat[1])
{
 mat.ypred[,i] <- mcmc.mat.dm2[,beta.id] %*% c(1,t(samp.mat[i,-c(1)]))
}

dim(mat.ypred)
head(mat.ypred)
tail(mat.ypred)
muPred <- apply(mat.ypred,1,mean)
mean(muPred)
sd(muPred)
head(muPred)
length(muPred)

hist(muPred, prob=TRUE)
# Y_rep is a function of mu_pred + sigma + noise-due-to-prediction-here-based-on-t-dist-and-nu(=df-of-t)
Y_rep <- muPred + mcmc.mat.dm2[,"sigma"] * rt(nrow(mcmc.mat.dm2),df=mcmc.mat.dm2[,"nu"])
# plot
hist(Y_rep, prob=TRUE, pre.plot=grid(), main="Y_rep")
lines(density(diss.model[,"W.noSC.log"]), col="darkred", lwd=2)
lines(density(Y_rep), lwd=2, col="blue")
legend("topright", c("empirical","Y_rep"), col=c("darkred","blue"), lwd=2, bty="n")

# create random sample from random draws from real values, but not full cases
anzbetas <- diss.model.d[2]-1 #-1 because one is the response
anzbetas
mat.ypred.d <- dim(mat.ypred)
mat.ypred.d
for(j in 1:mat.ypred.d[2])
{
 case.ids <- sample(1:dim(diss.model)[1], anzbetas, replace=TRUE)
 samp.mat[j,2:7] <- diag(diss.model[,-c(1)][case.ids,1:6]) #1:6? #not response
}

mat.ypred <- matrix(data=NA, ncol=d.samp.mat[1], nrow=d.mcmc.mat.dm2[1])
dim(mat.ypred)
dim(samp.mat)
for(i in 1:d.samp.mat[1])
{
 mat.ypred[,i] <- mcmc.mat.dm2[,beta.id] %*% c(1,t(samp.mat[i,2:7])) #not response
}
dim(mat.ypred)
muPred <- apply(mat.ypred,1,mean)
mean(muPred)
sd(muPred)
head(muPred)
length(muPred)
hist(muPred, prob=TRUE)

hist(Y_rep, prob=TRUE)
lines(density(diss.model[,"W.noSC.log"]), lwd=2)
lines(density(Y_rep), lwd=2)

# summary statistics
lapply(list(muPred=muPred,Y_rep=Y_rep), function(x) c(summary(x),sd=sd(x)))
# means
mean(muPred)
mean(Y_rep)

# does not make sense
mean(Y_rep == muPred) # = zero

# difference
mean(Y_rep-muPred)
# effect size
mean(Y_rep-muPred)/sd(Y_rep)

# Bayesian p-value
1-mean(Y_rep > muPred)                          # compare predict vs. predictive
1-mean(Y_rep > mean(diss.model[,"W.noSC.log"])) # compare predictive vs. empirical

# end of not run (outtake of full process)



# full process to perform a PPC based on full cases or category of cases (those sampled)
# Bootstrap approach

# http://doingbayesiandataanalysis.blogspot.com/2017/06/posterior-distribution-of-predictions.html

# convert to matrix and extract betas
# posterior
mcmc.mat.dm2 <- as.matrix(mcmc.dm2)
str(mcmc.mat.dm2)
summary(mcmc.mat.dm2)
d.mcmc.mat.dm2 <- dim(mcmc.mat.dm2)
colnames(mcmc.mat.dm2)

# betas
beta.nams <- grep("^beta",colnames(mcmc.mat.dm2),value=TRUE)
beta.id <- grep("^beta",colnames(mcmc.mat.dm2))
beta.nams
beta.id

set.seed(1823)
anzsim.reps <- 100
mat.ypred <- matrix(data=NA, ncol=dim(diss.model)[1], nrow=d.mcmc.mat.dm2[1])
Y_rep.mat <- matrix(data=NA, nrow=dim(mat.ypred)[1], ncol=anzsim.reps)
mat.ypred.d <- dim(mat.ypred)
Y_rep.mat.d <- dim(Y_rep.mat)
diss.model.d <- dim(diss.model)
mat.ypred.d
Y_rep.mat.d
diss.model.d
fullcase <- FALSE
fullcase <- TRUE
jitt <- FALSE
colnames(diss.model)
length(colnames(diss.model))
varsofinterest <- c(2,3,4,5,6,7) # c(1:6) #without response <<< cols of original table with data
varsofinterest
anzbetas <- diss.model.d[2]-1 #-1 because one is the response
anzbetas
# simulation
for(reps in 1:anzsim.reps)
{
  print(reps)
  if(fullcase)
  {
    # sample full cases
    samp.ids <- sample(1:dim(diss.model)[1], replace=TRUE)
    samp.mat <- diss.model[samp.ids,varsofinterest]
    d.samp.mat <- dim(samp.mat)
    d.samp.mat
  } else if(!fullcase)
  {

  # create random sample from random draws from real values, but not full cases 
  # not full cases, but categories ie. values
  for(j in 1:mat.ypred.d[2])
  {
    case.ids <- sample(1:diss.model.d[1], anzbetas, replace=TRUE)
    samp.mat[j,] <- diag(diss.model[case.ids,varsofinterest])
    # for(i in 1:length(case.ids)) samp.mat[j,i] <- diss.model[case.ids[i],i]
    } 
  } else stop("STOP - fullcase variable not defined")

  for(i in 1:d.samp.mat[1])
  {
    mat.ypred[,i] <- mcmc.mat.dm2[,beta.id] %*% c(1,t(samp.mat[i,]))
  }

  # mu_predict
  muPred <- apply(mat.ypred,1,mean)
  # Y=response_predict_rep
  Y_rep.mat[,reps] <- muPred + mcmc.mat.dm2[,"sigma"] * rt(nrow(mcmc.mat.dm2),df=mcmc.mat.dm2[,"nu"])
  # muPred.mat[,reps] <- apply(mat.ypred,1,mean)
}

# descriptive stats of Y_rep.mat
dim(Y_rep.mat)
# stats for each rep
Y_rep.mat.desc <- t(apply(Y_rep.mat,2, function(x) c(summary(x),SD=sd(x),VAR=var(x),fivenum2(x)[c(2,4)])))
# stats over all reps
Y_rep.mat.desc.sum <- t(apply(Y_rep.mat.desc,2, function(x) c(Min=min(x),Median=median(x),Mean=mean(x),Max=max(x))))
ratio <- Y_rep.mat.desc.sum[,"Max"]/Y_rep.mat.desc.sum[,"Min"]
Y_rep.mat.desc.sum <- data.frame(Y_rep.mat.desc.sum,"Ratio|Max/Min"=ratio, check.names=FALSE)
Y_rep.mat.desc.sum

# plot PPC
dens.orig.resp <- density(diss.model[,"W.noSC.log"])
fac <- 1.15
ylim <- c(0,max(density(Y_rep.mat[,1])$y)*fac)

par(oma=c(2,1,1,1), "cex.axis"=1, bty="l")
# hist(diss.model[,"W.noSC.log"], ylim=ylim, prob=TRUE, pre.plot=grid(), col="violetred3", border="white", main="", ylab="Density", xlab=expression(paste(Y^rep)), cex.lab=1.2)
hist(Y_rep.mat, ylim=ylim, prob=TRUE, pre.plot=grid(), col="orange", border="white", main="", ylab="Density", xlab=expression(paste(Y^rep)), cex.lab=1.2)
nlines <- 30
Y_rep.mat.d <- dim(Y_rep.mat)
chooses <- sample(Y_rep.mat.d[2], nlines, replace=FALSE)
apply(Y_rep.mat[,chooses],2,function(x) lines(density(x),col="violetred4"))
lines(dens.orig.resp, col="green", lwd=4)
mtext("Posterior Predictive Check (PPC)", outer=TRUE, line=-2, cex=1.5, side=3)

par(fig=c(0,1,0,1), oma=c(1,0,0,0), mar=c(0,0,0,0), new=TRUE)
plot(1, type="n", bty="n", xaxt="n", yaxt="n")
legend("bottom", legend=c("empirical","ppc"), lty=1, lwd=2, xpd=TRUE, horiz=TRUE, col=c("green","violetred4"), bty="n", cex=.9)

# end of full process



# https://m-clark.github.io/bayesian-basics/model-exploration.html#model-checking

summaryInfo <- smryMCMC2( mcmc.dm1 )
summaryInfo
anzsim.reps
varsofinterest
colnames(diss.model)[varsofinterest]
colnames(diss.model)[-varsofinterest]


# how to multiply betas and variables of interest
varsofinterest # not response (= 1)
str(diss.model)
model.pred <- mcmc.mat.dm2[,beta.id] %*% t( data.frame(1,diss.model[,varsofinterest]))
str(model.pred)
dim(model.pred)
MW <- apply(model.pred,1,mean)
SD <- apply(model.pred,1,sd)
head(MW)
length(MW)

# MW <- apply( mcmc.mat.dm2[,beta.id] %*% t( data.frame(1,head(diss.model[,-c(1)])) ), 1, mean)
# SD <- apply( mcmc.mat.dm2[,beta.id] %*% t( data.frame(1,head(diss.model[,-c(1)])) ), 1, sd)
# Y_rep.mat <- sapply(1:anzsim.reps, function(s) rnorm(n=dim(diss.nona)[1], mean=MW[i], sd=summaryInfo["sigma","Mean"]))

diss.model.d
anzsim.reps

seed <- 0987
set.seed(seed)

# Y_rep.mat.1 <- sapply(1:anzsim.reps, function(s) rnorm(n=diss.model.d[1], mean=MW[s], sd=SD[s]))

Y_rep.mat.1 <- sapply(1:anzsim.reps, function(s) rnorm(n=d.mcmc.mat.dm2[1], mean=MW[s], sd=SD[s]))
dim(Y_rep.mat.1)
Y_rep.1 <- apply(Y_rep.mat.1,1,mean) + mcmc.mat.dm2[,"sigma"] * rt(nrow(mcmc.mat.dm2),df=mcmc.mat.dm2[,"nu"])

# see above
# Y_rep.mat[,reps] <- muPred + mcmc.mat.dm2[,"sigma"] * rt(nrow(mcmc.mat.dm2),df=mcmc.mat.dm2[,"nu"])
# mat.ypred[,i] <- mcmc.mat.dm2[,beta.id] %*% c(1,t(samp.mat[i,]))# 1002 17  #360 6 -> 1002 360


1-mean(Y_rep.1 > mean(diss.model[,"W.noSC.log"])) 

# plot Y_rep
compval <- mean(diss.model[,"W.noSC.log"])
BEST:::plotPost(Y_rep.1, credMass=0.87, compVal=compval, ROPE=c(4.2,5.5), showMode=TRUE, xlab=expression(paste(Y^rep)))
# plot muPred = Y
abline(v=mean(Y_rep.1), col="orange", lwd=3)
abline(v=compval, col="green", lwd=3)


mean(Y_rep.1)
mean(diss.model[,"W.noSC.log"])
head(Y_rep.mat.1)
tail(Y_rep.mat.1)


# plot
# library(grDevices)
colo <- adjustcolor("blue", alpha.f=0.1)
fac <- 1.35
dxy <- density(Y_rep.mat.1[,i])
ylim <- c(0,max(dxy$y)*fac)
hist(Y_rep.mat.1, prob=TRUE, ylim=ylim, pre.plot=grid(), border="white", col="skyblue", xlab="Y_rep", breaks=25)
# Y_reps
for(i in 1:anzsim.reps)
{
 dxy <- density(Y_rep.mat.1[,i])
 if(jitt) dxy$y <- jitter(dxy$y) 
 lines(dxy$x, dxy$y, lwd=1, col=colo)
}
# empirical
dens.orig.resp # = density(diss.model[,"W.noSC.log"])
lines(dens.orig.resp, lwd=2, col="red")
legend("topright", c("empirical","Y_rep"), col=c("red",colo), lwd=2, bty="n")

# Bayesian p-value
1-mean(Y_rep.mat[,1] > mean(diss.model[,"W.noSC.log"]))
pvals <- apply(Y_rep.mat.1, 2, function(i) 1-mean(i > mean(diss.model[,"W.noSC.log"])))
pvals
median(pvals)
fivenum.wn(pvals)


# plot adjusted
fac <- 1.2
hist(Y_rep.mat.1, ylim=c(0,max(density(Y_rep.mat.1)$y)*fac), prob=TRUE, pre.plot=grid(), border="white", col="skyblue", xlab="Y_rep", breaks=30)
# Y_reps
dxymax <- 0
for(i in 1:anzsim.reps)
{
 dxy <- density(Y_rep.mat.1[,i])
 if(jitt) dxy$y <- jitter(dxy$y) 
 lines(dxy$x, dxy$y, lwd=1, col=colo)
 dxymax.temp <- max(dxy$y)
 if(dxymax < dxymax.temp) dxymax <- dxymax.temp
} 
# empirical
lines(dens.orig.resp, lwd=2, col="red")
# adjusted empirical
fac.dxy <- dxymax/max(dens.orig.resp$y)
fac.dxy
lines(dens.orig.resp$x, dens.orig.resp$y*fac.dxy, lwd=1, col="darkred", lty=2)


# muPred for single cases
# muPred <- mcmc.mat.dm2[,beta.id] %*% matrix(rep(1,length(beta.id)),ncol=1)

# selected case
head(diss.model)
dm.nams <- colnames(diss.model)[varsofinterest]
dm.nams
# 15 years, associated age category, stypeG=1, stypeR=0, sexw=1
mat.case1 <- matrix(data=c(1,log(15),1,0,1,0,1), ncol=1, dimnames=list(c("Intercept",dm.nams)))
t(mat.case1)
muPred.s <- mcmc.mat.dm2[,beta.id] %*% mat.case1
head(muPred.s)
tail(muPred.s)

# different case
mat.case1 <- matrix(data=c(1,log(17),1,0,0,1,0), ncol=1, dimnames=list(c("Intercept",dm.nams)))

# random sample n=1
muPred.s.rn <- mcmc.mat.dm2[,beta.id] %*% c(1,t(diss.model[sample(1:d.samp.mat[1],1),-c(1)]))


# calculate prediction based on predictors + sigma + rt noise
set.seed(1823)
Y_rep.s <- muPred.s.rn + mcmc.mat.dm2[,"sigma"] * rt(nrow(mcmc.mat.dm2),df=mcmc.mat.dm2[,"nu"])
length(Y_rep.s)
head(Y_rep.s)

mean(muPred.s)
mean(Y_rep.s)
mean(muPred.s.rn)

mean(Y_rep.s == muPred.s.rn) #zero
mean(Y_rep.s-muPred.s.rn)
mean(Y_rep.s-muPred.s.rn)/sd(Y_rep.s)

# Bayesian p-value
1-mean(Y_rep > muPred)                         # compare predict vs. predictive
1-mean(Y_rep > mean(diss.nona[,"W.noSC.log"])) # compare predictive vs. empirical

# plot Y_rep
compval # mean(diss.model[,"W.noSC.log"])
BEST:::plotPost(Y_rep.s, credMass=0.87, compVal=compval, ROPE=c(4.5,5.2), showMode=TRUE)
# plot muPred = Y
mean(Y_rep.s)
mean(muPred.s.rn)
mean(diss[,"W.noSC.log"])
abline(v=mean(Y_rep.s), col="orange", lwd=2)
abline(v=mean(muPred.s.rn), col="violetred3", lwd=2)
abline(v=mean(diss[,"W.noSC.log"]), col="darkred", lwd=2)

# summary
summary(as.mcmc(muPred.s.rn))
summary(as.mcmc(Y_rep.s))
# posterior predictive check
# calculate p-value
mean(Y_rep.s > muPred.s.rn)
mean(Y_rep.s > muPred.s.rn)/(1-mean(Y_rep.s > muPred))
mean(Y_rep.s - muPred.s.rn > 1)
mean(Y_rep.s - muPred.s.rn > 0.1)
mean(Y_rep.s - muPred.s.rn > 0.01)

par(mfrow=c(2,1))
plotPost(muPred.s.rn, credMass=0.87, showMode=TRUE, compVal=compval, ROPE=c(5.4,5.5), xlab=expression(paste(mu[pred])), ylab="Density")
plotPost(Y_rep.s, credMass=0.87, showMode=TRUE, compVal=compval, ROPE=c(4.9,5.5), xlab=expression(paste(Y^rep)), ylab="Density")



# exercise KRUSCHKE
# Kruschke Code
# Exercise 18.3 (p. 513) 
# http://doingbayesiandataanalysis.blogspot.com/2011/04/anova-with-non-homogeneous-variances.html
# Question: Is it statistically acceptable to run the non-homogeneous ANOVA on data that follow non-normal distributions?
# If so, how do you change the code to run such tests? Specifically, I am interested in analyzing data that are gamma distributed. 
# Answer: Yes, it's easy. In the code ANOVAonewayNonhomogvarJagSTZ.R, change this

for ( i in 1:Ntotal ) {
  y[i] ~ dnorm( mu[i] , tau[x[i]] )
  mu[i] <- a0 + a[x[i]]
}

# to this

for ( i in 1:Ntotal ) {
  y[i] ~ dgamma( sY[i] , rY[i] )
  sY[i] <- pow(mu[i],2) * tau[x[i]]
  rY[i] <- mu[i] * tau[x[i]]
  mu[i] <- max( a0 + a[x[i]] , 0.0001 )
}

# Also, do NOT standardize the data, because the data must be non-negative to come from a gamma distribution.
# Therefore, in the dataList, use the original data y, not standardized data. And, after JAGS/BUGS has generated an MCMC chain,
# do not convert back from standardized to original scale, because the data weren't standardized in the first place.

# https://people.ucsc.edu/~abrsvn/bayes_winbugs_jags_2.r

# If we re-run the model and monitor the residuals (as we did for the homosced. t-test), we can confirm heteroscedasticity:

cat("model{
  # Priors
  mu1~dnorm(0, 0.001)
  mu2~dnorm(0, 0.001)
  sigma1~dunif(0, 1000)
  sigma2~dunif(0, 1000)
  tau1 <- 1/(sigma1*sigma1)
  tau2 <- 1/(sigma2*sigma2) 
  # Likelihood
  for (i in 1:n1) {
      y1[i]~dnorm(mu1, tau1)
      residual1[i] <- y1[i]-mu1
  }
  for (i in 1:n2) {
      y2[i]~dnorm(mu2, tau2)
      residual2[i] <- y2[i]-mu2
  }
  # Derived quantities
  delta <- mu2-mu1
}", fill=TRUE, file="t_test3.txt")


# Data generation
set.seed(seed)

n1 <- 60 # Number of observations in group 1
n2 <- 40 # Number of observations in group 1
mu1 <- 105 # Population mean for group 1
mu2 <- 77.5 # Population mean for group 2
sigma1 <- 3 # Population SD for group 1
sigma2 <- 2.5 # Population SD for group 2

n <- n1+n2 # Total sample size
y1 <- rnorm(n1, mu1, sigma1) # Data for group 1
y2 <- rnorm(n2, mu2, sigma2) # Data for group 2

# data -- same as before
win.data <- list(y1=y1, y2=y2, n1=n1, n2=n2)

# inits -- same as before
inits <- function() {
    list(mu1=rnorm(1), mu2=rnorm(1), sigma1=rlnorm(1), sigma2=rlnorm(1))
}
 
params <- c("mu1", "mu2", "delta", "sigma1", "sigma2", "residual1", "residual2")
# MCMC settings -- same as before
nc <- 3
ni <- 2000
nb <- 500
nt <- 1


# run OpenBUGS

# win
# library("R2WinBUGS")
# pkg <- "package:R2WinBUGS"
# detach(pkg, character.only = TRUE)
# out <- bugs(data=win.data, inits=inits, parameters=params, model="t_test3.txt", n.thin=nt, n.chains=nc, n.burnin=nb, n.iter=ni, debug=TRUE, DIC=TRUE, clearWD=TRUE)

# linux
# requires openbugs as cmd tool
# version used: OpenBUGS version 3.2.3 rev 1012
library("R2OpenBUGS")
out <- bugs(data=win.data, inits=inits, parameters=params, model="t_test3.txt",
            n.thin=nt, n.chains=nc, n.burnin=nb, n.iter=ni,
            debug=FALSE, DIC=TRUE, clearWD=FALSE)


# Confirm heteroscedasticity:
par(mfrow=c(1, 2))
plot(c(out$mean$residual1, out$mean$residual2), col=c("black", "blue"), pch=c(1, 19), ylab="Residual RTs")
abline(h=0)                
faktor <- factor(c(rep("Group A",each=n1),rep("Group B",each=n2)))
plot(c(out$mean$residual1, out$mean$residual2)~faktor, col="lightblue", xlab="Groups", ylab="Residual RTs")


# Doing it with JAGS
library("R2jags")
outj <- jags(win.data, inits=inits, parameters.to.save=params, model.file="t_test3.txt", n.thin=nt, n.chains=nc, n.burnin=nb, n.iter=ni)

# diagnostics of MCMC
# alot of plots...
traceplot(outj, mfrow=c(4,4))

# Confirm heteroscedasticity:
par(mfrow=c(1, 2))
plot(c(outj$BUGSoutput$mean$residual1, outj$BUGSoutput$mean$residual2), col=c("black", "blue"), pch=c(1, 19), ylab="Residual RTs")
abline(h=0)                
plot(c(outj$BUGSoutput$mean$residual1, outj$BUGSoutput$mean$residual2)~faktor, col="lightblue", xlab="Groups", ylab="Residual RTs")

# end of exercise KRUSCHKE


# explanations
# school = school type written out
# stype = school type 3x (B, G, R)
# schooltype = school type 2x (B + R = R)
table(diss[,c("school","stype","schooltype")])



# not run below this point

# brms initial fit
# library 'brms'

# see BayesFactors and associated analyses on this data set

# check for model that shows how to handle age as a category or continuous case - wisely!
# ie. re-introduce the uncertainty that was dropped by using integer age categories like 12, 13, 14, ...
# see brm() and pp_check()


# categorical covariates in JAGS
# http://www.mikemeredith.net/blog/2017/Categories_in_JAGS.htm
# Bayesian ANOVA: Powerful inference with within-group sample size of 1
# http://www.petrkeil.com/?p=2819

diss.res01 <- brm(W.noSC ~ age + schooltype * sex, data=diss, family=gaussian(), save_all_pars=TRUE)
diss.res0 <- brm(log(W.noSC) ~ age + schooltype * sex, data=diss, family=gaussian(), save_all_pars=TRUE)

# better fit (see Y_pred)
diss.res <- brm(log(W.noSC) ~ log(age) + schooltype * sex, data=diss, family=gaussian(), save_all_pars=TRUE)
diss.res.t1 <- brm(log(W.noSC) ~ log(age) + schooltype + sex, data=diss, family=gaussian(), save_all_pars=TRUE)
diss.res.t2 <- brm(log(W.noSC) ~ schooltype + sex, data=diss, family=gaussian(), save_all_pars=TRUE)
diss.res.t3 <- brm(log(W.noSC) ~ age + schooltype + sex, data=diss, family=gaussian(), save_all_pars=TRUE)
diss.res.t4 <- brm(log(W.noSC) ~ log(age) + schooltype + sex, data=diss, family=student(), save_all_pars=TRUE)
diss.res.t5 <- brm(log(W.noSC) ~ log(age) + age.cat + schooltype + sex, data=diss, family=student(), save_all_pars=TRUE)
diss.res.t6 <- brm(log(W.noSC) ~ log(age) + age.cat + schooltype * sex, data=diss, family=student(), save_all_pars=TRUE)
diss.res.t7 <- brm(log(W.noSC) ~ log(age) + age.cat1 + schooltype * sex, data=diss, family=student(), save_all_pars=TRUE)
diss.res.t8 <- brm(log(W.noSC) ~ log(age) + age.cat1 + schooltype + sex, data=diss, family=student(), save_all_pars=TRUE)
diss.res.t9 <- brm(bf(log(W.noSC) ~ log(age) + age.cat1 + schooltype + sex, sigma ~ 0 + schooltype + sex), data=diss, family=student(), save_all_pars=TRUE)
diss.res.t10 <- brm(bf(log(W.noSC) ~ log(age) + age.cat1 + schooltype + sex, sigma ~ 0 + schooltype * sex), data=diss, family=student(), save_all_pars=TRUE)


# best model?
diss.res.t11 <- brm(bf(log(W.noSC) ~ log(age) + age.cat1 + stype + sex, sigma ~ 0 + stype * sex), data=diss, family=student(), save_all_pars=TRUE)
diss.res.t11 <- brm(bf(W.noSC.log ~ age.log + age.cat + stype + sex, sigma ~ 0 + stype * sex), data=diss, family=student(), save_all_pars=TRUE)


diss.res.t12.0 <- brm(bf(log(W.noSC) ~ log(age) + age.cat1 + stype + sex, sigma ~ 0 + stype + sex), data=diss, family=student(), save_all_pars=TRUE)
diss.res.t12.1 <- brm(bf(W.noSC.log ~ age.log + age.cat + stype * sex, sigma ~ 0 + stype * sex), data=diss, family=student(), save_all_pars=TRUE)
diss.res.t12.2 <- brm(bf(W.noSC.log ~ age.log + age.cat + stype * sex), data=diss, family=gaussian(), save_all_pars=TRUE)


diss.res.t13 <- brm(bf(log(W.noSC) ~ log(age) + age.cat1 + stype + sex, sigma ~ 0 + stype * sex), data=diss, family=lognormal(), save_all_pars=TRUE)
diss.res.t14 <- brm(bf(W.noSC ~ age + age.cat1 + stype + sex, sigma ~ 0 + stype * sex), data=diss, family=lognormal(), save_all_pars=TRUE)


#
diss.res.t12 <- brm(bf(W.noSC.log ~ age.log + age.cat + stype + sex), data=diss, family=student(), save_all_pars=TRUE)
summary(diss.res.t12)
pp_check(diss.res.t12, nsamples=100)
pp.t12 <- posterior_predict(diss.res.t12)



# unequal variances...
diss <- diss[1:363,]
diss$stype <- factor(diss$stype)
diss$sex <- factor(diss$sex)
# diss$age.cat1 <- factor(diss$age.cat1)
with(diss, tapply(W.noSC, stype, FUN=sd))
# with(diss, tapply(W.noSC.log, stype, FUN=sd))
with(diss, tapply(W.noSC, sex, FUN=sd))
# with(diss, tapply(W.noSC, age.cat1, FUN=sd))

boxplot(W.noSC~sex*stype,data=diss)

# plot pp_check for all models so far

modelstoplot <- c(paste("diss.res.t",c(1:14),sep=""), paste("diss.res.12.",0:2,sep=""),"diss.res.S")
modelstoplot
for(i in modelstoplot)
{
 par(ask=TRUE)
 texte <- parse(text=paste("pp_check(",i,")",sep=""))
 cat("\n",texte)
 plot(eval(texte))
}




# schooltype verus stype ????
diss.res.S <- brm(bf(log(W.noSC) ~ log(age) + schooltype * sex, sigma ~ 0 + schooltype*sex), data=diss, family=gaussian(), save_all_pars=TRUE)
bayes_factor(diss.res, diss.res.S)
bayes_factor(diss.res.t11, diss.res.S)
# output:
# Estimated Bayes factor in favor of bridge1 over bridge2: 27.68141


# best model?
diss.res.t11 <- brm(bf(log(W.noSC) ~ log(age) + age.cat1 + stype + sex, sigma ~ 0 + stype * sex), data=diss, family=student(), save_all_pars=TRUE)
diss.res.t11 <- brm(bf(W.noSC.log ~ age.log + age.cat + stype + sex, sigma ~ 0 + stype * sex), data=diss, family=student(), save_all_pars=TRUE)


# best mdoel? see above - t11.PRIORE
model.stan <- bf(log(W.noSC) ~ log(age) + age.cat1 + stype + sex, sigma ~ 0 + stype * sex)
priors <- get_prior(model.stan, data=diss)
priors
priore <- c(
	prior(normal(0,3), class=b),
	prior(normal(0,3), class=b, coef="age.cat120M25"),
	prior(normal(0,3), class=b, coef="logage"),
	prior(normal(0,3), class=b, coef="sexw"),
	prior(normal(0,3), class=b, coef="stypeG"),
	prior(normal(0,3), class=b, coef="stypeR"),
	prior(student_t(3,5,10), class=Intercept),
	prior(cauchy(0,3), class=b, dpar=sigma),
	prior(cauchy(0,3), class=b, coef="sexw", dpar=sigma),
	prior(cauchy(0,3), class=b, coef="stypeB", dpar=sigma),
	prior(cauchy(0,3), class=b, coef="stypeG", dpar=sigma),
	prior(cauchy(0,3), class=b, coef="stypeG:sexw", dpar=sigma),
	prior(cauchy(0,3), class=b, coef="stypeR", dpar=sigma),
	prior(cauchy(0,3), class=b, coef="stypeR:sexw", dpar=sigma)
)
priore

# check priors
make_stancode(model.stan, data=diss, family=student(), prior=priore, save_all_parts=TRUE)

# run model
diss.res.t11.pr1 <- brm(model.stan, data=diss, family=student(), prior=priore, save_all_pars=TRUE)

model.diss.t11 <- stancode(diss.res.t11)
model.diss.t11

# plot sigmas
# TODO - arrange next to each other
sigmas <- exp(posterior_samples(diss.res.t11, "^b_sigma_"))
# ibrary 'ggplot2'
# does not look good (layout), would require new layout
ggplot(stack(sigmas), aes(values)) + geom_density(aes(fill = ind))

# post prior values
prior_summary(diss.res.t11)
prior_summary(diss.res.t11.pr1)

# nonsense! almost same results... but different update of the prior knowledge!
bayes_factor(diss.res.t11, diss.res.t11.pr1)
# output:
# Estimated Bayes factor in favor of bridge1 over bridge2: 27298732393.50789
# =27'298'732'393.50789
# just don't believe a BF, because it is "just" a change of expectation and not absolte result

# summaries
summary(diss.res.t11)
summary(diss.res.t11.pr1)

# compare models
crits <- c("loo","waic","kfold","R2","marglik")
diss.t11.ac <- add_criterion(diss.res.t11, criterion=crits, reloo=TRUE)
diss.t11.pr1.ac <- add_criterion(diss.res.t11.pr1, criterion=crits, reloo=TRUE)
# leave out observation 317 -> check it! (= LOO)

# print out information criteria
crits.print <- c("loo","waic","kfold","marglik")
for(i in crits.print) print(diss.t11.ac[[i]])
for(i in crits.print) print(diss.t11.pr1.ac[[i]])

# comparison plot
par(mfrow=c(1,2))
plot(diss.t11.ac$loo)
plot(diss.t11.pr1.ac$loo)

# R^2
R2 <- t(rbind(
      c(summary(as.vector(diss.t11.ac$R2)),SD=sd(diss.t11.ac$R2),quantile(diss.t11.ac$R2)),
      c(summary(as.vector(diss.t11.pr1.ac$R2)),SD=sd(diss.t11.pr1.ac$R2),quantile(diss.t11.pr1.ac$R2))
	 ))
colnames(R2) <- c("R2|t11","R2|t11.pr1")
R2 <- data.frame(R2,Ratio=R2[,1]/R2[,2])
R2
	 
# comparisons
for(i in c("loo","waic","kfold"))
{
 cat("\n",i,"\n")
 print(loo_compare(diss.t11.ac, diss.t11.pr1.ac, criterion=i))
} 

# R^2
bayes_R2(diss.res.t11)
bayes_R2(diss.res.t11.pr1)
brms:::loo_R2.brmsfit(diss.res.t11)
brms:::loo_R2.brmsfit(diss.res.t11.pr1)
# help('pareto-k-diagnostic')
brms:::loo_R2.brmsfit(diss.res.t11, pareto_k_ids(x, threshold=0.5))

# diagnostic plots
stanplot(diss.res.t11)
stanplot(diss.res.t11.pr1)

# ppc plots
pp_check(diss.res.t11, type="scatter_avg_grouped", group="sex") + geom_abline(intercept=0, slope=1, color="red", lty=2)
pp_check(diss.res.t11, type="scatter_avg_grouped", group="stype") + geom_abline(intercept=0, slope=1, color="red", lty=2)
pp_check(diss.res.t11, type="scatter_avg_grouped", group="age.cat1") + geom_abline(intercept=0, slope=1, color="red", lty=2)

pp_check(diss.res.t11, nsamples=30)

pp_check(diss.res.t11.pr1, nsamples=30)

pp_check(diss.res.t11, type = "error_hist", nsamples=30)
pp_check(diss.res.t11, type = "scatter_avg", nsamples=30)
pp_check(diss.res.t11, type = "stat_2d", nsamples=30)
pp_check(diss.res.t11, type = "boxplot", nsamples=30)
pp_check(diss.res.t11, type = "xyz")

plot(diss.res.t11)
brms:::pairs.brmsfit(diss.res.t11)
marginal_effects(diss.res.t11)
plot(marginal_effects(diss.res.t11), points=TRUE, rug=TRUE)


par(mfrow=c(2,2))
with(diss,plot(jitter(log(W.noSC)),jitter(log(age))))
with(diss,plot(jitter(log(W.noSC)),jitter((age))))
with(diss,plot(jitter((W.noSC)),jitter(log(age))))
with(diss,plot(jitter((W.noSC)),jitter((age))))

par(mfrow=c(4,4))
traceplot(as.mcmc(diss.res.t11))


# not run
bayes_factor(diss.res, diss.res0)
bayes_factor(diss.res, diss.res01)
bayes_factor(diss.res, diss.res.t1) #no schooltype x sex interaction is better
bayes_factor(diss.res, diss.res.t2) #log(age) and interaction sex x schooltype is better than not log(age)
bayes_factor(diss.res.t1, diss.res.t2) #better to keep the log(age) in the model
bayes_factor(diss.res.t1, diss.res.t3) #better keep the log(age) instead of age
bayes_factor(diss.res.t1, diss.res.t4) #t dist is slightly better
bayes_factor(diss.res.t1, diss.res.t5) #slight favor for cut as a category
bayes_factor(diss.res.t1, diss.res.t6) #t1 better than t6 -> against interaction sex*schooltype
bayes_factor(diss.res.t5, diss.res.t6) #t5 better than t6 -> against interaction sex*schooltype
bayes_factor(diss.res.t1, diss.res.t7) #t7 better than t1
bayes_factor(diss.res.t8, diss.res.t7) #t8 better than t7
bayes_factor(diss.res.t8, diss.res.t9) #t9 better than t8 (different sigmas!)
bayes_factor(diss.res.t9, diss.res.t10) #t10 better than t9 BF=500 pro t10
bayes_factor(diss.res.t11, diss.res.t10) #t11 better than t10 by BF=6.83
#***
bayes_factor(diss.res.t11, diss.res.t12) #t11 much better than t12
#***
bayes_factor(diss.res.t11, diss.res.t13) #t11 better than t13
bayes_factor(diss.res.t11, diss.res.t14)



# PPC
# Y_pred
# with noise from measurement error
diss.t11.pred <- brms:::predict.brmsfit(diss.res.t11, summary=FALSE)
str(diss.t11.pred)
head(diss.t11.pred)

# posterior_linpred
# without noise from measurement error
fittedv <- fitted(diss.res.t11)
dat <- as.data.frame(cbind(Y = standata(diss.res.t11)$Y, fittedv))
ggplot(dat) + geom_point(aes(x = Estimate, y = Y))
plot(dat, col="darkred")


(diss[,"W.noSC.log"] %in% dat$Y)
!(diss[,"W.noSC.log"] %in% dat$Y)
which(diss[,"W.noSC.log"] %in% dat$Y)
which(!diss[,"W.noSC.log"] %in% dat$Y)
which(dat$Y %in% diss[,"W.noSC.log"])
which(!dat$Y %in% diss[,"W.noSC.log"])


# A data frame (matrix or array) containing the posterior samples, with one column per parameter.
diss.res.t11.post <- posterior_samples(diss.res.t11)
str(diss.res.t11.post)
head(diss.res.t11.post)
names(diss.res.t11.post)

diss.res.t11.pred <- predict(diss.res.t11, summary=TRUE)
str(diss.res.t11.pred)
hist(diss.res.t11.pred)

BEST:::plotPost(standata(diss.res.t11)$Y)
abline(v=mean(diss.res.t11.pred[,"Estimate"]), col="orange", lwd=3)
abline(v=median(diss.res.t11.pred[,"Estimate"]), col="green", lwd=3)
lines(density(diss.res.t11.pred[,"Estimate"]), col="darkred", lwd=2)


# not run
diss.res.t11.pred <- predict(diss.res.t11, summary=TRUE)
hist(diss.res.t11.pred[,"Estimate"])


# plots via R-package bayesplot
# library 'bayesplot'
# stan
nutsp.t11 <- nuts_params(diss.res.t11)
str(nutsp.t11)
# plots
mcmc_parcoord(as.array(diss.res.t11), np=nutsp.t11)
mcmc_parcoord(mcmc.red, pars=colnames(mcmc.red[[1]])[-c(8,9)])
# PPC
yrep.diss.t11 <- predict(diss.res.t11, summary=FALSE)
# from rstantools
# yrep.diss.t11 <- posterior_predict(diss.res.t11, draws=5)
bayesplot:::ppc_dens_overlay(diss.model[,"W.noSC.log"], yrep.diss.t11)



# https://stackoverflow.com/questions/37768534/logistic-regression-with-categorical-predictors-using-jags
# *** t11
diss.model <- with(diss, model.matrix(W.noSC.log ~ age.log + age.cat1 + stype + sex))
diss.model <- with(diss, model.matrix(W.noSC.log ~ age.log + age.cat + stype + sex))
diss.model <- with(diss, model.matrix(W.noSC.log ~ age.log + age.cat.17.19. + age.cat.20.25. + stype + sex))
# ***
diss.model <- with(diss, model.matrix(W.noSC.log ~ age.log + sex + schooltype + SS))
diss.model <- with(diss, model.matrix(W.noSC.log ~ age.log + sex + schooltype))
diss.model <- with(diss, model.matrix(W.noSC.log ~ age + sex + schooltype))
diss.model.list <- as.list(data.frame(diss.model[,-1]))
diss.model.list
diss.model <- data.frame(W.noSC.log=diss.nona$W.noSC.log, diss.model[,-1])
head(diss.model)
diss.model
xName <- colnames(diss.model[,-c(1)]) #without Y = dependent variable
xName
yName

# run rjags model
mcmc.dm11 <- genMCMC( data=diss.model, xName=xName, yName=yName,
                     numSavedSteps=numSavedSteps, thinSteps=thinSteps, saveName=fileNameRoot )

parameterNames = varnames(mcmc.dm11) # get all parameter names
graphFileType <- "png"
for ( parName in parameterNames ) {
  diagMCMC( codaObject=mcmc.dm11 , parName=parName , 
            saveName=fileNameRoot , saveType=graphFileType )
}
summaryInfo = smryMCMC2( mcmc.dm11 )
show(summaryInfo)
plotMCMC( mcmc.dm11, data=diss.model, xName=xName, yName=yName, pairsPlot=TRUE, showCurve=FALSE, saveName=fileNameRoot, saveType=graphFileType )


