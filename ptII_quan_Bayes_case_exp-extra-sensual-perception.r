###### function to calculate t-test BayesFactor
t.test.BF <- function(mean.delta=NA, sigma.delta=NA, n1=NA, n2=NA, mu1=NA, mu2=NA, s1=NA, s2=NA, samp1=NA, samp2=NA, PR=TRUE, digits=3)
{
  # Gönen, Johnson, Lu and Westfall (2005)
  # original: https://amstat.tandfonline.com/doi/abs/10.1198/000313005X55233
  
  # Jeff Gill, p.247
  # t-test = difference of means Bayes-Factor
  # H0: mu1=mu2, H1: mu1 != mu2
  # test samp1 versus samp2
  
  # prior beliefs
  # mean.delta
  # sigma.delta
  
  # empirical data
  if(!length(samp1) == 1 & !length(samp2) == 1)
  {
    n1 <- length(samp1)
    n2 <- length(samp2)
    mu1 <- mean(samp1)
    mu2 <- mean(samp2)
    s1 <- sd(samp1)
    s2 <- sd(samp2)
  }
  
  # calculations
  delta <- mu1-mu2
  ndelta <- ( n1^(-1) +  n2^(-1) )^(-1)
  dfree <- n1+n2-2
  # standard difference of means test statistic
  tvalue <- delta / ( sqrt( ((n1-1)*s1^2 + (n2-1)*s2^2)/(dfree) ) / sqrt(ndelta) )
  #
  pv <- sqrt(1+ndelta*sigma.delta^2)
  ncp <- mean.delta*sqrt(ndelta) / pv
  
  # BF.null H0 over H1
  BF.null <- dt(tvalue, dfree, 0) / (dt(tvalue/pv, dfree, ncp)/pv)
  # or BF.alternative
  BF.alt <- 1/BF.null
  TEST <- BF.null > BF.alt
  if(TEST) TEXT <- c("BF in favor of H0") else TEXT <- c("BF in favor of H1")
  NOTE <- c("BF01 favors zero difference | BF10 favors a difference")
  TYP <- c("Bayesian two-sample t-Test\nGönen, Johnson, Lu and Westfall (2005)")
  res <- structure(list("BF01"=BF.null, "BF10"=BF.alt, "Test BF01 > BF10"=TEST, "Test BF01 < BF10"=!TEST, "Result"=TEXT, TYP=TYP, note=NOTE))
  
  if(PR)
  {
    cat("\n    ", res$TYP, "\n\n")
    note <- res$note
    res[c("TYP", "note")] <- NULL
    cat(paste(format(names(res), width = 15L, justify = "right"), format(res, digits=digits), sep=" = "), sep = "\n")
    if (!is.null(note)) 
    {
      cat("\n", "NOTE: ", note, "\n\n", sep = "")
    } else cat("\n")
    #invisible(res)
  }
  return(res) 
}
# call:
# t.test.BF(mean.delta=2, sigma.delta=6, samp1=hell$subjsicher[hell$treat=="C"], samp2=hell$subjsicher[hell$treat=="T"])
########################## END OF FUNCTION

# file:
# ptII_quan_Bayes_case_exp-extra-sensual-perception.rr

# location:
# chap. 6 [6.7.1.4]
# Hellsehen — in guter Grund für Nullhypothesentesten?

# load necessary libs
library(LearnBayes)
library(HDInterval)
library(lme4)
library(arm)
library(BEST)
library(BayesFactor)
library(BayesianFirstAid)
library(lattice)
library(brms)

# load helper functions
source("ptall_generalfuncs.r")
source("ptall_generalfuncs_Bayes_binomial.r")
source("ptall_generalfuncs_Bayes_Beta_determine.r")


# pre-requisites - probs and chances
p <- 0.25
k <- 20 # playing cards
# prob
0.25^20
# 9.09495e-13
p^k

# win in lotto
1/choose(49,6)
# 7.15112e-08

# no hit
.75^20
# 0.00317121
(1-p)^k


# clairvoyance | second sight | Hellsehen

# read data
hell <- read.table("LG_clairvoyance-exp_raw_data.tab", sep="\t", header=TRUE)
head(hell)
dim(hell)
str(hell)

postsubj <- read.table("LG_clairvoyance-exp-post_raw_data.tab", sep="\t", header=TRUE)
postsubj
dim(postsubj)
str(postsubj)

# not as character
postsubj$treat <- as.factor(postsubj$treat)

# prepare tables
head(hell)
CT <- table(hell$Upn,hell$treat)
CT.fac <- as.factor(CT[,"C"])
levels(CT.fac) <- c("T","C")

upn.differ <- with(hell, table(Upn, differ))

hell.res <- data.frame(cbind(Upn=dimnames(upn.differ)$Upn, upn.differ[,1:2]), treat=CT.fac)
hell.res$WAHR <- as.numeric(hell.res$WAHR)
hell.res$FALSCH <- as.numeric(hell.res$FALSCH)

hell$differ <- as.factor(hell$differ)
hell$differ.TF <- ifelse(hell$differ == "WAHR", TRUE, FALSE)
hell$treat <- as.factor(hell$treat)

# actual capability clarivoyance
with(hell.res, boxplot(WAHR ~ treat, col="bisque", notch=TRUE, main="right solution"))
#with(hell.res, boxplot(FALSCH ~ treat, col="violetred3", notch=TRUE, main="wrong solution"))

# relationship correct solution and subjective confidence
cor(as.numeric(hell$differ), hell$subjsicher)

# EDA
tab.tf <- with(hell, table(solution, differ))
tab.tf
mosaicplot(tab.tf)
apply(tab.tf,2,sum)
95/(305+95)

# summary
t(apply(upn.differ,2,summary))

# outlier
upn.differ
hellp <- 0.25
Ni <- 20
dig <- 3
upn.differ.bp <- t(apply(upn.differ, 1, function(i)
{
  si <- i[2]
  cbind(
    # pbl(theta=hellp, si=i[2], Ni=sum(i), loga=TRUE, reexp=TRUE),
    # pjc(theta=hellp, si=i[2], Ni=sum(i), loga=TRUE, reexp=TRUE)
    mean.BL=round(((si + 1) / (Ni + 2)),dig),
    mean.JC=round(si/Ni,dig)  
  )
}))
colnames(upn.differ.bp) <- c("mean(pbl)","mean(pjc)")
upn.differ.bp

# https://stackoverflow.com/questions/3541713/how-to-plot-two-histograms-together-in-r
par(oma=c(2,1,1,1), "cex.axis"=1, bty="l")
hist(upn.differ.bp[,"mean(pbl)"], prob=TRUE, ylim=c(0,8), xlim=c(0,1), pre.plot=grid(), col=scales::alpha("yellowgreen",.5), border="white", xlab="mean of p(Bayes-Laplace) and p(Jeffreys-Carnap)", main="Clairvoyance")
hist(upn.differ.bp[,"mean(pjc)"], add=T, col=scales::alpha("orange",.5), border="white")
dens.pbl <- density(upn.differ.bp[,"mean(pbl)"])
dens.pjc <- density(upn.differ.bp[,"mean(pjc)"]) 
lines(dens.pbl, col="green", lwd=2)
lines(dens.pjc, col="darkorange", lwd=2)

# short summary statistics
apply(upn.differ.bp,2,summary)
apply(upn.differ.bp,2,fivenum2)
apply(upn.differ.bp,2,mean)
apply(upn.differ.bp,2,range)


# subjective confidence
summary(hell$subjsicher)
with(hell, tapply(subjsicher, treat, summary))
with(hell, tapply(subjsicher, treat, sd))

# boxplots
par(mfrow=c(1,2))
with(hell, boxplot(subjsicher ~ differ.TF, col="bisque", notch=TRUE, main="subjective confidence"))
with(hell, boxplot(subjsicher ~ treat, col="bisque", notch=TRUE, main="subjective confidence"))

# interaction plot subjective confidence
with(hell, interaction.plot(Upn, trial, subjsicher, col=rainbow(20), bty="n", legend=TRUE, main="Interaction plot\nsubjective confidence", pre.plot=grid()))

# more interaction plots
par(mfrow=c(2,2))
par(oma=c(2,1,1,1), "cex.axis"=1, bty="l")
with(hell, interaction.plot(differ, treat, subjsicher, col="violetred3", bty="n", pre.plot=grid()))
mtext("Interaction plot", outer=TRUE, line=-2, cex=1.5, side=3)
with(hell, interaction.plot(differ, Upn, subjsicher, col=rainbow(max(levels(factor(hell$Upn)))), bty="n", pre.plot=grid()))

upntreat <- with(hell, table(Upn,treat))
colos <- as.factor(upntreat[,1])
designcol <- c("red","blue")
colos <- as.character(factor(colos, labels=designcol))
upntreat <- data.frame(cbind(colos, upntreat))
upntreat

with(hell, interaction.plot(differ, Upn, subjsicher, col=colos, bty="n", legend=FALSE, main=""))
legend("top", legend=c("T","C"), col=designcol, bty="n", lwd=2, horiz=TRUE)

mtext("Interaction plots", outer=TRUE, line=-2, cex=1.5, side=3)

# further plots
coplot(subjsicher ~ as.factor(treat) | differ, data=hell, panel=panel.smooth)
coplot(subjsicher ~ differ | treat, data=hell, panel=panel.smooth)

# further interaction plots
par(mfrow=c(2,2))
with(hell, interaction.plot(trial, treat, subjsicher, col=c("red","blue"), bty="n", legend=TRUE, main="Interaction plot", pre.plot=grid()))
with(hell, interaction.plot(Upn, treat, subjsicher, col=c("red","blue"), bty="n", legend=TRUE, main="Interaction plot", pre.plot=grid()))
with(hell, interaction.plot(treat, trial, subjsicher, col=rainbow(20), bty="n", legend=TRUE, main="Interaction plot", pre.plot=grid()))
with(hell, interaction.plot(Upn, trial, subjsicher, col=rainbow(20), bty="n", legend=TRUE, main="Interaction plot\nsubjective confidence", pre.plot=grid()))


# if we want to remove person 9 and 20
# remove person 9 and 20
hell2 <- hell[-which(hell$Upn %in% c(9,20)),]
str(hell2)


##### differ = performance tests clairvoyance

# Bayes test success rate
# requires Bayesian helper functions (UMS stuff...)
hell.sum <- with(hell, table(differ))
si <- hell.sum["WAHR"]
Ni <- sum(hell.sum)
hell.mean.BL <- round(((si + 1) / (Ni + 2)),dig)
hell.mean.JC <- round(si/Ni,dig)
hell.mean.BL
hell.mean.JC

# Binomial probs (Studer 1996 paper)
steps <- 1000
theta <- seq(0,1,length.out=steps)
pbl.res <- pbl(theta=theta, si=si, Ni=Ni, loga=TRUE, reexp=TRUE)
pjc.res <- pjc(theta=theta, si=si, Ni=Ni, loga=TRUE, reexp=TRUE)
sN.ME.res <- data.frame(pbl.res, pjc.res)
head(sN.ME.res)
plot.bl.jc(theta, sN.ME.res=sN.ME.res, si=si, Ni=Ni, filling=FALSE)
# to see differences better
plot.bl.jc(theta, sN.ME.res=sN.ME.res, si=si, Ni=Ni, filling=FALSE, sele=c(0.17,0.35))
plot.bl.jc(theta, sN.ME.res=sN.ME.res, si=si, Ni=Ni, filling=TRUE)
sN.ME.post.summary <- sN.post.su(Ni=Ni, si=si)


# determine prior beta
prior.ab <- beta.determine.opt(p=c(0.5,0.95,0.05), qua=c(0.25,0.4,0.1), ab.start=c(10,25), graph=TRUE, leg1="topright", leg2="right")
str(prior.ab)
hell.prior.res <- do.call("cbind", beta.summary(a=prior.ab$res.ab[["a"]], b=prior.ab$res.ab[["b"]]) )
# output
hell.prior.res
prior.ab$res.ab
prior.ab$res.ab3

# likelihood
likeli.ab <- bino.ab.lik(si=si , Ni=Ni)
likeli.ab
hell.likeli.res <- do.call("cbind", beta.summary(a=likeli.ab[["a"]], b=likeli.ab[["b"]]) )
hell.likeli.res
# create posterior
post.ab <- bino.ab.post(a.prior=prior.ab$res.ab["a"], b.prior=prior.ab$res.ab["b"], si=si, Ni=Ni)
post.ab
hell.post.res <- do.call("cbind", beta.summary(a=post.ab[["a"]], b=post.ab[["b"]]) )
hell.post.res
unlist(hell.prior.res)

v <- c(unlist(hell.prior.res[,c("a","b")]),
       unlist(hell.likeli.res[,c("a","b")]),
  	   unlist(hell.post.res[,c("a","b","mean","sd")])
				)
v <- data.frame(t(v))
colnames(v) <- c("a.prior","b.prior","a.lik","b.lik","a.post","b.post","mean.post","sd.post")
v
beta.triplot(si=si, Ni=Ni, v=v, multiplot=FALSE, musdplot=F, plots=c(TRUE,TRUE,TRUE), filling=TRUE)
beta.triplot(si=si, Ni=Ni, v=v, multiplot=FALSE, musdplot=F, plots=c(TRUE,TRUE,TRUE), filling=FALSE, sele=c(0,0.5))
beta.triplot(si=si, Ni=Ni, v=v, multiplot=FALSE, musdplot=F, plots=c(TRUE,TRUE,TRUE), filling=TRUE, sele=c(0,0.5))

# hdis
prior <- dbeta(theta, v[,"a.prior"], v[,"b.prior"])
likelihood <- dbeta(theta, v[,"a.lik"], v[,"b.lik"])
posterior <- dbeta(theta, v[,"a.post"], v[,"b.post"])

probs <- c(0.69,0.87,0.99)
ab <- rbind(prior.ab$res.ab,  unlist(likeli.ab),  unlist(post.ab))
rownames(ab) <- c("prior","likelihood","post") 
ab

# library 'HDInterval'
hdis <- do.call("rbind", lapply(seq_along(probs), function(x)
{
  apply(ab, 1, function(i)
  {
   a <- unlist(i["a"])
   b <- unlist(i["b"])
   hdi(qbeta, shape1=a, shape2=b, credMass=probs[x])
  })
 }
))
hdis <- cbind(prob=rep(probs,each=2),hdis)
hdis 


# base contingency table
hell.tab <- with(hell, table(treat,differ))
#hell.tab <- table(hell$treat, hell$differ)
hell.tab


# library(BayesianFirstAid)
# BayesianFirstAid:::bayes.binom.test
binom.test(95,95+305,0.25)
bino1 <- bayes.binom.test(95,95+305,cred.mass=0.9, comp.theta=0.25)
summary(bino1)
plot(bino1)
hdi(bino1$mcmc_samples, credMass=0.99)
hdi(bino1$mcmc_samples, credMass=0.9)
hdi(bino1$mcmc_samples, credMass=0.8)
diagnostics(bino1) 

# posterior odds
mean(unlist(bino1$mcmc_samples[,"theta"]) > 0.21)
mean(unlist(bino1$mcmc_samples[,"theta"]) > 0.21 & unlist(bino1$mcmc_samples[,"theta"]) < 0.27)


# classical binomial chisquare test
chisq.test(hell.tab)

# library(BayesFactor)
# jointMulti"
bf1 <- contingencyTableBF(hell.tab, sampleType="jointMulti")
1/bf1
# indepMulti
bf0 <- contingencyTableBF(hell.tab, sampleType="indepMulti", fixedMargin="rows")
1/bf0

# posterior
iterations <- 1000
bf0.samps <- contingencyTableBF(hell.tab, sampleType="indepMulti", fixedMargin="rows", posterior=TRUE, it=iterations)
bf0.samps
plot(bf0.samps, col="darkred", bty="l")

# recap data
hell.res
# 1/4 = 5/20 "guess" probability
with(hell.res, tapply(WAHR,treat,mean))

# classical t-test
with(hell.res, t.test(WAHR[treat=="C"], WAHR[treat=="T"], var.equal=FALSE))

# simple Bayesian t-Test with prior believes clairvoyance
means.TC <- with(hell.res, tapply(WAHR, treat, mean))
contr <- hell.res$WAHR[hell.res$treat=="C"]
treat <- hell.res$WAHR[hell.res$treat=="T"]
do.call("rbind", lapply(list(contr=contr,treat=treat), function(x) c(N=length(x), summary(x), SD=sd(x), VAR=var(x), fivenum2(x))))
cohensd(s1=contr, s2=treat)
hell.bf.t <- t.test.BF(mean.delta=0.5, sigma.delta=2, samp1=contr, samp2=treat)


# BEST by Kruschke
# library(BEST)
hell.best <- with(hell.res, BESTmcmc(WAHR[treat=="C"], WAHR[treat=="T"]))
summary(hell.best)
plot(hell.best)

# group differences
meandiffhell.best <- hell.best$mu1 - hell.best$mu2
sigmadiffhell.best <- hell.best$sigma1 - hell.best$sigma2
mean(meandiffhell.best)
mean(sigmadiffhell.best)

# add plot with ROPE
# we compare +/- 5/20 = ie. more than 5 difference
plot(hell.best, credMass=0.87, compVal=6, ROPE=c(-5,5), showMode=TRUE)
pairs(hell.best)
hdi(hell.best, credMass=0.87)
plotAll(hell.best)
plotPostPred(hell.best)

# below or above zero?
# influences how we ask
mean(meandiffhell.best < 0)
mean(abs(meandiffhell.best) > 0.1)
mean(meandiffhell.best < -1 | meandiffhell.best > 1)
mean(meandiffhell.best < -3 | meandiffhell.best > 3)

# posterior odds against specific value
# test against critical values
# values chosen because we want to see real 2nd sight, not just a little bit
mu.crit <- 2.5
sigma.crit <- 1.3

mean(meandiffhell.best < mu.crit)
1/1-mean(meandiffhell.best < mu.crit)

mean(sigmadiffhell.best < sigma.crit)
mean(meandiffhell.best < mu.crit & sigmadiffhell.best < sigma.crit)
mean(meandiffhell.best < mu.crit | sigmadiffhell.best < sigma.crit)

# plot ROPE via function or manually
# we take the boundary of (5+5)/20 = ~50% instead of 25%
# that would be substantial...
plotAreaInROPE(meandiffhell.best, credMass=0.99, compVal=5, maxROPEradius=10)
# =
sek <- seq(-15,0,0.01)
y <- sapply(sek, function(i) mean(meandiffhell.best < i))
plot(sek, y, bty="n", type="l", col="violetred3", pre.plot=grid(), main="Plot area in ROPE")


# BayesFactor
# Upn as factor
hell.res$Upn <- factor(hell.res$Upn)
# create a prob value of TRUE vs. (TRUE+FALSE)
hell.res$probres <- with(hell.res, WAHR/(FALSCH+WAHR))
# create a mean subj confidence value
hell.res$subjsichersum <- with(hell, tapply(subjsicher, Upn, mean))
hell.res
str(hell.res)
# all models - does make sense only for exploratory tasks, not in general!
hell.res.BF <- generalTestBF(probres ~ treat * subjsichersum, data=hell.res, whichRandom="Upn",noSample=FALSE,whichModels='all')
hell.res.BF
1/hell.res.BF
plot(hell.res.BF)

# reduce error
hell.res.BF.re <- recompute(hell.res.BF, iterations=5e+5)
hell.res.BF
hell.res.BF.re

# posterior
nsamps <- 1e+5
hell.res.samps <- posterior(hell.res.BF, index=4, iterations=nsamps)
head(hell.res.samps)
summary(hell.res.samps)
plot(hell.res.samps, col="violetred3")

# T > C
TgreaterC.samps <- (hell.res.samps[, "treat-T"] > hell.res.samps[, "treat-C"])
TgreaterC.samps_N <- sum(TgreaterC.samps)
TgreaterC.samps_N
TgreaterC.samps_N/nsamps

##### END OF differ = performance tests clairvoyance


# short and rough overview for further work
# attitude towards ESP pre + post
with(postsubj, table(ASWpre,ASWpost))
# attitude towards ESP pre + post across groups
with(postsubj, ftable(treat, ASWpre,ASWpost))
# perceived concentration across groups
with(postsubj, ftable(treat, Konzentration))
with(postsubj, plot(treat, Konzentration))



##### subjective confidence

# classical anova
summary(aov(subjsicher ~ differ*treat, data=hell))

# library(BEST)
besthell.subjsec <- BESTmcmc(hell$subjsicher[hell$treat=="C"], hell$subjsicher[hell$treat=="T"])
# add plot with ROPE
plot(besthell.subjsec, credMass=0.87, compVal=-7, ROPE=c(-12,-6), showMode=TRUE)
summary(besthell.subjsec)
pairs(besthell.subjsec)
hdi(besthell.subjsec)
plotAll(besthell.subjsec)
plotPostPred(besthell.subjsec)
# differences between groups
meandiffbesthell.subjsec <- besthell.subjsec$mu1 - besthell.subjsec$mu2
sigmadiffbesthell.subjsec <- besthell.subjsec$sigma1 - besthell.subjsec$sigma2
mean(meandiffbesthell.subjsec)
mean(sigmadiffbesthell.subjsec)

# t.test.BF
contr <- hell$subjsicher[hell$treat=="C"]
treat <- hell$subjsicher[hell$treat=="T"]
do.call("rbind", lapply(list(contr=contr,treat=treat), function(x) c(N=length(x), summary(x), SD=sd(x), VAR=var(x), fivenum2(x))))
cohensd(s1=contr, s2=treat)
hell.bf.t <- t.test.BF(mean.delta=2, sigma.delta=6, samp1=contr, samp2=treat)

### not run - t.test.BF example
# single values
# prior beliefs
mean.delta <- 2
sigma.delta <- 6
# empirical data
n1 <- 200
n2 <- 200
mu1 <- 37.8
mu2 <- 45.425
s1 <- 21.39389
s2 <- 20.31027
hell.bf.t.single <- t.test.BF(mean.delta=mean.delta, sigma.delta=sigma.delta, n1=n1, n2=n2, mu1=mu1, mu2=mu2, s1=s1, s2=s2)

# prior beliefs
mean.delta <- 2
sigma.delta <- 6
# empirical data
n1 <- 25
n2 <- 24
mu1 <- 110
mu2 <- 114
s1 <- 10.1
s2 <- 9.5
bf.t <- t.test.BF(mean.delta=mean.delta, sigma.delta=sigma.delta, n1=n1, n2=n2, mu1=mu1, mu2=mu2, s1=s1, s2=s2)
### END OF not run


# BayesFactor
#
# http://bayesfactor.blogspot.com/2015/01/multiple-comparisons-with-bayesfactor-2.html
# Compare the unrestricted “full” model to the null (already done, with anovaBF)
# Compare the unrestricted “full” model to an order restriction
# Use the resulting two Bayes factors to compare the null to the order restriction.
# https://www.sciencedirect.com/science/article/pii/S0167715214001862
hell$Upn <- factor(hell$Upn)
hell$treat <- factor(hell$treat)
str(hell)

anova(lm(subjsicher ~ treat, data=hell))
anovabf0 <- anovaBF(subjsicher ~ treat * differ, data=hell)
anovabf0

anovabf0.x <- anovaBF(subjsicher ~ treat * differ, data=hell, whichModels="top")
anovabf0.x
plot(anovabf0[3:4]/anovabf0[2])

# test models
lmbf0 <- lmBF(subjsicher ~ treat + differ + treat:differ, data=hell)
lmbf1 <- lmBF(subjsicher ~ treat + differ, data=hell)
lmbf2 <- lmBF(subjsicher ~ treat, data=hell)
lmbf1/lmbf0
lmbf2/lmbf1
lmbf2/lmbf0

# reduce error
lmbf2.re <- recompute(lmbf2, iterations=5e+5)
lmbf2.re
chains <- posterior(lmbf2, iterations=10000)
summary(chains)
plot(chains, col="violetred3")

1/anovabf0
plot(anovabf0)
nsamps <- 1e+5
samps <- posterior(anovabf0, index=4, iterations=nsamps)
head(samps)
consistent <- (samps[, "treat-T"] > samps[, "treat-C"])
N_consistent <- sum(consistent)
N_consistent
N_consistent/nsamps
# the prior odds are easy. All order restrictions have the same probability, so the odds of any single order restriction is against the full model are just
# 1/Number of possible orderings
# With three factor levels, there are 6 orderings, so the prior odds are 1/6.
PRIORODDS <- 2
bf_restriction_against_full <- (N_consistent/nsamps) / (PRIORODDS)

bf0_full_against_null <- as.vector(anovabf0)

bf0_restriction_against_null <- bf_restriction_against_full * bf0_full_against_null
bf0_restriction_against_null

anovabf1 <- anovaBF(subjsicher ~ treat + differ + Upn, whichRandom="Upn", data=hell)
anovabf1
1/anovabf1
plot(anovabf1)

anovabf2 <- anovaBF(subjsicher ~ treat + differ + Upn, whichRandom="Upn", whichModels="top", data=hell)
anovabf2
1/anovabf2
plot(anovabf2)


# repeated measures
# https://cran.r-project.org/web/packages/BayesFactor/vignettes/manual.html#fixed
hell$trial <- factor(hell$trial)
summary(aov(subjsicher ~ treat*differ + Error(trial/Upn), data=hell))
summary(aov(subjsicher ~ treat + Error(trial/Upn), data=hell))
summary(aov(subjsicher ~ treat + Error(trial), data=hell))

hell.avobf0 <- anovaBF(subjsicher ~ trial + Upn, data=hell, whichRandom=c("trial","Upn"))
hell.avobf0
hell.avobf1 <- anovaBF(subjsicher ~ treat*differ + trial + Upn, data=hell, whichRandom=c("trial","Upn"))
hell.avobf1
hell.avobf2 <- anovaBF(subjsicher ~ treat*differ, data=hell)
hell.avobf2
hell.avobf3 <- anovaBF(subjsicher ~ trial , data=hell, whichRandom=c("trial"))
hell.avobf3
anovaBF(subjsicher ~ Upn , data=hell, whichRandom=c("Upn"))
hell.avobf4 <- anovaBF(subjsicher ~ treat + trial + Upn, data=hell, whichRandom=c("trial","Upn"))
hell.avobf4
hell.avobf5 <- anovaBF(subjsicher ~ treat + trial, data=hell, whichRandom=c("trial"))
hell.avobf5
hell.avobf6 <- anovaBF(subjsicher ~ treat + Upn, data=hell, whichRandom=c("Upn"))
hell.avobf6
hell.avobf6 <- anovaBF(subjsicher ~ treat, data=hell)
hell.avobf6
hell.avobf5 <- anovaBF(subjsicher ~ treat + differ + trial, data=hell, whichRandom=c("trial"))
hell.avobf5
anovaBF(subjsicher ~ treat + Upn, data=hell, whichRandom=c("Upn"))
# anovaBF(subjsicher ~ treat + differ + trial + Upn, data=hell, whichRandom=c("Upn"))
anovaBF(subjsicher ~ treat + Upn, data=hell, whichRandom=c("Upn"))
anovaBF(subjsicher ~ Upn, data=hell, whichRandom=c("Upn"))
anovaBF(subjsicher ~ Upn, data=hell)


# not possible: 'Bayes factors have different denominator models; they cannot be compared.'
anovaBF(subjsicher ~ treat + Upn, data=hell)/anovaBF(subjsicher ~ treat + Upn, data=hell, whichRandom=c("Upn"))

summary(lmer(subjsicher ~ treat*differ + (1|trial) + (1|Upn), data=hell))
summary(lmer(subjsicher ~ treat + (1|trial) + (1|Upn), data=hell))
summary(lmer(subjsicher ~ treat + (1|trial) + (treat|Upn), data=hell))
summary(lmer(subjsicher ~ treat + (1|trial), data=hell))
summary(lmer(subjsicher ~ treat + (1|Upn), data=hell))
summary(lm(subjsicher ~ treat*differ, data=hell))
summary(lm(subjsicher ~ treat, data=hell))

summary(lm(subjsicher ~ treat*differ, data=hell))
summary(lmer(subjsicher ~ treat*differ + (1|Upn), data=hell))
anovabf.rep <- anovaBF(subjsicher ~ treat*differ + Upn, data=hell, whichRandom="Upn")
anovabf.rep
1/anovabf.rep
plot(anovabf.rep)
lmbf.nornd <- lmBF(subjsicher ~ treat*differ, data=hell)
# lmbf.rnd <- lmBF(subjsicher ~ treat*differ + Upn, data=hell, whichRandom="Upn")
lmbf.rnd <- lmBF(subjsicher ~ Upn, data=hell, whichRandom="Upn")
bf2 <- lmbf.nornd / lmbf.rnd

bfall <- c(anovabf.rep,bf2)
bfall

anovabf.rep[4] / bf2

gbf1 <- generalTestBF(subjsicher ~ treat + differ + treat:differ + Upn, data=hell, whichRandom="Upn")
gbf1

# create factors so models work without any error message
hell2$trial.1 <- factor(hell2$trial.1)
hell2$Upn.1 <- factor(hell2$Upn.1)
str(hell2)

hell2.avobf0 <- anovaBF(subjsicher ~ trial.1 + Upn.1, data=hell2, whichRandom=c("trial.1","Upn.1"))
hell2.avobf0
hell2.avobf1 <- anovaBF(subjsicher ~ treat*differ + trial.1 + Upn.1, data=hell2, whichRandom=c("trial.1","Upn.1"))
hell2.avobf1
hell2.avobf2 <- anovaBF(subjsicher ~ treat*differ, data=hell2)
hell2.avobf2
hell2.avobf3 <- anovaBF(subjsicher ~ trial , data=hell2, whichRandom=c("trial"))
hell2.avobf3
anovaBF(subjsicher ~ Upn.1 , data=hell2, whichRandom=c("Upn.1"))
hell2.avobf4 <- anovaBF(subjsicher ~ treat + trial.1 + Upn.1, data=hell2, whichRandom=c("trial.1","Upn.1"))
hell2.avobf4
hell2.avobf5 <- anovaBF(subjsicher ~ treat + trial.1, data=hell2, whichRandom=c("trial.1"))
hell2.avobf5
hell2.avobf6 <- anovaBF(subjsicher ~ treat + Upn.1, data=hell2, whichRandom=c("Upn.1"))
hell2.avobf6
hell2.avobf6 <- anovaBF(subjsicher ~ treat, data=hell2)
hell2.avobf6
hell2.avobf5 <- anovaBF(subjsicher ~ treat + differ + trial.1, data=hell2, whichRandom=c("trial.1"))
hell2.avobf5
anovaBF(subjsicher ~ treat + Upn.1, data=hell2, whichRandom=c("Upn.1"))
anovaBF(subjsicher ~ treat + differ + trial.1 + Upn.1, data=hell2, whichRandom=c("Upn.1"))
anovaBF(subjsicher ~ treat + Upn.1, data=hell2, whichRandom=c("Upn.1"))
anovaBF(subjsicher ~ Upn.1, data=hell2, whichRandom=c("Upn.1"))
anovaBF(subjsicher ~ Upn.1, data=hell2)
anovaBF(subjsicher ~ treat + Upn.1, data=hell2, whichRandom=c("Upn.1"))
anovaBF(subjsicher ~ treat + Upn.1, data=hell2)

str(hell)
boxplot(subjsicher ~ Upn, data=hell)
boxplot(subjsicher ~ Upn, data=hell2)
boxplot(subjsicher ~ treat, data=hell)
boxplot(subjsicher ~ treat, data=hell2)
boxplot(subjsicher ~ trial, data=hell)
boxplot(subjsicher ~ trial, data=hell2)

# library(lattice)
# for each subject
xyplot(hell$subjsicher ~ trial|Upn, type="l", data=hell)
# for each trial
xyplot(hell$subjsicher ~ Upn|trial, type="l", data=hell)

# trials
bwplot(hell$subjsicher ~ trial, type="l", data=hell)
# subjects
bwplot(hell$subjsicher ~ Upn, type="l", data=hell)

with(hell2, ftable(differ, treat, Upn))
# acf fun over trials for each person
with(hell2, ftable(subjsicher, trial))
with(hell2, acf(table(subjsicher, trial), lag.max=20))

with(hell2,bwplot(subjsicher~Upn|treat))
with(hell2,bwplot(subjsicher~treat))
with(hell2,bwplot(subjsicher~treat|differ))

anovaBF(subjsicher ~ treat + Upn.1 + differ, data=hell2, whichRandom=c("differ"))
anovaBF(subjsicher ~ treat + Upn.1 + differ, data=hell2)


# full Bayesian
#
# library(brms)
# simple HLM
# treatment
brmX <- brm(subjsicher ~ treat, data=hell2, save_all_pars=TRUE, chains=4, cores=4, family=negbinomial(link="log"))
summary(brmX)
pairs(brmX)
plot(brmX)
pp_check(brmX)

# 1|Upn
brm0 <- brm(subjsicher ~ treat + (1|Upn), data=hell2, save_all_pars=TRUE, chains=4, cores=4, family=negbinomial(link="log"))
summary(brm0)
pairs(brm0)
plot(brm0)
pp_check(brm0)


# to play around with
# scale for mean=0, sd=1
hell2$subjsicher.sc <- scale(hell2$subjsicher)
# scale for mean=0
hell2$subjsicher.mc <- hell2$subjsicher-mean(hell2$subjsicher)
# scale for sd=1
hell2$subjsicher.ss <- hell2$subjsicher/sd(hell2$subjsicher)

# gaussian
brm5 <- brm(subjsicher.sc ~ treat +(1|Upn), data=hell2, save_all_pars=TRUE, chains=4, cores=4, family=gaussian)
summary(brm5)
pairs(brm5)
plot(brm5)
pp_check(brm5, nsamples=10)
bayes_R2(brm5)

bayes_factor(brm0,brm5)
bayes_factor(brm5,brm0)

# + differ = performance
brm2 <- brm(subjsicher ~ treat + differ + (1|Upn), data=hell2, save_all_pars=TRUE, chains=4, cores=4, family=negbinomial(link="log"))
summary(brm2)
pairs(brm2)
plot(brm2)
pp_check(brm2)

# + trial = sequence
brm3 <- brm(subjsicher ~ treat + differ + trial + (1|Upn), data=hell2, save_all_pars=TRUE, chains=4, cores=4, family=negbinomial(link="log"))
summary(brm3)
pairs(brm3)
plot(brm3)
pp_check(brm3)

bayes_R2(brmX)
bayes_R2(brm0)
bayes_R2(brm2)
bayes_R2(brm3)

bayes_factor(brmX, brm0)
bayes_factor(brm0, brm2)
bayes_factor(brm0, brm3)


##### END OF subjective confidence

