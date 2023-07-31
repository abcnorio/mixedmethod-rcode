### (C) 2005-2023 by Leo Guertler 
### R-code supplement
### to the book
###
### "Subjektive Ansichten und objektive Betrachtungen"
###
### written by Gürtler & Huber (2023)
###
### All R-code is published under the GPL v3 license:
###
### https://www.gnu.org/licenses/gpl-3.0.en.html
###
### except for 'borrowed' code - see links and references.
### For this R-code the original license of the respective
### authors is valid.
###
### R-code published on
###
### https://osdn.net/projects/mixedmethod-rcode
### https://github.com/abcnorio/mixedmethod-rcode



# file:
# ptII_quan_Bayes_Fisher_LadyBristol-BUGS.r

# location:
# chap. 6 [6.13.5]
# Fisher reloaded — mehr Tee

# load necessary libraries
library(HDInterval)
library(BEST)
library(rjags)
library(BRugs)
library(BayesianFirstAid)
library(Barnard)
library(Exact)
library(coda)


# load necessary helper functions
source("ptall_generalfuncs.r")
source("ptall_generalfuncs_Bayes_binomial-prop-test.r")
source("ptall_generalfuncs_Bayes_Beta_determine.r")
source("ptII_quan_Bayes_Fisher_LadyBristol-BUGS_helpfuncs.r")


# Lady Bristol's tea taste

# does the order matter?
# >>> YES, she got all eight right
# >>> NO, in case if got x successes from n right

# binomial 4 of 8 right
1/choose(8,4)
# 1/70


# frequentist solution to the problem

# min 4 max 7 cups of tea to get 8 of 8 right
# if there is 4:4 distribution of milk before/after tea
for(i in 1:8)
{
 if(i == 1) cat("\nLady Bristol exact binomial probability (one-sided test)\n\n")
 cat(i,"of 8 chosen properly with p = ",binom.test(x=i,n=8,p=0.5, alternative="greater")$p.value,"\n")
}

# data table
ladybristol.8x8 <- matrix(c(4,0,0,4), nrow=2, dimnames=list(trial=c("milk", "tea"), real=c("milk", "tea")))
ladybristol.6x8 <- matrix(c(3,1,1,3), nrow=2, dimnames=list(trial=c("milk", "tea"), real=c("milk", "tea")))

binom.test(x=8, n=8, p=0.5, alternative="two.sided")$p.value
binom.test(x=6, n=8, p=0.5, alternative="two.sided")$p.value

binom.test(x=6, n=8, p=0.5, alternative="greater")$p.value
binom.test(x=8, n=8, p=0.5, alternative="greater")$p.value

fisher.test(ladybristol.8x8, alternative="greater")$p.value
fisher.test(ladybristol.6x8, alternative="greater")$p.value


# chisq.test(ladybristol0)

# odds ratio for 2x2 table of counts (contingency table)
OR.2x2 <- function(tab)
{
 nom <- prod(tab[1,1], tab[2,2])
 denom <- prod(tab[1,2], tab[2,1])
return(nom/denom)
} 

ladybristol.8x8
ladybristol.6x8

OR.2x2(ladybristol.8x8)
OR.2x2(ladybristol.6x8)



# further readings


# max Bayes Factor






# Bayes binomial test





# Bayes solution - no mcmc, but analytical solution
#



# likelihood - data generation process
# content hypotheses != prior knowledge

# both is about the way the data are generated and looking at the data how likeli
# they are to occur under each hypothesis


# case 1
# Fisher is very skeptical that Lady Bristol can do it with p = 0.5 (!p = 1-0.5 = 0.5)
# he thinks she is guessing
# Muriel herself is rather confident that she can do it with p = 0.8 (!p = 1-0.8 = 0.2)
# not run
#P.Fisher.hypo <- 0.5
#P.Muriel.hypo <- 0.8

# case 2
# Fisher is less skeptical that Lady Bristol can do it with p = 0.6 (!p = 1-0.6 = 0.4)
# Muriel is highly confident that she can do it with p = 0.9 (!p = 1-0.9 = 0.1)
P.Fisher.hypo <- 0.6
P.Muriel.hypo <- 0.9

# IF AND ONLY IF THERE ARE NO OTHER INFOS WE CAN DO THE FOLLOWING
# re-scale prior knowledge to sum up to p = 1
P.Fisher.priorH1 <- P.Fisher.hypo/(P.Fisher.hypo + P.Muriel.hypo)
# 0.2/(0.2+0.6)
P.Muriel.priorH2 <- P.Muriel.hypo/(P.Fisher.hypo + P.Muriel.hypo)
# 0.6/(0.2+0.6)
P.Fisher.priorH1
P.Muriel.priorH2

# prior
P.Fisher.hypo
P.Muriel.hypo

# OTHERWISE WE CAN ADD PRIOR PROBS TO EACH HYPOTHESIS
# IMPORTANT - THEY HAVE TO SUM UP TO p = 1

# case we assume both hypos are both equal probable
#P.Fisher.priorH1 <- 0.5
#P.Muriel.priorH2 <- 0.5

# case we assume Fisher is more realistic than Lady Bristol
#P.Fisher.priorH1 <- 0.7
#P.Muriel.priorH2 <- 0.3

# case we are confident that Lady Bristol knows what she talks about and Fisher is a jerk
#P.Fisher.priorH1 <- 0.1
#P.Muriel.priorH2 <- 0.9

# prior Odds Ratio, some kind of Bayes Factor = BF_prior
BF.prior.skeptic <- P.Fisher.priorH1/P.Muriel.priorH2
BF.prior.skeptic
1/BF.prior.skeptic

# empirical data

# case 1
# arbitrary values from literature
successes <- 2
ntrials <- 8

# case 2
# values from literature
successes <- 6
ntrials <- 8

# case 3
# true values
successes <- 8
ntrials <- 8

# failures
failures <- ntrials - successes

successes
ntrials
failures

# likelihood under prior hypos = expectations = prior knowledge = whatever...
# likeli for binomial = (n over k) * p^s * q^f = (n over k) * p^s * (1-p)^(n-s)
# (n over k) = constant
const <- choose(ntrials,successes)
# =
# const <- exp(lchoose(ntrials,successes))
# =
# const <- exp(lfactorial(ntrials)-lfactorial(successes)-lfactorial(failures))
const

L.Fisher <- const * P.Fisher.priorH1^successes*(1-P.Fisher.priorH1)^(failures)
L.Muriel <- const * P.Muriel.priorH2^successes*(1-P.Muriel.priorH2)^(failures)
L.Fisher
L.Muriel

# total evidence = total probability = denominator of Bayes Theorem
totalprob.case1 <- (P.Fisher.priorH1*L.Fisher + P.Muriel.priorH2*L.Muriel)
totalprob.case1

# LR = likelihoodratio, another kind of Bayes Factor = BF
LR.FM <- L.Fisher/L.Muriel
LR.MF <- L.Muriel/L.Fisher 
LR.FM
LR.MF

# posterior probs according to Bayes Theorem
post.case1.FisherH1 <- P.Fisher.priorH1 * L.Fisher / totalprob.case1
post.case1.MurielH2 <- P.Muriel.priorH2 * L.Muriel / totalprob.case1
post.case1.FisherH1
post.case1.MurielH2

# posterior Odds
post.case1.FisherH1 / post.case1.MurielH2
post.case1.MurielH2 / post.case1.FisherH1

# update posterior odds ratio by prior believe and likelihood ie. data
# LR combined with prior knowledge beyond a flat prior
BF.posterior.hypoF.vs.hypoM.S <- P.Fisher.priorH1/P.Muriel.priorH2 * LR.FM
BF.posterior.hypoF.vs.hypoM.S
# =
BF.prior.skeptic*LR.FM
1/BF.posterior.hypoF.vs.hypoM.S


# However, with increasing evidence the absolute difference between both observers
# will decrease more and more.



# all in one
ladymuriel.BT(pr1=0.5, pr2=0.7, si=8, Ni=8)
# same prior, different empirical results si/Ni
ladymuriel.BT(pr1=0.5, pr2=0.7, si=6, Ni=8)
#ladymuriel.BT(pr1=0.5, pr2=0.7, si=6, Ni=8, prout=FALSE)
res.ft.BT <- fishertest.BT(pr1=0.5, pr2=0.7, si=6, Ni=8, prout=FALSE)
ladymuriel.BT(pr1=res.ft.BT[["post1"]], pr2=res.ft.BT[["post2"]], si=6, Ni=8)


### not run
# initial values
# requires log scale ... to work properly
pr1 <- 0.5
pr2 <- 0.7
si <- 4
Ni <- 8
#seed <- 123#89997
#set.seed(seed)
seks <- 1:8
# probs for empirical trials
probs <- c(0.05,0.1,0.1,0.1,0.2,0.3,0.1,0.05)
length(probs)
sum(probs)
plot(probs, type="l", pre.plot=grid(), col="darkred", bty="n", xlab="successes")
# create empirical trials
ntrials <- 100
trials <- sample(seks, ntrials, replace=TRUE, prob=probs)
trials
table(trials)
res.mat <- matrix(data=NA, nrow=ntrials, ncol=4)
colnames(res.mat) <- c("pr1","pr2","post1","post2")
for(i in 1:ntrials)
{
  ft.BT.res <- fishertest.BT(pr1=pr1, pr2=pr2, si=trials[i], Ni=Ni, prout=FALSE)
  res.mat[i,] <- as.numeric(unlist(ft.BT.res)[c("prior1","prior2","post1","post2")])
  print(res.mat[i,])
  pr1 <- ft.BT.res$post1
  pr2 <- ft.BT.res$post2
}  
res.mat
### END OF not run


# calculation via beta update
theta <- seq(0,1,0.01)

# data 6 of 8
#si <- 6
#Ni <- 8

# data 8 of 8
si <- 8
Ni <- 8

# Haldane prior
a.prior1 <- 0.5
b.prior1 <- 0.5

# less flat prior
a.prior1.1 <- 1.1
b.prior1.1 <- 1.1

# flat prior
a.prior1.2 <- 1
b.prior1.2 <- 1

# beta update
ab.post1 <- bino.ab.post(a.prior=a.prior1, b.prior=b.prior1, si=si, Ni=Ni)
ab.post1
beta.dens1 <- dbeta(theta, shape1=ab.post1["a"], shape2=ab.post1["b"])

# UM tuder (1996) maxentropy approach
muriel.pbl <- pbl(theta, si=si, Ni=Ni, loga=TRUE, reexp=TRUE)
muriel.pjc <- pjc(theta, si=si, Ni=Ni, loga=TRUE, reexp=TRUE)
muriel.pbl
muriel.pjc
# plot
plot.siNi(theta, beta.dens=beta.dens1, pbl.dens=muriel.pbl, pjc.dens=muriel.pjc, a.prior=a.prior1, b.prior=b.prior1)
# HDIs
hdi.densi(theta=theta, densis=beta.dens1)
hdi.densi(theta=theta, densis=muriel.pbl)
hdi.densi(theta=theta, densis=,muriel.pjc[!is.nan(muriel.pjc)])
# summaries
postbeta1 <- beta.summary(a=ab.post1["a"], b=ab.post1["b"])
t(unlist(postbeta1))
MAP(theta,beta.dens1)
MAP(theta,muriel.pbl)
MAP(theta,muriel.pjc[!is.nan(muriel.pjc)])


# determine prior beforehand
betadet.ab2 <- beta.determine.opt(p=c(0.5,0.99999,0.00001), qua=c(0.5,0.95,0.05), ab.start=NULL, graph=TRUE)
#betadet.ab2
a.prior2 <- betadet.ab2[["res.ab"]]["a"]
b.prior2 <- betadet.ab2[["res.ab"]]["b"]
a.prior2
b.prior2

# beta update
ab.post2 <- bino.ab.post(a.prior=a.prior2, b.prior=b.prior2, si=si, Ni=Ni)
ab.post2
beta.dens2 <- dbeta(theta, shape1=ab.post2["a"], shape2=ab.post2["b"])
# plot
plot.siNi(theta, beta.dens=beta.dens2, pbl.dens=muriel.pbl, pjc.dens=muriel.pjc)

# HDIs
hdi.densi(theta=theta, densis=beta.dens2)
hdi.densi(theta=theta, densis=muriel.pbl)
hdi.densi(theta=theta, densis=,muriel.pjc[!is.nan(muriel.pjc)])
# summaries
postbeta2 <- beta.summary(a=ab.post2["a"], b=ab.post2["b"])
t(unlist(postbeta2))
MAP(theta,beta.dens1)
MAP(theta,muriel.pbl)
MAP(theta,muriel.pjc[!is.nan(muriel.pjc)])

# posterior Odds Ratios
postbeta1
postbeta2
p1 <- postbeta1[["mean"]]
p2 <- postbeta2[["mean"]]
# simple ratio
p1/p2
1-(p1/p2)
# post OR
(p1 * (1-p2)) / (p2 * (1-p1))



# calculation via MCMC solution

# library 'BayesianFirstAid'
teataste.bayes.res <- bayes.binom.test(x=8, n=8, p=0.5)
plot(teataste.bayes.res)
summary(teataste.bayes.res)
# BF
theta.mean <- teataste.bayes.res$stats["theta","mean"]
(1-theta.mean)/theta.mean
theta.mean/(1-theta.mean)

# nonparametric alternative to Fisher exact
# library 'Barnard'

# library 'Exact'
# real data (!)
# 6 out of 8 correct

# binomial test
# model.code(bayes.binom.test(c(39, 25)))

# model code for the Bayesian First Aid alternative to the binomial test
# library(rjags)
# data set
x <- 8 #39
n <- 8 #64
x <- 6 #39
n <- 8 #64

# model in JAGS
model_string <-"model {
  x ~ dbinom(theta, n)
  theta ~ dbeta(1, 1)
  x_pred ~ dbinom(theta, n)
}"

cat(model_string)

# run jags
model <- jags.model(textConnection(model_string), data=list(x=x, n=n), n.chains=3, n.adapt=1000)
posteriors <- coda.samples(model, c("theta", "x_pred"), n.iter=5000)
# posterior
plot(posteriors)
summary(posteriors)  

# library 'HDInterval'
hdi(posteriors)

# library 'BEST'
plotPost(as.vector(posteriors[[1]][,"theta"]), xlab=expression(paste(theta)), ylab="Density")
lines(density(as.vector(posteriors[[1]][,"theta"])), col="darkred", lwd=2, lty=2)

plotAreaInROPE(as.vector(posteriors[[1]][,"theta"]), credMass=0.95, compVal=0.5, maxROPEradius=1)
# You can extract the mcmc samples as a matrix and compare the thetas 
# of the groups. For example, the following shows the median and 95%
# credible interval for the difference between Group 1 and Group 2.
probs <- c(0.025, 0.05, 0.1, 0.5, 0.8, 0.95, 0.975, 0.999)
posteriors.mat <- as.matrix(posteriors)
head(posteriors.mat)
quantile(posteriors.mat[,"theta"], probs)



# bayes.prop.test
# model.code(fit)

# model code for the Bayesian First Aid
# alternative to the test of proportions

# data set
x <- c(83, 90, 129, 70)
n <- c(86, 93, 136, 82)

# model in JAGS
model_string <- "model {
  for(i in 1:length(x)) {
    x[i] ~ dbinom(theta[i], n[i])
    theta[i] ~ dbeta(1, 1)
    x_pred[i] ~ dbinom(theta[i], n[i])
  }
}"

cat(model_string)

# run jags
model <- jags.model(textConnection(model_string), data = list(x = x, n = n), n.chains = 3, n.adapt=1000)
posteriors <- coda.samples(model, c("theta", "x_pred"), n.iter=5000)

# posterior
plot(posteriors)
summary(posteriors)  

hdi(posteriors)
str(posteriors)

head(posteriors[[1]])
plotPost(as.vector(posteriors[[1]][,"theta[1]"]))
plotAreaInROPE(as.vector(posteriors[[1]][,"theta[1]"]), credMass = 0.95, compVal = 0.9, maxROPEradius = 0.15)

# You can extract the mcmc samples as a matrix and compare the thetas 
# of the groups. For example, the following shows the median and 95%
# credible interval for the difference between Group 1 and Group 2.
probs <- c(0.025, 0.5, 0.975)
posteriors.mat <- as.matrix(posteriors)
quantile(posteriors.mat[, "theta[1]"] - posteriors.mat[, "theta[2]"], probs)



# calculation with BUGS

# library 'BRugs'
# help.BRugs(browser = getOption("browser"))

# empirical data

# Lady Bristol gets six out of eight cups right
dats6of8 <- c("
list(n=c(4,4), y=c(3,1))
Initial values for independent probabilities (though gen.inits is sufficient)
list(p=c(0.5, 0.5))
")

# Lady Bristol gets eight out of eight cups right
dats8of8 <- c("
list(n=c(4,4), y=c(4,0))
Initial values for independent probabilities (though gen.inits is sufficient)
list(p=c(0.5, 0.5))
")


# BUGS models - mostly taken from the BUGS book... see website for more models:
# https://www.mrc-bsu.cam.ac.uk/software/bugs/the-bugs-project-the-bugs-book/bugs-book-examples/

# both margins fixed, based on noncentral-hypergeometric distribution
# ~ exact Fisher test

### begin model
# FINAL MODEL!
# based on hypergeometric distribution
model8 <- c("
model {
     for (i in 1:2) {     
     y[i] ~ dhyper(n[1], m1, N, psi)
     }
 
  psi <- (p[1] * (1-p[2])) / (p[2] * (1-p[1]))
  p[2] ~ dunif(0, 1)
  p[1] ~ dunif(0, 1)
  N <- n[1] + n[2]
  
  m1 <- y[1] + y[2]
  y[1] ~ dbin(p[1], n[1])
  y[2] ~ dbin(p[2], n[2])
  post <- step(p[1] - p[2])
 }
")
### end of model


### begin model
# from the book
# independent model uniform prior
model1 <- c("
model {
    for (i in 1:2) {
    y[i] ~ dbin(p[i], n[i])
	p[i] ~ dbeta(1, 1)
 }
 
 post <- step(p[1] - p[2])
}
")
### end of model


### begin model
# independent model Jeffreys prior
model2 <- c("
model {
    for (i in 1:2) {
    y[i] ~ dbin(p[i], n[i])
    p[i] ~ dbeta(0.5, 0.5)
 }
 
 post <- step(p[1] - p[2])
}
")
### end of model


### begin model
# one-parameter priors uniform prior
model3 <- c("
model {
    for (i in 1:2) {
    y[i] ~ dbin(p[i], n[i])
    }
	
	p[2] <- 1 - p[1]
	p[1] ~ dunif(0, 1)
    post <- step(p[1] - p[2])
}
")
### end of model


### begin model
# one-parameter priors Jeffreys prior
model4 <- c("
model {
    for (i in 1:2) {
    y[i] ~ dbin(p[i], n[i])
    }
	
	p[2] <- 1 - p[1]
	p[1] ~ dbeta(0.5, 0.5)
	
 post <- step(p[1] - p[2])
}
")
### end of model


### begin model
# one-parameter priors sceptical prior
model5 <- c("
model {
    for (i in 1:2) {
    y[i] ~ dbin(p[i], n[i])
    }
	
	p[2] <- 1 - p[1]
    p[1] <- theta[pick]
    pick ~ dcat(q[])
    q[1] <- 0.8
    q[2] <- 0.2
    theta[1] <- 0.5
    theta[2] ~ dunif(0.5, 1)
	
    #post <- step(p[1] - p[2])
    #posterior probability that she correctly detects the pouring order
    #at least twice as often as she gets it wrong,
    post <- step(p[1] / p[2] - 2)
}
")
### end of model


### begin model
# Altham's priors to mimic Fisher's exact test
model6 <- c("
model {
    for (i in 1:2) {
    y[i] ~ dbin(p[i], n[i])
    }
	
 p[1] ~ dbeta(0.00001, 1)
 p[2] ~ dbeta(1,0.00001)	
 post <- step(p[1] - p[2])
}
")
### end of model


### begin model
# Dependent priors uniform priors
model7 <- c("
model {
    for (i in 1:2) {
    y[i] ~ dbin(p[i], n[i])
    }
	
 post <- step(p[1] - p[2])
 
 p[1] ~ dbeta(a, b)
 a <- 1
 b <- 1

 #with rho = 6/(1+1+6) = 0.75
 n.corr <- 6
 x ~ dbin(p[1], n.corr)
 a.post <- a + x
 b.post <- b + n.corr - x
 p[2] ~ dbeta(a.post, b.post)

}
")
### end of model


# show data and models
cat(dats6of8)
cat(dats8of8)
cat(model8)

# choose data
dats <- dats8of8
dats <- dats6of8

# choose model
model <- model1	#independent model uniform prior
model <- model2 #independent model Jeffreys prior
model <- model3 #one-parameter priors uniform prior
model <- model4 #one-parameter priors Jeffreys prior
model <- model5 #one-parameter priors sceptical prior
model <- model6 #Altham's priors to mimic Fisher's exact test
model <- model7 #Dependent priors, uniform priors
model <- model8 #

# run model
run.model(model=model, samples=c("post","p","n"), dats=dats)
samplesStats("*")

# run model 5
run.model(model=model5, samples=c("post","p","n","pick","theta"), dats=dats)
samplesStats("*")

# run model 7
run.model(model=model7, samples=c("post","p","n","x","a.post","b.post"), dats=dats)
samplesStats("*")

# run model 8 with 6 of 8
run.model(model=model8, samples=c("post","p","psi"), dats=dats6of8)
samplesStats("*")

# run model 8 with 8 of 8
run.model(model=model8, samples=c("post","p","psi"), dats=dats8of8)
samplesStats("*")

# selective sampling
samplesSample("post")


# better plot only parts of the chain...
# plot the chain
samplesHistory("*", mfrow=c(2,2))
# plot densities
samplesDensity("*", mfrow=c(2,2))
# plot BGR statistics = Brooks-Gelman-Rubin diagnostics
samplesBgr("*", mfrow=c(2,2))
# plot autocorrelations of 1st chain
samplesAutoC("*", 1, mfrow=c(2,2))
# display for sample values
samplesSample("post")
samplesSample("p[1]")
samplesSample("p[2]")
samplesSample("psi")


# altogether
par(oma=c(2,1,1,1), "cex.axis"=1, bty="l", mfrow=c(2,2))
hist(samplesSample("p[1]"), prob=TRUE, border="white", col="darkred", main="", xlab="p[1]")
lines(density(samplesSample("p[1]")), col="orange", lwd=2)
hist(samplesSample("p[2]"), prob=TRUE, border="white", col="darkred", main="", xlab="p[2]")
lines(density(samplesSample("p[2]")), col="orange", lwd=2)
hist(samplesSample("post"), prob=TRUE, border="white", col="darkred", main="", xlab="post")
lines(density(samplesSample("post")), col="orange", lwd=2)
hist(samplesSample("psi"), prob=TRUE, border="white", col="darkred", main="", xlab=expression(paste(psi)))
lines(density(samplesSample("psi")), col="orange", lwd=2)
mtext("MCMC diagnostics", outer=TRUE, line=-2, cex=1.5, side=3)


# single plots for single parameters
plotBgr("post")
plotAutoC("post") 
plotBgr("post")
plotDensity("post")
plotHistory("post")
bugs.out <- samplesHistory("*", plot=FALSE)
str(bugs.out)
mean(bugs.out$post)

# use coda from here on...
# library 'coda'
bugs.out.mcmc <- buildMCMC("*")
str(bugs.out.mcmc)
summary(bugs.out.mcmc) 
gelman.plot(bugs.out.mcmc) 
geweke.plot(bugs.out.mcmc)
plot(bugs.out.mcmc)
hdi(bugs.out.mcmc)
autocorr.plot(bugs.out.mcmc)
crosscorr.plot(bugs.out.mcmc)
effectiveSize(bugs.out.mcmc)
gelman.diag(bugs.out.mcmc)
geweke.diag(bugs.out.mcmc)
heidel.diag(bugs.out.mcmc)
raftery.diag(bugs.out.mcmc)
rejectionRate(bugs.out.mcmc)

str(bugs.out.mcmc)
par(mfrow=c(2,2))
plotPost(bugs.out.mcmc[[1]][,"p[1]"])
plotPost(bugs.out.mcmc[[2]][,"p[2]"])
plotPost(bugs.out.mcmc[[2]][,"post"])
plotPost(bugs.out.mcmc[[2]][,"psi"])

