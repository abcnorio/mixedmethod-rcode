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
# ptII_quan_Bayes_ROPE-BayesFactor.r

# location:
# chap. 6 [6.7.4.2]
# ROPE — region of practical equivalenceg

# load necessary libs
library(BEST)
library(evidence)
library(agridat)
library(lattice)
library(reshape2)
library(coda)

# load helper functions
source("ptall_generalfuncs.r")
source("ptall_generalfuncs_Bayes_binomial.r")
source("ptII_quan_Bayes_ROPE-BayesFactor_helpfuncs.r")


# difference in means of expected effect size d = (0.5-0)/2.5 = 0.2, same variances
# two sample test
seed <- 2745
set.seed(seed)
prob <- 0.87
fq1 <- rnorm(1e3,mean=0,sd=2.5)
fq2 <- rnorm(1e3,mean=0.5,sd=2.5)
# empirical effect size
cohensd(fq1,fq2)
mcmc0 <- BESTmcmc(fq1,fq2)
summary(mcmc0)
plotAll(mcmc0)
mudiff0 <- mcmc0$mu2 - mcmc0$mu1
plotPost(mudiff0, credMass=prob, compVal=0.5, ROPE=c(0.1,0.6), showMode=TRUE, col="grey90", border="white", xlab=expression(theta[2]-theta[1]), ylab="Density")
plotAreaInROPE(mudiff0, credMass=prob, compVal=0.5, maxROPEradius=0.5)


# difference in means example 2 - one sample test
# compared to zero -> effect size d = (10-0)/2.5 = 4
seed <- 2745
set.seed(seed)
n <- 100
prob <- 0.87
mu <- 10
sigma <- 2.5
norms <- rnorm(n=n, mean=mu, sd=sigma)
c(summary(norms),SD=sd(norms),VAR=var(norms))
# comparecrit
comparecrit <- 9
# empirical effect size against comparison value (scalar)
cohensd(s1=norms,s2=comparecrit)
# theory
mu - comparecrit
(mu - comparecrit)/sigma
hdi(norms, credMass=prob)
mcmc1 <- BESTmcmc(norms)
summary(mcmc1)
plotAll(mcmc1)
pairs(mcmc1)

# prob of mu_diff for various values
mean(mcmc1$mu)
mudiff <- mcmc1$mu - comparecrit
mean(mudiff)
mean(mudiff > 0)
mean(mudiff > 0.5)
mean(mudiff > 1)
mean(mudiff > 2)
sek <- seq(0,2.5,0.01)
probs <- vector()
for(i in 1:length(sek)) probs[i] <- mean(mudiff > sek[i])
par(oma=c(2,1,1,1), "cex.axis"=1, bty="l")
plot(sek,probs, bty="n", col="darkred", type="l", pre.plot=grid(), ylab="p", xlab=expression(paste(mu[diff])))
mtext(expression(paste("Probability of different ",mu[diff])), outer=TRUE, line=-2, cex=1.5, side=3)

# plot ROPE and area-in-ROPE
par(oma=c(2,1,3,1), "cex.axis"=1, bty="l", mfrow=c(2,2))
# mcmc difference in means (against zero!, see above)
plotAreaInROPE(mudiff, credMass=prob, compVal=1.2, maxROPEradius=1.5, main=expression(paste(mu[diff])))
# raw data
plotAreaInROPE(norms, credMass=prob, compVal=9, maxROPEradius=8, main="raw")
# posterior
plotPost(mudiff, credMass=prob, compVal=1.2, ROPE=c(0.8,1.4), showMode=TRUE, col="grey90", border="white", xlab=expression(theta[2]-theta[1]), ylab="Density", main=expression(paste(mu[diff])))
lines(density(mudiff), col="violetred3", lwd=2, lty=2)
# posterior + histogram
plotPostPred(mcmc1)
mtext("Area in ROPE", outer=TRUE, line=0, cex=1.5, side=3)


par(oma=c(2,1,3,1), "cex.axis"=1, bty="l", mfrow=c(2,2))
# area too small
plotAreaInROPE(mudiff, credMass=prob, compVal=1.2, maxROPEradius=0.1, main=expression(paste(mu[diff])))
# extend area
plotAreaInROPE(mudiff, credMass=prob, compVal=1.2, maxROPEradius=0.5, main=expression(paste(mu[diff])))
# extend it more
plotAreaInROPE(mudiff, credMass=prob, compVal=1.2, maxROPEradius=0.8, main=expression(paste(mu[diff])))
# extend it more
plotAreaInROPE(mudiff, credMass=prob, compVal=1.2, maxROPEradius=1, main=expression(paste(mu[diff])))
mtext("Area in ROPE - varying radius", outer=TRUE, line=0, cex=1.5, side=3)



# normal with known precision tau
# POST:
# mean was estimated from observations with total precision (sum of all individual precisions) τ 0 {\displaystyle \tau _{0}} \tau _{0} and with sample mean μ 0 {\displaystyle \mu _{0}} \mu _{0}
seed <- 2745
set.seed(seed)
samp.n <- 100
samp <- rnorm(n=samp.n, mean=108, sd=15)
reps <- 4
ndraws <- 1e+4
mu.prior <- 100
s2.prior <- 10^2
tau.prior <- 1/s2.prior
tau.samp <- 1/var(samp)
mu.post <- ( tau.prior*mu.prior + tau.samp*sum(samp) ) / (tau.prior + samp.n*tau.samp)
tau.post <- tau.prior + samp.n*tau.samp
s.post <- sqrt(1/tau.post)
theta.post <- matrix(rep(rnorm(ndraws, mu.post, s.post),each=reps), ncol=reps, nrow=ndraws, byrow=FALSE)
str(theta.post)
head(theta.post)
theta.post2 <- matrix(rep(rnorm(ndraws, mu.post, s.post),each=reps), ncol=reps, nrow=ndraws, byrow=FALSE)
head(theta.post2)

# plot HDI + ROPE
par(oma=c(2,1,1,1), "cex.axis"=1, bty="l")
plotPost(theta.post[,1], xlim=c(95,115), credMass=0.87, compVal=106, ROPE=c(98,107), showMode=TRUE, col="grey90", border="white", xlab=expression(theta), ylab="Density")
lines(density(theta.post[,1]), col="violetred3", lwd=2, lty=2)
mtext("Region of Practical Equivalence (ROPE)", outer=TRUE, line=-2, cex=1.5, side=3)

hdi(theta.post)
hdi(theta.post, credMass=0.87)


# Bayes Factor with poor precision
# Kruschke p.347
source("DBDA2E-utilities.R")
# p.270
pD <- function(si, Ni, a, b) beta(si+a, Ni-si+b) / beta(a,b)
pD.log <- function(si, Ni, a, b) exp( lbeta(si+a, Ni-si+b) - lbeta(a,b) )
pD.null <- function(theta.null, si, Ni) theta.null^si*(1-theta.null)^(Ni-si)
BF.null <- function(pD.null, pD)
{
 BF <- pD / pD.null
 names(BF) <- ""
return(BF) 
} 

# example Binomial Bayes-Factor and low precision

# define 50% prob = null value
theta.null <- 0.5 
xaxis <- seq(0,1,length=1000)


# [0]
# Kruschke p.347
# left picture
# data
success <- 1
ntrials <- 2
# prior (Haldane prior)
a1b1.prior <- list("a"=0.01, "b"=0.01)
# likelihood
a1b1.likeli <- bino.ab.lik(si=success, Ni=ntrials)
# posterior
a1b1.post <- bino.ab.post(a1b1.prior[["a"]], a1b1.prior[["b"]], success, ntrials)
# results
a1b1.prior 
a1b1.likeli
a1b1.post
# plots
par(oma=c(2,1,3,1), "cex.axis"=1, bty="l", mfrow=c(2,2))
plot(xaxis, dbeta(xaxis, a1b1.prior[["a"]], a1b1.prior[["b"]]), xlab="Prior (Beta)", ylab="Density", type="l", col="violetred3", main="")
plot(xaxis, dbeta(xaxis, a1b1.likeli[["a"]], a1b1.likeli[["b"]]), xlab="Likelihood (Bernoulli)", ylab="Density", type="l", col="violetred3", main="")
plot(xaxis, dbeta(xaxis, a1b1.post[["a"]], a1b1.post[["b"]]), xlab="Posterior (Beta)", ylab="Density", type="l", col="violetred3", main="")
mtext("Prior - Likelihood - Posterior", outer=TRUE, line=-0.5, cex=1.5, side=3)
mtext(paste("a = ",a1b1.prior[["a"]]," | b = ",a1b1.prior[["b"]]," | successes = ",success," | trials = ",ntrials,sep=""), outer=TRUE, line=-2, cex=1, side=3)


# alternate hypothesis
pD.res <- pD(si=success, Ni=ntrials, a=a1b1.prior[["a"]], b=a1b1.prior[["b"]])
pD.null.res <- pD.null(theta.null, si=success, Ni=ntrials)
BF.null.res <- BF.null(pD.null=pD.null.res, pD=pD.res)
pD.res
pD.null.res
BF.null.res
# OR
1/BF.null.res
# classic
binom.test(success, ntrials, p=theta.null)
# Kruschke DBDA2E-utilities
HDIofICDF(qbeta, shape1=a1b1.post[["a"]] , shape2=a1b1.post[["b"]])

# shows how the BF changes according to prior believe near Haldane prior
y.pD.BF.null <- BF.null(pD.null=pD.null(theta.null, si=success, Ni=ntrials), pD(si=success, Ni=ntrials, a=xaxis, b=xaxis))
plot(xaxis, y.pD.BF.null ,type="l", col="violetred3", ylab=expression(paste("BF"[0],sep="")), xlab=expression(theta))


# different values for alternate hypothesis(a,b) + prior belief -> test against theta.null
as <- c(2,1,0.1,0.01,0.001)
bes <- c(4,1,0.1,0.01,0.001)
ab <- data.frame(a=as,b=bes)
print(ab)
for(i in 1:dim(ab)[1])
{
  cat("\na = ",ab[i,"a"],"\nb = ",ab[i,"b"],"\nBF[0] = ",
  BF.null(pD.null=pD.null(theta.null, si=success, Ni=ntrials), pD=pD(si=success, Ni=ntrials, a=ab[i,"a"], b=ab[i,"b"])),"\n")
}


# [1]
# Kruschke p.347
# right picture
# data
success <- 7
ntrials <- 14
# prior (Haldane prior)
# a1b1.prior <- c("a"=0.01, "b"=0.01)
a1b1.prior <- c("a"=1, "b"=1)
attr(a1b1.prior, "type") <- "prior"
# likelihood
a1b1.likeli <- bino.ab.lik(si=success, Ni=ntrials)
# posterior
a1b1.post <- bino.ab.post(a1b1.prior["a"], a1b1.prior["b"], success, ntrials)
# results
a1b1.prior 
a1b1.likeli
a1b1.post
# plots
par(oma=c(2,1,3,1), "cex.axis"=1, bty="l", mfrow=c(2,2))
plot(xaxis, dbeta(xaxis, a1b1.prior[["a"]], a1b1.prior[["b"]]), xlab="Prior (Beta)", ylab="Density", type="l", col="violetred3", main="")
plot(xaxis, dbeta(xaxis, a1b1.likeli[["a"]], a1b1.likeli[["b"]]), xlab="Likelihood (Bernoulli)", ylab="Density", type="l", col="violetred3", main="")
plot(xaxis, dbeta(xaxis, a1b1.post[["a"]], a1b1.post[["b"]]), xlab="Posterior (Beta)", ylab="Density", type="l", col="violetred3", main="")
mtext("Prior - Likelihood - Posterior", outer=TRUE, line=-0.5, cex=1.5, side=3)
mtext(paste("a = ",a1b1.prior[["a"]]," | b = ",a1b1.prior[["b"]]," | successes = ",success," | trials = ",ntrials,sep=""), outer=TRUE, line=-2, cex=1, side=3)

pD.res <- pD(si=success, Ni=ntrials, a=a1b1.prior[["a"]], b=a1b1.prior[["b"]])
pD.null.res <- pD.null(theta.null, si=success, Ni=ntrials)
BF.null.res <- BF.null(pD.null=pD.null.res, pD=pD.res)
pD.res
pD.null.res
BF.null.res
# OR
1/BF.null.res
# Kruschke DBDA2E-utilities
HDIofICDF(qbeta, shape1=a1b1.post[["a"]] , shape2=a1b1.post[["b"]])

# shows how the BF changes according to prior believe near Haldane prior
y.pD.BF.null <- BF.null(pD.null=pD.null(theta.null, si=success, Ni=ntrials), pD(si=success, Ni=ntrials, a=xaxis, b=xaxis))
plot(xaxis, y.pD.BF.null ,type="l", col="violetred3", ylab=expression(paste("BF"[0],sep="")), xlab=expression(theta))


# [2]
# new data
success <- 7
ntrials <- 24
# prior (no more Haldane prior)
a1b1.prior <- c("a"=2, "b"=4)
attr(a1b1.prior, "type") <- "prior"
# likelihood
a1b1.likeli <- bino.ab.lik(si=success, Ni=ntrials)
# posterior
a1b1.post <- bino.ab.post(a1b1.prior["a"], a1b1.prior["b"], success, ntrials)
# results
a1b1.prior 
a1b1.likeli
a1b1.post
# plots
par(oma=c(2,1,3,1), "cex.axis"=1, bty="l", mfrow=c(2,2))
plot(xaxis, dbeta(xaxis, a1b1.prior[["a"]], a1b1.prior[["b"]]), xlab="Prior (Beta)", ylab="Density", type="l", col="violetred3", main="")
plot(xaxis, dbeta(xaxis, a1b1.likeli[["a"]], a1b1.likeli[["b"]]), xlab="Likelihood (Bernoulli)", ylab="Density", type="l", col="violetred3", main="")
plot(xaxis, dbeta(xaxis, a1b1.post[["a"]], a1b1.post[["b"]]), xlab="Posterior (Beta)", ylab="Density", type="l", col="violetred3", main="")
mtext("Prior - Likelihood - Posterior", outer=TRUE, line=-0.5, cex=1.5, side=3)
mtext(paste("a = ",a1b1.prior[["a"]]," | b = ",a1b1.prior[["b"]]," | successes = ",success," | trials = ",ntrials,sep=""), outer=TRUE, line=-2, cex=1, side=3)

pD.res <- pD(si=success, Ni=ntrials, a=1, b=1)
pD.null.res <- pD.null(theta.null, si=success, Ni=ntrials)
BF.null.res <- BF.null(pD.null=pD.null.res, pD=pD.res)
pD.res
pD.null.res
BF.null.res
# OR
1/BF.null.res
# Kruschke DBDA2E-utilities
HDIofICDF(qbeta, shape1=a1b1.post[["a"]] , shape2=a1b1.post[["b"]])

# shows how the BF changes according to prior believe near Haldane prior
y.pD.BF.null <- BF.null(pD.null=pD.null(theta.null, si=success, Ni=ntrials), pD(si=success, Ni=ntrials, a=xaxis, b=xaxis))
plot(xaxis, y.pD.BF.null ,type="l", col="violetred3", ylab=expression(paste("BF"[0],sep="")), xlab=expression(theta))


# Kruschke examples (p.247)
BF.prec.sim(a=0.01, b=0.01, s=1, n=2)
BF.prec.sim(a=1, b=1, s=7, n=14)

# new data
BF.prec.sim(a=2, b=4, s=7, n=24)
BF.prec.sim(a=2, b=4, s=1, n=1)
BF.prec.sim(a=0.01, b=0.01, s=1, n=1)


# ROPE
# library(BEST)

# library(agridat)
?darwin.maize
darwin.maize
head(darwin.maize)
do.call("rbind",with(darwin.maize, tapply(height, type, function(x) c(summary(x),SD=sd(x),VAR=var(x),fivenum2(x)))))

# from ?darwin.maize
dat <- darwin.maize
# Compare self-pollination with cross-pollination
# library 'lattice'
bwplot(height~type, dat, main="darwin.maize")
# library(reshape2)
dm <- melt(dat)
d2 <- dcast(dm, pot+pair~type)
d2
d2$diff <- d2$cross-d2$self
t.test(d2$diff)
dm
d2
cross <- subset(darwin.maize, type=="cross", select=height)
self <- subset(darwin.maize, type=="self", select=height)
differ <- cross - self
t.test(differ)
# identical
t.test(cross[,1], self[,1], paired=TRUE, var.equal=FALSE)

# plot the data
par(oma=c(2,1,1,1), "cex.axis"=1, bty="l", mfrow=c(2,2))
plot(1:dim(differ)[1],differ[,1], pch=21, bty="n", pre.plot=grid(), bg="yellow", col="steelblue", cex=2, xlab="pairs", ylab="difference in heights", xlim=c(1,15), ylim=c(-10,12), main="")
abline(h=0, col="darkred", lty=2)

plot(1:dim(differ)[1],differ[,1], type="n", bty="n", pre.plot=grid(), col="darkred", cex=2, xlab="pairs", ylab="difference in heights", xlim=c(1,15), ylim=c(-10,12), main="")
abline(h=0, col="darkred", lty=2)
text(1:dim(differ)[1],differ[,1], labels=d2$pair)

plot(1:dim(differ)[1],differ[,1], type="n", bty="n", pre.plot=grid(), col="darkred", cex=2, xlab="pot", ylab="difference in heights", xlim=c(1,15), ylim=c(-10,12), main="")
abline(h=0, col="darkred", lty=2)
text(1:dim(differ)[1],differ[,1], labels=d2$pot)

darwin.bp <- boxplot(dat$height ~ dat$type, plot=FALSE)
bxp(darwin.bp, notch=TRUE, frame=FALSE, xlab="pairs", ylab="heights", pch=4, boxfill=c("yellow","orange"), medcol="white", border=c("seagreen","darkred"), horizontal=TRUE, outline=TRUE, lwd=2)
rug(dat$height, col="steelblue")

mtext("Darwin's plant data from 1876", outer=TRUE, line=-2, cex=1.5, side=3)


bwplot(height ~ type, data=darwin.maize)


# Bayesian t-test
darwin.mcmc <- BESTmcmc(d2$cross, d2$self)
darwin.mcmc
summary(darwin.mcmc)
plot(darwin.mcmc)
plot(darwin.mcmc, credMass=0.87, compVal=2, ROPE=c(-3,2), showMode=TRUE, col="grey90", border="white")
pairs(darwin.mcmc)
plotAll(darwin.mcmc)

mean(d2$cross)-mean(d2$self)
(mean(d2$cross)-mean(d2$self)) / sd(d2$cross)
cohensd(d2$self, d2$cross, sd.theory=sd(d2$cross))

# that's our real interest
mudiff.darwin <- darwin.mcmc$mu1 - darwin.mcmc$mu2
mean(mudiff.darwin)
mean(mudiff.darwin > 0)
mean(mudiff.darwin > 0.5)
mean(mudiff.darwin > 1)
mean(mudiff.darwin > 5)
sek <- seq(0,6,0.01)
probs <- vector()
for(i in 1:length(sek)) probs[i] <- mean(mudiff.darwin > sek[i])
probscrit.tab <- data.frame(p=probs,crit=sek)
# have a look
head(probscrit.tab)
tail(probscrit.tab)
# plot
par(oma=c(2,1,1,1), "cex.axis"=1, bty="l")
plot(sek,probs, bty="n", col="darkred", type="l", pre.plot=grid(), ylab="p", xlab=expression(paste(mu[diff])))
mtext(expression(paste("Probability of different ",mu[diff])), outer=TRUE, line=-2, cex=1.5, side=3)

# plot ROPE and area-in-ROPE
compvalue <- 2.5
par(oma=c(2,1,3,1), "cex.axis"=1, bty="l", mfrow=c(2,2))
# mcmc difference in means (against zero!, see above)
plotAreaInROPE(mudiff.darwin, credMass=prob, compVal=compvalue, maxROPEradius=3, main=expression(paste(mu[diff])))
# raw data
plotAreaInROPE(d2$cross-d2$self, credMass=prob, compVal=compvalue, maxROPEradius=10, main="raw")

plotPost(mudiff.darwin, credMass=prob, compVal=compvalue, ROPE=c(1.4,3), showMode=TRUE, col="grey90", border="white", xlab=expression(theta[2]-theta[1]), ylab="Density", main=expression(paste(mu[diff])))
lines(density(mudiff.darwin), col="violetred3", lwd=2, lty=2)

plotPostPred(mcmc1)
mtext("Area in ROPE", outer=TRUE, line=0, cex=1.5, side=3)


# MCMC diagnostics
# library(coda)
darwin.post <- as.mcmc(darwin.mcmc)
coda:::plot.mcmc(darwin.post, col=c("violetred3","yellowgreen"), bty="n")

mean(darwin.post > 2)
1/mean(darwin.post > 2)

mean(darwin.post > 5)
1/mean(darwin.post > 5)

mean(darwin.post > 50)
1/mean(darwin.post > 50)


# posterior Odds Ratio for comparison values of 'differences'
compvs <- -2:50
datframe <- data.frame(compv=compvs,OR_post=sapply(compvs, function(i) mean(darwin.post > i)/(1-mean(darwin.post > i))))
datframe
head(datframe)
tail(datframe)
xlab <- expression(paste(theta[1]-theta[2]))
ylab <- expression(paste("Odds Ratio"[darwin.post]))
ylab1 <- expression(paste("log(Odds Ratio "[darwin.post],")"))
par(oma=c(2,1,2,1), "cex.axis"=1, bty="l", mfrow=c(2,2))
plot(datframe[-c(1:3),], type="l",col="darkred",bty="n",pre.plot=grid(), xlab=xlab, ylab=ylab)
plot(datframe[-c(1:3),1],log(datframe[-c(1:3),2]), type="l",col="darkred",bty="n",pre.plot=grid(),ylab=ylab1, xlab=xlab)
plot(datframe[-c(1:4),], type="l",col="darkred",bty="n",pre.plot=grid(),ylab=ylab, xlab=xlab)
plot(datframe[-c(1:5),], type="l",col="darkred",bty="n",pre.plot=grid(),ylab=ylab, xlab=xlab)
mtext("Posterior Odds Ratios", outer=TRUE, line=-1, cex=1.5, side=3)
mtext("for various differences in means", outer=TRUE, line=-2.5, cex=1, side=3)


mean(darwin.post > 1)/(1-mean(darwin.post > 1))
1/mean(darwin.post > 1)


# test one-sided: 0 < d < 2
1-mean(mudiff.darwin > 2)

# some more ROPEs and comparison values plotted
plot(darwin.mcmc, credMass=0.87, compVal=2, ROPE=c(0,2), showMode=TRUE, col="grey90", border="white")
plot(darwin.mcmc, credMass=0.95, compVal=2, ROPE=c(0,2), showMode=TRUE, col="grey90", border="white")
plot(darwin.mcmc, credMass=0.95, compVal=3, ROPE=c(0,3), showMode=TRUE, col="grey90", border="white")

par(oma=c(2,1,1,1), "cex.axis"=1, bty="l")
plotAreaInROPE(darwin.mcmc, credMass=0.87, compVal=4, maxROPEradius=55)
mtext("Area in ROPE", outer=TRUE, line=-2, cex=1.5, side=3)

par(oma=c(2,1,1,1), "cex.axis"=1, bty="l")
plotAreaInROPE(mudiff.darwin, credMass=0.87, compVal=2, maxROPEradius=3.5)
mtext("Area in ROPE", outer=TRUE, line=-2, cex=1.5, side=3)


### different data set

help.search("darwin")
# library(evidence)
data(darwin)
?darwin
# Darwin, C.R. 1876. The effects of cross and self fertilisation in the vegetable kingdom. John Murray, London.
darwin

# from R-package 'evidence'
# Bayesian analysis of a normal sample with SIR priors
?B1Nsir
# from manpage of 'B1Nsir'
B1Nsir(darwin$difference)
# used brms
B1Nmean(darwin$difference, hists=TRUE, pdf=TRUE)

par(mfrow=c(1,2))
plot(log(darwin[,1]+abs(max(darwin[,1]))), pch=20, col="steelblue", cex=2, pre.plot=grid(), bty="n")
plot(darwin[,1], pch=20, col="steelblue", cex=2, pre.plot=grid(), bty="n", ylab="difference")
abline(h=0, lty=3)
lines(lowess(darwin[,1]), col="darkred", lty=2, lwd=2)
x <- 1:15
abline(darwin.lm <- lm(darwin[,1] ~ x), col="orange")
summary(darwin.lm)

par(mfrow=c(2,2))
plot(darwin.lm)

par(mfrow=c(1,1))
qqnorm(darwin[,1])
qqline(darwin[,1])

# log model but to be estimated ie. >= log(0)
bestout <- BESTmcmc(log(darwin[,1]+abs(max(darwin[,1]))))
bestout1 <- BESTmcmc(darwin[,1])

bestout
summary(bestout)
bestout1
summary(bestout1)

plotAll(bestout)
plotAll(bestout1)

plot(bestout, credMass=0.87, compVal=10, ROPE=c(-10,25), showMode=TRUE, col="grey90", border="white")
pairs(bestout)

plot(bestout1, credMass=0.87, compVal=10, ROPE=c(-10,25), showMode=TRUE, col="grey90", border="white")
pairs(bestout1)

t.test(darwin[,1])

