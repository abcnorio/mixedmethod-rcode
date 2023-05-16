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
# ptII_quan_Bayes_posterior.r

# location:
# chap. 6 [6.5.1]
# Intuitives Verständnis von Wahrscheinlichkeit

# load necessary libs
library(BEST)
library(coda)
library(lattice)
library(boot)
library(agridat)

# load necessaray helper functions
source("ptall_generalfuncs.r")



# normal with known precision tau
# POST:
# mean was estimated from observations with total precision (sum of all individual precisions)
set.seed(2745)
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
# repeat with same specs, but without same seed
theta.post2 <- matrix(rep(rnorm(ndraws, mu.post, s.post),each=reps), ncol=reps, nrow=ndraws, byrow=FALSE)
head(theta.post2)

mu.post
tau.post
s.post

# according to Kruschke
# library 'BEST'
BEST:::plotPost(theta.post[,1], xlim=c(95,115), credMass=0.87, compVal=105, ROPE=c(95,105), showMode=TRUE, col="grey90", border="white", xlab=expression(theta), ylab="Density")
lines(density(theta.post[,1]), col="violetred3", lwd=2, lty=2)

hdi(theta.post)
hdi(theta.post, credMass=0.87)

# library(coda)
# combine two MCMC chains lists
mcmc.l <- mcmc.list(as.mcmc(theta.post),as.mcmc(theta.post2))


# no run
# plot(mcmc.l)
par(oma=c(2,1,1,1), "cex.axis"=1, bty="l")
plot(mcmc.l, type="l", col=c("yellowgreen","violetred3"), bty="n", smooth=TRUE, ylab=expression(theta))
mtext("MCMC chains", outer=TRUE, line=-1, cex=1.5, side=3)

summary(as.mcmc(theta.post))
summary(as.mcmc(theta.post2))
summary(mcmc.l)

par(oma=c(2,1,1,1), "cex.axis"=1, bty="l")
gelman.plot(mcmc.l)
mtext("Gelman plot", outer=TRUE, line=-2, cex=1.5, side=3)
# autocorrelation plot
acf(theta.post)
acf(theta.post2)
# cumulative quantile plot
#cumuplot(mcmc.l, type="l", col="darkred", bty="n", ylab=expression(theta))
par(mfrow=c(2,2))
cumuplot(theta.post, type="l", col="darkred", bty="n", ylab=expression(theta),auto.layout=FALSE)
autocorr.plot(theta.post)
autocorr.plot(theta.post2)
autocorr.plot(mcmc.l)

# Geweke's convergence diagnostic
geweke.diag(theta.post)
par(oma=c(2,1,1,1), "cex.axis"=1, bty="l")
geweke.plot(as.mcmc(theta.post))
mtext("Geweke plot", outer=TRUE, line=-1, cex=1.5, side=3)
geweke.diag(mcmc.l)
geweke.plot(mcmc.l)

# Raftery and Lewis's diagnostic
raftery.diag(theta.post, q=0.025, r=0.005, s=0.95)
raftery.diag(mcmc.l, q=0.025, r=0.005, s=0.95)

heidel.diag(theta.post)
heidel.diag(mcmc.l)
autocorr.diag(as.mcmc(theta.post))
autocorr.diag(mcmc.l)
autocorr.plot(theta.post)

crosscorr(mcmc.l)
par(oma=c(2,1,1,1), "cex.axis"=1, bty="l")
crosscorr.plot(mcmc.l)
mtext("Cross-Correlation plot", outer=TRUE, line=-1, cex=1.5, side=3)
effectiveSize(mcmc.l)

# Gelman and Rubin's convergence diagnostic
gelman.diag(mcmc.l, multivariate=FALSE)
gelman.diag(mcmc.l, multivariate=TRUE)

# MCMC chain plots
# library(lattice)
xyplot(mcmc.l[,1:4])
acfplot(mcmc.l[,1:4], lag.max=200) 
lapply(mcmc.l, cor)


# plot posterior various credmass (BEST)
# library(BEST)
hdis <- c(0.99,0.95,0.87,0.67)
rope <- c(102,111)
par(oma=c(2,1,1,1), "cex.axis"=1, bty="l", mfrow=c(2,2))
for(i in hdis)
{
 BEST:::plotPost(theta.post[,1], xlim=c(95,115), credMass=i, compVal=105, ROPE=rope, showMode=TRUE, col="grey90", border="white", xlab=expression(theta), ylab="Density")
 lines(density(theta.post[,1]), col="violetred3", lwd=2, lty=2)
 print(hdi(theta.post, credMass=i))
}
mtext("Plot Posterior (various HDIs, const. ROPE)", outer=TRUE, line=-2, cex=1.5, side=3)



#### Darwin's Plant Height Differences

# ROPE
# library(boot)
?darwin
data(darwin)
plot(log(darwin[,1]+abs(max(darwin[,1]))))
plot(darwin[,1], pch=20, col="steelblue", cex=2, pre.plot=grid(), bty="n")
abline(h=0, lty=3)
lines(lowess(darwin[,1]), col="darkred", lty=2, lwd=2)
x <- 1:15
abline(darwin.lm <- lm(darwin[,1] ~ x), col="orange")
summary(darwin.lm)

par(mfrow=c(2,2))
plot(darwin.lm)

qqnorm(darwin[,1])
qqline(darwin[,1], col="darkred")

bestout <- BESTmcmc(log(darwin[,1]+abs(max(darwin[,1]))))
bestout <- BESTmcmc(darwin[,1])
summary(bestout)
plot(bestout)
plot(bestout, credMass=0.87, compVal=10, ROPE=c(-10,25), showMode=TRUE, col="grey90", border="white")
pairs(bestout)
plotAll(bestout)
t.test(darwin[,1])


# library(agridat)
?darwin.maize
darwin.maize
with(darwin.maize, tapply(height, type, summary))
with(darwin.maize, tapply(height, type, sd))
with(darwin.maize, tapply(height, type, fivenum.wn))

dat <- darwin.maize
# Compare self-pollination with cross-pollination
# library 'lattice'
bwplot(height~type, dat, main="darwin.maize")
require(reshape2)
dm <- melt(dat)
d2 <- dcast(dm, pot+pair~type)
d2$diff <- d2$cross-d2$self
t.test(d2$diff)
dm
d2
cross <- subset(darwin.maize, type=="cross", select=height)
self <- subset(darwin.maize, type=="self", select=height)
differ <- cross - self
t.test(differ)
#identical
t.test(cross[,1], self[,1], paired=TRUE, var.equal=FALSE)

plot(1:dim(differ)[1],differ[,1], pch=20, bty="n", pre.plot=grid(), col="darkred", cex=2, xlab="pairs", ylab="difference in heights", xlim=c(1,15), ylim=c(-10,12), main="Darwin's plant data from 1876")

bwplot(height ~ type, data=darwin.maize)
# does not work because it is a list
# bestout <- BESTmcmc(cross, self)
bestout <- BESTmcmc(d2$cross, d2$self)
summary(bestout)
plot(bestout)
plot(bestout, credMass=0.87, compVal=2, ROPE=c(-3,2), showMode=TRUE, col="grey90", border="white")
pairs(bestout)
plotAll(bestout)
(mean(d2$cross)-mean(d2$self))/sd(d2$cross)

cohensd(d2$cross, d2$self)

library(coda)
posterior <- as.mcmc(bestout)
plot(posterior, col="violetred3", bty="n")
mean(posterior > 2)
1/mean(posterior > 2)

mean(posterior > 5)
1/mean(posterior > 5)

compvs <- -2:10
data.frame(compv=compvs,OR_post=sapply(compvs, function(i) mean(posterior > i)/(1-mean(posterior > i))))

for(i in 0:10) print(mean(posterior > i)/(1-mean(posterior > i)))

mean(posterior > 1)/(1-mean(posterior > 1))
1/mean(posterior > 1)


plot(bestout, credMass=0.87, compVal=2, ROPE=c(0,2), showMode=TRUE, col="grey90", border="white")
plot(bestout, credMass=0.95, compVal=2, ROPE=c(0,2), showMode=TRUE, col="grey90", border="white")
plot(bestout, credMass=0.95, compVal=3, ROPE=c(0,3), showMode=TRUE, col="grey90", border="white")


