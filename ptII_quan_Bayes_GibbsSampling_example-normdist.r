###
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
# ptII_quan_Bayes_GibbsSampling_example-normdist.r

# location:
# chap. 6 [6.13.4.2]
# Der Gibbs Sample im R

# load necessary libraries
library(Bolstad)

# load helper functions
source("ptall_generalfuncs.r")
source("ptII_quan_Bayes_MH-Gibbs_example_helpfuncs.r")


# Gibbs Sampler

# conjugate prior and posterior -> normal distribution

digs <- 3
seed <- 1
set.seed(seed)
dig <- 3


# create artificial data
# y <- round(rnorm(n=n.emp,mean=mu,sd=sigma),dig)
#
# or
#
# A First Course in Bayesian Statistical Methods (Springer Texts in Statistics)
# by Peter D. Hoff (2009-07-14), chap. 10
y <- c(9.37, 10.18, 9.16, 11.60, 10.33)
y
# sample size
n.emp <- length(y)

# empirical summary
list(N=n.emp,summary=summary(y),sd=sd(y),var=var(y))
fivenum.wn(y)

# prior values
mu.prior <- 5
sigma2.prior <- 10
# a <- 0.01
# b <- 0.01
mu <- 10
sigma <- 1
sigma2 <- sigma^2
tau2 <- 1/sigma2

# create posterior values via conjugation from prior and likelihood
# mu.sigma2.res <- mu.sigma2.post(y=c(9.37, 10.18, 9.16, 11.60, 10.33), mu.prior=5, sigma2.prior=10, sigma2.data=1)
mu.sigma2.res <- mu.sigma2.post(y=y, mu.prior=mu.prior, sigma2.prior=sigma2.prior, sigma2.pop=sigma2)
mu.sigma2.res

# create "draw" values from posterior
nsamps <- 1e5
mc.res <- rnorm(n=nsamps, mean=mu.sigma2.res[,"mu.post"], sd=mu.sigma2.res[,"s.post"])

# plot
par(oma=c(2,1,2,1), "cex.axis"=1, bty="l")
BEST:::plotPost(mc.res, credMass=0.87, compVal=10, ROPE=c(9.5,10.5), showMode=TRUE, xlab=expression(mu), ylab="Density", main="")
lines(density(mc.res), lty=2, lwd=2, col="darkred")
mtext("MC-Simulation from posterior", outer=TRUE, line=-1.2, cex=1.5, side=3)
mtext("Normal values derived from analytical posterior", outer=TRUE, line=-2.5, cex=1, side=3)

plotPost(mc.res, credMass=0.87, xlab=expression(theta), ROPE=c(3,9), compVal=10, xlim=c(8,12))


# compare with function from R package 'Bolstad'
# library(Bolstad)
# check and compare results
mu.sigma2.res
list(summary=summary(mc.res),sd=sd(mc.res),var=var(mc.res))
summary(as.mcmc(mc.res))
# compare with Bolstad package
# normnp(x=y, m.x=mu.prior, s.x=sqrt(sigma2.prior), sigma.x=sigma, mu=mu, plot=FALSE)
#
#mu.sigma2.res
normnp(x=y, m.x=mu.prior, s.x=sqrt(sigma2.prior), sigma.x=1, mu=NULL, plot=FALSE)
# normnp(x=c(9.37, 10.18, 9.16, 11.60, 10.33), m.x=5, s.x=sqrt(10), sigma.x=1, mu=NULL, plot=FALSE)



# another example, same procedure, different values

digs <- 3
seed <- 112
set.seed(seed)

# Gibbs sampling for mu and TAU (inverse variance)



# sample statistics
n <- 30
xbar.data <- 15
s2.data   <- 3

n.burnin <-  1e3
sampsize <- 1e4 + n.burnin

# sample from the joint posterior(mu, TAU | data)
mu <- rep(NA, sampsize)
TAU <- rep(NA, sampsize)
# starting value for TAU (= intialization)
TAU[1] <- 1
for(i in 2:sampsize)
{   
  mu[i] <- rnorm(n=1, mean=xbar.data, sd=sqrt(1 / (n*TAU[i-1])))    
  TAU[i] <- rgamma(n=1, shape=n/2, scale=2 / ((n-1) * s2.data + n * (mu[i]-xbar.data)^2))
}

# remove burnin
mu  <- mu[-(1:n.burnin)]
TAU <- TAU[-(1:n.burnin)]
list(summary=summary(mu),sd=sd(mu),var=var(mu))
list(summary=summary(TAU),sd=sd(TAU),var=var(TAU))

# transform to MCMC object to process with functions from 'coda'
coda:::traceplot(as.mcmc(mu), col="darkred")
coda:::traceplot(as.mcmc(TAU), col="purple")

# library(BEST)
par(oma=c(2,1,2,1), "cex.axis"=1, bty="l", mfrow=c(1,2))
plotPost(mu, xlab=expression(mu),)
lines(density(mu), col="darkred", lwd=2, lty=2)
plotPost(TAU, xlab=expression(tau))
lines(density(TAU), col="darkred", lwd=2, lty=2)
mtext("MC-Simulation via Gibbs sampling", outer=TRUE, line=-1, cex=1.5, side=3)
mtext(expression(paste("Normal values for ",mu," (= mean) and ",tau," (= inverse variance)",sep="")), outer=TRUE, line=-2.5, cex=1, side=3)


r1 <- cor(mu,TAU)
r1
# remove burnins (must be more in other cases)
r2 <- cor(mu[-c(1:n.burnin)],TAU[-c(1:n.burnin)])
r2
r1/r2

plot.mcmc(mu, TAU)
plot.mcmc.parts(mu,TAU)

plot.mcmc.parts(mu,TAU, part=1:10)
plot.mcmc.parts(mu,TAU, part=1:20)
plot.mcmc.parts(mu,TAU, part=200:300)
plot.mcmc.parts(mu,TAU, part=1:300)
plot.mcmc.parts(mu,TAU, part=1:nsim)


# another example, same procedure, different values

digs <- 3
seed <- 1
set.seed(seed)

# empirical data
n <- 30
xbar.data <- 10
sigma2.pop <- 1

# create artificial data
y <- round(rnorm(n=n,mean=xbar.data,sd=sigma2.pop),digs)
y
# empirical summary
list(summary=summary(y),sd=sd(y),var=var(y),fivenum=fivenum2(y))

res.post <- mu.sigma2.post(y=y, mu.prior=mu.prior, sigma2.prior=sigma2.prior, sigma2.pop=sigma2.pop)
res.post

daten <- list(mu.prior=res.post[,"mu.prior"], sigma2.prior=res.post[,"sigma2.prior"],
              y=y, sigma2.pop=res.post[,"sigma2.pop"], 
              mu.post=res.post[,"mu.post"], sigma2.post=res.post[,"s2.post"]
             )
daten
nsim <- 1e3
gibbs.res <- gibbs(daten=daten, nsim=nsim)
head(gibbs.res)
tail(gibbs.res)
mus <- gibbs.res[,"mu"]
taus <- gibbs.res[,"tau"]

par(oma=c(2,1,2,1), "cex.axis"=1, bty="l", mfrow=c(1,2))
plotPost(mus, xlab=expression(mu),)
lines(density(mus), col="darkred", lwd=2, lty=2)
plotPost(taus, xlab=expression(tau))
lines(density(taus), col="darkred", lwd=2, lty=2)
mtext("MC-Simulation via Gibbs sampling", outer=TRUE, line=-1, cex=1.5, side=3)
mtext(expression(paste("Normal values for ",mu," (= mean) and ",tau," (= inverse variance)",sep="")), outer=TRUE, line=-2.5, cex=1, side=3)


cor(mus,taus)
# remove first 100 burn in (must be more in other cases)
cor(mus[-c(1:100)],taus[-c(1:100)])

plot(mus,taus, col="violetred3", bty="n", pre.plot=grid())
# thinned (without 1:100)
plot(mus[-c(1:100)],taus[-c(1:100)], col="violetred3", bty="n", pre.plot=grid())


plot.mcmc(mus, taus)
plot.mcmc.parts(mus,taus)

plot.mcmc.parts(mus,taus, part=1:10)
plot.mcmc.parts(mus,taus, part=1:20)
plot.mcmc.parts(mus,taus, part=200:300)
plot.mcmc.parts(mus,taus, part=1:300)
plot.mcmc.parts(mus,taus, part=1:nsim)

summary(as.mcmc(mus))
summary(as.mcmc(taus))


