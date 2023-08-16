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
# ptII_quan_Bayes_JAGS_example-norm.r

# location:
# chap. 6 [6.13.4.3]
# Gibbs-Sampling mit JAGS

# load necessary libraries
library(rjags)
library(BEST)
library(coda)
library(Bolstad)

# load necessary helper functions
source("ptall_generalfuncs.r")


# MCMC via JAGS -> determine mu and s2

# see also


# create artificial data
# n.emp <- 30
# mu <- 10
# y <- round(rnorm(n=n.emp,mean=mu,sd=sigma),dig)
# Hoff 2009
y <- c(9.37, 10.18, 9.16, 11.60, 10.33)
y


# define model for JAGS
model_string <- "model{

  # Likelihood
  # inv.var = tau = 1/var
  for(i in 1:n){
    Y[i] ~ dnorm(mu,tau)
  }

  # Prior for mu
  mu ~ dnorm(mu.prior, tau0)
  tau0 <- 1/sigma2.prior
  
  # Prior for the inverse variance
  tau ~ dgamma(a, b)

  # Compute the variance
  sigma2 <- 1/tau
  
}
"
# end of JAGS model

# check
cat(model_string)

seed <- 1999
set.seed(seed)
dig <- 3

# sample size
n.emp <- length(y)
# empirical summary
list(summary=summary(y),sd=sd(y),var=var(y),fivenum=fivenum2(y))

# prior values
mu.prior <- 5
sigma2.prior <- 10

# dgamma prior parameters for tau
a <- 2
b <- 2
# a <- 0.01
# b <- 0.01

sigma <- 1

# bring model into JAGS
model <- jags.model(textConnection(model_string), n.chains=3,
                    data=list(Y=y, n=n.emp, mu.prior=mu.prior, sigma2.prior=sigma2.prior, a=a, b=b))
# number of burn-ins for 10000 samples
burnin <- 1e3
n.iter <- 1e5
# run model with JAGS
update(model, burnin, n.iter=n.iter, progress.bar="none")

# number of iterations for MCMC samples
iterats <- 1e4
# create samples from posterior
samps <- coda.samples(model, variable.names=c("mu","sigma2","tau"), n.iter=iterats, progress.bar="none")
str(samps)
head(samps[[1]])
tail(samps[[1]])
summary(samps)

# compare with Bolstad package
# normnp(x=y, m.x=mu.prior, s.x=sqrt(sigma2.prior), sigma.x=sigma, mu=mu, plot=FALSE)
# normnp(x=y, m.x=mu.prior, s.x=sqrt(sigma2.prior), sigma.x=sigma, mu=NULL, plot=FALSE)
# use this:
normnp(x=y, m.x=mu.prior, s.x=sqrt(sigma2.prior), sigma.x=NULL, mu=NULL, plot=FALSE)
# =
# normnp(x=y, m.x=mu.prior, s.x=sqrt(sigma2.prior), mu=mu, plot=FALSE)


# various MCMC plots
plot(samps)
autocorr.plot(samps)
# sample size adjusted for autocorrelation
eS <- effectiveSize(samps)
eS
ratio <- eS/ (length(samps)* dim(samps[[1]])[1])
1-ratio
# evolution of Gelman and Rubin's shrink factor as the number of iterations increases
gelman.plot(samps)
gelman.diag(samps)

# comparison see below for Gibbs sampling code
str(samps)
dimnames(samps[[1]])
summary(as.mcmc(samps[[1]][,"mu"]))
summary(as.mcmc(samps[[1]][,"sigma2"]))

# plot posteriors next to each other
par(oma=c(2,1,2,1), "cex.axis"=1, bty="l", mfrow=c(2,2))
plotPost(samps[[1]][,"mu"], xlab=expression(mu))
# from ptII_quan_Bayes_GibbsSampling_example-normdist.r
# create "draw" values from posterior
nsamps <- 1e5
mc.res <- rnorm(n=nsamps, mean=mean(samps[[1]][,"mu"]), sd=mean(samps[[1]][,"sigma2"]))
plotPost(mc.res, xlab=expression(mu))
# sigma2
plotPost(samps[[1]][,"sigma2"], xlab=expression(sigma))
# tau
plotPost(1/samps[[1]][,"sigma2"], xlab=expression(tau))
mtext("MC-Simulation via Gibbs sampling (JAGS)", outer=TRUE, line=-1, cex=1.5, side=3)
mtext(expression(paste("Normal values for ",mu,", ",sigma," and ",tau,sep="")), outer=TRUE, line=-2.5, cex=1, side=3)

