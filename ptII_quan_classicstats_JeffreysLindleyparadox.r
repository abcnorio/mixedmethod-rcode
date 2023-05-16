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
# ptII_quan_classicstats_JeffreysLindleyparadox.r

# location:
# chap. 4 [4.6.11.2]
# Jeffreys-Lindley Paradox

# load required libraries
library(Brobdingnag)

# load necessary helper functions
source("ptall_generalfuncs_brob-integral.r")


# NOT RUN EXTERNAL SOURCE

# fair coin uniform priori
# Frank Schmid, NCCI, 2011

# binomial parameter for a fair coin
prob <- 0.5
# tosses
N <- 200

# heads
# = prob of observing
L <- 115

# heads when coin is fair
dbinom(L,N,prob)

# bias of coin is uniformly distributed
integrand <- function(x) (choose(N,L)*x^L*(1-x)^(N-L))
posterior <- integrate(integrand, lower=0, upper=1)$value
# heads when bis is uniformly distributed
posterior

# bayes factor
# BF of H0 (fairness) to H1
BF <- dbinom(L,N,prob)/posterior
BF

# p-value
x <- L:N
# p-value in two-sided test
crit <- 0.05
pv <- sum(2*dbinom(x,N,prob))
pv
pv < crit

# compare with binomial test
binom.test(L,N,p=prob, alternative="two.sided")
# END NOT RUN EXTERNAL SOURCE


# Wikipedia data

# observed data
gb <- c(boys=49581, girls=48870)
gb
# sum
n <- sum(gb)
n
# observed proportions
gb/n

# frequentist solution
# H0: theta = 0.5
# H1: theta != 0.5
theta <- 0.5

# normal approximation
# X ~ N(mu, sigma^2)
# mu = n*p = n*theta
mu <- n * theta
mu
# var = #= n * theta *(1-theta)
sigma2 <- mu*(1-theta)
sigma2

# p-value
# binom.test(n-k,n,theta)
k <- gb["girls"]
k <- gb["boys"]
binom.test(n-k,n,theta)
#
ND.fun <- function(u) 1 / sqrt(2*pi*sigma2) * exp(-((u-mu)^2)/(2*sigma2))
coveredarea <- integrate(ND.fun, lower=k, upper=n)
# two-sided
pv1 <- 2*(coveredarea$value)
pv1
#= normal approximation
pv2 <- pnorm(k,n,sigma2)
pv2
# check
all.equal(pv1,pv2)
abs(pv1-pv2)


# Bayes solution
# prior
# equal for H0 and H1
prior.H0 <- 0.5
prior.H1 <- 0.5

# p(k|H0)
k <- gb["girls"]
k <- gb["boys"]

# p(k|H0)
dbinom(k,n,theta)
# likelihood = Bernoulli/Binomial model
p.k.H0 <- exp(lchoose(n,k) + log(theta)*(k) + log(1-theta)*(n-k))
p.k.H0

# p(k|H1)
n #total
k #boys or girls
p.k.H1 <- exp(lchoose(n,k) + lbeta(k+1,n-k+1))
p.k.H1

# likelihood function with log()
p.k.H1.fun <- function(theta) exp(lchoose(n,k) + log(theta)*(k) + log(1-theta)*(n-k))
integrate(p.k.H1.fun, lower=0, upper=1)
# =
# likelihood function with large number package
p.k.H1.brob.fun1 <- function(theta) brob(lchoose(n,k)) * brob(log(theta)*k) * brob(log(1-theta)*(n-k))
sintegral.brob.parallel(fx=p.k.H1.brob.fun1, sL=0, sH=1, Nsteps=1000)
p.k.H1.brob.fun2 <- function(theta) brob(lchoose(n,k)) * as.brob(theta)^k * as.brob(1-theta)^(n-k)
p.k.H1.brob <- sintegral.brob.parallel(fx=p.k.H1.brob.fun2, sL=0, sH=1, Nsteps=1000)
p.k.H1.brob
exp(p.k.H1.brob)
log(p.k.H1)
as.brob(p.k.H1)

# plot H0 with k=n/2 i.e. p=0.5 against p=k/n
sek <- seq(0.48,0.52,length.out=1000)
p.k.H0.fun <- function(theta, n, k) exp(lchoose(n,k) + log(theta)*(k) + log(1-theta)*(n-k))
p.k.H0.sek <- p.k.H0.fun(sek, n=n, k=n/2)
p.k.H1.sek <- p.k.H1.brob.fun2(sek)
plot(sek,p.k.H0.sek, col="darkred", xlab=expression(theta), ylab="p(k)", bty="n", pre.plot=grid(), type="l", main="Jefferys-Lindley Paradox")
lines(sek, p.k.H1.sek, col="green")
legend("topright", legend=c("p(k|H0) with k=n/2 (p=0.5)","p(k|H1) with k=empirical data"), col=c("darkred","green"), lty=c(1,1), bty="n")

# applying Bayes' Theorem
# p(H0|k)
p.H0.k <- p.k.H0 * prior.H0 / (p.k.H0*prior.H0 + p.k.H1*prior.H1)
# p(H1|k)
p.H1.k <- p.k.H1 * prior.H1 / (p.k.H0*prior.H0 + p.k.H1*prior.H1)
# prob in favor of H0
p.H0.k
# prob in favor of H1
p.H1.k
1-p.H0.k
# odds ratio of H0/H1 in favor of H0
p.H0.k/p.H1.k




