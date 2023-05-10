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
# ptII_quan_Bayes_HDI.r

# location:
# chap. 6 [6.7.4.1]
# Intervallschätzung

# load necessary libs
library(HDInterval)

# load necessary helper functions
source("ptII_quan_Bayes_HDI_helpfuncs.r")


# plot HDIs and CIs (Bayes)

# example how to use hdi()
prob <- 0.87
probs <- c(0.1,0.2,0.8,0.9,0.95)

seed <- 0987
set.seed(seed)

# vector
rn <- rnorm(100,10,2.5)
hdi(rn, credMass=prob)
c(summary(rn),SD=sd(rn),VAR=var(rn))
# symmetrical CI
quantile(rn, probs)

# HDI
hdi(density(rn), credMass=prob)

# function
set.seed(seed)
hdi(qnorm, mean=10, sd=2.5)
data.frame(probs, quantile=qnorm(probs, mean=10, sd=2.5))


# plot various HDIs and CIs (Bayes)
# prob of intervall
prob <- 0.87

set.seed(seed)
# create some data / density
theta <- seq(0,1,0.001)
randist <- rnorm(1e5, 2, 1.23)
plotHDI(dens=randist, prob=prob, densTF=FALSE)


# create some data / density
randist <- rbeta(1e5, shape1=2.5, shape2=8)
plotHDI(dens=randist, prob=prob, densTF=FALSE)


# create some density based on density function
randist <- dbeta(theta, shape1=5, shape2=8)
dens <- list(x=theta, y=randist)
attr(dens, "class") <- "density"
plotHDI(dens=dens, prob=prob, densTF=TRUE)


# create some density based on density function
theta <- seq(0,1,0.000001)
randist <- dbeta(theta, shape1=1, shape2=11)
dens <- list(x=theta, y=randist)
attr(dens, "class") <- "density"
plotHDI(dens=dens, prob=prob, densTF=TRUE, digs=4)


# theoretical quantile
# qbeta(c(0.025,0.975),1,11)
prob <- 0.95 # to reproduce Kruschke's values from 2012-04 blogpost
theta <- seq(0,1,0.001)
randist <- dbeta(theta, shape1=1, shape2=11)
dens <- list(x=theta, y=randist)
attr(dens, "class") <- "density"
plotHDI(dens=dens, prob=prob, quants=qbeta(c(0.025,0.975),1,11), densTF=TRUE, digs=4)

