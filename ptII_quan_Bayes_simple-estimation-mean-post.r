# file:
# ptII_quan_Bayes_simple-estimation-mean-post.r
# location:
# chap. 6 [6.5.1]
# Intuitives Verständnis von Wahrscheinlichkeit

# load necessary libs
library(Bolstad)

# load necessary helper functions
source("ptII_quan_Bayes_simple-estimation-mean-post_helpfuncs.r")


# posterior distribution of a mean
# library(Bolstad)
seed <- 3856
set.seed(seed)
N <- 10
MW <- 6.5
SD <- 2
x <- rnorm(N, MW, SD)

# informed prior
PRIOR <- c(6.5,2)
bp.m.res <- normgcp(x, sigma.x=sd(x), density="normal", params=PRIOR, plot=FALSE)#.alt
str(bp.m.res)
plot.mean.post(bp.m.res)
# output
mean(bp.m.res)

# changed informed prior
PRIOR1 <- c(4,3)
bp.m.res1 <- normgcp(x, sigma.x=sd(x), density="normal", params=PRIOR1, plot=FALSE)
mean(bp.m.res1)
str(bp.m.res1)
layout(matrix(1:2,ncol=2), width = c(2,2),height = c(1,1))
plot.mean.post(bp.m.res, xlim=c(0,14), add=TRUE)
plot.mean.post(bp.m.res1, xlim=c(0,14), add=TRUE)
# comparison
bp.m.res1$likelihood == bp.m.res$likelihood
# options("digits"=9)
# output
mean(bp.m.res)
mean(bp.m.res1)
mean(bp.m.res)/mean(bp.m.res1)
# options("digits"=7)


# non-informative prior
bp.m.res2 <- normgcp(x, sigma.x=sd(x), density="uniform", params=NULL, plot=FALSE)
# layout(matrix(1:2,ncol=2), width = c(2,2),height = c(1,1))
# plot.mean.post(bp.m.res, xlim=c(0,14), add=TRUE)
# plot.mean.post(bp.m.res2, xlim=c(0,14), add=TRUE)
mean(bp.m.res)
mean(bp.m.res2)
mean(bp.m.res)/mean(bp.m.res2)
# scaled prior and likelihood
plot.mean.post(bp.m.res2)
# non-scaled prior and likelihood
plot.mean.post(bp.m.res2, scaleprior=FALSE, scalelikeli=FALSE)

# replication
# new data
# prior = posterior of round 1 (=bp.m.res)
PRIOR3 <- c(mean(bp.m.res), sd(bp.m.res))
x2 <- rnorm(N, MW, SD)
bp.m.res3 <- normgcp(x2, sigma.x=sd(x2), density="normal", params=PRIOR3, plot=FALSE)
mean(bp.m.res3)
# layout(matrix(1:2,ncol=2), width = c(2,2),height = c(1,1))
# plot.mean.post(bp.m.res, xlim=c(0,14), add=TRUE)
# plot.mean.post(bp.m.res3, xlim=c(0,14), add=TRUE)
mean(bp.m.res)
mean(bp.m.res3)
mean(bp.m.res)/mean(bp.m.res3)
# scaled prior and likelihood
plot.mean.post(bp.m.res3,add=TRUE)
# non-scaled prior and likelihood
plot.mean.post(bp.m.res3,add=TRUE, scaleprior=FALSE, scalelikeli=FALSE)

