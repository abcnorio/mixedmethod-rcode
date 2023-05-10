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
# ptII_quan_Bayes_Prior-Likeli-Post_relationship.r

# location:
# chap. 6 [6.12.1]
# Verhältnis Prior — Likelihood — Posterior

# load necessary libs
library(ggplot2)
library(LearnBayes)
library(BEST)

# load necessary helper functions
source("ptall_generalfuncs_Bayes_binomial.r")
source("ptall_generalfuncs_Bayes_Beta_determine.r")


# show the effect and interplay of prior and likelihood on posterior

# with "delay" o the influence of the likelihood on the posterior
# depends on the N

npriors <- c(2,10,20,100,500,5000)

# use this to show how the prior dominates the posterior
si <- 0
Ni <- 2
par(oma=c(2,1,4,1), "cex.axis"=1, bty="l", mfrow=c(3,2))
for(i in npriors) bino.abs(si=si, Ni=Ni, theta.prior=0.5, nprior=i, rn=paste("nprior = ",i,sep=""), graph=TRUE)
mtext("Various Priors and resulting Likelihood + Posterior", outer=TRUE, line=1.7, cex=1.3, side=3)
mtext(paste(paste("si = ",si," | N = ",Ni," | priors = [",sep=""), paste(npriors, collapse=", "),"]",sep=""), outer=TRUE, line=-0.4, cex=1, side=3)

bino.abs.res <- bino.abs(si=si, Ni=Ni, theta.prior=0.5, nprior=i, graph=FALSE)
beta.triplot(si=si, Ni=Ni, v=bino.abs.res$res, multiplot=FALSE)


# use this to show how the prior dominates more and more the likelihood
# the likelihood dominates at the beginning
si <- 7
Ni <- 23
par(oma=c(2,1,4,1), "cex.axis"=1, bty="l", mfrow=c(3,2))
for(i in npriors) bino.abs(si=si, Ni=Ni, theta.prior=0.5, nprior=i, rn=paste("nprior = ",i,sep=""), graph=TRUE)
mtext("Various Priors and resulting Likelihood + Posterior", outer=TRUE, line=1.7, cex=1.3, side=3)
mtext(paste(paste("si = ",si," | N = ",Ni," | priors = [",sep=""), paste(npriors, collapse=", "),"]",sep=""), outer=TRUE, line=-0.4, cex=1, side=3)


# use this to show how the likelihood dominates the posterior
# and slowly the prior becomes influential
si <- 700
Ni <- 1000
par(oma=c(2,1,4,1), "cex.axis"=1, bty="l", mfrow=c(3,2))
for(i in npriors) bino.abs(si=si, Ni=Ni, theta.prior=0.5, nprior=i, rn=paste("nprior = ",i,sep=""), graph=TRUE)
mtext("Various Priors and resulting Likelihood + Posterior", outer=TRUE, line=1.7, cex=1.3, side=3)
mtext(paste(paste("si = ",si," | N = ",Ni," | priors = [",sep=""), paste(npriors, collapse=", "),"]",sep=""), outer=TRUE, line=-0.4, cex=1, side=3)


# manual example
si <- 7
Ni <- 23
par(oma=c(2,1,4,1), "cex.axis"=1, bty="l", mfrow=c(3,2))
npriors <- c(2,10,100,500,5000)
par(oma=c(2,1,4,1), "cex.axis"=1, bty="l", mfrow=c(3,2))
# uniform prior
bino.abs(si=si, Ni=Ni, theta.prior=0.5, nprior=2, rn=paste("nprior = ",i,sep=""), graph=TRUE)
# weakly informed equal prior
bino.abs(si=si, Ni=Ni, theta.prior=0.5, nprior=10, rn=paste("nprior = ",i,sep=""), graph=TRUE)
# strongly informed equal prior
bino.abs(si=si, Ni=Ni, theta.prior=0.5, nprior=100, rn=paste("nprior = ",i,sep=""), graph=TRUE)
# very strongly informed equal prior
bino.abs(si=si, Ni=Ni, theta.prior=0.5, nprior=500, rn=paste("nprior = ",i,sep=""), graph=TRUE)
# very strongly informed equal prior
bino.abs(si=si, Ni=Ni, theta.prior=0.5, nprior=5000, rn=paste("nprior = ",i,sep=""), graph=TRUE)
mtext("Different sample sizes for prior", outer=TRUE, line=1.7, cex=1.3, side=3)
mtext(paste(paste("si = ",si," | N = ",Ni," | npriors = [",sep=""), paste(npriors, collapse=", "),"]",sep=""), outer=TRUE, line=-0.4, cex=1, side=3)

