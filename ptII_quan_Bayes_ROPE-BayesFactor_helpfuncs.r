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
# ptII_quan_Bayes_ROPE-BayesFactor_helpfuncs.r

# location:
# chap. 6 [6.7.4.2]
# ROPE — region of practical equivalenceg

# HELPER FUNCTIONS

# load helper functions
source("ptall_generalfuncs.r")
source("ptall_generalfuncs_Bayes_binomial-prop-test.r")


# show Bayes Factor, precision, prior - likelihood - posterior along s/N and beta posterior
#... just create entries for the variables... that's it (here...)
BF.prec.sim <- function(a=2,b=4,s=7,n=24, theta.null=0.5)
{  
  # BF with poor precision
  # Kruschke p.347
  # p.270
  # requires
  # source("DBDA2E-utilities.R")

  pD <- function(si, Ni, a, b) beta(si+a, Ni-si+b) / beta(a,b)
  pD.log <- function(si, Ni, a, b) exp( lbeta(si+a, Ni-si+b) - lbeta(a,b) )
  pD.null <- function(theta.null, si, Ni) theta.null^si*(1-theta.null)^(Ni-si)
  BF.null <- function(pD.null, pD) unname(pD / pD.null)

  # example Binomial Bayes-Factor and low precision

  # define 50% prob = null value
  # theta.null <- 0.5 
  xaxis <- seq(0,1,length=1000)
  success <- s
  ntrials <- n
 
  # prior (Haldane prior)
  a1b1.prior <- c("a"=a, "b"=b)
  attr(a1b1.prior, "type") <- "prior"
  # likelihood
  a1b1.likeli <- bino.ab.lik(si=success, Ni=ntrials)
  # posterior
  a1b1.post <- bino.ab.post(a1b1.prior["a"], a1b1.prior["b"], success, ntrials)

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

  # OR
  OR <- 1/BF.null.res
  
  # Kruschke DBDA2E-utilities
  HDIofICDF(qbeta, shape1=a1b1.post[["a"]] , shape2=a1b1.post[["b"]])

  # shows how the BF changes according to prior believe near Haldane prior
  y.pD.BF.null <- BF.null(pD.null=pD.null(theta.null, si=success, Ni=ntrials), pD(si=success, Ni=ntrials, a=xaxis, b=xaxis))
  plot(xaxis, y.pD.BF.null ,type="l", col="violetred3", ylab=expression(paste("BF"[0],sep="")), xlab=expression(theta))

  res <- list(
          # initial values
          success=success,
          ntrials=ntrials,
          a=a,
          b=b,
          theta.null=theta.null,
  
          # results
          a1b1.prior=a1b1.prior,
          a1b1.likeli=a1b1.likeli,
          a1b1.post=a1b1.post,
  
          #
          pD.res=pD.res,
          pD.null.res=pD.null.res,
          BF.null.res=BF.null.res,
          OR=OR
          )

return(res)  
}
#call:
#BF.prec.sim()

# Kruschke examples (p.247)
#BF.prec.sim(a=0.01, b=0.01, s=1, n=2)
#BF.prec.sim(a=1, b=1, s=7, n=14)

# new data
#BF.prec.sim(a=2, b=4, s=7, n=24)
#BF.prec.sim(a=0.01, b=0.01, s=1, n=1)
