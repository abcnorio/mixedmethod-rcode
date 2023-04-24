# file:
# ptII_quan_Bayes_find-Beta-distribution-shapeparams.r

# location:
# chap. 6 [6.12]
# Die Wahl priorer Verteilungen

# load necessary libs
library(LearnBayes)
library(rriskDistributions)

# load necessary helper functions
source("ptall_generalfuncs_Bayes_Beta_determine.r")


# call original code from website - slow...
# https://a-little-book-of-r-for-bayesian-statistics.readthedocs.io/en/latest/src/bayesianstats.html
quantile1 <- list(p=0.5, x=0.85)    # we believe the median of the prior is 0.85
quantile2 <- list(p=0.99999,x=0.95) # we believe the 99.999th percentile of the prior is 0.95
quantile3 <- list(p=0.00001,x=0.60) # we believe the 0.001st percentile of the prior is 0.60
system.time( a.b.values <- findBeta(quantile1,quantile2,quantile3) )
# system.time( a.b.values <- findBeta(quantile1,quantile2,quantile3) )
# [1] "The best beta prior has a= 52.22 b= 9.52105105105105"
#        User      System verstrichen 
#       23.79        0.00       23.81 
#curve(dbeta(x,52.22,9.52105105105105))


# call and measure time
system.time(a.b.values <- beta.determine(p=c(0.5,0.99999,0.00001), qua=c(0.85,0.95,0.60)))
a.b.values

# call and measure time
system.time(a.b.values.opt <- beta.determine.opt(p=c(0.5,0.99999,0.00001), qua=c(0.85,0.95,0.60), ab.start=NULL, graph=FALSE) )
a.b.values.opt


# with plot of resulting Beta distribution
beta.determine.opt(p=c(0.5,0.99999,0.00001), qua=c(0.85,0.95,0.60), ab.start=NULL, graph=TRUE)

# obly plot
betaoptim.plot(res.ab=a.b.values.opt$res.ab, res.ab3=a.b.values.opt$res.ab3, quans=a.b.values.opt$quans)


# no result for
ab.start=c(100,25)
beta.determine.opt(p=c(0.50,0.9,0.15), qua=c(0.35,0.65,0.15), ab.start=c(100,25), graph=FALSE)
warnings()


# not run
# direct calls to ab.optim() and ab.optim3() - subfunctions in 'beta.determine.opt()'

# linear
# optim(par=c(81,14), ab.optim)
# optim(par=c(100,25), ab.optim)
# optim(par=c(110,20), ab.optim)
# optim(par=c(50,8), ab.optim)

# RMS
# optim(par=c(100,25), ab.optim3)
# end of not run


# library 'rriskDistributions'
get.beta.par(p=c(0.00001,0.5,0.99999), q=c(0.60,0.85,0.95))

beta.determine.opt(p=c(0.00001,0.5,0.99999), qua=c(0.60,0.85,0.95), ab.start=NULL, graph=FALSE)

probs <- c(0.025, 0.5, 0.975)
a <- 2
b <- 3
quantiles <- stats::qbeta(p=probs, shape1=a, shape2=b)
probs
quantiles
bt.opt <- beta.determine.opt(p=probs, qua=quantiles, graph=TRUE)
gbp <- get.beta.par(q=quantiles)
bt.opt
gbp
# compare outputs whether equivalent
for(i in 1:2) cat(paste(c("a","b")[i]," | ",(all.equal(bt.opt$res.ab[i], gbp[i])),sep=""),"\n")

# plot next to each other
thetas <- seq(0,1,0.01)
par(oma=c(2,1,3,1), "cex.axis"=1, bty="l", mfrow=c(1,2))
plot(thetas,dbeta(thetas,shape1=a,shape2=b),type="l",col="darkred",bty="n",pre.plot=grid(),main="",xlab=expression(theta),cex.lab=1.2,ylab="Density")
lines(thetas,dbeta(thetas,shape1=bt.opt$res.ab[1],shape2=bt.opt$res.ab[2]),type="l",col="steelblue",bty="n",pre.plot=grid(),lty=3)
lines(thetas,dbeta(thetas,shape1=gbp[1],shape2=gbp[2]),type="l",col="seagreen",bty="n",pre.plot=grid(),lty=3)

plot(thetas,dbeta(thetas,shape1=a,shape2=b),type="l",col="darkred",bty="n",pre.plot=grid(),main="",xlab=expression(theta),cex.lab=1.2,ylab="Density")
lines(jitter(thetas),jitter( dbeta(thetas,shape1=bt.opt$res.ab[1],shape2=bt.opt$res.ab[2]) ),type="l",col="steelblue",bty="n",pre.plot=grid(),lty=3)
lines(jitter(thetas),jitter( dbeta(thetas,shape1=gbp[1],shape2=gbp[2]) ),type="l",col="seagreen",bty="n",pre.plot=grid(),lty=3)
mtext("Beta shape parameters optmization comparison", outer=TRUE, line=-0.5, cex=1.5, side=3)
mtext(paste("Theory: a = ",a," | b = ",b," | No jitter (left), some jitter (right)",sep=""), outer=TRUE, line=-2.5, cex=1, side=3)




# not run below this point
# ... just to compare with the script from
# http://a-little-book-of-r-for-bayesian-statistics.readthedocs.io/en/latest/src/bayesianstats.html

### original function from website above
findBeta <- function(quantile1,quantile2,quantile3)
{
  # find the quantiles specified by quantile1 and quantile2 and quantile3
  quantile1_p <- quantile1[[1]]; quantile1_q <- quantile1[[2]]
  quantile2_p <- quantile2[[1]]; quantile2_q <- quantile2[[2]]
  quantile3_p <- quantile3[[1]]; quantile3_q <- quantile3[[2]]
  
  # find the beta prior using quantile1 and quantile2
  priorA <- beta.select(quantile1,quantile2)
  priorA_a <- priorA[1]; priorA_b <- priorA[2]
  
  # find the beta prior using quantile1 and quantile3
  priorB <- beta.select(quantile1,quantile3)
  priorB_a <- priorB[1]; priorB_b <- priorB[2]
  
  # find the best possible beta prior
  diff_a <- abs(priorA_a - priorB_a); diff_b <- abs(priorB_b - priorB_b)
  step_a <- diff_a / 100; step_b <- diff_b / 100
  if (priorA_a < priorB_a) { start_a <- priorA_a; end_a <- priorB_a }
  else                     { start_a <- priorB_a; end_a <- priorA_a }
  if (priorA_b < priorB_b) { start_b <- priorA_b; end_b <- priorB_b }
  else                     { start_b <- priorB_b; end_b <- priorA_b }
  steps_a <- seq(from=start_a, to=end_a, length.out=1000)
  steps_b <- seq(from=start_b, to=end_b, length.out=1000)
  max_error <- 10000000000000000000
  best_a <- 0; best_b <- 0
  for (a in steps_a)
  {
    for (b in steps_b)
    {
      # priorC is beta(a,b)
      # find the quantile1_q, quantile2_q, quantile3_q quantiles of priorC:
      priorC_q1 <- qbeta(c(quantile1_p), a, b)
      priorC_q2 <- qbeta(c(quantile2_p), a, b)
      priorC_q3 <- qbeta(c(quantile3_p), a, b)
      priorC_error <- abs(priorC_q1-quantile1_q) +
        abs(priorC_q2-quantile2_q) +
        abs(priorC_q3-quantile3_q)
      if (priorC_error < max_error)
      {
        max_error <- priorC_error; best_a <- a; best_b <- b
      }
    }
  }
  print(paste("The best beta prior has a=",best_a,"b=",best_b))
}
### end of function

probs <- c(0.00001,0.5,0.99999)
quantiles <- c(0.60, 0.85, 0.95)
probs
quantiles
bt.opt <- beta.determine.opt(p=c(0.00001,0.5,0.99999), qua=c(0.60,0.85,0.95), graph=TRUE)
gbp <- get.beta.par(q=quantiles)
bt.opt
gbp
# compare outputs whether equivalent
for(i in 1:2) cat(paste(c("a","b")[i]," | ",(all.equal(bt.opt$res.ab[i], gbp[i])),sep=""),"\n")

bt.opt$res.ab
quantiles1 <- stats::qbeta(p=probs, shape1=52.016710, shape2=9.451692)
probs
quantiles1
beta.determine.opt(p=probs, qua=quantiles1, graph=TRUE)
get.beta.par(q=quantiles)

stats::qbeta(p=probs, shape1=52.22, shape2=9.52105105105105)
stats::qbeta(p=probs, shape1=14.41707, shape2=80.14816)

# but turn around shape1 and shape2
stats::qbeta(p=probs, shape2=14.41707, shape1=80.14816)
stats::qbeta(p=probs, shape1=52.22, shape2=9.52105105105105)

# compare one way turn around...
# looks much better!!! but not identical
stats::qbeta(p=probs, shape2=52.22, shape1=9.52105105105105)
stats::qbeta(p=probs, shape1=14.41707, shape2=80.14816)

