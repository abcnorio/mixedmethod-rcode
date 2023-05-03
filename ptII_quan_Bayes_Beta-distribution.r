# file:
# ptII_quan_Bayes_Beta-distribution.r

# location:
# chap. 6 [6.12]
# Die Wahl priorer Verteilungen

# load necessary libs
library(rriskDistributions)
library(BEST)
library(VGAM)

# load necessary helper functions
# from Kruschke DBDA
source("DBDA2E-utilities.R")
# adopted from Kruschke DBDA
source("ptII_quan_Bayes_case_wordcounts-PPC_helpfuncs.r")
# plot prior-likeli-posterior
source("ptall_generalfuncs_Bayes_binomial.r")


# calculate beta posterior from prior and likelihood
# in two versions (conjugate case and grid approximation)
si <- 10
fi <- 8 # Ni-si
Ni <- si+fi
theta <- seq(0,1,.01)
a <- 1
b <- 1
likelihood <- function(theta,si,fi) return( theta^si * (1-theta)^fi )
prior <- function(theta,a,b) return( dbeta(theta,a,b) )
posterior <- function(theta,a,b,si,fi) return( dbeta(theta, a+si, b+fi) )

# not standardized
post.d0 <- prior(theta,a,b) * likelihood(theta,si,fi)

# standardize posterior
post.d0.st <- post.d0 / (sum(post.d0)/length(theta))
post.d1 <- posterior(theta,a,b,si,fi)

# plot
par(mfrow=c(1,2))
par(oma=c(2,1,1,1), "cex.axis"=1, bty="l")
plot(theta,post.d0.st, type="l",col="darkred",pre.plot=grid(),bty="n", lwd=2, xlab=expression(theta), ylab="Density", cex.lab=1.2)
lines(theta,post.d1,col="orange", lty=2,lwd=2)
mtext("Posterior Beta", outer=TRUE, line=-2, cex=1.5, side=3)
# or do it that way...
#par(oma=c(2,1,1,1), "cex.axis"=1, bty="l")
plot(theta,post.d0*(max(post.d1)/max(post.d0)), type="l",col="darkred",pre.plot=grid(),bty="n", lwd=2, xlab=expression(theta), ylab="Density", cex.lab=1.2)
lines(theta,post.d1,col="orange", lty=2,lwd=2)
mtext("Posterior Beta", outer=TRUE, line=-2, cex=1.5, side=3)
# -> differences hardly to see


# influence of priors
# example success rates
# empirical data
si <- 10
Ni <- 14
prior.vs <- data.frame(theta.prior=c(0.5,0.5,0.4,0.2,0.7),
                       nprior=c(2,5,15,100,30))
prior.vs
# 1- uniform prior
# 2- less heavily informed prior, different than likelihood
# 3- heavily informed prior, different than likelihood
# 4- extreme informed prior, different than likelihood
# 5- heavily informed prior, same as likelihood
par(ask=TRUE)
for(i in 1:dim(prior.vs)[1])
{
  bino.abs.res <- bino.abs(si=si, Ni=Ni, theta.prior=prior.vs[i,"theta.prior"],
                           nprior=prior.vs[i,"nprior"], graph=FALSE)
  v <- bino.abs.res$res
  beta.triplot(si, Ni, v, filling=FALSE)  
} 

# single runs...
# uniform prior
theta.prior <- 0.5
nprior <- 2
bino.abs.res <- bino.abs(si=si, Ni=Ni, theta.prior=theta.prior, nprior=nprior, graph=FALSE)
v <- bino.abs.res$res
beta.triplot(si, Ni, v, filling=FALSE)
# less heavily informed prior, different than likelihood
theta.prior <- 0.5
nprior <- 5
bino.abs.res <- bino.abs(si=si, Ni=Ni, theta.prior=theta.prior, nprior=nprior, graph=FALSE)
v <- bino.abs.res$res
beta.triplot(si, Ni, v, filling=FALSE)
# heavily informed prior, different than likelihood
theta.prior <- 0.4
nprior <- 15
bino.abs.res <- bino.abs(si=si, Ni=Ni, theta.prior=theta.prior, nprior=nprior, graph=FALSE)
v <- bino.abs.res$res
beta.triplot(si, Ni, v, filling=FALSE)
# heavily informed prior, same as likelihood
theta.prior <- 0.7
nprior <- 30
bino.abs.res <- bino.abs(si=si, Ni=Ni, theta.prior=theta.prior, nprior=nprior, graph=FALSE)
v <- bino.abs.res$res
beta.triplot(si, Ni, v, filling=FALSE)

#0.7,30
#0.4,.6


# plot various beta distributions with varying a, base
xaxis <- seq(0,1,length.out=1000)
a <- c(0.001,0.01,0.1,1,5,seq(0.5,5,by=0.25),100)
b <- c(0.001,0.01,0.1,1,5,seq(5,0.5,by=-0.25),100)
ab <- data.frame(a,b)
ab
ab.dim <- dim(ab)
colo <- rainbow(ab.dim[1])
areadim <- ceiling(sqrt(ab.dim[1]))
par(mfrow=c(areadim, areadim), oma=c(1,1,5,1), cex.axis=0.8)
  
for(i in 1:ab.dim[1])
{
 plot(xaxis, dbeta(xaxis, ab[i,"a"], ab[i,"b"]), type="l", col="violetred3", xlab="", ylab="", bty="n",
      main=paste("a = ",ab[i,"a"],", b = ",ab[i,"b"],sep=""), pre.plot=grid())
}
mtext(expression(paste("The various shapes of Beta-distributions",sep="")), 3, line=1.6, cex=1.5, outer=TRUE)


# not run

# general find distribution parameters
# library(rriskDistributions)
# example beta distribution
prior.vals <- get.beta.par(p=c(.05,.5,.95),q=c(.1,.3,.9), plot=FALSE, show.output=FALSE)
# a,b values = shape1, shape2
dig <- 3
prior.vals
xaxis <- seq(0,1,length=1000)
par(oma=c(2,1,1,1), "cex.axis"=1, bty="l")
plot(xaxis, dbeta(xaxis, prior.vals["shape1"], prior.vals["shape2"]), type="l",
     xlab=expression(theta), ylab="Density", col="violetred3", main="", bty="n", pre.plot=grid(), cex.lab=1.2)
mtext(paste("Beta (a = ",round(prior.vals["shape1"],dig)," | b = ",round(prior.vals["shape2"],dig),")",sep=""), outer=TRUE, line=-2, cex=1.5, side=3)

# library 'BEST'
summarizePost2(dbeta(xaxis, prior.vals["shape1"],prior.vals["shape2"]), ROPE=c(1.1,1.3))
# plot
par(oma=c(2,1,1,1), "cex.axis"=1, bty="l")
plotPost(rbeta(1e4,prior.vals["shape1"],prior.vals["shape2"]), ROPE=c(0.3,0.8))
mtext(paste("Beta (a = ",round(prior.vals["shape1"],dig)," | b = ",round(prior.vals["shape2"],dig),")",sep=""), outer=TRUE, line=-2, cex=1.5, side=3)


# library(VGAM)
# expectation
n <- 50
n * prior.vals["shape1"] / (prior.vals["shape1"] + prior.vals["shape2"])
# prob greater than 11 = p(Y >= y) if y=10
1-pbetabinom.ab(10,size=50,shape1=prior.vals["shape1"],shape2=prior.vals["shape2"]) # Pr(Y >= 11) = 1 - Pr(Y <= [<10])
# =
# see in BUGS

