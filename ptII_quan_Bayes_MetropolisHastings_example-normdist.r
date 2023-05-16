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
# ptII_quan_Bayes_MetropolisHastings_example-normdist.r

# location:
# chap. 6 [6.13.4.1]
# Der Metropolis-Hastings Algorithmus im R

# load necessary libraries
library(BEST)
library(coda)
library(HDInterval)
library(car)

# load functions
source("ptall_generalfuncs.r")
source("ptII_quan_Bayes_MH-Gibbs_example_helpfuncs.r")


# MCMC - Metropolis Hastings algorithm
# scenario: conjugate Normal-Normal model with a known variance situation.

# values taken from http://www2.stat.duke.edu/~rcs46/lecturesModernBayes/601-module6-markov/metropolisNormal.R

# see abstract R code


seed <- 1
set.seed(seed)

# create artifical data
# y <- round(rnorm(n,10,1),2)

# OR USE

# empirical data

# slide 11, data from Hoff, 2009
y <- c(9.37, 10.18, 9.16, 11.60, 10.33)
summary(y)
# analytical solution
n <- length(y)
s2 <- 1       # variance likelihood
t2 <- 10      # variance prior tau^2
mu <- 5       # mu prior
n <- 5        # sample size
s2.prop <- 2	# variance of random draw = candidate = thetaST(ar)

# number simulations
nsim <- 1e+5     
# initial value of theta (= parameter of interest)
theta0 <- 0      
    
# seed
seed <- 1889
# run MH
mat <- MH_norm(y=y, s2.prop=s2.prop, s2=s2, t2=t2, mu=mu, theta0=theta0, nsim=nsim, seed=seed)
head(mat)
tail(mat)
theta.post <- mat[,"theta"]

# summary
list(summary=summary(theta.post),sd=sd(theta.post),var=var(theta.post),fivenum=fivenum2(theta.post))

# plot posterior
# library(BEST)
compVal <- 10
par(oma=c(2,1,2,1), "cex.axis"=1, bty="l")
plotPost(theta.post, credMass=0.87, compVal=compVal, ROPE=c(9.5,11.4), xlim=c(8,12), xlab=expression(theta))
lines(density(theta.post), col="darkred", lwd=2, lty=2)
mtext("MC-Simulation via Metropolis Hastings", outer=TRUE, line=-1, cex=1.5, side=3)
mtext(expression(paste("Normal values for unknown parameter ",theta,"",sep="")), outer=TRUE, line=-2.5, cex=1, side=3)


# or use Darwin's data
y <- agridat::darwin.maize[,"height"]
y
# summary
as.data.frame(t(c(N=length(y),summary(y),SD=sd(y),VAR=var(y),fivenum2(y))))
# analytical solution
n <- length(y)
mu <- 20        # mu prior
# n <- 30       # sample size
s2 <- sd(y)     # variance likelihood
t2 <- 1/1e2     # variance prior tau^2
s2.prop <- 2	# variance of random draw = candidate = thetaST(ar)


# normal distribution is a conjugate prior
# calculate theoretical true posterior parameters

# mean of the normal posterior
mu.n <- ( mean(y)*n/s2 + mu/t2 ) / ( n/s2+1/t2) 
# precision of the normal posterior 
t2.n <- (n/s2+1/t2)
t2.n
# mu posterior
mu.n		    
# variance posterior
s2.n <- 1/t2.n	
s2.n
# sd posterior
sqrt(s2.n)


# number simulations
nsim <- 1e+5     
# initial value of theta (= parameter of interest)
theta0 <- 20
s2
t2
s2.prop

# seed
seed <- 1889
# run MH
mat <- MH_norm(y=y, s2.prop=s2.prop, s2=s2, t2=t2, mu=mu, theta0=theta0, nsim=nsim, seed=seed)
theta.post <- mat[,"theta"]

head(mat)
tail(mat)

# in case of theta0 <- 0 -> remove first few estimations from theta.post
theta.post.noburnin <- theta.post[-c(1:500)]

# summary
list(summary=summary(theta.post),sd=sd(theta.post),var=var(theta.post),fivenum=fivenum2(theta.post))

# plot posterior
compVal <- 20
par(oma=c(2,1,2,1), "cex.axis"=1, bty="l")
plotPost(theta.post, credMass=0.87, compVal=compVal, ROPE=c(119.6,20.2), xlim=c(19.5,20.2), xlab=expression(theta))
lines(density(theta.post), col="darkred", lwd=2, lty=2)
mtext("MC-Simulation via Metropolis Hastings", outer=TRUE, line=-1, cex=1.5, side=3)
mtext(expression(paste("Normal values for unknown parameter ",theta,"",sep="")), outer=TRUE, line=-2.5, cex=1, side=3)

# MCMC chain diagnostics
# library(coda)
theta.mcmc <- as.mcmc(theta.post)
coda:::plot.mcmc(theta.mcmc, col="darkred")
par(mfrow=c(2,2))
densplot(theta.mcmc, col="darkred", bty="n")
traceplot(theta.mcmc, bty="n", col="darkred")
#autocorr.plot(theta.mcmc)
acf(theta.mcmc)
summary(as.mcmc(theta.post))
# library(HDInterval)
hdi(theta.post, credMass=0.87)


# compare with sequence without burn-ins
# no real difference
coda:::plot.mcmc(as.mcmc(theta.post.noburnin), col="darkred")
list(summary=summary(theta.post.noburnin),sd=sd(theta.post.noburnin),var=var(theta.post.noburnin),fivenum=fivenum2(theta.post.noburnin))


mat.red <- mat[-1,]
# acceptance rate
acc.r <- sum(mat.red[,"acceptance"])/nsim
# rejection rate
rej.r <- 1-acc.r
acc.r
rej.r

# acceptance versus rejection rate
acc.r/rej.r
1-(acc.r/rej.r)
acc.r.cs <- cumsum(mat.red[,"acceptance"])
head(acc.r.cs)
tail(acc.r.cs)


# plot acceptance rate
par(oma=c(2,1,2,1), "cex.axis"=1, bty="l", mfrow=c(1,2))
plot(acc.r.cs, type="l", xlab="draw", ylab="acceptance rate (cumsum)", col="violetred3", pre.plot=grid(), bty="n")
# looks like a straight line, zoom in:
plot(acc.r.cs[1:100], type="l", xlab="draw", ylab="acceptance rate (cumsum)", col="violetred3", pre.plot=grid(), bty="n")
mtext("Metropolis Hastings MCMC", outer=TRUE, line=-1, cex=1.5, side=3)
mtext(expression(paste("Cumulated acceptance rates (zoom in)",sep="")), outer=TRUE, line=-2.5, cex=1, side=3)


# plot thetaST(ar) (= proposed candidate) vs. theta (=chosen candidate)
par(oma=c(2,1,2,1), "cex.axis"=1, bty="l")
plot(mat[,"thetaST"],mat[,"theta"], pch=21, cex=1, bg="gray10", col="violetred3", bty="n",
     pre.plot=grid(), ylab=expression(theta), xlab=eval(substitute(expression(paste(theta,"*",sep="")))),
  	 main="", cex.lab=1.2)

#mtext("Metropolis Hastings MCMC", outer=TRUE, line=-1, cex=1.5, side=3)
#mtext(expression(paste(theta,"* (= candidate) versus ",theta," (= chosen)",sep="")), outer=TRUE, line=-2.5, cex=1, side=3)

# just an outtake
START <- 1
END <- 100

#START <- 500
#END <- 900

#START <- 1
#END <- 900

plot(mat[,"thetaST"][START:END],mat[,"theta"][START:END], type="b", col="darkred", pre.plot=grid(), bty="n",
     ylab=expression(theta), xlab=eval(substitute(expression(paste(theta,"*",sep="")))),
     main="", cex.lab=1.2)

mtext("Metropolis Hastings MCMC", outer=TRUE, line=-1, cex=1.5, side=3)
mtext(eval(substitute(expression(paste(theta,"* (= candidate) versus ",theta," (= chosen) for full and ",START," to ",END," steps",sep="")),list(START=START,END=END))), outer=TRUE, line=-2.5, cex=1, side=3)



# not run
# more MCMC diagnostics
# only with minimum two chains...

# does not work, requires at least two mcmc chains...
gelman.plot(theta.mcmc)
BayesianFirstAid:::plotPost(theta.post)
densplot(theta.mcmc, col="darkred")
traceplot(theta.mcmc, col="darkred")
cumuplot(theta.mcmc[1:1000], col="violetred3", bty="n", pre.plot=grid())
crosscorr.plot(theta.mcmc)
autocorr.plot(theta.mcmc, col="violetred3", bty="n", pre.plot=grid())
acf(theta.mcmc, col="violetred3", bty="n", pre.plot=grid())





########################################



### NOT RUN Metropolis example

############################# FUNCTION
#normal posterior predictive distribution
norm.pred.post <- function(n, mu.post, sigma2.post, sigma2.data)
{
 return( rnorm(n, mu.post, sqrt(sigma2.post+sigma2.data)) )
}
############################# END OF FUNCTION

############################# FUNCTION
#MH sample for normal distribution
MH.sampler <- function(daten=NA, nsim=1e+4, theta0=0, delta=2, seed=1, type="MH",
                       thresh=100, hist.cand.sigma=FALSE, loga=TRUE)
 {
  set.seed(seed)
  
  mu.prior <- daten[["mu.prior"]]
  sigma2.prior <- daten[["sigma2.prior"]]
  y <- daten[["y"]]
  sigma2.data <- daten[["sigma2.data"]]
  
  cnam <- c("no","curr","cand","cand.sigma","aprob","comp","theta")
  outv <- matrix(data=NA, nrow=nsim, ncol=length(cnam))
  colnames(outv) <- cnam
  outv[1,"no"] <- 1
  outv[1,"theta"] <- theta0
  head(outv)

  cand.sigma <- sqrt(delta)
  
#MH part
  for(i in 2:nsim)
  {
#current
   current <- outv[i-1,"theta"]
#candidate
   candidate <- rnorm(1, current, cand.sigma)
   prior.cand <- dnorm(candidate, mu.prior, sqrt(sigma2.prior), log=loga)
   likeli.cand <- sum(dnorm(y,candidate,sqrt(sigma2.data), log=loga))
   prior.curr <- dnorm(current, mu.prior, sqrt(sigma2.prior), log=loga)
   likeli.curr <- sum(dnorm(y,current,sqrt(sigma2.data), log=loga))
   if(loga)
   {
    aratio.log <- (prior.cand+likeli.cand) - (prior.curr+likeli.curr)
   } else
   {
    aratio <- (prior.cand*likeli.cand)	/ (prior.curr*likeli.curr)
    aratio.log <- log(aratio)
   }   
   if(type == "MH")
   {
    compare <- runif(1, 0, 1)
    theta <- ifelse( log(compare) <= aratio.log, candidate, current )
   } else stop()
   outv[i,] <- c(i, current, candidate, cand.sigma, aratio.log, compare, theta)
   if(hist.cand.sigma)
   {
    if(i > thresh)
    {
    cand.sigma <- sd(outv[floor(i/2):i,"theta"])
	} else
    {
     cand.sigma <- cand.sigma + rnorm(1,0,1)
	}
   }	   
  }
return(outv)
}
#MH.sampler(daten=daten)
############################# END OF FUNCTION

#Metropolis Hastings example

#data from Hoff (2009)
y <- c(9.37,10.18,9.16,11.60,10.33)
mu.prior <- 5
sigma2.prior <- 10
sigma2.data <- 1
nsim <- 1e3
#use conjugate possibilities of normal distribution prior -> posterior
post.vars <- mu.sigma2.post(y=y, mu.prior=mu.prior, sigma2.prior=sigma2.prior, sigma2.data=sigma2.data)
daten <- list(mu.prior=mu.prior, sigma2.prior=sigma2.prior, y=y, sigma2.data=sigma2.data,
              mu.post=post.vars[1], sigma2.post=post.vars[2])

#use MH sampler to create two data sets
#same specs, so should be identical!
MH.res <- MH.sampler(daten=daten, nsim=nsim, type="MH")
MH.res1 <- MH.sampler(daten=daten, nsim=nsim, type="MH", hist.cand.sigma=TRUE)

#summary statistics
summary(mcmc1 <- as.mcmc(MH.res[,"theta"]))
summary(mcmc2 <- as.mcmc(MH.res1[,"theta"]))

#create reduced dataset without burnins
rem <- round(nsim*0.1,0)
if(rem >= nsim) stop(paste("remove (=",rem,") >= nsim (=",nsim,")",sep=""))
theta1 <- MH.res[,"theta"][-c(1:rem)]
theta2 <- MH.res1[,"theta"][-c(1:rem)]

#scatterplot
par(oma=c(2,1,1,1), "cex.axis"=1, bty="l")
plot(theta1,theta2, col="black", bty="n", pre.plot=grid(), pch=21, bg="skyblue", xlab=expression(theta[1]), ylab=expression(theta[2]), cex.lab=1.2)

#correlation?
cor(theta1,theta2)


#plot both densities next to each other
theta1.dens <- density(theta1)
theta2.dens <- density(theta2)
fac <- 1.05
ylim <- c(0,max(theta1.dens$y,theta2.dens$y)*fac)
par(oma=c(2,1,1,1), "cex.axis"=1, bty="l")
hist(theta1, ylim=ylim, prob=TRUE, border="white", col="skyblue", pre.plot=grid(), ylab="Density", xlab=expression(theta), cex.lab=1.2, main="")
lines(theta1.dens, col="orange", lwd=2)
lines(theta2.dens, col="darkred", lwd=2)


#calculate theta1 - theta2 from posterior
differ <- NA
sek <- seq(-2.5,2.5,0.01)
for(i in 1:length(sek))
{
 differ[i] <- mean(theta1 - theta2 > sek[i])
} 
differ
#plot
par(oma=c(2,1,1,1), "cex.axis"=1, bty="l")
plot(sek,differ, pre.plot=grid(), col="darkred", bty="n", type="l",xlab="critical value",
     ylab=expression(paste("probability (",bar(theta[1]),"-",bar(theta[2])," > crit.)", sep="")), cex.lab=1.2)
mtext("Probabilities of differences across range of values", outer=TRUE, line=-2, cex=1.5, side=3)


mean(theta1-theta2)
summary(theta1)
summary(theta2)
sd(theta1)
sd(theta2)
fivenum2(theta1)
fivenum2(theta2)
1-fivenum(theta1)/fivenum(theta2)

differ
differ <- theta1-theta2
differ.dens <- density(differ)
ylim <- c(0,max(differ.dens$y)*fac)

#effect size
cohensd(theta1,theta2, sd.theory=sd(theta1))
(mean(theta1) - mean(theta2))/sd(theta1)

#plot differences of theta1 - theta2
plotPost(differ, credMass=0.87, ROPE=c(-0.1,0.1), compVal=0.1, showMode=TRUE, xlab=expression(delta))

#different plot, same: theta1 - theta2
par(oma=c(2,1,1,1), "cex.axis"=1, bty="l")
hist(differ, ylim=ylim, prob=TRUE, border="white", col="skyblue", pre.plot=grid(), xlab=expression(paste(theta[1]," - ",theta[2],sep="")), cex.lab=1.2)
lines(differ.dens, type="l", bty="n", col="violetred3", lwd=2, lty=2)
lines(hdi(differ.dens),c(0,0), lwd=5, col="orange")


#posterior predictive + plot
fac <- 1.2
seed <- 0987651234
set.seed(seed)
n <- 1e4

#posterior
post.dist <- rnorm(n=n, mean=daten[["mu.post"]][,1], sd=daten[["sigma2.post"]][,1])
post.dist.dens <- density(post.dist)

#predictive posterior
ppc.dist <- norm.pred.post(n=n, mu.post=daten[["mu.post"]][,1], sigma2.post=daten[["sigma2.post"]][,1], sigma2.data=sigma2.data)
ppc.dist.dens <- density(ppc.dist)

#density empirical data
y
y.dens <- density(y)

#ylim <- c(0,max(post.dist.dens$y)*fac)
ylim <- c(0, max(post.dist.dens$y, ppc.dist.dens$y, y.dens$y)*fac)

#plot
par(oma=c(2,1,1,1), par("cex.axis"=0.8))
#posterior
hist(post.dist, ylim=ylim, prob=TRUE, border="white", col="darkorange", bty="n", pre.plot=grid(), xlab=expression(theta), main="", cex.lab=1.2)
lines(post.dist.dens, col="darkred", lwd=2)
#predictive posterior
lines(ppc.dist.dens, col="steelblue", lwd=2, lty=2)
#empirical data
lines(yd$x,yd$y, col="yellowgreen", lwd=2, lty=1)
mtext("Histogram and density plots", outer=TRUE, line=-1.5, cex=1.5, side=3)
mtext("empirical data, posterior, and predictive posterior", outer=TRUE, line=-3, cex=1, side=3)
#legend
par(fig=c(0,1,0,1), oma=c(1,0,0,0), mar=c(0,0,0,0), new=TRUE)
plot(1, type="n", bty="n", xaxt="n", yaxt="n")
categs <- c("posterior","predictive posterior","empirical data")
cols <- c("darkred","steelblue","yellowgreen")
legend("bottom", legend=categs, lty=c(1,2,1), lwd=2, xpd=TRUE, horiz=TRUE, col=cols, bty="n", cex=.9)


 
 
#MH mcmc analysis
fac <- 1.05
#library(BEST)
mcmc.res <- MH.res[-c(1:rem),]
#plot various ways
par(oma=c(2,1,2,1), "cex.axis"=1, bty="l", mfrow=c(2,2))
#
BEST:::plotPost(mcmc.res[,"theta"], credMass=0.87, compVal=10, ROPE=c(8,9.5), showMode=TRUE, xlab=expression(theta), main="")
lines(density(mcmc.res[,"theta"]), col="violetred3", lwd=3, lty=2)
#
ylim <- range(mcmc.res[,"theta"])
ylim <- ylim + ylim*(1-fac)*c(1,-1)
ylim
plot(mcmc.res[,"theta"], ylim=ylim, col="violetred3", type="l", bty="n", pre.plot=grid(), ylab=expression(theta), xlab="MCMC iteration")
#lines(mcmc.res[,"cand"], col="blue")
#lines(mcmc.res[,"theta"], col="red")
#
acf(mcmc.res[,"theta"], col="violetred3", bty="n", pre.plot=grid(), main="")
#with(as.data.frame(mcmc.res), plot(curr,cand, col="violetred3", bty="n", pre.plot=grid()))
r.cc <- with(as.data.frame(mcmc.res), cor(curr,cand))
xy.center <- c(median(mcmc.res[,"curr"]), median(mcmc.res[,"cand"]))
xy.center
#
#library(car)
dataEllipse(mcmc.res[,"curr"], mcmc.res[,"cand"], xlab="current", ylab="candidate", bty="n", pch=23,
            fill.alpha=".2", fill=TRUE, bg="steelblue", col="orange", grid=FALSE, pre.plot=grid(), main="",
			levels=c(0.5,0.87,0.99),
			center.pch=21, center.cex=3)
mtext("Metropolis Hastings (normal model estimation)", outer=TRUE, line=-1.3, cex=1.5, side=3)
			
#library(coda)
mcmc.res.MC <- as.mcmc(mcmc.res)[,"theta"]
head(mcmc.res.MC)
summary(mcmc.res.MC)

