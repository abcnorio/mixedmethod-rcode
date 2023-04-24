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


# taken from http://www2.stat.duke.edu/~rcs46/lecturesModernBayes/601-module6-markov/metropolisNormal.R


# MCMC - Metropolis Hastings algorithm
# scenario: conjugate Normal-Normal model with a known variance situation.

# abstract R code
# https://m-clark.github.io/bayesian-basics/issues.html
# p.8 https://www.statistics.com/papers/LESSON1_Notes_MCMC.pdf

seed <- 1
set.seed(seed)

# create artifical data
# y <- round(rnorm(n,10,1),2)

# OR USE

# empirical data
# http://www2.stat.duke.edu/~rcs46/lecturesModernBayes/601-module9-metropolis/metropolis.pdf p.10
# https://www.statisticshowto.datasciencecentral.com/metropolis-hastings-algorithm/
# data from Hoff, 2009
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
s2.prop <- 2	  # variance of random draw = candidate = thetaST(ar)


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

#https://stats.stackexchange.com/questions/232824/bayesian-updating-with-conjugate-priors-using-the-closed-form-expressions
#First of all, the formulas are defined in terms of variance, not standard deviations.
#Second, the variance of the posterior is not a variance of your data but variance of estimated parameter μ.
#As you can see from the description ("Normal with known variance σ2"), this is formula for estimating μ when σ2 is known. The prior parameters μ0 and σ20 are parameters of distribution of μ, hence the assumed model is

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

#data from Hoff (xxxx)
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


























################################### NOT RUN BELOW THIS POINT

#http://www2.stat.duke.edu/~rcs46/lecturesModernBayes/601-module9-metropolis/metropolis.pdf
#http://www2.stat.duke.edu/~rcs46/lecturesModernBayes/601-module6-markov/metropolisNormal.R


### Random Walks

#https://assemblingnetwork.wordpress.com/2014/03/24/random-walks-in-r/
#https://www.r-bloggers.com/a-plot-of-250-random-walks/

# Generate k random walks across time {0, 1, ... , T}
T <- 100
k <- 250
initial.value <- 10
GetRandomWalk <- function() {
  # Add a standard normal at each step
  initial.value + c(0, cumsum(rnorm(T)))
}
# Matrix of random walks
values <- replicate(k, GetRandomWalk())
# Create an empty plot
plot(0:T, rep(NA, T + 1), main=sprintf("%s Random Walks", k),
     xlab="time", ylab="value", col="violetred3",
     ylim=10 + 4.5 * c(-1, 1) * sqrt(T))
mtext(sprintf("%s%s} with initial value of %s",
              "Across time {0, 1, ... , ", T, initial.value))
for (i in 1:k) {
  lines(0:T, values[ , i], lwd=0.25, col="violetred3")
}
for (sign in c(-1, 1)) {
  curve(initial.value + sign * 1.96 * sqrt(x), from=0, to=T,
        n=2*T, col="steelblue", lty=2, lwd=2, add=TRUE)
}
legend("topright", "1.96 * sqrt(t)",
       bty="n", lwd=3, lty=2, col="steelblue")


### Gibbs sampliong
cc <- c(10, 27, 17, 7, 1, 5, 6, 15, 9, 18, 16, 5, 7, 19)
rr <- c(0, 0, 0, 0, 0, 0, 2, 1, 5, 5, 4, 2, 2, 3)
uu <- cumsum(cc - rr)
n <- length(cc)
M <- 10000
B <- 1000
N <- numeric(B + M)
w <- matrix(0, nrow=B + M, ncol=n)
a <- b <- 1
m <- 457
N[1] <- m
w[1,] <- 0.02
for(r in 2:(B + M)) {
w[r,] <- rbeta(n, a + cc, b + N[r-1] - cc)
N[r] <- uu[n] + rpois(1, m * prod(1 - w[r,]))
}
N.gibbs <- N[-(1:B)]
hist(N.gibbs, freq=FALSE, xlab="N", col="gray", border="white", main="")
plotPost(N.gibbs)
head(w)
head(N)


#https://theclevermachine.wordpress.com/2012/11/05/mcmc-the-gibbs-sampler/

### Gibbs sampling of a bivariate normal distribution

#http://www.mas.ncl.ac.uk/~ndjw1/teaching/sim/gibbs/gibbs.html
# bivariate normal
# first the "proper way"

rbvn<-function (n, rho) 
{
        x <- rnorm(n, 0, 1)
        y <- rnorm(n, rho * x, sqrt(1 - rho^2))
        cbind(x, y)
}

bvn<-rbvn(10000,0.98)
par(mfrow=c(3,2))
plot(bvn,col=1:10000)
plot(bvn,type="l")
plot(ts(bvn[,1]))
plot(ts(bvn[,2]))
hist(bvn[,1],40)
hist(bvn[,2],40)
par(mfrow=c(1,1))

# now with a gibbs sampler...
gibbs.bivarnorm <-function (n, rho) 
{
        mat <- matrix(ncol = 2, nrow = n)
        x <- 0
        y <- 0
        mat[1, ] <- c(x, y)
        for (i in 2:n) {
                x <- rnorm(1, rho * y, sqrt(1 - rho^2))
                y <- rnorm(1, rho * x, sqrt(1 - rho^2))
                mat[i, ] <- c(x, y)
        }
        mat
}

bvn <- gibbs.bivarnorm(n=10000, rho=0.98)

par(mfrow=c(3,2))
plot(bvn,col=1:10000)
plot(bvn,type="l")
plot(ts(bvn[,1]))
plot(ts(bvn[,2]))
hist(bvn[,1],40)
hist(bvn[,2],40)
par(mfrow=c(1,1))
#http://www.mas.ncl.ac.uk/~ndjw1/teaching/sim/gibbs/gibbs.html
#https://blog.revolutionanalytics.com/2016/02/multivariate_data_with_r.html
library(MASS)
bivar <- kde2d(bvn[,1], bvn[,2])
image(bivar)
contour(bivar, add=TRUE)

library(ellipse)
rho <- cor(bvn)
bivn <- bvn
y_on_x <- lm(bivn[,2] ~ bivn[,1])    # Regression Y ~ X
x_on_y <- lm(bivn[,1] ~ bivn[,2])    # Regression X ~ Y
plot_legend <- c("99% CI green", "95% CI red","90% CI blue",
                 "Y on X black", "X on Y brown")
 
plot(bivn, xlab = "X", ylab = "Y",
     col = "dark blue",
     main = "Bivariate Normal with Confidence Intervals")
lines(ellipse(rho), col="red")       # ellipse() from ellipse package
lines(ellipse(rho, level = .99), col="green")
lines(ellipse(rho, level = .90), col="blue")
abline(y_on_x)
abline(x_on_y, col="brown")
legend(3,1,legend=plot_legend,cex = .5, bty = "n")

# Three dimensional surface
# Basic perspective plot
persp(bivar, phi = 45, theta = 30, shade = .1, border = NA) # from base graphics package
 
# RGL interactive plot
library(rgl)
col2 <- heat.colors(length(bivar$z))[rank(bivar$z)]
persp3d(x=bivar, col = col2)

# threejs Javascript plot
library(threejs)
# Unpack data from kde grid format
x <- bivar$x; y <- bivar$y; z <- bivar$z
# Construct x,y,z coordinates
xx <- rep(x,times=length(y))
yy <- rep(y,each=length(x))
zz <- z; dim(zz) <- NULL
# Set up color range
ra <- ceiling(16 * zz/max(zz))
col <- rainbow(16, 2/3)
# 3D interactive scatter plot
scatterplot3js(x=xx,y=yy,z=zz,size=0.4,color = col[ra],bg="black")

# Draw from multi-t distribution without truncation
library(tmvtnorm)
Sigma <- matrix(c(1, .1, .1, 1), 2)  # Covariance matrix
X1 <- rtmvt(n=1000, mean=rep(0, 2), sigma = Sigma, df=2) # from tmvtnorm package
t.var <- kde2d(X1[,1], X1[,2], n = 50)   # from MASS package
col2 <- heat.colors(length(bivar$z))[rank(bivar$z)]
persp3d(x=t.var, col = col2)


#http://dirk.eddelbuettel.com/blog/2011/07/14/
#https://www.r-bloggers.com/the-simple-gibbs-example-in-julia/
#https://darrenjw.wordpress.com/2010/04/28/mcmc-programming-in-r-python-java-and-c/
#
#The task is to create a Gibbs sampler for the unscaled density
#
# f(x,y) = x x^2 \exp(-xy^2 - y^2 + 2y - 4x)
#
#using the conditional distributions
#
# x|y \sim Gamma(3, y^2 +4)
# y|x \sim Normal(\frac{1}{1+x}, \frac{1}{2(1+x)})

 Rgibbs <- function(N,thin) {
    mat <- matrix(0,ncol=2,nrow=N)
    x <- 0
    y <- 0
    for (i in 1:N) {
        for (j in 1:thin) {
            x <- rgamma(1,3,y*y+4)
            y <- rnorm(1,1/(x+1),1/sqrt(2*(x+1)))
        }
        mat[i,] <- c(x,y)
    }
    mat
}

N <- 11000
thin <- 100
res <- Rgibbs(N=N,thin=thin)[1:(N-thin),]
str(res)

par(mfrow=c(3,2))
hist(res[,1])
hist(res[,2])
plot(res[,1], type="l")
plot(res[,2], type="l")
acf(res[,1])
acf(res[,2])

fun=function(x,y)
{
    x*x*exp(-x*y*y-y*y+2*y-4*x)
}
 
op=par(mfrow=c(2,1))
x=seq(0,4,0.1)
y=seq(-2,4,0.1)
z=outer(x,y,fun)
contour(x,y,z,main="Contours of actual distribution")
require(KernSmooth)
fit=bkde2D(as.matrix(res[,1:2]),c(0.1,0.1))
contour(fit$x1,fit$x2,fit$fhat,main="Contours of empirical distribution")
par(op)


#https://www.r-bloggers.com/visualising-the-metropolis-hastings-algorithm/
#A common problem in the implementation of this algorithm is the selection of σ.
#Efficient mixing (the rate at which the chain converges to the
#target distribution) occurs when σ approximates the standard deviation of the target distribution.
#When we don’t know this value in advance. we can allow σ to adapt based on
#the history of the chain so far. In the above example, σ is simply updated to
#take the value of the standard deviation of some previous points in the chain.
prop_sd=0.1
target_mu=0
target_sd=1
seed=1
iter=5000

track <- NULL
k_X = seed; 

for(i in 1:iter)
{
track<-c(track,k_X)    ## The chain
k_Y = rnorm(1,k_X,prop_sd) ## Candidate point
 
pi_Y = dnorm(k_Y,target_mu,target_sd,log=TRUE)
pi_X = dnorm(k_X,target_mu,target_sd,log=TRUE)
a_X_Y = (pi_Y)-(pi_X)
a_X_Y = a_X_Y
 
if (a_X_Y > 0)
a_X_Y = 0
 
## Accept move with a probability a_X_Y
if (log(runif(1))<=a_X_Y)
{
k_X = k_Y
}
if(i>100)
prop_sd=sd(track[floor(i/2):i])
 
}

hist(track)
acf(track)
plot(track, type="l")


#https://www.r-bloggers.com/hey-i-made-you-some-wiener-processes/
#https://www.r-bloggers.com/the-tightrope-of-the-random-walk/
# A coin toss path.
plot(cumsum(sign(rnorm(1000))), type="l")


#http://www.mas.ncl.ac.uk/~ndjw1/teaching/sim/metrop/metrop.html
# metropolis for N(0,1) based on uniform candidates
norm<-function (n, alpha) 
{
        vec <- vector("numeric", n)
        x <- 0
        vec[1] <- x
        for (i in 2:n) {
                innov <- runif(1, -alpha, alpha)
                can <- x + innov
                aprob <- min(1, dnorm(can)/dnorm(x))
                u <- runif(1)
                if (u < aprob) 
                        x <- can
                vec[i] <- x
        }
        vec
}

#So, innov is a uniform random innovation and can is the candidate point.
#aprob is the acceptance probability. The decision on whether or not to
#accept is then carried out on the basis of whether or not a U(0,1) is
# less than the acceptance probability. We can test this as follows. 
normvec<-norm(10000,1)
par(mfrow=c(2,1))
plot(ts(normvec))
hist(normvec,30)
par(mfrow=c(1,1))
#This shows a well mixing chain and reasonably normal distribution for the values.
#However, this was based on a choice of alpha of 1. Other values of alpha will not
#affect the stationary distribution, but will affect the rate of mixing of the chain. 


#http://www.mas.ncl.ac.uk/~ndjw1/teaching/sim/metrop/indep.html
# metropolis-hastings independence sampler for a
# gamma based on normal candidates with the same mean and variance
gamm<-function (n, a, b) 
{
        mu <- a/b
        sig <- sqrt(a/(b * b))
        vec <- vector("numeric", n)
        x <- a/b
        vec[1] <- x
        for (i in 2:n) {
                can <- rnorm(1, mu, sig)
                aprob <- min(1, (dgamma(can, a, b)/dgamma(x, 
                        a, b))/(dnorm(can, mu, sig)/dnorm(x, 
                        mu, sig)))
                u <- runif(1)
                if (u < aprob) 
                        x <- can
                vec[i] <- x
        }
        vec
}
#So, can is the candidate point and aprob is the acceptance probability.
#The decision on whether or not to accept is then carried out on the basis
#of whether or not a U(0,1) is less than the acceptance probability. We can test this as follows. 

vec<-gamm(10000,2.3,2.7)
par(mfrow=c(2,1))
plot(ts(vec))
hist(vec,30)
par(mfrow=c(1,1))

#https://stephens999.github.io/fiveMinuteStats/MH-examples1.html

#https://www.rdocumentation.org/packages/MCMCpack/versions/1.4-4/topics/MCMCmetrop1R

 
#David Giles
#http://web.uvic.ca/~dgiles/downloads/bayes/index.html
#http://web.uvic.ca/~dgiles/blog/Bayes_Slides_New_8.pdf
 
#http://faculty.washington.edu/eliezg/teaching/StatR503/CourseMaterials/Week9/BayesianMCMC.html#metropolis-hastings:_coin_example
 
#https://stephens999.github.io/fiveMinuteStats/MH-examples1.html

#http://faculty.washington.edu/eliezg/teaching/StatR503/CourseMaterials/Week9/BayesianMCMC.html#metropolis-hastings
 
#http://people.duke.edu/~ccc14/sta-663-2016/16A_MCMC.html

#http://www.sumsar.net/blog/2013/06/three-ways-to-run-bayesian-models-in-r/

 
### MH example binomial
# 
#thetas <- nplinspace(0,1,200)
#prior <- Beta(1,1)
#likeli <- Binomial with p and q=p-1 and size n binom(n, thetas)
#post <- prior.pdf * likeli / (sum(post)/length(thetas))


#https://stephens999.github.io/fiveMinuteStats/gibbs2.html
#https://stephens999.github.io/fiveMinuteStats/shiny_normal_example.html

#https://github.com/road2stat/conjugate-normal-umkv

#https://stephens999.github.io/fiveMinuteStats/bayes_conjugate_normal_mean.html

#http://patricklam.org/teaching/mcmc_print.pdf
#Using a random walk Metropolis algorithm to sample from a
#Gamma(1.7, 4.4) distribution with a Normal jumping distribution
#with standard deviation of 2.
mh.gamma <- function(n.sims, start, burnin, cand.sd, shape, rate) {
     theta.cur <- start
     draws <- c()
     theta.update <- function(theta.cur, shape, rate) {
         theta.can <- rnorm(1, mean = theta.cur, sd = cand.sd)
         accept.prob <- dgamma(theta.can, shape = shape, rate = rate)/dgamma(theta.cur,
             shape = shape, rate = rate)
         if (runif(1) <= accept.prob)
             theta.can
         else theta.cur
     }
     for (i in 1:n.sims) {
         draws[i] <- theta.cur <- theta.update(theta.cur, shape = shape,
             rate = rate)
     }
     return(draws[(burnin + 1):n.sims])
 }
mh.draws <- mh.gamma(10000, start = 1, burnin = 1000, cand.sd = 2,
     shape = 1.7, rate = 4.4)
 
	 
#https://blog.stata.com/2016/11/15/introduction-to-bayesian-statistics-part-2-mcmc-and-the-metropolis-hastings-algorithm/
#Checking convergence of the chain
#The term “convergence” has a different meaning in the context of MCMC than in the context of maximum likelihood. Algorithms used for maximum likelihood estimation iterate until they converge to a maximum. MCMC chains do not iterate until an optimum value is identified. The chain simply iterates until the desired sample size is reached, and then the algorithm stops. The fact that the chain stops running does not indicate that an optimal sample from the posterior distribution has been generated. We must examine the sample to check for problems. We can examine the sample graphically using bayesgraph diagnostics.
 
#https://www.statisticshowto.datasciencecentral.com/metropolis-hastings-algorithm	 

#https://theclevermachine.wordpress.com/tag/metropolis-hastings-sampling/

#https://link.springer.com/article/10.3758/s13423-016-1015-8#App1

#https://www.nicksolomon.me/post/learn-metropolis-hastings-sampling-with-r/
  
#https://sada2013.sciencesconf.org/17197/document

#http://staff.ustc.edu.cn/~zwp/teach/Stat-Comp/Lec8.R

  
### MH binomial logistic model
#https://www4.stat.ncsu.edu/~reich/ABA/code/MH.R

post <- function(Y,beta,pri_mn=0,pri_sd=1){

   prob <- exp(beta)/(1+exp(beta))

   prior <- dnorm(beta,pri_mn,pri_sd)
   like  <- prod(dbinom(Y,1,prob))

return(like*prior)}

set.seed(0820)
 Y<-rbinom(20,1,0.7)

# Compute the posterior on a grid for reference

 beta_grid <- seq(-3,3,length=100)
 dense     <- rep(0,100)
 for(i in 1:100){
   dense[i]<-post(Y,beta_grid[i])
 }


# MCMC set-up

  n.iters <- 10000
  can_sd  <- 0.5

#initial value

  beta      <- 0
  keep_beta <- rep(0,n.iters)

# Go!

  par(ask=TRUE,mfrow=c(1,1))
  for(iter in 1:n.iters){

    # Draw a candidate and compute acceptance ratio:

     can_beta <- rnorm(1,beta,can_sd)
     p1       <- post(Y,can_beta)
     p2       <- post(Y,beta)
     R        <- p1/p2
     R        <- ifelse(R>1,1,R)

    # Plot the candidate:

     if(iter<50){

      plot(beta_grid,dense,type="l",lwd=2,xlab="beta",ylab="f(beta|Y)")
      lines(c(beta,beta),c(0,p2),col=2,lwd=2)
      lines(c(can_beta,can_beta),c(0,p1),col=3,lwd=2)

      leg    <- NULL
      leg[1] <- paste("R = ",round(R,2))
      leg[2] <- paste("Old value = ",round(beta,2))
      leg[3] <- paste("Candidate = ",round(can_beta,2))

      legend("topleft",leg,lty=1,col=1:3,inset=0.05)
     }

    # Make a decision: 

     keep     <- rbinom(1,1,R)==1
     if(keep){
        beta<-can_beta
     }
     keep_beta[iter]<-beta
  }

# Plot the results:

  par(ask=FALSE,mfrow=c(2,2))
  plot(keep_beta,type="l",xlab="MCMC Iteration",ylab="beta",main="Trace plot")
  acf(keep_beta)
  hist(keep_beta,breaks=50,main="Posterior of beta")
  keep_p <- exp(keep_beta)/(1+exp(keep_beta))
  hist(keep_p,breaks=50,main="Posterior of p")
  

  