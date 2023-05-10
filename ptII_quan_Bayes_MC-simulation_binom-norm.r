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
# ptII_quan_Bayes_MC-simulation_binom-norm.r

# location:
# chap. 6 [6.13]
# Marko Chain Monte Carlo Simulationen — MCMC

# load necessary libs
library(BEST)
library(HDInterval)


# MC-Simulation
# binomial distribution
set.seed(223)
N <- 1e+6
p <- 0.5
size <- 20
rb <- rbinom(n=N, size=size, p=p)
db1 <- density(rb)
sek <- 1:size
db2 <- dbinom(x=sek,size=size,p=p)
fac <- 1.12
ylim <- range(c(db1$y, db2))*c(1,fac)
xlim <- c(0,size)
rb.dens <- density(rb)
ylim <- c(0,max(rb.dens$y))
ylim

par(oma=c(2,1,1,1), "cex.axis"=1, bty="l")
hist(rb, ylim=ylim, xlim=xlim, prob=TRUE, border="white", col="skyblue",ylab="Density", pre.plot=grid(),
     xlab="size", main="")
lines(sek,db2, col="darkred", lty=2, lwd=2, type="h")
lines(sek,db2, col="darkred", lty=2, lwd=2, type="l")
lines(rb.dens, col="blue", lty=2, lwd=2, type="h")
mtext("Monte Carlo Simulation of a binomial density", outer=TRUE, line=-1.4, cex=1.5, side=3)
mtext(eval(substitute(expression(paste("N = ",N," | p = ",p," | size = ",size)),
      list(N=N, p=p, size=size))),
  	  outer=TRUE, line=-3.3, cex=1, side=3)
	  

# not scaled
# hist(rb, xlim=xlim, prob=TRUE, border="white", col="skyblue",ylab="Density", pre.plot=grid(),
#      xlab="size", main=paste("Binom (N=",N,", p=",p,", size=",size,")",sep=""))
# lines(db1, col="blue", lty=1, lwd=2)
# lines(db1, col="green", lty=2, lwd=2, type="h")

plot(db2, col="darkred", lty=2, lwd=2, type="h")

mean(rb)
hdi(db1)
hdi(db2)



# MC-simulation
# normal distribution
set.seed(32232)
mu <- 100
sigma <- 10
N <- 1e+3
sek <- seq(mu-sigma*3,mu+sigma*3,0.01)
rn <- rnorm(n=N, mean=mu, sd=sigma)
db1 <- density(rn)
db2 <- dnorm(x=sek, mean=mu, sd=sigma)
fac <- 1.12
ylim <- range(c(db1$y, db2))*c(1,fac)
xlim <- range(sek)
par(oma=c(2,1,1,1), "cex.axis"=1, bty="l")
hist(rn, ylim=ylim, xlim=xlim, prob=TRUE, border="white", col="skyblue",ylab="Density", pre.plot=grid(),
     xlab="Quantile", main="")
lines(db1, col="blue", lty=1, lwd=2)
lines(sek, db2, col="violetred3", lty=2, lwd=2, type="l")
rug(rn, col="steelblue")
mtext("Monte Carlo Simulation of a normal density", outer=TRUE, line=-1.4, cex=1.5, side=3)
mtext(eval(substitute(expression(paste("N = ",N," | ",mu," = ",muu," | ",sigma," = ",sigmaa)),
      list(N=N, muu=mu, sigmaa=sigma))),
	  outer=TRUE, line=-3.3, cex=1, side=3)
par(fig=c(0,1,0,1), oma=c(1,0,0,0), mar=c(0,0,0,0), new=TRUE)
plot(1, type="n", bty="n", xaxt="n", yaxt="n")
legend("bottom", legend=c("normal","simulation"), lty=c(2,1), lwd=2, xpd=TRUE, horiz=TRUE, col=c("violetred3","blue"), bty="n", cex=.9)

