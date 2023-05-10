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
# ptII_quan_Bayes_regularization.r

# location:
# chap. 6 [6.12]
# Die Wahl priorer Verteilungen


# after McElreath (2015, p. 187)

# different sigmas (normal distribution)
thetas <- seq(50,150,0.01)
dnorm1 <- dnorm(thetas, 100, 10)
dnorm2 <- dnorm(thetas, 100, 5)
dnorm3 <- dnorm(thetas, 100, 2)
par(oma=c(2,1,1,1), "cex.axis"=1, bty="l")
plot(thetas, dnorm1, col="violetred3", type="l", ylim=c(0,0.22), pre.plot=grid(), bty="n", xlab=expression(mu), ylab="Density")
lines(thetas, dnorm2, lty=2, col="darkred")
lines(thetas, dnorm3, lty=3, col="blue")
legend("right", legend=c("10","5","2"), col=c("violetred3","darkred","blue"), lty=1:3, lwd=2,bty="n", horiz=FALSE, title=expression(paste(sigma)))
mtext(expression(paste("Different ",sigma," of the normal distribution",sep="")), outer=TRUE, line=-2, cex=1.5, side=3)


# flat prior
thetas <- seq(-500,500,0.01)
dnorm1 <- dnorm(thetas, 0, 200)
par(oma=c(2,1,1,1), "cex.axis"=1, bty="l", mfrow=c(3,1))
plot(thetas, dnorm1, col="violetred3", type="l", pre.plot=grid(), bty="n", xlab=expression(mu), ylab="Density")
abline(v=c(-3,3),col="blue",lty=2)

plot(thetas, dnorm1, col="violetred3", xli=c(-3,3), type="l", pre.plot=grid(), bty="n", xlab=expression(mu), ylab="Density")
abline(v=c(-3,3),col="blue",lty=2)

plot(thetas, dnorm1, col="violetred3", xli=c(200,203), type="l", pre.plot=grid(), bty="n", xlab=expression(mu), ylab="Density")
mtext(expression(paste("Flat Prior - various perspectives",sep="")), outer=TRUE, line=-2, cex=1.5, side=3)


# percent area below the curve for different N(0,1) sds from 1 to 4
2*(1-pnorm(1:6))
1-pnorm(1:6)
pnorms <- sapply(1:2, function(i) i*(1-pnorm(1:6)))
colnames(pnorms) <- c("one-sided","two-sided")
pnorms


# flat priors - different distributions
fak <- 1.3
thetas <- seq(-500,500,0.01)
dnorm1 <- dnorm(thetas, 0, 200)
dcauchy1 <- dcauchy(thetas, 0, 200)
dunif1 <- dunif(thetas,-500,500)
par(oma=c(2,1,2,1), "cex.axis"=1, bty="l", mfrow=c(3,2))
xlim <- c(-3,3)
ylim.dnorm <- c(0,max(dnorm1))
ylim.dcauchy <- c(0,max(dcauchy1))
ylim.dunif <-  c(0,max(dunif1))
plot(thetas, dnorm1, col="violetred3", ylim=ylim.dnorm*fak, type="l", pre.plot=grid(), bty="n", xlab=expression(mu), ylab="Density normal distribution")
plot(thetas, dnorm1, col="violetred3", ylim=ylim.dnorm*fak, xlim=xlim, type="l", pre.plot=grid(), bty="n", xlab=expression(mu), ylab="Density normal distribution")

plot(thetas, dcauchy1, col="steelblue", ylim=ylim.dcauchy*fak, type="l", pre.plot=grid(), bty="n", xlab=expression(mu), ylab="Density Cauchy distribution")
plot(thetas, dcauchy1, col="steelblue", ylim=ylim.dcauchy*fak, xlim=xlim, type="l", pre.plot=grid(), bty="n", xlab=expression(mu), ylab="Density Cauchy distribution")

plot(thetas, dunif1, col="seagreen", ylim=ylim.dunif*fak, type="l", pre.plot=grid(), bty="n", xlab=expression(mu), ylab="Density uniform distribution")
plot(thetas, dunif1, col="seagreen", ylim=ylim.dunif*fak, xlim=xlim, type="l", pre.plot=grid(), bty="n", xlab=expression(mu), ylab="Density uniform distribution")
mtext(expression(paste("Flat Priors",sep="")), outer=TRUE,line=-1, cex=1.5, side=3)
mtext(expression(paste("Comparison of different distributions",sep="")), outer=TRUE,line=-3, cex=1, side=3)


# everything on one plot
par(oma=c(2,1,2,1), "cex.axis"=1, bty="l", mfrow=c(1,2))
plot(thetas, dnorm1, col="violetred3", ylim=c(0,max(ylim.dnorm,ylim.dcauchy,ylim.dunif))*fak, type="l", pre.plot=grid(), bty="n", xlab=expression(mu), ylab="Density")
lines(thetas, dcauchy1, lty=2, col="steelblue")
lines(thetas, dunif1, lty=2, col="seagreen")

plot(thetas, dnorm1, col="violetred3", ylim=c(0,max(ylim.dnorm,ylim.dcauchy,ylim.dunif))*fak, xlim=xlim, type="l", pre.plot=grid(), bty="n", xlab=expression(mu), ylab="Density")
lines(thetas, dcauchy1, lty=2, col="steelblue")
lines(thetas, dunif1, lty=3, col="seagreen")

mtext(expression(paste("Flat Priors",sep="")), outer=TRUE,line=-1, cex=1.5, side=3)
mtext(expression(paste("Comparison of different distributions",sep="")), outer=TRUE,line=-3, cex=1, side=3)

par(fig=c(0,1,0,1), oma=c(1,0,0,0), mar=c(0,0,0,0), new=TRUE)
plot(1, type="n", bty="n", xaxt="n", yaxt="n")
legend("bottom", legend=c("normal","cauchy","uniform"), col=c("violetred3","steelblue","seagreen"), lty=1:3, lwd=2, xpd=TRUE, horiz=TRUE, bty="n", cex=.9)



# no flat priors!
# http://mc-stan.org/rstanarm/articles/priors.html
lim <- 1500  
theta <- seq(-lim,lim,0.1)
mu <- 0
sigma <- 500
norm.dens <- dnorm(theta, mean=mu, sd=sigma)
compare <- sigma/2
norm.tab <- data.frame(theta,norm.dens)
head(norm.tab)
tail(norm.tab)
# -lim < compare < lim
p <- 1 - 2*pnorm(-compare, mean=mu, sd=sigma)
p
colos <- adjustcolor(c("orange","darkred"),alpha=0.2)
par(oma=c(2,1,1,1), "cex.axis"=1, bty="l")
plot(theta,norm.dens, type="l", col="darkred", bty="n", pre.plot=grid(), xlab=expression(paste(theta)), ylab="Density", cex.lab=1.2)
theta.outtake <- theta[norm.tab[,"theta"] > -compare & norm.tab[,"theta"] < compare]
dens.outtake <- norm.tab[norm.tab[,"theta"] > -compare & norm.tab[,"theta"] < compare,"norm.dens"]
theta.outt.l <- length(theta.outtake)
theta.startx <- theta.outtake[1]
theta.endx <- theta.outtake[theta.outt.l]
min.d <- min(norm.dens)
polygon(x=c(theta), y=c(norm.dens), col=colos[1], border=NA)
polygon(x=c(theta.startx, theta.outtake, theta.endx,theta.endx),
        y=c(min.d,dens.outtake,dens.outtake[theta.outt.l],min.d), col=colos[2], border=NA)
mtext("Prior distribution areas", outer=TRUE, line=-2, cex=1.5, side=3)
 

# not run
alpha <- 0.05
2*(1-pnorm(qnorm(1-alpha/2, mean=0, sd=1)))
qnorm(1-1e-15/2, mean=0, sd=1)

