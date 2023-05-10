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
# ptII_quan_classicstats_pvalue-as-base.r

# location:
# chap. 4 [4.5.8.1]
# Die Berechnung des p-Wertes


# normal vs. mixed normal distribution
seed <- 556
set.seed(seed)
n <- 1e5
fac <- .2
#dfs <- n*fac
SD <- 5
p1 <- rnorm(n)
#p2 <- p1 + rt(n, df=dfs)
p1.5 <- rnorm(n*fac, sd=5)
length(p1)
length(p1.5)
p2 <- sample(c(p1,p1.5), size=n, replace=FALSE)
length(p2)
par(oma=c(2,1,3,1), "cex.axis"=1, bty="l")
hist(p1, panel.first=grid(), prob=TRUE, border="white", col="skyblue",
     main="", xlab="quantile", ylab="Density")
lines(density(p1), col="darkred", lwd=2, lty=2)
lines(density(p2),col="black", lwd=2, lty=3)
# legend
legend("topright", legend=c("NV","NV + t"),					
       lwd=c(2,2), lty=c(2,3), col=c("darkred","black"), bty="n")
mtext("Histogram", 3, line=4, cex=2)
mtext("normal + mixed normal distribution", 3, line=2, cex=1.5)
# quantiles
probs <- c(0,0.01,0.025,0.05,0.1,0.25,0.5,0.75,0.9,0.95,0.975,0.99,1)
q1 <- quantile(p1, prob=probs)
q2 <- quantile(p2, prob=probs)
cbind(q1,q2)
q2/q1
apply(cbind(q1,q2),2,function(x) c(summary(x),sd=sd(x),var=var(x)))


# scatterplot
plot(sort(p1),sort(p2), type="l", col="darkred", bty="n", pre.plot=grid(),
     xlab="standard normal distribution", ylab="mixed normal distribution")
abline(lm(sort(p2) ~ sort(p1)))
mtext("Relationship standard normal dist. and mixed normal dist.", 3, line=2, cex=2)

# differences in probs in accordance to distribution, dfs, and parameter, etc.
v <- 2.5
pnorm(v)
pt(v,15:30)

# calculation of the p value
# simulation H0
seed <- 9876
set.seed(seed)
mu1 <- 100
sigma1 <- 10
trials <- 10000
n <- 30
mean.sim <- replicate(trials, mean(rnorm(n=n, mu1, sigma1)))
mean.sim.dens <- density(mean.sim)

# critical values
alpha <- 0.05
crit.sig.values <- quantile(mean.sim, probs=c(0.025, 0.975))

# histogram and density
color <- rgb(1,0,0,alpha=.2)
xlim <- range(mean.sim.dens$x)
ylim <- c(0,max(mean.sim.dens$y))
par(oma=c(2,1,1,1), "cex.axis"=1, bty="l")
hist(mean.sim, ylim=ylim, panel.first=grid(), prob=TRUE, border="white", col="skyblue",
     main="", xlab="mean values", ylab="Density")
lines(mean.sim.dens, col="red", lwd=2)
# lower limit rejection area
polygon(x=c(xlim[1],mean.sim.dens$x[mean.sim.dens$x < crit.sig.values[1]],crit.sig.values[1]),
        y=c(ylim[1],mean.sim.dens$y[mean.sim.dens$x <= crit.sig.values[1]],ylim[1]), col=color, border=NA)
# upper limit rejection area
polygon(x=c(crit.sig.values[2],mean.sim.dens$x[mean.sim.dens$x > crit.sig.values[2]],xlim[2]),
        y=c(ylim[1],mean.sim.dens$y[mean.sim.dens$x >= crit.sig.values[2]],ylim[1]), col=color, border=NA)
mtext("Simulation H0 (null distribution)", outer=TRUE, line=-2, cex=1.5, side=3)

# now arbitrary empirical value
xbar1 <- 104
# calculate p-value
mean.sim.sort <- sort(mean.sim)
# upper limit
emp.pv <- 1 - length(mean.sim.sort[which(mean.sim.sort < xbar1)])/trials
emp.pv
# lower limit
1 - length(mean.sim.sort[which(mean.sim.sort > xbar1)])/trials

# calculate critical values for significance test
mean.sim.sd <- sd(mean.sim)
fak <- qnorm(1-alpha/2)
crit <- mu1 + c(-fak,fak)*mean.sim.sd
crit

# abline(v=crit, col="magenta", lty=2, lwd=1)
lines(x=c(xbar1,xbar1), y=c(0,max(mean.sim.dens$y)/2), col="darkred", lty=3, lwd=3)

#legend
legend("topright", legend=c(paste("crit. (low) = ",round(crit[1],2),sep=""), paste("crit. (up) = ",round(crit[2],2),sep=""),
                            eval(substitute(expression(paste(bar(x)," (emp.) = ",xbar1,sep="")),list(xbar1=xbar1)))),					
       lwd=c(2,2,2), lty=c(1,1,3), col=c(color, color,"darkred"), bty="n")
mtext(eval(substitute(expression(paste(mu," = ",mu1," | ",sigma," = ",sigma1," | ",alpha," = ",alpha1," | ",bar(x)," = ",xbar1," | N = ",n," | trials = ",trials)),
           list(mu1=mu1, sigma1=sigma1, alpha1=alpha, xbar1=xbar1, n=n, trials=trials))), 1, line=5, cex=1.1)
			 
# test xbar
xbar1 > mean.sim.sort[trials*(1-alpha/2)] | xbar1 < mean.sim.sort[trials*(alpha/2)]

# equivalent
emp.pv < alpha/2

# direct calculation
# two-sided test
# t distributed values
n1 <- 75
alpha <- 0.0412
c(-1,1)*qt(1-alpha/2, df=n1-1)
# normal distribution
alpha <- 0.05
c(-1,1)*qnorm(1-alpha/2)
c(-1,1)*qt(1-alpha/2, df=n1-1)
c(-1,1)*qt(1-alpha/2, df=Inf)

# test auf roughly "the same" (not necessarily 100% identical due to rounding)
all.equal(c(-1,1)*qnorm(1-alpha/2), c(-1,1)*qt(1-alpha/2, df=Inf))

# lower tail
a1 <- 2
b1 <- 5 
qbeta(alpha/2, shape1=a1, shape2=b1, lower.tail=TRUE)
# upper tail
qbeta(alpha/2, shape1=a1, shape2=b1, lower.tail=FALSE)

# plot Beta curve
sek.b <- seq(-0,1,length.out=100)
plot(sek.b,dbeta(sek.b,2,5), type="l", panel.first=grid(), lwd=1, lty=1, col="red", bty="l", main="", xlab="", ylab="pdf")
mtext(expression(paste("Beta distribution",sep="")), 3, line=1.7, cex=1.6)
mtext(eval(substitute(expression(paste(alpha," = ",a1," | ",beta," = ",b1)), list(a1=a1, b1=b1))), 3, line=0.2, cex=1.1)
	
