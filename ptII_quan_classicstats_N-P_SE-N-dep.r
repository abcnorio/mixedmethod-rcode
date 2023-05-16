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
# ptII_quan_classicstats_N-P_SE-N-dep.r

# location:
# chap. 4 [4.5.5]
# Stichprobengröße


N <- seq(5,1000,10)
N.l <- length(N)
set.seed(1234)
mu1 <- 100
sigma1 <- 10
ses <- do.call("rbind", lapply(seq_along(N), function(i)
{
 samp <- rnorm(n=N[i], mean=mu1, sd=sigma1)
 mw <- mean(samp)
 sabw <- sd(samp)
 se <- sabw/N[i]
 tv <- t.test(samp, alternative=("two.sided"))[["statistic"]]
 tv.pop.mw <- t.test(samp, alternative=("two.sided"), mu=mu1)[["statistic"]]
 d <- (mw-mu1)/sigma1
 return(c(mu1,mw,sigma1,sabw,se,N[i],tv,tv.pop.mw,d))
}))

colnames(ses) <- c("mu","mean","sigma","sd","se","N","t","t.mu","d")
head(ses)
tail(ses)
describe(ses)

#par(mar=c(5,6,5,5))
par(oma=c(2,1,1,1), "cex.axis"=1, bty="l", mfrow=c(3,3))

plot(N,ses[,"mean"], panel.first=grid(), type="l", col="red", ylab="mean")
lines(lowess(ses[,"mean"] ~ N), lty=2, lwd=1.4, col="darkred")

plot(N,ses[,"sd"], panel.first=grid(), type="l", col="blue", ylab="SD")
lines(lowess(ses[,"sd"] ~ N), lty=2, lwd=1.4, col="darkred")

plot(N,ses[,"se"], panel.first=grid(), type="l", col="darkgreen", ylab="SE")
lines(lowess(ses[,"se"] ~ N), lty=2, lwd=1.4, col="darkred")

plot(N,log(ses[,"se"]), panel.first=grid(), type="l", col="darkgreen", ylab="log(SE)")
lines(lowess(log(ses[,"se"]) ~ N), lty=2, lwd=1.4, col="darkred")

plot(N,ses[,"t"], panel.first=grid(), type="l", col="magenta", ylab="t (H0 = 0)")
lines(lowess(ses[,"t"] ~ N), lty=2, lwd=1.4, col="darkred")

plot(N,ses[,"t.mu"], panel.first=grid(), type="l", col="magenta", ylab=paste("t (H0 = ",mu1,")",sep=""))
lines(lowess(ses[,"t.mu"] ~ N), lty=2, lwd=1.4, col="darkred")
abline(h=c(-1.96,1.96), col="green", lty=2, lwd=0.8)

plot(N,ses[,"d"], panel.first=grid(), type="l", col="magenta", ylab=paste("Cohen's d (vs. mu=",mu1,")",sep=""))
lines(lowess(ses[,"d"] ~ N), lty=2, lwd=1.4, col="darkred")

plot(ses[,"t.mu"],ses[,"d"], panel.first=grid(), type="l", col="magenta", xlab="t", ylab=paste("Cohen's d (vs. mu=",mu1,")",sep=""))
lines(lowess(ses[,"d"] ~ ses[,"t.mu"]), lty=2, lwd=1.4, col="darkred")

plot(ses[,"se"],ses[,"d"], panel.first=grid(), type="l", col="magenta", xlab="se", ylab=paste("Cohen's d (vs. mu=",mu1,")",sep=""))
lines(lowess(ses[,"d"] ~ ses[,"SE"]), lty=2, lwd=1.4, col="darkred")

mtext("Interdependence of SE, t and N", outer=TRUE, line=-2, cex=1.5, side=3)


#how many t values are greater than critical value?
alpha <- 0.05
sum(abs(ses[,"t.mu"]) > qnorm(1-alpha/2))/N.l
sum(abs(ses[,"t"]) > qnorm(1-alpha/2))/N.l
mean(ses[,"mean"])/mu1
mean(ses[,"sd"])/sigma1

#plot histogram of t-against-NULL vs. t-against-mu
par(oma=c(2,1,1,1), "cex.axis"=1, bty="l", mfrow=c(1,2))
hist(ses[,"t"], panel.first=grid(), prob=TRUE, col="orange", border="white", main="", xlab="t (H0 = 0)", ylab="density")
lines(density(ses[,"t"]), col="red", lwd=2, lty=2)
hist(ses[,"t.mu"], panel.first=grid(), prob=TRUE, col="skyblue", border="white", main="", xlab=paste("t (H0 = ",mu1,")",sep=""), ylab="density")
lines(density(ses[,"t.mu"]), col="darkblue", lwd=2, lty=2)
mtext("Histograms", outer=TRUE, line=-2, cex=1.5, side=3)



