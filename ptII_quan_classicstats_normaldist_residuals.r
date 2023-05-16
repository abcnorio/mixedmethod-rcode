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
# ptII_quan_classicstats_normaldist_residuals.r

# location:
# chap. 4 [4.6.9.1]
# Normal-Verteilung der Residuen

# load necessary libraries
library(nortest)


# normal distribution of residuals

# example from '?lm' (Annette Dobson)
ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
group <- gl(2, 10, 20, labels = c("Ctl","Trt"))
weight <- c(ctl, trt)
lm.fit <- lm(weight ~ group)

residual <- residuals(lm.fit)
par(oma=c(2,1,1,1), "cex.axis"=1, bty="l", mfrow=c(1,2))
qqnorm(residual, panel.first=grid(), col="blue", cex=1.4, pch=21, bg="skyblue", main="", xlab="theoretical quantiles", ylab="empirical quantiles", bty="l")
qqline(residual, col="red", lwd=2, lty=1)
mtext("Comparison residuals versus normal distribution", outer=TRUE, line=-1.5, cex=1.5, side=3)
mtext(paste("linear model: ",deparse(formula(lm.fit)),sep=""), line=1.02)

# histogram
resid.dens <- density(residual)
xlim <- range(resid.dens$x)
ylim <- range(resid.dens$y)
hist(residual, xlim=xlim, ylim=ylim, panel.first=grid(), prob=TRUE, border="white", col="skyblue", main="", xlab="residuals", ylab="density", breaks="FD")
lines(resid.dens, col="steelblue", lwd=2, lty=1)
width <- round(1.5*range(residual),1)
sek <- seq(from=width[1], to=width[2], length.out=100)
lines(sek,dnorm(sek,mean(residual),sqrt(var(residual))), col="red", lwd=2, lty=1)
legend("topright", legend=c("theory","empirical"), lty=c(1,1), lwd=2, col=c("red","steelblue"), bty="n")
mtext("Histogram residuals", line=1.02)


# normality tests
# shapiro
shapiro.test(residual)
# kolmogorov-smirnof
ks.test(residual,"pnorm",mean(residual),sqrt(var(residual)))
# aanderson-darling
# library 'nortest'
ad.test(residual)
	 
# skewness and kurtosis, they should be around (0,3)
# skewness
# library(e1071)
# library(moments)
moments:::skewness(residual)
for(i in 1:3) cat(paste("type = ",i,": skewness = ",round(e1071::skewness(residual,type=i),5),"\n",sep=""))
# kurtosis
moments:::kurtosis(residual)
for(i in 1:3) cat(paste("type = ",i,": kurtosis = ",round(e1071::kurtosis(residual,type=i),5),"\n",sep=""))


# bigger samples
# random draws
# plot against rnorm()
seed <- 9876
set.seed(seed)
rnd.call <- c("rnorm(n=1e5, mean=100, sd=10)")
rand.nd <- eval(parse(text=rnd.call))
par(oma=c(2,1,1,1), "cex.axis"=1, bty="l", mfrow=c(1,1))
qqnorm(rand.nd, panel.first=grid(), col="blue", cex=0.8, pch=21, bg="skyblue", main="", xlab="theoretical quantiles", ylab="empirical quantiles", bty="l")
qqline(rand.nd, col="red", lwd=1.4, lty=1)
mtext("Plot random values against normal distribution", outer=TRUE, line=-1.5, cex=1.5, side=3)
mtext(paste("Source: ",noquote(rnd.call),sep=""))


# normality tests
seed <- 9876
set.seed(seed)
rand.nd2 <- rnorm(n=5000, mean=100, sd=10)
# shapiro
# only possible sample sizes between 3 and 5000
shapiro.test(rand.nd2)
# kolmogorov-smirnof
# theoretical values possible! see above
ks.test(rand.nd2,"pnorm",100,10)
# anderson-Darling
ad.test(rand.nd)
	 

# skewness and kurtosis, they should be around (0,3)
# skewness
moments:::skewness(rand.nd)
for(i in 1:3) cat(paste("type = ",i,": skewness = ",round(e1071::skewness(rand.nd,type=i),5),"\n",sep=""))
# kurtosis
moments:::kurtosis(rand.nd)
for(i in 1:3) cat(paste("type = ",i,": kurtosis = ",round(e1071::kurtosis(rand.nd,type=i),5),"\n",sep=""))


