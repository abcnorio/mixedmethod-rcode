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
# ptII_quan_Bayes_over-and-underfitting.r

# location:
# chap. 6 [6.7.3]
# Overfitting und Underfitting

# load necessary libs
library(entropy)


# overfitting with polynomials
set.seed(2836)
x <- 1:10
y <- 1:10
y <- y + rnorm(10, 1, 2)
par(oma=c(2,1,1,1), "cex.axis"=1, bty="l")
plot(x,y, bty="n", col="darkred", pre.plot=grid(), pch=21, bg="yellow", cex=1.2)
mtext("Scatterplot", outer=TRUE, line=-2, cex=1.5, side=3)

summary(lm(y ~ x))
summary(lm(y ~ x + x^2))
summary(lm(y ~ x + I(x^2)))
summary(lm(y ~ poly(x,length(y)-2)))

# plot polynomials
par(oma=c(2,1,4,1), "cex.axis"=1, bty="l", mfrow=c(3,3))
plot(x,y, col="violetred3", pre.plot=grid(), pch=21, cex=1.5, bg="darkred", bty="n", main="Scatterplot")
for(i in 1:8)
{
  plot(x,y, col="violetred3", pre.plot=grid(), pch=21, cex=1.5, bg="darkred", bty="n", main=paste("poly degree = ",i,sep=""))
  lines(x,predict(fit <- lm(y ~ poly(x,i))), col="blue")
  summary(fit)
}
mtext("Polynomial (over-)fitting", outer=TRUE, line=0.7, cex=1.5, side=3)


# seed
seed <- 2836

# example 1 - underfitting
set.seed(seed)
x <- 1:100
y <- log(x)
# y <- log(1:100 + rnorm(10, 4, 6))

# create models
fit0.linear <- lm(y ~ x)
fit0.exp <- lm(exp(y) ~ x)
fit0.log <- lm(y ~ log(x))

# plot
par(oma=c(2,1,1,1), "cex.axis"=1, bty="l")
plot(x,y, col="darkred", pre.plot=grid(), pch=21, cex=1.3, bg="yellow", bty="n")
lines(x,y, col="violetred3", lwd=1, lty=2)
lines(x, predict(fit0.linear), col="yellowgreen", lwd=2)
mtext("Underfitting", outer=TRUE, line=-1.5, cex=1.5, side=3)
par(fig=c(0,1,0,1), oma=c(1,0,0,0), mar=c(0,0,0,0), new=TRUE)
plot(1, type="n", bty="n", xaxt="n", yaxt="n")
legend("bottom", legend=c(expression(paste(log(x))),"linear"), lty=c(2,1), lwd=c(1,2), xpd=TRUE, horiz=TRUE, col=c("violetred3","yellowgreen","steelblue"), bty="n", cex=.9)


# example 2 - underfitting
set.seed(seed)
x <- sort(rnorm(100, 1, 2))
y <- x + x^2 - x^3 + x^4

# create models
fit.linear <- lm(y ~ x)
fit.poly <- lm(y ~ poly(x,3))

# plot
par(oma=c(2,1,1,1), "cex.axis"=1, bty="l")
plot(x,y, col="darkred", pre.plot=grid(), pch=21, cex=1.3, bg="yellow", bty="n")
lines(x,y, col="violetred3", lwd=1, lty=2)
lines(x, predict(fit.linear), col="yellowgreen", lwd=2)
lines(x, predict(fit.poly), col="steelblue", lwd=2, lty=1)
mtext("Underfitting and polynomial fit", outer=TRUE, line=-1.5, cex=1.5, side=3)
par(fig=c(0,1,0,1), oma=c(1,0,0,0), mar=c(0,0,0,0), new=TRUE)
plot(1, type="n", bty="n", xaxt="n", yaxt="n")
legend("bottom", legend=c(expression(paste(x^1+x^2-x^3-x^4)),"linear","poly(x,3)"), lty=c(2,1,1), lwd=c(1,2,2), xpd=TRUE, horiz=TRUE, col=c("violetred3","yellowgreen","steelblue"), bty="n", cex=.9)

# compare models
summary(fit0.linear)
summary(fit0.exp)
summary(fit0.log)

summary(fit.linear)
summary(fit.poly)

# example 3 - underfitting
# sinus curve
set.seed(seed)
x <- sort(rnorm(100, 1, 2))

y.sin <- sin(x)
fit.sin <- lm(y.sin ~ x)
fit.sin.poly5 <- lm(y.sin ~ poly(x,5))

summary(fit.sin)
summary(fit.sin.poly5)

par(oma=c(2,1,1,1), "cex.axis"=1, bty="l")
plot(x,y.sin, col="darkred", pre.plot=grid(), pch=21, cex=1.3, bg="yellow", bty="n")
lines(x,y.sin, col="violetred3", lwd=1, lty=2)
lines(x, predict(fit.sin), col="yellowgreen", lwd=2)
# with poly = 5 it almost works
lines(x, predict(fit.sin.poly5), col="steelblue", lwd=2, lty=1)
mtext("Underfitting and polynomial fit", outer=TRUE, line=-1.5, cex=1.5, side=3)
par(fig=c(0,1,0,1), oma=c(1,0,0,0), mar=c(0,0,0,0), new=TRUE)
plot(1, type="n", bty="n", xaxt="n", yaxt="n")
legend("bottom", legend=c("sinus","linear","poly(x,5)"), lty=c(2,1,1), lwd=c(1,2,2), xpd=TRUE, horiz=TRUE, col=c("violetred3","yellowgreen","steelblue"), bty="n", cex=.9)


# library(entropy)
seed <- 83756
set.seed(seed)

# create some counts
freq1 <- runif(10,0,11)
freq2 <- runif(10,0,20)
freq3 <- runif(20,0,20)
freq4 <- runif(20,0,10)
freq5 <- runif(10,0,11)
freqlist <- list(freq1,freq2,freq3,freq4,freq5)
freqlist
# create probabilities from frequencies
flist <- lapply(freqlist, function(x) x/sum(x))
flist
# sanity check whether sum up to 1
lapply(flist, sum)
chooses <- combn(length(flist),2)
chooses
rownames(chooses) <- c("freq#1","freq#2")

# calculate Kullback-Leibler divergence between the probabilities
KL.plugin.res <- apply(chooses,2,function(x) KL.plugin(freqlist[[x[1]]], freqlist[[x[2]]]))

# manual calculation
KL.man <- function(pe,qu)
{
  pe <- pe/sum(pe)
  qu <- qu/sum(qu)
  # if p((x)) <= 0 -> corresponding term equals zero,
  # because lim x*log(x) = 0 for x<0
  logfac <- ifelse(pe>0, log(pe/qu), 0)
  KL <- sum(pe * logfac)
return(KL)
}
# apply it
KL.man.res <- apply(chooses,2,function(x) KL.man(freqlist[[x[1]]], freqlist[[x[2]]]))

# compare
all.equal(KL.plugin.res,KL.man.res)
  
# calculate chi^2-statistic between the probabilities
chi2 <- apply(chooses,2,function(x) chi2.plugin(freqlist[[x[1]]], freqlist[[x[2]]]))
chi2half <- chi2/2
comparisontab <- t(rbind(chooses,KL.plugin.res,KL.man.res,chi2,chi2half))
round(comparisontab,3)

