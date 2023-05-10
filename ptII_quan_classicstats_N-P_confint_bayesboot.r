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
# ptII_quan_classicstats_N-P_confint_bayesboot.r

# location:
# chap. 4 [4.5.2.6]
# Konfidenzintervalle

# load necessary libs
library(psych)
library(boot)
library(bayesboot)

# load helper functions
source("ptall_generalfuncs.r")


# number of replications / bootstrap samples
repli <- 1e+3

# bootstrap confidence interval from empirical sample
N <- 30
trials <- 40
pop.mean <- 4.5
pop.sd <- 1.7
digits <- 5
mu <- 4.7

#seed
seed <- 56569
set.seed(seed)

# simulation part
res <- do.call("rbind", lapply(seq_along(1:repli), function(i)
{
  x <- rnorm(N, mean=pop.mean, sd=pop.sd)
  mw.x <- mean(x)
  sd.x <- sd(x)
  se.x <- sd.x/sqrt(N)
  ttest.res.mu <- unlist(t.test(x, mu=mu, alternative=c("two.sided")))
  c(mw.x, sd.x, se.x, as.numeric(ttest.res.mu[c("statistic.t","p.value","conf.int1","conf.int2")]))
}))
colnames(res) <- c("mw","sd","se","tvalue","p","CI.low","CI.up")  
str(res)
head(res)

# descriptive statistics
print(psych:::describe(res), digits=digits)

# plot
res <- data.frame(res)
toplot <- c(1,4,5,6,7)
par(oma=c(2,1,1,1), "cex.axis"=1, bty="l", mfrow=c(2,3))
for(i in toplot)
{
 hist(res[,i], prob=TRUE, pre.plot=grid(), col="darkgreen", border="grey", xlab=colnames(res)[i], main="")
 lines(density(res[,i]), pre.plot=grid(), col="magenta", lwd=2)
}
mtext("Bootstrap quantities of interest", outer=TRUE, line=-2, cex=1.5, side=3)

 
# boot t-value and Cohens'd between two samples
N1 <- 43
N2 <- 45
mu1 <- 1.89
mu2 <- 1.5
s1 <- 0.5
s2 <- 0.4

#different sample values
#N1 <- 30
#N2 <- 27
#mu1 <- 4.5
#mu2 <- 4.7
#sd1 <- 1.01
#sd2 <- 1.3

set.seed(seed)
samp1 <- rnorm(N1, mean=mu1, sd=s1)
samp2 <- rnorm(N2, mean=mu2, sd=s2)
samplevalues <- c(samp1, samp2)
group <- c(rep(1,N1),rep(2,N2))

summary(samp1)
summary(samp2)
sd(samp1)
sd(samp2)

dens1 <- density(samp1)
dens2 <- density(samp2)
yrange <- range(c(dens1$y, dens2$y))
plot(dens1, col="darkred", ylim=yrange)
lines(dens2, col="blue")
abline(v=mean(samp1), col="darkred", lty=2)
abline(v=mean(samp2), col="blue", lty=2)

# boot t-value
# Welch version with different sample sizes
boot.tv <- function(dats, ids)
{
  dat1 <- dats[ids[group==1]]
  dat2 <- dats[ids[group==2]]
  n1 <- length(dat1)
  n2 <- length(dat2)
  mean1 <- mean(dat1)
  mean2 <- mean(dat2)
  sd1 <- sd(dat1)
  sd2 <- sd(dat2)
  s.pooled <- sqrt( ( (n1-1)*sd1^2 + (n2-1)*sd2^2 ) / (n1+n2-2) )
  tv <- (mean1-mean2)/(s.pooled*sqrt(1/n1+1/n2))
return(tv)
}

set.seed(seed)
b.tv.res <- boot(samplevalues, boot.tv, strata=group, R=repli)
b.tv.res
boot.ci(b.tv.res)
plot(b.tv.res)
boot.ci(b.tv.res, conf=c(0.5,0.89,0.95),type = c("norm", "basic", "perc", "bca"))

# bayesboot t-value
# https://github.com/rasmusab/bayesboot
set.seed(seed)
bb.mean1 <- bayesboot(samp1, mean, R=repli)
bb.mean2 <- bayesboot(samp2, mean, R=repli)
bb.sd1 <- bayesboot(samp1, sd, R=repli)
bb.sd2 <- bayesboot(samp2, sd, R=repli)
bb.s.pooled <- as.bayesboot( sqrt( ( (N1-1)*bb.sd1^2 + (N2-1)*bb.sd2^2 ) / (N1+N2-2) ) )
bb.tv.res <- as.bayesboot( (bb.mean1-bb.mean2) / (bb.s.pooled*sqrt(1/N1+1/N2)) )

summary(bb.tv.res)
par(oma=c(2,1,1,1), "cex.axis"=1, bty="l")
plot(bb.tv.res)
mtext("Bayes Bootstrap", outer=TRUE, line=-1, cex=1.5, side=3)
plot(bb.tv.res, showCurve=TRUE, showMode=TRUE, pre.plot=grid())


# boot cohens's d 's pooled' version
boot.d <- function(dats, ids)
{
  # calculate delta based on sample2 minus sample1
  return( cohensd( dats[ids[group==2]], dats[ids[group==1]] )[2] )
}

set.seed(seed)
b.d.res <- boot(samplevalues, boot.d, strata=group, R=repli)
b.d.res
boot.ci(b.d.res)
plot(b.d.res, jack=TRUE)
boot.ci(b.d.res, conf=c(0.5,0.89,0.95),type = c("norm", "basic", "perc", "bca"))


# bayesboot cohen's d 's pooled' version
bboot.meandiff <- as.bayesboot(bb.mean1-bb.mean2)
bboot.d.res <- as.bayesboot( bboot.meandiff/bb.s.pooled )

summary(bboot.d.res)
par(oma=c(2,1,1,1), "cex.axis"=1, bty="l")
plot(bboot.d.res, showCurve=TRUE, showMode=TRUE, pre.plot=grid())
plot(bboot.d.res, showCurve=FALSE, showMode=TRUE, pre.plot=grid())
mtext("Bayes Bootstrap", outer=TRUE, line=-1, cex=1.5, side=3)


# simulate difference in means for two normal distributed samples
set.seed(seed)
probs <- c(0.01,0.025,0.05,c(seq(0.1,1,by=0.1)),0.95,0.975,0.99)
digits <- 2

# simulation part
# simulate data itself
sim.diff.means1 <- do.call("rbind", lapply(seq_along(1:repli), function(i)
{
  samp1 <- rnorm(N1, mean=mu1, sd=s1)
  samp2 <- rnorm(N2, mean=mu2, sd=s2)
  xbar1 <- mean(samp1)
  xbar2 <- mean(samp2)
  data.frame(xbar1, xbar2, tv=t.test(samp1,samp2)$statistic, d=cohensd(samp1,samp2)[2])
}))

set.seed(seed)
sim.diff.means2 <- do.call("rbind", lapply(seq_along(1:repli), function(i)
{
  samp1.boot <- sample(samp1, N1, replace=TRUE)
  samp2.boot <- sample(samp2, N2, replace=TRUE)
  xbar1 <- mean(samp1.boot)
  xbar2 <- mean(samp2.boot)
  data.frame(xbar1, xbar2,
             tv=t.test(samp1.boot,samp2.boot, var.equal=FALSE)$statistic,
             d=cohensd(samp2.boot,samp1.boot)[2])
}))

#sim1
head(sim.diff.means1)
print(t(apply(sim.diff.means1,2,quantile, probs=probs)), digits=digits)

#sim2
head(sim.diff.means2)
print(t(apply(sim.diff.means2,2,quantile, probs=probs)), digits=digits)

# plot
par(mfrow=c(1,2))
hist(sim.diff.means1$tv, pre.plot=grid(), col="skyblue", border="white", main="Difference in Means", xlab="t-value (new random values)", ylab="Density", prob=TRUE)
lines(density(sim.diff.means1$tv), col="red", lwd=2)
hist(sim.diff.means1$d, pre.plot=grid(), col="skyblue", border="white", main="Difference in Means", xlab="Cohen's d (s pooled) (new random values)", ylab="Density", prob=TRUE)
lines(density(sim.diff.means1$d), col="red", lwd=2)

# plot
par(mfrow=c(1,2))
hist(sim.diff.means2$tv, pre.plot=grid(), col="skyblue", border="white", main="Difference in Means", xlab="t-value (Bootstrap sample)", ylab="Density", prob=TRUE)
lines(density(sim.diff.means2$tv), col="red", lwd=2)
hist(sim.diff.means2$d, pre.plot=grid(), col="skyblue", border="white", main="Difference in Means", xlab="Cohen's d (s pooled) (Bootstrap sample)", ylab="Density", prob=TRUE)
lines(density(sim.diff.means2$d), col="red", lwd=2)

# compare results
print(t(apply(sim.diff.means1,2,quantile, probs=probs)), digits=digits)
print(t(apply(sim.diff.means2,2,quantile, probs=probs)), digits=digits)
boot.ci(b.tv.res, conf=c(0.5,0.89,0.95),type = c("norm", "basic", "perc", "bca"))
boot.ci(b.d.res, conf=c(0.5,0.89,0.95),type = c("norm", "basic", "perc", "bca"))
summary(bb.tv.res)
summary(bb.d.res)

# direct comparisons Bayes posterior distribution
mean( bb.tv.res > 7)
mean( bb.tv.res > 7 & bb.tv.res < 9.5)
mean( bb.d.res > 0.8)
mean( bb.d.res > 0.5)
mean( bb.d.res > 0.2 & bb.d.res < 0.8)
mean( bboot.d.res > 0.8 & bboot.d.res < 1.26)



# comparison t-value and CI area
# see 'example(lm)'
# Annette Dobson (1990) "An Introduction to Generalized Linear Models".
# Page 9: Plant Weight Data.
ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
group <- gl(2, 10, 20, labels = c("control","treatment"))
weight <- c(ctl, trt)
lm.fit <- lm(weight ~ group)
summary(lm.fit)

# t-value 'grouptreatment'
all.equal(-0.3710/0.3114, summary(lm.fit)$coef[2,3])

# CI
alpha <- 0.05
fak <- qnorm(1-alpha/2, lower=TRUE)
# CI.low
summary(lm.fit)$coef[2,1] - fak * summary(lm.fit)$coef[2,2]
# CI.up
summary(lm.fit)$coef[2,1] + fak * summary(lm.fit)$coef[2,2]
# true mean difference
mu1 - mu2

