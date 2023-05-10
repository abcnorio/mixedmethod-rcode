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
# ptII_quan_classicstats_p-hacking-sim.r

# location:
# chap. 4 [4.6.2]
# Auf der Suche nach Signifikanzen — unbewusste Forschungsintentionen und p-hacking

# load necessary libs
library(arm)

# load helper functions
source("ptall_generalfuncs.r")
source("ptII_quan_classicstats_p-hacking-sim_helpfuncs.r")


# example from 'lm' manpage
# Annette Dobson (1990) "An Introduction to Generalized Linear Models".
# Page 9: Plant Weight Data.
ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
group <- gl(2, 10, 20, labels = c("Ctl","Trt"))
weight <- c(ctl, trt)
lm.fit <- lm(weight ~ group)

# without and with p-values
display(lm.fit)
display(lm.fit, detail=TRUE)
lm.fit.sim <- arm:::sim(lm.fit)
str(lm.fit.sim)

# sim values
coef.lm.fit.sim <- coef(lm.fit.sim)
sigma.lm.fit.sim <- sigma.hat(lm.fit.sim)
head(coef.lm.fit.sim)
head(sigma.lm.fit.sim)

# quantiles of the uncertainty
t(data.frame(apply(coef(lm.fit.sim), 2, quantile),
             sigma=quantile(sigma.hat(lm.fit.sim)),
             check.names=FALSE))

# plot result
plot.lm.sim(lm.fit.sim)


# see R source code
arm:::sim
showMethods("sim")
?getMethod
getMethod("sim", "lm")


# simulate p-hacking
seed <- 0798
set.seed(seed)

dev.off()
res1 <- p.hack.sim(pr=TRUE)
dev.off()
res2 <- p.hack.sim(d=0.2, pr=TRUE)
dev.off()
res3 <- p.hack.sim(d=0.3, pr=TRUE)
dev.off()
res4 <- p.hack.sim(d=0.4, pr=TRUE)
dev.off()
res01 <- p.hack.sim(d=0.01, ESrange=c(0.008,1.5), pr=TRUE)
# obviously the two curves approach each other while d is increasing...
# what do we learn out of that behavior for small p-values in case of small effects????


# just have a look with random seeds how big samples must be to be "significant"
# according to conventional levels like alpha < 5%
trials <- 1000
# set initial seed to replicate all results
set.seed(seed)
seeds <- replicate(trials,round(runif(1)*trials,0))
sim.mat <- t(sapply(seq_along(1:trials), function(x)
{
  sim.res <- p.hack.sim(d=0.2, seed=seeds[x], sigma0=1, alpha=0.05, pr=FALSE, graph=FALSE, ppaur=FALSE)
  sim.res.d <- dim(sim.res)
  sim.res[sim.res.d[1],c("no","N|each group")]
}))
colnames(sim.mat) <- c("no steps","samplesize")
head(sim.mat)

# compare with a priori power analysis of the t-test to attain a power of 0.8
ptt <- power.t.test(n=NULL, delta=0.2, sd=1, sig.level=0.05, power=0.8, type="two.sample", alternative="two.sided", strict=TRUE)
#n = number of observations (per group)
ptt
describes(sim.mat)
summary(sim.mat)
fivenum.wn(sim.mat)
# single run
#p.hack.sim(d=0.2, seed=seeds[i], sigma0=1, alpha=0.05, pr=FALSE, graph=FALSE, ppaur=FALSE)

# check for specific number (percent) of sample size
sim.mat.d <- dim(sim.mat)
# percent less than n=100
(sum(sim.mat[,"samplesize"] < 100)+0)/sim.mat.d[1]
# global
sim.mat.ssize <- table(sim.mat[,"samplesize"])
sim.mat.ssize.cs <- cumsum(sim.mat.ssize)
sim.mat.ssize.cs.pc <- sim.mat.ssize.cs/sim.mat.d[1]*100

# compare with theoretical value by power.t.test()
# percent of trials required that are less than the theoretical value
# (= required N / sample size) but nevertheless attain 'statistical significance'
max(sim.mat.ssize.cs.pc[as.numeric(names(sim.mat.ssize.cs.pc)) <= ptt$n])


dev.off()
# plot results
par(oma=c(2,1,4,1), "cex.axis"=1, bty="l", mfrow=c(2,2))
plot(as.numeric(names(sim.mat.ssize)), sim.mat.ssize.cs.pc, col="darkred", pre.plot=grid(), type="l", bty="n", xlab="N (cum.)", ylab="percent")
# power t-test
abline(v=ptt$n, col="purple", lty=2, lwd=2)
# samplesize <= 100
abline(v=100, lty=3, col="blue")
# samplesize <= 50
abline(v=50, lty=3, col="red")
# samplesize <= 30
abline(v=30, lty=3, col="purple")

plot(log(as.numeric(names(sim.mat.ssize))), sim.mat.ssize.cs.pc, col="darkred", pre.plot=grid(), type="l", bty="n", xlab="log(N (cum.))", ylab="percent")
# power t-test
abline(v=log(ptt$n), col="purple", lty=2, lwd=2)
# samplesize <= 100
abline(v=log(100), lty=3, col="blue")
# samplesize <= 50
abline(v=log(50), lty=3, col="red")
# samplesize <= 30
abline(v=log(30), lty=3, col="purple")
#mtext("Cumulative sample size (each group) to attain 'significance'", outer=TRUE, line=-2, cex=1.5, side=3)
#mtext("t-Test, 2 groups, p < crit.", outer=TRUE, line=-3.3, cex=1, side=3)

nums <- sim.mat[,"samplesize"]
#nums <- sim.mat[,"no steps"]
hist(nums, breaks=15, pre.plot=grid(), prob=TRUE, bty="n", main="", col="steelblue", border="white",ylab="Density", xlab="N")
rug(nums, col="seagreen")
lines(density(nums), col="magenta", lwd=2)
abline(v=ptt$n, col="purple", lty=2, lwd=2)
boxplot(nums, notch=TRUE, col="orange", bty="n", xlab="", ylab="N", border="steelblue", outcol="seagreen", outbg="black")
rug(nums, side=2, col="seagreen")
abline(h=ptt$n, col="purple", lty=2, lwd=2)

mtext("Random seeds and sample sizes (each group) to attain 'significance'", outer=TRUE, line=0, cex=1.5, side=3)
mtext("t-Test, 2 groups, p < crit.", outer=TRUE, line=-1.5, cex=1, side=3)




### NOT RUN BELOW THIS POINT

# comparisons with N-P a priori power analysis to estimate sample size
power.t.test(n=NULL, delta=0.1, sd=1, sig.level=0.05, power=0.8, type="two.sample", alternative="two.sided", strict=TRUE)

power.t.test(n=NULL, delta=0.1, sd=1, sig.level=0.05, power=0.8, type="two.sample", alternative="two.sided", strict=FALSE)
power.t.test(n=NULL, delta=0.2, sd=1, sig.level=0.05, power=0.8, type="two.sample", alternative="two.sided", strict=FALSE)
power.t.test(n=NULL, delta=0.3, sd=1, sig.level=0.05, power=0.8, type="two.sample", alternative="two.sided", strict=FALSE)
power.t.test(n=NULL, delta=0.4, sd=1, sig.level=0.05, power=0.8, type="two.sample", alternative="two.sided", strict=FALSE)
power.t.test(n=NULL, delta=0.01, sd=1, sig.level=0.05, power=0.8, type="two.sample", alternative="two.sided", strict=FALSE)

#plot required sample sizes
sek <- seq(0.01,3,0.01)
nres <- NULL
for(i in 1:length(sek))
{
 print(i)
 ptest <- power.t.test(n=NULL, delta=sek[i], sd=1, sig.level=0.05, power=0.8, type="two.sample", alternative="two.sided", strict=FALSE)
 nres[i] <- ptest$n
}
par(mfrow=c(2,1))
plot(sek,nres,type="l", col="red", bty="n", pre.plot=grid(), xlab="effect size d", ylab="N")
abline(v=0.1, col="blue")
plot(sek,log(nres),type="l", col="red", bty="n", pre.plot=grid(), xlab="effect size d", ylab="log(N)")
abline(v=0.1, col="blue")



