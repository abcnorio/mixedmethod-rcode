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
# ptII_quan_classicstats_N-P_confint_p-t-value.r

# location:
# chap. 4 [4.5.2.6]
# Konfidenzintervalle

# load helper functions
source("ptall_generalfuncs.r")
source("ptII_quan_classicstats_N-P_confint_p-t-value_helpfuncs.r")


op <- options
options(digits=3)

#initial values
alpha <- 0.05
seed <- 9876
n1 <- 30
mu1 <- 4.7
sigma1 <- 1.8
n2 <- 30
mu2 <- 4.67#4.4
sigma2 <- 1.7
trials <- 1000


ttest.res <- sim.ttest(n1=n1, mu1=mu1, sigma1=sigma1, n2=n2, mu2=mu2, sigma2=sigma2, trials=trials)
#ttest.res <- sim.ttest(n1=n1, mu1=mu1, sigma1=sigma1, n2=n2, mu2=mu2, sigma2=sigma2, trials=trials, seed=runif(1)*1000)

str(ttest.res)
head(ttest.res)

# plot p- and t-values  (histogram and log scaled)
par(oma=c(2,1,1,1), "cex.axis"=1, bty="l", mfrow=c(2,2))
hist(ttest.res[,"t"], pre.plot=grid(), col="darkred", border="white", xlab="t-values", prob=TRUE, main=NA)
lines(density(ttest.res[,"t"]), col="steelblue",lwd=2, lty=1)


hist(ttest.res[,"p"], pre.plot=grid(), col="orange", border="skyblue", xlab="p-values", prob=TRUE, main=NA)
lines(density(ttest.res[,"p"]), col="steelblue",lwd=2, lty=1)

plot(log(sort(ttest.res[,"p"])), col="darkred", type="l", bty="n", pre.plot=grid(), ylab="log(p-value)")
plot(as.brob(sort(ttest.res[,"t"])), col="darkred", type="l", bty="n", pre.plot=grid(), ylab="log(t-value)")

mtext(text="Simulation t-test (t- and p-values)", outer=TRUE, line=-2, cex=1.5, side=3)

# mot run
hist(log(ttest.res[,"p"]))

#default seed
#> head(ttest.res)
#       t   df      p CI(low)  CI(up) xbar1 xbar2  sd1  sd2   delta       d
#1  1.058 47.9 0.2952  -0.456  1.4695  5.03  4.53 2.24 1.36 -0.5068 -0.2733
#2  1.690 56.9 0.0966  -0.154  1.8194  5.12  4.29 1.77 2.04 -0.8326 -0.4363
#3 -2.101 57.9 0.0400  -1.856 -0.0449  4.32  5.27 1.80 1.71  0.9502  0.5425
#4 -0.546 56.6 0.5871  -1.166  0.6663  4.37  4.62 1.90 1.63  0.2498  0.1410
#5 -0.198 57.2 0.8436  -0.958  0.7857  4.24  4.33 1.58 1.78  0.0863  0.0512
#6  1.195 50.2 0.2377  -0.291  1.1466  4.73  4.31 1.64 1.08 -0.4277 -0.3086
#
#>> line 3 is an example for a type S error and type M!

# p-values lower than alpha
sum(ttest.res[,"p"] < alpha)/trials

par(oma=c(2,1,1,1), "cex.axis"=1, bty="l", mfrow=c(1,2))

# plot first few p-values
plot(ttest.res[1:100,"p"], pre.plot=grid(), type="b", col="red", lty=1, bty="l", xlab="trial", ylab="p-value", main="")
abline(h=c(0.01,0.05), col="blue", lty=2, lwd=1)

# plot d
plot(ttest.res[1:100,"d"], pre.plot=grid(), type="b", col="dark green", lty=1, bty="l", xlab="trial", ylab="Cohen's d", main="")
abline(h=c(0.4,-0.4), col="red", lty=2, lwd=1)
mtext("Simulations (process)", outer=TRUE, line=-2, cex=1.5, side=3)

# plot t, p, delta
# TODO plot dots within boundaries of "significance" with color (filled)


# show how quantile() works
N <- 1001
probs <- c(0, 0.025, 0.05, 0.25, 0.5, 0.75, 0.95, 0.975, 1)
x <- rt(N, df=Inf)
q1 <- quantile(x, probs=probs)
q2 <- qt(probs, df=Inf, lower.tail=TRUE)
q1
q2
q1/q2*100


#confint
prob <- 0.95
alpha <- 1-prob
cohensd <- ttest.res[,"d"]
probs <- c(0, 0.025, 0.05, 0.25, 0.5, 0.75, 0.95, 0.975, 1)
quantile(cohensd, probs=probs)
summary(cohensd)

plot.d.sim(cohensd=cohensd)


###################### NOT RUN BELOW THIS POINT

classic.konf(param=5, sd.param=1.5, N=100, ci=0.8)
classic.konf(param=5, sd.param=1.5, N=100, ci=0.95)
classic.konf(param=5, sd.param=1.5, N=100, ci=0.99)

# Check against 'display' from 'arm'
require(arm)
example(lm)
display(XXX)

# t-test against zero (one sample)

set.seed(9876)
sdev <- 1.3
N <- 50
mw <- 0.5
x <- rnorm(n=N, mean=mw, sd=sdev)
t.test(x)
se.x <- sdev/sqrt(N)
mw.sample <- mean(x)
sd.sample <- sd(x)
classic.konf(param=mw.sample, sd.param=sd.sample, N=N)

# compare with example from 'lm'

set.seed(9876)
x <- rnorm(100, mean=2, sd=1.1)
y <- x+rnorm(100, mean=2, sd=0.2)
plot(x,y, col="blue")
abline(lm(y~x), col="red")
lm.res <- lm(y ~x)
lm.res.sum <- summary(lm.res)
lm.res.sum
coefs <- lm.res.sum$coef
coefs
coefs[,"Estimate"]/coefs[,"Std. Error"] 
coefs[,"Estimate"]/coefs[,"Std. Error"] == coefs[,"t value"]

# plot theory curve and maximum (value to test)
sek <- seq(-1,11,by=0.01)
sek.t <- dt(sek,df=N-1, ncp=5)
sek.t.max <- max(sek.t)
sek.t.max
plot(sek,sek.t,col="red",type="l")
lines(list(x=c(5,5),y=c(0,sek.t.max)), col="darkred", lty=2)



