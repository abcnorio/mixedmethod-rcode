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
# ptII_quan_classicstats_equivalentmethods.r

# location:
# chap. 4 [4.6.8.1]
# Äquivalenz von Messverfahren und -methoden

# load necessary helper functions
source("ptall_generalfuncs.r")
source("ptII_quan_classicstats_equivalentmethods_helpfuncs.r")
source("DiM_Bretthorst_UMS.r")

# load necessary libs
library(BlandAltmanLeh)
library(lattice)
library(PairedData)
library(TOSTER)
library(BayesFactor)
library(BEST)


#see appendix
seed <- 0987
set.seed(seed)

n <- 100
mu <- 10
sigma <- 2

x <- rnorm(n=n, mean=mu, sd=sigma)
y <- x + rnorm(n=n, mean=0, sd=1)
y1 <- x + rnorm(n=n, mean=1, sd=1)
y2 <- x + rnorm(n=n, mean=0, sd=2)
y3 <- x + rnorm(n=n, mean=2, sd=1)
y4 <- x + rnorm(n=n, mean=2, sd=2)

# mean-difference plot (Tukey)
# also known as Bland-Altman plot
res <- MD.plot(x,y)
#library(BlandAltmanLeh)
bland.altman.stats(x,y, two=2)
bland.altman.plot(x,y, two=2, silent=TRUE, conf.int=.95)
# library(lattice)
tmd(xyplot(x ~ y))

# library 'BlandAltmanLeh'
# original values from Bland-Altman (1980)
# to reproduce exact results vom original article, we have to use tvn=2
# otherwise tvn <- qnorm(1-alpha/2)
res <- MD.plot(bland.altman.PEFR[,1],bland.altman.PEFR[,3], tvn=2)
# compare
bland.altman.stats(bland.altman.PEFR[,1],bland.altman.PEFR[,3], two=2)
bland.altman.plot(bland.altman.PEFR[,1],bland.altman.PEFR[,3], two=2, silent=TRUE, conf.int=.95)


# library 'lattice)'
# Tukey Mean-Difference Plot
tmd(xyplot(bland.altman.PEFR[,1] ~ bland.altman.PEFR[,3]))
example(tmd)


# simple t-test
# Student
t.test(bland.altman.PEFR[,1], bland.altman.PEFR[,3], alternative="two.sided", paired=TRUE, var.equal=TRUE)
# Welch
t.test(bland.altman.PEFR[,1], bland.altman.PEFR[,3], alternative="two.sided", paired=TRUE, var.equal=FALSE)
# Cohens d
cohensd(bland.altman.PEFR[,1],bland.altman.PEFR[,3])

# simple t-test
# Student
t.test(x,y, alternative="two.sided", paired=TRUE, var.equal=TRUE)
# Welch
t.test(x,y, alternative="two.sided", paired=TRUE, var.equal=FALSE)
# Cohens d
cohensd(x,y)


# x y dataset
head(xy <- data.frame(x,y))
N <- dim(xy)[1]
paur <- 0.8
alpha <- 0.05
# small d
d <- 0.2
dataTOSTpaired(data=xy, pairs=list(c(i1="x",i2="y")),
               low_eqbound=-d, high_eqbound=d, alpha=alpha, desc=TRUE, plots=TRUE)
powerTOSTpaired(alpha=alpha, statistical_power=paur, low_eqbound_dz=-d, high_eqbound_dz=d) 
powerTOSTpaired(alpha=alpha, N=N, low_eqbound_dz=-d, high_eqbound_dz=d) 
powerTOSTpaired(alpha=alpha, statistical_power=paur, N=N)
# now with bigger d...
d <- 0.7
dataTOSTpaired(data=xy, pairs=list(c(i1="x",i2="y")),
               low_eqbound=-d, high_eqbound=d, alpha=alpha, desc=TRUE, plots=TRUE)
powerTOSTpaired(alpha=alpha, statistical_power=paur, low_eqbound_dz=-d, high_eqbound_dz=d) 
powerTOSTpaired(alpha=alpha, N=N, low_eqbound_dz=-d, high_eqbound_dz=d) 
powerTOSTpaired(alpha=alpha, statistical_power=paur, N=N)

# correlation of deviations^2 from mean
r.dev2s <- cor((x-mean(x))^2, (y-mean(y))^2)
data.frame(r=r.dev2s,R2=r.dev2s^2)
# NOT RUN
v1.dev <- (x-mean(y))^2
v2.dev <- (y-mean(y))^2
plot(v1.dev,v2.dev)
plot(log(v1.dev),log(v2.dev))

# library(PairedData)
# test variance of correlated data
BA.paired <- paired(x,y)
plot(BA.paired)

# Pitman-Morgan Test
# manual Pitman-Morgan Test

Var.test(x,y,paired=TRUE)


# manual Pitman-Morgan Test

N <- (length(x))
r.xy <- cor((x+y),(x-y))
r.xy
tvalue <- r.xy*sqrt((N-2)/(1-r.xy^2))
# two-sided
pvalue <- 2*(1-pt(tvalue, df=N-2, lower.tail=T))
alpha <- 0.04
data.frame(r.xy, t=tvalue, p=pvalue, alpha, sig=pvalue<alpha)

# library(BayesFactor)
# comparison with Bayes Factors
boxplot(x,y)

BA.PEFR.1 <- ttestBF(x=x, y=y, mu=0, paired=TRUE)
BA.PEFR.1
#str(BA.PEFR.1)
BA.PEFR.1@bayesFactor
extractBF(BA.PEFR.1)[,"bf"]
bf.alt <- extractBF(BA.PEFR.1)[,"bf"]
# OR BF.null versus BF.alt
(1-bf.alt)/bf.alt

# with null interval d=+/-0.2
BA.PEFR.2 <- ttestBF(x=x, y=y, mu=0, nullInterval=c(-0.2,0.2), paired=TRUE)
BA.PEFR.2
#BA.PEFR.2[1]
#BA.PEFR.2[2]
BA.PEFR.2[1]/BA.PEFR.2[2]
BA.PEFR.2[2]/BA.PEFR.2[1]

# draw samples from posterior
ba.PEFR.samp <- ttestBF(x=x, y=y, paired=TRUE, posterior=TRUE, iterations=1e3)
str(ba.PEFR.samp)
#plot(ba.PEFR.samp)
samps <- as.data.frame(ba.PEFR.samp)
head(samps)
par(oma=c(2,1,1,1), "cex.axis"=1, bty="l", mfrow=c(4,2))
namen <- c("substitute(mu)","substitute(sigma^2)","substitute(delta)","substitute(g)")
for(i in 1:4)
{
  plot(samps[,i], typ="l", bty="n", col="darkred", pre.plot=grid(), ylab="Trace",
       xlab=eval(parse(text=namen[i])),cex.lab=1.2)
  hist(samps[,i], prob=TRUE, pre.plot=grid(), bty="n", col="darkred", border="white", main="",
       xlab=eval(parse(text=namen[i])), ylab="Density", cex.lab=1.2)
  rug(samps[,i],col="darkorange")
  lines(density(samps[,i]), col="green", lwd=2, cex.lab=1.2)
}
mtext("Samples from Posterior (t-Test with BayesFactor)", outer=TRUE, line=-2, cex=1.3, side=3)

# library(BEST)
# comparison with ROPE
BA.best <- BESTmcmc(y1=x, y2=y, parallel=TRUE)
print(BA.best)
summary(BA.best)
plot(BA.best)
plot(BA.best, which="mean", credMass=0.95, compVal=0, ROPE=c(-70,70))
pairs(BA.best)

# Bayesian comparisons
mean.diff <- BA.best$mu1 - BA.best$mu2
out <- vector()
for(i in 1:100) out[i] <- mean(mean.diff > i/100)
par(oma=c(2,1,1,1), "cex.axis"=1, bty="l")
plot(out, type="l", bty="n", pre.plot=grid(), col="darkred", ylab="p(mean.diff > comp.value)", xlab="mean differences (comp.value)")
mtext("Posterior comparison (BEST t-Test)", outer=TRUE, line=-2, cex=1.5, side=3)

mean(mean.diff)
mean(mean.diff > 0)
mean(mean.diff > .1)
mean(mean.diff > .7)

plotAreaInROPE(mean.diff, credMass=0.95, compVal=0.4, maxROPEradius=3)
plotAll(BA.best, credMass=0.8, ROPEm=c(-5,5), ROPEeff=c(-7,7), compValm=0.4)
plotPostPred(BA.best)

# Bretthorst
source("DiM_Bretthorst_UMS.r")
inval <- list(Si=NULL,
              Ni = N,
              Sii=NULL,
              Nii = N,
              smin = 0,
              Di = mean(x),
              si = sd(x),
              Dii = mean(y),
              sii = sd(y),
              L = 7,
              H = 14,
              sL = 2,
              sH = 6,
              snames = c("x","y")
)
DiM.BA <- DiffinMeans(inval=inval, out=FALSE)
DiM.BA$prob.df
DiM.BA$OR.df
UMSprint(results=DiM.BA)



#############

#original Bland Altman Daten

# library 'TOSTER'
# TOST procedure
head(bland.altman.PEFR)
N <- dim(bland.altman.PEFR)[1]
paur <- 0.8
alpha <- 0.05

# small d
d <- 0.2
dataTOSTpaired(data=bland.altman.PEFR, pairs=list(c(i1="bigger.first",i2="smaller.first")),
               low_eqbound=-d, high_eqbound=d, alpha=alpha, desc=TRUE, plots=TRUE)
powerTOSTpaired(alpha=alpha, statistical_power=paur, low_eqbound_dz=-d, high_eqbound_dz=d) 
powerTOSTpaired(alpha=alpha, N=N, low_eqbound_dz=-d, high_eqbound_dz=d) 
powerTOSTpaired(alpha=alpha, statistical_power=paur, N=N)

# now with bigger d...
d <- 0.7
dataTOSTpaired(data=bland.altman.PEFR, pairs=list(c(i1="bigger.first",i2="smaller.first")),
               low_eqbound=-d, high_eqbound=d, alpha=alpha, desc=TRUE, plots=TRUE)
powerTOSTpaired(alpha=alpha, statistical_power=paur, low_eqbound_dz=-d, high_eqbound_dz=d) 
powerTOSTpaired(alpha=alpha, N=N, low_eqbound_dz=-d, high_eqbound_dz=d) 
powerTOSTpaired(alpha=alpha, statistical_power=paur, N=N)			   

			   
# correlation of deviations^2 from mean
v1 <- bland.altman.PEFR[,1]
v2 <- bland.altman.PEFR[,3]
r.dev2s <- cor((v1-mean(v1))^2, (v2-mean(v2))^2)
data.frame(r=r.dev2s,R2=r.dev2s^2)
# NOT RUN
v1.dev <- (v1-mean(v1))^2
v2.dev <- (v2-mean(v2))^2
plot(v1.dev,v2.dev)
plot(log(v1.dev),log(v2.dev))


# library 'PairedData'
# test variance of correlated data
BA.paired <- paired(bland.altman.PEFR[,1], bland.altman.PEFR[,3])
plot(BA.paired)

# Pitman-Morgan Test
# manual Pitman-Morgan Test

Var.test(bland.altman.PEFR[,1], bland.altman.PEFR[,3],paired=TRUE)

# see above
#n <- length(bland.altman.PEFR[,1])
#v1 <- bland.altman.PEFR[,1]
#v2 <- bland.altman.PEFR[,3]
r.v1v2 <- cor((v1+v2),(v1-v2))
r.v1v2
tvalue <- r.v1v2*sqrt((N-2)/(1-r.v1v2^2))
# two-sided
pvalue <- 2*(1-pt(tvalue, df=N-2, lower.tail=T))
alpha <- 0.04
data.frame(r.v1v2, t=tvalue, p=pvalue, alpha, sig=pvalue<alpha)


# library(BayesFactor)
# comparison with Bayes Factors
boxplot(x,y)

BA.PEFR.1 <- ttestBF(x=x, y=y, mu=0, paired=TRUE)
BA.PEFR.1
#str(BA.PEFR.1)
BA.PEFR.1@bayesFactor
extractBF(BA.PEFR.1)[,"bf"]
bf.alt <- extractBF(BA.PEFR.1)[,"bf"]
# OR BF.null versus BF.alt
(1-bf.alt)/bf.alt


# with null interval d=+/-0.2
BA.PEFR.2 <- ttestBF(x=x, y=y, mu=0, nullInterval=c(-0.2,0.2), paired=TRUE)
BA.PEFR.2
#BA.PEFR.2[1]
#BA.PEFR.2[2]
BA.PEFR.2[1]/BA.PEFR.2[2]
BA.PEFR.2[2]/BA.PEFR.2[1]


# draw samples from posterior
ba.PEFR.samp <- ttestBF(x=v1, y=v2, paired=TRUE, posterior=TRUE, iterations=1e3)
str(ba.PEFR.samp)
plot(ba.PEFR.samp)
samps <- as.data.frame(ba.PEFR.samp)
head(samps)
par(oma=c(2,1,1,1), "cex.axis"=1, bty="l", mfrow=c(4,2))
namen <- c("substitute(mu)","substitute(sigma^2)","substitute(delta)","substitute(g)")
for(i in 1:4)
{
 plot(samps[,i], typ="l", bty="n", col="darkred", pre.plot=grid(), ylab="Trace",
 xlab=eval(parse(text=namen[i])),cex.lab=1.2)
 hist(samps[,i], prob=TRUE, pre.plot=grid(), bty="n", col="darkred", border="white", main="",
 xlab=eval(parse(text=namen[i])), ylab="Density", cex.lab=1.2)
 rug(samps[,i],col="darkorange")
 lines(density(samps[,i]), col="green", lwd=2, cex.lab=1.2)
}
mtext("Samples from Posterior (t-Test with BayesFactor)", outer=TRUE, line=-2, cex=1.3, side=3)


# library 'BEST'
# comparison with ROPE
BA.best <- BESTmcmc(y1=bland.altman.PEFR[,1], y2=bland.altman.PEFR[,3], parallel=TRUE)
print(BA.best)
summary(BA.best)
plot(BA.best)
plot(BA.best, which="mean", credMass=0.95, compVal=0, ROPE=c(-70,70))
pairs(BA.best)


mean.diff <- BA.best$mu1 - BA.best$mu2
out <- vector()
for(i in 1:100) out[i] <- mean(mean.diff > i)
out
par(oma=c(2,1,1,1), "cex.axis"=1, bty="l")
plot(out, type="l", bty="n", pre.plot=grid(), col="darkred", ylab="p(mean.diff > comp.value)", xlab="mean differences (comp.value)")
mtext("Posterior comparison (BEST t-Test)", outer=TRUE, line=-2, cex=1.5, side=3)

mean(mean.diff > 0)
mean(mean.diff > 10)
mean(mean.diff > 70)

plotAreaInROPE(mean.diff, credMass=0.95, compVal=0, maxROPEradius=500)
plotAll(BA.best, credMass=0.8, ROPEm=c(-50,50), ROPEeff=c(-70,70), compValm=0)
plotPostPred(BA.best)


# Bretthorst (1993) exact Bayes solution


inval <- list(Si=NULL,
     Ni = N,
     Sii=NULL,
     Nii = N,
     smin = 0,
     Di = mean(v1),
     si = sd(v1),
     Dii = mean(v2),
     sii = sd(v2),
     L = 440,
     H = 480,
     sL = 100,
     sH = 120,
     snames = c("BA.PEFR.1","BA.PEFR.2")
    )
DiM.BA <- DiffinMeans(inval=inval, out=FALSE)
DiM.BA$prob.df
DiM.BA$OR.df
UMSprint(results=DiM.BA)


# not run
# alternative
# changed equivalence region on original scale
inval.1 <- list(Si=NULL,
     Ni = N,
     Sii=NULL,
     Nii = N,
     smin = 0,
     Di = mean(v1),
     si = sd(v1),
     Dii = mean(v2),
     sii = sd(v2),
     L = 380,
     H = 520,
     sL = 100,
     sH = 120,
     snames = c("BA.PEFR.1","BA.PEFR.2")
    )
DiM.BA.1 <- DiffinMeans(inval=inval, out=FALSE)
DiM.BA.1$prob.df
DiM.BA.1$OR.df
UMSprint(results=DiM.BA.1)

