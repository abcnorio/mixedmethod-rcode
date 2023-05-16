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
# ptII_quan_classicstats_R-index_z-curve.r

# location:
# chap. 4 [4.6.4.1]
# R-Index

# load helper functions
source("ptII_quan_classicstats_R-index_z-curve_helpfuncs.r")


# R Index arbitrary example
dats.1 <- data.frame(stat.test=rep("t",3),
                     type.test=rep("twoway",3),
                     test.statistic=c(1.7,2,1.1),
                     success=c(0,1,1),
                     df.nominator=c(1,1,1),
                     df.denominator=c(38,36,28),
                     alpha=rep(0.05,3))
dats.1
r.indx.dats.res <- R.index(dats=dats.1)

# R Index reproduce Schimmack spreadsheet data (single study)
dats.2 <- data.frame(stat.test="F",
                     type.test="twoway",
                     test.statistic=4.44,
                     success=1,
                     df.nominator=1,
                     df.denominator=38,
                     alpha=0.05)
dats.2
r.indx.dats2.res <- R.index(dats=dats.2)

# output
r.indx.dats.res
r.indx.dats2.res


# manual
# Bem data from Schimmack


# Bem proposes 80% Power in advance
# sample sizes
Ns <- c(100,150,100,100,100,150,150,200,100,50)
median(Ns)
mean(Ns)
# observed power
obs.pwr <- c(79,78,82,73,70,57,52,35,59,88)/100
obs.pwr
# successes
successes <- c(1,1,1,1,1,1,1,0,1,1)
successes
# Cohen's deltas
ds <- c(25,20,26,23,22,15,14,9,19,42)/100
ds
med.obs.pwr <- median(obs.pwr)
mean.succ <- mean(successes)
inflation.rate <- mean.succ - med.obs.pwr
inflation.rate
r.indx.Bem <- med.obs.pwr - inflation.rate
r.indx.Bem
# median effect size
median(ds)


# TODO check below whether the same results as above...
# test statistic to p-value
dats.1
dats.2
calc.pval.v3(dats.2)
sapply(seq_along(1:nrow(dats.1)), function(x) calc.pval.v3(dats.1[x,]))


# pvalue to z-score
pval <- 0.06
z.score <- qnorm(1-pval/2, mean=0, sd=1)
pval
z.score
# and back
(1-pnorm(z.score, mean=0, sd=1))*2

# two-way test + alpha=0.05
alpha <- 0.05
# alpha to z-score
crit.noncentral.z <- qnorm(1-alpha/2)
# case lower bound, p-values with uniform distribution
lowerbound <- TRUE
if(lowerbound) crit.noncentral.z <- qnorm(1-0.025/2)
success <- 1
obs.power <- pnorm(z.score, mean=crit.noncentral.z, sd=1) #mean=1.96
inflation.single <- success-obs.power
r.indx.single <- obs.power-inflation.single

obs.power
inflation.single
r.indx.single

beta.error.rate <- 1-obs.power
beta.error.rate


# from Schimmack
alpha <- 0.05
# median uniform distribution 0.05 - 0 in case H0 is true -> median = 0.025
median.p.unif <- punif(0.05)/2
non.central.z <- qnorm(median.p.unif/2,lower.tail=FALSE)#=qnorm(1-median.p.unif/2)
non.central.z
critical.z <- qnorm(1-alpha/2)
critical.z
# because H0 = TRUE -> = type II error rate
beta.errorrate <- pnorm(critical.z,non.central.z)
beta.errorrate
median.obs.power = 1-beta.errorrate
median.obs.power
inflation <- 1-median.obs.power
R.index <- median.obs.power-inflation
R.index



alpha <- 0.05 # habitual alpha=5% level
dnorm(0) # density at modal value = mean = median
dnorm(qnorm(1-alpha/2)) # density at typical critical significance level
pnorm(qnorm(1-alpha/2)) #cum prob at 1-alpha/2 (two sided test) quantile


# test of insufficient variance (TIVA)
# source:


# Bem study
# 9 out of 10 studies significant at p < 0.05 one-tailed
zzs <- c(2326,2336,2457,2197,2197,1787,1762,1305,1896,2878)/1000
pvals <- c(10,9,7,14,14,37,39,96,29,2)/1000

zzs
pvals


# see also https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0114255


# chisquare value with k-1 df
TIVA(z.scores=zzs, type.test="oneway")
TIVA(pvals=pvals, type.test="oneway")



# Vohs et al. 2006
pvals1 <- c(26,50,46,39,21,40,26,23,6)/1000
zzs1 <- c(223,196,199,206,230,206,223,228,273)/100 #no.5 = 299 (Schimmack, probably wrong!)

pvals1
zzs1

TIVA(z.scores=zzs1, type.test="twoway")
TIVA(pvals=pvals1, type.test="twoway")


library(zcurve)
#library(help=zcurve)
#?zcurve
# load existent data set
OSC.z

# EM without bootstrap
m.EM.nb <- zcurve(OSC.z, method = "EM", bootstrap = FALSE)
# EM with bootstrap
m.EM <- zcurve(OSC.z, method = "EM", bootstrap = 1000)

# KD2 without bootstrap
m.D.nb <- zcurve(OSC.z, method = "density", bootstrap = FALSE)
# KD2 with bootstrap
m.D <- zcurve(OSC.z, method = "density", bootstrap = 1000)

# output
summary(m.EM.nb)
summary(m.EM)
summary(m.D.nb)
summary(m.D)

# plot the results
plot(m.EM.nb)
plot(m.EM)
plot(m.D.nb)
plot(m.D)

# variance z-scores
var(zzs1)

