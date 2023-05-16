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
# ptII_quan_Bayes_PPC_model-check.r

# location:
# chap. 6 [6.7.4.4]
# Praxis — posterior predictive check

# load necessary libraries
library(BEST)

# load necessary helper functions
source("ptall_generalfuncs.r")
source("ptII_quan_Bayes_PPC_model-check_helpfuncs.r")
	

# PPC = posterior predictive checks

# read in 'sa.all' and 'sa.bino'
sa.all <- read.table("startagain_statistics_1992-2017_all-out.tab",sep="\t",header=TRUE)
sa.all.d <- dim(sa.all)
sa.all
sa.all.d

sa.bino <- read.table("startagain_statistics_1992-2017_bino.tab",sep="\t",header=TRUE)
sa.bino.d <- dim(sa.bino)
sa.bino
sa.bino.d


# not run
# alternative priors
thetas <- seq(0,1,0.01)
plot(thetas,dbeta(thetas,1.79,3.068),type="l", pre.plot=grid(), col="darkred", bty="n")
lines(thetas,dbeta(thetas,1,1),lty=2,col="red")
# end of not run


# plot results of prior likelihood, and posterior for the year 2017
thetas <- seq(0,1,0.01)
par(oma=c(2,1,2,1), "cex.axis"=1, bty="l")
plot(thetas,dbeta(thetas,shape1=sa.bino[sa.all$year == 2017,"a.lik"],shape2=sa.bino[sa.all$year == 2017,"b.lik"]),type="l", pre.plot=grid(), col="yellowgreen", lty=2,lwd=2, ylab="Density", xlab=expression(theta))
lines(thetas,dbeta(thetas,shape1=1,shape2=1),lty=2,lwd=2,col="blue")
lines(thetas,dbeta(thetas,shape1=sa.bino[sa.all$year == 2017,"a.post"],shape2=sa.bino[sa.all$year == 2017,"b.post"]),col="darkred", lwd=1, lty=1)
mtext("Prior, Likelihood and Posterior", outer=TRUE, line=-0.5, cex=1.5, side=3)
mtext("success rates start again 1992-2017", outer=TRUE, line=-2, cex=1, side=3)
par(fig=c(0,1,0,1), oma=c(1,0,0,0), mar=c(0,0,0,0), new=TRUE)
plot(1, type="n", bty="n", xaxt="n", yaxt="n")
categs <- c("prior","likelihood","posterior")
legend("bottom", legend=categs, lty=c(2,2,1), lwd=c(2,2,1), xpd=TRUE, horiz=TRUE, col=c("yellowgreen","blue","darkred"), bty="n", cex=.9)


# number of replicated samples
nsims <- 1e3 
z <- sa.all[sa.all.d[1],"s"]
a <- sa.bino[sa.bino.d[1],"a.prior"] + z
b <- sa.bino[sa.bino.d[1],"b.prior"] + z
n <- sa.all[sa.all.d[1],"N.cs"]

# generating nsims theta values from posterior
# ie. random values from posterior distribution with
# a.post = a.prior + z
# b.post = b.prior + n - z
# z = si = successes
# thetas <- rbeta(nsims, a + z, b + n - z)

year <- 2017

a.post <- sa.bino[sa.all[,"year"] == year,"a.post"]
b.post <- sa.bino[sa.all[,"year"] == year,"b.post"]
a.post
b.post
seed <- 0987
set.seed(seed)
thetas <- rbeta(nsims, shape1=299, shape2=305)
head(thetas)
tail(thetas)


# length of the vector to replicate for prediction
N.cs <- sa.all[sa.all[,"year"] == year,"N.cs"]
N.cs
# number of successes to have something to compare
s.cs <- sa.all[sa.all[,"year"] == year,"s.cs"]
s.cs
s.cs/N.cs


# after https://www.barelysignificant.com/post/ppc/
# create vis simulation new dataset Yrep
# we draw from posterior to create predict
Yrep <- sapply(1:length(thetas),
               function(i) sample(c(0,1), N.cs, replace=TRUE, prob=c(thetas[i], 1-thetas[i]))
              )
dim(Yrep)
str(Yrep)			  
head(Yrep)
table(Yrep)

dim(Yrep)

# success rate for each replication/ simulation of predictive posterior
Trep <- apply(Yrep, 2, function(x) sum(x)/N.cs)
head(Trep)
tail(Trep)
describes(Trep)

# description of predictive posterior
data.frame(T_rep=round(c(summary(Trep), SD=sd(Trep), VAR=var(Trep), fivenum2(Trep)),3))
# original values
sa.bino[sa.all[,"year"] == year,]

# comparison value
Ty <- sa.bino[sa.all[,"year"] == year,"mode.post"]
Ty
credMass <- 0.95
ROPE <- c(Ty-0.08,Ty+0.08)


# plot predictive distribution
# library 'BEST'
par(oma=c(2,1,1,1), "cex.axis"=1, bty="l")
plotPost(Trep, compVal=Ty, breaks=20, credMass=credMass, ROPE=ROPE, col="#E6E6E6", xlab=expression(T(y^rep) ) , ylab="Density")	
lines(density(Trep), col="darkred", lwd=2, lty=2)
MAP.post <- sa.bino[sa.all[,"year"] == year,"mode.post"]
abline(v=MAP.post, col="orange",lwd=2,lty=2)
legend("topright", legend=paste("MAP 1992-2017\n[p = ",signif(MAP.post,3),"]",sep=""), lty=2, lwd=2, col="orange", bty="n", cex=.9)
mtext("Simulating Posterior for replications of Y", outer=TRUE, line=-2, cex=1.5, side=3)


# how many Trep > Ty ?
# ie. are there more switches in Trep = posterior predictive compared to Ty = empirical?
# ie. location of T_y in the distribution of T_rep
sum(Trep > Ty)
sum(Trep > Ty)/nsims
sum(Trep > Ty)/length(thetas)
# probability
mean(Trep > Ty)
1/mean(Trep > Ty)
mean(Trep - Ty > 0)
# inverse= Bayesian p-value (p < alphacrit)
1-mean(Trep - Ty > 0)
1-mean(Trep - Ty > 0.05)
1-mean(Trep - Ty > 0.1)
1-mean(Trep - Ty > 0.2)

# plot
sekstart <- -0.10
sek <- seq(sekstart,0.15,0.001)
postps <- vector()
for(i in 1:length(sek)) postps[i] <- mean(Trep-Ty > sek[i])
postps.tab <- data.frame(crit=sek,p=postps)
head(postps.tab)
tail(postps.tab)


# plot
par(oma=c(2,1,1,1), "cex.axis"=1, bty="l")
plot(sek,postps, bty="n", type="l", col="darkred", pre.plot=grid(), xlab="Difference = critical comparison value", ylab="p > crit", main="")
mtext("Bayesian p-value [ = 1 - p]", outer=TRUE, line=-2, cex=1.5, side=3)
# zero line
plot.comp.bpv(sek,postps,compcrit=0)
# empirical posterior mode
plot.comp.bpv(sek,postps,compcrit=mean(Trep)-Ty, Trep=Trep, colo="red")
# one sided hypotheses:
# some interesting value -> very high Bayesian p-value
plot.comp.bpv(sek,postps,compcrit=0.06, Trep=Trep, colo="yellowgreen")
# some interesting value -> very low Bayesian p-value
plot.comp.bpv(sek,postps,compcrit=-0.06, Trep=Trep, colo="yellowgreen")


# compare to crit
postps[postps > Ty]
postps[postps < Ty]


# Or we can compute a Bayesian p-value as (Gelman et al., 2013, page 146):
# pB=Pr(T(yrep,θ) ≥ T(y,θ)|y)
1 - sum(Trep > Ty) / nsims # equivalent to sum(Trep <= Ty) / nsims
sum(Trep <= Ty) / nsims
mean(Trep > Ty)
1-mean(Trep > Ty)
# e.g. =0.0079 ~ 0.008 -> ie. "p" << 0.05 / 0.01 etc. if interpreted as a p-value
# = prob whether observations = y = data are probable
# under this type of model (= hypothesis) with the parameters chosen

