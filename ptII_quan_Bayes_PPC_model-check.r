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


### NOT RUN - ORIGINAL CODE
# after https://www.barelysignificant.com/post/ppc/
# generating nsims new datasets (Yrep)
Yrep <-
    sapply(
        # for each theta
        1:length(thetas),
        # generating samples
        function(i) sample(
            c(0, 1),
            # of the same length as y
            N.cs,#length(y),
            replace = TRUE,
            # with prob of presence equals to theta
            # and prob of absence equals to 1 - theta
            prob = c(thetas[i], 1 - thetas[i])
            )
        )	
# for each theta = random draw from posterior!!!
# generating samples via bootstrap (with replacement)
# of the same length as y
# with prob of presence equals to theta
# and prob of absence equals to 1 - theta
# i.e. prob for each individual random draw from posterior
#
# i.e. for each random draw from posterior sample a full set of y with possible values taken from y
# based on probs according to the random draw from the posterior in question = thetas[i] & 1-thetas[i]
#
# sample from possible values 0 and 1 = binomial case (see y)
# ie. binary outcomes = presence or absence or success and failure etc.
# ?sample
# >>> prob: a vector of probability weights for obtaining the elements of the vector being sampled.
#
# for each new Y_rep sample, computing the number of switches T_rep, and
# comparing it to observed number of switches Ty
# calculate the number of changes = switches from 0 to 1 or 1 to 0
# #nb_switches)
#
### END OF NOT RUN


# adopted to our needs...
# draw from posterior to predict
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


# https://www.barelysignificant.com/post/ppc/
# Which gives the probability of observing this number of switches under our model.
# What does it mean ? Does it mean that our model is wrong ? Well, not exactly.
# Models are neither right or wrong (see Crane & Martin, 2018).
# But our model does not seem to capture the full story,
# it does not seem to give a good representation of the process that generated our data
# (which is arguably one of the characteristics that contribute to the soundness of a model).

# More precisely, it misses the point that the probabilities of successive participants
# being present are not independent. This, in our case, seems to be due to temporal fluctuations
# of this probability throughout the day. For instance, the probability of a participant being present
# seems to be the lowest early in the morning or late in the afternoon, as well as between 12am and 2pm.
# This temporal dependency could be better taken into account by using gaussian process regression models,
# that generalise the varying effect strategy of multilevel models to continuous variables.
# In other words, it would allow to take into account that participants coming to the lab at similar hours
# (e.g., 9am and 9.30am) are more similar (in their probability of being present)
# than participants coming at very different hours (e.g., 9am and 3pm).

