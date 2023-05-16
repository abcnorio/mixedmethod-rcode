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
# ptII_quan_Bayes_case_presidential-debates.r

# location:
# chap. 6 [6.15.3]
# „I, we, and nation“ — präsidiale Eigenwerbung Teil 2

# note:
# does not run after R 3.4 unless you have the appropriate package "appell" for your working R version

# load necessary libs
library(pwr)
library(BayesianFirstAid)
library(emdbook)
library(ggplot2)
library(coda)
library(HDInterval)
library(BayesFactor)

# load functions
source("ptall_generalfuncs_Bayes_binomial.r")
source("ptall_generalfuncs_Bayes_binomial-prop-test.r")
# requires windows
# source("ptall_generalfuncs_appell.r")
source("DiM_Bretthorst_UMS.r")


# start the presidential debates analyses
pres <- t(matrix(c(16,101,91, 32,131,88), nrow=2, byrow=TRUE,dimnames=list(c("Bush","Kerry"),c("nation","I","we"))))
pres

counts.bk <- c(16,101,91, 32,131,88)
president <- gl(2,3, labels=c("Bush","Kerry"))
term <- gl(3,1,6, labels=c("nation","I","we"))
pres.dat <- data.frame(counts.bk,president,term)
pres.dat


# descriptive

# rows
prop.table(pres, 1)
margin.table(pres, 1)

# cols
prop.table(pres, 2)
margin.table(pres, 2)

# all
addmargins(pres)


# frequentist solution

cor(pres[,1], pres[,2])
chisq.test(pres)
set.seed(88772)
chisq.test(pres, sim=TRUE, B=1e5)
prop.test(pres)


# we reduce to 2x2 Chi^2 table
pres
pres.2x2 <- rbind(pres["I",],pres["nation",]+pres["we",])
rownames(pres.2x2) <- c("I","we/nation")
pres.2x2
addmargins(pres.2x2)


cor(pres.2x2[,1], pres.2x2[,2])
chisq.test(pres.2x2)
set.seed(88772)
chisq.test(pres.2x2, sim=TRUE, B=1e5)
prop.test(pres.2x2)


# power
# library(pwr)
# power = ?
pwr.2p2n.test(h=0.2,n1=208,n2=251,power=NULL,sig=0.07)$power
# effect size = ?
pwr.2p2n.test(h=NULL,n1=208,n2=251,power=0.7,sig=0.07)$h
# sample size = ?
pwr.2p2n.test(h=0.2,n1=208,n2=NULL,power=0.7,sig=0.07)$n2
pwr.2p2n.test(h=0.2,n1=NULL,n2=251,power=0.7,sig=0.07)$n1

# necessary samples versus empirical samples (Bush, Kerry)
c(299,397)/colSums(pres.2x2)


####

# JAGS MCMC proportion test
# library(BayesianFirstAid)
# library(emdbook)

# analyze for rows means to t(pres.2x2) so that Bush versus Kerry is analyzed!!!
t(pres.2x2)
pres.2x2    # rows = terms -> analyze for different frequency of usage of the combined terms I vs. we/nation
t(pres.2x2) # rows = terms -> analyze for different frequency of usage between Bush and Kerry

pres.2x2.bprop <- bayes.prop.test(t(pres.2x2), cred.mass=0.95, n.iter=15000, progress.bar="text")
pres.2x2.bprop
# summary
BFA.summary.bayes_prop_test(pres.2x2.bprop)

# graphical diagnostics
BFA.mcmcplot.thetas(pres.2x2.bprop)

# library(ggplot2)
qplot(as.data.frame(pres.2x2.bprop)[,1],as.data.frame(pres.2x2.bprop)[,2],geom=c("hex"), xlab=expression(theta[1]), ylab=expression(theta[2]))

# MCMC diagnostics
par(oma=c(2,1,4,1), "cex.axis"=1, bty="l")
diagnostics(pres.2x2.bprop)
mtext("MCMC diagnostics", outer=TRUE, line=1.7, cex=1.5, side=3)

# model code
model.code(pres.2x2.bprop)
# TODO - tweak the prior!!!!

# plot posterior
plot(pres.2x2.bprop)


# not run
# analyze for original rows!
pres.2x2 # rows = terms I versus we/nation
pres.t2x2.bprop <- bayes.prop.test(pres.2x2, cred.mass=0.95, n.iter=15000, progress.bar="text")
# summary
BFA.summary.bayes_prop_test(pres.t2x2.bprop)
# graphical diagnostics
BFA.mcmcplot.thetas(pres.t2x2.bprop)
# end of not run


# posterior odds ratios -> see BFA.summary.bayes_prop_test()
# compare models relative freqs
# diff crit for ROPE prob. -> ROPE = region of practical equivalence
# see Kruschke 2011 5% = ROPE[-5,+5]
diff.crit <- 3/100
theta.prop.within.ROPE <- as.data.frame(pres.2x2.bprop)
head(theta.prop.within.ROPE)
# whether probability that the group difference is within the ROPE
theta.prob.equal <- mean(abs((theta.prop.within.ROPE$theta1 - theta.prop.within.ROPE$theta2)) < diff.crit)
theta.prob.equal
# relative frequencies are practically different
1 - theta.prob.equal
# odds ratio in favor of equality
theta.prob.equal / (1 - theta.prob.equal)
# odds ratio in favor of a difference
(1 - theta.prob.equal) / theta.prob.equal



# excursion
26+26+10+15
# 77
77^45
# 7.799785e+84
(77^45)/100000/3600/24/365.25
# 2.471603e+72
(26+10)^7
# 78364164096
(36^7)/100000/3600/24/365.25
# 0.02483211



#### Simulation from posterior draws
# brute force MCMC!
# regardless whether prior or posterior
# use a1, b1 as well as a2, b2

# library(coda)
# library(HDInterval)


# MCMC brute force variante 1

# some arbitrary values
res <- bayes.prop.mcmc(a1=4, b1=8, a2=3, b2=6, n.mcmc=1e+4, nchains=4, credMass=0.95)
str(res)
plot(as.mcmc(res$mcmc), col="darkred")


# Jeffrey's prior
#a.prior <- 1/2
#b.prior <- 1/2


# simple gibbs sampler
seed <- 5555
set.seed(seed)

# here comparison of prior and a beta distribution
# for practical purpose one compares two beta distributions
# AND adds a prior to each of them
# uniform prior, shape first beta distribution
a1 <- 1
b1 <- 1
# second beta distribution
a2 <- 0.6
b2 <- 5

n.mcmc <- 1e5
crit.equivalence <- 0.6
reps <- 4
theta.diff <- replicate(reps, rbeta(n.mcmc, shape1=a1, shape2=b1) - rbeta(n.mcmc, shape1=a2, shape2=b2))
str(theta.diff)
prob.diff <- apply(theta.diff,2, function(x) mean(x < crit.equivalence))
prob.diff


# in this case with prior a=1, b=1 result is the same
# as using "only" likelihood function to convert si,Ni to a,b

# analysis
pres.2x2
# default: analysis for rows (here: I vs we/nation)
# we want to analyze for Bush vs. Kerry
pres.2x2.bprop.mcmc <- bprop.mcmc(pres.2x2, analyze="cols")
str(pres.2x2.bprop.mcmc)
bayes.plot.mcmc(bprop.mcmc.res=pres.2x2.bprop.mcmc)

# TODO
# diagnostics
# posterior Odds Ratios
credMass <- 0.99
mean(abs(pres.2x2.bprop.mcmc$mcmc[["theta (diff)"]]) < (1-credMass))
# better ROPE
mean(abs(pres.2x2.bprop.mcmc$mcmc[["theta (diff)"]]) < 0.03)
mean(abs(pres.2x2.bprop.mcmc$mcmc[["theta (diff)"]]) < 0.05)
mean(abs(pres.2x2.bprop.mcmc$mcmc[["theta (diff)"]]) < 0.1)
#etc... further hypotheses possible


# plot sequence of hypotheses
# define credMass area
sek <- seq(0,0.3,0.01)
bprop.vs.sek <- sapply(sek, function(i) mean(abs(pres.2x2.bprop.mcmc$mcmc[["theta (diff)"]]) > i) )
bprop.vs.sek
par(oma=c(2,1,1,1), "cex.axis"=1, bty="l")
plot(sek, bprop.vs.sek, type="l", bty="n", col="darkred", pre.plot=grid(),
     xlab="credMass",
  	 ylab=eval(substitute(expression(paste("p(",theta[diff],")",sep="")))),
	   main="")
mtext(eval(substitute(expression(paste(theta[diff]," > credMass",sep="")))), outer=TRUE, line=-2, cex=1.5, side=3)


# for book
# data
pres.2x2
# analysis for Bush vs. Kerry
pres.2x2.bprop.mcmc <- bprop.mcmc(pres.2x2, analyze="cols")
pres.post.betavalues <- pres.2x2.bprop.mcmc$a1b1a2b2
names(pres.post.betavalues) <- c("a1","b1","a2","b2")
pres.post.betavalues


	
### MCMC brute force variante 2

# library(HDInterval)

# add chains
nchains <- 3
n.mcmc <- 1e+4
# library(coda)
# library(BEST)
theta.diff.mcmc <- gr1.mcmc <- gr2.mcmc <- vector(mode="list", length=nchains)

# calculate x chains of MCMC for group1 and group2
set.seed(78824)
#a1b1a2b2 <- pres.2x2.bprop.mcmc[["a1b1a2b2"]]
a1b1a2b2 <- pres.post.betavalues
# posteriors
gr1.mcmc <- lapply(gr1.mcmc, function(x) x <- rbeta(n.mcmc, shape1=a1b1a2b2[1], shape2=a1b1a2b2[2]))
gr2.mcmc <- lapply(gr2.mcmc, function(x) x <- rbeta(n.mcmc, shape1=a1b1a2b2[3], shape2=a1b1a2b2[4]))

# calculate theta.diff = theta1 - theta2
for(i in 1:nchains)
{
 theta.diff.mcmc[[i]] <- gr1.mcmc[[i]] - gr2.mcmc[[i]]
}

# calculate summary statistics
sums <- function(x) c(summary(x), sd=sd(x), var=var(x))
rnams <- c("Min.","1st Qu.","Median","Mean","3rd Qu.","Max.","sd","var")
megalist <- list(gr1.mcmc, gr2.mcmc, theta.diff.mcmc)
mega.res <- sapply(megalist, function(x) sapply(x, sums))
# calculate hdis
hdis.l <- sapply(megalist, function(x) sapply(x, hdi, credMass=credMass))
res <- rbind(mega.res, hdis.l)
statistic <- c(rep(rnams,nchains),
               rep(paste(c("HDI lower ","HDI upper "),"(",round(credMass*100),"%)",sep=""),dim(hdis.l)[1]/2)
               )
mcmcchain <- c(rep(c(1:nchains),each=dim(mega.res)[1]/nchains),
               rep(c(1:nchains),each=2)
               )
res <- data.frame(statistic, res, "MCMC chain"=mcmcchain, check.names=FALSE)
lnam <- c("theta 1","theta 2","theta (diff)")
colnames(res)[2:4] <- lnam
res <- res[order(mcmcchain),]
rownames(res) <- NULL
abc <- lapply(list(1,11,21), function(x) x+0:9)
abcm <- do.call("rbind",abc)
dim(abcm) <- c(30,1)
abcm
res.sorted <- res[abcm,]
res
# sorted after MCMC chains
res.sorted
megalist.mcmc.list <- lapply(megalist, function(x) as.mcmc.list(lapply(x, as.mcmc)))
lnam


# plot
par(oma=c(2,1,4,1), "cex.axis"=1, bty="l", mfrow=c(3,2))
sapply(seq_along(lnam), function(x)
{
 coda:::traceplot(megalist.mcmc.list[[x]], smooth=TRUE, main=lnam[x])
 coda:::densplot(megalist.mcmc.list[[x]], col="red", main=lnam[x])
 rug(jitter(unlist(megalist.mcmc.list[[x]]), amount = 0.05), ticksize=0.04, side=1, col="darkorange") 
 return(invisible)
} 
)
mtext("MCMC diagnostics", outer=TRUE, line=0.5, cex=1.5, side=3)


# test some hypotheses
# Probability theta1 > theta2 and theta1 < theta2
# over all chains
critv <- 0
# remember: diff = theta1 - theta2
# diff > 0 -> theta1 > theta2
diffGREATERzero <- sapply(theta.diff.mcmc, function(x) mean(x > critv))
# diff < 0 -> theta1 < theta2
diffSMALLERzero <- sapply(theta.diff.mcmc, function(x) mean(x < critv))

# output of hypotheses over all MCMC-chains
diffGREATERzero
diffSMALLERzero

# posterior odds
diffGREATERzero / (1-diffGREATERzero)

# posterior odds
diffSMALLERzero / (1-diffSMALLERzero)

# crit 0.1
# over all chains
critv <- 0.1
sapply(theta.diff.mcmc, function(x) mean(x > critv))

# ask about absolute difference without any direction
sapply(theta.diff.mcmc, function(x) mean(abs(x) > critv))
#sapply(theta.diff.mcmc, function(x) mean(abs(x) < critv))

# plot just first mcmc
dens <- density(theta.diff.mcmc[[1]])
par(oma=c(2,1,1,1), "cex.axis"=1, bty="l", mfrow=(c(1,2)))
hist(theta.diff.mcmc[[1]], prob=TRUE, xlab=eval(substitute(expression(paste(theta[diff],sep="")))),
     ylab="density", main="", col="steelblue", border="white", breaks="Sturges", cex.lab=1.2, pre.plot=grid())
lines(dens, col="green", lwd=2)
mtext("Bayesian proportion test (brute force)", outer=TRUE, line=-2, cex=1.5, side=3)
# add HDI line
credMass <- 0.87
hdis <- hdi(dens, credMass=credMass)
hdis
xrange <- dens$x[dens$x < hdis["upper"] & dens$x > hdis["lower"]]
#yrange <- dens$y[dens$x < hdis["upper"] & dens$x > hdis["lower"]]
#lines(x=c(xrange[1],xrange[length(xrange)]), y=c(yrange[1],yrange[length(yrange)]), col="orange", lwd=5)
yheight <- attr(hdis,"height")
lines(x=c(xrange[1],xrange[length(xrange)]), y=c(yheight,yheight), col="orange", lwd=6)
legend("left", legend=c(paste("HDI [",credMass*100,"%]",sep="")), lty=1, lwd=2, xpd=TRUE, horiz=FALSE, col="orange", bty="n", cex=.9)


# plot just first mcmc
thetadiff1.abs <- abs(theta.diff.mcmc[[1]])
dens <- density(thetadiff1.abs)
# remove values < 0 BUT y before x (!)
dens$y <- dens$y[dens$x > 0]
dens$x <- dens$x[dens$x > 0]
str(dens)
#par(oma=c(2,1,1,1), "cex.axis"=1, bty="l")
hist(thetadiff1.abs, prob=TRUE, xlab=eval(substitute(expression(paste("abs(",theta[diff],")",sep="")))),
     ylab="density", main="", col="steelblue", border="white", breaks="Sturges", cex.lab=1.2, pre.plot=grid())
lines(dens, col="green", lwd=2)
#mtext("Bayesian proportion test (brute force)", outer=TRUE, line=-2, cex=1.5, side=3)
# add HDI line
credMass <- 0.87
hdis <- hdi(dens, credMass=credMass)
hdis
xrange <- dens$x[dens$x < hdis["upper"] & dens$x > hdis["lower"]]
yheight <- attr(hdis,"height")
lines(x=c(0,xrange[length(xrange)]), y=c(yheight,yheight), col="orange", lwd=5)
legend("right", legend=c(paste("HDI [",credMass*100,"%]",sep="")), lty=1, lwd=2, xpd=TRUE, horiz=FALSE, col="orange", bty="n", cex=.9)


### not run

# use first chain
prob.gr1.vs.gr2.diff <- gr1.mcmc[[1]] - gr2.mcmc[[1]]
mean(prob.gr1.vs.gr2.diff > 0)

# Probability, that theta_1 < theta_2
prob.gr1.vs.gr2 <- mean(gr1.mcmc[[1]] > gr2.mcmc[[1]])
prob.gr1.vs.gr2
1 - prob.gr1.vs.gr2

# prob theta_1 - theta_2 > 0.1
mean(prob.gr1.vs.gr2.diff > 0.1)

# Odds ratio in favor of theta_1 > theta_2
prob.gr1.vs.gr2 / (1-prob.gr1.vs.gr2)
# Odds ratio in favor of theta_1 < theta_2
(1-prob.gr1.vs.gr2) / prob.gr1.vs.gr2
# absolute difference (thetas) below diff.crit = 1-credMass
credMass <- 0.87
diff.crit <- 1-credMass
mean(abs(gr1.mcmc[[1]] - gr2.mcmc[[1]]) < diff.crit)
mean((gr1.mcmc[[1]] - gr2.mcmc[[1]]) < diff.crit)


# take only one chain
credMass <- 0.99
diff.crit <- 1-credMass
n.mcmc <- 1e5

# extract posterior a's and b's for B(a,b)
a1b1a2b2 <- pres.2x2.bprop.mcmc[["a1b1a2b2"]]
a1b1a2b2

seed <- 78824
set.seed(seed)
g1 <- rbeta(n.mcmc, shape1=a1b1a2b2[1], shape2=a1b1a2b2[2])
g2 <- rbeta(n.mcmc, shape1=a1b1a2b2[3], shape2=a1b1a2b2[4])
theta.diff <- g1 - g2

# plot
dens <- density(theta.diff)
par(oma=c(2,1,1,1), "cex.axis"=1, bty="l")
hist(theta.diff, prob=TRUE, xlab=eval(substitute(expression(paste(theta[diff],sep="")))),
     ylab="density", main="", col="steelblue", border="white", breaks="Sturges", cex.lab=1.2, pre.plot=grid())
lines(dens, col="green", lwd=2)
mtext("Bayesian proportion test (brute force)", outer=TRUE, line=-2, cex=1.5, side=3)
# add HDI line
credMass <- 0.87
hdis <- hdi(dens, credMass=credMass)
hdis
xrange <- dens$x[dens$x < hdis["upper"] & dens$x > hdis["lower"]]
yrange <- dens$y[dens$x < hdis["upper"] & dens$x > hdis["lower"]]
lines(x=c(xrange[1],xrange[length(xrange)]), y=c(yrange[1],yrange[length(yrange)]), col="orange", lwd=5)
legend("right", legend=c(paste("HDI [",credMass*100,"%]",sep="")), lty=1, lwd=2, xpd=TRUE, horiz=FALSE, col="orange", bty="n", cex=.9)

### END OF not run


### Numerical integration OR grid approximation

a1 <- a1b1a2b2[1]
b1 <- a1b1a2b2[2]
a2 <- a1b1a2b2[3]
b2 <- a1b1a2b2[4]

int.width <- 1e-3
start.sek <- 0
end.sek <- 1

nsim <- 1e+5
sek <- seq(start.sek, end.sek, int.width)
if(nsim > 5000) nsamp <- 5000 else nsamp <- nsim
set.seed(33345)
mcmc.1 <- rbeta(nsim, shape1=a1, shape2=b1)
mcmc.2 <- rbeta(nsim, shape1=a2, shape2=b2)
mcmc.diff <- mcmc.1 - mcmc.2
# mean theta difference (posterior)
mean(mcmc.diff)

# posterior probs and odds ratios
mean(abs(mcmc.diff) > 0.1) 	# how much difference > 10%

mean(mcmc.diff > 0) 		    # how much difference > 0%
# =
mean(mcmc.1 > mcmc.2)		    # theta1 > theta2

mean(mcmc.diff < 0) 		    # how much difference < 0%
# =	
1-mean(mcmc.1 > mcmc.2)		  # theta1 < theta2

mean(mcmc.diff > 0)/mean(mcmc.diff < 0)
mean(mcmc.diff < 0)/mean(mcmc.diff > 0)

# rough plot
# - densities against each other
# - densities next to each other + diff
# - scatterplot
# - mcmc chains for each other + diff
par(oma=c(2,1,4,1), "cex.axis"=1, bty="l", mfrow=c(2,3))

dbetas1 <- dbeta(sek, shape1=a1, shape2=b1)
dbetas2 <- dbeta(sek, shape1=a2, shape2=b2)

theta1.nam <- eval(substitute(expression(paste(theta[1],sep=""))))
theta2.nam <- eval(substitute(expression(paste(theta[2],sep=""))))
main.nam <- eval(substitute(expression(paste(theta[1]," vs. ",theta[2],sep=""))))
plot(dbetas1, dbetas2, xlab=theta1.nam, ylab=theta2.nam, pre.plot=grid(),
     main=main.nam, col="violetred3", type="l", bty="n", lwd=2)
abline(a=0, b=1, col="blue", lty=2, lwd=3)

plot(sek,dbetas1, xlim=c(-0.4, 0.6), main="Histograms", ylab="density", xlab="p",
	 type="l",col="violetred3", bty="n", pre.plot=grid(),, lwd=2)
points(sek, dbetas2, col="steelblue", type="l", lwd=2)
lines(density(mcmc.diff), col="yellowgreen", type="l", lwd=2)
legend("topleft", bty="n", legend=c(theta1.nam, theta2.nam, main.nam), col=c("violetred3","steelblue","yellowgreen"), lwd=3)

main.nam <- eval(substitute(expression(paste("Scatterplot ", theta[1]," vs. ",theta[2],sep=""))))
plot(mcmc.1, mcmc.2, col="black", pch=21, bg="yellowgreen", cex=0.8, main=main.nam, bty="n",
     xlab=theta1.nam, ylab=theta2.nam, pre.plot=grid(),)
abline(a=0, b=1, col="orange", lty=2, lwd=3)

mcmc1.nam <- eval(substitute(expression(paste("MCMC chain ",theta[1],sep=""))))
plot(mcmcsub1 <- sample(mcmc.1,nsamp), main=mcmc1.nam, col="skyblue", type="l", bty="n",
     xlab="", ylab=theta1.nam, pre.plot=grid())
lines(lowess(mcmcsub1),col="salmon", lwd=3, lty=2)

mcmc2.nam <- eval(substitute(expression(paste("MCMC chain ",theta[2],sep=""))))
plot(mcmcsub2 <- sample(mcmc.2,nsamp), main=mcmc2.nam, col="violetred3", type="l", bty="n",
     xlab="", ylab=theta2.nam, pre.plot=grid())
lines(lowess(mcmcsub2),col="blue", lwd=3, lty=2)
mtext("MCMC diagnostics", outer=TRUE, line=0.5, cex=1.5, side=3)



### Brute force numerical integration / grid approximation

# content of the function
bayes.prop.grid

# outer product
1:10 %o% 1:10
outer(1:10,1:10,"*")

# remember values
pres.2x2
# posterior beta values
a1
b1
a2
b2

# calculate based on grid approximation
prob.a1b1.vs.a2b2 <- bayes.prop.grid(a1=a1, b1=b1, a2=a2, b2=b2, int.width=1e-3)

# prob theta1 > theta2
sum.prob <- sum(prob.a1b1.vs.a2b2)
sum.prob
# prob theta1 < theta2
1 - sum.prob
# odds ratios
sum.prob / (1-sum.prob)
(1-sum.prob) / sum.prob


# analysis of presidents Bush vs. Kerry over "I" vs. "we/nation"
pres.2x2
pres.post.betavalues
prob.a1b1.vs.a2b2 <- bayes.prop.grid(a1=pres.post.betavalues["a1"],
                                     b1=pres.post.betavalues["b1"],
                  									 a2=pres.post.betavalues["a2"],
									                   b2=pres.post.betavalues["b2"],
									                   int.width=1e-3)


# test some hypotheses
# prob theta1 > theta2
sum.prob <- sum(prob.a1b1.vs.a2b2)
sum.prob
# prob theta1 < theta2
1 - sum.prob
# odds ratios
sum.prob / (1-sum.prob)
(1-sum.prob) / sum.prob


### Exact tests

a1 <- pres.post.betavalues["a1"]
b1 <- pres.post.betavalues["b1"]
a2 <- pres.post.betavalues["a2"]
b2 <- pres.post.betavalues["b2"]
pres.2x2
a1
b1
a2
b2
c(a1,b1,a2,b2)

# important: if loga=TRUE
# result is a BROB object
# Pr(GR2 > GR1)
h.res <- h(a1=a1, b1=b1, a2=a2, b2=b2)
h.res
# log version
h.res.log <- h(a1=a1, b1=b1, a2=a2, b2=b2, loga=TRUE)
h.res.log
as.numeric(h.res.log)

# Probability(GR2 > GR1)
h.res
# Pr(GR2 < GR1)
1 - h.res
# Ratio in favor of GR2 > GR1
h.res / (1-h.res)
# GR2 < GR1
1/( h.res / (1-h.res) )
# Odds ratio in favor of GR2 > GR1
((h.res)/(1-h.res)) / ((1-h.res)/h.res)
# GR2 < GR1
((1-h.res)/h.res) / ((h.res)/(1-h.res))

# difference between brute force numerical integration and h()
h.res.inv <- 1-h.res
# difference
sum.prob - h.res.inv
# equal
1 - abs(sum.prob - h.res.inv)
# ratio
sum.prob / h.res.inv

# difference between BayesianFirstAid MCMC and h()
diff.crit <- 0
thetas <- as.data.frame(pres.2x2.bprop)
pROPE.equal <- mean(thetas[,"theta1"] - thetas[,"theta2"] > diff.crit)
pROPE.equal

# difference
pROPE.equal - h.res.inv
# equal
1 - abs(pROPE.equal - h.res.inv)
# ratio
pROPE.equal / h.res.inv



###### decision rule Bayesian A/B Testing after Chris Stucchio




# [theta2 - theta1] < crit (Test)
credMass <- 0.99
res <- bayes.prop.loss(a1=a1, b1=b1, a2=a2, b2=b2, crit=1-credMass)
# change criteria
credMass <- 0.1
res <- bayes.prop.loss(a1=a1, b1=b1, a2=a2, b2=b2, crit=1-credMass)
credMass <- 0.22
res <- bayes.prop.loss(a1=a1, b1=b1, a2=a2, b2=b2, crit=1-credMass)

# table TRUE vs. FALSE
loss.v <- res[,4]
sek <- seq(0,1,0.001)
tab <- table(loss.v < sek)
tab/sum(tab)
# when does it change from FALSE to TRUE
tf.IDs <- which(( lossvBELOWsek <- loss.v < sek) == TRUE)[1]
data.frame(loss.v,sek, "loss.v < sek"=lossvBELOWsek, check.names=FALSE)[(tf.IDs[1]-1):tf.IDs[1],]

tab.tf <- data.frame(loss.v,sek,loss.v<sek, check.names=FALSE)
tf.IDs <- which(tab.tf[,3] == TRUE, arr.ind=TRUE)
tab.tf[(tf.IDs[1]-1):tf.IDs[1],]




############## NOT RUN
bayes.prop.grid2 <- function(a1=a1, b1=b1, a2=a2, b2=b2, int.width=1e-3, start.sek=0, end.sek=1, CRIT=0)
{
  sek <- seq(start.sek, end.sek, int.width)
  # important part: (x > y)
  grid.res <- outer(sek, sek, function(x, y) (y-x<CRIT) * dbeta(x, a1, b1) * int.width * dbeta(y, a2, b2) * int.width)
  # (y - x <0.1)
  # grid.res <- outer(sek, sek, function(x, y) (y - x <0.1) * dbeta(x, a1, b1) * int.width * dbeta(y, a2, b2) * int.width)
  return(grid.res)
}

p1 <- sum(bayes.prop.grid2(a1,b2,a2,b2, CRIT=mean(bf.diff)))
p2 <- 1-p1
p1
p2


# brute force
set.seed(192934)
bf.N <- 10e7
bf.res1 <- rbeta(n=bf.N, shape1=a1, shape2=b1)
bf.res2 <- rbeta(n=bf.N, shape1=a2, shape2=b2)
bf.diff <- bf.res2 - bf.res1
mean(bf.diff)
############## END OF NOT RUN






### more exact test

### FOLLOWING FUNCTIONS ARE REPROGRAMMED BUT IN PRINCIPLE TAKEN FROM
# original paper by Nadajarah & Kotz (2007)
# corrections by Chen & Luo (2011)
# original R code by Sverdlov, Ryeznik & Wu (2015)

# pdf of theta ratio = theta_2 / theta_1
# see also Pham-Gia (2000)

# not run
# similar functions in R package 'tolerance'
# qdiffprop
#
# and appell function
# comparison
# library(tolerance)
#
# F1(a = 3, b = 4, b.prime = 5, c = 13, x = 0.2, y = 0.4)
# [1] 2.110471
# versus
# appellf1(a = 3, b1 = 4, b2 = 5, c = 13, x = 0.2, y = 0.4)
# [1] 2.110471+0i
#
# hyp2f1(a = 3, b = 4, c = 13, z = 0.5) 


# not run - infos
# original paper versions
# CDF = p2beta
# PDF = d2beta
# Quantile = q2beta
# CI = ci2beta

# a1,b1,a2,b2 are a and b values of a posterior beta distribution (see sum symbol below from original paper)

# original function from paper
source("bayesian2beta.r")

# comparison with original paper versions:
d2beta("DIFF", x=0.5, a1 = 1/3+0, b1 = 1/3+5-0, a2 = 1/3+2, b2 = 1/3+5-2)
#[1] 1.402381
pdf.theta.diff(theta=0.5, a1 = 1/3+0, b1 = 1/3+5-0, a2 = 1/3+2, b2 = 1/3+5-2, loga=FALSE)
#[1] 1.402381
pdfthetadiff.1 <- pdf.theta.diff(theta=0.5, a1 = 1/3+0, b1 = 1/3+5-0, a2 = 1/3+2, b2 = 1/3+5-2, loga=TRUE)
pdfthetadiff.1
exp(pdfthetadiff.1)
#Inf
pdfthetadiff.2 <- pdf.theta.diff(theta=0.5, a1 = 1/3+0, b1 = 1/3+5-0, a2 = 1/3+2, b2 = 1/3+5-2, loga=TRUE, sL=1/10000,sH=1-1/10000)
pdfthetadiff.2
# sL/sH +/- 1/10000
#[1] 0.3515413
exp(pdfthetadiff.2)
#[1] 1.421256
pdfthetadiff.2 <- pdf.theta.diff(theta=0.5, a1 = 1/3+0, b1 = 1/3+5-0, a2 = 1/3+2, b2 = 1/3+5-2, loga=TRUE, sL=0,sH=1-1/9175)
pdfthetadiff.2
# sH- 1/9175
#[1] 0.3381657
exp(pdfthetadiff.2)
#1.402373
1.402373/1.402381
#> 1.402373/1.402381
#[1] 0.9999943
1-1.402373/1.402381
#> 1-1.402373/1.402381
#[1] 5.704584e-06


d2beta("RR", x=0.5, a1 = 1/3+0, b1 = 1/3+5-0, a2 = 1/3+2, b2 = 1/3+5-2)
pdf.theta.ratio(theta=0.5, a1 = 1/3+0, b1 = 1/3+5-0, a2 = 1/3+2, b2 = 1/3+5-2, loga=FALSE)

d2beta("OR", x=0.5, a1 = 1/3+0, b1 = 1/3+5-0, a2 = 1/3+2, b2 = 1/3+5-2)
pdf.theta.OR(theta=0.5, a1 = 1/3+0, b1 = 1/3+5-0, a2 = 1/3+2, b2 = 1/3+5-2, loga=FALSE)
exp(pdf.theta.OR(theta=0.5, a1 = 1/3+0, b1 = 1/3+5-0, a2 = 1/3+2, b2 = 1/3+5-2, loga=TRUE))

# prob( p2-p1 < crit )
p2beta ("DIFF", "DIRECT", x = 0, a1 = 1/3+0, b1 = 1/3+5-0, a2 = 1/3+2, b2 = 1/3+5-2)
cdf.theta.diff(theta=0, a1 = 1/3+0, b1 = 1/3+5-0, a2 = 1/3+2, b2 = 1/3+5-2)

# prob( p2/p1 < crit )
p2beta(relation='RR', approach='DIRECT', x = 1.5, a1 = 1/3+0, b1 = 1/3+5-0, a2 = 1/3+2, b2 = 1/3+5-2)
cdf.theta.ratio(theta=1.5, a1 = 1/3+0, b1 = 1/3+5-0, a2 = 1/3+2, b2 = 1/3+5-2)

# prob( (p2/(1-p2)) / (p1/1-p1)) < crit )
p2beta(relation='OR', approach='DIRECT', x = 1.5, a1 = 1/3+0, b1 = 1/3+5-0, a2 = 1/3+2, b2 = 1/3+5-2)
cdf.theta.OR(theta=1.5, a1 = 1/3+0, b1 = 1/3+5-0, a2 = 1/3+2, b2 = 1/3+5-2)

# CI % p2-p1 (Nelder-Mead)
# original paper version:
ci2beta(relation='DIFF', method='neldermead', a1 = 1/3+0, b1 = 1/3+5-0, a2 = 1/3+2, b2 = 1/3+5-2, alpha=.05, left0=-.2, right0=.8)
theta.diff.hdi(a1 = 1/3+0, b1 = 1/3+5-0, a2 = 1/3+2, b2 = 1/3+5-2, alpha=.05, le=-.2, re=.8)

# CI % p2-p1 -> quantile function / inverse CDF
# original paper version:
ci2beta(relation='DIFF', method='inv.cdf', a1 = 1/3+0, b1 = 1/3+5-0, a2 = 1/3+2, b2 = 1/3+5-2, alpha=.05, left0=0, right0=0)
# does work only for p2-p1
CI.DIFF.inv.cdf(a1 = 1/3+0, b1 = 1/3+5-0, a2 = 1/3+2, b2 = 1/3+5-2, alph=0.05, tol=1e-5, methode="diff")

# CORRECT! d2beta = correct for DIFF, RR, and OR

# correct
# p2beta(relation='DIFF', approach='DIRECT', x=0.5, a1,b1,a2,b2)
# cdf.theta.diff(theta=0.5, a1,b1,a2,b2)

# end of not run



### dynamically load old R function BECAUSE not available in recent R version
dyn.load("appell/libs/i386/appell.dll")
# replace f21_sub by „f21_sub“ (with quotation marks)
results <- .Fortran("f21_sub", a = a, b = b, c = c, z = z, hyp2f1 = algorithm, val = val)

# replace f1 by „f1“ (with quotation marks)
results <- .Fortran("f1", a = a, b1 = b1, b2 = b2, c = c, x = x,
                    y = y, algoflag = algoflag, userflag = userflag, debug = debug,
                    val = val, hyp2f1 = hyp2f1)
### END OF dynamically load old R function



################ BOOK

# values from paper
a1 <- 1/3+7
b1 <- 1/3+12-7
a2 <- 1/3+6
b2 <- 1/3+18-6
theta.res <- prop.theta.sek(a1=a1, b1=b1, a2=a2, b2=b2,
                            loga=c(T,T,T),
                            parallel=TRUE,
                            numer=TRUE,
                            BROB=c(T,T,T),
                            xlim.diff=c(-1,1,-1,1), l.diff=100,#xlim.diff=c(-.999,.999, -.999,.999), l.diff=100,
                            xlim.RR=c(0,10, 0,2), l.RR=100,
                            xlim.OR=c(0,100, 0,2.5), l.OR=100,
                            theta.crit=c(0.5,1,1)
                            )

# brute force
set.seed(192934)
bf.N <- 10e6
bf.res1 <- rbeta(n=bf.N, shape1=a1, shape2=b1)
bf.res2 <- rbeta(n=bf.N, shape1=a2, shape2=b2)
bf.diff <- bf.res2 - bf.res1
plotPost(bf.diff, credMass=0.87, ROPE=c(-0.5,0.5), xlab="theta difference", showMode=TRUE, col="skyblue", border="white", compVal=0.5)

# MAP exact
theta.res.exp <- theta.res$post$differ$pdf
theta.res.exp[,2] <- exp(theta.res.exp[,2])
MAP.xct <- theta.res.exp[(theta.res.exp[,2] == max(theta.res.exp[,2])),]
# MAP brute force
MAP.bf <- mean(bf.diff)

# output and comparison
MAP.xct
MAP.bf
(MAP.xct[,1]-MAP.bf)/MAP.xct[,1]

# plot
bf.diff.dens <- density(bf.diff)
par(mfrow=c(1,2))
# non-log
plot(theta.res.exp[,1], theta.res.exp[,2], type="l", col="darkred", bty="n", pre.plot=grid(), xlab="theta diff", ylab="pdf.diff")
abline(v=MAP.xct[,1], col="darkred")
lines(bf.diff.dens$x, bf.diff.dens$y, col="blue")
abline(v=MAP.bf, lty=3, col="blue")
# log
theta.res.log <- theta.res$post$differ$pdf
plot(theta.res.log[,1], theta.res.log[,2], type="l", col="darkred", bty="n", pre.plot=grid(), xlab="theta.diff", ylab="log(pdf.diff)")
abline(v=MAP.xct[,1], col="darkred")
lines(bf.diff.dens$x, log(bf.diff.dens$y), col="blue", lty=3, lwd=2)
abline(v=MAP.bf, lty=3, col="blue")
mtext(expression(paste("Bayesian Analysis of Difference of Proportions ",theta[2]," - ",theta[1]," | Exact vs. Brute Force",sep="")),
      3, line=-3, cex=1.6, outer=TRUE)

# presidential data
# values see above
pres.2x2

# Bush vs. Kerry
pres.post.betavalues

a1 <- pres.post.betavalues["a1"]
b1 <- pres.post.betavalues["b1"]
a2 <- pres.post.betavalues["a2"]
b2 <- pres.post.betavalues["b2"]
a1
b1
a2
b2
rm(theta.res)
theta.res <- prop.theta.sek(a1=a1, b1=b1, a2=a2, b2=b2,
                            loga=c(T,T,T),
                            parallel=TRUE,
                            numer=TRUE,
                            BROB=c(T,T,T),
                            xlim.diff=c(-1,1,-1,1), l.diff=100,#xlim.diff=c(-.999,.999, -.999,.999), l.diff=100,
                            xlim.RR=c(0,10, 0,2), l.RR=100,
                            xlim.OR=c(0,100, 0,2.5), l.OR=100,
                            theta.crit=c(0.5,1,1)
                            )
# only plot result
prop.theta.sek.plot(theta.res)

# brute force
set.seed(192934)
bf.N <- 10e6
bf.res1 <- rbeta(n=bf.N, shape1=a1, shape2=b1)
bf.res2 <- rbeta(n=bf.N, shape1=a2, shape2=b2)
bf.diff <- bf.res2 - bf.res1
plotPost(bf.diff, credMass=0.87, ROPE=c(-0.25,0.25), xlab="theta difference", showMode=TRUE, col="skyblue", border="white", compVal=0.2)

# MAP exact
theta.res.exp <- theta.res$post$differ$pdf
theta.res.exp[,2] <- exp(theta.res.exp[,2])
MAP.xct <- theta.res.exp[(theta.res.exp[,2] == max(theta.res.exp[,2])),]
# MAP brute force
MAP.bf <- mean(bf.diff)

# output and comparison
MAP.xct
MAP.bf
(MAP.xct[,1]-MAP.bf)/MAP.xct[,1]

# plot
bf.diff.dens <- density(bf.diff)
par(mfrow=c(1,2))
# non-log
plot(theta.res.exp[,1], theta.res.exp[,2], type="l", col="darkred", bty="n", pre.plot=grid(), xlab="theta diff", ylab="pdf.diff")
abline(v=MAP.xct[,1], col="darkred")
lines(bf.diff.dens$x, bf.diff.dens$y, col="blue")
abline(v=MAP.bf, lty=3, col="blue")
# log
theta.res.log <- theta.res$post$differ$pdf
plot(theta.res.log[,1], theta.res.log[,2], type="l", col="darkred", bty="n", pre.plot=grid(), xlab="theta.diff", ylab="log(pdf.diff)")
abline(v=MAP.xct[,1], col="darkred")
lines(bf.diff.dens$x, log(bf.diff.dens$y), col="blue", lty=3, lwd=2)
abline(v=MAP.bf, lty=3, col="blue")

mtext(expression(paste("Bayesian Analysis of Difference of Proportions ",theta[2]," - ",theta[1]," | Exact vs. Brute Force",sep="")),
      3, line=-3, cex=1.6, outer=TRUE)

# check values
thetaC <- 0.05
# brute force
# p(difference < thetaC) = MCMC based
mean(bf.diff < thetaC)
# p(difference > thetaC) = MCMC based
mean(bf.diff > thetaC)

# exact solution
cdf.theta.diff(theta=thetaC, a1, b1, a2, b2)

################ END OF BOOK


################ NOT RELEVANT FOR BOOK
# Bush vs. Kerry
a1 <- pres.post.betavalues["a1"]
b1 <- pres.post.betavalues["b1"]
a2 <- pres.post.betavalues["a2"]
b2 <- pres.post.betavalues["b2"]
a1
b1
a2
b2
pdf.diff <- vector()
sek.pdf.diff <- seq(-1,1,length=100)
for(i in 1:length(sek.pdf.diff))
{
  print(i)
  test <- try( pdf.theta.diff.MULT(theta=sek.pdf.diff[i], a1=a1, b1=b1, a2=a2, b2=b2, loga=TRUE, numer=TRUE) )
  if(is.numeric(test)) pdf.diff[i] <- test else pdf.diff[i] <- NA
}

set.seed(192934)
bf.N <- 10e6
bf.res1 <- rbeta(n=bf.N, shape1=a1, shape2=b1)
bf.res2 <- rbeta(n=bf.N, shape1=a2, shape2=b2)
bf.diff <- bf.res2 - bf.res1

# MAP
MAP.bf <- mean(bf.diff)
pdf.diff.exp <- exp(pdf.diff)
MAPID <- which(pdf.diff.exp == max(pdf.diff.exp))
MAP.Xct <- data.frame(theta=sek.pdf.diff,pdf.dens=pdf.diff.exp)[MAPID,]

# output MAPs
MAP.Xct[1,1]/MAP.bf
MAP.Xct[1,1]-MAP.bf
(MAP.Xct[1,1]-MAP.bf)/MAP.Xct[1,1]

# log scale
MAP.Xct.thetadiff <- sek.pdf.diff[pdf.diff == max(pdf.diff)]
MAP.Xct.thetadiff/MAP.bf
MAP.Xct.thetadiff-MAP.bf
(MAP.Xct.thetadiff-MAP.bf)/MAP.Xct.thetadiff

# plot
bf.diff.dens <- density(bf.diff)
par(mfrow=c(1,2))
plot(sek.pdf.diff, pdf.diff, type="l", col="darkred", bty="n", pre.plot=grid(), xlab="theta diff", ylab="log(pdf.diff)")
lines(bf.diff.dens$x, log(bf.diff.dens$y), col="blue")
abline(v=MAP.Xct.thetadiff, lty=2, col="darkred")
abline(v=MAP.bf, lty=3, col="blue")

plot(sek.pdf.diff, pdf.diff.exp, type="l", col="darkred", bty="n", pre.plot=grid(), xlab="theta.diff", ylab="pdf.diff")
abline(v=sek.pdf.diff[pdf.diff.exp == max(pdf.diff.exp)], lty=2, col="darkred")
lines(bf.diff.dens$x, bf.diff.dens$y, col="blue")
abline(v=bf.diff.dens$x[bf.diff.dens$y == max(bf.diff.dens$y)], lty=3)

# this above shows that there is numerical instability near zero
# which prevents the whole thing of being really exact round zero as can
# be seen on the plot
# IT WORKS FOR THE VALUES FROM THE PAPER
#
# initial values
a1 <- 1/3+7
b1 <- 1/3+12-7
a2 <- 1/3+6
b2 <- 1/3+18-6

# exact test
pdf.diff <- vector()
sek.pdf.diff <- seq(-1,1,length=100)
for(i in 1:length(sek.pdf.diff))
{
  print(i)
  test <- try( pdf.theta.diff.MULT(theta=sek.pdf.diff[i], a1=a1, b1=b1, a2=a2, b2=b2, loga=TRUE, numer=TRUE) )
  if(is.numeric(test)) pdf.diff[i] <- test else pdf.diff[i] <- NA
}

# brute force
set.seed(192934)
bf.N <- 10e6
bf.res1 <- rbeta(n=bf.N, shape1=a1, shape2=b1)
bf.res2 <- rbeta(n=bf.N, shape1=a2, shape2=b2)
bf.diff <- bf.res2 - bf.res1

# MAPs
MAP.bf <- mean(bf.diff)
pdf.diff.exp <- exp(pdf.diff)
MAPID <- which(pdf.diff.exp == max(pdf.diff.exp))
MAP.Xct <- data.frame(theta=sek.pdf.diff,pdf.dens=pdf.diff.exp)[MAPID,]

# output MAPs
MAP.Xct[1,1]/MAP.bf
MAP.Xct[1,1]-MAP.bf
(MAP.Xct[1,1]-MAP.bf)/MAP.Xct[1,1]

# log scale
MAP.Xct.thetadiff <- sek.pdf.diff[pdf.diff == max(pdf.diff)]
MAP.Xct.thetadiff/MAP.bf
MAP.Xct.thetadiff-MAP.bf
(MAP.Xct.thetadiff-MAP.bf)/MAP.Xct.thetadiff

# plot
bf.diff.dens <- density(bf.diff)
par(mfrow=c(1,2))

plot(sek.pdf.diff, pdf.diff, type="l", col="darkred", bty="n", pre.plot=grid(), xlab="theta diff", ylab="log(pdf.diff)")
lines(bf.diff.dens$x, log(bf.diff.dens$y), col="blue")
abline(v=MAP.Xct.thetadiff, lty=2, col="darkred")
abline(v=MAP.bf, lty=3, col="blue")

plot(sek.pdf.diff, pdf.diff.exp, type="l", col="darkred", bty="n", pre.plot=grid(), xlab="theta.diff", ylab="pdf.diff")
abline(v=sek.pdf.diff[pdf.diff.exp == max(pdf.diff.exp)], lty=2, col="darkred")
lines(bf.diff.dens$x, bf.diff.dens$y, col="blue")
abline(v=bf.diff.dens$x[bf.diff.dens$y == max(bf.diff.dens$y)], lty=3)
################ END OF NOT RELEVANT FOR BOOK


################ NOT RELEVANT FOR BOOK
# create only simpson rule values and use them later
for(i in 1:sekk.l)
{
  theta <- sekk[i]
  cat(i,"\t",theta,"\n")
  F2.brob.list[[i]] <- try( F2.brob(a = b1, b = a1 + b1 + a2 + b2 - 2,
                                    b.prime = 1 - a1,
                                    c = a2 + b1,
                                    x = 1 + theta,
                                    y = 1 - theta^2,
                                    numer=FALSE,
                                    parallel=TRUE,
                                    # limit F1 Appell integral 0<x<1
                                    sL=0.0001,
                                    sH=0.9999                              )
                            #res1=brob gammas
                            #res2=simpsonrule.brob.2 list
  )
  orig.list[[i]] <- try( tolerance:::F1(a = b1, b = a1 + b1 + a2 + b2 - 2,
                                        b.prime = 1 - a1,
                                        c = a2 + b1,
                                        x = 1 + theta,
                                        y = 1 - theta^2
  )
  )
}

# LOG version create only simpson rule values and use them later
loga <- TRUE
loga <- FALSE
for(i in 1:sekk.l)
{
  #test <- try( pdf.theta.diff.MULT(theta=sek.pdf.diff[i],
  #                            a1=a1, b1=b1, a2=a2, b2=b2,
  #                            loga=loga[1],
  #                            numer=FALSE)
  #)
  theta <- sekk[i]
  cat("\ni=",i,"\t",theta,"\n")
  pdf.list.nonlog[[i]] <- try( pdf.theta.diff.MULT(theta = theta,
                                                   a1 = a1,
                                                   b1 = b1,
                                                   a2 = a2,
                                                   b2 = b2,
                                                   loga = loga,
                                                   numer = TRUE,
                                                   parallel=TRUE,
                                                   debug=TRUE,
                                                   sL=0.000001,
                                                   sH=0.999999
  )
  )
}  

### non-log
str(pdf.list.nonlog)
length(pdf.list.nonlog)
str(pdf.list.nonlog[[1]])
pdf.num.nonlog <- as.numeric(lapply( pdf.list.nonlog, "[[",1 ))
data.frame(thetadiff = sekk, dens = pdf.num.nonlog)
pdf.num.nonlog.max <- max(pdf.num.nonlog)
data.frame(thetadiff = sekk[pdf.num.nonlog == pdf.num.nonlog.max],
           dens = pdf.num.nonlog.max)

if(loga)
{
  par(mfrow=c(1,2))
  plot(sekk, exp(pdf.num.nonlog), type="l", bty="n", pre.plot=grid(), col="darkred")
}  
plot(sekk, pdf.num.nonlog, type="l", bty="n", pre.plot=grid(), col="darkred")
### END non-log

### log
# MAP
MAP.ID <- which(pdf.num == max(pdf.num))
MAP.Xct <- data.frame(sekk, pdf.num)[MAP.ID,]
MAP.Xct

# brute force
set.seed(192934)
bf.N <- 10e6
bf.res1 <- rbeta(n=bf.N, shape1=a1, shape2=b1)
bf.res2 <- rbeta(n=bf.N, shape1=a2, shape2=b2)
# Kerry vs. Bush
bf.diff <- bf.res2 - bf.res1
MAP.bf <- mean(bf.diff)
summary(bf.diff)

# plot integral values from simpsonrule only NOT! final result
par(mfrow=c(1,2))
plot(intv.x, pdf.mat[1,], col="darkred", bty="n", type="l", pre.plot=grid(), main="brob (=log)")
plot(intv.x, exp(pdf.mat[1,]), col="darkred", bty="n", type="l", pre.plot=grid(), main="exp(brob)")

inf.ID.exp <- which(is.infinite(pdf.mat.exp), arr.ind=FALSE)
inf.ID.exp
hilo.exp <- range(pdf.mat.exp[-inf.ID.exp])
hilo.exp

# restrict
mean(pdf.mat.exp[-inf.ID.exp])
quantile(pdf.mat.exp, probs=seq(0, 1, 0.05))
probrestrict <- 0.90
hilo.exp[2] <- quantile(pdf.mat.exp, probs=(probrestrict))
################ END OF NOT RELEVANT FOR BOOK


################ FOR BOOK
pres.post.betavalues
a1 <- pres.post.betavalues["a1"]
b1 <- pres.post.betavalues["b1"]
a2 <- pres.post.betavalues["a2"]
b2 <- pres.post.betavalues["b2"]
a1
b1
a2
b2

low <- -1
hi <- 1
thetadiff.l <- 100
sekk <-seq(low,hi,length=thetadiff.l)
sekk.l <- length(sekk)
sekk

F2.brob.list <- list()
orig.list <- list()
pdf.list <- list()
pdf.list.nonlog <- list()

# apply values for hypergeo integral calculation
a <- b1
b <-a1 + b1 + a2 + b2 - 2
b.prime <- 1 - a1
c <- a2 + b1
x <- 1 + theta
y <- 1 - theta^2
numer <- FALSE
loga <- FALSE

# create only simpson rule values and use them later
for(i in 1:sekk.l)
{
  theta <- sekk[i]
  cat("\ni = ",i,"\ttheta = ",theta,"\n")
  pdf.list[[i]] <- try( pdf.theta.diff.MULT(theta = theta,
                                            a1 = a1,
                                            b1 = b1,
                                            a2 = a2,
                                            b2 = b2,
                                            loga = TRUE,
                                            numer = FALSE,
                                            parallel=TRUE,
#                                            sL=0,
#                                            sH=1    
  )
  )
}  

### log
length(pdf.list)
str(pdf.list[[1]])
pdf.num.log <- as.numeric(lapply( pdf.list, "[[",1 ))
pdf.num <- exp( pdf.num.log )
sekk

# extract table of in-between results
str(lapply( lapply( lapply( pdf.list, "[[",2 ), "[[",2 ), "[[",1 ))[[1]]
pdf.df <- do.call("rbind", lapply( lapply( lapply( pdf.list, "[[",2 ), "[[",2 ), "[[",1 ) )
dim(pdf.df)
head(pdf.df)
tail(pdf.df)
simpson.l <- dim(pdf.df)[1]/sekk.l
intv.x <- pdf.df[1:simpson.l,"intv.x"]
pdf.mat <- matrix(data=pdf.df[,"inity"], ncol=simpson.l, nrow=sekk.l, byrow=TRUE)
dim(pdf.mat)
length(intv.x)
pdf.mat.exp <- exp( pdf.mat )

# plot simpson rule values for each theta from pdf.theta.diff [-1 to 1]
# ie. base values of the integral F1 Appell [0 to 1] without summing up
inf.ID <- which(is.infinite(pdf.mat), arr.ind=FALSE)
inf.ID
hilo <- range(pdf.mat[-inf.ID])
hilo

inf.ID.exp <- which(is.infinite(pdf.mat.exp), arr.ind=FALSE)
inf.ID.exp
hilo.exp <- range(pdf.mat.exp[-inf.ID.exp])
hilo.exp


##### 2d plot hypergeometric integral
# over all values of theta and integral
par(mfrow=c(1,2))
plot(intv.x, pdf.mat[1,], col="darkred", bty="n", type="l", pre.plot=grid(),
     main="brob (=log)", ylim=hilo,
     xlab="F1 integral [0 to 1]", ylab="inity [single values simpson rule]")
# all
for(i in 2:sekk.l)
{
  lines(intv.x, pdf.mat[i,], col="darkred")
}
# exp
plot(intv.x, pdf.mat.exp[1,], col="darkred", bty="n", type="l", pre.plot=grid(),
     main="exp(brob)", ylim=hilo.exp,
     xlab="F1 integral [0 to 1]", ylab="inity [single values simpson rule]")
# all
for(i in 2:sekk.l)
{
  lines(intv.x, pdf.mat.exp[i,], col="darkred")
}
#####


##### 3D plot hypergeometric integral
# over all values of theta and integral
par(mfrow=c(1,1))
library(plot3D)
# replace INF values by minimum
pdf.mat.out <- pdf.mat
pdf.mat.out[inf.ID] <- min(pdf.mat.out[-inf.ID])
nbcol = 1000
color = rev(rainbow(nbcol, start = 0/6, end = 4/6))
persp3D(x=sekk, y=seq(0,1,length.out=501), z=pdf.mat.out, 
        theta=50, phi=25, col=color,
        ticktype="detailed",
        xlab="theta difference", ylab="hypergeo integral", zlab="dens",
        lighting=TRUE
)


# plot hypothesis
Xprop.res <- data.frame(sekk,pdf.diff=pdf.num,pdf.diff.log=log(pdf.num))
head(Xprop.res)
tail(Xprop.res)
a1
b1
a2
b2
# plot
plot.bayes.prop.test.Xct(res.Xct=Xprop.res[,c("sekk","pdf.diff")],
                         a1=a1, b1=b1, a2=a2, b2=b2,
                         thetaCs=c(0.05,0.85,1.15), loga=FALSE, drawmcmc=TRUE)
# plot with theta-diff crit = 0.5
plot.bayes.prop.test.Xct(res.Xct=Xprop.res[,c("sekk","pdf.diff")],
                         a1=a1, b1=b1, a2=a2, b2=b2,
                         thetaCs=c(1,1,1), loga=FALSE, drawmcmc=TRUE)

# plot as log
plot.bayes.prop.test.Xct(res.Xct=Xprop.res[,c("sekk","pdf.diff.log")],
                         a1=a1, b1=b1, a2=a2, b2=b2,
                         thetaCs=c(0,1,1), loga=TRUE, drawmcmc=TRUE)

plot.bayes.prop.test.Xct(res=Xprop.res, a1=a1, b1=b1, a2=a2, b2=b2,
                         thetaCs=c(0.1,1,1), loga=FALSE, drawmcmc=TRUE)

################ END FOR BOOK


################ NOT RUN

library(rgl)
color <- rgb(85, 141, 85, maxColorValue=255)
nbcol = 100
color = rev(rainbow(nbcol, start = 0/6, end = 4/6))
zcol  = cut(pdf.mat.out, nbcol)
persp3d(x=sekk, y=seq(0,1,length.out=501), z=pdf.mat.out, theta=50, phi=25, expand=0.75, col=color[zcol],
        ticktype="detailed", xlab="theta difference", ylab="hypergeo integral", zlab="dens",axes=TRUE)

#library(plotly)
#plot_ly(x=intv.x, y=sekk, z=~pdf.mat, type="surface")
################ END OF NOT RUN


######### NOT RUN
### difference between exact and brute force
bf.diff.dens <- density(bf.diff)
MAP.bf <- bf.diff.dens$x[ bf.diff.dens$y == max(bf.diff.dens$y) ]
MAP.Xct
MAP.bf
# theta diff difference
MAP.Xct[,"sekk"]-MAP.bf
# relative ratio
(MAP.Xct[,"sekk"]-MAP.bf)/MAP.Xct[,"sekk"]

### plot results summaries theta difference
par(mfrow=c(1,1))

xlim1 <- c(-1,1)
xlim2 <- c(-1,1)

par(mfrow=c(2,2), mar=c(2,2,7,2), oma=c(4,2,2,2), "cex.axis"=0.8)
# LOG
# exact
plot(sekk, pdf.num.log, col="darkred", bty="n", type="l", pre.plot=grid(), main="exact (brob = log)", xlim=xlim1)
abline(v=MAP.bf, col="blue", lty=2, lwd=2)
abline(v=MAP, col="red", lty=2, lwd=2)
# brute force
plot(bf.diff.dens$x, log(bf.diff.dens$y), col="darkred", bty="n", type="l", pre.plot=grid(), main="brute force (brob = log)", xlim=xlim1)
abline(v=MAP.bf, col="blue", lty=2, lwd=2)
abline(v=MAP, col="red", lty=2, lwd=2)

# EXP
# exact
plot(sekk, pdf.num, col="darkred", bty="n", type="l", pre.plot=grid(), main="exact", , xlim=xlim2)
abline(v=MAP.bf, col="blue", lty=2, lwd=2)
abline(v=MAP, col="red", lty=2, lwd=2)
# brute force
hist(bf.diff, prob=TRUE, main="brute force", pre.plot=grid())
lines(bf.diff.dens, lty=2, col="darkgrey", lwd=2)
abline(v=MAP.bf, col="blue", lty=2, lwd=2)
abline(v=MAP, col="red", lty=2, lwd=2)

mtext(expression(paste("Bayesian Analysis of Difference of Proportions ",theta[2]," - ",theta[1],sep="")),
      3, line=-2, cex=1.6, outer=TRUE)
######### END OF NOT RUN


######### NOT RUN
# par(mfrow=c(2,1))

if(loga == TRUE) addontext <- c("log(",")") else addontext <- c("","")
plot(c(0,0), ylim=ylim, xlim=xlim,
     pre.plot=grid(),
     main="",
     xlab="",
     ylab=eval(substitute(expression(paste(textadd1,"p(",theta[2]," - ",theta[1],")",textadd2,sep="")), list(textadd1=addontext[1],textadd2=addontext[2]))),
     type="l", lty="solid", lwd=1.8, col="white", bty="n", axes=FALSE)

plot.bayes.prop.test.Xct(res=Xprop.res)

plot.bayes.prop.test.Xct(res=Xprop.res[,c(1,2)], a1=a1, b1=b1, a2=a2, b2=b2,
                         thetaCs=c(-0.03,1.1,1.2), loga=FALSE, drawmcmc=TRUE)

####### calculate integral single values over complete theta range and plot 3d
####### + calculate final integral value via simpsonrule
# funs
# simpsonrule.brob.2
# F2.brob
# F2.brob.res <- F2.brob(a = b1, b = a1 + b1 + a2 + b2 - 2, b.prime = 1 - a1, c = a2 + b1, x = 1 + theta, y = 1 - theta^2, ...)
# pdf.theta.diff.MULT
#pres.2x2 <- matrix(data=c(101,131,107,120), ncol=2, byrow=TRUE)
#colnames(pres.2x2) <- c("Bush","Kerry")
#rownames(pres.2x2) <- c("I", "we/nation")
pres.2x2

# posterior beta values to compare (see above)
pres.post.betavalues

# I vs. we/nation
#a1 <- pres.2x2["I","Bush"]
#b1 <- pres.2x2["I","Kerry"]
#a2 <- pres.2x2["we/nation","Bush"]
#b2 <- pres.2x2["we/nation","Kerry"]
a1
b1
a2
b2

# Bush vs. Kerry
a1 <- pres.post.betavalues["a1"]
b1 <- pres.post.betavalues["b1"]
a2 <- pres.post.betavalues["a2"]
b2 <- pres.post.betavalues["b2"]

pres.2x2

#a1 <- pres.2x2["I","Bush"]
#b1 <- pres.2x2["we/nation","Bush"]
#a2 <- pres.2x2["I","Kerry"]
#b2 <- pres.2x2["we/nation","Kerry"]
a1
b1
a2
b2


# data from original paper
a1 <- 1/3+0
b1 <- 1/3+5-0
a2 <- 1/3+2
b2 <- 1/3+5-2
a1
b1
a2
b2

# brute force
library(BayesianFirstAid)
pres.2x2
bayes.prop.test(t(pres.2x2))

set.seed(192934)
bf.N <- 10e6
bf.res1 <- rbeta(n=bf.N, shape1=a1, shape2=b1)
bf.res2 <- rbeta(n=bf.N, shape1=a2, shape2=b2)
# Kerry vs. Bush
bf.diff <- bf.res2 - bf.res1
mean(bf.diff)
low <- floor(min(bf.diff))
hi <- ceiling(max(bf.diff))
low <- min(bf.diff)
hi <- max(bf.diff)
low
hi
low.r <- floor(low*100)/100
hi.r <- ceiling(hi*100)/100
low.r
hi.r

low <- -1
hi <- 1

thetadiff.l <- 100
thetadiff.l <- 500
sekk <-seq(low,hi,length=thetadiff.l)
sekk.l <- length(sekk)
sekk

thetadiff.l <- 100
thetadiff.l <- 500
sekk <-seq(low.r,hi.r,length=thetadiff.l)
sekk.l <- length(sekk)
sekk

F2.brob.list <- list()
orig.list <- list()
pdf.list <- list()

pdf.list.nonlog <- list()
sekk <- seq(-1,1,length=1000)
sekk.l <- length(sekk)
sekk
#
a = b1
b = a1 + b1 + a2 + b2 - 2
b.prime = 1 - a1
c = a2 + b1
x = 1 + theta
y = 1 - theta^2
numer = FALSE
#
# create only simpson rule values and use them later
for(i in 1:sekk.l)
{
  #test <- try( pdf.theta.diff.MULT(theta=sek.pdf.diff[i],
  #                            a1=a1, b1=b1, a2=a2, b2=b2,
  #                            loga=loga[1],
  #                            numer=FALSE)
  #)
  theta <- sekk[i]
  cat(i,"\t",theta,"\n")
  F2.brob.list[[i]] <- try( F2.brob(a = b1, b = a1 + b1 + a2 + b2 - 2,
                              b.prime = 1 - a1,
                              c = a2 + b1,
                              x = 1 + theta,
                              y = 1 - theta^2,
                              numer=FALSE,
                              parallel=TRUE,
                              # limit F1 Appell integral 0<x<1
                              sL=0.0001,
                              sH=0.9999                              )
                              #res1=brob gammas
                              #res2=simpsonrule.brob.2 list
                          )
  orig.list[[i]] <- try( tolerance:::F1(a = b1, b = a1 + b1 + a2 + b2 - 2,
                            b.prime = 1 - a1,
                            c = a2 + b1,
                            x = 1 + theta,
                            y = 1 - theta^2
                                      )
                        )
}


# create only simpson rule values and use them later
for(i in 1:sekk.l)
{
  #test <- try( pdf.theta.diff.MULT(theta=sek.pdf.diff[i],
  #                            a1=a1, b1=b1, a2=a2, b2=b2,
  #                            loga=loga[1],
  #                            numer=FALSE)
  #)
  theta <- sekk[i]
  cat("\ni = ",i,"\ttheta = ",theta,"\n")
  pdf.list[[i]] <- try( pdf.theta.diff.MULT(theta = theta,
                                            a1 = a1,
                                            b1 = b1,
                                            a2 = a2,
                                            b2 = b2,
                                            loga = TRUE,
                                            numer = FALSE,
                                            parallel=TRUE,
                                            debug=FALSE,
#                                            sL=0.00001,
#                                            sH=0.99999
#                                            sL=0.001,
#                                            sH=0.999
  sL=0,
  sH=1    
                                            )
  )
}  

# NON LOG version create only simpson rule values and use them later
loga <- TRUE
loga <- FALSE
for(i in 1:sekk.l)
{
  #test <- try( pdf.theta.diff.MULT(theta=sek.pdf.diff[i],
  #                            a1=a1, b1=b1, a2=a2, b2=b2,
  #                            loga=loga[1],
  #                            numer=FALSE)
  #)
  theta <- sekk[i]
  cat(i,"\t",theta,"\n")
  pdf.list.nonlog[[i]] <- try( pdf.theta.diff.MULT(theta = theta,
                                            a1 = a1,
                                            b1 = b1,
                                            a2 = a2,
                                            b2 = b2,
                                            loga = loga,
                                            numer = TRUE,
                                            parallel=TRUE,
                                            debug=TRUE,
                                            sL=0.000001,
                                            sH=0.999999
                                            )
  )
}  


str(pdf.list.nonlog)
length(pdf.list.nonlog)
str(pdf.list.nonlog[[1]])
pdf.num.nonlog <- as.numeric(lapply( pdf.list.nonlog, "[[",1 ))
data.frame(thetadiff = sekk, dens = pdf.num.nonlog)
pdf.num.nonlog.max <- max(pdf.num.nonlog)
data.frame(thetadiff = sekk[pdf.num.nonlog == pdf.num.nonlog.max],
           dens = pdf.num.nonlog.max)

if(loga)
{
  par(mfrow=c(1,2))
  plot(sekk, exp(pdf.num.nonlog), type="l", bty="n", pre.plot=grid(), col="darkred")
}  
plot(sekk, pdf.num.nonlog, type="l", bty="n", pre.plot=grid(), col="darkred")


###
str(pdf.list)
length(pdf.list)
str(pdf.list[[1]])
pdf.num.log <- as.numeric(lapply( pdf.list, "[[",1 ))
pdf.num <- exp( pdf.num.log )
sekk
# MAP
MAP <- sekk[pdf.num == max(pdf.num)]
MAP
#brute force
summary(bf.diff)
#extract table of in-between results
str(lapply( lapply( lapply( pdf.list, "[[",2 ), "[[",2 ), "[[",1 ))[[1]]
pdf.df <- do.call("rbind", lapply( lapply( lapply( pdf.list, "[[",2 ), "[[",2 ), "[[",1 ) )
dim(pdf.df)
head(pdf.df)
tail(pdf.df)
simpson.l <- dim(pdf.df)[1]/sekk.l
intv.x <- pdf.df[1:simpson.l,"intv.x"]
pdf.mat <- matrix(data=pdf.df[,"inity"], ncol=simpson.l, nrow=sekk.l, byrow=TRUE)
dim(pdf.mat)
length(intv.x)
pdf.mat.exp <- exp( pdf.mat )

# plot integral values from simpsonrule only NOT! final result
par(mfrow=c(1,2))
plot(intv.x, pdf.mat[1,], col="darkred", bty="n", type="l", pre.plot=grid(), main="brob (=log)")
plot(intv.x, exp(pdf.mat[1,]), col="darkred", bty="n", type="l", pre.plot=grid(), main="exp(brob)")

### plot simpson rule values for each theta from pdf.theta.diff [-1 to 1]
# ie. base values of the integral F1 Appell [0 to 1] without summing up
hilo <- range(pdf.mat)
hilo

hilo.exp <- range(pdf.mat.exp)
hilo.exp
# restrict
mean(pdf.mat.exp)
quantile(pdf.mat.exp, probs=seq(0, 1, 0.05))
probrestrict <- 0.90
hilo.exp[2] <- quantile(pdf.mat.exp, probs=(probrestrict))

par(mfrow=c(1,2))
plot(intv.x, pdf.mat[1,], col="darkred", bty="n", type="l", pre.plot=grid(),
     main="brob (=log)", ylim=hilo,
     xlab="F1 integral [0 to 1]", ylab="inity [single values simpson rule]")
# all
for(i in 2:sekk.l)
{
  lines(intv.x, pdf.mat[i,], col="darkred")
}
# exp
plot(intv.x, pdf.mat.exp[1,], col="darkred", bty="n", type="l", pre.plot=grid(),
     main="exp(brob)", ylim=hilo.exp,
     xlab="F1 integral [0 to 1]", ylab="inity [single values simpson rule]")
# all
for(i in 2:sekk.l)
{
  lines(intv.x, pdf.mat.exp[i,], col="darkred")
}

### 3D plot
par(mfrow=c(1,1))
library(plot3D)
persp3D(z=pdf.mat)
#library(plotly)
#plot_ly(x=intv.x, y=sekk, z=~pdf.mat, type="surface")
IDs <- which(pdf.mat.exp == Inf)
IDs
range(pdf.mat.exp[-IDs])
#[1]  0.00000e+00 1.77473e+308
pdf.mat.exp[IDs] <- NA
pdf.mat[IDs] # no NaN values, just huge values!
pdf.mat.exp[IDs] <- 1.77473e+308
#plot_ly(x=intv.x, y=sekk, z=~pdf.mat.exp, type="surface")
#plot_ly(x=intv.x, y=sekk, z=~pdf.mat, type="surface")

pdf.mat.noINF <- pdf.mat
IDs.Inf <- which(pdf.mat.noINF== Inf)
IDs.minusInf <- which(pdf.mat.noINF == -Inf)
IDs.Inf
IDs.minusInf
pdf.mat.noINF[IDs.minusInf]
pdf.mat.noINF[IDs.Inf]

pdf.mat.noINF[IDs.Inf] <- max(pdf.mat.noINF[-IDs.Inf])
pdf.mat.noINF[IDs.minusInf] <- min(pdf.mat.noINF[-IDs.minusInf])
range(pdf.mat.noINF)
persp3D(z=pdf.mat.noINF)
###

### difference between exact and brute force
bf.diff.dens <- density(bf.diff)
MAP.bf <- bf.diff.dens$x[ bf.diff.dens$y == max(bf.diff.dens$y) ]
MAP
MAP.bf
#theta diff difference
MAP.bf-MAP
#relative ratio
(MAP.bf-MAP)/MAP

pres.2x2
### plot results summaries theta_diff

par(mfrow=c(1,1))

# zoom
xlim1 <- c(-0.3,0.3)
xlim2 <- c(-0.2,0.3)

MAP # on log level
sekk[pdf.num.log == max(pdf.num.log)]

### for book
xlim1 <- c(-1,1)
xlim2 <- c(-1,1)

par(mfrow=c(2,2))
# LOG
# exact
plot(sekk, pdf.num.log, col="darkred", bty="n", type="l", pre.plot=grid(), main="brob (=log) [exact]", xlim=xlim1)
abline(v=MAP.bf, col="blue", lty=2, lwd=2)
abline(v=MAP, col="red", lty=2, lwd=2)
# brute force
plot(bf.diff.dens$x, log(bf.diff.dens$y), col="darkred", bty="n", type="l", pre.plot=grid(), main="brob (=log) [brute force]", xlim=xlim1)
abline(v=MAP.bf, col="blue", lty=2, lwd=2)
abline(v=MAP, col="red", lty=2, lwd=2)

# EXP
plot(sekk, pdf.num, col="darkred", bty="n", type="l", pre.plot=grid(), main="exp(brob) [exact]", xlim=xlim2)
abline(v=MAP.bf, col="blue", lty=2, lwd=2)
abline(v=MAP, col="red", lty=2, lwd=2)

hist(bf.diff, prob=TRUE, main="theta diff [brute force}", pre.plot=grid())
lines(bf.diff.dens, lty=2, col="darkgrey", lwd=2)
abline(v=MAP.bf, col="blue", lty=2, lwd=2)
abline(v=MAP, col="red", lty=2, lwd=2)
###

################ END OF NOT RUN


################ NOT RUN
orig.num <- unlist(orig.list)
orig.num
str(F2.brob.list)
F2.brob.list.l <- length(F2.brob.list)

# first element
lapply( F2.brob.list, "[[",1)
unlist(lapply(lapply( lapply( F2.brob.list, "[[",2), "[[",1), function(x) x@x))

# only tables = first element of each second element
F2.brob.res.df <- do.call( "rbind", lapply( lapply( F2.brob.list, "[[",2), "[[",1) )
simpson.l <- dim(table(F2.brob.res.df$intv.x))
intv.x <- F2.brob.res.df[1:simpson.l,"intv.x"]
simpson.l
sekk.l
# create x y matrix
inity.mat <- matrix(data=F2.brob.res.df[,"inity"], ncol=simpson.l, nrow=sekk.l, byrow=TRUE)
dim(inity.mat)
#[1] 100 501
#100 = sekk = theta diff                | -1 to 1
#501 = intv.x = steps in simpson rule   |  0 to 1
head(inity.mat)

par(mfrow=c(1,2))
plot(intv.x, inity.mat[1,], col="darkred", bty="n", type="l", pre.plot=grid(), main="brob (=log)")
plot(intv.x, exp(inity.mat[1,]), col="darkred", bty="n", type="l", pre.plot=grid(), main="exp(brob)")

# deal with infinite values
# otherwise they are DROPPED!
inf.IDs <- which(is.infinite(inity.mat), arr.ind=TRUE)
intv.x[inf.IDs[,2]]
inity.mat[inf.IDs] <- 500 # give maximal value to INF
inity.mat[inf.IDs] <- NA # give maximal value to INF
inity.mat[inf.IDs] <- 0 # give maximal value to INF
hilo <- range(inity.mat)
hilo

### plot simpson rule values for each theta from pdf.theta.diff [-1 to 1]
# ie. base values of the integral F1 Appell [0 to 1] without summing up
par(mfrow=c(1,1))
plot(intv.x, inity.mat[1,], col="darkred", bty="n", type="l", pre.plot=grid(),
     main="exp(brob)", ylim=hilo,
     xlab="F1 integral [0 to 1]", ylab="inity [single values simpson rule]")
for(i in 2:sekk.l)
{
  lines(intv.x, inity.mat[i,], col="darkred")
}



### 3D plot
library(plot3D)
persp3D(z=inity.mat)
#plot_ly(x=intv.x, y=sekk, z=~inity.mat, type="surface")
###



###
# create final sum values (simpson rule + F1 + pdf.theta.diff)
F2.brob.sum.list <- list()
loga <- TRUE

for(i in 1:thetadiff.l)
{
  print(i)
  theta <- sekk[i]
  F2.brob.sum.list[[i]] <- try( pdf.theta.diff.MULT(theta=theta,
                                                    a1=a1, b1=b1, a2=a2, b2=b2,
                                                    loga=loga,
                                                    numer=TRUE,
                                                    # limit F1 Appell integral 0<x<1
                                                    sL=0.0001,
                                                    sH=0.9999)
  )
  
  #F2.brob.list[[i]] <- try( F2.brob(a = b1, b = a1 + b1 + a2 + b2 - 2,
  #                                  b.prime = 1 - a1,
  #                                  c = a2 + b1,
  #                                  x = 1 + theta,
  #                                  y = 1 - theta^2,
  #                                  numer=FALSE
                                    #)
                                    #res1=brob gammas
                                    #res2=ssimpsonrule.brob.2 list
                          #)

}

### a lot of Inf values...
str(F2.brob.sum.list)
F2.brob.sum <- unlist(F2.brob.sum.list)

mean(bf.diff)
median(bf.diff)
summary( exp(F2.brob.sum) )

par(mfrow=c(1,2))
plot(sekk, exp(F2.brob.sum), col="darkred", bty="n", type="l", pre.plot=grid(), main="exp(brob) (non-log)")
abline(v=mean(bf.diff), col="blue", lwd=2, lty=2)
abline(v=median(bf.diff), col="pink", lwd=2, lty=2)
#plot brute force
hist(bf.diff, prob=TRUE)
lines(density(bf.diff), col="darkred", bty="n", type="l", pre.plot=grid(), main="exp(brob) (non-log)", lwd=2)
abline(v=mean(bf.diff), col="blue", lwd=2, lty=2)
abline(v=median(bf.diff), col="pink", lwd=2, lty=2)
lines(sekk, exp(F2.brob.sum), col="orange", lty=2, lwd=2)

#
plot(sekk, F2.brob.sum, col="darkred", bty="n", type="l", pre.plot=grid(), main="brob (=log)")

### the same again, but this time we rule out the inf values
# create x y matrix
inity.mat.noinf <- matrix(data=F2.brob.res.df[,"inity"], ncol=simpson.l, nrow=sekk.l, byrow=TRUE)
dim(inity.mat)
inf.IDs.repl <- which(is.infinite(inity.mat.noinf), arr.ind=TRUE)

intv.x[inf.IDs.repl[,2]]
table(intv.x[inf.IDs.repl[,2]])
hist(intv.x[inf.IDs.repl[,2]])

# archive
# limits on F1 Appell integral
F2.brob.sum.list.001.999 <- unlist(F2.brob.sum.list)
F2.brob.sum.list.0001.9999 <- unlist(F2.brob.sum.list)

brob.nums <- unlist(lapply(F2.brob.res[[2]], as.numeric))
brob.nums
#example list element no 500
brob.list[[2]][[500]]
brob.list[[2]][[500]]@x
brob.list[[2]][[500]]@positive
as.numeric(brob.list[[2]][[500]])
unlist(brob.list[[2]])
brobs.inlogs <- unlist(lapply(brob.list[[2]],function(x) x@x))
brobs.inlogs
#
log(brob.nums)
par(mfrow=c(1,2))
plot(intv.x,brob.nums, col="darkred", bty="n", pre.plot=grid(), type="l")
plot(intv.x,log(brob.nums), col="darkred", bty="n", pre.plot=grid(), type="l")
plot(intv.x,brobs.inlogs, col="darkred", bty="n", pre.plot=grid(), type="l")
abline(h=0, col="grey")
lines(intv.x,abs(brobs.inlogs), col="orange", bty="n", pre.plot=grid(), type="l")

################ END OF NOT RUN


################ NOT RUN
#c(a1,b1,a2,b2)

a1 = 1/3+0
b1 = 1/3+5-0
a2 = 1/3+2
b2 = 1/3+5-2
theta <- 0.5
library(Brobdingnag)


single.OR.Jeffreys <- singletable(a1=0.5, b1=0.5, a2=0.5,
                                  b2=0.5, y1=40, n1=96, y2=49, n2=109,
                                  model="Independent",
                                  measure="RD", method="exact")
summary(single.OR.Jeffreys)
# Analyze the dataset withdrawal to conduct inference of the risk differences
data(withdrawal)
multiple.RD <- multipletables(data=withdrawal, measure="RD",
                             model="Sarmanov")
#summary(multiple.RD)
#plot(multiple.RD, type="forest", addline=0)
#plot(multiple.RD, type="overlap", select=c(3,8,14,16))
#plot(multiple.RD, type="sidebyside", select=c(3,8,14,16))
#plot(multiple.RD, type="sidebyside", select=c(3,8,14,16),
#     ylim=c(0,6), xlim=c(-0.2,0.4))
#print(multiple.RD.table)
#print(multiple.RD.table, type="html")

############# from tolerance
F1 <- function (a, b, b.prime, c, x, y, ...) 
{
  A1.simple <- function(u, a, b, b.prime, c, x, y) {
    u^(a - 1) * (1 - u)^(c - a - 1) * (1 - u * x)^(-b) * 
      (1 - u * y)^(-b.prime)
  }
  gamma(c)/(gamma(a) * gamma(c - a)) * as.numeric(integrate(A1.simple, 
                                       0, 1, a = a, b = b, b.prime = b.prime, c = c, x = x, 
                                       y = y, ...)$value)
}
# red
F1 <- function (...)#(a, b, b.prime, c, x, y, ...) 
{
  A1.simple <- function(u, a, b, b.prime, c, x, y) {
    u^(a - 1) * (1 - u)^(c - a - 1) * (1 - u * x)^(-b) * 
      (1 - u * y)^(-b.prime)
  }
  gamma(c)/(gamma(a) * gamma(c - a)) * as.numeric(integrate(A1.simple, 
                                                            0, 1, a = a, b = b, b.prime = b.prime, c = c, x = x, 
                                                            y = y, ...)$value)
}
###
F1.tw <- function (a, b, b.prime, c, x, y, ...) 
{
  A1.simple <- function(u, a, b, b.prime, c, x, y) {
    u^(a - 1) * (1 - u)^(c - a - 1) * (1 - u * x)^(-b) * 
      (1 - u * y)^(-b.prime)
  }
  res1 <- gamma(c)/(gamma(a) * gamma(c - a))
  res2 <- as.numeric(integrate(A1.simple, 0, 1,
                               a = a, b = b, b.prime = b.prime, c = c, x = x, 
                               y = y, ...)$value)
  print(res1)
  print(log(res1))
  print("^^^res1 + log")
  print(res2)
  print(log(res2))
  print("^^^res2 + log")
return(res1*res2)
}
###
F1(a = b2, b = a1 + b1 + a2 + b2 - 2 , b.prime = 1 - a2, c = a1 + b2, x = 1 - theta, y = 1 - theta^2)
F1.tw(a = b2, b = a1 + b1 + a2 + b2 - 2 , b.prime = 1 - a2, c = a1 + b2, x = 1 - theta, y = 1 - theta^2)
###

a = b2
b = a1 + b1 + a2 + b2 - 2
b.prime = 1 - a2
c = a1 + b2
x = 1 - theta
y = 1 - theta^2

pre <- data.frame(a1,b1,a2,b2,theta,a,b,b.prime,c,x,y)
colnames(pre) <- c("a1","b1","a2","b2","theta","a","b","b.prime","c","x","y")
pre

### different functions from F1
A1.simple <- function(u, a, b, b.prime, c, x, y) {
  u^(a - 1) * (1 - u)^(c - a - 1) * (1 - u * x)^(-b) * 
    (1 - u * y)^(-b.prime)
}
#
A1.simple.red <- function(u) {#, a, b, b.prime, c, x, y
  u^(a - 1) * (1 - u)^(c - a - 1) * (1 - u * x)^(-b) * 
    (1 - u * y)^(-b.prime)
}
A1.simple.red(0.501996)

A1.simple.brob <- function(u, a, b, b.prime, c, x, y) {
  u^(a - 1) * (1 - u)^(c - a - 1) * (1 - u * x)^(-b) * 
    (1 - u * y)^(-b.prime)
}

A1.simple.brob.1 <- function(u) {
  u^(a - 1) * (1 - u)^(c - a - 1) * (1 - u * x)^(-b) * 
    (1 - u * y)^(-b.prime)
}
##### in-between calculations
#exp(lgamma(c) - lgamma(a) - lgamma(c-a))
#lgamma(c) - lgamma(a) - lgamma(c-a)
#gamma(c) / (gamma(a) * gamma(c-a))
## CORRECT:
#brob(lgamma(c) - lgamma(a) - lgamma(c-a))
#####

###
###### function to calculate si, Ni from a, b (back!)
rev.ab.lik <- function(a,b)
{
  si <- a - 1
  Ni <- b + si - 1
  return(c(si=si,Ni=Ni))
}
bino.ab.lik(17,23)
rev.ab.lik(18,7)
########################## END OF FUNCTION

###
F1.brob <- function (a, b, b.prime, c, x, y, sL=0, sH=1, numer=TRUE, ...) 
{
  
  require(Brobdingnag)
  
  A1.simple.brob.2 <- function(u) {
    as.brob(u) ^ (a-1) *
    as.brob(1-u) ^ (c-a-1) *
    as.brob(1-u*x) ^ (-b) *
    as.brob(1-u*y) ^ (-b.prime)
  }
  
  res1 <- brob(lgamma(c)-lgamma(a) - lgamma(c - a))
  res2 <- simpsonrule.brob.1(A1.simple.brob.2, sL=sL, sH=sH, Nsteps=250, numer=numer)
  
  #print(res1)
  #print(log(res1))
  #print("^^^res1 + log")
  #print(res2)
  #print(log(res2))
  #print("^^^res2 + log")
  if(numer) res <- res1*res2 else res <- list(res1=res1,res2=res2)
return(res)
}
###



#identical
A1.simple.brob.1(0.5)
exp(A1.simple.brob.2(0.5)@x)

#
simpsonrule.brob.2(A1.simple.brob.2, sL=sL, sH=sH, Nsteps=250)
#
integrate(A1.simple.brob.1, 0.0000, 1)
sekk <- seq(0,1,length=100)
sekk <- seq(0.0001,0.9999,length=100)
sintegral(sekk,A1.simple.brob.1(sekk))
simpsonrule.brob.1(A1.simple.brob.1, 0.0001, 0.9999, Nsteps=500)
simpsonrule.brob.1(A1.simple.brob.1, 0.00001, 0.99999, Nsteps=1000)
simpsonrule.brob.1(A1.simple.brob.2, 0.00001, 0.99999, Nsteps=1000)
simpsonrule.brob.1(A1.simple.brob.2, 0, 1, Nsteps=100)
xyz <- simpsonrule.brob.2(A1.simple.brob.2, 0, 1, Nsteps=100)

integrate(A1.simple, 0, 1, a=a, b=b, b.prime=b.prime, c=c, x=x, y=y)
integrate(A1.simple.brob, 0, 1, a=a, b=b, b.prime=b.prime, c=c, x=x, y=y)
integrate(A1.simple.brob.1, 0.00001, 0.99988)
integrate(A1.simple.brob.1, 0.00001, 0.99999)
integrate(A1.simple.brob.1, 0.00001, 1)
integrate(A1.simple.brob.1, 0.0000, 1)
simpsonrule.brob.1(A1.simple.brob.1, 0.001, 0.999, Nsteps=1000)
simpsonrule.brob.1(A1.simple.brob.1, 0.000001, 0.99999, Nsteps=1000)
simpsonrule.brob.1(A1.simple.brob.1, 0, 1, Nsteps=1000)
simpsonrule.brob(A1.simple.brob, sL=0, sH=1)

#
temp <- simpsonrule.brob.2(A1.simple.brob.2, 0, 1, Nsteps=100)
str(temp)
temp <- unlist(lapply(temp[[1]], as.numeric))
plot(temp[-c(201)], type="l", col="darkred", bty="n")

###
x<-seq(-3,3,length=1000)
fx<-dnorm(x)
sintegral(x,fx)$int
simpsonrule.brob(fx=dnorm, sL=-3, sH=3, Nsteps=1000)

###
F1(a = b2, b = a1 + b1 + a2 + b2 - 2 , b.prime = 1 - a2, c = a1 + b2, x = 1 - theta, y = 1 - theta^2)
F1.tw(a = b2, b = a1 + b1 + a2 + b2 - 2 , b.prime = 1 - a2, c = a1 + b2, x = 1 - theta, y = 1 - theta^2)
###
res <- F1.brob(a = b2, b = a1 + b1 + a2 + b2 - 2 , b.prime = 1 - a2, c = a1 + b2, x = 1 - theta, y = 1 - theta^2)
res
exp(res@x)

pre <- data.frame(a1,b1,a2,b2,theta,a,b,b.prime,c,x,y)
colnames(pre) <- c("a1","b1","a2","b2","theta","a","b","b.prime","c","x","y")
pre



  
### DEV BROB
#############  FUNCTION
simpsonrule.brob.1 <- function(fx, sL, sH, Nsteps=100, numer=TRUE, pr.out=FALSE, ...)
{
  # taken from sintegral from Bolstad2
  sek <- seq(sL,sH,length=Nsteps)
  l.intv <- 2*Nsteps+1
  intv.x <- approx(sek,sek,n=l.intv)$x
  h <- diff(intv.x)[1]
  inity <- as.list(sapply(seq_along(1:l.intv), function(x) fx(intv.x[x], ...)))
  inity1 <- inity[2 * (1:Nsteps) - 1]
  inity2 <- inity[2 * (1:Nsteps)]
  inity3 <- inity[2 * (1:Nsteps) + 1]
  # print out list of in-between results if required
  if(pr.out) print(inity)
  sum <- 0
  for(i in inity1) sum <- sum + unlist(i)
  for(i in inity2) sum <- sum + 4*unlist(i)
  for(i in inity3) sum <- sum + unlist(i)
  sum <- sum*(h/3)
  # give back numerical summary or list of in-between-results
  if(!numer) return(inity) else return(sum)
}
############# END OF FUNCTION


############# FUNCTION
simpsonrule.brob <- function(fx, sL, sH, Nsteps=100)
{
  # taken from sintegral from Bolstad2
  sek <- seq(sL,sH,length=Nsteps)
  l.intv <- 2*Nsteps+1
  intv.x <- approx(sek,sek,n=l.intv)$x
  h <- diff(intv.x)[1]
  inity <- as.list(sapply(seq_along(1:l.intv), function(x) fx(intv.x[x])))
  inity1 <- inity[2 * (1:Nsteps) - 1]
  inity2 <- inity[2 * (1:Nsteps)]
  inity3 <- inity[2 * (1:Nsteps) + 1]
  sum <- 0
  for(i in inity1) sum <- sum + unlist(i)
  for(i in inity2) sum <- sum + 4*unlist(i)
  for(i in inity3) sum <- sum + unlist(i)
  sum <- sum*(h/3)
  return(sum)
}
############# END OF FUNCTION

################ NOT RUN


################ NOT RUN
a1 <- 102 
b1 <- 132 
a2 <- 108 
b2 <- 121 
theta.res <- prop.theta.sek(a1,b1,a2,b2,
                            xlim.diff=c(0.54,0.57,-0.2,0.2),
					              		xlim.RR=c(0.95,1.01,0.5,3),
							              xlim.OR=c(0,2.5,0,2.1),
              							theta.crit=c(0,1,1),
							              loga=c(T,T,F))
#prop.theta.sek.plot(theta.res)							
		
theta.res.log <- prop.theta.sek(a1,b1,a2,b2,
                            xlim.diff=c(0.54,0.57,-0.2,0.2),
                            xlim.RR=c(0.95,1.01,0.5,3),
                            xlim.OR=c(0,2.5,0,2.1),
                            theta.crit=c(0,1,1),
                            loga=c(T,T,T),
                            logaplot=c(T,T,T))
#prop.theta.sek.plot(theta.res.log)

theta.res <- prop.theta.sek(xlim.diff=c(-1,0.5,-1,0.2), xlim.RR=c(0,2,0,4), xlim.OR=c(0,2.5,0,2), fac2=1.2)
#prop.theta.sek.plot(theta.res)
theta.res <- prop.theta.sek(xlim.RR=c(0,100), xlim.OR=c(0,100))
#prop.theta.sek.plot(theta.res)
theta.res <- prop.theta.sek(xlim.RR=c(0,3,0,4), xlim.OR=c(0,2))
#prop.theta.sek.plot(theta.res)
theta.res$meta$HDI <- c(T,T,T)
prop.theta.sek.plot(theta.res)
str(theta.res)


# not run
pdf.theta.diff(theta=0.5, a1 = 1/3+0, b1 = 1/3+5-0, a2 = 1/3+2, b2 = 1/3+5-2, loga=FALSE)
exp(pdf.theta.diff(theta=0.5, a1 = 1/3+0, b1 = 1/3+5-0, a2 = 1/3+2, b2 = 1/3+5-2, loga=TRUE))
# original paper version:
d2beta(relation="DIFF", x = 0.5, a1 = 1/3+0, b1 = 1/3+5-0, a2 = 1/3+2, b2 = 1/3+5-2)
# end of not run


# presidential data
a1 <- 102
b1 <- 108
a2 <- 132
b2 <- 121

Si <- 101
Ni <- 208
Sii <- 131
Nii <- 251


# not run
# library(tolerance)
# concrete parameters

# values too large...
qdiffprop(p=0.5, k1=Si, k2=Sii, n1=Ni, n2=Nii)
qdiffprop(p=0.5, k1=Si, k2=Sii, n1=Ni, n2=Nii, log=TRUE)
cdf.theta.diff(theta=0.5, a1=a1,b1=b1,a2=a2,b2=b2)
# from original paper by Sverdlov et al
p2beta("DIFF", "DIRECT", x=0.5, a1=a1,b1=b1,a2=a2,b2=b2, n = 1000000)
p2beta("DIFF", "SIMULATION", x=0.5, a1=a1,b1=b1,a2=a2,b2=b2, n = 1000000)
# all the same

# comparison of the various methods
sek <- seq(-1,1,0.01)
out <- data.frame(sek,matrix(data=NA, ncol=4, nrow=length(sek)))
colnames(out) <- c("theta","qdiffprop","cdf.theta.diff","p2beta|DIRECT","p2beta|SIM")
head(out)
tail(out)

for(i in 1:dim(out)[1])
{
  print(i)
  
  test0 <- try( qdiffprop(p=out[i,"theta"], k1=Si, k2=Sii, n1=Ni, n2=Nii) )
  if(is.numeric(test0)) out[i,"qdiffprop"] <- test0 else out[i,"qdiffprop"] <- NA
  
  test1 <- try( cdf.theta.diff(theta=out[i,"theta"], a1=a1,b1=b1,a2=a2,b2=b2) )
  if(is.numeric(test1)) out[i,"cdf.theta.diff"] <- test1 else out[i,"cdf.theta.diff"] <- NA
  
  test2 <- try( p2beta("DIFF", "DIRECT", x=out[i,"theta"], a1=a1,b1=b1,a2=a2,b2=b2) )
  if(is.numeric(test2)) out[i,"p2beta|DIRECT"] <- test2 else out[i,"p2beta|DIRECT"] <- NA
  
  test3 <- try( p2beta("DIFF", "SIMULATION", x=out[i,"theta"], a1=a1,b1=b1,a2=a2,b2=b2, n=1e5) )
  if(is.numeric(test3)) out[i,"p2beta|SIM"] <- test3 else out[i,"p2beta|SIM"] <- NA
}
head(out)
tail(out)

# plot
cols <- adjustcolor(c("darkred","steelblue","green","orange"), alpha=0.5)
par(oma=c(2,1,1,1), "cex.axis"=1, bty="l")
plot(sek, out[,"qdiffprop"], col=cols[1], ylim=c(-1,1), type="l", bty="n", pre.plot=grid(), lwd=2, xlab=expression(paste(theta)), cex.lab=1.2)
lines(sek, out[,"cdf.theta.diff"], col=cols[2], lwd=2)
lines(sek, out[,"p2beta|DIRECT"], col=cols[3], lwd=2)
lines(sek, out[,"p2beta|SIM"], col=cols[4], lwd=2)
legend("right", legend=colnames(out)[2:5], lty=1, lwd=2, xpd=TRUE, horiz=FALSE, col=cols, bty="n", cex=.9)
mtext("Comparison of proportion difference calc methods", outer=TRUE, line=-2, cex=1.5, side=3)

# it seems the qdiffprop is problematic! all other curves are roughly identical (ie. plottet over each other)

# even that does not work
jitter( lines(sek, out[,"cdf.theta.diff"], col=cols[2], lwd=2) )
jitter( lines(sek, out[,"p2beta|DIRECT"], col=cols[3], lwd=2) )
jitter( lines(sek, out[,"p2beta|SIM"], col=cols[4], lwd=2) )
# end of not run

################ END OF NOT RUN


################ NOT RUN
# presidential data
# theta.crit = 0.5 for diff as default!
# full spectrum - no log
theta.res <- prop.theta.sek(a1=a1,b1=b1,a2=a2,b2=b2,loga=c(F,F,F))
prop.theta.sek.plot(theta.res)
# full spectrum - log
theta.res <- prop.theta.sek(a1=a1,b1=b1,a2=a2,b2=b2,logaplot=c(T,T,T))
prop.theta.sek.plot(theta.res)
# zoom in
theta.res <- prop.theta.sek(a1=a1,b1=b1,a2=a2,b2=b2,xlim.diff=c(0.4,0.6,-0.2,0.6),xlim.RR=c(0,100,-1,10),xlim.OR=c(0,100,0,10), logaplot=c(T,T,T))
prop.theta.sek.plot(theta.res)
# zoom further in
theta.res <- prop.theta.sek(a1=a1,b1=b1,a2=a2,b2=b2,xlim.diff=c(0.4,0.6,-0.2,0.6),xlim.RR=c(0,100,-1,3),xlim.OR=c(0,100,0,3), logaplot=c(T,T,T))
prop.theta.sek.plot(theta.res)


theta.res <- prop.theta.sek(a1=a1,b1=b1,a2=a2,b2=b2,xlim.diff=c(0.35,0.7,-0.2,0.6),xlim.RR=c(0,100,-1,3),xlim.OR=c(0,100,0,3), logaplot=c(T,T,T))
theta.res$meta$HDI <- c(T,T,T)
prop.theta.sek.plot(theta.res)


# not run
# plot en passent
par(oma=c(2,1,1,1), "cex.axis"=1, bty="l", mfrow=c(3,2))
lapply(theta.res$post$differ, function(x) plot(x[,1],x[,2], type="l", col="darkred", pre.plot=grid(), bty="n"))
lapply(theta.res$post$RR, function(x) plot(x[,1],x[,2], type="l", col="darkred", pre.plot=grid(), bty="n"))
lapply(theta.res$post$OR, function(x) plot(x[,1],x[,2], type="l", col="darkred", pre.plot=grid(), bty="n"))
# end of not run


a1
b1
a2
b2

# example presidential debates
# posterior beta-values
a1 <- 102
b1 <- 132
a2 <- 108
b2 <- 121

si <- 101
Ni <- 208
sii <- 131
Nii <- 251
sekk <- seq(-0.999,0.999,0.01)
sekk <- seq(0.54,0.57,0.01)

a1 = 1/3+0
b1 = 1/3+5-0
a2 = 1/3+2
b2 = 1/3+5-2

siNi <- rev.ab.lik(a1,b1)
siiNii <- rev.ab.lik(a2,b2)

siNi
siiNii

# pdf.diff <- d2beta.r <- rep(NA,length(sekk))
pdf.diff <- rep(NA,length(sekk))
for(i in 1:length(sekk))
{
 print(i)
  test <- try( pdf.theta.diff(
    theta = sekk[i],
    a1 = a1,
    b1 = b1,
    a2 = a2,
    b2 = b2,
    loga = T
  ) )
 if(is.numeric(test)) pdf.diff[i] <- test else pdf.diff[i] <- NA
 #d2beta.r <- try( d2beta(relation='DIFF', x=sekk[i], a1, b1, a2, b2) )
 #if(is.numeric(test)) d2beta.r[i] <- test else d2beta.r[i] <- NA
}

################ END OF NOT RUN


################ NOT RUN
Xprop.res <- data.frame(sekk,pdf.diff)
#workaround
infIDs <- which(Xprop.res[,"pdf.diff"] == -Inf | Xprop.res[,"pdf.diff"] == Inf)
infIDs
Xprop.res.nonInf <- Xprop.res[-infIDs,]
Xprop.res.nonInf[,"pdf.diff"] <- exp(Xprop.res.nonInf[,"pdf.diff"])
head(Xprop.res.nonInf)
tail(Xprop.res.nonInf)
head(Xprop.res)
tail(Xprop.res)



plot.bayes.prop.test.Xct(res=Xprop.res[,c(1,2)], si=siNi["si"], Ni=siNi["Ni"], sii=siiNii["si"], Nii=siiNii["Ni"], a1=a1, b1=b1, a2=a2, b2=b2,
                         thetaCs=c(0.3,1.5,1.2), loga=TRUE, drawmcmc=TRUE)

plot.bayes.prop.test.Xct(res=Xprop.res[,c(1,2)], a1=a1, b1=b1, a2=a2, b2=b2,
                         thetaCs=c(0.3,1.5,1.2), loga=TRUE, drawmcmc=TRUE)

# res.Xct <- Xprop.res
plot.bayes.prop.test.Xct(res=Xprop.res[,c(1,2)], si=si, Ni=Ni, sii=sii, Nii=Nii, a1=a1, b1=b1, a2=a2, b2=b2,
						 thetaCs=c(0.3,1.5,1.2), loga=TRUE, drawmcmc=FALSE)

plot.bayes.prop.test.Xct(res=Xprop.res[,c(1,2)], si=si, Ni=Ni, sii=sii, Nii=Nii, a1=a1, b1=b1, a2=a2, b2=b2,
						 thetaCs=c(0.3,1.5,1.2), loga=TRUE, drawmcmc=TRUE)


plot.bayes.prop.test.Xct(res=Xprop.res[,c(1,2)], si=si, Ni=Ni, sii=sii, Nii=Nii, a1=a1, b1=b1, a2=a2, b2=b2,
                         thetaCs=c(0.01,1.1,1.2), loga=TRUE, drawmcmc=TRUE)

plot.bayes.prop.test.Xct(res=Xprop.res.nonInf, a1=a1, b1=b1, a2=a2, b2=b2,
                         thetaCs=c(0.5,1.1,1.2), loga=TRUE, drawmcmc=TRUE)
################ END OF NOT RUN




################ NOT for BOOK
### MCMC drawing incl. thetaC (= theta crit) & ROPE (Kruschke, ...)
# define parameters

pres.2x2

# Bush vs. Kerry
a1 <- pres.post.betavalues["a1"]
b1 <- pres.post.betavalues["b1"]
a2 <- pres.post.betavalues["a2"]
b2 <- pres.post.betavalues["b2"]
a1
b1
a2
b2

# roughly at MAP
pdf.theta.diff.MULT(theta=0.03, a1=a1, b1=b1, a2=a2, b2=b2, loga=TRUE, BROB=TRUE)


### not required
for(i in seq(-1,1,0.01))
{   
  print(i)
  print(
    pdf.theta.diff.MULT(theta=i, 
                        a1=a1, b1=b1, a2=a2, b2=b2,
                        loga=TRUE, numer=TRUE, BROB=TRUE)
    )
}
### END OF not required


n.mcmc <- 1e4
# arbitrary values
thetaCs <- c(0.05,1.5,1.2)
thetaC <- thetaCs[1]
credMass <- 0.87
seed <- 2846
set.seed(seed)

# create data
mcmc1 <- rbeta(n.mcmc, a1, b1)
mcmc2 <- rbeta(n.mcmc, a2, b2)

# calculate difference
differ <- mcmc2 - mcmc1
# calculate ratio
RR <- mcmc2 / mcmc1
# calculate odds ratio
OR <- (mcmc2/(1-mcmc2)) / (mcmc1/(1-mcmc1))
# p(difference < thetaC) = MCMC based
prob.mcmc <- mean(differ < thetaC)
prob.mcmc
# p(difference > thetaC) = MCMC based
1/prob.mcmc

# exact solution
prob.Xct <- cdf.theta.diff(theta=thetaC, a1, b1, a2, b2)
prob.Xct
# p(difference < thetaC) = exact solution
all.equal(prob.mcmc, prob.Xct)
# general
median(differ)
mean(differ)

# plot
# library(BEST)
par(oma=c(2,1,1,1), "cex.axis"=1, bty="l", mfrow=c(2,2))
# differ
xlim <- c(-0.2,0.6)
ROPE <- c(-0.1,0.1)
plotPost(differ, credMass=credMass, compVal=thetaCs[1], ROPE=ROPE, xlab=expression(paste(theta[2]-theta[1],sep="")), xlim=xlim, main="")
# ratio
xlim <- c(0.5,1.7)
ROPE <- c(1.2,1.5)
plotPost(RR, credMass=credMass, compVal=thetaCs[2], ROPE=ROPE, xlab=expression(paste("RR ",theta[2]/theta[1],sep="")), xlim=xlim, main="")
# odds ratio
xlim <- c(0,2)
ROPE <- c(0.5,1.1)
plotPost(OR, credMass=credMass, compVal=thetaCs[3], ROPE=ROPE, xlab=expression(paste("OR ",theta[2]," vs. ",theta[1],sep="")), xlim=xlim, main="")
mtext("Bayesian Proportion Test (Difference, RR, OR)", outer=TRUE, line=-2, cex=1.5, side=3)


 
### Brute force overview
seed <- 29567
set.seed(seed)

# criteria for difference, ratio, and odds ratio
thetaCs <- c(0.5,1.5,1.5)

# MCMC chains
RB1 <- rbeta(1e+6, shape1=a1, shape2=b1)
RB2 <- rbeta(1e+6, shape1=a2, shape2=b2)

# difference
mean(RB2-RB1 < thetaCs[1])

# ratio
mean(RB2/RB1 < thetaCs[2])

# odds ratio
mean( (RB2/(1-RB2)) / (RB1/(1-RB1)) < thetaCs[3])

# densities
DBS <- seq(0,1,0.01)
DB1 <- dbeta(DBS, shape1=a1, shape2=b1)
DB2 <- dbeta(DBS, shape1=a2, shape2=b2)

# comparisons
critcompare <- c(-rev(DBS),DBS)
critp <- data.frame(critcompare,p=sapply(critcompare, function(i) mean(RB2-RB1 < i)))
head(critp)
tail(critp)
temp.p <- unique(critp[,2])
# only values above zero and below one
critp[which(critp[,2] %in% temp.p[temp.p > 0 & temp.p < 1]),]

CRIT <- sapply(critcompare, function(i) mean(RB2-RB1 < i))
str(CRIT)
summary(CRIT)

bayes.prop.BForce(a1=102,b1=108,a2=132,b2=121, diffcompare="smaller", thetaCs=c(0.5,1.5,1.5), DBS=seq(0,1,0.01), n.mcmc=1e6, seed=29567, PLOT=TRUE)

bayes.prop.BForce(diffcompare="smaller")
bayes.prop.BForce(diffcompare="bigger")
bayes.prop.BForce(diffcompare="bigger", thetaCs=c(0.03,1.5,1.5))



### Bayes Factor


# example presidential debates
# posterior beta-values
pres.2x2
addmargins(pres.2x2)
Si <- pres.2x2["I","Bush"]
Ni <- sum(pres.2x2[,"Bush"])
Sii <- pres.2x2["I","Kerry"]
Nii <- sum(pres.2x2[,"Kerry"]) 
Si
Ni
Sii
Nii

a1 <- Si + 1
b1 <- Ni-Si+1
a2 <- Sii + 1
b2 <- Nii - Sii + 1



# library(BayesFactor)
bf.prop.pres <- proportionBF(y=c(Si,Sii), N=c(Ni,Nii), p=0.5)
# H1
bf.prop.pres
# H0
1/bf.prop.pres
# mcmc
nsims <- 1e5
bf.prop.mcmc <- posterior(bf.prop.pres, iterations=nsims)
plot(bf.prop.mcmc, col="darkred", bty="n")
colnames(bf.prop.mcmc)
# MAP
dens.mcmc <- density(bf.prop.mcmc[,"p"])
max.dens <- max(dens.mcmc$y)
MAP <- dens.mcmc$x[dens.mcmc$y == max.dens]
c("MAP"=MAP, "density"=max.dens)
### END OF NOT RUN


# classical
prop.test(x=c(Si,Sii), n=c(Ni,Nii))
prop.test(x=c(Si,Sii), n=c(Ni,Nii), p=c(0.5,0.5))


pres.2x2
chisq.test(pres.2x2)
bf.pres <- contingencyTableBF(pres.2x2, sampleType="indepMulti", fixedMargin="cols")
# H1
bf.pres
# H0
1/bf.pres

nsims <- 1e5
bf.cont.mcmc <- posterior(bf.pres, iterations=nsims)
bf.cont.mcmc
colnames(bf.cont.mcmc)
sametermsgivenBush <- bf.cont.mcmc[,"pi[1,1]"] / bf.cont.mcmc[,"pi[*,1]"]
sametermsgivenKerry <- bf.cont.mcmc[,"pi[1,2]"] / bf.cont.mcmc[,"pi[*,2]"]
theta.diff <- sametermsgivenKerry - sametermsgivenBush
mean(theta.diff)
# plot
par(oma=c(2,1,1,1), "cex.axis"=1, bty="l")
plot(mcmc(theta.diff), main="", col="darkred", bty="n")
plot(theta.diff, main="", col="darkred", bty="n", pre.plot=grid())
mtext(expression(paste("Bush vs. Kerry (",theta[1],"-",theta[2],")",sep="")), outer=TRUE, line=-2, cex=1.5, side=3)
# library(BEST)
plotPost(theta.diff, credMass=0.87, ROPE=c(-0.1,0.1), xlab="theta difference", showMode=TRUE, col="skyblue", border="white", compVal=0.25)

# MAP
dens.mcmc <- density(theta.diff)
max.dens <- max(dens.mcmc$y)
MAP <- dens.mcmc$x[dens.mcmc$y == max.dens]
c("MAP"=MAP, "density"=max.dens)
sd(theta.diff)



### UM Studer (1999) solution
# via GL Bretthorst paper 'Difference in Means'
pres.2x2
addmargins(pres.2x2)
Si <- pres.2x2["I","Bush"]
Ni <- sum(pres.2x2[,"Bush"])
Sii <- pres.2x2["I","Kerry"]
Nii <- sum(pres.2x2[,"Kerry"]) 
Si
Ni
Sii
Nii

res.SIB.pres <- SucRatesIntBounds(Si=Si, Ni=Ni, Sii=Sii, Nii=Nii, smin=0, snames=c("I","we/nation"))
# res.SIB.pres
#$L
#[1] 0
#$H
#[1] 1
#$sL
#[1] 0.003
#$sH
#[1] 0.035
res.SIB.pres.upd <- res.SIB.pres

# "priors"

# just different
res.SIB.pres.upd["L"] <- 0.42
res.SIB.pres.upd["H"] <- 0.43
res.SIB.pres.upd["sL"] <- 0.03
res.SIB.pres.upd["sH"] <- 0.1

# less narrow, but not completely wide
# informed prior for means
# less informed for variances - more wide
res.SIB.pres.upd["L"] <- 0.4
res.SIB.pres.upd["H"] <- 0.6
res.SIB.pres.upd["sL"] <- 0.01
res.SIB.pres.upd["sH"] <- 0.1

DiM.res.pres <- DiffinMeans(inval=res.SIB.pres.upd, out=FALSE, BROB=TRUE)
UMSprint(results=DiM.res.pres)
UMSplot(inval=res.SIB.pres.upd,pdfout=FALSE)

# compare with classical
chisq.test(pres.2x2)

# Si, Ni
theta <- seq(0,1,0.01)
pbl.res <- pbl(theta=theta, si=Si, Ni=Ni, loga=TRUE, reexp=TRUE)
pjc.res <- pjc(theta=theta, si=Si, Ni=Ni, loga=TRUE, reexp=TRUE)
head(pbl.res)
tail(pbl.res)
head(pjc.res)
tail(pjc.res)
sN.ME.res <- data.frame(pbl.res, pjc.res)
head(sN.ME.res)
tail(sN.ME.res)
plot.bl.jc(theta, sN.ME.res=sN.ME.res, si=Si, Ni=Ni, filling=FALSE)
plot.bl.jc(theta, sN.ME.res=sN.ME.res, si=Si, Ni=Ni, filling=TRUE)
sN.ME.post.summary <- sN.post.su(Ni=Ni, si=Si)
sN.ME.post.summary

# Sii, Nii
theta <- seq(0,1,0.01)
pbl.res <- pbl(theta=theta, si=Sii, Ni=Nii, loga=TRUE, reexp=TRUE)
pjc.res <- pjc(theta=theta, si=Sii, Ni=Nii, loga=TRUE, reexp=TRUE)
head(pbl.res)
tail(pbl.res)
head(pjc.res)
tail(pjc.res)
sN.ME.res <- data.frame(pbl.res, pjc.res)
head(sN.ME.res)
tail(sN.ME.res)
plot.bl.jc(theta, sN.ME.res=sN.ME.res, si=Sii, Ni=Nii, filling=FALSE)
plot.bl.jc(theta, sN.ME.res=sN.ME.res, si=Sii, Ni=Nii, filling=TRUE)
sN.ME.post.summary <- sN.post.su(Ni=Nii, si=Sii)
sN.ME.post.summary
