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
# ptII_quan_classicstats_p-t-df-relationship.r

# location:
# chap. 4 [4.5.9]
# Erkenntnistheorie reloaded — klassische Statistik

# load helper functions
source("ptall_generalfuncs.r")


# relationship N, t, p
seed <- 567
N <- seq(30,1000,10)
ts <- vector()
pvs <- vector()
mu <- 0.5
sd <- 1.2
usesameseed <- TRUE
usesameseed <- FALSE
for(i in 1:length(N))
{
  print(N[i])
  if(usesameseed) set.seed(seed)
  samp <- rnorm(N[i], mu, sd)
  samp.t.test <- t.test(samp)
  ts[i] <- samp.t.test$statistic
  pvs[i] <- samp.t.test$p.value
  
}
par(mfrow=c(1,3), oma=c(2,1,4,1), "cex.axis"=1, bty="l")
plot(N, ts, bty="n", type="l", col="darkred", pre.plot=grid())
plot(N, log(pvs), bty="n", type="l", col="darkred", pre.plot=grid())
plot(ts, log(pvs), bty="n", type="l", col="darkred", pre.plot=grid())
mtext("Dependence N, t and p-value", side=3, line=0.8, cex=2, outer=TRUE)
mtext(paste("same seed = ",usesameseed,sep=""), side=3, line=-1.8, cex=1.5, outer=TRUE)


# relationship df and p value in face of growing df's and const. t values
sek3 <- 1:100
tvalue <- 3
par(mar=c(4,4,2,1), oma=c(1,1,3,1), "cex.axis"=0.8, mfrow=c(2,1))
plot(sek3,2*pt(-tvalue,df=sek3), panel.first=grid(), type="l", col="red", bty="l", main="", xlab="df", ylab="p") 
plot(sek3,log(2*pt(-tvalue,df=sek3)), panel.first=grid(), type="l", col="red", bty="l", main="", xlab="df", ylab="log(p)") 
mtext(paste("Relationship of df and p-values",sep=""), 3, outer=TRUE, line=0.5, cex=1.4)
mtext(paste("t-values (t = ",tvalue,") are held constant",sep=""), 3, outer=TRUE, line=-1, cex=1.1)


# post hoc power via simulation

# bootstrap parametric model post hoc power
#
# t-test
seed <- 9876
set.seed(seed)
alpha <- 0.01
trials <- 1000
LM <- FALSE
#LM <- TRUE
sim.DiM <- function(LM=TRUE,n1=25, n2=25,mu1=6.5,mu2=7.8,s1=2,s2=2)
{
  a <- round(rnorm(n=n1, mean=mu1, sd=s1))
  b <- round(rnorm(n=n2, mean=mu2, sd=s2))
  if(LM)
  {
    res <- summary(lm(a ~ b))$coef["b","Pr(>|t|)"]
  } else
  {
    res <- t.test(a, b)$p.value
  }
return(res)  
}
power.boot.res <- replicate(trials, sim.DiM(LM=LM))
#describes(power.boot.res)
#summary(power.boot.res)
fivenum.wn(power.boot.res)
sd(power.boot.res)

# plot
par(mar=c(4,4,2,1), oma=c(1,1,3,1), "cex.axis"=0.8, mfrow=c(2,1))
hist(power.boot.res, panel.first=grid(), prob=TRUE, border="white", col="skyblue", main="", xlab="p", ylab="density", breaks="FD")
# restrict p-value due to density estimation algorithm
# only densities above zero and below 1
dens.pbr <- density(power.boot.res)
dens.pbr$x[dens.pbr$x <= 0] <- .Machine$double.eps
dens.pbr$x[dens.pbr$x > 1] <- 1-.Machine$double.eps
lines(dens.pbr, col="red", lwd=2)

# power for different p-values
alphas <- c(0.1,0.05,0.01,0.001)
for(a in alphas) cat( paste("alpha = ",a,"\t|\tpower = ",length(power.boot.res[power.boot.res < a])/trials,"\n",sep="" ))

alphas <- seq(0.001,0.5, length.out=1000)
powers <- unlist(lapply(seq_along(alphas), function(i) length(power.boot.res[power.boot.res < alphas[i]])/trials))
plot(alphas, powers, panel.first=grid(), ylim=c(0,1), type="l", col="red", bty="l", main="", xlab=expression(alpha), ylab="power") 
if(LM) testtype <- c("linear model") else testtype <- c("t-test")
mtext(paste("Simulated power (",testtype,", post-hoc)",sep=""), 3, outer=TRUE, line=0.5, cex=1.4)
mtext(eval(substitute(expression(paste("range ",alpha," = [",mialphas,", ",maalphas,"] | trials = ",trials,sep="")),
      list(mialphas=min(alphas), maalphas=max(alphas), trials=trials))), 3, outer=TRUE, line=-1, cex=1.1)
ap.tab <- data.frame(alphas, powers)
head(ap.tab)
tail(ap.tab)

# only t-test
length(power.boot.res[power.boot.res < 0.0678])/trials
# Cohen's d
(6.5-7.8)/2


