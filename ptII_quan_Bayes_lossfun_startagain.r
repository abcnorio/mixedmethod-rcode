# file:
# ptII_quan_Bayes_lossfun_startagain.r

# location:
# chap. 6 [6.7.1.2]
# Verlustfunktionen

# load necessary libs
library(LaplacesDemon)

# load necessary helper functions
source("ptall_generalfuncs_Bayes_binomial.r")


# library(LaplacesDemon)
# ?LossMatrix
# Point-estimated loss and state probabilities
L <- matrix(c(-500,1000,-300,-300), 2, 2)
rownames(L) <- c("s[1]: !Defaults","s[2]: Defaults")
colnames(L) <- c("a[1]: Buy ZZZ", "a[2]: Buy XXX")
L
p.theta <- matrix(c(0.9, 0.1, 1, 0), 2, 2)
fit <- LossMatrix(L, p.theta)
fit
# Before using the ‘LossMatrix’ function, the user should have
# already considered all possible actions (choices), states of the
# world (outcomes unknown at the time of decision-making), chosen a
# loss function L(theta, alpha), estimated loss, and elicited prior
# probabilities p(theta | x).


# success rates start again 1992-2017
# colnames "year","org","IN","OUT","s","f","indiff","mpyear"
sa <- read.table(file="startagain_statistics_1992-2017.tab", header=TRUE, sep="\t") 
sa

# set directory
sa.3 <- read.table("startagain_statistics_1992-2017_all-out.tab", header=TRUE, sep="\t")
head(sa.3)

steps <- 1000
theta <- seq(0,1,length.out=steps)

sa.3.d <- dim(sa.3)
si.3 <- sa.3[sa.3.d[1],"s.cs"]
Ni.3 <- sa.3[sa.3.d[1],"N.cs"]
pbl.res <- exp(pbl(theta=theta, si=si.3, Ni=Ni.3, loga=TRUE))
pjc.res <- exp(pjc(theta=theta, si=si.3, Ni=Ni.3, loga=TRUE))
# plot
plot(theta, pbl.res, pre.plot=grid(), col="darkred", type="l", bty="n")
lines(theta,pjc.res, col="green")
# better plots
sN.ME.res <- data.frame(pbl.res, pjc.res)
head(sN.ME.res)
tail(sN.ME.res)
plot.bl.jc(theta, sN.ME.res=sN.ME.res, si=si.3, Ni=Ni.3, filling=TRUE)
plot.bl.jc(theta, sN.ME.res=sN.ME.res, si=si.3, Ni=Ni.3, filling=FALSE)
plot.bl.jc(theta, sN.ME.res=sN.ME.res, si=si.3, Ni=Ni.3, filling=TRUE, sele=c(0.4,0.6))

# create dataframe
theta.mat <- data.frame(theta, pbl.res)
# we work with the pbl and not pjc, difference between both is not really much if at all
theta.mat.pjc <- data.frame(theta, pjc.res)
colnames(theta.mat) <- c("theta","post")

# MAP = maximum aposteriori
theta.map <- theta.mat[theta.mat[,"post"] == max(theta.mat[,"post"]),"theta"]
# mean
theta.mean <- sum(theta.mat[,"theta"] * theta.mat[,"post"] / steps)

# alternative from McElreath p.60f.
# median = min linear loss function
# infids <- apply(res.Xct,2, function(i) which(is.infinite(i),arr.ind=TRUE))
# theta.mat <- res.Xct[-infids$pbl.res,]
theta.linear <- sapply(theta.mat[,"theta"], function(x) sum(theta.mat[,"post"]*abs(x - theta.mat[,"theta"])))
theta.median <- theta.mat[which.min(sapply(theta.mat[,"theta"], function(x) sum(theta.mat[,"post"]*abs(x - theta.mat[,"theta"])))) ,1]
# mean = min quadratic loss function
# MW <- res[which.min(sapply(res[,1], function(x) sum(res[,2]*abs(x - res[,1])^2 ))) ,1]
theta.quad <- sapply(theta.mat[,"theta"], function(x) sum(theta.mat[,"post"]*abs(x - theta.mat[,"theta"])^2 ))
theta.mean <- theta.mat[which.min(sapply(theta.mat[,"theta"], function(x) sum(theta.mat[,"post"]*abs(x - theta.mat[,"theta"])^2 ))) ,1]


# theta mean = a/(a+b)
theta.map
theta.median
theta.mean


# actual loss function calculation
# define reference
ref <- 0.43

# calculate deviations for the loss function
# linear
theta.linear <- sapply(theta.mat[,"theta"], function(x) sum(theta.mat[,"post"]*abs(x - ref)))
head(theta.linear)

# quadratic
theta.quad <- sapply(theta.mat[,"theta"], function(x) sum(theta.mat[,"post"]*(x - ref)^2 ))
head(theta.quad)
tail(theta.quad)

# where to invest
# specific context
theta.linear1 <- sapply(theta.mat[,"theta"], function(x) mean(theta.mat[,"post"]*(abs(x - ref) > 0 & abs(x-ref) < .17)))
head(theta.linear1)
tail(theta.linear1)



# plot loss functions
# ref and associated values
ref
p.max <- 0.6
fac <- 1.2
maxdist <- p.max - ref


# plot single loss functions
par(oma=c(2,1,1,1), "cex.axis"=1, bty="l", cex.axis=1, cex.lab=1.2, cex.main=1, cex.sub=1, ask=TRUE)
# linear difference
theta.linear <- sapply(theta.mat[,"theta"], function(x) mean(theta.mat[,"post"]*abs(x - ref)))
plot(theta.mat[,"theta"], theta.linear, type="l", bty="n", xlab=expression(theta), col="darkred", ylab="linear loss", pre.plot=grid())
abline(v=theta.median, lty=2, lwd=2, col="blue")
abline(v=ref, lty=2, lwd=2, col="green")
text(theta.median,max(theta.linear)/2, "median", col="blue", pos=4)
text(ref,max(theta.linear)/2, "ref", col="green", pos=2)
mtext("Linear loss functions", outer=TRUE, line=-2, cex=1.5, side=3)


# quadratic difference
theta.quad <- sapply(theta.mat[,"theta"], function(x) mean(theta.mat[,"post"]*(x - ref)^2 ))
plot(theta.mat[,"theta"], theta.quad, type="l", bty="n", xlab=expression(theta), col="darkred", ylab="quadratic loss", pre.plot=grid())
abline(v=theta.mean, lty=2, lwd=2, col="blue")
abline(v=ref, lty=2, lwd=2, col="green")
text(theta.mean,max(theta.quad)/2, "mean", col="blue", pos=4)
text(ref,max(theta.quad)/2, "ref", col="green", pos=2)
mtext("Quadratic loss functions", outer=TRUE, line=-2, cex=1.5, side=3)


# limited area where to invest
# use theta.linear1 - see above
ylim <- c(0,1.2)
plot(theta.mat[,"theta"], theta.linear1, ylim=ylim, type="l", bty="n", xlab=expression(theta), col="darkred", ylab="limited area for investment", pre.plot=grid())
abline(v=theta.median, lty=2, lwd=1, col="blue")
abline(v=theta.mean, lty=2, lwd=1, col="red")
abline(v=ref, lty=2, lwd=1, col="green")
text(theta.median,max(theta.linear1)/2, "median", col="blue", pos=4)
text(theta.mean,max(theta.quad)/2, "mean", col="red", pos=2)
text(ref,max(theta.linear1)/2, "ref", col="green", pos=2)
mtext("Linear loss functions with limited area", outer=TRUE, line=-2, cex=1.5, side=3)


# limited area where to invest + bonus
theta.linear2 <- sapply(theta.mat[,"theta"], function(x) {
  DIFF <- x-ref
  BENEFIT <- 0
  if(abs(DIFF) > 0 & abs(DIFF) < maxdist) BENEFIT <- 1
  if(DIFF > 0 & abs(DIFF) < .17) BENEFIT <- 1 + DIFF
  mean(theta.mat[,"post"] * BENEFIT)
 } ) 
plot(theta.mat[,"theta"], theta.linear2, ylim=ylim, type="l", bty="n", xlab=expression(theta), col="darkred", ylab="limited area for investment + bonus", pre.plot=grid())
abline(v=theta.median, lty=2, lwd=2, col="blue")
abline(v=theta.mean, lty=2, lwd=2, col="red")
abline(v=ref, lty=2, lwd=2, col="green")
text(theta.median,max(theta.linear)/2, "median", col="blue", pos=4)
text(theta.mean,max(theta.quad)/2, "mean", col="red", pos=2)
text(ref,max(theta.linear2)/2, "ref", col="green", pos=2)
mtext("Linear loss functions (with area to invest)", outer=TRUE, line=-2, cex=1.5, side=3)

# bonus with upper limit
uplimit <- 0.6
theta.b <- seq(ref,uplimit,0.01)
theta.b
satz <- 450
fac <- 1.05
length(theta.b)
bonus <- satz * seq(1,1.05,length.out=18)
bonus
theta.l <- length(theta.b[theta.b >= 0.43 & theta.b < 0.6])
theta.b <- theta
theta.b[theta.b < 0.2] <- 0
theta.b[theta.b >= 0.2 & theta.b < 0.43] <- 450
theta.b[theta.b >= 0.43 & theta.b < 0.6] <- satz * seq(1,1.05,length.out=theta.l)
theta.b[theta.b >= 0.6 & theta.b <= 1] <- satz*fac



# everything on one plot...
par(oma=c(2,1,4,1), "cex.axis"=1, bty="l", mfrow=c(3,2))
#
# linear loss
plot(theta.mat[,"theta"], theta.linear, main="linear loss", type="l", bty="n", xlab=expression(theta), col="darkred", ylab="linear loss", pre.plot=grid(), cex.lab=1.2)
abline(v=ref, lty=2, lwd=2, col="blue")
text(ref,max(theta.linear)/2, "ref", col="blue", pos=2)
#
# quadratic loss
plot(theta.mat[,"theta"], theta.quad, main="quadratic loss", type="l", bty="n", xlab=expression(theta), col="darkred", ylab="quadratic loss", pre.plot=grid(), cex.lab=1.2)
abline(v=ref, lty=2, lwd=2, col="blue")
text(ref,max(theta.quad)/2, "ref", col="blue", pos=2)
#
# linear1 limited area to invest
ylim <- c(0,1.2)
plot(theta.mat[,"theta"], theta.linear1, ylim=ylim, main="linear loss functions (with limited area to invest)", type="l", bty="n", xlab=expression(theta), col="darkred", ylab="linear loss + investment area", pre.plot=grid())
abline(v=theta.median, lty=2, lwd=1, col="blue")
abline(v=theta.mean, lty=2, lwd=1, col="red")
abline(v=ref, lty=2, lwd=1, col="green")
text(theta.median,max(theta.linear1)/2, "median", col="blue", pos=4)
text(theta.mean,max(theta.quad)/2, "mean", col="red", pos=2)
text(ref,max(theta.linear1)/2, "ref", col="green", pos=2)
#
# linear2 limited area to invest + bonus
plot(theta.mat[,"theta"], theta.linear2, main="linear loss (with limited area to invest + bonus)", ylim=ylim, type="l", bty="n", xlab=expression(theta), col="darkred", ylab="linear loss + area to invest + bonus", pre.plot=grid(), cex.lab=1.2)
abline(v=ref, lty=2, lwd=2, col="blue")
text(ref,max(theta.linear2)/2, "ref", col="blue", pos=2)
#
# bonus with upper limit
plot(theta, theta.b, main="linear loss (with bonus and upper limit)", ylim=c(0,600), type="l", bty="n", xlab=expression(theta), col="darkred", ylab="linear loss + bonus + upper limit", pre.plot=grid(), cex.lab=1.2)
abline(v=ref, lty=2, lwd=2, col="blue")
text(ref,mean(theta.b)/2, "ref", col="blue", pos=2)
# main title
mtext("Loss functions", outer=TRUE, line=0.5, cex=1.8, side=3)
# end of all plots



# UMS example (Studer, 1996)
si <- 23
Ni <- 27
pbl.res <- exp(pbl(theta=theta, si=si, Ni=Ni, loga=TRUE))
pjc.res <- exp(pjc(theta=theta, si=si, Ni=Ni, loga=TRUE))
# plot
plot(theta, pbl.res, pre.plot=grid(), col="darkred", type="l", bty="n")

sN.ME.res <- data.frame(pbl.res, pjc.res)
head(sN.ME.res)
tail(sN.ME.res)
plot.bl.jc(theta, sN.ME.res=sN.ME.res, si=si, Ni=Ni, filling=TRUE)
plot.bl.jc(theta, sN.ME.res=sN.ME.res, si=si, Ni=Ni, filling=FALSE)
plot.bl.jc(theta, sN.ME.res=sN.ME.res, si=si, Ni=Ni, filling=TRUE, sele=c(0.5,1))

# difference between both posterior distributions
par(mfrow=c(2,2))
plot((with(sN.ME.res, pbl.res-pjc.res)), type="l", bty="n", pre.plod=grid(), main="BL - JC", ylab="BL-JC")
plot((with(sN.ME.res, abs(pbl.res-pjc.res))), type="l", bty="n", pre.plod=grid(), main="BL - JC", ylab="abs(BL-JC)")
plot((with(sN.ME.res, log(abs(pbl.res-pjc.res)))), type="l", bty="n", pre.plod=grid(), main="BL - JC", ylab="log(abs(BL-JC))")


sN.ME.post.summary <- sN.post.su(Ni=Ni, si=si)
ab <- bino.ab.lik(si=si, Ni=Ni)
a <- as.numeric(ab[["a"]])
b <- as.numeric(ab[["b"]])
a
b

a/(a+b)
1-(theta.mean/(a/(a+b)))

theta.var <- a*b / ( (a+b)^2 * (a+b+1) )
theta.sd <- sqrt(theta.var)
theta.var
theta.sd

theta.summary <- c(theta.map, theta.median, theta.mean, theta.sd, theta.var)
names(theta.summary) <- c("mode (MAP)","median","mean","sd","var")
theta.summary



