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
# ptII_quan_Bayes_Fisher_LadyBristol-Beta-disttribution.r

# location:
# chap. 6 [6.12]
# Die Wahl priorer Verteilungen

# load necessary libs
library(LearnBayes)

# load necessary helper functions
source("ptall_generalfuncs_Bayes_Beta_determine.r")
source("ptall_generalfuncs_Bayes_binomial.r")


# Lady Bristol - choose a prior 
# we believe the median of the prior is 0.85
quantile1 <- list(p=0.5, qua=0.65)    
# we believe the 99.999th percentile of the prior is 0.95
quantile2 <- list(p=0.8, qua=0.8) 
# we believe the 0.001st percentile of the prior is 0.60
quantile3 <- list(p=0.2, qua=0.25) 
prior.ab <- beta.determine.opt(p=c(0.5,0.8,0.2), qua=c(0.65,0.8,0.25), ab.start=c(1,1), graph=TRUE)
prior.ab

# Lady Bristol - use empirical data for likelihood
si <- 8
Ni <- 8
lik.ab <- bino.ab.lik(si=si, Ni=Ni)

# post
post.ab <- bino.ab.post(a.prior=prior.ab$res.ab3["a"], b.prior=prior.ab$res.ab3["b"], si=si, Ni=Ni)

# output
prior.ab$res.ab3
lik.ab
post.ab


# plot prior, likelihood, and posterior
fac <- 1.15
thetas <- seq(0,1,0.01)
dbetas.prior <- dbeta(thetas, prior.ab$res.ab3["a"], prior.ab$res.ab3["b"])
dbetas.lik <- dbeta(thetas, lik.ab[["a"]], lik.ab[["b"]])
dbetas.post <- dbeta(thetas, post.ab[["a"]], post.ab[["b"]])
dbetas <- c(dbetas.prior,dbetas.lik,dbetas.post)
# plot
ylim.max <- max(dbetas[dbetas < Inf & dbetas > -Inf])*fac
par(oma=c(2,1,1,1), "cex.axis"=1, bty="l")
plot(thetas, dbetas.prior, ylim=c(0,ylim.max), type="l", col="darkred", bty="n", pre.plot=grid(), xlab=expression(theta), ylab="Density", main="", lwd=2, cex.lab=1.2)
lines(thetas, dbetas.lik, col="steelblue", lty=2, lwd=2)
lines(thetas, dbetas.post, col="seagreen", lty=3, lwd=2)
mtext(paste("Lady Bristol's tea experiment",sep=""), outer=TRUE, line=-1.5, cex=1.5, side=3)
mtext(paste(si," successes of ",Ni," trials",sep=""), outer=TRUE, line=-3, cex=1, side=3)
par(fig=c(0,1,0,1), oma=c(1,0,0,0), mar=c(0,0,0,0), new=TRUE)
plot(1, type="n", bty="n", xaxt="n", yaxt="n")
legend("bottom", legend=c("prior","likelihood","posterior"), lty=1:3, lwd=2, xpd=TRUE, horiz=TRUE, col=c("darkred","steelblue","seagreen"), bty="n", cex=.9)

# summary
post.summary <- beta.summary(a=post.ab[["a"]], b=post.ab[["b"]])
unlist(post.summary)

v <- data.frame(a.prior = prior.ab$res.ab3["a"], b.prior = prior.ab$res.ab3["b"],
                a.lik = lik.ab[["a"]], b.lik = lik.ab[["b"]],
        				a.post = post.ab[["a"]], b.post = post.ab[["b"]],
				        mean.post = as.numeric(post.summary$mean),
    						sd.post = as.numeric(post.summary$sd)
		    			 )
v

# beta.triplot(si, Ni, v)
beta.triplot2(si, Ni, v, TITLE="Lady Bristol (8 successes/ 8 trials)")

