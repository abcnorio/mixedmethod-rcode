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
# ptII_quan_Bayes_BayesFactors_dependence-on-N-sim_helpfuncs.r

# location:
# chap. 6 [6.7.1.3]
# Aktualität von Bayes-Faktoren

# HELPER FUNCTIONS


###### function to simulate Bayes factors and dependence on N sample size
sim.p.bf <- function(N1=30, N2=30, mu1=100, mu2=100, sigma1=10, sigma2=10, nsim=100, seed=44444,#8876,
                     muplus=0.05, graphout=TRUE, xlim=c(0,1), ylim=c(0,20), usesameseed=TRUE)
{
 require(BayesFactor)
 cnam <- c("n1","n2","mu1","mu2","sigma1","sigma2","xbar1","xbar2","s1","s2","t","p","BF01","BF10","ES")
 cnam.l <- length(cnam)
 res <- data.frame(matrix(data=NA, nrow=nsim, ncol=cnam.l))
 colnames(res) <- cnam

 for(i in 1:nsim)
 {
  if(usesameseed) set.seed(seed)
  rv1 <- rnorm(N1, mean=mu1, sd=sigma1)
  rv2 <- rnorm(N2, mean=mu2, sd=sigma2)
  xbar1 <- mean(rv1)
  xbar2 <- mean(rv2)
  s1 <- sd(rv1)
  s2 <- sd(rv2)
  ES <- (xbar1-xbar2)/sigma1
  tres <- t.test(rv1,rv2,var.equal=FALSE)
  tv <- tres$statistic
  pv <- tres$p.value
  ttestbf <- ttestBF(rv1,rv2)
  BF10 <- exp(ttestbf@bayesFactor[,"bf"])
  linie <- c(N1,N2,mu1,mu2,sigma1,sigma2,xbar1,xbar2,s1,s2,tv,pv,1/BF10,BF10,ES)
  res[i,] <- linie
  mu2 <- mu2 + muplus
 }
 if(graphout)
 {
  with(res, plot(p, BF01, col="violetred3", type="p", bty="n",
                 pch=20, cex=0.9, ylab=expression(BF[0][1]),
				 xlim=xlim, ylim=ylim, 
				 pre.plot=grid(), main=paste("N1 = ",N1," | N2 = ",N2,sep="")))
  with(res, points(p, BF10, col="blue", pch=20, cex=0.9))
  abline(v=0.05, col="green", lty=2)
  abline(h=1, col="orange", lty=2)
  abline(h=3, col="darkred", lty=2)
 } 
return(res)
}
# call:
# sim.p.bf(n1,n2,mu1,mu2,sigma1,sigma2, nsim=100)
########################## END OF FUNCTION


###### function to calibrate p-values to attain BayesFactors
BF.calib <- function(pv)
{
 if(pv >= 1/exp(1)) stop("functions works only for p < 1/e")
 B_p <- -exp(1)*pv*log(pv)
 alpha_p <- (1+(-exp(1) * pv * log(pv))^(-1))^(-1)
 res <- c(B_p, alpha_p)
 names(res) <- c("BF_p","alpha_p")
return(res) 
}
# call:
# BF.calib(pvalue)
########################## END OF FUNCTION


###### function to show dependence of p-value and sample size
pvsN.sim <- function(en=15*(1:20), nsim=20, seed=0987, mu=100, sigma=10 )
{
 set.seed(seed)
 pes <- vector()
 for(i in 1:nsim)
 {
  vec <- rnorm(en[i], mean=mu, sd=sigma)
  pes[i] <- t.test(vec)$p.value
 }
 par(oma=c(2,1,2,1), "cex.axis"=1, bty="l")
 plot(log(pes), en[1:nsim], type="b", bty="n", pch=20, col="violetred3", ylab="N", ylim=c(0,max(en)), xlim=c(min(log(pes)),0), xlab="log(p)", pre.plot=grid())
 mtext(expression(paste("Dependence of p-value and sample size",sep="")), outer=TRUE, line=-1, cex=1.5, side=3)
 mtext(expression(paste("One sample t-test against zero (NHST)",sep="")), outer=TRUE, line=-3, cex=1, side=3)
return(pes) 
}
# call:
# pvsN.sim()
########################## END OF FUNCTION

