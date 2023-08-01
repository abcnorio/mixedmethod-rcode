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
# ptII_quan_EDA_intro_overviewrobust.r

# location:
# chap. 5 [5.2.-5.3.]
# Typische Verfahren der EDA in R
# Robuste Datenanalysen als Teil der EDA

# HELPER FUNCTIONS


###### function to plot residuals of lm vs. rlm
resid.plot <- function(lmfit, rlmfit)
{  
  lm.sresid <- lmfit$residuals
  rlm.sresid <- rlmfit$residuals
  lm.sresid.sc <- scale(lm.sresid)
  rlm.sresid.sc <- scale(rlm.sresid)
  
  if(dev.cur() != 1) dev.off()
  #fig=c(0,1,0,1), 
  par(oma=c(2,1,2,1), cex.axis=0.8, mfrow=c(2,2))
  # abs(OLS-RLM)
  plot(lmfit$residuals - rlmfit$residuals, bty="n", pre.plot=grid(), col="purple", ylab="abs(OLS-RLM)", )
  abline(h=0, col="red", lty=2, lwd=2)
  # both models over each other
  plot( lm.sresid , bty="n", pre.plot=grid(), col="green", xlab="index (OLS and RLM)", ylab="Absolute Residuals")
  points(rlm.sresid, col="blue")
  abline(h=c(-1,1)*2, col="red", lty=2, lwd=2)
  # OLS
  plot( lm.sresid.sc , bty="n", pre.plot=grid(), col="green", xlab="Index (OLS)", ylab="Scaled Residuals")
  abline(h=c(-1,1)*2, col="red", lty=2, lwd=2)
  # RLM
  plot( rlm.sresid.sc , bty="n", pre.plot=grid(), col="blue", xlab="Index (RLM)", ylab="Scaled Residuals")
  abline(h=c(-1,1)*2, col="red", lty=2, lwd=2)
  mtext("OLS vs RLM (residuals)", outer=TRUE, line=-1, cex=1.5)
  
  par(fig=c(0,1,0,1), oma=c(1,0,0,0), mar=c(0,0,0,0), new=TRUE)
  plot(1,type="n", bty="n", xaxt="n", yaxt="n")
  legend("bottom", legend=c("OLS","RLM"), lty=c(1,1), lwd=c(2,2), col=c("green","blue"), bty="n", cex=0.9, horiz=TRUE)
  
}
# call:
#resid.plot(lmfit=chic.lm, rlmfit=chic.rlm)
########################## END OF FUNCTION


###### function to simulate median and mean from normal distribution and their ratios
mwmed.sim <- function(N=NA, mu=NA, sigma=NA, seed=round(runif(1)*1e4), reps=1e2, starti=30, pr=TRUE, abso=FALSE, usesameseed=FALSE)
{

 mwvsmed <- function(N, mu, sigma, seed, abso=FALSE, usesameseed=TRUE)
 {
  if(usesameseed) set.seed(seed)
  dats <- rnorm(n=N, mean=mu, sd=sigma)
  mw <- mean(dats)
  med <- median(dats)
  ratio.medmw <- 1-med/mw
  if(abso) ratio.medmw <- abs(ratio.medmw)
 
  dev.mw <- (mw-mu)/sigma
  dev.med <- (med-mu)/sigma
  res <- data.frame(mw,med,ratio.medmw,dev.mw,dev.med)
  return(res)
 }

 sek <- seq(starti,N,length.out=reps)
 res <- do.call("rbind",lapply(seq_along(sek), function(i) mwvsmed(N=sek[i],mu=mu,sigma=sigma,seed=seed,abso=abso, usesameseed)))
 if(pr)
 {
  out <- data.frame(t(apply(res,2,summary)),SD=apply(res,2,sd), check.names=FALSE)
  out$VAR <- out$SD^2
  print(out)
 } 
return(list(res,out,sek))
}
# call:
# res <- mwmed.sim(n=100, mu=6, sigma=2.34, seed=0987, abso=FALSE, pr=TRUE)
########################## END OF FUNCTION


###### function to plot results of simulation mean/ median
mwmed.sim.plot <- function(res,mu)
{
  if(dev.cur() != 1) dev.off()
  par(oma=c(2,1,1,1), "cex.axis"=1, bty="l", mfrow=c(2,2))

  plot(res[[1]][,"mw"],res[[1]][,"med"], xlab=expression(bar(x)), ylab="median", bty="n", pre.panel=grid(), main="", pch=21, col="blue", bg="red")
  abline(lm(res[[1]][,"mw"]~res[[1]][,"med"]), col="green")
  points(mu,mu, bg="green", col="black", pch=21, cex=2, lwd=2)
 
  plot(res[[3]],res[[1]][,"ratio.medmw"],ylab=expression(paste("diff = 1 - median / ",bar(x),sep="")),xlab="N", bty="n", pre.panel=grid(), main="", pch=21, col="blue", bg="red")
  abline(h=0, col="green")
 
  plot(res[[3]],res[[1]][,"dev.mw"],ylab=expression(paste("diff = (",bar(x)," - ",mu,") / ",sigma,sep="")),xlab="N", bty="n", pre.panel=grid(), main="", pch=21, col="blue", bg="red")
  abline(h=0, col="green")
 
  plot(res[[3]],res[[1]][,"dev.med"],ylab=expression(paste("diff = (median - ",mu,") / ",sigma,sep="")),xlab="N", bty="n", pre.panel=grid(), main="", pch=21, col="blue", bg="red")
  abline(h=0, col="green")
  
  mtext(expression(paste("Median, ",bar(x)," and N",sep="")), outer=TRUE, line=-2, cex=1.5, side=3)
}
# call:
# mwmed.sim.plot(res,mu)
########################## END OF FUNCTION



