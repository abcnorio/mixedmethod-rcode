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
# ptII_quan_classicstats_N-P_confint_helpfuncs.r

# location:
# chap. 4 [4.5.2.6]
# Konfidenzintervalle

# HELPER FUNCTIONS


###### function to calculate classical CI for a mean
ci.mean <- function(n1=NA, xbar1=NA, sd1=NA, ci.prob=0.95, digits=2, printshort=FALSE)
{
 alpha <- 1-ci.prob
 emp.se1 <- sd1/sqrt(n1)
 dfree <- n1-1
 t.value1 <- qt(1-alpha/2, df=dfree)
 ci.halfwidth1 <- t.value1*emp.se1
 ci.low1 <- xbar1 - ci.halfwidth1
 ci.up1 <- xbar1 + ci.halfwidth1
 res1 <- data.frame(n1,ci.low1,xbar1,ci.up1,2*ci.halfwidth1,sd1,emp.se1,ci.prob,t.value1,dfree)
 colnames(res1) <- c("N","CI(low)","Mean","CI(up)","CI(width)","SD","SE","CI(prob)","t","df")
 rownames(res1) <- ""
 if(printshort)
 {
  short <- round(res1[2:4], digits=digits)
  cat(paste("\n",short[2]," [",short[1],"; ",short[3],"]\n\n",sep=""))
 } 
return(res1)
}
#call:
#ci.mean(n1=n1, xbar1=xbar1, sd1=sd1)
########################## END OF FUNCTION


###### function to calculate classical CI for a difference in means
ci.diff.in.means <- function(n1=NA, xbar1=NA, sd1=NA, n2=NA, xbar2=NA, sd2=NA, equal.var=FALSE, ci.prob=0.95)
{
 alpha <- 1-ci.prob
 
 N <- n1+n2
 
 #sample 1
 samp1.res <- ci.mean(n1=n1, xbar1=xbar1, sd1=sd1)
 #sample 2
 samp2.res <- ci.mean(n1=n2, xbar1=xbar2, sd1=sd2)
 
 #difference in means
 diff.mw <- xbar2 - xbar1

 #equal variances
 if(equal.var==TRUE)
 {
  dfree.diff <- n1+n2-2
  t.value.diff <- qt(1-alpha/2, df=dfree.diff)
  #Welch's t-interval
  sd.diff.pooled <- sqrt(sd1^2/n1 + sd2^2/n2)
  ci.halfwidth <- t.value.diff * sd.diff.pooled
  ci.low.diff.ev <- diff.mw - ci.halfwidth
  ci.up.diff.ev <- diff.mw + ci.halfwidth
  diff.in.means.res <- data.frame(N,ci.low.diff.ev,diff.mw,ci.up.diff.ev,2*ci.halfwidth,NA,sd.diff.pooled,ci.prob,t.value.diff,dfree.diff,equal.var)
 } else
 {
 #unequal variances
  dfree.diff.uev <- (sd1^2/n1 + sd2^2/n2)^2 / ( (sd1^2/n1)^2/(n1-1) + (sd2^2/n2)^2/(n2-1) )
  t.value.diff.uev <- qt(1-alpha/2, df=floor(dfree.diff.uev) )
  sd.diff.pooled <- sqrt(sd1^2/n1 + sd2^2/n2)
  ci.halfwidth <- t.value.diff.uev * sd.diff.pooled
  ci.low.diff.uev <- diff.mw - ci.halfwidth
  ci.up.diff.uev <- diff.mw + ci.halfwidth
  diff.in.means.res <- data.frame(N,ci.low.diff.uev,diff.mw,ci.up.diff.uev,2*ci.halfwidth,NA,sd.diff.pooled,ci.prob,t.value.diff.uev,dfree.diff.uev,equal.var)
 }
 samp.res <- data.frame(rbind(samp1.res, samp2.res),"var(equal)"=c(NA,NA), check.names=FALSE)
 colnames(samp.res)[3] <- "theta"
 colnames(diff.in.means.res) <- colnames(samp.res)
 res <- rbind(samp.res, diff.in.means.res)
 rownames(res) <- c("sample(1)","sample(2)","delta(mean)")
return(res)
}
#call
#ci.diff.in.means(n1=n1, xbar1=xbar1, sd1=sd1, n2=n2, xbar2=xbar2, sd2=sd2, equal.var=TRUE)
########################## END OF FUNCTION


###### function to plot histogramm + density + CI lower + CI upper
plot.CI <- function(res=NA, trials=NA, alpha=0.05)
{
 tvalue <- qt(1-alpha/2, df=trials, lower.tail=TRUE)
 delta <- res[,"theta"]
 delta.se <- sd(delta)
 delta.mean <- mean(delta)
 delta.dens <- density(delta)
 y.mean <- delta.dens$y[sort(delta.dens$x) > delta.mean][1]
 ci.low <- delta.mean - tvalue*delta.se
 y.low <- delta.dens$y[sort(delta.dens$x) > ci.low][1]
 ci.up <- delta.mean + tvalue*delta.se
 y.up <- delta.dens$y[sort(delta.dens$x) > ci.up][1]
 par(oma=c(2,1,1,1), "cex.axis"=1, bty="l")
 hist(delta, prob=TRUE, pre.plot=grid(), col="orange", border="white", xlab="Difference of Means", ylab="Density", main="")
 lines(list(x=c(delta.mean,delta.mean), y=c(0,y.mean)), col="blue", lty=2, lwd=2)
 lines(list(x=c(ci.low,ci.low), y=c(0,y.low)), col="blue", lty=2, lwd=2)
 lines(list(x=c(ci.up,ci.up), y=c(0,y.up)), col="blue", lty=2, lwd=2)
 lines(delta.dens, col="blue", lwd=2, lty=1)
 mtext(paste("Simulation Classical Confidence Interval",sep=""), outer=TRUE, line=-2, cex=1.5, side=3)
 mtext(paste("(CI = ",(1-alpha)*100,"%)",sep=""), outer=TRUE, line=-3.6, cex=1.2, side=3)
 res <- list(tvalue=tvalue,
             delta=delta,
             delta.se=delta.se,
             delta.mean=delta.mean,
             delta.dens=delta.dens,
             y.mean=y.mean,
             y.low=y.low,
             y.up=y.up,
             ci.low=ci.low,
             ci.up=ci.up
             )
return(res) 
}
#call:
#plot.CI(res=res, trials=trials)
########################## END OF FUNCTION

