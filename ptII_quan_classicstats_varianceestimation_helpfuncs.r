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
# ptII_quan_classicstats_varianceestimation.r

# location:
# chap. 4 [4.6.5]
# (Selbst-)Täuschungen

# HELPER FUNCTIONS


###### function to calculate variance estimation
varest <- function(x, sigma=NA, LOG=FALSE)
{
 n <- length(x)
 xbar <- sum(x)/n
 ms <- sum((x-xbar)^2)
 
 #s2
 s2.ml <- 1/n*ms
 s2.ub <- 1/(n-1)*ms
 s2.mmse <- 1/(n+1)*ms
 
 #s
 if(LOG == FALSE)
 {
  tn <- gamma(n/2)/(gamma((n-1)/2))
 } else 
 {
  tn <- exp(lgamma(n/2)-lgamma((n-1)/2))
 } 
 s.ml <- sqrt(1/n*ms)
 s.ub <- 1/tn*sqrt(ms/2)
 s.mmse <- tn/(n-1)*sqrt(2*ms)
 
 shat2 <- c(s2.ml,s2.ub,s2.mmse)
 shat <- c(s.ml,s.ub,s.mmse)
 
 if(!is.na(sigma))
 {
  #s2
  #E
  s2.exp.ml <- (n-1)/n*sigma^2
  s2.exp.ub <- sigma^2
  s2.exp.mmse <- (n-1)/(n+1)*sigma^2
  #MSE
  s2.mse.ml <- (2*n-1)/n^2*sigma^4
  s2.mse.ub <- 2/(n-1)*sigma^4
  s2.mse.mmse <- 2/(n+1)*sigma^4
  
  #s
  #E
  s.exp.ml <- tn*sqrt(2/n)*sigma
  s.exp.ub <- sigma
  #s.exp.mmse <- 2*tn^2/(n-1)*sigma^2
  s.exp.mmse <- sqrt(2*tn^2/(n-1))*sigma
  #MSE
  s.mse.ml <- 2*sigma^2*(1-tn*sqrt(2/n)-1/(2*n))
  s.mse.ub <- sigma^2*((n-1)/(2*tn^2)-1)
  s.mse.mmse <- sigma^2*(1-2*tn^2/(n-1))
  
  E.shat2 <- c(s2.exp.ml, s2.exp.ub, s2.exp.mmse)
  MSE.shat2 <- c(s2.mse.ml, s2.mse.ub, s2.mse.mmse)

  E.shat <- c(s.exp.ml, s.exp.ub, s.exp.mmse)
  MSE.shat <- c(s.mse.ml, s.mse.ub, s.mse.mmse)
  
  res <- data.frame(label=c("ML","UB","MMSE"), n=rep(n,3),
                    shat2,  
                    s2.normmean=E.shat2/sigma^2, E.shat2, MSE.shat2, s2.normRMSE=sqrt(MSE.shat2/sigma^4),
                    shat,
					s.normmean=E.shat/sigma, E.shat, MSE.shat, s.normRMSE=sqrt(MSE.shat/sigma^2)
					)
 } else
 {
  res <- data.frame(label=c("ML","UB","MMSE"), n=rep(n,3), shat2, shat)
 }
 
 rownames(res) <- c("ML","UB","MMSE")
 
return(res)
}
#call:
#
#if sigma is known
#varest(x=rnorm(n=50, mean=100, sd=10), sigma=10, LOG=FALSE)
#
#if sigma is unknown
#varest(x=rnorm(n=50, mean=100, sd=10), sigma=NA, LOG=FALSE)
########################## END OF FUNCTION


###### function to plot the different variance estimations
plot.varest <- function(n=20, mu=100, sigma=10, plotwhat="s2", seed=9876, pr=FALSE)
{
 set.seed(seed)

 ll <- vector(mode="list", length=n)
 for(i in 2:n)
 {
  x <- rnorm(n=i, mean=mu, sd=sigma)
  ll[[i]] <- varest(x=x, sigma=sigma)
 } 
 ll.tab <- do.call("rbind",ll)
 ll.tab <- ll.tab[order(ll.tab$label, sort(ll.tab$label)),]
 rownames(ll.tab) <- 1:dim(ll.tab)[1]
 if(pr)
 {
  op.old <- options()
  options(width=200)
  print( head(ll.tab) )
  print( tail(ll.tab) )
  options(op.old)
 } 

 categs <- c("ML","UB","MMSE")
 cols <- rainbow(length(categs))  
 fac <- 1.1

 if(plotwhat == "s2")
 {
#SIGMA^2 
#s2 normalized mean
 par(mfrow=c(2,1), oma=c(2,1,1,1), par("cex.axis"=0.8))
 plot(1, type="n", xlim=c(1,n), ylim=c(0,fac*max(ll.tab$s2.normmean,na.rm=TRUE)), panel.first=grid(), bty="l", main="", xlab="n", ylab="normalized mean")
 for(i in 1:3)
 {
  temp <- ll.tab[ll.tab$label == categs[i],]
  lines(temp$n,temp$s2.normmean, col=cols[i])
 }
 mtext(expression(paste("E[",hat(sigma)^"2","]/",sigma^"2",sep="")), line=1, cex=1.1)

#s2 root-mean-squar error R-MSE
 plot(1, type="n", xlim=c(1,n), ylim=c(0,fac*max(ll.tab$s2.normRMSE,na.rm=TRUE)), panel.first=grid(), bty="l", main="", xlab="n", ylab="normalized R-MSE")
 for(i in 1:3)
 {
  temp <- ll.tab[ll.tab$label == categs[i],]
  lines(temp$n,temp$s2.normRMSE, col=cols[i])
 } 
 mtext(expression(sqrt(paste("MSE[",hat(sigma)^"2","]/",sigma^"4"),sep="")), line=1, cex=1.1)
 mtext(expression(paste("Comparison of estimators of ",sigma^"2",sep="")), outer=TRUE, line=-1, cex=1.5)

 par(fig=c(0,1,0,1), oma=c(1,0,0,0), mar=c(0,0,0,0), new=TRUE)
 plot(1, type="n", bty="n", xaxt="n", yaxt="n")
 legend("bottom", legend=categs, lty=1, lwd=2, xpd=TRUE, horiz=TRUE, col=cols, bty="n", cex=.9)
 } else if(plotwhat == "s")
 {
###SIGMA
#s normalized mean
 par(mfrow=c(2,1), oma=c(2,1,1,1), par("cex.axis"=0.8))
 plot(1, type="n", xlim=c(1,n), ylim=c(0,fac*max(ll.tab$s.normmean,na.rm=TRUE)), panel.first=grid(), bty="l", main="", xlab="n", ylab="normalized mean")
 for(i in 1:3)
 {
  temp <- ll.tab[ll.tab$label == categs[i],]
  lines(temp$n,temp$s.normmean, col=cols[i])
 } 
 mtext(expression(paste("E[",hat(sigma),"]/",sigma,sep="")), line=1, cex=1.1)

#s root-mean-squar error R-MSE
 plot(1, type="n", xlim=c(1,n), ylim=c(0,fac*max(ll.tab$s.normRMSE,na.rm=TRUE)), panel.first=grid(), bty="l", main="", xlab="n", ylab="normalized R-MSE")
 for(i in 1:3)
 {
  temp <- ll.tab[ll.tab$label == categs[i],]
  lines(temp$n,temp$s.normRMSE, col=cols[i])
 } 
 mtext(expression(sqrt(paste("MSE[",hat(sigma),"]/",sigma^"2"),sep="")), line=1, cex=1.1)
 mtext(expression(paste("Comparison of estimators of ",sigma,sep="")), outer=TRUE, line=-1, cex=1.5)

 par(fig=c(0,1,0,1), oma=c(1,0,0,0), mar=c(0,0,0,0), new=TRUE)
 plot(1, type="n", bty="n", xaxt="n", yaxt="n")
 legend("bottom", legend=categs, lty=1, lwd=2, xpd=TRUE, horiz=TRUE, col=cols, bty="n", cex=.9)
 } else stop("choose 's2' or 's' for 'plotwhat'")
}
#call:
#plot.varest(plotwhat="s2")
#plot.varest(plotwhat="s")
########################## END OF FUNCTION



