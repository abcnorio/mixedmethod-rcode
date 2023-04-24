# file:
# ptII_quan_classicstats_equivalentmethods.r

# location:
# chap. 4 [4.6.8.1]
# Äquivalenz von Messverfahren und -methoden

# HELPER FUNCTIONS

###### function to plot mean-difference according to Tukey
# identical to Bland-Altman Plot
MD.plot <- function(x, y, alpha=0.05, tvn=NULL, pr=TRUE, fak=1.1)
{
 stopifnot(length(x) == length(y))
 n.eff <- length(x)

 # Bland-Altman statistics
 differ <- x-y
 diff.mean.xy <- mean(differ)
 var.mean.xy <- var(differ)
 sd.mean.xy <- sd(differ)

 SE1 <- sqrt(var.mean.xy/n.eff)
 SE2 <- sqrt(3*var.mean.xy/n.eff)

 if(is.null(tvn)) tvn <- qnorm(1-alpha/2)

 differ.critical <- tvn*sd.mean.xy
 abs.lohi <- diff.mean.xy + differ.critical*c(-1,1)
 tv1 <- qt(1-alpha/2, df=n.eff-1)
 SE1 <- sd.mean.xy/sqrt(n.eff)
 SE2 <- sqrt(3*var.mean.xy/n.eff)
 CI.differ.lowup <- diff.mean.xy + tv1*SE1 * c(-1,1)
 CI.lower.lowup <- abs.lohi + tv1*SE2 * c(-1,1)
 CI.upper.lowup <- abs.lohi + (-tv1)*SE2 * c(-1,1)

 par(oma=c(2,1,1,1), "cex.axis"=1, bty="l", mfrow=c(2,2))
 # scatterplot
 plot(x,y, pch=21, cex=1.4, col="steelblue", bg="yellow", bty="n", pre.plot=grid())
 abline(lm(x~y), col="seagreen", lwd=2)
 abline(0,1, col="red", lwd=2, lty=2)
 # histogram
 hist(differ, prob=TRUE, pre.plot=grid(), bty="n", col="darkblue", border="white", main="", xlab="x - y", ylab="Density")
 lines(density(differ), col="green", lwd=2)
 # mean-difference plot = Bland-Altman plot
 ylim <- range(differ)*fak
 plot((x+y)/2, x-y, pch=21, cex=1.4, col="steelblue", bg="yellow", bty="n", pre.plot=grid(),
      ylim=c(ylim[1],ylim[2]), main="MD plot | BA plot")
 abline(h=diff.mean.xy, col="seagreen", lwd=2)
 abline(h=abs.lohi, col="darkred", lwd=2,lty=2)
 abline(h=CI.lower.lowup, col="darkorange", lwd=2, lty=2)
 abline(h=CI.upper.lowup, col="magenta", lwd=2,lty=3)
 abline(h=CI.differ.lowup, col="skyblue", lwd=2,lty=2)
 mtext("Mean-Differences between two samples", outer=TRUE, line=-2, cex=1.5, side=3)
 # MA-plot
 plot((log2(x)+log2(y))/2, log2(x)-log2(y), pch=21, cex=1.4, col="steelblue", bg="yellow", bty="n",
      pre.plot=grid(), xlab=expression(log[2](x)-log[2](y)), main="MA plot")
 
 res <- structure(list(n=n.eff, differ=differ, alpha=alpha, z=tvn, "t-value"=tv1, SE1=SE1, SE2=SE2,
  diff.mean.xy=diff.mean.xy,
  differ.critical=differ.critical,
  LI.critical.lohi=abs.lohi,
  CI.lower.lowup=CI.lower.lowup,
  CI.upper.lowup=CI.upper.lowup,
  CI.differ.lowup=CI.differ.lowup,
  method="Bland-Altman statistics"
 ), class = "power.htest")

 if(pr) print(res)
return(res)
}
#call:
#res <- MD.plot(x,y)
########################## END OF FUNCTION

