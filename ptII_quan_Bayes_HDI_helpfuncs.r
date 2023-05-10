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
# ptII_quan_Bayes_HDI.r

# location:
# chap. 6 [6.7.4.1]
# Intervallschätzung

# HELPER FUNCTIONS


###### function to plot HDI
plotHDI <- function(dens, prob, quants=NULL, densTF=TRUE, theta=seq(0,1,0.01), fac=0.1, digs=3)
{
 require(HDInterval)
 #helper function
 plot.interval <- function(dxy, interval, colo="red")
 {
  y1id <- which(dxy$x >= interval[1])[1]
  interval.y1 <- dxy$y[c(y1id,y1id+1)]
  interval.x1 <- median(dxy$x[c(y1id,y1id+1)])
  y2id <- which(dxy$x >= interval[2])[1]
  interval.y2 <- dxy$y[c(y2id-1,y2id)]
  interval.x2 <- median(dxy$x[c(y2id-1,y2id)])
  interval.y <- median(c(interval.y1, interval.y2))
  lines(x=c(interval.x1,interval.x2),y=rep(interval.y,2), col=colo, lwd=6)
 }

 #prob interval based on prob (=width)
 probsfromwidth <- function(prob)
 {
  probs.lo <- (1-prob)/2
  probs.up <- prob + probs.lo
  probs <- c(probs.lo, probs.up)
  return(probs)
 }
 #call:
 probs <- probsfromwidth(prob=prob)

 #hdi
 hdi1 <- hdi(dens, credMass=prob)
 
 #plot
 par(oma=c(2,1,1,1), "cex.axis"=1, bty="l")
 if(densTF == TRUE)
 {
  plot(dens$x, dens$y, type="l", main="", pre.plot=grid(), xlab=expression(theta),
       ylab="density", col="steelblue", lwd=2, bty="n", cex.lab=1.2)
  #plot hdi
  plot.interval(dxy=dens, interval=hdi1, colo="orange")	  
  #CI
  if(is.null(quants))
  {
   dy.cs <- cumsum(dens$y)
  #scale density to one (= mass) to get probs
   dens.y.cs <- dy.cs/max(dy.cs)
   round(dens.y.cs,2)
   lower <- max(which(dens.y.cs <= probs[1]))
   upper <- max(which(dens.y.cs <= probs[2]))
   lo <- c(lower,upper)
   quants <- dens$x[lo]
  }
  lines(x=quants,y=rep(0,2), col="yellowgreen", lwd=6)
 } else
 {
  quants <- quantile(dens, probs)
  hist(dens, prob=TRUE, main="", pre.plot=grid(), xlab=expression(theta),
       ylab="density", col="steelblue", border="white", cex.lab=1.2)
  lines(dxy <- density(dens), col="magenta3", lwd=3, lty=1)
  #hdi
  plot.interval(dxy=dxy, interval=hdi1, colo="orange")
  #quantile
  lines(x=quants,y=rep(0,2), col="yellowgreen", lwd=6)
 }

 mtext("Density and Confidence limits", outer=TRUE, line=-1.5, cex=1.5, side=3)
 mtext(expression(paste("Highest Density Interval (HDI) and Confidence Interval (",CI[Bayes],")")), outer=TRUE, line=-2.8, cex=1, side=3)

 legend("topright",legend=c(paste("HDI [",paste(probs,collapse="; "),"]",sep=""),
 						    paste(round(hdi1[1:2],digs),
                            c(" (lower)"," (upper)"),sep=""),
						    "",
						    paste("CI [",paste(probs,collapse="; "),"]",sep=""),
						    paste(round(quants,digs),
                            c(" (lower)"," (upper)"),sep="")						
						   ),
        xpd=TRUE, horiz=FALSE, inset=c(0,0), y.intersp=1.5,	bty="n", cex=0.9,
		text.col=c("black",rep("orange",2),"white","black",rep("yellowgreen",2))
	   )

 cat("\n############################\n\n")
 cat("Interval [", paste(probs, collapse="; "),"]\n",sep="")
 cat("HDI      [", paste(round(hdi1,digs), collapse="; "),"]\n",sep="")
 cat("CI (sym) [", paste(round(quants,digs), collapse="; "),"]\n",sep="")
 cat("\n############################\n\n")
 
}
# call:
# plotHDI(dens=dens, prob=0.95, quants=qbeta(c(0.025,0.975),1,11), densTF=TRUE, digs=4)
########################## END OF FUNCTION


