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
# ptII_quan_classicstats_Simpsonparadox.r

# location:
# chap. 4 [4.6.11.1]
# Simpson Paradox

# HELPER FUNCTIONS


###### function to plot two groups (to detect Simpson Paradox)
SP.sim <- function(v, pr.out=TRUE, plot.out=TRUE)
{
  par(mfrow=c(1,1), oma=c(2,1,1,1), par("cex.axis"=0.8))
  with(v, plot(x, y, panel.first=grid(), xlab="x", ylab="y", main="", bty="l"))
  lm.fit <- lm(y ~ x, data=v)
  lm.fit1 <- lm(y~x, data=subset(v, group==1))
  lm.fit2 <- lm(y~x, data=subset(v, group==2))
  abline(lm.fit, col="red", lty=1, lwd=2)
  abline(lm.fit1, col="skyblue", lty=2, lwd=2)
  abline(lm.fit2, col="seagreen", lty=2, lwd=2)
  with(subset(v, group==1), points(x,y, cex=1.7, col="seagreen", pch=21, bg="orange"))
  with(subset(v, group==2), points(x,y, cex=1.7, col="blue", pch=21, bg="yellow"))
  mtext("Simpson Paradox", outer=TRUE, line=-2, cex=1.5, side=3)
 
 # check linear models
  if(pr.out)
  {
    display(lm.fit)
    display(lm.fit1)
    display(lm.fit2)
  }
 
#legend
  if(plot.out)
  {
    par(fig=c(0,1,0,1), oma=c(1,0,0,0), mar=c(0,0,0,0), new=TRUE)
    plot(1, type="n", bty="n", xaxt="n", yaxt="n")
    legend("bottom", legend=c("all","group 1","group 2"), lty=c(1,2,2), lwd=2, xpd=TRUE, horiz=TRUE, 
           col=c("red","skyblue","seagreen"), bty="n", cex=.9)
  }
}  
#call:
#SP.sim(v)
########################## END OF FUNCTION



