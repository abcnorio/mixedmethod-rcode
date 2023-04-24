# file:
# ptII_quan_EDA_case_Titanic_death-and-dying_helpfuncs.r

# location:
# chap. 5 [5.5.4.6]
# Leben und Sterben auf der Titanic

# HELPER FUNCTIONS


###### function to plot histogram + density for various subgroups
hist.titanic <- function(daten=NA, TITLE="",SUB="", xaxtext="", yaxtext="density",
                         cols=c("greenyellow","orange","steelblue"),
                         breaks="Freedman-Diaconis", fac=1.3, ...)
{ 
  par(mar=c(5,5,4,2), oma=c(2,1,1,1), "cex.axis"=0.8)
  d1 <- density(daten, na.rm=TRUE)
  xlim <- range(d1$x)
  ylim <- range(d1$y)*fac
  plot(NULL, xlim=xlim, ylim=ylim, bty="n", xlab="", ylab="", axes=F)
  grid(col="grey80", lwd=1.2, lty=1)
  hist(daten, main="", xlab="", ylab="", cex.lab=0.8, breaks=breaks, cex.axis=0.8, bty="n", axes=F, prob=TRUE, col=scales::alpha(cols[1],.5), border="white", add=TRUE)
  
  axis(side = 1, pretty(xlim), tck =- .02, labels=NA, line=.6)
  axis(side = 2, pretty(ylim), tck = -.02, labels=NA, line=.6)
  axis(side = 1, lwd = 0, line = .4)
  axis(side = 2, lwd = 0, line = .4, las = 1)
  
  mtext(TITLE, 3, line=2, cex=1.5)
  mtext(SUB, 3, line=.252, cex=1.1)
  mtext(xaxtext, 1, line=4, cex=1)
  mtext(yaxtext, 2, line=4, cex=1)
  
  hist(daten[survived == FALSE], main="", xlab="", ylab="", cex.lab=0.8, breaks=breaks, cex.axis=0.8, bty="n", axes=F, prob=TRUE, col=scales::alpha(cols[2],.5), border="white", add=TRUE)
  hist(daten[survived == TRUE], main="", xlab="", ylab="", cex.lab=0.8, breaks=breaks, cex.axis=0.8, bty="n", axes=F, prob=TRUE, col=scales::alpha(cols[3],.5), border="white", add=TRUE)
  
  legend("topright",c("all","non-survivors","survivors"), fill = c(cols), bty = 'n', border = NA)
  
  lines(d1, col=cols[1], lty=1, lwd=2)
  lines(density(daten[survived == FALSE], na.rm=TRUE), col=cols[2], lty=2, lwd=2)
  lines(density(daten[survived == TRUE], na.rm=TRUE), col=cols[3], lty=2, lwd=2)
}
# call:
#hist.titanic(daten=age, TITLE="Titanic dataset", SUB="age (separated by surviving status)",
#             xaxtext="age")
########################## END OF FUNCTION

