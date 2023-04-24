# file:
# ptII_quan_classicstats_outliers-and-influentialpoints.r

# location:
# chap. 4 [4.6.9.3]
# Ausreisser und einflussreiche Datenpunkte

# HELPER FUNCTIONS

###### function to plot outlier demo
plot.outlier <- function(a,b, weg=weg)
{
 par(oma=c(2,1,1,1), "cex.axis"=1, bty="l", mfrow=c(1,2))
 plot(a,b, col="steelblue", panel.first=grid(), cex=1.6, pch=21, bg="skyblue", bty="l", main="", xlab="a", ylab="b")
 abline(lm(b~a),col="magenta", lty=1, lwd=2)
 # local regression
 lines(lowess(b~a),col="darkgreen",lty=2,lwd=2)
 mtext("Outlier - good or bad?", outer=TRUE, line=-2, cex=1.5, side=3)
 mtext("all data (complete)", line=0.2)
 points(a[weg], b[weg], cex=1.6, pch=21, bg="orange", bty="l")
 text(a[weg], b[weg],weg, pos=2)

 # regression line without outlier
 plot(a[-weg], b[-weg], col="steelblue", panel.first=grid(), pch=21, cex=1.6, bg="skyblue", bty="l", main="", xlab="a", ylab="b")
 abline(lm(b[-weg]~a[-weg]),col="magenta",lty=1, lwd=2)
 # local regression without outlier
 lines(lowess(b[-weg]~a[-weg]),col="darkgreen", lty=2, lwd=2)
 mtext("outlier removed", line=0.2)

 par(fig=c(0,1,0,1), oma=c(1,0,0,0), mar=c(0,0,0,0), new=TRUE)
 plot(1, type="n", bty="n", xaxt="n", yaxt="n")
 legend("bottom", legend=c("regression line","lowess regression"), lty=c(1,2), lwd=2, xpd=TRUE, horiz=TRUE, 
        col=c("magenta","darkgreen"), bty="n", cex=.9)
}
#call:
#plot.outlier(a=a, b=b, weg=weg)
########################## END OF FUNCTION


