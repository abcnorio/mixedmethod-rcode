# file:
# ptII_quan_EDA_case_Suisse-fertility_helpfuncs.r

# location:
# chap. 5 [5.5.2]
# Fruchtbarkeit und Fertilität

# HELPFER FUNCTIONS


###### function to plot two variables along with a continous index
# https://stackoverflow.com/questions/13355176/gradient-legend-in-base
scaplot.cont <- function(x,y, TITLE="", SUB="", xtext="", ytext="", legendTITLE="", fac=1.1)
{
 xlim <- range(x)
 ylim <- range(y)
 
 layout(matrix(1:2,ncol=2), width = c(2,1),height = c(1,1))
 
 par(mar=c(5,5,4,2))
 par(oma=c(2,1,1,1))
 par("cex.axis"=0.8)
 
 plot(x,y, main="", xlab="", ylab="", cex.lab=0.8, cex.axis=0.8, bty="n", axes=F, col="white")

 axis(side = 1, pretty(xlim), tck =- .02, labels=NA, line=.6)
 axis(side = 2, pretty(ylim), tck = -.02, labels=NA, line=.6)
 axis(side = 1, lwd = 0, line = .4)
 axis(side = 2, lwd = 0, line = .4, las = 1)
 
 mtext(TITLE, 3, line=2, cex=1.5)
 mtext(SUB, 3, line=.252, cex=1.1)

 grid(col="grey80", lwd=1.2, lty=1)
 mtext(xtext, 1, line=4, cex=1)
 mtext(ytext, 2, line=4, cex=1) 
 
 # color ramp based on x
 require(hexbin)
 plotcolo <- BTC(length(x))
 points(x, y, col=plotcolo, pch=21, bg=plotcolo, bty="n", cex=fac)
 
 legend_image <- as.raster(matrix(plotcolo, ncol=1))
 plot(c(0,2), c(0,1), type ="n", axes=F, xlab="", ylab="", main=legendTITLE)
 legtext <- seq(floor(min(y)), ceiling(max(y)), l=5)
 text(x=1.5, y=seq(0, 1, l=5), labels=legtext)
 rasterImage(legend_image, 0, 0, 1,1)
}
# call:
# scaplot.cont(x=swiss$Catholic, y=swiss$Fertility,
#             TITLE="Swiss data on Fertility from 1888", SUB="Fertility versus Catholic",
#             legendTITLE="Catholic Index", xtext="Catholic", ytext="Fertility")
########################## END OF FUNCTION



