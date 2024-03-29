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
# ptII_qual_Boole_fuzzy-logic.r

# location:
# chap. 12 [12.7]
# Fuzzy Logic

# example fuzzy logic temperature measurement
cold <- c(rep(1,10),seq(1,0,length=20),rep(0,35))
length(cold)
warm <- c(rep(0,10),seq(0,1,length=10),rep(1,15),seq(1,0,length=15),rep(0,15))
length(warm)
hot <- c(rep(0,30),seq(0,1,length=15),rep(1,20))
length(hot)

par(oma=c(2,1,1,1), "cex.axis"=1, bty="l")
plot(cold, type="l", bty="n", col="blue", pre.plot=grid(), lwd=2, xlab="temperature", ylab="", lty=2)
lines(warm, col="green", lwd=2)
lines(hot, col="darkred", lwd=2, lty=3)
lnames <- c("cold","warm","hot")
colos <- c("blue","green","darkred")
legend("right",legend=lnames, col=colos, lty=c(2,1,3), lwd=2, bty="n")
mtext(expression(paste("Fuzzy logic - example temperature")),outer=TRUE,line=-2,cex=1.5, side=3)


