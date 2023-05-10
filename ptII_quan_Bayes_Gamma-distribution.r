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
# ptII_quan_Bayes_Gamma-distribution.r

# location:
# chap. 6 [6.12]
# Die Wahl priorer Verteilungen



sek <- seq(-5,5,0.01)

par(mfrow=c(2,2), oma=c(2,1,4,1), "cex.axis"=1, bty="l")

plot(sek,gamma(sek), type="l", col="darkred", pre.plot=grid())
abline(h=0, lty=3, col="black")
plot(sek,lgamma(sek), type="l", col="darkred", pre.plot=grid())
abline(h=0, lty=3, col="black")     

plot(sek, dgamma(sek, shape=1, rate=1), type="l", col="darkred", ylim=c(0,5), bty="n", pre.plot=grid(),
     ylab="dgamma(shape=1, rate=various)")
rates <- seq(0.5,5,0.5)
for(i in 1:length(rates))
{
  lines(sek, dgamma(sek, shape=1, rate=rates[i]), col=i)
}
legend("topleft", legend=as.character(rates), col=1:length(rates), lwd=1,
       bty="n", cex=1, title="rate", ncol=2, inset=c(0.1,0))

plot(sek, dgamma(sek, shape=1, rate=1), type="l", col="darkred", ylim=c(0,2), bty="n", pre.plot=grid(),
     ylab="dgamma(shape=various, rate=1)")
shapes <- seq(0.5,5,0.5)
for(i in 1:length(shapes))
{
  lines(sek, dgamma(sek, shape=shapes[i],rate=1), col=i)
}
legend("topleft", legend=as.character(shapes), col=1:length(shapes), lwd=1,
       bty="n", cex=1, title="shape", ncol=2, inset=c(0.1,0))

mtext("Gamma function", side=3, line=0.8, cex=2, outer=TRUE)
mtext("gamma function (non-log, log), density function (shape=VA, rate=VA)", side=3, line=-1.8, cex=1.5, outer=TRUE)

