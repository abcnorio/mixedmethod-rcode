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



# 2020-20-02
# file:
# ptII_quan_classicstats_N-P_stat-signif-isNOT-practsignif.r
#
# location:
# chap. 4 [4.5.2.1]
# Praktische Bedeutsamkeit im Kontext von statistischer Bedeutsamkeit

# load necessary libs
library(pwr)

# load necessary script with general functions
source("ptall_generalfuncs.r")


# define population parameters
mu1 <- 100
mu2 <- 104
sigma <- 10

# seed for random numbers to replicate results
seed <- 9876
set.seed(seed)

# first sample
n1 <- 30
samp1 <- rnorm(n=n1, mean=mu1, sd=sigma)
samp2 <- rnorm(n=n1, mean=mu2, sd=sigma)
summary(samp1)
summary(samp2)
# check for identical d's based on different calcs of pooled sd
cohensd(samp1,samp2, check=TRUE)
t.test(samp1, samp2)

# second sample, same parameters, n2 = 2*n1
n2 <- 60
samp3 <- rnorm(n=n2, mean=mu1, sd=sigma)
samp4 <- rnorm(n=n2, mean=mu2, sd=sigma)
summary(samp3)
summary(samp4)
cohensd(samp3,samp4)
t.test(samp3, samp4)


#Cohen's d population
cohensd.pop <- (mu2-mu1) / sigma
cohensd.pop


#define plot function
plot.lines <- function(samp, colo="red", lty=2, titel="", fac=1.15, add=FALSE, breite=NA, pop.mw=NA, mu2=NA, sigma=NA, d=NA)
{
 ds <- density(samp)
 ds.y.max <- max(ds$y)
 ds.x.max <- ds$x[which(ds$y == max(ds$y))]
 if(length(breite) == 1 && is.na(breite))
 {
  breite <- list(x = range(ds$x), y = range(ds$y))
  breite$y <- breite$y * fac
 } 
 if(add == FALSE)
 {
  xdesc <- eval(substitute(expression(paste("N = ",ll," | ",mu[1]," = ",pop.mw," | ",mu[2]," = ",mu2," | ",sigma," = ",sabw," | ",delta," = ",d)),list(ll=length(samp), pop.mw=pop.mw, mu2=mu2, sabw=sigma, d=d)))
  plot(breite, type="n", main=titel, xlab="", ylab="Density", bty="l")
  mtext(xdesc, 4, line=1, cex=0.9)
  grid(col="grey80", lwd=1.2, lty=2)
 } 
 #curve
 lines(ds$x, ds$y, col=colo) 
 
 #max density
 lines(x=c(ds.x.max,ds.x.max),y=c(0,ds.y.max), col=colo, lty=2)
 
 #pop mean
 samp.pop.mw.ds.y <- ds$y[which(ds$x >= pop.mw)[1]]
 lines(x=c(pop.mw, pop.mw),y=c(0,samp.pop.mw.ds.y), col=colo, lty=1, lwd=2)

 #sample mean
 samp.mw <- mean(samp)
 samp.mw.ds.y <- ds$y[which(ds$x >= samp.mw)[1]]
 lines(x=c(samp.mw, samp.mw),y=c(0,samp.mw.ds.y), col=colo, lty=3, lwd=2)

 return(breite)
}
#call
par(mfrow=c(2,1), mar=c(5,6,5,5), oma=c(2,1,1,1), "cex.axis"=0.8)

#plot 1
breite1 <- plot.lines(samp=samp1, pop.mw=mu1, mu2=mu2, sigma=sigma, d=cohensd.pop)
plot.lines(samp2, colo="blue", pop.mw=mu2, add=TRUE)
mtext(expression(paste("sample 1",sep="")), 4, line=3, cex=0.9)

#plot 2
plot.lines(samp3, breite=breite1, pop.mw=mu1, mu2=mu2, sigma=sigma, d=cohensd.pop)
plot.lines(samp4, col="blue", pop.mw=mu2, add=TRUE)
mtext(expression(paste("sample 2",sep="")), 4, line=3, cex=0.9)

#title
mtext(expression(paste("Comparison of two normal distributed samples",sep="")), 3, line=-2, cex=1.5, outer=TRUE)
mtext(expression(paste("with constants ",mu[1],", ",mu[2],", ",sigma," and ",delta)), 3, line=2, cex=1.2, outer=FALSE)

#legend
par(fig=c(0, 1, 0, 1), oma=c(0, 0, 0, 0), mar=c(0, 0, 0, 0), new=TRUE)
plot(0, 0, type="n", bty="n", xaxt="n", yaxt="n")
# add a nice legend with information
legend("bottom", legend=c(  expression(paste(mu," (= population)")),
                            expression(paste(bar(x)," (= sample)")),
                            expression(paste("max. density (= sample)"))  ),
                            xpd=TRUE, horiz=TRUE, inset=c(0,0), y.intersp=2.4,
                            col="black", text.col="black",
                            lty=1:3, lwd=c(2,2,2), bty="n", cex=1) 

							
#power calculations
crit.alpha <- 0.05

#sample 1 vs 2
power.t.test(n=n1, delta=mu2-mu1, sd=sigma, sig.level=crit.alpha, power=NULL, type="two.sample")

#sample 3 vs 4
power.t.test(n=n2, delta=mu2-mu1, sd=sigma, sig.level=crit.alpha, power=NULL, type="two.sample")

#N necessary
power.t.test(n=NULL, delta=mu2-mu1, sd=sigma, sig.level=crit.alpha, power=0.90, type="two.sample")

#N
pwr.t.test(d=cohensd.pop, n=NULL, sig.level=crit.alpha, type="two.sample", power=0.90, alternative="two.sided")
     
	 
