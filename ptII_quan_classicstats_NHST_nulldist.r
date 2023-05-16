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
# ptII_quan_classicstats_NHST_nulldist.r

# location:
# chap. 4 [4.5.8]
# Die Wahrscheinlichkeit von Daten als Basis von Testentscheidungen


N <- 1000
crit.alpha <- 0.05
w <- c(-4,4)
sek <- seq(w[1],w[2],length.out=N)
pop.mw <- 0
pop.sd <- 1
sek.dnv <- dnorm(sek, mean=pop.mw, sd=pop.sd)

cols <- c("#FFA500B2","#90EE90B2")

# plot one curve
par(oma=c(2,1,1,1), "cex.axis"=1, bty="l")
plot(sek, sek.dnv, type="l", bty="n", pre.plot=grid(), col="red", xlab="z", ylab="Density", lty=2, lwd=1.6)
q.ca.low <- qnorm(crit.alpha/2, lower.tail=TRUE)
q.ca.up <- qnorm(crit.alpha/2, lower.tail=FALSE)
# crit alpha
polygon(x=c(sek[sek <= q.ca.low],q.ca.low,-3,-3), y=c(sek.dnv[sek <= q.ca.low],0,0,sek.dnv[1]), col=cols[1], border=NA)
polygon(x=c(q.ca.up,sek[sek >= q.ca.up],3,3), y=c(0,sek.dnv[sek >= q.ca.up],sek.dnv[N],0), col=cols[1], border=NA)
mtext("Rejection regions (two sided)", outer=TRUE, line=-2, cex=1.7, side=3)
mtext(eval(substitute(expression(paste("One sample (",alpha," = ",crit.alpha,")",sep="")),list(crit.alpha=crit.alpha))), outer=TRUE, line=-3.5, cex=1.2, side=3)
#error rates
text(x=-2.5,y=0.04,expression(alpha), cex=1.2)
text(x=2.5,y=0.04,expression(alpha), cex=1.2)


# plot 2 curves
# two-sided case
digits <- 2
par(mar=c(5,6,5,5), oma=c(2,1,1,1), cex.axis=0.8)
#par(oma=c(2,1,1,1), "cex.axis"=1, bty="l")
plot(c(-3,6),c(0,0.5), type="n", bty="n", pre.plot=grid(), xlab="z", ylab="Density")
lines(sek, sek.dnv, type="l", col="red", lty=2, lwd=1.4)
#crit alpha
polygon(x=c(sek[sek <= q.ca.low],q.ca.low,-3,-3), y=c(sek.dnv[sek <= q.ca.low],0,0,sek.dnv[1]), col=cols[1], border=NA)
polygon(x=c(q.ca.up,sek[sek >= q.ca.up],3,3), y=c(0,sek.dnv[sek >= q.ca.up],sek.dnv[N],0), col=cols[1], border=NA)
#second curve
ncp <- 3
pop.sd2 <- pop.sd
sek2 <- sek+ncp
lines(sek2, sek.dnv, type="l", col="blue", lty=2, lwd=1.4)
#crit beta
polygon(x=c(sek2[sek2 <= q.ca.up],q.ca.up,sek2[1]), y=c(sek.dnv[sek2 <= q.ca.up],sek.dnv[sek2==q.ca.up],0,0), col=cols[2], border=NA)
mtext("Rejection regions (two sided)", outer=TRUE, line=-2, cex=1.7, side=3)
betaerr <- pnorm(q=qnorm((1-crit.alpha/2),mean=pop.mw,sd=pop.sd), mean=ncp, sd=pop.sd2)
mtext(eval(substitute(expression(paste("Two samples (",alpha," = ",crit.alpha,", ",beta," = ",betaerr,")",sep="")),list(crit.alpha=crit.alpha,betaerr=round(betaerr,digits)))), outer=TRUE, line=-3.7, cex=1.2, side=3)
#error rates
text(x=-2.5,y=0.04,expression(alpha), cex=1.2)
text(x=2.5,y=0.04,expression(alpha), cex=1.2)
text(x=1.5,y=0.04,expression(beta), cex=1.2)
#numbers at the right side
mtext(eval(substitute(expression(paste(mu[1]," = ",pop.mw," |  ",sigma[1]," = ",pop.sd,"")),
      list(pop.mw=pop.mw, pop.sd=pop.sd ))) , 4, line=1, cex=0.9)
mtext(eval(substitute(expression(paste(mu[2]," = ",pop.mw2," |  ",sigma[2]," = ",pop.sd,"")),
      list(pop.mw2=pop.mw+ncp, pop.sd=pop.sd ))) , 4, line=2, cex=0.9)

# add a nice legend with information
par(fig=c(0, 1, 0, 1), oma=c(0, 0, 0, 0), mar=c(0, 0, 0, 0), new=TRUE)
plot(0, 0, type="n", bty="n", xaxt="n", yaxt="n")
lwd.width <- 3
legend("bottom", legend=c(expression(paste(alpha,"-error rate (Type I error)")),
                          expression(paste(beta,"-error rate (Type II error)")) ),
       xpd=TRUE, horiz=TRUE, inset=c(0,0), y.intersp=2.4,
       col=cols, lty=1, lwd=lwd.width, bty="n", cex=0.9) 
						   
						   


						   
#### NOT RUN (but works)
N <- 1000
crit.alpha <- 0.05
sek <- seq(-3,3,length.out=N)
sek.dnv <- dnorm(sek, mean=0, sd=1)
cols <- c("red","darkgreen","magenta","orange","lightgreen","violet")
cols.rgb <- col2rgb(cols[4:6])/255
cols
cols.rgb
 
x.range <- c(-3,6)
y.range <- c(0,0.5)
q.ca.low <- qnorm(crit.alpha/2, lower.tail=TRUE)
q.ca.up <- qnorm(crit.alpha/2, lower.tail=FALSE)
  

# plot 2 curves
par(mar=c(5,6,5,5), oma=c(2,1,1,1), cex.axis=0.8)
plot(x.range, y.range, type="n", xlab="", ylab="Density", bty="l", main="")
grid(col="grey80", lwd=1.2, lty=2)

#curve 1
lines(sek, sek.dnv, type="l", col=rgb(1,0,0), lty=1, lwd=1.4)

#crit alpha
#"#FFA500B2"
polygon(x=c(sek[sek <= q.ca.low],q.ca.low,-3,-3), y=c(sek.dnv[sek <= q.ca.low],0,0,sek.dnv[1]), col=rgb(cols.rgb[1,1],cols.rgb[2,1],cols.rgb[3,1],0.7), border=NA)
polygon(x=c(q.ca.up,sek[sek >= q.ca.up],3,3), y=c(0,sek.dnv[sek >= q.ca.up],sek.dnv[N],0), col=rgb(cols.rgb[1,1],cols.rgb[2,1],cols.rgb[3,1],0.7), border=NA)

ncp <- 3
sek2 <- sek+ncp
lines(sek2, sek.dnv, type="l", col=rgb(0,0,1), lty=1, lwd=1.4)
#crit beta
#"#90EE90B2"
polygon(x=c(sek2[sek2 <= q.ca.up],q.ca.up,sek2[1]), y=c(sek.dnv[sek2 <= q.ca.up],sek.dnv[sek2==q.ca.up],0,0), col=rgb(cols.rgb[1,2],cols.rgb[2,2],cols.rgb[3,2],0.7), border=NA)

axis(2)
axis(1)
 
mtext(expression(paste("Classical Nullhypothesis Test",sep="")), 3, line=2, cex=1.7)
mtext(expression(paste(alpha," and ",beta," error rate")), 3, line=0.6, cex=1.2)
mtext(expression(paste("z")), 1, line=3, cex=1.2)

# add a nice legend with information
pop.mw <- 0
pop.sd <- 1

 mtext(eval(substitute(expression(paste(lambda," = ",pop.mw," (mean)  |  ",sigma," = ",pop.sd," (sd)")),
       list(pop.mw=pop.mw, pop.sd=pop.sd ))) , 4, line=1, cex=0.9, col=rgb(1,0,0))
 mtext(eval(substitute(expression(paste(lambda," = ",pop.mw2," (mean)  |  ",sigma," = ",pop.sd," (sd)")),
       list(pop.mw2=pop.mw+ncp, pop.sd=pop.sd ))) , 4, line=2, cex=0.9, col=rgb(0,0,1))

 par(fig=c(0, 1, 0, 1), oma=c(0, 0, 0, 0), mar=c(0, 0, 0, 0), new=TRUE)
 plot(0, 0, type="n", bty="n", xaxt="n", yaxt="n")
# add a nice legend with information
lwd.width <- 3
legend("bottom", legend=c(expression(paste(alpha,"-error rate (Type I error)")),
                           expression(paste(beta,"-error rate (Type II error)"))
#                           expression(paste("p(",delta," | D" ["1"] ,", D" ["2"] ,", I)"))
			    ),
                           xpd=TRUE, horiz=TRUE, inset=c(0,0), y.intersp=2.4,
                           col=c(rgb(cols.rgb[1,1],cols.rgb[2,1],cols.rgb[3,1],0.7), rgb(cols.rgb[1,2],cols.rgb[2,2],cols.rgb[3,2],0.7)),
                           lty=1, lwd=lwd.width, bty="n", cex=0.9) 


                           
