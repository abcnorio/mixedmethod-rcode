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
# ptII_quan_classicstats_normal-vs-t.r

# location:
# chap. 4 [4.5.8.2]
# Zum Verhältnis von Normalverteilung und t-Verteilung


# relationship of normal and t distribution
# normal distribution and t distribution rejection area under H0
sek1 <- seq(-6,6,length.out=150)
dfree <- 2
norm.dens <- dnorm(sek1)
xlim.n <- range(sek1)
ylim.n <- c(0,max(norm.dens))

t.dens <- dt(sek1, df=dfree)
xlim.t <- range(sek1)
ylim.t <- c(0,max(t.dens))

# plot
par(oma=c(2,1,1,1), "cex.axis"=1, bty="l")
plot(sek1, norm.dens, panel.first=grid(), type="l", col="red", bty="l", lwd=1, main="", xlab="", ylab="density")
lines(sek1, t.dens, col="steelblue", lwd=1, lty=2)
legend("topright", legend=c("normal",paste("t (df = ",dfree,")",sep="")),
       lty=c(1,2), lwd=1, col=c("red","steelblue"), bty="n", bg="yellow")
mtext("Normal versus t-distribution", outer=TRUE, line=-2, cex=1.5, side=3)
alpha <- 0.05

# critical regions
crit.nv <- c(-1,1)*qnorm(1-alpha/2)
crit.tv <- c(-1,1)*qt(1-alpha/2, df=dfree)

# histogram and density
color.n <- rgb(1,0,0,alpha=.2)
color.t <- rgb(70/255,130/255,180/255,alpha=.2)

# normal distribution
# lower limit rejection area
polygon(x=c(xlim.n[1],sek1[sek1 <= crit.nv[1]], crit.nv[1]),
        y=c(ylim.n[1],norm.dens[sek1 <= crit.nv[1]], ylim.n[1]), col=color.n, border=NA)
#upper limit rejection area
polygon(x=c(crit.nv[2],sek1[sek1 > crit.nv[2]],xlim.n[2]),
        y=c(ylim.n[1],norm.dens[sek1 >= crit.nv[2]],ylim.n[1]), col=color.n, border=NA)

# t distribution
# lower limit rejection area
polygon(x=c(xlim.t[1],sek1[sek1 <= crit.tv[1]], crit.tv[1]),
        y=c(ylim.t[1],t.dens[sek1 <= crit.tv[1]], ylim.t[1]), col=color.t, border=NA)
#upper limit rejection area
polygon(x=c(crit.tv[2],sek1[sek1 > crit.tv[2]],xlim.t[2]),
        y=c(ylim.t[1],t.dens[sek1 >= crit.tv[2]],ylim.t[1]), col=color.t, border=NA)

# critical values n vs. t
# ratio seen from t perspective
crit.nv
crit.tv
crit.tv/crit.nv


# probs n vs. t
pnorm(crit.nv)
pt(crit.nv,df=dfree)
pt(crit.nv,df=dfree)-pnorm(crit.nv)


# at which point (ie. higher df's for t) do normal-dist and t-dist roughly match?
# plot
par(oma=c(2,1,1,1), "cex.axis"=1, bty="l")
plot(log(abs(pt(crit.nv[1],df=1:200)-pnorm(crit.nv[1]))), type="l", bty="l", col="red", panel.first=grid(), xlab="df", ylab="log(p value diff)", main="")
mtext("Difference normal and t distribution", outer=TRUE, line=-2, cex=1.7, side=3)
mtext("p-value differences as a function of degrees of freedom", outer=TRUE, line=-3.6, cex=1.2, side=3)
# for different dfs
ids <- which(abs(pt(crit.nv[1],df=1:200)-pnorm(crit.nv[1])) < 0.001)
ids
abline(v=ids[1], col="darkred", lty=3, lwd=2)
	   
# normal distribution and t-distribution (various df's)
sek2 <- seq(-3, 3, length.out=100)
dfs <- 10
cols <- terrain.colors(dfs)

par(oma=c(2,1,1,1), "cex.axis"=1, bty="l")
plot(sek2, dnorm(sek2), panel.first=grid(), type="l", col="red", bty="l", lwd=2, main="", xlab="", ylab="Density")
for(i in 1:dfs) lines(sek2, dt(sek2, df=i), col=cols[i], lty=2)
legend("topright", legend=c("N",paste("t (",1:dfs,")",sep="")), lty=c(1, rep(2,dfs)), lwd=1, col=c("red",cols), bty="n", bg="yellow")
mtext("Normal versus t-distribution", outer=TRUE, line=-2, cex=1.7, side=3)
mtext("for various degrees of freedom", outer=TRUE, line=-3.6, cex=1.2, side=3)


