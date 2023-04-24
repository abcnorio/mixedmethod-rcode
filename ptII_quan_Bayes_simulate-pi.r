# file:
# ptII_quan_simulate-pi.r

# location:
# chap. 6 [6.13]
# Marko Chain Monte Carlo Simulationen — MCMC

# load necessary libs
library(plotrix)

# load necessary helper functions
source("ptII_quan_simulate-pi_helpfuncs.r")


seed <- 5
set.seed(seed)
for(i in 10^(1:7))
{
 cat("i = ",i,"\t",piR(i),"\n")
}

# piR(1e10)
# error -> uses too much RAM
# everything above piR(7) can block your computer...


# give out graph output pi simulation
# pi simulation
set.seed(9879)
n <- 5e+3
x <- runif(n,-1,1)
y <- runif(n,-1,1)
z <- sqrt(x^2+y^2)
head(z)
z.lowereq1 <- which(z <= 1)
z.greater1 <- which(z > 1)
# plot areas
par(oma=c(2,1,1,1), "cex.axis"=1, bty="l")
plot(x[z.lowereq1],y[z.lowereq1],xlab="X",ylab="Y",main="", col="darkred",
       pre.plot=grid(), bty="n",xlim=c(-1,1),ylim=c(-1,1),
       xaxt="n", yaxt="n",asp=1)
points(x[z.greater1],y[z.greater1],col='skyblue')
draw.circle(0, 0, 1, nv=100, col=NA, lty=1, lwd=4, border="orange")
axis(1, at=c(-1,0,1))
axis(2, at=c(-1,0,1))
pi.est <- length(z.lowereq1)*4/length(z)
mtext(expression(paste("Monte Carlo ",pi," estimation")),outer=TRUE,line=-2,cex=1.5, side=3)
mtext(eval(substitute(expression(paste("(",pi," ~ ",pi.est,")")),
                      list(pi.est=pi.est))),
      outer=TRUE,line=-3.5,cex=1.2, side=3)
