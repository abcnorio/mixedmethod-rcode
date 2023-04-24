# 2020-20-02
# file:
# ptII_quan_classicstats_N-P_powerfunc.r
#
# location:
# chap. 4 [4.5.2.1]
# Praktische Bedeutsamkeit im Kontext von statistischer Bedeutsamkeit

# load necessary libs
library(pwr)

# power function - different effect sizes
alpha <- 0.05
N <- 100

# power <- 0.8
ES <- seq(-1,1,0.01)

# calculate power based on other values (see above)
res1 <- pwr.t.test(n=N,d=ES,power=NULL,sig.level=alpha,type="two.sample",alternative="less")
res2 <- pwr.t.test(n=N,d=ES,power=NULL,sig.level=alpha,type="two.sample",alternative="greater")
res3 <- pwr.t.test(n=N,d=ES,power=NULL,sig.level=alpha,type="two.sample",alternative="two.sided")

# plot power functions versus effect sizes for less/ greater/ two-sided
kol <- rainbow(3)
plot(res1[["d"]],res1[["power"]], panel.first=grid(), bty="l", type="l",lty=1,col=kol[1],main=paste("Power versus effect size (N=",N,")",sep=""),xlab="effect size",ylab="power")
lines(res2[["d"]],res2[["power"]],type="l",lty=2,col=kol[2])
lines(res3[["d"]],res3[["power"]],type="l",lty=3,col=kol[3])
legend("left", col=kol,lty=c(1,2,3),legend=c("less","greater","two sided"), bty="n", cex=1)

