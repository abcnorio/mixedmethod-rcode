# file:
# ptII_quan_classicstats_N-P_confint_p-t-value_helpfuncs.r

# location:
# chap. 4 [4.5.2.6]
# Konfidenzintervalle

# HELPER FUNCTIONS


###### function to simulate a lot of t-tests
sim.ttest <- function(n1=NA, mu1=NA, sigma1=NA, n2=NA, mu2=NA, sigma2=NA, trials=100, alpha=0.05, digits=2, seed=9876)
{
  set.seed(seed)
  res <- data.frame(do.call("rbind", lapply(seq_along(1:trials), function(i)
  {
    s1 <- rnorm(n1, mu1, sigma1)
    s2 <- rnorm(n2, mu2, sigma2)
    xbar1 <- mean(s1)
    xbar2 <- mean(s2)
    sd1 <- sd(s1)
    sd2 <- sd(s2)
    cd <- cohensd(s1, s2)[["d|pooled sd"]]
    delta <- xbar2-xbar1
    ttest <- c(as.numeric(unlist(t.test(s1, s2, alternative="two.sided", paired=FALSE, var.equal=FALSE))[1:7]),sd1,sd2,delta,cd)
  })))
  colnames(res) <- c("t","df","p","CI(low)","CI(up)","xbar1","xbar2","sd1","sd2","delta","d")
  return(res)
}
# call:
# ttest.res <- sim.ttest(n1=n1, mu1=mu1, sigma1=sigma1, n2=n2, mu2=mu2, sigma2=sigma2, trials=trials)
########################## END OF FUNCTION


###### function to compare delta vs. graph
plot.d.sim <- function(cohensd=NA)
{
 par(oma=c(2,1,1,1), "cex.axis"=1, bty="l")
 hist(cohensd, border="white", pre.plot=grid(), col="orange",prob=TRUE, xlab="Cohen's delta", ylab="Density", main="", breaks="Scott")
 cd.dens <- density(cohensd)
 cd.mean <- mean(cohensd)
 y.cd.mean <- cd.dens$y[cd.dens$x >= cd.mean][1]
 q025 <- quantile(cohensd, 0.025)
 q975 <- quantile(cohensd, 0.975)
 y.q025 <- cd.dens$y[cd.dens$x >= q025][1]
 y.q975 <- cd.dens$y[cd.dens$x >= q975][1]
 lines(cd.dens, col="steelblue", lwd=2)
 lines(list(x=c(cd.mean,cd.mean),y=c(0,y.cd.mean)), col="steelblue",lwd=2, lty=2)
 lines(list(x=c(q025,q025),y=c(0,y.q025)), col="steelblue",lwd=2, lty=2)
 lines(list(x=c(q975,q975),y=c(0,y.q975)), col="steelblue",lwd=2, lty=2)
 mtext("Histogramm [CI = 95%]", outer=TRUE, line=-2, cex=1.5, side=3)
}
# call:
# plot.d.sim(cohensd=cohensd)
########################## END OF FUNCTION


###################### NOT RUN BELOW THIS POINT

###### function to calculate simple upper/ lower confidence limit for parameter
#e.g. mean
classic.konf <- function(param, sd.param, N, ci=0.95)
{
 se.param <- sd.param/sqrt(N)
 t.val <- qt((1 - ci)/2, N - 1, lower.tail=FALSE)
 ci.up <- param + se.param * t.val
 ci.low <- param - se.param * t.val 
 konfi <- data.frame(ci.low, param, ci.up, sd.param, se.param, N, t.val, ci)
 colnames(konfi) <- c("lower","param","up","sd","se","N","t","ci")
 return(konfi)
}
# call:
# classic.konf(param=5, sd.param=1.5, N=100)
########################## END OF FUNCTION



