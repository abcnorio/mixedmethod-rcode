# file:
# ptII_quan_classicstats_effectsizes_helpfuncs.r

# location:
# chap. 4 [4.6.10]
# Effektstärken — Größe, Häufigkeit und Bezug zur Originalskala

# HELPER FUNCTIONS

###### function to calculate and plot a function to relate N, pv and d
# smaller p-values due to increasing sample size
# and constant effect size
pv.vs.cd <- function(N=NA, mu0=0, mu1=1, s.both=1, seed, usesameseed=FALSE, log.pv=TRUE)
{
  seed <- 4466
  set.seed(seed)
  # use the same seeed?
  #usesameseed <- TRUE
  if(is.na(N)) N <- seq(2,1000,1)
  cohensd <- (mu0-mu1)/s.both
  cat("\nCohen's d = ",cohensd,sep="")
  N.l <- length(N)
  pv <- rep(NA,N.l)
  cd <- rep(NA,N.l)
  for(i in 1:N.l)
  {
    if(usesameseed) set.seed(seed)
    samp1 <- rnorm(N[i], mean=mu0, sd=s.both)
    samp2 <- rnorm(N[i], mean=mu1, sd=s.both)
    pv[i] <- t.test(samp1, samp2)$p.value
    cd[i] <- (mean(samp1)-mean(samp2))/sd(samp1) # due to theoretical value of sigma for both samples
  }
  par(oma=c(1,2,2,2), mfrow=c(1,2))
  if(log.pv)
  {
    pv.plotv <- log(pv) 
    ylabpv <- "log(p-value)"
  } else
  {
    pv.plotv <- pv
    ylabpv  <- "p-value"
  }  
  plot(N,pv.plotv,col="darkred", bty="n", pre.plot=grid(), lty=1, xlab="N", ylab=ylabpv)
  abline(lm.pv <- lm(pv.plotv~N), col="green", lwd=2)
  plot(N,cd,col="darkred", bty="n", pre.plot=grid(), lty=1, xlab="N", ylab="Cohen's d")
  abline(lm.cd <- lm(cd~N), col="green", lwd=2)
  abline(h=-1+c(-1,1)*0.1,col="blue", lty=2)
  mtext("N, p-values and effect size d", outer=TRUE, cex=1.5, line=-2)
return(list(pv=pv,cd=cd, lm.pv=lm.pv, lm.cd=lm.cd))  
}
# call:
# res.notsameseed <- pv.vs.cd()
# res.sameseed <- pv.vs.cd(usesameseed=TRUE)
########################## END OF FUNCTION

