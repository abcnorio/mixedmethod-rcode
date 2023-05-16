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
# ptII_quan_classicstats_p-hacking-sim.r

# location:
# chap. 4 [4.6.2]
# Auf der Suche nach Signifikanzen — unbewusste Forschungsintentionen und p-hacking

# HELPER FUNCTIONS


###### function to plot simulation of a linear model via arm:::sim
plot.lm.sim <- function(lm.fit.sim)
{
  
  d.coef <- dim(lm.fit.sim@coef)
  d.sigma <- dim(lm.fit.sim@sigma)
  if(!is.null(d.sigma))
  {
    ds <- d.coef[2] + d.sigma[2]
  } else
  {
    ds <- d.coef[2] + 1
  } 
  ds <- ceiling(sqrt(ds))
  ds
  par(oma=c(2,1,1,1), "cex.axis"=1, bty="l", mfrow=c(ds,ds))
  for(i in 1:d.coef[2])
  {
    v <- lm.fit.sim@coef[,i]
    cv.nam <- colnames(lm.fit.sim@coef)
    hist(v, prob=TRUE, pre.plot=grid(), main="", col="steelblue", breaks="FD", border="white", ylab="Density", xlab=cv.nam[i])
    lines(density(v), col="orange", lwd=2)
  } 
  if(is.null(d.sigma))
  {
    hist(lm.fit.sim@sigma, prob=TRUE, pre.plot=grid(), main="", col="steelblue", breaks="FD", border="white", ylab="Density", xlab="sigma")
    lines(density(lm.fit.sim@sigma), col="magenta", lwd=2)
  } else
  {
    for(i in 1:d.sigma[2])
    {
      v <- lm.fit.sim@sigma[,i]
      cv.nam <- colnames(lm.fit.sim@sigma)
      hist(v, prob=TRUE, pre.plot=grid(), main="", col="steelblue", border="white", ylab="Density", xlab=cv.nam[i])
      lines(density(v), col="magenta", lwd=2)
    } 
  } 
  mtext("Distribution simulated values (linear model)", outer=TRUE, line=-2, cex=1.5, side=3)  
}
#call:
#plot.lm.sim(lm.fit.sim)
########################## END OF FUNCTION


###### function to simulate p-hacking
p.hack.sim <- function(mu0=0, sigma0=1, n=30, addon=5, d=0.1, alpha=0.05, digits=3, seed=0798,
                       pr=TRUE, ppaur=TRUE, paur=0.8, graph=TRUE, ESrange=c(0.09,1.5), ...)
{
 # d = effect size
 # alpha = critical alpha error rate
 # print(seed)
 set.seed(seed)
 F <- FALSE
 no <- 1
 res <- NULL
 N <- n
 while(F == FALSE)
 {
  if(no > 1)
  {
   s1.1 <- rnorm(addon, mu0, sigma0)
   s2.1 <- rnorm(addon, mu0+d, sigma0)
   s1 <- c(s1, s1.1)
   s2 <- c(s2, s2.1)
  } else if(no == 1)
  {
   s1 <- rnorm(n, mu0, sigma0)
   s2 <- rnorm(n, mu0+d, sigma0)
  } else stop()
 # tests
  pv <- t.test(s1, s2)$p.value
  cd <- cohensd(s1, s2, sd.theory=1)[2]
  res <- rbind(res, c(no, N,addon,mu0,mu0+d,sigma0,alpha,pv,d,cd))
  if(pv < alpha)
  {
    F <- TRUE
  } else
  {  
    N <- N + addon
    no <- no + 1
  }  
  #print(no)
 } 
 colnames(res)[c(1:9)] <- c("no","N|each group","N|addon","mu0","mu0+d","sigma0","alpha","pv","d|true|theory")
 res[1,"N|addon"] <- 0
 
 # print results
 if(pr) print(res, digits=3)
 
 # print a priori power test -> required sample size
 if(ppaur) print(power.t.test(n=NULL, delta=d, sd=sigma0, sig.level=alpha, power=paur, type="two.sample", alternative="two.sided", strict=FALSE))
 
 # plot required sample size against d
 if(graph)
 {
 # plot required sample sizes
  sek <- seq(ESrange[1],ESrange[2],0.01)
  nres <- NULL
  for(i in 1:length(sek))
  {
  # print(i)
   ptest <- power.t.test(n=NULL, delta=sek[i], sd=1, sig.level=alpha, power=paur, type="two.sample", alternative="two.sided", strict=FALSE)
   nres[i] <- ptest$n
  }
 
 restore.opt <- options()
 par(oma=c(2,1,1,1), "cex.axis"=1, bty="l", mfrow=c(2,1))
 plot(sek,nres,type="l", col="red", bty="n", pre.plot=grid(), xlab="d", ylab="N")
 abline(v=d, col="blue")
 abline(h=N, col="seagreen")
 points(d,N, pch=23, col="darkred", cex=1.5, bg="blue")
 
 plot(sek,log(nres),type="l", col="red", bty="n", pre.plot=grid(), xlab="d", ylab="log(N)")
 abline(v=d, col="blue")
 N.log <- log(N)
 abline(h=N.log, col="seagreen")
 points(d,N.log, pch=23, col="darkred", cex=1.5, bg="blue")
 
 mtext("p-hacking --- required sample size", outer=TRUE, line=-2, cex=1.5, side=3)
 mtext("based on power.t.test()", outer=TRUE, line=-3, cex=0.9, side=3)
 #legend
 par(fig=c(0,1,0,1), oma=c(1,0,0,0), mar=c(0,0,0,0), new=TRUE)
 plot(1, type="n", bty="n", xaxt="n", yaxt="n")
 legend("bottom", legend=c(paste("final sample size [N=",N,"}",sep=""),paste("theoretical effect size [d=",d,"]",sep="")),
        lty=1, lwd=2, xpd=TRUE, horiz=TRUE, col=c("seagreen","blue"), bty="n", cex=.9)
 options(restore.opt)
 }
return(res) 
}
#call:
#dev.off()
#res1 <- p.hack.sim(pr=TRUE)
########################## END OF FUNCTION

