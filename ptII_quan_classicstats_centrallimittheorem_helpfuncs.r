# file:
# ptII_quan_classicstats_centrallimittheorem.r

# location:
# chap. 4 [4.5.8.3]
# Exkurs --- zentraler Grenzwertsatz

# HELPER FUNCTIONS

###### function to simulate central limit theorem
clt.simulate <- function(fun, trials=1000, seed=9876, digits=2, stretchfak=1.25, PLOT=TRUE)
{
 set.seed(seed)
 print(fun)
 cat("\n")
 
 dis.sim <- function(fun)
 {
  res <- eval(fun)
  return(c(xbar=mean(res), sabw=sd(res)))
 }

 clt.sim <- t(replicate(trials, dis.sim(fun=fun)))
 
 if(PLOT)
 {
  clt.sim.xbar <- mean(clt.sim[,"xbar"])
  clt.sim.sd <- sd(clt.sim[,"xbar"])
  clt.sim.dens <- density(clt.sim[,"xbar"])
  clt.sim.range <- range(clt.sim[,"xbar"])
  sek1 <- seq(clt.sim.range[1], clt.sim.range[2], length.out=100)
  theory.norm.dens <- dnorm(sek1, mean=clt.sim.xbar, sd=clt.sim.sd)
  clt.sim.ylim <- c(0, max(theory.norm.dens,clt.sim.dens$y)) 

  par(mar=c(5,5,4,2))
  par(oma=c(1,1,4,1))
  par("cex.axis"=0.8)

  par(mfrow=c(2,2))
  #clt
  hist(clt.sim[,"xbar"], panel.first=grid(), ylim=clt.sim.ylim, prob=TRUE, border="white", col="skyblue", main="", xlab=expression(bar(x)), ylab="density", breaks="Scott")
  lines(clt.sim.dens, col="steelblue", lwd=2)
  lines(sek1, theory.norm.dens, col="red", lwd=2, lty=2)
  legend("topright", legend=c(eval(substitute(expression(paste(bar(x)," = ",mu1,)), list(mu1=round(mean(clt.sim[,"xbar"]),digits)))),
         c("normal dist.")), lty=c(1,2), lwd=2, col=c("steelblue","red"), bty="n", bg="yellow")
							
  #against nv
  qqnorm(clt.sim[,"xbar"], main="", xlab="normal distribution", ylab=expression(bar(x)), col="steelblue", pch=21, bg="skyblue", bty="l")
  grid()
  qqline(clt.sim[,"xbar"], col="red", lwd=2, lty=2)
 
  #sd
  clt.sim.sd.dens <- density(clt.sim[,"sabw"])
  hist(clt.sim[,"sabw"], panel.first=grid(), ylim=c(0,max(clt.sim.sd.dens$y)), prob=TRUE, border="white", col="skyblue", main="", xlab=c("s"), ylab="density", breaks="Scott")
  lines(clt.sim.sd.dens, col="steelblue", lwd=2)
  legend("topright", legend=c(paste("s = ",round(sd(clt.sim[,"xbar"]),digits),sep="")), lty=c(1,2), lwd=2, col=c("steelblue"), bty="n", bg="yellow")
 
  #one sample
  onesample <- eval(fun)
  onesample.dens <- density(onesample)
  onesample.range <- range(onesample.dens$x)
  sek2 <- seq(onesample.range[1], onesample.range[2], length.out=100)
  onesample.xbar <- mean(onesample)
  onesample.sd <- sd(onesample)
  hist(onesample, panel.first=grid(), xlim=c(onesample.range), ylim=c(0,max(onesample.dens$y)*stretchfak), prob=TRUE, border="white", col="skyblue", main="", xlab=expression(bar(x)), ylab="density", breaks="FD")
  lines(onesample.dens, col="steelblue", lwd=2)
  lines(sek2, dnorm(sek2, mean=onesample.xbar, sd=onesample.sd), col="red", lwd=2, lty=2)
  legend("topright", legend=c(eval(substitute(expression(paste(bar(x)," = ",xbar)), list(xbar=round(onesample.xbar,digits)))),
         paste("s =",round(onesample.sd,digits)), c("normal dist.")),
		 lty=c(1,1,2), lwd=2, col=c("steelblue","steelblue","red"), bty="n", bg="yellow")
  mtext(expression(paste("one sample")), 3, line=0.7, cex=1.1)
  
  #main title
  mtext(expression(paste("Simulation Central Limit Theorem (CLT)")), line=1.6, outer=TRUE, cex=1.5)
  mtext(paste("Call: '",deparse(fun),"'  |  trials = ",trials,sep=""), line=0, outer=TRUE, cex=1.2)
 }
return(clt.sim)
}							
#call:
#clt.simulate(fun=quote(rnorm(n=30, mean=4, sd=2)), trials=100)
########################## END OF FUNCTION


###### function to simulate CLT for different sample sizes, but each amount of trials
compare.clt.sim <- function(fun, Ns, trials, seed=9876)
{
 print(fun)
 print(Ns)
 res.clt.sim.Ns <- lapply(seq_along(Ns), function(x)
 {
  print(Ns[x])
  fun.temp1 <- gsub(pattern="X", replacement=Ns[x], x=deparse(fun), perl=FALSE, fixed=TRUE)
  clt.simulate(fun=parse(text=fun.temp1), trials=trials, PLOT=FALSE, seed=seed)
 })
return(res.clt.sim.Ns)
} 
#call:
#Ns <- c(15,30,50,100)
#fun <- quote(rnorm(n=X, mean=4, sd=2))
#c.clt.sim.res <- compare.clt.sim(fun=fun, Ns=Ns, trials=1000)
#str(c.clt.sim.res)
########################## END OF FUNCTION


###### function to simulate CLT for different Ns and standardize to mu=1, s=1 or not
clt.sim.diffN <- function(fun=quote(rnorm(n=X, mean=4, sd=2)), Ns, trials=1000, PLOT=TRUE, norma=TRUE, lwd=1, seed=9876)
{
 res.Ns <- compare.clt.sim(fun=fun, Ns=Ns, trials=trials, seed=seed)
 if(PLOT)
 {
  
  if(norma == TRUE)
  {
   #res.Ns.norm <- lapply(res.Ns, function(x) (x[,"xbar"]-mean(x[,"xbar"]))/sd(x[,"xbar"]))
   res.Ns.norm <- lapply(res.Ns, scale)
   res.Ns.dens <- lapply(res.Ns.norm, density)
   mu <- 0
   SD <- 1
   xlab <- expression(paste(bar(x)," (standardized to ",mu," = 0, s = 1)",sep=""))
  } else
  {
   res.Ns.dens <- lapply(res.Ns, function(x) density(x[,"xbar"]))
   mu <- mean(unlist(lapply(res.Ns, function(x) x[,"xbar"])))
   SD <- sd(unlist(lapply(res.Ns, function(x) x[,"xbar"])))
   xlab <- expression(bar(x))
  }
  ylim <- c(0, max(unlist(lapply(res.Ns.dens, function(x) max(x$y)))))
  xlim <- range(unlist(lapply(res.Ns.dens, function(x) range(x$x))))
  sek2 <- seq(xlim[1], xlim[2], length.out=100)
   
   par(mar=c(4,4,2,1))
   par(oma=c(1,1,4,1))
   par("cex.axis"=0.8)
   plot(1, panel.first=grid(), xlim=xlim, ylim=ylim, bty="l",  main="", xlab=xlab, ylab="density")
   Ns.l <- length(Ns)
   cols <- rainbow(Ns.l+1)
   for(i in 1:Ns.l)
   {
    lines(res.Ns.dens[[i]], col=cols[i+1], lty=2, lwd=lwd)
   } 
   lines(sek2, dnorm(sek2, mean=mu, sd=SD), col=cols[1], lwd=2, lty=1)
   legend("topright", legend=c("normal dist.",paste("N = ",Ns,sep="")),
          lty=c(1,rep(2,Ns.l)), lwd=lwd, col=c(cols), bty="n", bg="yellow")
   mtext(expression(paste("Simulation Central Limit Theorem (CLT)")), line=1.6, outer=TRUE, cex=1.5)
   mtext(paste("Call: '",deparse(fun),"'  |  trials = ",trials,sep=""), line=0, outer=TRUE, cex=1.2)
 } 
return(res.Ns)  
}
#call:
#Ns <- c(15, 30, 50, 100, 150, 1000, 10000)
#trials <- 100
#res.clt.sim.Ns <- clt.sim.diffN(fun=quote(rnorm(n=X, mean=4, sd=2)), Ns=Ns, norma=FALSE, seed=123565, trials=trials)  
#str(res.clt.sim.Ns)
#clt.sim.diffN(fun=quote(rnorm(n=X, mean=4, sd=2)), Ns=Ns, norma=TRUE, seed=123565, trials=trials)  
########################## END OF FUNCTION

