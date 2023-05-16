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
# ptII_quan_classicstats_N-P_confint-errorbars_helpfuncs.r

# location:
# chap. 4 [4.5.2.6]
# Konfidenzintervalle

# HELPER FUNCTIONS


###### function to calculate how confidence intervals evolve with errorbars
CI.evolve <- function(N=NA, trials=NA, pop.mean=NA, pop.sd=NA, graph=TRUE, prob=0.95, seed=9876, digits=2)
{
 #digits = ... after comma
 #N = sample size
 #trials = number of drawing from population
 #pop.mean = population mean
 #pop.sd = population standard deviation
 #seed = random seed to reproduce result

 require(Hmisc)

 set.seed(seed)
 alpha <- 1-prob

 #create empty matrix for data
 res <- do.call("rbind", lapply(seq_along(1:trials), function(i)
 {
  x <- rnorm(N, mean=pop.mean, sd=pop.sd)
  mw.x <- mean(x)
  sd.x <- sd(x)
  se.x <- sd.x/sqrt(N)
  t.value <- qt(1-alpha/2,df=N-1)
  ci.halfwidth <- t.value*se.x
  ci.low <- mw.x - ci.halfwidth
  ci.up <- mw.x + ci.halfwidth
  c(ci.low, mw.x, ci.up, sd.x, se.x, 2*ci.halfwidth, t.value)
 }))
 colnames(res) <- c("CI.low","mean","CI.up","sd","se","CI.width","t-value")
 
 grand.mw <- mean(res[,"mean"])

 #plot errorbars and confidence limits
 if(graph == TRUE)
 {
  par(oma=c(2,1,1,1), "cex.axis"=1, bty="l")
  errbar(x=1:trials,y=res[,"mean"],
         yplus=res[,"CI.up"],yminus=res[,"CI.low"],
         col="green",pch=21,bg="red",errbar.col="blue",
         xlab=paste("Simulations (n = ",trials,")",sep=""),
		 ylab="Interval",
		 bty="n", pre.plot=grid())
  mtext(paste("Classical Confidence Interval [CI = ",(1-alpha)*100,"%]",sep=""), outer=TRUE, line=-2, cex=1.5, side=3)
  abline(h=grand.mw,col="red")
  abline(h=pop.mean,col="orange")
 }
 back <- list(res,pop.mean, pop.sd, trials, N, prob, seed)
 names(back) <- c("simulation","pop.mean","pop.sd","trials","N","prob","seed")
return(back)
}
# call:
# CI.evolve(N=30, trials=trials, pop.mean=pop.mean, pop.sd=pop.sd)
########################## END OF FUNCTION


###### function to calculate covered CIs
#empirically! how many CIs cover the "true" mean of the population we choose?
#question: lower/ upper limit of CI greater/ smaller than population mean?
#which are out of bounds? ie. outside of the CI limits?
CI.cover.mean <- function(sim.res, digits=2, id.out=TRUE)
{
 require(psych)

 simulation <- sim.res[["simulation"]]
 #descriptive statistics
 outofbounds <- simulation[,"CI.low"] > pop.mean | simulation[,"CI.up"] < pop.mean
 ids <- which(outofbounds == TRUE)
 ids.l <- length(ids)
 cat(paste("\n-- Simulation parameters --\n\n",sep=""))
 print(sim.res[-c(1)])
 cat(paste("\n-- Descriptive statistics --\n\n",sep=""))
 desc <- psych:::describe(simulation, IQR=TRUE)
 print(desc, digits=digits)
 cat(paste("\n-- Analysis of values lying outside of CI = ",sim.res[["prob"]]*100,"% --",sep=""))
 cat(paste("\n\namount:\t\t",ids.l,sep=""))
 cat(paste("\npercent:\t",ids.l/sim.res[["trials"]]*100,"%",sep=""))
 ids.mws <- cbind(ID=ids,Mittelwert=simulation[ids,])
 if(id.out == TRUE)
 {
  cat(paste("\n\nIDs and means\n\n",sep=""))
  print(ids.mws, digits)
 } else
 {
  cat(paste("\n\nIDs and means (heads and tails)\n\n",sep=""))
  print(head(ids.mws), digits=digits)
  print(tail(ids.mws), digits=digits)
 }
 cat(paste("\n",sep=""))
}
# call:
# CI.cover.mean(sim.res=sim.res, digits=digits)
########################## END OF FUNCTION


