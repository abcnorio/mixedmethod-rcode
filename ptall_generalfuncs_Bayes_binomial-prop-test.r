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



# 2020-10-23
# file:
# ptall_generalfuncs_Bayes_binomial-prop-test.r

# location:
# none
# general functions for various scripts
# especially binomial Bayesian functions 

# content:
# helper functions for Bayesian binomial proportion test in various versions


### FUNCTION
# taken and modified from
# BayesianFirstAid:::summary.bayes_prop_test

BFA.summary.bayes_prop_test <- function(object, diff.crit=0.05, dig=3, groupn=NULL, datfn=NULL, ...)
{
  require(BayesianFirstAid)
  s <- round(object$stats, 3) 
  cat("\n  Data\n\n")
  #pad_width <- max(nchar(as.character(c(object$x, object$n)))) + 1
  datf <- data.frame(Successes=object$x, Failures=object$n-object$x, "N (total)"=object$n, check.names=FALSE)
  if(!is.null(datfn)) colnames(datf) <- c(datfn,"N (total)")
  if(is.null(groupn)) groupn <- paste("Group ",1:dim(datf)[1]," |  ",sep="")
  rownames(datf) <- groupn
  print(datf, right=FALSE) 
  # cat("number of successes: ", paste(str_pad(object$x, pad_width), collapse = ","), "\n", sep="")
  # cat("number of trials:    ", paste(str_pad(object$n, pad_width), collapse = ","), "\n", sep="")
  cat("\n")
  
  cat("  Model parameters and generated quantities\n\n")
  cat(paste("*- theta[i]\t   = the relative frequency of column '",colnames(datf)[1],"' for Group i\n",sep=""))
  cat("*- x_pred[i]\t   = predicted number of successes in a replication for Group i\n")
  cat("*- theta_diff[i,j] = the difference between two groups (theta[i] - theta[j])\n\n")
  cat("  Measures\n" )
  print(s[, c("mean", "sd", "HDIlo", "HDIup", "%<comp", "%>comp")])
  cat("\n*- 'HDIlo', 'HDIup'   = limits of a ", s[1, "HDI%"] ,"% HDI credible interval.\n", sep="")
  cat("*- '%<comp', '%>comp' = probabilities of the respective parameter being\n")
  cat("\t\t\tsmaller or larger than ", s[1, "comp"] ," (except for the theta_diff\n", sep="")
  cat("\t\t\tparameters where the comparison value 'comp' = 0.0).\n", sep="")
  
  cat("\n")
  cat("  Quantiles\n" )
  print(s[, c("q2.5%", "q25%", "median","q75%", "q97.5%")] )
  
  # taken from http://www.sumsar.net/blog/2014/06/bayesian-first-aid-prop-test/  
  thetas <- as.data.frame(object)
  pROPE.equal <- mean(abs(thetas$theta1 - thetas$theta2) < diff.crit)
  cat("\n  Probabilities and Odds Ratios in relation to ROPE [crit < ",diff.crit,"]\n",sep="") 
  cat("\nProbability, that the groups (sets) are equivalent/ the same\t= ",round(pROPE.equal,dig),sep="")
  cat("\nProbability, that the groups (sets) are different/ not the same\t= ",round(1 - pROPE.equal,dig),"\n",sep="")
  cat("\nOdds ratio in favor of the groups (sets) being equivalent/ the same\t= ",round(pROPE.equal / (1 - pROPE.equal),dig),sep="")
  cat("\nOdds ratio in favor of the groups (sets) being different/ not the same\t= ",round((1 - pROPE.equal) / pROPE.equal,dig),"\n",sep="")
  
  # prob: theta1 > theta2
  p.theta1.theta2 <- mean(thetas$theta1 > thetas$theta2)
  cat("\n  Probabilities and Odds Ratios (theta_1 > theta_2)\n",sep="") 
  cat("\nProbability, that theta_1 > theta_2 = ",round(p.theta1.theta2,dig),sep="")
  cat("\nProbability, that theta_1 < theta_2 = ",round(1 - p.theta1.theta2,dig),"\n",sep="")
  cat("\nOdds ratio in favor of theta_1 > theta_2 = ",round(p.theta1.theta2 / (1 - p.theta1.theta2),dig),sep="")
  cat("\nOdds ratio in favor of theta_1 < theta_2 = ",round((1 - p.theta1.theta2) / p.theta1.theta2,dig),"\n\n",sep="")  
  invisible(object$stats)
}
# call:
# BFA.summary.bayes_prop_test(ums2.bprop)
### END OF FUNCTION


### FUNCTION
# scatterplot 2D of 2 MCMC chains
# useful for Bayesian test of proportions
# ie. theta.diff = theta_1 - theta_2
BFA.mcmcplot.thetas <- function(mcmc.obj, dig=2, pr.theta=TRUE, ellips=FALSE, hpdi.region=TRUE, probs=c(0.99, 0.95, 0.69), nsamp=10000, fac=1.15, ...)
{
  require(grid)
  
  thetas <- as.data.frame(mcmc.obj)
  m.theta1 <- mean(thetas[,"theta1"])
  m.theta2 <- mean(thetas[,"theta2"])
  sd.theta1 <- sd(thetas[,"theta1"])
  sd.theta2 <- sd(thetas[,"theta2"])
  if(pr.theta)
  {
    print(head(thetas))
    cat("\n")
    print(tail(thetas))
    cat("\n")
    VARs <- apply(thetas,2,var)
    print(rbind(apply(thetas,2,summary),"var"=VARs,"sd"=sqrt(VARs)))
    cat("\n")
  } 
  
  x.max <- max(thetas[,"theta1"]) * fac
  y.max <- max(thetas[,"theta2"]) * fac
  if(x.max > 1) x.max <- 1
  if(y.max > 1) x.max <- 1
  x.lim <- c(min(thetas[,"theta1"]) - abs(1-fac)/2, x.max)
  y.lim <- c(min(thetas[,"theta2"]) - abs(1-fac)/2, y.max)
  
  par(mar=c(5,6,5,5), oma=c(2,1,1,1))
  par("cex.axis" = 0.8)
  plot(x.lim,y.lim,
       main="",
       xlab="", 
       ylab="", 
       col="white", bty="l", pch=19, cex=0.6, xlim=c(), ylim=c(), axes=FALSE)
  rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4], col="grey95", border=NA)
  grid(col="white", lwd=1, lty=1)
  points(thetas[,"theta1"],thetas[,"theta2"],col="blue", bty="l", pch=19, cex=0.6)
  axis(2)
  axis(1)
  abline(a=0, b=1, col="red", lwd=2, lty="dashed")
  mtext(eval(substitute(expression(paste(theta["1"],sep="")))), 1, line=3, cex=1.5, col="red")
  mtext(eval(substitute(expression(paste(theta["2"],sep="")))), 2, line=3, cex=1.5, col="red")
  mtext(eval(substitute(expression(paste("Bayesian Binomial Proportion Test",sep="")))), 3, line=2, cex=1.4)
  mtext(eval(substitute(expression(paste("MCMC Samples ",theta["1"]," vs. ",theta["2"],sep="")))), 3, line=0.7, cex=1, col="red")
  mtext(eval(substitute(expression(paste(bar(theta)["1"] ," = ",m.theta1,"  |  ",
                                         bar(theta)["2"] ," = ",m.theta2,"  |  ",
                                         sigma["1"] ," = ",sd.theta1, "  |  ",
                                         sigma["2"] ," = ",sd.theta2, "")),
                        list(m.theta1=round(m.theta1,dig),m.theta2=round(m.theta2,dig),sd.theta1=round(sd.theta1,dig),sd.theta2=round(sd.theta2,dig)))),
        4, line=1, cex=0.9, col="black")
  

  if(ellips & hpdi.region)
  {
    cat("\nDo not print ellipsis and hpdi regions in one print... standard = hpdi region\n")
    ellips <- FALSE
    hpdi.region <- TRUE
  }
  if(ellips)
  {
    require(MASS)
    require(cluster)
    
    thetas.sub <- thetas[,c("theta1","theta2")]
    cols <- c("green","orange","magenta")
    cat("\ncov.mve stage ...")
    for(i in 1:length(probs))
    {
      cat(i,"...")
      fit.mve <- cov.mve(thetas.sub, quantile.used=nrow(thetas) * probs[i], nsamp = nsamp)
      ellipse.points <- as.matrix(thetas.sub[fit.mve$best,])
      ellipse.boundary <- predict(ellipsoidhull(ellipse.points))
      lines(ellipse.boundary, col=cols[i], lwd=3)
    }
    cat("end\n") 
  }
  
  if(hpdi.region)
  {
    require(emdbook)
    
    thetas.sub <- thetas[,c("theta1","theta2")]
    cols <- c("orange", "green", "magenta")
    HPDregionplot(thetas.sub, prob=probs, col=cols, lty=c(1,2,3), lwd=c(3,3,3), add=TRUE)
    par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
    plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
    legend("bottom",legend=paste(c(round(probs*100)),"%",sep=""),
           xpd=TRUE, horiz=TRUE, inset=c(0,0), y.intersp=2.4,
           col=cols, lty=c(1,2,3), lwd=1.75, bty="n", cex=0.9)
  }
}
# call:
# BFA.mcmcplot.thetas(ums1.bprop)
### END OF FUNCTION


### FUNCTION
# plot MCMC chains 
bayes.plot.mcmc <- function(bprop.mcmc.res, cols=NULL, fac=1.15)
{
  
  if(is.null(cols)) cols <- c("lightgreen","salmon","lightsteelblue","yellowgreen")
  
  mcmc.v <- bprop.mcmc.res$mcmc
  nchains <- bprop.mcmc.res$meta[1,2]
  n.mcmc <- bprop.mcmc.res$meta[1,1]
  start.ids <- c(1,(1:(nchains-1))*n.mcmc+1)
  end.ids <- start.ids+(n.mcmc-1)
  # end.ids[length(end.ids)] <- nchains*n.mcmc
  start.ids
  end.ids
  startend.l <- length(start.ids)
  
  no.vars <- ncol(bprop.mcmc.res$mcmc)
  cnams <- colnames(bprop.mcmc.res$mcmc)
  
  par(oma=c(1,1,4,1))
  par("cex.axis" = 0.8)
  par(mfrow=c(3,2))
  
  for(i in 1:no.vars)
  {
    #  print(i)
    # traceplots MCMC chains
    # all  plot(mcmc[,i], type="l", col="magenta", bty="l", xlab="chain", ylab="", main=cnams[i])
    plot(mcmc.v[start.ids[1]:end.ids[1],i], type="l", col="magenta", bty="l", xlab="", ylab="", main="", axes=FALSE)
    rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4], col="grey95", border=NA)
    grid(col="white", lwd=1, lty=1)
    mtext(eval(substitute(expression(paste("MCMC chain")))), 1, line=3, cex=1, col="red")
    thetanam <- strsplit(cnams[i],split=" ")[[1]][2]
    mtext(eval(substitute(expression(paste(theta[thetanam])),list(thetanam=thetanam))), 3, line=1, cex=1, col="steelblue")
    for(j in 1:startend.l) lines(mcmc.v[start.ids[j]:end.ids[j],i],col=cols[j])
    for(j in 1:startend.l) lines(lowess(mcmc.v[start.ids[j]:end.ids[j],i]), col=rainbow(20)[round(runif(1:20)*20)])
    axis(2)
    axis(1)  
    
    # histogram and density plots
    dens <- density(mcmc.v[,i])
    y.max <- max(dens$y)
    hist(mcmc.v[,i], prob=TRUE, xlab="", ylab="Density", main="", ylim=c(0,y.max*fac), axes=FALSE)
    
    #mtext(eval(substitute(expression(paste(theta)))), 1, line=3, cex=1, col="red")
    mtext(eval(substitute(expression(paste("histogram + density")))), 1, line=3, cex=1, col="red")
    
    #mtext(eval(substitute(expression(paste(theta[thetanam]," = ",theta["1"],"-",theta["2"])), list(thetanam=thetanam))), 3, line=1, cex=1, col="steelblue")
    mtext(eval(substitute(expression(paste(theta[thetanam])), list(thetanam=thetanam))), 3, line=1, cex=1, col="steelblue")
    
    rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4], col="grey95", border=NA)
    grid(col="white", lwd=1, lty=1)  
    axis(2)
    axis(1)
    #  hist(mcmc.v[,i], add=TRUE, prob=TRUE, col="lightsteelblue", border="white")
    
    for(k in 1:startend.l)
    {
      #   print(k)
      plotobj <- mcmc.v[start.ids[k]:end.ids[k],i]
      col.v <- col2rgb(cols[k])/255
      hist(plotobj, add=TRUE, prob=TRUE, col=rgb(col.v[1], col.v[2], col.v[3], alpha=0.3), border="white")
    }
    
    for(k in 1:startend.l) lines(density(mcmc.v[start.ids[k]:end.ids[k],i]), col=cols[k])
    rug(jitter(mcmc.v[,i], amount = 0.1), ticksize=0.04, side=1, lwd=0.2, col="lightgreen", quiet=TRUE)
    axis(2)
    axis(1)
  }
  
  # main title
  mtext("Bayesian Proportion Test", col="steelblue", line=1, cex=1.5, outer=TRUE)
  mtext(eval(substitute(expression(paste("Brute force MCMC samples to calculate ",theta[thetanam])), list(thetanam=thetanam))), line=-1, cex=1, col="black", outer=TRUE)
  
}
# call:
# bayes.plot.mcmc(bprop.mcmc.res=res)
#
# TODO:
# - draw hdpinterval (density, hist) for theta_1, theta_2, theta_diff
# - insert main statistics (MAP, Median, Mean)
# - overlay of filled densities https://stackoverflow.com/questions/3541713/how-to-plot-two-histograms-together-in-r
#legend with 2D cubes
## calculate the density - don't plot yet
#densCarrot <- density(carrots)
#densCuke <- density(cukes)
### calculate the range of the graph
#xlim <- range(densCuke$x,densCarrot$x)
#ylim <- range(0,densCuke$y, densCarrot$y)
##pick the colours
#carrotCol <- rgb(1,0,0,0.2)
#cukeCol <- rgb(0,0,1,0.2)
### plot the carrots and set up most of the plot parameters
#plot(densCarrot, xlim = xlim, ylim = ylim, xlab = 'Lengths',
#     main = 'Distribution of carrots and cucumbers', 
#     panel.first = grid())
##put our density plots in
#polygon(densCarrot, density = -1, col = carrotCol)
#polygon(densCuke, density = -1, col = cukeCol)
### add a legend in the corner
#legend('topleft',c('Carrots','Cucumbers'),
#       fill = c(carrotCol, cukeCol), bty = 'n',
#       border = NA)
#
#### END OF FUNCTION



#### brute force - simulation from posterior draws
### FUNCTION
bayes.prop.mcmc <- function(a1, b1, a2, b2, n.mcmc=1e+3, nchains=3, credMass=0.95)
{
  require(coda)
  require(HDInterval)
  
  # calculate x chains of MCMC for theta_1 and theta_2
  chain.id <- rep(1:nchains, each=n.mcmc)
  mcmc1 <- replicate(nchains,rbeta(n.mcmc, a1, b1))
  mcmc2 <- replicate(nchains,rbeta(n.mcmc, a2, b2))
  dim(mcmc2) <- dim(mcmc1) <- c(n.mcmc*nchains, 1)
  
  # calculate theta.diff = theta_1 - theta_2
  tdiff.mcmc <- mcmc1 - mcmc2
  mcmc <- data.frame("theta 1"=mcmc1, "theta 2"=mcmc2, "theta (diff)"=tdiff.mcmc, check.names=FALSE)
  head(mcmc)
  tail(mcmc)
  
  cnams <- colnames(mcmc)
  
  # calculate summary statistics
  mcmc.summary <- apply(mcmc,2, function(y) c(summary(y),"sd"=sd(y),"var"=var(y)))
  mcmc.summary.nchain <- apply(mcmc, 2, function(x) tapply(x, chain.id, function(y) c(summary(y),"sd"=sd(y),"var"=var(y))))
  mcmc.summary.nchain.tab <- cbind("MCMC chain"=rep(1:nchains, each=3), do.call("rbind", lapply(mcmc.summary.nchain, function(x) do.call("rbind",x))))
  
  # calculate hdis
  hdinams <- rep(paste(c("HDI lower ","HDI upper "),"(",round(credMass*100),"%)",sep=""))
  mcmc.hdi <- data.frame(interval=hdinams, apply(mcmc, 2, function(x) hdi(x, credMass=credMass)), check.names=FALSE)
  mcmc.hdi.nchain <- apply(mcmc, 2, function(x) tapply(x, chain.id, function(y) hdi(y, credMass=credMass)))
  no.vars <- length(cnams)
  mcmc.hdi.nchain.tab <- data.frame(rep(cnams, each=length(hdinams)), rep(hdinams, no.vars), do.call("rbind", lapply(mcmc.hdi.nchain, function(x) do.call("cbind",x))), check.names=FALSE, row.names=NULL)
  colnames(mcmc.hdi.nchain.tab) <- c("nchain","interval",paste("MCMC chain",1:nchains,sep=" "))
  
  # mcmc.summary.nchain
  # mcmc.summary.nchain.hdi
  
  # mcmc.summary
  # mcmc.summary.nchain.tab
  # mcmc.hdi
  # mcmc.hdi.nchain.tab
  
  # sort results
  # res <- res[order(mcmcchain),]
  metainfo <- data.frame("No. MCMCs"=n.mcmc, "No. MCMC chains"=nchains, "cred. Mass"=credMass, check.names=FALSE)
  mcmc.res <- list(a1b1a2b2 = c(a1,b1,a2,b2),
                   meta = metainfo,
                   mcmc.summary = mcmc.summary,
                   mcmc.summary.nchain = mcmc.summary.nchain.tab,
                   mcmc.hdi = mcmc.hdi,
                   mcmc.hdi.nchain = mcmc.hdi.nchain.tab,
                   mcmc = mcmc)
  
  return(mcmc.res)
}
# call:
# bayes.prop.mcmc(a1=a1, b1=b1, a2=a2, b2=b2, n.mcmc=1e+3, nchains=3, credMass=0.95)
### END OF FUNCTION


### FUNCTION
bprop.mcmc <- function(tab, prior1=c(a=1,b=1), prior2=c(a=1,b=1), analyze="rows",
                       whichone=1, n.mcmc=1e3, credMass=0.95, nchains=4)
{
  
  # analyze by rows or cols
  if(analyze == "rows")
  {
    ntotal <- apply(tab,1,sum)
  } else if(analyze == "cols") #= "col"
  {
    ntotal <- apply(tab,2,sum)
    tab <- t(tab)
  } else stop("neither 'rows' nor 'cols' as perspective to analyze chosen!")
  
  # analyze by first row (col)
  if(whichone == 1)
  {
    si <- tab[1,1]
    sii <- tab[2,1] 
  } else if(whichone == 2) #whichone == 2
  {
    si <- tab[1,2]
    sii <- tab[2,2]
  } else stop("neither first nor second col/row given to analyze for! We analyze only 2x2 tables here.")
  Ni <- ntotal[1]
  Nii <- ntotal[2]
  
  # tab
  # ntotal
  # si
  # sii
  # Ni
  # Nii
  
  a1b1 <- unlist(bino.ab.post(a.prior=prior1["a"], b.prior=prior1["b"], si, Ni))
  a2b2 <- unlist(bino.ab.post(a.prior=prior2["a"], b.prior=prior2["b"], sii, Nii))
  names(a1b1) <- names(a2b2) <- c("a","b")
  
  # a1b1
  # a2b2
  
  res <- bayes.prop.mcmc(a1=a1b1["a"], b1=a1b1["b"], a2=a2b2["a"], b2=a2b2["b"],
                         n.mcmc=n.mcmc, nchains=nchains, credMass=credMass)
  return(res) 
}
# call:
# res.bprop.mcmc <- bprop.mcmc(pres.2x2)
### END OF FUNCTION


### brute force numerical integration / grid approximation
# totally inefficient, but works

### FUNCTION
bayes.prop.grid <- function(a1=a1, b1=b1, a2=a2, b2=b2, int.width=1e-3, start.sek=0, end.sek=1)
{
  sek <- seq(start.sek, end.sek, int.width)
  # important part: (x > y)
  grid.res <- outer(sek, sek, function(x, y) (x > y) * dbeta(x, a1, b1) * int.width * dbeta(y, a2, b2) * int.width)
  # (y - x <0.1)
  # grid.res <- outer(sek, sek, function(x, y) (y - x <0.1) * dbeta(x, a1, b1) * int.width * dbeta(y, a2, b2) * int.width)
  return(grid.res)
}
# call:
# prob.a1b1.vs.a2b2 <- bayes.prop.grid(a1=a1, b1=b1, a2=a2, b2=b2, int.width=1e-3)
### END OF FUNCTION


### exact tests

###### function to calculate closed form Bayesian A/B Testing after Evan Miller
### closed form exact solution (Evan Miller)


h <- function(a1, b1, a2, b2, loga=FALSE)
{
  # PR(p_B=GR2 > P_A=GR1)
  
  ### closed form exact solution (Evan Miller)

  # formula 6
  
  # important: if loga=TRUE
  # result is not subtracted from 1, ie. NOT DONE:
  # 1 - exp(h.hresult)
  
  cat("\nBayesian A/B Testing - closed form after Evan Miller\nNOTE: Test works with PR(GR2 > GR1)\n\n")
  
  ### SUB-FUNCTON
  h.part <- function(i, a1, b1, a2, b2)
  {
    # beta(a1 + i, b1 + b2) / ( (b2 + i) * beta(1 + i, b2) * beta(a1, b1)
    lbeta(a1 + i, b1 + b2) - ( log(b2 + i) + lbeta(1 + i, b2) + lbeta(a1, b1))
  }
  ### END OF SUBFUNCTION
  
  h.probs <- vector(length=(a2-1) )
  for(i in 0:(a2-1) )
  {
    h.probs[i] <- h.part(i=i, a1=a1, b1=b1, a2=a2, b2=b2)
  }
  if(loga==FALSE)
  {
    res <- sum(exp(h.probs))
    return(res)
  } else {
    require(Brobdingnag)
    h.prob <- brob(h.probs)
    res <- sum(h.prob)
    return(res)
  } 
}
################################ END OF FUNCTION



#### loss function/ decision rule by Chris Stucchio

###### function to calculate loss function/ decision rule by Chris Stucchio
# after the closed form Bayesian A/B Testing after Evan Miller
bayes.prop.loss <- function(a1, b1, a2, b2, crit=0.05, loga=TRUE, pr.out=TRUE)
{



  
  if(!loga)
  {  
    loss1 <- beta(a1 + 1, b1) / beta(a1, b1) * h(a1 + 1, b1, a2, b2)
    loss2 <- beta(a2 + 1, b2) / beta(a2, b2) * h(a1, b1, a2 + 1, b2)
    # GR1 > GR2
    loss.diff <- loss2 - loss1
  } else
  {  
    loss1 <- lbeta(a1 + 1, b1) - lbeta(a1, b1) + h(a1 + 1, b1, a2, b2)$h.prob.inv
    loss2 <- lbeta(a2 + 1, b2) - lbeta(a2, b2) + h(a1, b1, a2 + 1, b2)$h.prob.inv
    # GR1 > GR2
    loss.diff <- exp(loss2) - exp(loss1)
  }  
  res <- data.frame(loga, loss1, loss2, loss.diff, crit, 1-crit, loss.diff < crit, check.names=FALSE)
  colnames(res) <- c("LOG", "loss GR1", "loss GR2", "loss [GR2-GR1]", "crit","credMass", "loss [GR2-GR1] < crit")
  rownames(res) <- NULL
  if(pr.out)
  {
    cat("\n### Bayesian A/B Testing ###\n\nTest [Group_2 - Group_1] < crit\n\n")
    cat("a1 =",a1,", b1 =",b1,"\n\tvs.\na2 =",a2,", b2 =",b2,"\n\n")
    print(res)
  }  
  return(res)
}
################################ END OF FUNCTION


### more exact test

### FOLLOWING FUNCTIONS ARE REPROGRAMMED BUT IN PRINCIPLE TAKEN FROM
# Sverdlov, Ryeznik, Wu (2015)

### FUNCTION - old and do not user it anymore
pdf.theta.diff.old <- function(theta, a1, b1, a2, b2, loga=FALSE)
{
  # pdf of theta difference = theta_2 - theta_1 < crit
  # log and non-log
  # theta=sek
  
  # log version
  # require(appell)
  require(tolerance)
  # theta out of boundaries -1 < theta < 1
  # if(theta < -1 | theta > 1) stop(paste("Failure!!! theta out of boundaries, should be: -1 < theta < 1, but theta = ", theta,sep=""))
  
  if(loga) {
    # constant
    A.l.const <- lbeta(a1, b1) + lbeta(a2, b2)
    
    # theta = 0
    if(theta == 0) res <- log(1) - A.l.const + lbeta(a1 + a2 - 1, b1 + b2 - 1)
    
    # theta < 0
    if(theta < 0) {
      res <- log(1) - A.l.const + lbeta(a2, b1) + (b1 + b2 - 1)*log(-1 * theta) + (a2 + b1 - 1)*log(1 + theta) +
        #log( abs(appellf1(a = b1, b1 = a1 + b1 + a2 + b2 - 2, b2 = 1 - a1, c = a2 + b1, x = 1 + theta, y = 1 - theta^2)$val) )
        log( abs(F1(a = b1, b = a1 + b1 + a2 + b2 - 2, b.prime = 1 - a1, c = a2 + b1, x = 1 + theta, y = 1 - theta^2)) )
      #F1(a = 3, b = 4, b.prime = 5, c = 13, x = 0.2, y = 0.4)
    } 
    
    # theta > 0
    if(theta > 0) {
      res <- log(1) - A.l.const + lbeta(a1, b2) + (b1 + b2 - 1)*log(theta) + (a1 + b2 - 1)*log(1 - theta) +
        #log( abs(appellf1(a = b2, b1 = a1 + b1 + a2 + b2 - 2 , b2 = 1 - a2, c = a1 + b2, x = 1 - theta, y = 1 - theta^2)$val) )      
        log( abs(F1(a = b2, b = a1 + b1 + a2 + b2 - 2 , b.prime = 1 - a2, c = a1 + b2, x = 1 - theta, y = 1 - theta^2)) )      
    }
    attr(res,"scale") <- c("log")
    
  } else {
    
    # constant
    A.const <- beta(a1, b1) * beta(a2, b2)
    
    # theta = 0
    if(theta == 0) res <- 1 / A.const * beta(a1 + a2 - 1, b1 + b2 - 1)
    
    # theta < 0
    if(theta < 0) {
      res <- 1 / A.const * beta(a2, b1) * (-1 * theta)^(b1 + b2 - 1) * (1 + theta)^(a2 + b1 - 1) *
        #     abs(appellf1(a = b1, b1 = a1 + b1 + a2 + b2 - 2, b2 = 1 - a1, c = a2 + b1, x = 1 + theta, y = 1 - theta^2)$val)
        abs(F1(a = b1, b = a1 + b1 + a2 + b2 - 2, b.prime = 1 - a1, c = a2 + b1, x = 1 + theta, y = 1 - theta^2))
    }
    
    # theta > 0
    if(theta > 0) {
      res <- 1 / A.const * beta(a1, b2) * (theta)^(b1 + b2 - 1) * (1 - theta)^(a1 + b2 - 1) *
        #       abs(appellf1(a = b2, b1 = a1 + b1 + a2 + b2 - 2 , b2 = 1 - a2, c = a1 + b2, x = 1 - theta, y = 1 - theta^2)$val)
        abs(F1(a = b2, b = a1 + b1 + a2 + b2 - 2 , b.prime = 1 - a2, c = a1 + b2, x = 1 - theta, y = 1 - theta^2))
    }
  }
  
  return(res)
}
# call:
# pdf.theta.diff(theta=0.5, a1 = 1/3+0, b1 = 1/3+5-0, a2 = 1/3+2, b2 = 1/3+5-2, loga=FALSE)
# exp(pdf.theta.diff(theta=0.5, a1 = 1/3+0, b1 = 1/3+5-0, a2 = 1/3+2, b2 = 1/3+5-2, loga=TRUE))
# original paper version:
# d2beta("DIFF", x = 0.5, a1 = 1/3+0, b1 = 1/3+5-0, a2 = 1/3+2, b2 = 1/3+5-2)
### END OF FUNCTION


### FUNCTION
pdf.theta.ratio <- function(theta, a1, b1, a2, b2, loga=FALSE)
{
  # log and non-log
  # theta=sek
  
  if(theta < 0) print("theta < 0")
  
  # log version
  # require(appell)
  require(hypergeo)
  
  if(loga) {
    # constant
    A.l.const.1 <- lbeta(a1 + a2, b1) - lbeta(a1, b1) - lbeta(a2, b2)
    A.l.const.2 <- lbeta(a1 + a2, b2) - lbeta(a1, b1) - lbeta(a2, b2)
    
    # theta out of boundaries -1 < theta < 1
    # if(theta < 0) stop(paste("Failure!!! theta out of boundaries, should be: theta > 0, but theta = ", theta,sep=""))
    # if(theta < 0) theta <- 0
    
    # 0 < theta < 1
    if(theta <= 1) {
      #res <- A.l.const.1 + (a2 - 1)*log(theta) + log(abs( hyp2f1(a = a1 + a2, b = 1 - b2, c = a1 + a2 + b1, z = theta) ))
      res <- A.l.const.1 + (a2 - 1)*log(theta) + log(abs( hypergeo(A = a1 + a2, B = 1 - b2, C = a1 + a2 + b1, z = theta) ))
    }
    
    # theta < 0
    if(theta > 1) {
      #res <- A.l.const.2 + (-a1 - 1)*log(theta) + log(abs( hyp2f1(a = a1 + a2, b = 1 - b1, c = a1 + a2 + b2, z = 1 / theta )) )
      res <- A.l.const.2 + (-a1 - 1)*log(theta) + log(abs( hypergeo(A = a1 + a2, B = 1 - b1, C = a1 + a2 + b2, z = 1 / theta )) )
    } 
    attr(res,"scale") <- c("log")
    
  } else {
    # constant
    A.const.1 <- beta(a1 + a2, b1) / beta(a1, b1) / beta(a2, b2)
    A.const.2 <- beta(a1 + a2, b2) / beta(a1, b1) / beta(a2, b2)
    
    # theta out of boundaries -1 < theta < 1
    # if(theta < 0) stop(paste("Failure!!! theta out of boundaries, should be: theta > 0, but theta = ", theta,sep=""))
    # if(theta < 0) theta <- 0
    
    # 0 < theta < 1
    if(theta <= 1) {
      #res <- A.const.1 * (theta)^(a2 - 1) * abs( hyp2f1(a = a1 + a2, b = 1 - b2, c = a1 + a2 + b1, z = theta) )
      res <- A.const.1 * (theta)^(a2 - 1) * abs( hypergeo(A = a1 + a2, B = 1 - b2, C = a1 + a2 + b1, z = theta) )
    }
    
    # theta < 0
    if(theta > 1) {
      #res <- A.const.2 * (theta)^(-a1 - 1) * abs( hyp2f1(a = a1 + a2, b = 1 - b1, c = a1 + a2 + b2, z = 1 / theta ))
      res <- A.const.2 * (theta)^(-a1 - 1) * abs( hypergeo(A = a1 + a2, B = 1 - b1, C = a1 + a2 + b2, z = 1 / theta ))
    } 
  }
  
  return(res)
}
# call:
# pdf.theta.ratio(theta = 0.5, a1 = 1/3+7, b1 = 1/3+12-7, a2 = 1/3+6, b2 = 1/3+18-6, loga=FALSE)
# exp(pdf.theta.ratio(theta = 0.5, a1 = 1/3+7, b1 = 1/3+12-7, a2 = 1/3+6, b2 = 1/3+18-6, loga=TRUE))
# original paper version:
# d2beta("RR", x = 0.5, a1 = 1/3+7, b1 = 1/3+12-7, a2 = 1/3+6, b2 = 1/3+18-6)
### END OF FUNCTION


### FUNCTION
pdf.theta.OR <- function(theta, a1, b1, a2, b2, loga=FALSE)
{
  # pdf of theta OR = (p2/(1-p2)) / (p1/(1-p1))
  # see also Marshall (1988)
  # log and non-log
  # theta=sek
  
  if(theta < 0) print("theta < 0")
  
  # theta out of boundaries -1 < theta < 1
  # if(theta < 0) stop(paste("Failure!!! theta out of boundaries, should be: theta > 0, but theta = ", theta,sep=""))
  
  # log version
  # require(appell)
  require(hypergeo)
  
  if(loga) {
    # constant
    A.l.const <- lbeta(a1 + a2, b1 + b2) - lbeta(a1, b1) - lbeta(a2, b2)
    
    # theta <= 1
    if(theta <= 1) {
      #res <- A.l.const + (a2 - 1)*log(theta) + log(abs( hyp2f1(a = a2 + b2, b = a1 + a2, c = a1 + a2 + b1 + b2, z = 1 - theta) ))
      res <- A.l.const + (a2 - 1)*log(theta) + log(abs( hypergeo(A = a2 + b2, B = a1 + a2, C = a1 + a2 + b1 + b2, z = 1 - theta) ))
    }
    
    # theta > 1
    if(theta > 1) {
      #res <- A.l.const + (-b2 - 1)*log(theta) + log(abs( hyp2f1(a = a2 + b2, b = b1 + b2, c = a1 + a2 + b1 + b2, z = 1 - (1 / theta )) ))
      res <- A.l.const + (-b2 - 1)*log(theta) + log(abs( hypergeo(A = a2 + b2, B = b1 + b2, C = a1 + a2 + b1 + b2, z = 1 - (1 / theta )) ))
    } 
    attr(res,"scale") <- c("log")
    
  } else {
    # constant
    A.const <- beta(a1 + a2, b1 + b2) / beta(a1, b1) / beta(a2, b2)
    
    # theta <= 1
    if(theta <= 1) {
      #res <- A.const * (theta)^(a2 - 1) * abs( hyp2f1(a = a2 + b2, b = a1 + a2, c = a1 + a2 + b1 + b2, z = 1 - theta) )
      res <- A.const * (theta)^(a2 - 1) * abs( hypergeo(A = a2 + b2, B = a1 + a2, C = a1 + a2 + b1 + b2, z = 1 - theta) )
    }
    
    # theta > 1
    if(theta > 1) {
      #res <- A.const * (theta)^(-b2 - 1) * abs( hyp2f1(a = a2 + b2, b = b1 + b2, c = a1 + a2 + b1 + b2, z = 1 - (1 / theta )) )
      res <- A.const * (theta)^(-b2 - 1) * abs( hypergeo(A = a2 + b2, B = b1 + b2, C = a1 + a2 + b1 + b2, z = 1 - (1 / theta )) )
    } 
  }
  
  return(res)
}

# works only with library(appell)
pdf.theta.OR1 <- function(theta, a1, b1, a2, b2, loga=FALSE)
{
  # pdf of theta OR = (p2/(1-p2)) / (p1/(1-p1))
  # see also Marshall (1988)
  # log and non-log
  # theta=sek
  
  if(theta < 0) print("theta < 0")
  
  # theta out of boundaries -1 < theta < 1
  # if(theta < 0) stop(paste("Failure!!! theta out of boundaries, should be: theta > 0, but theta = ", theta,sep=""))
  
  # log version
  # require(appell)
  
  if(loga) {
    # constant
    A.l.const <- lbeta(a1 + a2, b1 + b2) - lbeta(a1, b1) - lbeta(a2, b2)
    
    # theta <= 1
    if(theta <= 1) {
      res <- A.l.const + (a2 - 1)*log(theta) + log(abs( hyp2f1(a = a2 + b2, b = a1 + a2, c = a1 + a2 + b1 + b2, z = 1 - theta) ))
    }
    
    # theta > 1
    if(theta > 1) {
      res <- A.l.const + (-b2 - 1)*log(theta) + log(abs( hyp2f1(a = a2 + b2, b = b1 + b2, c = a1 + a2 + b1 + b2, z = 1 - (1 / theta )) ))
    } 
    attr(res,"scale") <- c("log")
    
  } else {
    # constant
    A.const <- beta(a1 + a2, b1 + b2) / beta(a1, b1) / beta(a2, b2)
    
    # theta <= 1
    if(theta <= 1) {
      res <- A.const * (theta)^(a2 - 1) * abs( hyp2f1(a = a2 + b2, b = a1 + a2, c = a1 + a2 + b1 + b2, z = 1 - theta) )
    }
    
    # theta > 1
    if(theta > 1) {
      res <- A.const * (theta)^(-b2 - 1) * abs( hyp2f1(a = a2 + b2, b = b1 + b2, c = a1 + a2 + b1 + b2, z = 1 - (1 / theta )) )
    } 
  }
  
  return(res)
}
# call:
# pdf.theta.OR(theta = 0.5, a1 = 1/3+7, b1 = 1/3+12-7, a2 = 1/3+6, b2 = 1/3+18-6, loga=FALSE)
# exp(pdf.theta.OR(theta = 0.5, a1 = 1/3+7, b1 = 1/3+12-7, a2 = 1/3+6, b2 = 1/3+18-6, loga=TRUE))
# original paper version:
# d2beta("OR", x = 0.5, a1 = 1/3+7, b1 = 1/3+12-7, a2 = 1/3+6, b2 = 1/3+18-6)
### END OF FUNCTION


### FUNCTION
cdf.theta.diff <- function(theta, a1, b1, a2, b2)
{
  # theta=sek
  # theta out of boundaries -1 < theta < 1
  
  if(any(theta < -1 | theta > 1)) stop(paste("Failure!!! theta out of boundaries, should be: -1 < theta < 1, but theta = ", theta,sep=""))
  
  cdf.theta.diff.belo.eq0 <- function(lfd, a1, b1, a2, b2)
  {
    integrate(function(te) pbeta(lfd + te, a2, b2) * dbeta(te, a1, b1), -lfd, 1)$value
  } 
  
  cdf.theta.diff.abov0 <- function(lfd, a1, b1, a2, b2)
  {
    integrate(function(te) pbeta(lfd + te, a2, b2) * dbeta(te, a1, b1), 0, 1 - lfd)$value +
      integrate(function(te) dbeta(te, a1, b1), 1 - lfd, 1)$value
  } 
  
  if(theta > -1 | theta <= 0) cdf <- cdf.theta.diff.belo.eq0(lfd=theta, a1, b1, a2, b2)
  if(theta > 0 | theta <= 1) cdf <- cdf.theta.diff.abov0(lfd=theta, a1, b1, a2, b2)
  
  return(cdf)
}
# call:
# cdf.theta.diff(theta=0, a1 = 1/3+0, b1 = 1/3+5-0, a2 = 1/3+2, b2 = 1/3+5-2)
# original paper version:
# p2beta ("DIFF", "DIRECT", x = 0, a1 = 1/3+0, b1 = 1/3+5-0, a2 = 1/3+2, b2 = 1/3+5-2, n = 1000000)
# p2beta ("DIFF", "SIMULATION", x = 0, a1 = 1/3+0, b1 = 1/3+5-0, a2 = 1/3+2, b2 = 1/3+5-2, n = 1000000)
### END OF FUNCTION


### FUNCTION
cdf.theta.ratio <- function(theta, a1, b1, a2, b2)
{
  # theta=sek
  # theta out of boundaries -1 < theta < 1
  # if(any(theta < 0)) stop(paste("Failure!!! theta out of boundaries, should be: 0 < theta, but theta = ", theta,sep=""))
  
  if(theta < 0) cdf <- 0
  
  cdf.theta.ratio.betw0and1 <- function(theta, a1, b1, a2, b2)
  {
    integrate(function(te) pbeta(theta * te, a2, b2) * dbeta(te, a1, b1), 0, 1)$value
  }
  
  cdf.theta.ratio.abov1 <- function(theta, a1, b1, a2, b2)
  {
    integrate(function(te) pbeta(theta * te, a2, b2) * dbeta(te, a1, b1), 0, 1/theta)$value +
    integrate(function(te) dbeta(te, a1, b1), 1/theta, 1)$value
  }
  
  # if(theta >= 0 & theta <= 1) cdf <- integrate(function(te) pbeta(theta * te, a2, b2) * dbeta(te, a1, b1), 0, 1)$value
  if(theta >= 0 & theta <= 1) cdf <- cdf.theta.ratio.betw0and1(theta, a1, b1, a2, b2)
  
  # if(theta > 1) cdf <- integrate(function(te) pbeta(theta * te, a2, b2) * dbeta(te, a1, b1), 0, 1/theta)$value +
  #                      integrate(function(te) dbeta(te, a1, b1), 1/theta, 1)$value
  if(theta > 1) cdf <- cdf.theta.ratio.abov1(theta, a1, b1, a2, b2)
  
  return(cdf)
}
# call:
# cdf.theta.ratio(theta = 0.5, a1 = 1/3+7, b1 = 1/3+12-7, a2 = 1/3+6, b2 = 1/3+18-6)
# original paper version:
# p2beta(relation='RR', approach='DIRECT', x = 0.5, a1 = 1/3+7, b1 = 1/3+12-7, a2 = 1/3+6, b2 = 1/3+18-6)
### END OF FUNCTION


### FUNCTION
cdf.theta.OR <- function(theta, a1, b1, a2, b2)
{
 # theta=sek
 # theta out of boundaries -1 < theta < 1

 if(any(theta < 0)) stop(paste("Failure!!! theta out of boundaries, should be: 0 < theta, but theta = ", theta,sep=""))

 cdf.theta.OR.abov0 <- function(lfd, a1, b1, a2, b2)
 {
  integrate(function(te) pf(a1 * b2 / a2 / b1 * lfd * te, 2 * a2, 2 * b2) * df(te, 2 * a1, 2 * b1), 0, Inf)$value
 }

 cdf <- cdf.theta.OR.abov0(lfd=theta, a1, b1, a2, b2)

return(cdf)
}
# call:
# cdf.theta.OR(theta = 0.5, a1 = 1/3+7, b1 = 1/3+12-7, a2 = 1/3+6, b2 = 1/3+18-6)
# original paper version:
# p2beta(relation='OR', approach='DIRECT', x = 0.5, a1 = 1/3+7, b1 = 1/3+12-7, a2 = 1/3+6, b2 = 1/3+18-6)
### END OF FUNCTION


### FUNCTION

theta.diff.hdi <- function(a1, b1, a2, b2, alpha, le, re)
{
 # CI in % (Nelder-Mead algorithm)
 # based on theta_2 - theta_1

 theta.2v1.diff.optim <- function(x) {
      abs(cdf.theta.diff(theta=x[2], a1, b1, a2, b2) - 
          cdf.theta.diff(theta=x[1], a1, b1, a2, b2) - 1 + alpha) +
      abs(pdf.theta.diff(theta=x[2], a1, b1, a2, b2) - 
          pdf.theta.diff(theta=x[1], a1, b1, a2, b2))
 }
 res <- optim(c(le, re), theta.2v1.diff.optim)
return(res)
}
# call:
# theta.diff.hdi(a1 = 1/3+0, b1 = 1/3+5-0, a2 = 1/3+2, b2 = 1/3+5-2, alpha=.05, le=-.2, re=.8)
# original paper version:
# ci2beta(relation='DIFF', method='neldermead', a1 = 1/3+0, b1 = 1/3+5-0, a2 = 1/3+2, b2 = 1/3+5-2, alpha=.05, left0=-.2, right0=.8)
### END OF FUNCTION


### FUNCTION
CI.DIFF.inv.cdf <- function(a1, b1, a2, b2, alph, le=-0.9999, re=0.9999, tol=1e-5, methode="diff")
{
 # CI in % inverse CDF -> quantile function

 alpha <- c(alph/2,1-alph/2)
 a.l <- 2 # length(alpha)
 res <- rep(NA,a.l)
 methoden <- c("ratio","OR")

 cat("\n",paste(c("lower","upper"),alpha,sep="="),"\n\n")
 if(methode == "diff")
 {
  cdf.func <- function(te, a1, b1, a2, b2, a) cdf.theta.diff(te, a1, b1, a2, b2) - a
 } else if(methode %in% methoden)
 {
  cdf.func <- function(te, a1, b1, a2, b2, a) cdf.theta.ratio(tan(pi*te/2), a1, b1, a2, b2) - a
 } else stop("wrong method - does not exist!.")
 cdf.func
 a.l
  for(i in 1:a.l)
 {  
  res[i] <- uniroot(cdf.func, c(le,re), a1=a1, b1=b1, a2=a2, b2=b2, a=alpha[i])$root
  if(methode %in% methoden) res[i] <- tan(pi*res[i]/2)
 }
return(res) 
}
# call:
# CI.DIFF.inv.cdf(a1 = 1/3+0, b1 = 1/3+5-0, a2 = 1/3+2, b2 = 1/3+5-2, alph=0.05, tol=1e-5, methode="diff")
# original paper version:
# ci2beta(relation='DIFF', method='inv.cdf', a1 = 1/3+0, b1 = 1/3+5-0, a2 = 1/3+2, b2 = 1/3+5-2, alpha=.05, left0=0, right0=0)
### END OF FUNCTION


### THE FOLLOWING FUNCTIONS ARE TO HANDLE EXACT TESTS ABOVE (Sverdlov et al, 2015)
### AND SUMMARIZE AND PLOT

# Euler integral

# Rec > Reb > 0

# Euler transformation


#f15.3.1(2,1,2,-1/2) -2/3

#hypergeo.brob(a=2, b=1, c=2, z=-1/2, numer=FALSE)
#a=2
#b=1
#c=2
#z=-1/2
#sekk <- seq(-10,10,length=1000)
#sekk.d <- vector()
#for(i in 1:length(sekk))
#{
#  sekk.d[i] <- as.numeric(F2.1.brob(sekk[i]))
#}
#par(mfrow=c(1,2))
#plot(sekk,log(sekk.d), type="l", col="darkred", bty="n", pre.plot=grid())
#plot(sekk,sekk.d, type="l", col="darkred", bty="n", pre.plot=grid())
#
#sekk <- seq(0,1,length=1000)
#sekk.l <- list()
#for(i in 1:length(sekk))
#{
#  sekk.l[[i]] <- F2.1.log(sekk[i])
#}
#par(mfrow=c(1,2))
#sekk.l.n <- unlist(lapply(sekk.l, as.numeric))
#sekk.l.l <- unlist( lapply(sekk.l, function(x) x@x) )
#plot(sekk,sekk.l.l, type="l", col="darkred", bty="n", pre.plot=grid())
#plot(sekk,sekk.l.n, type="l", col="darkred", bty="n", pre.plot=grid())

################################
### same function, but gives everything back
hypergeo.brob <- function(a, b, c, z, sL=0, sH=1,
                          numer=TRUE, parallel=TRUE,
                          debug=FALSE, BROB=FALSE, loga=FALSE, ...)
{
  
  #  if( !( abs(x) < 1 ) ) cat("not given: abs(x) < 1\tx = ",x,"\n")
  #  if( !( abs(y) < 1 ) ) cat("not given: abs(y) < 1\ty = ",y,"\n")
  #  if( !( c-a > 0 ) ) cat("not given: c-a > 0\tc-a = ",c-a,"\n")
  #  if( !( a > 0 ) ) cat("not given: a > 0\ta = ",a,"\n")
  # if(debug)
  # {
  #   cat("\n")
  #   cat("abs(x) = ",abs(x),"\trequired: abs(x) < 1 \t: ",abs(x) < 1,"\n")
  #   cat("abs(y) = ",abs(y),"\trequired: abs(y) < 1 \t: ",abs(y) < 1,"\n")
  #   cat("c-a    = ",c-a,"\t\trequired: c-a > 0 \t: ",c-a > 0,"\n")
  #   cat("a      = ",a,"\t\trequired: a > 0 \t: ",a > 0,"\n")
  # }
  
  require(Brobdingnag)
  
  gauss.hypergeo.brob <- function(u)
  {
    as.brob(1)/
      as.brob(beta(b, c-b)) *
      as.brob(u) ^ (b-1) *
      as.brob( (1-u) ^ (c-b-1) ) *
      as.brob(1-z*u) ^ (-a)
  }
  
  gauss.hypergeo.log <- function(u)
  {
    temp <- 
      log(1) -
      lbeta(b, c-b) +
      (b-1) * log(u)  +
      (c-b-1) * log(1-u) +
      (-a) * log(1-z*u)
    #return(as.brob(temp))
    return(temp)
  }
  
  ###?????res1 <- brob(lgamma(c) - lgamma(b) - lgamma(c-b))
  #res1 <- 1/brob(lbeta(b,c-b))  
  if(BROB)
  {    
    # gets back either numerical value or list with single values + sum of single values
    # of function
    if(parallel)
    {
      res <- sintegral.brob.parallel.2(gauss.hypergeo.brob, sL=sL, sH=sH, Nsteps=250, numer=numer)
    } else
    {
      res <- simpsonrule.brob.2(gauss.hypergeo.brob, sL=sL, sH=sH, Nsteps=250, numer=numer)
    }  
  } else
  {
    res <- sintegral.brob.parallel.2(gauss.hypergeo.log, sL=sL, sH=sH, Nsteps=250, numer=numer, BROB=BROB, loga=loga)
  }  
  if(numer) res <- res else res <- list(F1=res[["sum"]],steps=res)
  return(res)
}
################################

#a=a2+b2
#b=a1+a2
#c=a1+a2+b1+b2
#z=1-theta

################################
### FUNCTION
pdf.theta.OR.brob <- function(theta, a1, b1, a2, b2, numer=TRUE, debug=FALSE, parallel=TRUE, loga=FALSE, BROB=FALSE, sL=0, sH=1)
{
  # pdf of theta OR = (p2/(1-p2)) / (p1/(1-p1))
  # see also Marshall (1988)
  # log and non-log
  # theta=sek
  
  #print(BROB)
  #print(loga)
  if(theta < 0) print("theta < 0")
  
  # theta out of boundaries -1 < theta < 1
  # if(theta < 0) stop(paste("Failure!!! theta out of boundaries, should be: theta > 0, but theta = ", theta,sep=""))
  
  # log version
  # require(appell)
  require(hypergeo)
  
  if(loga) {
    # constant
    A.l.const <- lbeta(a1 + a2, b1 + b2) - lbeta(a1, b1) - lbeta(a2, b2)
    
    # theta <= 1
    if(theta <= 1) {
      
      hypergeo.brob.res <- hypergeo.brob(a=a2+b2, b=a1+a2, c=a1+a2+b1+b2, z=1-theta,
                                         numer=numer, debug=debug,
                                         parallel=parallel, BROB=BROB, loga=loga,
                                         sL=sL, sH=sH)
      # BROB
      if(BROB | (!BROB & loga))
      {
        if(numer) hypergeo.res <- hypergeo.brob.res@x else hypergeo.res <- hypergeo.brob.res[["F1"]]@x
      } else
      {  
        # non BROB
        if(numer) hypergeo.res <- hypergeo.brob.res else hypergeo.res <- hypergeo.brob.res[["F1"]]
      }
      
      res <- A.l.const + (a2 - 1)*log(theta) + hypergeo.res
      
      #res <- A.l.const + (a2 - 1)*log(theta) + log(abs( hyp2f1(a = a2 + b2, b = a1 + a2, c = a1 + a2 + b1 + b2, z = 1 - theta) ))
      
      #res <- A.l.const + (a2 - 1)*log(theta) + log(abs( hypergeo(A = a2 + b2, B = a1 + a2, C = a1 + a2 + b1 + b2, z = 1 - theta) ))
    }
    
    # theta > 1
    if(theta > 1) {
      hypergeo.brob.res <- hypergeo.brob(a=a2+b2, b=b1+b2, c=a1+a2+b1+b2, z=1-(1/theta),
                                         numer=numer, debug=debug,
                                         parallel=parallel, BROB=BROB, loga=loga,
                                         sL=sL, sH=sH)
      
      # BROB
      if(BROB | (!BROB & loga))
      {
        if(numer) hypergeo.res <- hypergeo.brob.res@x else hypergeo.res <- hypergeo.brob.res[["F1"]]@x
      } else
      {  
        # non BROB
        if(numer) hypergeo.res <- hypergeo.brob.res else hypergeo.res <- hypergeo.brob.res[["F1"]]
      }      
      
      res <- A.l.const + (-b2 - 1)*log(theta) + hypergeo.res
      
      #res <- A.l.const + (-b2 - 1)*log(theta) + log(abs( hyp2f1(a = a2 + b2, b = b1 + b2, c = a1 + a2 + b1 + b2, z = 1 - (1 / theta )) ))
      
      #res <- A.l.const + (-b2 - 1)*log(theta) + log(abs( hypergeo(A = a2 + b2, B = b1 + b2, C = a1 + a2 + b1 + b2, z = 1 - (1 / theta )) ))
    } 
    attr(res,"scale") <- c("log")
    
  } else {
    # constant
    A.const <- beta(a1 + a2, b1 + b2) / beta(a1, b1) / beta(a2, b2)
    
    # theta <= 1
    if(theta <= 1) {
      #res <- A.const * (theta)^(a2 - 1) * abs( hyp2f1(a = a2 + b2, b = a1 + a2, c = a1 + a2 + b1 + b2, z = 1 - theta) )
      
      #res <- A.const * (theta)^(a2 - 1) * abs( hypergeo(A = a2 + b2, B = a1 + a2, C = a1 + a2 + b1 + b2, z = 1 - theta) )
      res <- A.const * (theta)^(a2 - 1) * hypergeo(A = a2 + b2, B = a1 + a2, C = a1 + a2 + b1 + b2, z = 1 - theta)
    }
    
    # theta > 1
    if(theta > 1) {
      #res <- A.const * (theta)^(-b2 - 1) * abs( hyp2f1(a = a2 + b2, b = b1 + b2, c = a1 + a2 + b1 + b2, z = 1 - (1 / theta )) )
      
      #res <- A.const * (theta)^(-b2 - 1) * abs( hypergeo(A = a2 + b2, B = b1 + b2, C = a1 + a2 + b1 + b2, z = 1 - (1 / theta )) )
      res <- A.const * (theta)^(-b2 - 1) * hypergeo(A = a2 + b2, B = b1 + b2, C = a1 + a2 + b1 + b2, z = 1 - (1 / theta ))
    } 
  }
  
  # return list only if loga=TRUE and numer=FALSE  
  if(!numer & loga) return(list(res=res, hypergeo.brob.res=hypergeo.brob.res)) else return(res)  
  #return(res)
}
################################


################################
### same function, but gives everything back
F2.brob <- function (a, b, b.prime, c, x, y, sL=0, sH=1, numer=TRUE, parallel=TRUE, debug=FALSE, ...) 
{
  
  #  if( !( abs(x) < 1 ) ) cat("not given: abs(x) < 1\tx = ",x,"\n")
  #  if( !( abs(y) < 1 ) ) cat("not given: abs(y) < 1\ty = ",y,"\n")
  #  if( !( c-a > 0 ) ) cat("not given: c-a > 0\tc-a = ",c-a,"\n")
  #  if( !( a > 0 ) ) cat("not given: a > 0\ta = ",a,"\n")
  if(debug)
  {
    cat("\n")
    cat("abs(x) = ",abs(x),"\trequired: abs(x) < 1 \t: ",abs(x) < 1,"\n")
    cat("abs(y) = ",abs(y),"\trequired: abs(y) < 1 \t: ",abs(y) < 1,"\n")
    cat("c-a    = ",c-a,"\t\trequired: c-a > 0 \t: ",c-a > 0,"\n")
    cat("a      = ",a,"\t\trequired: a > 0 \t: ",a > 0,"\n")
  }
  
  require(Brobdingnag)
  
  A1.simple.brob.2 <- function(u) {
    as.brob(u) ^ (a-1) *
      as.brob(1-u) ^ (c-a-1) *
      as.brob(1-u*x) ^ (-b) *
      as.brob(1-u*y) ^ (-b.prime)
  }
  
  res1 <- brob(lgamma(c) - lgamma(a) - lgamma(c-a))
  # gets back either numerical value or list with single values + sum of single values
  # of function
  if(parallel)
  {
    res2 <- sintegral.brob.parallel.2(A1.simple.brob.2, sL=sL, sH=sH, Nsteps=250, numer=numer)
  } else
  {
    res2 <- simpsonrule.brob.2(A1.simple.brob.2, sL=sL, sH=sH, Nsteps=250, numer=numer)
  }  
  
  if(numer) res <- res1*res2 else res <- list(F1=res1*res2[["sum"]],steps=res2)
  return(res)
}
################################

################################
#give everything back incl. NaN + Inf
simpsonrule.brob.2 <- function(fx, sL, sH, Nsteps=100, remINF=TRUE, numer=TRUE, pr.out=FALSE, BROB=TRUE, ...)
{
  # taken from sintegral from Bolstad2
  sek <- seq(sL,sH,length=Nsteps)
  l.intv <- 2*Nsteps+1
  intv.x <- approx(sek,sek,n=l.intv)$x
  h <- diff(intv.x)[1]
  inity <- as.list(sapply(seq_along(1:l.intv), function(x) fx(intv.x[x], ...)))
  inity1 <- inity[2 * (1:Nsteps) - 1]
  inity2 <- inity[2 * (1:Nsteps)]
  inity3 <- inity[2 * (1:Nsteps) + 1]
  # print out list of in-between results if required
  if(pr.out) print(inity)
  sum <- 0
  for(i in inity1) sum <- sum + unlist(i)
  for(i in inity2) sum <- sum + 4*unlist(i)
  for(i in inity3) sum <- sum + unlist(i)
  sum <- sum*(h/3)
  if(BROB)
  {
    # give back numerical summary or list of in-between-results
    inity.res <- unlist( lapply(inity, function(x) x@x) ) # on log.scale
    inity.TF <-  unlist( lapply(inity, function(x) x@positive) ) # associated positive
    seq.brob <- data.frame(intv.x=intv.x, inity=inity.res, inity.TF=inity.TF)
    if(numer) return(sum) else return(list(seq.brob=seq.brob, l.intv=l.intv, sum=sum))
  } else
  {
    return(sum)
  }  
  #print(inity.res)
  #print(inity.TF)
  #print(intv.x)
}
################################

#str(inity)
#inity[121]
#inity[252]
#inity.res <- unlist( lapply(inity, function(x) x@x) ) # on log.scale
#inity.TF <-  unlist( lapply(inity, function(x) x@positive) ) # associated positive
#seq.brob <- data.frame(intv.x=intv.x, inity=inity.res, inity.TF=inity.TF)
#inity.res
#inity.TF
#seq.brob

#A1.simple.brob.2(252)

###### function to calculate an integral
# after Bolstad2:::sintegral
#
sintegral.brob.parallel.2 <- function(fx, sL, sH, Nsteps=100, remINF=TRUE, numer=TRUE, pr.out=FALSE, BROB=TRUE, loga=FALSE, doBROBsumup=FALSE, ...)
{
  # parallel computing
  require(parallel)
  if(length(grep("windows", sessionInfo())) > 0) cores <- 1 else cores <- detectCores()
  # rewritten from sintegral from Bolstad2 package
  sek <- seq(sL,sH,length=Nsteps)
  l.intv <- 2*Nsteps+1
  intv.x <- approx(sek,sek,n=l.intv)$x
  h <- diff(intv.x)[1]
  inity <- mclapply(seq_along(1:l.intv), function(x) fx(intv.x[x]),
                    mc.silent = FALSE, mc.cores = cores)
  # print out list of in-between results if required
  if(pr.out) print(inity)
  if(loga & !BROB)
  {
    doBROBsumup <- TRUE
    inity <- lapply(inity, brob)
  }  
  if(BROB | (doBROBsumup & loga) )
  {
    summe <- (h/3)*( sum(list2vec.brob(inity[2 * (1:Nsteps) - 1])) +
                       sum(4*list2vec.brob(inity[2 * (1:Nsteps)])) +
                       sum(list2vec.brob(inity[2 * (1:Nsteps) + 1])) )
    # give back numerical summary or list of in-between-results
    inity.res <- unlist( lapply(inity, function(x) x@x) ) # on log.scale
    inity.TF <-  unlist( lapply(inity, function(x) x@positive) ) # associated positive
    #print(inity.res)
    #print(inity.TF)
    #print(intv.x)
    #print(summe)
    seq.brob <- data.frame(intv.x=intv.x, inity=inity.res, inity.TF=inity.TF)
    if(numer) return(summe) else return(list(seq.brob=seq.brob, l.intv=l.intv, sum=summe, Nsteps=Nsteps, inity=inity))
  } else
  {
    inity.num <- unlist(inity)
    summe <- (h/3)*( sum( inity.num[2 * (1:Nsteps) - 1])  +
                       sum( 4*(inity.num[2 * (1:Nsteps)])) +
                       sum( inity.num[2 * (1:Nsteps) + 1]) )
    if(numer) return(summe) else return(list(inity=inity.num, l.intv=l.intv, sum=summe))
  }  
}
# call:
# sek <- seq(0,pi, length=1000)
#sek <- seq(-3,3, length=1000)
#funx <- dnorm(sek) #fx(sek)
#as.brob(sintegral(sek, funx)$int)
#sintegral.brob.parallel.2(fx=dnorm, sL=-3, sH=3, Nsteps=500)
########################## END OF FUNCTION

###### function to calculate an integral
# after Bolstad2:::sintegral
#
sintegral.brob.parallel <- function(fx, sL, sH, Nsteps=100)
{
  # parallel computing
  require(parallel)
  if(length(grep("windows", sessionInfo())) > 0) cores <- 1 else cores <- detectCores()
  # rewritten from sintegral from Bolstad2 package
  sek <- seq(sL,sH,length=Nsteps)
  l.intv <- 2*Nsteps+1
  intv.x <- approx(sek,sek,n=l.intv)$x
  h <- diff(intv.x)[1]
  inity <- mclapply(seq_along(1:l.intv), function(x) fx(intv.x[x]), mc.silent = FALSE, mc.cores = cores)
  summe <- (h/3)*( sum(list2vec.brob(inity[2 * (1:Nsteps) - 1])) +
                     sum(4*list2vec.brob(inity[2 * (1:Nsteps)])) +
                     sum(list2vec.brob(inity[2 * (1:Nsteps) + 1])) )
  return(summe)
}
# call:
# sek <- seq(0,pi, length=1000)
# funx <- fx(sek)
#
# as.brob(sintegral(sek, funx)$int)
# sintegral.brob.parallel(fx=f, sL=0, sH=pi, Nsteps=500)
########################## END OF FUNCTION

################################
pdf.theta.diff.MULT <- function(theta, a1, b1, a2, b2, loga=FALSE, numer=TRUE, debug=FALSE, ...)
{
  # pdf of theta difference = theta_2 - theta_1 < crit
  # log and non-log
  # theta=sek
  
  # log version
  # require(appell)
  # require(tolerance)
  # theta out of boundaries -1 < theta < 1
  # if(theta < -1 | theta > 1) stop(paste("Failure!!! theta out of boundaries, should be: -1 < theta < 1, but theta = ", theta,sep=""))
  
  # give back everything via simpson rule only for loga=TRUE (and BROBs)
  if(loga) {
    # constant
    A.l.const <- lbeta(a1, b1) + lbeta(a2, b2)
    
    # theta = 0
    if(theta == 0) res <- log(1) - A.l.const + lbeta(a1 + a2 - 1, b1 + b2 - 1)
    
    # theta < 0
    if(theta < 0) {
      F2.brob.res <- F2.brob(a = b1, b = a1 + b1 + a2 + b2 - 2, b.prime = 1 - a1, c = a2 + b1, x = 1 + theta, y = 1 - theta^2, numer=numer, debug=debug, ...)
      if(numer) F2.res <- F2.brob.res@x else F2.res <- F2.brob.res[["F1"]]@x
      res <- log(1) - A.l.const + lbeta(a2, b1) + (b1 + b2 - 1)*log(-1 * theta) + (a2 + b1 - 1)*log(1 + theta) + F2.res
      
      #log( abs(appellf1(a = b1, b1 = a1 + b1 + a2 + b2 - 2, b2 = 1 - a1, c = a2 + b1, x = 1 + theta, y = 1 - theta^2)$val) )
      #F1.brob(a = b1, b = a1 + b1 + a2 + b2 - 2, b.prime = 1 - a1, c = a2 + b1, x = 1 + theta, y = 1 - theta^2, ...)@x
      #F1(a = 3, b = 4, b.prime = 5, c = 13, x = 0.2, y = 0.4)
    } 
    
    # theta > 0
    if(theta > 0) {
      F2.brob.res <- F2.brob(a = b2, b = a1 + b1 + a2 + b2 - 2 , b.prime = 1 - a2, c = a1 + b2, x = 1 - theta, y = 1 - theta^2, numer=numer, debug=debug, ...)
      if(numer) F2.res <- F2.brob.res@x else F2.res <- F2.brob.res[["F1"]]@x
      res <- log(1) - A.l.const + lbeta(a1, b2) + (b1 + b2 - 1)*log(theta) + (a1 + b2 - 1)*log(1 - theta) + F2.res
      
      #log( abs(appellf1(a = b2, b1 = a1 + b1 + a2 + b2 - 2 , b2 = 1 - a2, c = a1 + b2, x = 1 - theta, y = 1 - theta^2)$val) )      
      #F1.brob(a = b2, b = a1 + b1 + a2 + b2 - 2 , b.prime = 1 - a2, c = a1 + b2, x = 1 - theta, y = 1 - theta^2, ...)@x
    }
    attr(res,"scale") <- c("log")
    
  } else {
    
    # constant
    A.const <- beta(a1, b1) * beta(a2, b2)
    
    # theta = 0
    if(theta == 0) res <- 1 / A.const * beta(a1 + a2 - 1, b1 + b2 - 1)
    
    # theta < 0
    if(theta < 0) {
      res <- 1 / A.const * beta(a2, b1) * (-1 * theta)^(b1 + b2 - 1) * (1 + theta)^(a2 + b1 - 1) *
        #     abs(appellf1(a = b1, b1 = a1 + b1 + a2 + b2 - 2, b2 = 1 - a1, c = a2 + b1, x = 1 + theta, y = 1 - theta^2)$val)
        abs(F1(a = b1, b = a1 + b1 + a2 + b2 - 2, b.prime = 1 - a1, c = a2 + b1, x = 1 + theta, y = 1 - theta^2))
    }
    
    # theta > 0
    if(theta > 0) {
      res <- 1 / A.const * beta(a1, b2) * (theta)^(b1 + b2 - 1) * (1 - theta)^(a1 + b2 - 1) *
        #       abs(appellf1(a = b2, b1 = a1 + b1 + a2 + b2 - 2 , b2 = 1 - a2, c = a1 + b2, x = 1 - theta, y = 1 - theta^2)$val)
        abs(F1(a = b2, b = a1 + b1 + a2 + b2 - 2 , b.prime = 1 - a2, c = a1 + b2, x = 1 - theta, y = 1 - theta^2))
    }
  }
  # return list only if loga=TRUE and numer=FALSE  
  if(!numer & loga) return(list(res=res, F2.brob.res=F2.brob.res)) else return(res)
}
################################

### FUNCTION
#a1=1/3+7
#b1=1/3+12-7
#a2=1/3+6
#b2=1/3+18-6
#credMass=0.87
#xlim.diff=c(-.999,.999, -.999,.999)
#xlim.RR=c(0,100, 0,100)
#xlim.OR=c(0,100, 0,100)
#l.diff=1000
#l.RR=1000
#l.OR=1000
#lowerbound=0
#digs=3
#fac=1.25
#fac2=1.125
#theta.crit=c(0.5,1,1)
#loga=c(F,F,F)
#logaplot=c(F,F,F)


###### function to calculate exact binomial difference test
prop.theta.sek <- function(a1=1/3+7, b1=1/3+12-7, a2=1/3+6, b2=1/3+18-6,
                           credMass=0.87,
                           xlim.diff=c(-.999,.999, -.999,.999), l.diff=100,
                           xlim.RR=c(0,2, 0,2), l.RR=100,
                           xlim.OR=c(0,100, 0,3), l.OR=100,
                           lowerbound=0,
                           digs=3, fac=1.25, fac2=1.125,
                           theta.crit=c(0.5,1,1),
                           loga=c(T,F,T),
                           logaplot=c(F,F,F),
                           numer=TRUE,
                           parallel=TRUE,
                           debug=FALSE,
                           plot.out=TRUE,
                           BROB=c(T,F,T),
                           sL=0,
                           sH=1,
                           HDI=c(F,F,F)
)
{
  
  xlim <- cbind(rbind(xlim.diff,xlim.RR,xlim.OR),c(l.diff,l.RR,l.OR))
  colnames(xlim) <- c("pdf.lo","pdf.up","cdf.lo","cdf.up","length")
  
  pdf.diff <- cdf.diff <- pdf.RR <- cdf.RR <- pdf.OR <- cdf.OR <- NA
  
  cat("\nsek.pdf.DIFF\n")
  sek.pdf.diff <- seq(xlim["xlim.diff","pdf.lo"],xlim["xlim.diff","pdf.up"],length.out=xlim["xlim.diff","length"])
  for(i in 1:length(sek.pdf.diff))
  {
    print(i)
    test <- try( pdf.theta.diff.MULT(theta=sek.pdf.diff[i],
                                     a1=a1, b1=b1, a2=a2, b2=b2,
                                     loga = loga[1],
                                     numer = numer,
                                     parallel=parallel,
                                     debug=debug,
                                     BROB=BROB[1],
                                     sL=sL,
                                     sH=sH
    ))
    if(is.numeric(test)) pdf.diff[i] <- test else pdf.diff[i] <- NA
  }
  
  cat("\nsek.cdf.DIFF\n")
  sek.cdf.diff <- seq(xlim["xlim.diff","cdf.lo"],xlim["xlim.diff","cdf.up"],length.out=xlim["xlim.diff","length"])
  for(i in 1:length(sek.cdf.diff))
  {
    cdf.diff[i] <- cdf.theta.diff(theta=sek.cdf.diff[i], a1=a1, b1=b1, a2=a2, b2=b2)
  }
  
  cat("\nsek.pdf.RR\n")
  sek.pdf.RR <- seq(xlim["xlim.RR","pdf.lo"],xlim["xlim.RR","pdf.up"],length.out=xlim["xlim.RR","length"])
  for(i in 1:length(sek.pdf.RR))
  {
    pdf.RR[i] <- pdf.theta.ratio(theta=sek.pdf.RR[i], a1=a1, b1=b1, a2=a2, b2=b2, loga=loga[2])
  }
  
  cat("\nsek.cdf.RR\n")
  sek.cdf.RR <- seq(xlim["xlim.RR","cdf.lo"],xlim["xlim.RR","cdf.up"],length.out=xlim["xlim.RR","length"])
  for(i in 1:length(sek.cdf.RR))
  {
    cdf.RR[i] <- cdf.theta.ratio(theta=sek.cdf.RR[i], a1=a1, b1=b1, a2=a2, b2=b2)
  }
  
  #!!!!!!!!
  cat("\nsek.pdf.OR\n")
  sek.pdf.OR <- seq(xlim["xlim.OR","pdf.lo"],xlim["xlim.OR","pdf.up"],length.out=xlim["xlim.OR","length"])
  for(i in 1:length(sek.pdf.OR))
  {
    print(i)
    test <- try( pdf.theta.OR.brob(theta=sek.pdf.OR[i],
                                   a1=a1, b1=b1, a2=a2, b2=b2,
                                   loga=loga[3],
                                   numer=numer,
                                   parallel=parallel,
                                   BROB=BROB[3],
                                   sL=sL,
                                   sH=sH
    ))
    if(is.infinite(test)) cat("\nInf at i = ",i,"\tsek.pdf.OR[i] = ",sek.pdf.OR[i],"\n")
    if(is.na(test)) cat("\nNaN at i = ",i,"\tsek.pdf.OR[i] = ",sek.pdf.OR[i],"\n")
    if(is.numeric(test))
    {
      pdf.OR[i] <- test
    }  else
    {
      cat("\nNon-convergence at ni = ",i,"\tsek.pdf.OR[i] = ",sek.pdf.OR[i],"\n")
      pdf.OR[i] <- NA
    }  
  }
  #END OF !!!!!!!
  
  sek.cdf.OR <- seq(xlim["xlim.OR","cdf.lo"],xlim["xlim.OR","cdf.up"],length.out=xlim["xlim.OR","length"])
  for(i in 1:length(sek.cdf.OR))
  {
    cdf.OR[i] <- cdf.theta.OR(theta=sek.cdf.OR[i], a1=a1, b1=b1, a2=a2, b2=b2)
  }
  
  res <- list(differ=list(pdf=data.frame(sek.pdf.diff, pdf.diff),
                          cdf=data.frame(sek.cdf.diff, cdf.diff)
  ),
  RR=list(pdf=data.frame(sek.pdf.RR, pdf.RR),
          cdf=data.frame(sek.cdf.RR, cdf.RR)
  ),
  OR=list(pdf=data.frame(sek.pdf.OR, pdf.OR),
          cdf=data.frame(sek.cdf.OR, cdf.OR)
  ))
  
  # handle NaN / NA
  res.na.id <- lapply(res, function(i) lapply(i,function(j) which(is.na(j))))
  
  titles <- c("Difference of Proportion ", "Ratio of Proportion ", "Odds Ratio of Proportion ")
  comb <- c("-","/"," vs. ")
  xlabtext <- c("diff","ratio","odds ratio")
  meta <- list(titles=titles,
               comb=comb,
               xlabtext=xlabtext,
               xlim=xlim,
               HDI=HDI,
               theta.crit=theta.crit,
               loga=loga,
               logaplot=logaplot
  )          
  
  abs.post <- c(a1,b1,a2,b2)
  names(abs.post) <- c("a1","b1","a2","b2")
  res <- list(post=res, meta=meta, abs.post=abs.post)
  if(plot.out) prop.theta.sek.plot(res)	
  
  return(res)
}
########################## END OF FUNCTION



### FUNCTION
plot.prop.b <- function(base, meta=meta, i=NA, credMass=0.95, allowSplit=FALSE, logaplot=c(F,F,F), ...)
{
 require(HDInterval)

 # check for NAs
 naid <- lapply(base, function(i) which(is.na(i), arr.ind=TRUE))
 print(naid)
 # namen <- names(naid)
 namen <- names(base)
 for(j in namen)
 {
  naid.local <- which(is.na(base[[j]]), arr.ind=TRUE)
  base[[j]][naid.local] <- 0 # set NAs to zero
 }
 dxpdf <- base$pdf[,1] # sek
 dypdf <- base$pdf[,2] # pdf
 dxcdf <- base$cdf[,1] # sek
 dycdf <- base$cdf[,2] # cdf
 

 ### SUB-FUNCTION
 add.plot <- function(dx, dy, xlim=NA, xlabtext=meta$xlabtext[i], comb=meta$comb[i], titles=meta$titles[i], ylabtext=ylabtext)
 {					   
  if(max(dx) < xlim[2]) xlim[2] <- ceiling(max(dx))
  # check for INF
  isINF <- which(is.infinite(dy))
  if(length(isINF) != 0)
  {
   print("remove infinite values for plotting")
   dy <- dy[-isINF]
   dx <- dx[-isINF]
  }
  if(length(uniq <- unique(round(dy,3))) == 1)
  {
   ylim <- c(0,uniq)
   dy <- rep(uniq, length(dy))
  } else
  {
   ylim <- c(min(dy), max(dy))
  }
  ylim[2] <- ylim[2] + diff(ylim)*0.2
  plot(dx, dy, type="l", col="darkred", bty="n", pre.plot=grid(),
       xlim=xlim, ylim=ylim, xlab="", ylab="", main="", axes=F)
  mtext(eval(substitute(expression(paste(theta[xlabtext],sep="")),
                        list(xlabtext=xlabtext)
                       )), 1, line=3, cex=1.2, col="darkred")
  mtext(eval(substitute(expression(paste(ylabtext,sep="")),
                        list(ylabtext=ylabtext)
                        )), 2, line=2.5, cex=1.2, col="darkred")
  mtext(eval(substitute(expression(paste(titles,theta[2],comb,theta[1],sep="")),
                        list(titles=titles, comb=comb)
                       )), 3, line=1.8, cex=1.2, col="black")
  axis(2)
  axis(1)	  
 }
 ### END OF SUB-FUNCTION
 
 
 ### SUB-FUNCTION
 add.HDI.bprop <- function(dx, dy, allowSplit=FALSE, credMass=0.95, digs=3)
 {
   dens <- list(x=dx, y=dy, bw=0, n=0, has.na=FALSE)
   attr(dens, "class") <- "density"
   hdis <- hdi(dens, credMass=credMass, allowSplit=allowSplit)
   lines(hdis,rep(attr(hdis,"height"),2),col="violetred3",lwd=4)
   text(hdis, rep(attr(hdis,"height")), pos=c(2,4), labels=round(hdis,digs))
   mtext(eval(substitute(expression(paste("HDI credMass = ",credMass,sep="")),
                         list(credMass=credMass)
                        )), 3, line=0.4, cex=0.8, col="darkred") 
   print(hdis)						
 }
 ### END OF SUB-FUNCTION
 
 ### SUB-FUNCTION
 add.MAP <- function(dx,dy, digs=3, pos=3) 
 {
  MAP <- max(dy)
  MAP.x <- dx[dy == MAP]
  text(MAP.x, MAP, pos=pos, labels=paste("MAP = ",signif(MAP,digs)," at ",signif(MAP.x,digs),sep=""))
 }
 ### END OF SUB-FUNCTION
 
 
 ### SUB-FUNCTION
 add.theta.crit <- function(dx, dy, thetaC, xlim, ...)
 {
  print(thetaC)
  belo.tC <- which(dx <= thetaC)
  if(length(belo.tC) == 0)
  {
   print("dx <= theta crit. not possible - check dx area!")
  } else thetaC.id <- max(belo.tC)
  dx.thetaC <- dx[thetaC.id]
  # dypdf.thetaC <- dypdf[thetaC.id]
  dycdf.thetaC <- dycdf[thetaC.id]
  lines(rep(dx.thetaC,2),c(0,dycdf.thetaC), col="steelblue", lwd=2, lty=2)
  lines(c(xlim[1],dx.thetaC),rep(dycdf.thetaC,2), col="steelblue", lwd=2, lty=2)#min(dx)
  text(dx.thetaC,dycdf.thetaC,#*fac2,
       eval(substitute(expression(paste(theta[crit]," = ",theta.crit, sep="")),
                       list(theta.crit=thetaC)
				   )), pos=3, ...)
 }
 ### END OF SUB-FUNCTION
 
 
 # plot pdf
 if(meta$loga[i] == TRUE)
 {
  ylabtext <- "log(PDF)"
 } else if(meta$logaplot[i] == TRUE)
 {
  dypdf <- log(dypdf)
  ylabtext <- "log(PDF)"
 } else ylabtext <- "PDF"
 add.plot(dx=dxpdf, dy=dypdf, ylabtext=ylabtext, xlim=meta$xlim[i,1:2])
 if(i == 1) pos <- 3 else pos <- 4
 add.MAP(dx=dxpdf, dy=dypdf, pos=pos)
 # print(credMass)
 if(meta$HDI[i] == TRUE) add.HDI.bprop(dx=dxpdf, dy=dypdf, credMass=credMass, allowSplit=allowSplit)

 # plot cdf
 add.plot(dx=dxcdf, dy=dycdf, ylabtext="CDF", xlim=meta$xlim[i,3:4])
 add.theta.crit(dx=dxcdf, dy=dycdf, thetaC=meta$theta.crit[i], xlim=meta$xlim[i,3:4], cex=1.5)

}
# call:
# loop over diff, RR, OR
# plot.prop.b(base=theta.res$post[[i]], meta=meta, i=i)
### END OF FUNCTION


### FUNCTION
prop.theta.sek.plot <- function(theta.res)
{
 meta <- theta.res$meta
 par(oma=c(2,1,5,1), "cex.axis"=1, bty="l", mfrow=c(3,2))
 for(i in 1:3)
 {
  plot.prop.b(base=theta.res$post[[i]], meta=meta, i=i)
 }
 mtext("Exact Bayesian Proportion Test", outer=TRUE, line=1.5, cex=1.5, side=3)
}
# call:
# prop.theta.sek.plot(theta.res)
### END OF FUNCTION


###### function to plot exact binomial difference test and calculate some statistics
plot.bayes.prop.test.Xct <- function(res.Xct, a1=102, b1=108, a2=132, b2=121,
                                     digs=2, probs=NULL, thetaCs=c(0,1,1), fac=1.15,
                                     cols=c("violetred3","steelblue","#f8e0a0"),
                                     n.mcmc=1e+6,
                                     na.rm=TRUE, inf.rm=TRUE,
                                     ylim=NA,
                                     loga=FALSE, drawmcmc=TRUE,
                                     seed=12234
)
{ 
  
  sN <- c(rev.ab.lik(a1,b1), rev.ab.lik(a2,b2))
  names(sN) <- c("si","Ni","sii","Nii")
  si <- sN["si"]
  Ni <- sN["Ni"]
  sii <- sN["sii"]
  Nii <- sN["Nii"]
  
  thetaC <- thetaCs[1]
  thetaC.RR <- thetaCs[2]
  thetaC.OR <- thetaCs[3]
  
  # remove inf values for plotting
  if(inf.rm)
  {
    infid.rows <- which(is.infinite(res.Xct[,2]))
    if(length(infid.rows) > 0)
    {
      cat("\nRemoving INF values for plot\n")
      print( res.Xct[infid.rows,] )
      res.Xct <- res.Xct[-infid.rows,]
    }  
  }  
  # remove NAs 
  if(na.rm)
  {
    naid.rows <- which(is.na(res.Xct[,2]))
    if(length(naid.rows) > 0)
    {
      cat("\nRemoving NA values for plot\n")
      print( res.Xct[-naid.rows,] )
      res.Xct <- res.Xct[-naid.rows,]
    }  
  }
  # res.Xct
  dd <- dim(res.Xct)
  
  # calculate maximum of posterior density and index
  MAP <- max(res.Xct[,2])
  MAP.id <- which(res.Xct[,2] == MAP)
  MAP.x <- res.Xct[MAP.id,1]
  
  
  ### NOT RUN
  # spanish paper 
  # prob theta.diff = theta_1 - theta_2
  # MAP <- sum(res[,"sek"]*res[,"theta.diff"]*steps)
  ### END OF NOT RUN#
  
  # steps <- 1/((dd[1]-1)/2)
  steps <- 1/((dd[1])/2)
  # prob theta.diff (theta1 - theta2) > crit
  above.id <- which(res.Xct[,1] > thetaC)
  if(length(above.id) == 0)
  {
    p.theta.diff.above.crit <- 0
  } else p.theta.diff.above.crit <- sum(res.Xct[above.id[1]:dd[1],2]*steps)
  # prob theta.diff (theta2 - theta1) < crit
  below.id <- which(res.Xct[,1] < thetaC)
  if(length(below.id) == 0)
  {
    p.theta.diff.below.crit <- 0
  } else p.theta.diff.below.crit <- sum(res.Xct[below.id,2]*steps)
  
  sum(res.Xct[which(res.Xct[,1] >= thetaC),2]*steps) + sum(res.Xct[which(res.Xct[,1] < thetaC),2]*steps)
  1- sum(res.Xct[which(res.Xct[,1] <= thetaC),2]*steps)
  sum(res.Xct[which(res.Xct[,1] < thetaC),2]*steps)
  
  # brute force
  set.seed(seed)
  mcmc1 <- rbeta(n.mcmc, a1, b1)
  mcmc2 <- rbeta(n.mcmc, a2, b2)
  mcmc.diff <- mcmc2-mcmc1
  mcmc.den <- density(mcmc2 - mcmc1)
  MAP.den <- max(mcmc.den$y)
  mean(mcmc2-mcmc1 < thetaC)
  mean(mcmc2-mcmc1 > thetaC) 
  mean(mcmc2-mcmc1 == thetaC)
  
  # exact hypothesis: p < theta crit
  p.theta.diff.below.crit <- cdf.theta.diff(theta=thetaC, a1, b1, a2, b2)
  p.theta.RR.below.crit <- cdf.theta.ratio(theta=thetaC.RR, a1, b1, a2, b2)
  p.theta.OR.below.crit <- cdf.theta.OR(theta=thetaC.OR, a1, b1, a2, b2)
  cat("\n##################################\n\n# Exact Bayesian Test of Proportions\n")
  
  cat("\n### MAP Difference in Proportions:\n\nPr( [p2-p1] == max) Closest match\n")
  MAP.Xct <- as.vector(res.Xct[res.Xct[,2] == max(res.Xct[,2]),])
  class(MAP.Xct) <- "numeric"
  cat("Exact:\t\tMAP =",round(MAP.Xct[2],digs+2),"at theta difference =",round(MAP.Xct[1],digs+2),"\n")
  cat("Brute Force:\tMAP =",round(MAP.den,digs+2),"at theta difference =",round(mean(mcmc.diff),digs+2),"\n")
  
  cat("\n### Difference in Proportions:\n\nExact:\n[CDF] Pr( [p2-p1] < ",thetaC," ) = ", round(p.theta.diff.below.crit,digs),"\n",sep="")
  cat("[CDF] Inverse Pr( [p2-p1] > ",thetaC," ) = ", round(1-p.theta.diff.below.crit,digs),"\n",sep="")
  cat("[Ratio] ([p2-p1] < ",thetaC," ) / Pr( [p2-p1] > ",thetaC,") = ", round(1/p.theta.diff.below.crit,digs),"\n",sep="")
  
  cat("\nBrute Force:\nPr( [p2-p1] < ",thetaC,") = ",round(mean(mcmc.diff < thetaC),digs),"\n")
  cat("Pr( [p2-p1] > ",thetaC,") = ",round(mean(mcmc.diff > thetaC),digs),"\n")
  
  cat("\n### Ratio of Proportions:\n\n[CDF] Pr( [p2/p1] < ",thetaC.RR," ) = ", round(p.theta.RR.below.crit,digs),"\n",sep="")
  cat("[CDF] Inverse Pr( [p2/p1] > ",thetaC.RR," ) = ", round(1-p.theta.RR.below.crit,digs),"\n",sep="")
  cat("[Ratio] ([p2/p1] < ",thetaC.RR," ) / Inverse Pr( [p2/p1] > ",thetaC.RR,") = ", round(p.theta.RR.below.crit/(1-p.theta.RR.below.crit),digs),"\n",sep="")
  
  cat("\n### Odds Ratio of Proportions:\n\n[CDF] Pr( [p2/(1-p2)]/[p1/(1-p1)] < ",thetaC.OR," ) = ", round(p.theta.OR.below.crit,digs),"\n",sep="")
  cat("[CDF] Inverse Pr( [p2/(1-p2)]/[p1/(1-p1)] > ",thetaC.OR," ) = ", round(1-p.theta.OR.below.crit,digs),"\n",sep="")
  cat("[Ratio] ([p2/(1-p2)]/[p1/(1-p1)] < ",thetaC.OR," / Inverse Pr( [p2/(1-p2)]/[p1/(1-p1)] > ",thetaC.OR,") = ", round(p.theta.OR.below.crit/(1-p.theta.OR.below.crit),digs),"\n",sep="")
  
  if(is.na(ylim)) ylim <- c(min(res.Xct[,2]), MAP * fac)
  xlim <- range(res.Xct[,1])
  
  par(mar=c(5,6,5,5))
  par(oma=c(2,1,1,1))
  par("cex.axis"=0.8)
  # par(mfrow=c(2,1))
  
  if(loga == TRUE) addontext <- c("log(",")") else addontext <- c("","")
  plot(c(0,0), ylim=ylim, xlim=xlim,
       pre.plot=grid(),
       main="",
       xlab="",
       ylab=eval(substitute(expression(paste(textadd1,"p(",theta[2]," - ",theta[1],")",textadd2,sep="")), list(textadd1=addontext[1],textadd2=addontext[2]))),
       type="l", lty="solid", lwd=1.8, col="white", bty="n", axes=FALSE)
  
  mtext(expression(paste("Bayesian Analysis of Difference of Proportions ",theta[2]," - ",theta[1],sep="")), 3, line=2, cex=1.4)
  mtext(eval(substitute(expression(paste("s"["1"],"/N"["1"]," vs. s"["2"],"/N"["2"]," | ",si,"/",Ni," vs. ",sii,"/",Nii)),
                        list(si=si, Ni=Ni, sii=sii, Nii=Nii))),
        3, line=0.7, col="black")
  mtext(expression(paste(theta[2]," - ",theta[1], " < ",theta[crit])), 1, line=3, cex=1.3, col=cols[1])
  
  mtext(eval(substitute(expression(paste("p(",theta[2]," - ",theta[1],") < ",thetaC," (",theta[crit],") = ",p.theta.diff.below.crit,"%")),
                        list(MAP=round(MAP,digs), MAP.x=round(MAP.x,digs),
                             thetaC=round(thetaC,digs),
                             p.theta.diff.below.crit=round(p.theta.diff.below.crit*100,digs))
  )), 4, line=1.9, cex=0.8)					   
  mtext(eval(substitute(expression(paste("p"["MAP"],"(",theta[diff],") = ",MAP," at ",theta," = ",MAP.x,)),
                        list(MAP=round(MAP,digs), MAP.x=round(MAP.x,digs),
                             thetaC=round(thetaC,digs),
                             p.theta.diff.below.crit=round(p.theta.diff.below.crit*100,digs))
  )), 4, line=.6, cex=0.8)
  axis(1)
  axis(2)
  
  # adjust for approx. matching... and print that...
  #
  # above thetaC
  start.id <- which(res.Xct[,1] >= thetaC)[1]
  cat("\n### Closest match of p(p2-p1 > crit) with crit = ",thetaC,"\n\n",sep="")
  print(t(as.data.frame(res.Xct[start.id,])))
  cat("\n")
  x <- c(res.Xct[start.id,1], res.Xct[start.id:dd[1],1], res.Xct[dd[1],1])
  y <- c(0,res.Xct[start.id:dd[1],2],0)
  # polygon(x, y, col=cols[3], border=NA)
  
  # below thetaC
  end.id <- which(res.Xct[,1] < thetaC)
  cat("\n### Closest match of p(p2-p1 < crit) with crit = ",thetaC,"\n\n",sep="")
  print(t(as.data.frame(res.Xct[length(end.id),])))
  cat("\n")
  x <- c(res.Xct[1,1],res.Xct[end.id,1], res.Xct[max(end.id),1])
  y <- c(res.Xct[1,2],res.Xct[end.id,2], res.Xct[1,2])
  polygon(x, y, col=cols[3], border=NA)
  
  # full area below curve
  # x <- c(res.Xct[start.id,1], res.Xct[start.id:dd[1],1], res.Xct[dd[1],1])
  # y <- c(0,res.Xct[start.id:dd[1],2],0)
  # polygon(x, y, col=cols[3], border=NA)
  
  # plot exact curve
  # pdf not cdf!!!
  lines(res.Xct[,1], res.Xct[,2], col=cols[1], lwd=2)
  
  # plot mcmc simulation (Gibbs sampler)
  if(drawmcmc == TRUE)
  {
    if(loga == TRUE) mcmc.den$y <- log(mcmc.den$y)
    lines(mcmc.den, col=cols[2], lwd=3, lty="dashed")
  }
  
  # new plot - new legend... below the first one

  par(fig=c(0,1,0,1), oma=c(0,0,0,0), mar=c(0,0,0,0), new=TRUE)
  plot(0, 0, type="n", bty="n", xaxt="n", yaxt="n")			  
  if(drawmcmc)
  {
    legend("bottom",legend=c(expression(paste("",theta[2]," - ",theta[1],sep="")),
                             paste("MCMC",sep=""),
                             eval(substitute(expression(paste("p(",theta["diff"],") < ",thetaC,sep="")),list(thetaC=round(thetaC,digs+1))))
    ),
    xpd=TRUE, horiz=TRUE, inset=c(0,0), y.intersp=2.4,
    bty="n", cex=0.9, fill=cols, border=NA) #col=cols, lty=c(1,2,1), lwd=2, 
  } else
    legend("bottom",legend=c(expression(paste("",theta[2]," - ",theta[1],sep="")),
                             eval(substitute(expression(paste("p(",theta["diff"],") < ",thetaC,sep="")),list(thetaC=round(thetaC,digs+1))))
    ),
    xpd=TRUE, horiz=TRUE, inset=c(0,0), y.intersp=2.4,
    bty="n", cex=0.9, fill=cols[c(1,3)], border=NA) #col=cols, lty=c(1,2,1), lwd=2,  
}
########################## END OF FUNCTION



### brute force again - comparison plot of difference, RR, and OR

### FUNCTION
bayes.prop.BForce <- function(a1=102,b1=108,a2=132,b2=121, diffcompare="smaller", thetaCs=c(0.5,1.5,1.5), DBS=seq(0,1,0.01), n.mcmc=1e6, seed=29567, PLOT=TRUE)
{
 set.seed(seed)

 # criteria for difference, ratio, and odds ratio
 names(thetaCs) <- c("diff","RR","OR")

 # MCMC chains
 RB1 <- rbeta(n.mcmc, shape1=a1, shape2=b1)
 RB2 <- rbeta(n.mcmc, shape1=a2, shape2=b2)

 # densities
 DBS
 DB1 <- dbeta(DBS, shape1=a1, shape2=b1)
 DB2 <- dbeta(DBS, shape1=a2, shape2=b2)
 
if(diffcompare == "smaller")
{
 # difference
 p.diff.theta <- mean(RB2-RB1 < thetaCs[1])

 # ratio
 ratio.theta <- mean(RB2/RB1 < thetaCs[2])

 # odds ratio
 OR.theta <- mean( (RB2/(1-RB2)) / (RB1/(1-RB1)) < thetaCs[3])

 #
 critcompare <- c(-rev(DBS),DBS)
 critp <- data.frame(critcompare,p=sapply(critcompare, function(i) mean(RB2-RB1 < i)))
 head(critp)
 tail(critp)
 temp.p <- unique(critp[,2])
 # only values above zero and below one
 critp[which(critp[,2] %in% temp.p[temp.p > 0 & temp.p < 1]),]

 #
 CRIT <- sapply(critcompare, function(i) mean(RB2-RB1 < i))
 str(CRIT)
 summary(CRIT)
 
} else if(diffcompare == "bigger")
{
 # difference
 p.diff.theta <- mean(RB2-RB1 > thetaCs[1])

 # ratio
 ratio.theta <- mean(RB2/RB1 > thetaCs[2])

 # odds ratio
 OR.theta <- mean( (RB2/(1-RB2)) / (RB1/(1-RB1)) > thetaCs[3])

 #
 critcompare <- c(-rev(DBS),DBS)
 critp <- data.frame(critcompare,p=sapply(critcompare, function(i) mean(RB2-RB1 > i)))
 head(critp)
 tail(critp)
 temp.p <- unique(critp[,2])
 # only values above zero and below one
 critp[which(critp[,2] %in% temp.p[temp.p > 0 & temp.p < 1]),]

 #
 CRIT <- sapply(critcompare, function(i) mean(RB2-RB1 > i))
 str(CRIT)
 summary(CRIT)
 
} else stop("either smaller or bigger, nothing else...")

 if(PLOT) BProptest.bf.plot(DBS, DB1, DB2, critcompare, critp, CRIT, diffcompare, thetaCs)

 res <- list(comparison=diffcompare, crits=thetaCs, prob.diff=p.diff.theta, RR=ratio.theta, OR=OR.theta, critp=critp)
return(res)
}
# call:
# bayes.prop.BForce(a1=102,b1=108,a2=132,b2=121, diffcompare="smaller", thetaCs=c(0.5,1.5,1.5), DBS=seq(0,1,0.01), n.mcmc=1e6, seed=29567, PLOT=TRUE)
# bayes.prop.BForce(diffcompare="smaller")
# bayes.prop.BForce(diffcompare="bigger")
### END OF FUNCTION


### FUNCTION
BProptest.bf.plot <- function(DBS, DB1, DB2, critcompare, critp, CRIT, diffcompare, thetaCs)
{
 # plot all combinations
 par(oma=c(2,1,4,1), "cex.axis"=1, bty="l", mfrow=c(3,3))
 plot(DBS, DB1, col="darkred", bty="n", pre.plot=grid(), type="l", main="theta_1", xlab=expression(theta[1]), ylab="Density")
 plot(DBS, DB2, col="steelblue", bty="n", pre.plot=grid(), type="l", main="theta_2", xlab=expression(theta[2]), ylab="Density")
 ylim <- range(c(DB1,DB2))
 plot(DBS, DB1, ylim=ylim, col="steelblue", bty="n", pre.plot=grid(), type="l", main="theta_1 (blue) & theta_2 (red)", xlab=expression(paste(theta[1]," & ",theta[2],sep="")), ylab="Density")
 lines(DBS, DB2, col="darkred")
 plot(DBS, abs(DB2-DB1), col="darkred", bty="n", pre.plot=grid(), type="l", main="abs(DIFF)", xlab=expression(paste("abs (",theta[2]," - ",theta[1],")",sep="")), ylab="Density")
 plot(DBS, DB2-DB1, col="darkred", bty="n", pre.plot=grid(), type="l", main="Dfference [theta_2 - theta_1]", xlab=expression(paste(theta[2]," - ",theta[1],sep="")), ylab="Density")
 
 if(diffcompare == "smaller")
 {
  STRING <- " < "
 } else STRING <- " > "
 plot(critcompare, CRIT, col="darkred", bty="n", pre.plot=grid(), type="l", main=paste("[theta_2 - theta_ 1] ",STRING,thetaCs[1],sep=""), xlab=eval(substitute(expression(paste(theta[2]," - ",theta[1],STRING,crit,sep="")),list(STRING=STRING,crit=thetaCs[1]))), ylab="probability")
  
 
 plot(DBS, DB2/DB1, col="darkred", bty="n", pre.plot=grid(), type="l", main="Ratio [theta_2 / theta_1]", xlab=expression(paste(theta[2]," / ",theta[1],sep="")), ylab="Density")
 plot(DBS, (DB2/(1-DB1)) / (DB1/(1-DB2)), col="darkred", bty="n", pre.plot=grid(), type="l", main="Odds Ratio [theta_2 vs. theta_1]", xlab=expression(paste("OR [",theta[2],", ",theta[1],"]",sep="")), ylab="Density")
 mtext("Bayesian proportion test (brute force)", outer=TRUE, line=0.5, cex=1.5, side=3)
}
# call:
# BProptest.bf.plot(DBS, DB1, DB2, critcompare, critp, CRIT, diffcompare)
### END OF UNCTION

