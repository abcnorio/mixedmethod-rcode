# 2020-10-16
# file:
# ptall_generalfuncs_Bayes_binomial.r

# location:
# none
# general functions for various scripts
# especially binomial Bayesian functions


###### function to calculate Bayes-Laplace prior probability distribution
# Studer (1996, p.30, equation 5.16)
# defined for 0 < theta < 1
pbl <- function(theta, si, Ni, loga=FALSE, reexp=FALSE)
{
 if(loga) {
  res <- lfactorial(Ni+1) - (lfactorial(si) + lfactorial(Ni-si)) + log(theta^si) + log((1-theta)^(Ni-si))
 } else {
  res <- factorial(Ni+1) / (factorial(si) * factorial(Ni-si)) * theta^si * (1-theta)^(Ni-si)
 }
 if(loga && reexp) res <- exp(res)
return(res) 
}
# call:
# pbl(theta=0.5, si=13, Ni=16, loga=FALSE)
# exp( pbl(theta=0.5, si=13, Ni=16, loga=TRUE) )
# pbl(theta=0.5, si=13, Ni=16, loga=TRUE, reexp=TRUE)
########################## END OF FUNCTION


###### function to calculate Jeffreys-Carnap prior probability distribution
# Studer (1996, p.30, equation 5.17)
# defined for 0 < theta < 1
pjc <- function(theta, si, Ni, loga=FALSE, reexp=FALSE)
{
 if(loga) {
  res <- lfactorial(Ni-1) - (lfactorial(si-1) + lfactorial(Ni-si-1)) + (si-1)*log(theta) + (Ni-si-1)*log(1-theta)
 } else {
  res <- factorial(Ni-1) / (factorial(si-1) * factorial(Ni-si-1)) * theta^(si-1) * (1-theta)^(Ni-si-1)
 }
 if(loga && reexp) res <- exp(res)
return(res)
}  
# call:
# pjc(theta=0.5, si=13, Ni=16, loga=FALSE)
# exp( pjc(theta=0.5, si=13, Ni=16, loga=TRUE) )
# pjc(theta=0.5, si=13, Ni=16, loga=TRUE, reexp=TRUE)
########################## END OF FUNCTION


###### function to plot UMS paper 1996
# plotting Jeffreys-Carnap + Bayes-Laplace density distribution
plot.bl.jc <- function(theta, sN.ME.res, si, Ni, dig=2, filling=FALSE, cols=NULL, sele=c(0,1))
{ 
  stopifnot(length(theta) == dim(sN.ME.res)[1])
  
  IDs.select <- which(theta >= sele[1] & theta <= sele[2])
  theta <- theta[IDs.select]
  sN.ME.res <- sN.ME.res[IDs.select,]
  
  # replace NaN by zeros
  sN.ME.res[which(apply(sN.ME.res, 2, is.nan), arr.ind=TRUE)] <- 0
  
  ylim.max <- max(sN.ME.res, na.rm=TRUE)
  if(is.null(cols)) cols <- c("steelblue","green","yellow","lightgreen")
  
  par(mar=c(5,6,5,5), oma=c(2,1,1,1))
  plot(0,0, main="", xlab="",
       ylab=expression(paste("p(H[",theta,"] | s/N, I)",sep="")),
       type="l", lty="solid", lwd=2, col="white", xlim=c(sele[1],sele[2]), ylim=c(0,ylim.max), bty="l")
  
  if(filling)
  {
    col.v <- col2rgb(cols)/255
    polygon(c(0,theta,max(theta),0), c(0,sN.ME.res[,"pjc.res"],0,0), col=cols[3], border=NA)
    polygon(c(0,theta,max(theta),0), c(0,sN.ME.res[,"pbl.res"],0,0), col=rgb(col.v[1,4], col.v[2,4], col.v[3,4], alpha=0.3), border=NA)
  }
  points(theta, sN.ME.res[,"pjc.res"], type="l", lty="solid", lwd=2, col=cols[1])
  points(theta, sN.ME.res[,"pbl.res"], type="l", lty="solid", lwd=2, col=cols[2])
  mtext(expression(paste("Posterior success probabilities",sep="")), 3, line=2, cex=1.5)
  mtext(eval(substitute(expression(paste("success rate s/N = ",si,"/",Ni)),list(si=si, Ni=Ni))), 3, line=0.7, col="black")
  mtext(expression(paste(theta)), 1, line=3, cex=1.5)
  mean.BL <- round(((si + 1) / (Ni + 2))*100,dig)
  mean.JC <- round(si/Ni*100,dig)
  mtext(eval(substitute(expression(paste("BL (mean) = ",mean.BL,"%  |  JC (mean) = ",mean.JC,"%")),
                        list(mean.BL=mean.BL, mean.JC=mean.JC))), 4, line=1, cex=0.8)
  
  #new plot - new legend... below the first one
  #http://dr-k-lo.blogspot.de/2014/03/the-simplest-way-to-plot-legend-outside.html  
  par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
  plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
  legend("bottom",legend=c("Bayes-Laplace","Jeffreys-Carnap"),
         xpd=TRUE, horiz=TRUE, inset=c(0,0), y.intersp=2.4,
         col=cols[c(1,2)], lty=c(1,1), lwd=2, bty="n", cex=0.9)
  
} 
# call:
# plot.bl.jc(theta, sN.ME.res, si=si, Ni=Ni, filling=FALSE)
# plot.bl.jc(theta, sN.ME.res, si=si, Ni=Ni, filling=TRUE)
########################## END OF FUNCTION


###### function to calculate summary statistics
# Bayes-Laplace posterior
# Jeffreys-Carnap posterior
sN.post.su <- function(Ni, si, probs=c(0.69,0.95,0.99), rn=NA, steps=1000, loga=TRUE, dig=2, printout=TRUE)
{
# UMS 1996 (5.20)
# special case "no success"
# si = 0
# mean.BL = 1 / (Ni + 2)
# sigma.BL = 1 / (Ni + 2) * sqrt( (Ni + 1) / (Ni + 3) )

# interval sigma = 1 = 69%, sigma = 2 = ~96%
# mean.BL - sqrt(vari.BL), mean.BL + sqrt(vari.BL)
# mean.JC - sqrt(vari.JC), mean.JC + sqrt(vari.JC)
# UMS 1996
# (5.18)
 mean.BL <- function(Ni, si) (si + 1) / (Ni + 2)
 mean.JC <- function(Ni, si) si/Ni
# (5.19)
 vari.BL <- function(Ni, si) ( (si + 1) / (Ni + 2) ) * (1 - ( (si + 1) / (Ni + 2) ) ) / (Ni + 3)
 vari.JC <- function(Ni, si) ( si/Ni ) * (1 - ( si/Ni ) ) / (Ni + 1)

 sN.ME.post.mean <- c(mean.BL(Ni=Ni, si=si), mean.JC(Ni=Ni, si=si))
 names(sN.ME.post.mean) <- c("BL", "JC")
 sN.ME.post.mean
 sN.ME.post.vari <- c(vari.BL(Ni=Ni, si=si), vari.JC(Ni=Ni, si=si))
 sN.ME.post.vari
 sqrt(sN.ME.post.vari)

# maximum of pdf = mode
# theta.max.pbl.res <- optimize(pbl, si=si, Ni=Ni, lower=0, upper=1, maximum=TRUE, loga=FALSE, reexp=FALSE)$maximum
# theta.max.pjc.res <- optimize(pjc, si=si, Ni=Ni, lower=0, upper=1, maximum=TRUE, loga=FALSE, reexp=FALSE)$maximum
# theta.max.pbl.res
# theta.max.pjc.res

 theta <- seq(0,1,length.out=steps)
 if(loga) reexp <- TRUE else reexp <- FALSE
 pbl.res <- pbl(theta=theta, si=si, Ni=Ni, loga=loga, reexp=reexp) #log
 pjc.res <- pjc(theta=theta, si=si, Ni=Ni, loga=loga, reexp=reexp) #log
 if(any(is.nan(pjc.res)))
 {
  nan.id <- which(is.nan(pjc.res))
  cat(paste("Werte = NaN bei theta = ",theta[nan.id],"\n\n",sep=""))
  theta.max.pjc.res <- theta[which(pjc.res[-nan.id] == max(pjc.res[-nan.id]))] 
 } else {
  theta.max.pjc.res <- theta[which(pjc.res == max(pjc.res))]
 }
 theta.max.pbl.res <- theta[which(pbl.res == max(pbl.res))]
 
 if(length(theta.max.pjc.res) > 1)
 {
  cat(paste("Several or no maximum (MAP/ mode) for Jeffreys-Carnap.\nPlot the function with these parameters to understand 'why'.\n\n",sep=""))
  theta.max.pjc.res <- NA
 } 

# calculation credible intervals based on HPD intervals
# in case of skewed distributions do not use mean +/n p*sd
 require(HDInterval)
 if(si!=0)
 {
  hdi.pbl <-  sapply(probs, function(z) 
					{
					pbl.res.list <- list(x=theta, y=pbl.res)
					attr(pbl.res.list,"class") <- "density"
					hdi(pbl.res.list,credMass=z)
					})
 } else {
  cat(paste("s = ",si," of N = ",Ni," >> i.e. no successes >>\nHPD interval does not make sense for Bayes-Laplace and Jeffreys-Carnap method (success rates).\nPlot the function with these parameters to understand 'why'.\n\n",sep=""))
  hdi.pbl <- matrix(data=NA, nrow=2, ncol=3)#nrow=5
 }
  colnames(hdi.pbl) <- paste(probs,"%",sep="")
  rownames(hdi.pbl) <- c("lower","upper")
  hdi.pbl <- cbind(rn,hdi.pbl)

  if(si >= 2 & Ni >= 4) # everything below 2/3, works from 2/4...
 {
    hdi.pjc <-  sapply(probs, function(z) 
					{
					pjc.res.list <- list(x=theta, y=pjc.res)
					attr(pjc.res.list,"class") <- "density"
					hdi(pjc.res.list,credMass=z)
					})
 } else {
  cat(paste("s = ",si," of N = ",Ni," >> i.e. not enough successes or not enough data (= N)>>\nHPD interval does not make sense for Jeffreys-Carnap method (success rates).\nPlot the function with these parameters to understand 'why'.\n\n",sep=""))
  hdi.pjc <- matrix(data=NA, nrow=2, ncol=3)#nrow=5
 }
  colnames(hdi.pjc) <- paste(probs,"%",sep="")
  rownames(hdi.pjc) <- rownames(hdi.pbl) <- c("lower","upper")
  hdi.pjc <- cbind(rn, hdi.pjc)

 hdi.pbl
 hdi.pjc

 res <- data.frame(ID=rn, si, Ni, theta.max.pbl.res, theta.max.pjc.res, cbind(t(sN.ME.post.mean), sqrt(t(sN.ME.post.vari)), t(sN.ME.post.vari)))
 colnames(res) <- c("ID","si","Ni","BL (mode)","JC (mode)","BL (mean)","JC (mean)","BL (sd)","JC (sd)","BL (var)","JC (var)")
 rownames(res) <- ""
 res.list <- structure(list(res=res, hdi.BL=hdi.pbl, hdi.JC=hdi.pjc))
 res.list 
 if(printout)
 {
  op.orig <- options(digits=2)
  cat("\n\n################################\n")
  cat("\nOUTPUT posterior results and HDI\n")
  cat("\nBayes-Laplace and Jeffreys-Carnap\n\n")
  print(res.list, digits=dig)
  cat("################################\n")
  options(op.orig)
 }
return(res.list)
}
# call:
# si <- 23
# Ni <- 27
# sN.ME.post.summary <- sN.post.su(Ni=Ni, si=si)

#> print(sN.ME.post.summary, right=FALSE)
#$res
#  ID si Ni BL (mode) JC (mode) BL (mean) JC (mean)    BL (sd)    JC (sd)    BL (var)    JC (var)
#1    23 27 0.8518519 0.8798799 0.8275862 0.8518519 0.06896552 0.06713533 0.004756243 0.004507153
#
#$hdi.BL
#          0.69%     0.95%     0.99%
#lower 0.7757758 0.6916917 0.6326326
#upper 0.9109109 0.9499499 0.9679680
#
#$hdi.JC
#          0.69%     0.95%     0.99%
#lower 0.8058058 0.7197197 0.6576577
#upper 0.9349349 0.9679680 0.9809810
########################## END OF FUNCTION


###### function to plot UMS BL & JC results
sN.sum.plot <- function(tab, rn=NULL, TITLE="", xlab="", type="l")
{
 rn <- tab[,"ID"]
 if(is.null(rn)) rn <- rownames(tab)
# non-fancy plots
 par(mar=c(5,6,5,5), oma=c(2,1,1,1))
 par(mfrow=c(3,2))

 plot(rn,tab[,"BL (mean)"], type=type, col="red", bty="l",xlab=xlab,ylab="mean change",xaxt="n")
 axis(side=1, at=rn)
 points(rn,tab[,"JC (mean)"], type=type, col="blue", bty="l", lty=2)

 plot(rn,tab[,"BL (sd)"], type=type, col="red", bty="l",xlab=xlab,ylab="sd change",xaxt="n")
 axis(side=1, at=rn)
 points(rn,tab[,"JC (sd)"], type=type, col="blue", bty="l", lty=2)

 plot(rn,tab[,"BL (mean)"]/tab[,"BL (sd)"], type=type, col="red", bty="l",xlab=xlab,ylab="mean/sd change",xaxt="n")
 axis(side=1, at=rn)
 points(rn,tab[,"JC (mean)"]/tab[,"JC (sd)"], type=type, col="blue", bty="l", lty=2)

 plot(rn,tab[,"BL (mean)"]/tab[,"BL (var)"], type=type, col="red", bty="l",xlab=xlab,ylab="mean/var change",xaxt="n")
 axis(side=1, at=rn)
 points(rn,tab[,"JC (mean)"]/tab[,"JC (var)"], type=type, col="blue", bty="l", lty=2)

 plot(tab[,"BL (mean)"],tab[,"BL (sd)"], type=type, col="red", bty="l",xlab="mean",ylab="sd")
 points(tab[,"JC (mean)"],tab[,"JC (sd)"], type=type, col="blue", bty="l", lty=2)

 plot(tab[,"BL (mean)"],tab[,"BL (var)"], type=type, col="red", bty="l",xlab="mean",ylab="var")
 points(tab[,"JC (mean)"],tab[,"JC (var)"], type=type, col="blue", bty="l", lty=2)
 
 mtext(TITLE, 3, line=-3, cex=1.5, outer=TRUE)	
 
 par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
 plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
 legend("bottom",legend=c("Bayes-Laplace","Jeffreys-Carnap"),
        xpd=TRUE, horiz=TRUE, inset=c(0,0), y.intersp=2.4,
        col=c("red","blue"), lty=c("solid","dashed"), lwd=1.75, bty="n", cex=1)		
}
# call:
# sN.sum.plot(tab=sa.res, TITLE="Verlauf Durchlaufquoten start again", xlab="year")
########################## END OF FUNCTION

# obscure analytics & Co.
# http://www.obscureanalytics.com/2012/07/04/to-the-basics-bayesian-inference-on-a-binomial-proportion/

# theta.prior = m = probability/ mean of beta distribution
# nprior = n = sample size prior
# a, b = alpha, beta distribution values
# si = Y = successes likelihood
# Ni = N = N likelihood


###### function to calculate prior beta values
bino.ab.prior <- function(theta.prior, nprior)
{ 
 ab.prior <- c(nprior * theta.prior, nprior * (1 - theta.prior))
 names(ab.prior) <- c("a","b")
 attr(ab.prior, "type") <- c("prior")
return(ab.prior)
}
########################## END OF FUNCTION



###### function to calculate likelihood beta values
bino.ab.lik <- function(si, Ni)
{ 
 ab.lik <- c(si + 1, Ni - si + 1)
 names(ab.lik) <- c("a","b")
 attr(ab.lik, "type") <- c("likelihood")
return(ab.lik)
}
########################## END OF FUNCTION


###### function to calculate posterior beta values
bino.ab.post <- function(a.prior, b.prior, si, Ni)
{ 
 ab.post <- c(a.prior + si, b.prior + Ni - si)
 names(ab.post) <- c("a","b")
 attr(ab.post, "type") <- c("post")
return(ab.post)
}
########################## END OF FUNCTION


###### function to calculate beta posterior
# Beta(a,b) = prior 
# Binomial(theta,n) = Binomial(si,Ni) = likelihood
# Beta.post(a.prior + si, beta + Ni - si)
# posterior beta values
bino.ab.post.1 <- function(a.prior, b.prior, a.lik, b.lik)
{ 
 ab.post <- c(a.prior + a.lik - 1, b.prior + b.lik - 1)
 names(ab.post) <- c("a","b")
 attr(ab.post, "type") <- c("post")
return(ab.post)
}
########################## END OF FUNCTION


###### function to calculate posterior beta values alternative
# bino.ab.post.2 <- function(theta.prior, nprior, si, Ni)
#{ return(c("a.post" = si + (nprior * theta.prior) - 1, "b.post" = Ni - si + (nprior * (1 - theta.prior)) - 1)) }

# mean, mode, variance beta distribution
# independent of prior or posterior
#beta.summary <- function(a, b)
#{
# MEAN <- a / (a + b)   # = 1/(1+b/a)
# MODE <- (a - 1) / (a + b - 2)
# VAR <- (a * b) / (((a + b)^2) * (a + b + 1))
# SD <- sqrt(VAR)
# beta.sum <- list(a = a, b = b, mode = MODE, mean = MEAN, sd = SD, var = VAR)
# names(beta.sum) <- c("a", "b", "mode","mean","sd","var")
## attr(beta.sum, "type") <- c("summary") 
#return(beta.sum)
##https://lingpipe-blog.com/2009/09/23/bayesian-estimators-for-the-beta-binomial-model-of-batting-ability/
##?map mode <-  (si + a - 1) / (Ni + a + b - 2)
##http://www.cs.cmu.edu/~aarti/Class/10701/slides/Lecture3.pdf
##MAP/MODE <- (a.prior + si - 1) / (a.prior + si + (Ni - si) + b.prior - 2)
#}
##beta.summary(a=2, b=4)
########################## END OF FUNCTION


###### function to calculate beta summary
beta.summary <- function(a, b)
{
 a <- as.vector(a)
 b <- as.vector(b)
 MEAN <- a / (a + b)   # = 1/(1+b/a)
 MODE <- (a - 1) / (a + b - 2)
 VAR <- (a * b) / (((a + b)^2) * (a + b + 1))
 SD <- sqrt(VAR)
 beta.sum <- list(a = a, b = b, mode = MODE, mean = MEAN, sd = SD, var = VAR)
 names(beta.sum) <- c("a", "b", "mode","mean","sd","var")
# attr(beta.sum, "type") <- c("summary") 
return(beta.sum)
#https://lingpipe-blog.com/2009/09/23/bayesian-estimators-for-the-beta-binomial-model-of-batting-ability/
#?map mode <-  (si + a - 1) / (Ni + a + b - 2)
#http://www.cs.cmu.edu/~aarti/Class/10701/slides/Lecture3.pdf
#MAP/MODE <- (a.prior + si - 1) / (a.prior + si + (Ni - si) + b.prior - 2)
}
# call:
# beta.summary(a ,b)
########################## END OF FUNCTION


###### function to estimate a and b from mu (mean) and v(ariance)
# independent of prior or posterior
beta.ab <- function(MEAN, VAR)
{
 a <- (((1 - MEAN) / VAR) - (1 / MEAN)) * MEAN^2
 b = a * ((1 / MEAN) - 1)
 return(c("a"=a, "b"=b))
}
########################## END OF FUNCTION


###### function to calculate si, Ni from a, b (back!)
rev.ab.lik <- function(a,b)
{
  si <- a - 1
  Ni <- b + si - 1
  return(c(si=si,Ni=Ni))
}
bino.ab.lik(17,23)
rev.ab.lik(18,7)
########################## END OF FUNCTION

###### function to calculate summary statistics
#####
bino.abs.2.OLD <- function(si, Ni, theta.prior=NULL, nprior=NULL, probs=c(0.69,0.95,0.99), rn="", graph=TRUE)
{
  # calculate ab values
  # uniform prior
  ab.prior <- bino.ab.prior(theta.prior=theta.prior, nprior=nprior)
  ab.lik <- bino.ab.lik(si=si, Ni=Ni)
  ab.post <- bino.ab.post(a.prior=ab.prior[["a"]], b.prior=ab.prior[["b"]], si=si, Ni=Ni)
  ab.prior
  ab.lik
  ab.post
  
  # calculate summary statistics
  prior.sum <- beta.summary(a=ab.prior[["a"]], b=ab.prior[["b"]])
  lik.sum <- beta.summary(a=ab.lik[["a"]], b=ab.lik[["b"]])
  post.sum <- beta.summary(a=ab.post[["a"]], b=ab.post[["b"]])
  prior.sum
  lik.sum
  post.sum
  
  # res
  res <- t(as.matrix(c(ab.prior, ab.lik, ab.post, prior.sum, lik.sum, post.sum)))
  res <- c(ab.prior, ab.lik, ab.post, prior.sum, lik.sum, post.sum)
  # dimnames(res)[[2]][1:18]
  res <- t(data.frame(unlist(res)))
  rownames(res) <- c("")
  colnames(res) <- c("a.prior","b.prior","a.lik","b.lik","a.post","b.post",
                     "a","b","mode.prior","mean.prior","sd.prior","var.prior",
                     "a","b","mode.lik","mean.lik","sd.lik","var.lik",
                     "a","b","mode.post","mean.post","sd.post","var.post")
  res
  
  # calculate hdi intervals 
  ab <- rbind(ab.prior, ab.lik, ab.post)
  rownames(ab) <- c("prior","likelihood","post")
  require(HDInterval)
  hdis <- do.call("rbind", lapply(seq_along(probs), function(x)
  {
    apply(ab, 1, function(i)
    {
      a <- unlist(i["a"])
      b <- unlist(i["b"])
      hdi(qbeta, shape1=a, shape2=b, credMass=probs[x])
    })
  }
  ))
  hdis <- cbind(prob=rep(probs,each=2),hdis)
  
  if(graph) beta.triplot(si=si, Ni=Ni, v=res, multiplot=TRUE, rn=rn)
  return(list(res=res,hdi=hdis))
}
# call:
# si <- 23
# Ni <- 27
# theta.prior <- 0.5
# nprior <- 2
# bino.abs(si=si, Ni=Ni, theta.prior=theta.prior, nprior=nprior, graph=TRUE)
########################## END OF FUNCTION

###### function to calculate summary statistics
bino.abs.2 <- function(si, Ni,
                       theta.prior=NULL, nprior=NULL,
                       a.prior=NULL, b.prior=NULL,
                       probs=c(0.69,0.95,0.99),
                       rn="", graph=TRUE)
{
  # calculate ab values
  # uniform prior OR previous posterior values (a,b values)
  if(!is.null(theta.prior) & !is.null(nprior))
  {
    ab.prior <- bino.ab.prior(theta.prior=theta.prior, nprior=nprior)
  } else
  {
    ab.prior <- c(a.prior, b.prior)
    names(ab.prior) <- c("a","b")
    attr(ab.prior, "type") <- c("prior")
  }  
  
  ab.lik <- bino.ab.lik(si=si, Ni=Ni)
  ab.post <- bino.ab.post(a.prior=ab.prior[["a"]], b.prior=ab.prior[["b"]], si=si, Ni=Ni)
  ab.prior
  ab.lik
  ab.post
  
  # calculate summary statistics
  prior.sum <- beta.summary(a=ab.prior[["a"]], b=ab.prior[["b"]])
  lik.sum <- beta.summary(a=ab.lik[["a"]], b=ab.lik[["b"]])
  post.sum <- beta.summary(a=ab.post[["a"]], b=ab.post[["b"]])
  prior.sum
  lik.sum
  post.sum
  
  # res
  res <- t(as.matrix(c(ab.prior, ab.lik, ab.post, prior.sum, lik.sum, post.sum)))
  res <- c(ab.prior, ab.lik, ab.post, prior.sum, lik.sum, post.sum)
  # dimnames(res)[[2]][1:18]
  res <- t(data.frame(unlist(res)))
  rownames(res) <- c("")
  colnames(res) <- c("a.prior","b.prior","a.lik","b.lik","a.post","b.post",
                     "a","b","mode.prior","mean.prior","sd.prior","var.prior",
                     "a","b","mode.lik","mean.lik","sd.lik","var.lik",
                     "a","b","mode.post","mean.post","sd.post","var.post")
  res
  
  # calculate hdi intervals 
  ab <- rbind(ab.prior, ab.lik, ab.post)
  rownames(ab) <- c("prior","likelihood","post")
  require(HDInterval)
  hdis <- do.call("rbind", lapply(seq_along(probs), function(x)
  {
    apply(ab, 1, function(i)
    {
      a <- unlist(i["a"])
      b <- unlist(i["b"])
      hdi(qbeta, shape1=a, shape2=b, credMass=probs[x])
    })
  }
  ))
  hdis <- cbind(prob=rep(probs,each=2),hdis)
  
  if(graph) beta.triplot(si=si, Ni=Ni, v=res, multiplot=TRUE, rn=rn)
  return(list(res=res,hdi=hdis))
}
# call:
# si <- 23
# Ni <- 27
# theta.prior <- 0.5
# nprior <- 2
# bino.abs(si=si, Ni=Ni, theta.prior=theta.prior, nprior=nprior, graph=TRUE)
########################## END OF FUNCTION

###### function to calculate updates prior and posts based on
#- prior (if given)
#- all likelihoods given
#- parameters prior[i] = post[i-1]
# lbyxp = learning by experience
betabinomial.lbyxp <- function(sa.res.py, prior=NA, sameprior=FALSE)
{
 sa.res.py.dim <- dim(sa.res.py)
 if(sameprior) r <- 1:sa.res.py.dim[1] else r <- 1
 if(is.list(prior)) sa.res.py[r,c("a.prior","b.prior")] <- unlist(prior)

 for(i in 1:sa.res.py.dim[1])
 {
  if(i > 1 && !sameprior)
  {
   sa.res.py[i,c("a.prior","b.prior")] <- sa.res.py[i-1,c("a.post","b.post")]
  }
  ab.post <- bino.ab.post.1(a.prior=sa.res.py[i,"a.prior"],b.prior=sa.res.py[i,"b.prior"],
                            a.lik=sa.res.py[i,"a.lik"], b.lik=sa.res.py[i,"b.lik"])
  sa.res.py[i,c("a.post","b.post")] <- unlist(ab.post)
 }
return(sa.res.py)
}
# call:
# betabinomial.lbyxp(sa.res.py=sa.res.py, prior=list(a=1,b=1))
# betabinomial.lbyxp(sa.res.py=sa.res.py, prior=list(a=1,b=1), sameprior=TRUE)
########################## END OF FUNCTION


###### function to plot beta triplot
beta.triplot <- function(si, Ni, v, steps=1000, fac=1.15,
                         multiplot=FALSE, musdplot=TRUE, plots=c(TRUE,TRUE,TRUE),
                         rn="", dig=3, filling=TRUE,
                         sele=c(0,1))
{
 
 theta <- seq(sele[1],sele[2], length.out=steps)
 names(plots) <- c("prior","likelihood","post")

 prior <- dbeta(theta, v[,"a.prior"], v[,"b.prior"])
 likelihood <- dbeta(theta, v[,"a.lik"], v[,"b.lik"])
 posterior <- dbeta(theta, v[,"a.post"], v[,"b.post"])
 
# check for NaNs
 if(sum(is.nan(prior)) > 2)
 { 
  plots["prior"] <- FALSE
  prior <- Inf  
 }

 if(sum(is.nan(posterior)) > 2)
 {
  plots["post"] <- FALSE
  posterior <- Inf
 }

# prior
# likelihood
# posterior 
# plots

 dbetas <- c(prior,likelihood,posterior)
 ylim.max <- max(dbetas[dbetas < Inf & dbetas > -Inf])*fac

 if(!multiplot) par(mar=c(5,6,5,5), oma=c(2,1,1,1), cex.axis=0.8)
 
 cols <- c("steelblue","lawngreen","red","blue","green","red")
 col.v <- col2rgb(cols)/255
  
 plot(0, 0,#1,ylim.max
      main="", xlab=rn, ylab=expression(paste("p(H[",theta,"] | s/N, I)",sep="")),
	  pre.plot=grid(),
      type="l", lty=2, lwd=2, col="white", xlim=sele, ylim=c(0,ylim.max), bty="l", axes=FALSE)
 axis(2)
 axis(1)

 if(musdplot)
 {
   mtext(eval(substitute(expression(paste(mu," = ",mean.post," | ",sigma," = ",sd.post)),
	       list(mean.post=round(v[,"mean.post"],dig),sd.post=round(v[,"sd.post"],dig)))),
         3, line=0.8, col="black")
 }
 
 if(plots["prior"])
 {
  # uniform prior
  if(length(unique(prior)) == 1)
  {
   prior.area <- theta[prior < Inf & prior > -Inf]
   if(filling) polygon(c(0,0,prior.area,max(prior.area)), c(0,prior,0,0), col=rgb(col.v[1,1], col.v[2,1], col.v[3,1], alpha=0.3), border=NA)
  } else
  {
   if(filling) polygon(c(0,theta[prior < Inf & prior > -Inf],0), c(0,prior[prior < Inf & prior > -Inf],0), col=rgb(col.v[1,1], col.v[2,1], col.v[3,1], alpha=0.3), border=NA)
  }
  points(theta[prior < Inf & prior > -Inf], prior[prior < Inf & prior > -Inf], type="l", lty=3, lwd=2, col=cols[4])
 } 
 
 if(plots["likelihood"])
 {
  likelihood.area <- theta[likelihood < Inf & likelihood > -Inf]
  if(filling) polygon(c(0,0,likelihood.area,max(likelihood.area)), c(0,likelihood,0,0), col=rgb(col.v[1,2], col.v[2,2], col.v[3,2], alpha=0.3), border=NA)
  points(theta, likelihood, type="l", lty=3, lwd=4, col=cols[5])
 }
 
 if(plots["post"])
 {
  if(filling) polygon(c(0,theta[posterior < Inf & posterior > -Inf],0), c(0,posterior[posterior < Inf & posterior > -Inf],0), col=rgb(col.v[1,3], col.v[2,3], col.v[3,3], alpha=0.3), border=NA)
  points(theta[posterior < Inf & posterior > -Inf], posterior[posterior < Inf & posterior > -Inf], type="l", lty=1, lwd=2, col=cols[6])
 }
  
# mtext(expression(paste(theta)), 1, line=3, cex=1.5)
 
 if(!multiplot)
 {
  mtext(expression(paste("Success rates",sep="")), 3, line=2, cex=1.5)
  mean.BL <- round(((si + 1) / (Ni + 2))*100,dig)
  mean.JC <- round(si/Ni*100,dig)
  mtext(eval(substitute(expression(paste("BL (mean) = ",mean.BL,"%  |  JC (mean) = ",mean.JC,"%")),
  list(mean.BL=mean.BL, mean.JC=mean.JC))), 4, line=1, cex=0.8)
# new plot - new legend... below the first one
# http://dr-k-lo.blogspot.de/2014/03/the-simplest-way-to-plot-legend-outside.html  
  par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
  plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
  legend("bottom",legend=c("Prior","Likelihood","Posterior"),
         xpd=TRUE, horiz=TRUE, inset=c(0,0), y.intersp=3.4,
         col=c("blue","green","red"), lty=c(2,3,1), lwd=1.9, bty="n", cex=0.9)         
 }
 
} 
# call:
# si <- 23
# Ni <- 27
# theta.prior <- 0.5
# nprior <- 2
# bino.abs.res <- bino.abs(si=si, Ni=Ni, theta.prior=theta.prior, nprior=nprior, graph=FALSE)
# v <- data.frame(do.call("cbind",bino.abs.res$res))
# colnames(v) <- attr(bino.abs.res$res,"dimnames")[[2]]
# v
# plot success rates (prior, likelihood, posterior)
# beta.triplot(si=si, Ni=Ni, v=v, multiplot=FALSE, filling=FALSE)
# beta.triplot(si=si, Ni=Ni, v=v, multiplot=FALSE, filling=TRUE)
# end of automatic functions
########################## END OF FUNCTION


###### function taken and modified from
# BayesianFirstAid:::summary.bayes_prop_test
# https://github.com/rasmusab/bayesian_first_aid
BFA.summary.bayes_prop_test <- function(object, diff.crit=0.05, dig=3, groupn=NULL, ...)
{
  s <- round(object$stats, 3) 
  cat("\n  Data\n\n")
  #pad_width <- max(nchar(as.character(c(object$x, object$n)))) + 1
  datf <- data.frame(Successes=object$x, Failures=object$n-object$x, "N (total)"=object$n, check.names=FALSE)
  if(is.null(groupn)) groupn <- paste("Group ",1:dim(datf)[1]," |  ",sep="")
  rownames(datf) <- groupn
  print(datf, right=FALSE) 
 # cat("number of successes: ", paste(str_pad(object$x, pad_width), collapse = ","), "\n", sep="")
 # cat("number of trials:    ", paste(str_pad(object$n, pad_width), collapse = ","), "\n", sep="")
  cat("\n")
  
  cat("  Model parameters and generated quantities\n\n")
  cat("*- theta[i]\t   = the relative frequency of success for Group i\n")
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
########################## END OF FUNCTION


###### function to plot scatterplot 2D of 2 MCMC chains
# usefull for Bayesian test of proportions
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
 
 par(mar=c(5,6,5,5), oma=c(2,1,1,1), cex.axis=0.8)
# par(bg="yellow")
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
 
# https://www.r-bloggers.com/how-to-summarize-a-2d-posterior-using-a-highest-density-ellipse/
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
########################## END OF FUNCTION


###### function to plot beta triplot
# alternate version
beta.triplot2 <- function(si, Ni, v, steps=1000, fac=1.15, multiplot=FALSE, rn="", dig=3, filling=TRUE,
                          TITLE="Erfolgswahrscheinlichkeiten")
{
  plots <- c(TRUE,TRUE,TRUE)
  names(plots) <- c("prior","lik","post")
  plots
  theta <- seq(0,1,length.out=steps)
  prior <- dbeta(theta, v[,"a.prior"], v[,"b.prior"])
  likelihood <- dbeta(theta, v[,"a.lik"], v[,"b.lik"])
  posterior <- dbeta(theta, v[,"a.post"], v[,"b.post"])
  v
  # head(prior)
  # head(likelihood)
  # head(posterior)
  if(sum(is.nan(prior)) > 2)
  { 
    plots["prior"] <- FALSE
    prior <- Inf  
  }
  prior
  if(sum(is.nan(posterior)) > 2)
  {
    plots["post"] <- FALSE
    posterior <- Inf
  }
  
  posterior 
  plots
  
  dbetas <- c(prior,likelihood,posterior)
  ylim.max <- max(dbetas[dbetas < Inf & dbetas > -Inf])*fac
  ylim.max
  if(!multiplot)
  {
    par(mar=c(5,6,5,5), oma=c(2,1,1,1))
    par("cex.axis"=0.8)
  }
  cols <- c("steelblue","lawngreen","red","blue","green","red")
  col.v <- col2rgb(cols)/255
  col.v
  
  plot(0,0,
       main=rn,
       xlab="",
       ylab=expression(paste("p(H[",theta,"] | s/N, I)",sep="")),
       type="l", lty=2, lwd=2, col="white", xlim=c(0,1), ylim=c(0,ylim.max), bty="l", axes=FALSE)
  rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4], col="grey95", border=NA)
  grid(col="white", lwd=1, lty=2)
  axis(2)
  axis(1)
  
  mtext(eval(substitute(expression(paste("p(",theta,"|s/N,I): mu = ",mean.post," | sd = ",sd.post)),
                        list(mean.post=round(v[,"mean.post"],dig),sd.post=round(v[,"sd.post"],dig)))),
        3, line=0.8, col="black")
  
  if(plots["prior"])
  {
    polygon(c(theta[prior < Inf & prior > -Inf],max(theta[prior < Inf]),theta[prior < Inf][1]), c(prior[prior < Inf & prior > -Inf],0,0), col=rgb(col.v[1,1], col.v[2,1], col.v[3,1], alpha=0.3), border=NA)
    points(theta[prior < Inf & prior > -Inf], prior[prior < Inf & prior > -Inf], type="l", lty=3, lwd=2, col=cols[4])
  } 
  
  if(plots["lik"])
  {
    polygon(c(theta,max(theta),theta[1]), c(likelihood,0,0), col=rgb(col.v[1,2], col.v[2,2], col.v[3,2], alpha=0.3), border=NA)
    points(theta, likelihood, type="l", lty=3, lwd=2, col=cols[5])
  }
  
  if(plots["post"])
  {
    polygon(c(theta[posterior < Inf & posterior > -Inf],max(theta[posterior < Inf]),theta[posterior < Inf][1]), c(posterior[posterior < Inf & posterior > -Inf],0,0), col=rgb(col.v[1,3], col.v[2,3], col.v[3,3], alpha=0.3), border=NA)
    points(theta[posterior < Inf & posterior > -Inf], posterior[posterior < Inf & posterior > -Inf], type="l", lty=1, lwd=2, col=cols[6])
  }
  
  # mtext(expression(paste(theta)), 1, line=3, cex=1.5)
  
  if(!multiplot)
  {
    mtext(eval(substitute(expression(TITLE),list(TITLE=TITLE))), 3, line=2, cex=1.5)
    mean.BL <- round(((si + 1) / (Ni + 2)),dig)
    mean.JC <- round(si/Ni,dig)
    mtext(eval(substitute(expression(paste("BL (mean) = ",mean.BL,"  |  JC (mean) = ",mean.JC)),
                          list(mean.BL=mean.BL, mean.JC=mean.JC))), 4, line=1, cex=0.8)
    #new plot - new legend... below the first one
    #http://dr-k-lo.blogspot.de/2014/03/the-simplest-way-to-plot-legend-outside.html  
    par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
    plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
    legend("bottom",legend=c("prior","likelihood","posterior"),
           xpd=TRUE, horiz=TRUE, inset=c(0,0), y.intersp=3.4,
           col=c("blue","green","red"), lty=c(2,3,1), lwd=1.9, bty="n", cex=0.9)         
  }
} 
# call:
# beta.triplot2(si, Ni, v, TITLE="Lady Bristol's tea experiment")
########################## END OF FUNCTION

