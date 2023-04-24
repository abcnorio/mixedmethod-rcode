# file:
# ptII_quan_classicstats_N-P_nulldist-hypotest_helpfuncs.r

# location:
# chap. 4 [4.5.2.5]
# Die Vollkostenrechnung

# HELPER FUNCTIONS


###### function to plot H0
#t and normal distribution (but no noncentral distributions)
plot.H0 <- function(alpha.err=0.05, mu0=NA, sigma0=NA, type="n", N=NA, alternative="two.sided", direction="less", sek.N=1000, fak=1.25, range.SD=3, digits=2)
{
 if(alternative == "two.sided") alpha.fak <- 2 else
 {
  alpha.fak <- 1
 } 
 
 if(type == "n")
 {
  sek <- seq(mu0-range.SD*sigma0,mu0+range.SD*sigma0,length.out=sek.N)
  sek.dnv <- dnorm(sek, mean=mu0, sd=sigma0)
  dfree <- "none"
  ncp <- NA
 } else if(type == "t")
 {
  if(is.na(N)) stop("\nwith type = 't' the sample size N must be chosen\n")
  cat("\nsigma will be ignored for type = 't'\n")
  if(N > 2)
  { 
   var0 <- N/(N-2)
   sigma0 <- sqrt(var0)
   dfree <- N-1
   ncp <- 0
  } else stop("\nN must be greater than 2 for type = 't'\n") 
  sek <- seq(ncp-range.SD*sigma0,ncp+range.SD*sigma0,length.out=sek.N)
  sek.dnv <- dt(sek, df=dfree, ncp=ncp) 
  sek <- sek + mu0
 }
 
 # plot one curve
 col.H0 <- "#AAAAAA"
 xrange <- range(sek) + c(-1,1)*fak
 yrange <- round(range(sek.dnv)*fak,3)

 par(mar=c(5,5,4,2))
 par(oma=c(2,1,1,1))
 par("cex.axis"=0.8)
 
 plot(xrange, yrange, type="n", xlab="", ylab="", lty=2, bty="n", main="")

 grid(col="grey80", lwd=1.2, lty=2)
 
 polygon(x=c(min(sek),sek,max(sek)), y=c(0,sek.dnv,0), col=col.H0, lwd=2, density=c(10,40), angle=-45, border=0)

 mtext(expression(paste("Classical Null Hypothesis Test (NHST)",sep="")), 3, line=2, cex=1.5)
 mtext(expression(paste(H[0]," and ",alpha," error rates")), 3, line=0.7, cex=1.1)
 if(type == "n") typeX <- "z" else typeX <- "t"
 mtext(eval(substitute(expression(paste(typeX)), list(typeX=typeX))), side=1, line=3, cex=1.1)
 mtext(expression(paste("density")), side=2, line=3, cex=1.1)

 #crit.alpha
 if(type == "n")
 {
  q.ca.low <- qnorm(alpha.err/alpha.fak, mean=mu0, sd=sigma0, lower.tail=TRUE)
  q.ca.up <- qnorm(alpha.err/alpha.fak, mean=mu0, sd=sigma0, lower.tail=FALSE)
 } else if(type == "t")
 {
  q.ca.low <- mu0 + qt(alpha.err/alpha.fak, df=dfree, ncp=ncp, lower.tail=TRUE)
  q.ca.up <- mu0 + qt(alpha.err/alpha.fak, df=dfree, ncp=ncp, lower.tail=FALSE)  
 }
 
 if(alternative == "two.sided")
  {
   polygon(x=c(sek[sek <= q.ca.low],q.ca.low,min(sek),min(sek)),
           y=c(sek.dnv[sek <= q.ca.low],0,0,sek.dnv[1]),
   	       col="skyblue", border=NULL)
   arr.x <- c(q.ca.low-.6*sigma0, q.ca.up+.6*sigma0)
   arr.y1 <- sek.dnv[sek >= q.ca.low][1]*.8
   arr.y2 <- sek.dnv[sek >= arr.x[1]][1]*.55
   arrows(arr.x[1], arr.y1, arr.x[1], arr.y2, length=".15", col="red", code="2", lwd=2)
   text(x=arr.x[1], y=sek.dnv[sek >= q.ca.low][1]*1.1, expression(paste(alpha,"/2")), cex=1.25)
   
   polygon(x=c(q.ca.up,sek[sek >= q.ca.up],max(sek),max(sek)),
           y=c(0,sek.dnv[sek >= q.ca.up],sek.dnv[sek.N],0),
   	       col="skyblue", border=NULL) 
   arr.y1 <- sek.dnv[sek >= q.ca.up][1]*.8
   arr.y2 <- sek.dnv[sek >= arr.x[2]][1]*.55
   arrows(arr.x[2], arr.y1, arr.x[2], arr.y2, length=".15", col="red", code="2", lwd=2)
   text(x=arr.x[2], y=sek.dnv[sek >= q.ca.up][1]*1.1, expression(paste(alpha,"/2")), cex=1.25)
   
  } else if(alternative == "less")
  {
   polygon(x=c(sek[1],sek[sek <= q.ca.low],max(sek[sek <= q.ca.low]),max(sek[sek <= q.ca.low])),
 	   y=c(0,sek.dnv[sek <= q.ca.low][1],sek.dnv[sek <= q.ca.low],0),
 	   col="skyblue", border=NULL) 
   arr.x <- q.ca.low-.7*sigma0
   arr.y1 <- sek.dnv[sek >= q.ca.low][1]*.9
   arr.y2 <- sek.dnv[sek >= arr.x][1]*.55
   arrows(arr.x, arr.y1, arr.x, arr.y2, length=".15", col="red", code="2", lwd=2)
   text(x=q.ca.low-.7*sigma0, y=sek.dnv[sek >= q.ca.low][1]*1.1, expression(paste(alpha)), cex=1.8)   
   
  } else if(alternative == "greater")
  {
   polygon(x=c(q.ca.up,sek[sek >= q.ca.up][1],q.ca.up,sek[sek >= q.ca.up]),
           y=c(0,sek.dnv[sek >= q.ca.up],sek.dnv[sek.N],0),
           col="skyblue", border=NULL)
   arr.x <- q.ca.up+.7*sigma0
   arr.y1 <- sek.dnv[sek >= q.ca.up][1]*.9
   arr.y2 <- sek.dnv[sek >= arr.x][1]*.55
   arrows(arr.x, arr.y1, arr.x, arr.y2, length=".15", col="red", code="2", lwd=2)
   text(x=q.ca.up+.7*sigma0, y=sek.dnv[sek >= q.ca.up][1]*1.1, expression(paste(alpha)), cex=1.8)
   
  }
  

 sek.dnv.max <- max(sek.dnv)
 text(x=mu0, y=sek.dnv.max*1.1, expression(paste("H"[0])), cex=1.2)
 
 lines(sek, sek.dnv, type="l", col="orange", lwd=1.4)
 lines(x=c(mu0,mu0), y=c(0,max(sek.dnv)), col="orange", lwd=1.4, lty=3)

 mtext(eval(substitute(expression(paste(mu[0]," = ",mu0,"  |  ",sigma[0]," = ",sigma0,"  |  ",alpha," = ",alpha.err,"  |  alternative = '",alternative,"' | df = ",dfree)),
	   list(mu0=round(mu0,digits), sigma0=round(sigma0,digits), alpha.err=alpha.err, type=type, alternative=alternative, dfree=dfree))),
	   side=1, line=5, cex=0.9, col="black")
 
}
#call
#plot.H0(mu0=0, sigma0=1, type="two.sided")
########################## END OF FUNCTION


###### function to plot alpha and beta error rate for t-distributed densities
# normal densities not yet supported
plot.ab.err <- function(n1=NA, n2=NA, mu1=NA, sigma1=NA, sigma2=NA, delta=NA, alpha.err=0.05, beta.err=NA, type="t", alternative="two.sided",  sek.N=1000, fak=1.25, fak.sigma=4,  digits=3)
{
 require(pwr)
 
 if(type != "t") stop("type = 't' - only supported at the moment, please choose type = 't'")
 
 #pooled sd of both distributions
 pooled.sd <- sqrt((sigma1^2 + sigma2^2)/2)
 if(!is.null(beta.err)) paur <- 1-beta.err
 
 if(sum(sapply(list(delta, n1, alpha.err, beta.err), is.null)) != 1)
 {
  stop("One of delta, [n1, n2], alpha, beta must be NULL")
 }
 
 #if mu1,mu2 are set and delta not set
 if(sum(is.na(c(mu1,mu2))) == 0)
 {
  #effect size
  delta <- (mu2 - mu1)/pooled.sd
  beta.err <- NULL
  cat(paste("\nmu1 & mu2 are set, therefor delta is set now, beta.err is set to 'NULL'.\n\n",sep=""))
 }

#t-case 
 if(type == "t")
 {
  xaxis <- c("t-values")
  if(is.null(delta))
  {
   try.res <- try(pwr.t2n.test(d=delta, n1=n1, n2=n2, sig.level=alpha.err, power=1-beta.err, alternative=alternative))
   if(inherits(try.res,"try-error"))
   {
    cat(paste("\nA 'try-error' in 'uniroot' occured, use power.t.test() for equal variances to approximate result\n\n",sep=""))
	try.res <- power.t.test(n=(n1+n2)/2, delta=delta, sd=pooled.sd, sig.level=alpha.err, power=1-beta.err, alternative=alternative, type="two.sample")
	delta <- try.res$delta
   } else delta <- try.res$d
   mu1 <- 0
   mu2 <- mu1 + delta*pooled.sd
  } else if(is.null(beta.err))
  {
   paur <- pwr.t2n.test(d=delta/pooled.sd, n1=n1, n2=n2, sig.level=alpha.err, power=NULL, alternative=alternative)$power
   beta.err <- 1-paur
   if(sum(is.na(c(mu1,mu2))) != 0)
   {
    mu1 <- 0
	mu2 <- mu1 + delta*pooled.sd
   }
  }
 } else
 {
  stop("...works only for 'n'ormal- or 't'-distributions")
 }

 #combined df's (Welch?)
 #http://www.ncss.com/wp-content/themes/ncss/pdf/Procedures/PASS/Two-Sample_T-Tests_Allowing_Unequal_Variance-Enter_Difference.pdf
 #t-case
 dfree <- (sigma1^2/n1 + sigma2^2/n2)^2 / ( 1/(n1-1)*(sigma1^2/n1)^2 + 1/(n2-1)*(sigma2^2/n2)^2 )
 
 ncp <- delta / sqrt(sigma1^2/n1 + sigma2^2/n2)

 #https://en.wikipedia.org/wiki/Noncentral_t-distribution
 stopifnot(dfree > 2)
 ncp.mean <- ncp*sqrt(dfree/2) * exp( lgamma((dfree-1)/2) - lgamma(dfree/2) )
 ncp.sigma <- dfree*(1+ncp^2)/(dfree-2) - (ncp^2*dfree/2) * exp( lgamma((dfree-1)/2) - lgamma(dfree/2) )^2
 
 #create sequence
 sek1 <- seq(mu1-fak.sigma*sigma1, mu1+fak.sigma*sigma1, length.out=sek.N)
 sek2 <- seq(ncp.mean-fak.sigma*ncp.sigma, ncp.mean+fak.sigma*ncp.sigma, length.out=sek.N)
 
 if(type == "t")
 {
  #more simple: df = n1+n2-2
  sek1.dnv <- dt(sek1, ncp=mu1, df=dfree)
  sek2.dnv <- dt(sek2, ncp=ncp, df=dfree)
 }
 
 #define colors
 cols <- c("red","darkgreen","magenta","orange","lightgreen","violet")
 cols.rgb <- col2rgb(cols[4:6])/255
 
 #parameters of both curves
 sek1.dnv.max <- max(sek1.dnv)
 sek2.dnv.max <- max(sek2.dnv)
 
 #plot range
 x.range <- range(sek1,sek2)*fak
 y.range <- round(range(sek1.dnv,sek2.dnv)*fak,1)

 #6 6 5 2
 par(mar=c(6,6,5,5))
 par(oma=c(2,1,1,1))
 par("cex.axis"=0.8)
 
 #create plot range
 plot(x.range, y.range, type="n", xlab="", ylab="", bty="l", main="")
 #rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4], col="grey95", border=NA)
 grid(10, col="grey80", lwd=1.2, lty=2)

 #curve 1
 lines(sek1, sek1.dnv, type="l", col=rgb(1,0,0), lty=1, lwd=1.4)
 
 #crit alpha/ crit t-value 
 if(alternative == "two.sided") alpha.fak <- 2 else alpha.fak <- 1
 if(type == "t")
 {
  q.ca.low <- qt(alpha.err/alpha.fak, ncp=mu1, df=n1+n2-2, lower.tail=TRUE)
  q.ca.up <- qt(alpha.err/alpha.fak, ncp=mu1, df=n1+n2-2, lower.tail=FALSE)
 }

 #curve 2
 lines(sek2, sek2.dnv, type="l", col=rgb(0,0,1), lty=1, lwd=1.4)
 
 #alpha ie. H0 rejection regions
 if(alternative %in% c("two.sided", "less"))
 {
  polygon(x=c(sek1[sek1 <= q.ca.low],q.ca.low,-3,-3),
          y=c(sek1.dnv[sek1 <= q.ca.low],0,0,sek1.dnv[1]),
		  col=rgb(cols.rgb[1,1],cols.rgb[2,1],cols.rgb[3,1],0.7), border=NA)
 }		  
 if(alternative %in% c("two.sided", "greater"))
 {
  polygon(x=c(q.ca.up,sek1[sek1 >= q.ca.up],3,3),
          y=c(0,sek1.dnv[sek1 >= q.ca.up],sek1.dnv[n1],0),
		  col=rgb(cols.rgb[1,1],cols.rgb[2,1],cols.rgb[3,1],0.7), border=NA)
 }		  

 #beta ie. H1 rejection region
 if(alternative %in% c("two.sided", "greater"))
 {
  polygon(x=c(sek2[sek2 <= q.ca.up],q.ca.up,sek2[1]),
          y=c(sek2.dnv[sek2 <= q.ca.up],sek2.dnv[sek2==q.ca.up],0,0),
		  col=rgb(cols.rgb[1,2],cols.rgb[2,2],cols.rgb[3,2],0.7), border=NA)
 }		  
 if(alternative %in% c("less"))
 {
  polygon(x=c(q.ca.low,sek2[sek2 >= q.ca.low],sek2[n2]),
          y=c(0,sek2.dnv[sek2==q.ca.low],sek2.dnv[sek2 >= q.ca.low],0),
		  col=rgb(cols.rgb[1,2],cols.rgb[2,2],cols.rgb[3,2],0.7), border=NA)
 }
 
 #power ie. H1 acceptance region
 if(alternative %in% c("two.sided", "greater"))
 {
  polygon(x=c(sek2[sek2 > q.ca.up][1],sek2[sek2 > q.ca.up],sek2[n1]),
          y=c(0,sek2.dnv[sek2 > q.ca.up],0),
		  col=rgb(cols.rgb[1,3],cols.rgb[2,3],cols.rgb[3,3],0.7), border=NA)
 }
 if(alternative %in% c("less"))
 {
  polygon(x=c(sek2[1],sek2[sek2 < q.ca.low],q.ca.low),
          y=c(0,sek2.dnv[sek2 < q.ca.low],sek2.dnv[sek2==q.ca.low],0),
		  col=rgb(cols.rgb[1,3],cols.rgb[2,3],cols.rgb[3,3],0.7), border=NA)
 }
 
 #mark mu and mean(lambda)=ncp.mean
 lines(x=c(mu1,mu1), y=c(0,sek1.dnv.max), col="red", lty=2)
 y.ncp <- sek2.dnv[sek2 >= ncp.mean][1]
 lines(x=c(ncp.mean,ncp.mean), y=c(0,y.ncp), col="blue", lty=2)
 text(x=mu1, y=sek1.dnv.max*1.1, expression(paste(mu[1])), cex=1.2)
 text(x=ncp.mean, y=sek2.dnv.max*1.1, expression(paste(mu[2])), cex=1.2)
 
 #arrows for effect size
 arr.y <- sum(range(sek1.dnv, sek2.dnv))/2
 arrows(mu1, arr.y, mu1+ncp.mean, arr.y, length=".15", col="olivedrab", code="3")
 text(x=ncp.mean/2, y=arr.y*1.1, "ES [d]", cex=1)
 
 #axes plus descriptions 
 axis(2)
 axis(1)
 mtext(expression(paste("Classical Null Hypothesis (NHST) two sample t-test",sep="")), 3, line=2, cex=1.5)
 mtext(expression(paste(alpha," and ",beta," error rates")), 3, line=0.8, cex=1.2)
 mtext(expression(paste("t")), 1, line=3, cex=1.2)
 mtext(expression(paste("density")), 2, line=3, cex=1.2)

 #add information
 mtext(eval(substitute(expression(paste(mu[1]," = ",mu1,"  |  ",sigma[1]," = ",sigma1,"  |  ",mu[2]," = ",mu2,"  |  ",sigma[2]," = ",sigma2)),
       list(mu1=round(mu1,digits), sigma1=round(sigma1,digits), mu2=round(mu2,digits), sigma2=round(sigma2,digits)))),
	   side=4, line=0.7, cex=0.9, col="black")
 mtext(eval(substitute(expression(paste(lambda," = ",ncp,"  |  ",lambda[mean]," = ",ncp.mean,"  |  ",lambda[sd]," = ",ncp.sigma)),
       list(ncp=round(ncp,digits),ncp.mean=round(ncp.mean,digits),ncp.sigma=round(ncp.sigma,digits)))),
	   side=4, line=2, cex=0.9, col="black")	   
 mtext(eval(substitute(expression(paste(t[crit]," = ",abs(q.ca.up),"  |  ES [d] = ",delta,"  |  Power [1-",beta,"] = ",paur)),
       list(q.ca.up=round(q.ca.up,digits), paur=round(paur,digits), delta=round(delta,digits)))),
	   side=4, line=3.3, cex=0.9, col="black")

 #add nice legend below	   
 par(fig=c(0,1,0,1), oma=c(1,0,0,0), mar=c(0,0,0,0), new=TRUE)
 plot(1, type="n", bty="n", xaxt="n", yaxt="n")
 lwd.width <- 3
 legend("bottom", legend=c(eval(substitute(expression(paste(alpha," = ",alpha.err,"")),list(alpha.err=round(alpha.err,digits)))),
                        eval(substitute(expression(paste(beta," = ",beta.err,"")),list(beta.err=round(beta.err,digits)))),
						eval(substitute(expression(paste("Power = ",paur)),list(paur=round(paur,digits))))
                        ),
                        xpd=TRUE, horiz=TRUE, inset=c(0,0), y.intersp=2.4,
                        col=c("#FFA500B2","#90EE90B2","#EE82EEB2"),
                        lty=1, lwd=lwd.width, bty="n", cex=0.9)
						  
 res <- structure(list(n1=n1, mu1=mu1, sigma1=sigma1, n2=n2, mu2=mu2, sigma2=sigma2,
                       df=n1+n2-2, "ES [d]"=delta,
					   "lambda"=ncp, "lambda [mean]"=ncp.mean, "lambda [sd]"=ncp.sigma,
					   "sd [pooled]"=pooled.sd,
					   "alpha"=alpha.err, "t [crit]"=abs(q.ca.up), "beta"=beta.err,
					   "Power [1-beta]"=paur, type=type, "df [comb.]"=dfree, alternative=alternative))
 cat("\n    Hypothesis testing with alpha and beta error-rate areas | type='",res$alternative,"'\n\n")
 cat(paste(format(names(res), width=15L, justify="right"), format(res, digits=digits), sep=" = "), sep="\n")
 cat("\n")
 
invisible(res)
}
#call
#plot.ab.err(n1=n1, n2=n2, mu1=mu1, sigma1=sigma1, sigma2=sigma2, delta=delta, alpha.err=alpha.err, beta.err=beta.err, type=type, alternative=alternative)
########################## END OF FUNCTION


