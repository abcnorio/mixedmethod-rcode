###
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



# 2020-10-16
# file:
# ptall_generalfuncs_Bayes_Beta_determine.r

# location:
# none
# general functions for various scripts
# especially determine Beta values to match a Beta distribution


################################################################################
beta.determine <- function(p, qua)
{
require(LearnBayes)
quans <- data.frame(p=p, x=qua)
quans
q.1.2.prior <- beta.select(quans[1,], quans[2,])
q.1.3.prior <- beta.select(quans[1,], quans[3,])
names(q.1.2.prior) <- names(q.1.3.prior) <- c("a","b")
q.1.2.prior #shapes: a, b
q.1.3.prior #shapes: a, b
quans.prior <- rbind(q.1.2.prior, q.1.3.prior)
quans.prior.sorted <- apply(quans.prior,2,sort)
rownames(quans.prior.sorted) <- c("start","end")
quans.prior.sorted
fac <- 1000
steps.a.b <- apply(quans.prior.sorted,2,function(x) seq(from=x[1], to=x[2], length.out=fac))
head(steps.a.b)
tail(steps.a.b)

a.b <- expand.grid(steps.a.b[,"a"][1:10],steps.a.b[,"b"][1:10])
a.b <- expand.grid(steps.a.b[,"a"],steps.a.b[,"b"])
colnames(a.b) <- c("a","b")
dim(a.b)
head(a.b)
quans

#alternate1
prior.qs.1 <- apply(quans, 1, function(x) qbeta(x[1], a.b[,"a"],a.b[,"b"]))
prior.qs.2 <- abs(prior.qs.1 - quans[,"x"])
prior.qs.3 <- rowSums(prior.qs.2)
res <- a.b[which(prior.qs.3 == min(prior.qs.3)),]

#alternate2, slower...
#prior.qs <-  apply(quans, 1, function(x) abs(qbeta(x[1], a.b[,"a"],a.b[,"b"]) - x[2]) ) 
#prior.qs.sum <- apply(prior.qs, 1, sum)
#res <- a.b[which(prior.qs.sum == min(prior.qs.sum)),]

return(res)
}
#call
#system.time(a.b.values <- beta.determine(p=c(0.5,0.99999,0.00001), qua=c(0.85,0.95,0.60)))
#a.b.values

################################################################################
beta.determine.opt <- function(p, qua, ab.start=NULL, graph=FALSE, steps=1000, dig=5, leg1="topleft", leg2="left")
{
 require(LearnBayes)
 ab.optim <- function(ab)
 {
  sum( abs(qbeta(quans[1,"p"], ab[1], ab[2]) - quans[1,"x"]) +
       abs(qbeta(quans[2,"p"], ab[1], ab[2]) - quans[2,"x"]) +
       abs(qbeta(quans[3,"p"], ab[1], ab[2]) - quans[3,"x"]) )
 } 

#RMS
 ab.optim3 <- function(ab)
 {
  sqrt(sum( (qbeta(quans[1,"p"], ab[1], ab[2]) - quans[1,"x"])^2 +
       (qbeta(quans[2,"p"], ab[1], ab[2]) - quans[2,"x"])^2 +
       (qbeta(quans[3,"p"], ab[1], ab[2]) - quans[3,"x"])^2 )/3)
 }
   
 quans <- data.frame(p=p, x=qua)
 q.1.2.prior <- beta.select(quans[1,], quans[2,])
 q.1.3.prior <- beta.select(quans[1,], quans[3,])
 names(q.1.2.prior) <- names(q.1.3.prior) <- c("a","b")
 q.1.2.prior   #shapes: a, b
 q.1.3.prior   #shapes: a, b
 quans.prior <- rbind(q.1.2.prior, q.1.3.prior)
 
 if(is.null(ab.start)) ab.start <- round(colMeans(quans.prior),0)

 res.optim <- optim(par=ab.start, fn=ab.optim)
 res.quad.optim <- optim(par=ab.start, fn=ab.optim3)
 res.ab <- res.optim[["par"]]
 res.ab3 <- res.quad.optim[["par"]]
 names(res.ab) <- c("a","b")
 names(res.ab3) <- c("a","b")
 
 if(graph) betaoptim.plot(res.ab=res.ab, res.ab3=res.ab3, quans=quans, steps=steps, dig=dig, legpos=c(leg1,leg2))
 
return(list(quans=quans, res.ab=res.ab, res.ab3=res.ab3, quans.prior=quans.prior, res.optim=res.optim))
}
#call
#system.time(res <- beta.determine.opt(p=c(0.5,0.99999,0.00001), qua=c(0.85,0.95,0.60), ab.start=NULL, graph=TRUE) )
#res

#no result for
#ab.start=c(100,25)

#direct calls to optim()
#optim(par=c(81,14), ab.optim)
#optim(par=c(100,25), ab.optim)
#optim(par=c(100,25), ab.optim2)
#optim(par=c(100,25), ab.optim3)
#optim(par=c(110,20), ab.optim)
#optim(par=c(100,25), ab.optim)
#optim(par=c(50,8), ab.optim)

#
#ab.priors <- beta.determine.opt(p=c(0.50,0.9,0.15), qua=c(0.35,0.65,0.15), ab.start=c(100,25), graph=TRUE)

################################################################################
betaoptim.plot <- function(res.ab, res.ab3, quans, steps=1000, dig=5, legpos=c("topleft","left"))
{
  sek <- seq(from=0,to=1,by=1/steps)
  densities <- dbeta(sek,res.ab["a"],res.ab["b"])
  par(mar=c(5,6,5,5))
  plot(sek,densities, col="blue", lty=2, lwd=1.75, type="l", bty="l",
       main="", xlab="", ylab="Probability Density")
  mtext(expression(paste("Beta distribution",sep="")), 3, line=2, cex=1.5)
#  mtext(eval(substitute(expression(paste("Optimized values: a = ",a,", b = ",b)),list(a=round(res.ab["a"],dig), b=round(res.ab["b"],dig)))), 3, line=0.8, col="red")
  mtext(eval(substitute(expression(paste("Optimized values for a and b")))), 3, line=0.7, col="black")
  mtext(expression(paste(theta)), 1, line=3, cex=1.5)     
 
  densities3 <- dbeta(sek,res.ab3["a"],res.ab3["b"])
  points(sek,densities3, type="l", lty=1, col="red", lwd=1.75)

 # add a nice legend to list prior assumptions and resulting percentiles
 legend(legpos[1], legend=c(eval(substitute(expression(paste("lower    = ", x3, " (",p3,"%)")),
                                 list(p3 = signif(quans[3,"p"]*100,dig), x3 = signif(quans[3,"x"],dig)))),
                            eval(substitute(expression(paste("median = ", x1, " (",p1,"%)")),
                                 list(p1 = signif(quans[1,"p"]*100,dig), x1 = signif(quans[1,"x"],dig)))),
                            eval(substitute(expression(paste("upper    = ", x2, " (",p2,"%)")),
                                 list(p2 = signif(quans[2,"p"]*100,dig), x2 = signif(quans[2,"x"],dig))))
                            ),                           
                            text.col=c("black"),
                            bty="n", cex=0.9, y.intersp=1.2,
                            title="Prior assumptions and percentiles", title.col="black", title.adj=1.4)                                                       
 dig2 <- 3
 legend(legpos[2], legend=c(eval(substitute(expression(paste("a = ",a,", b = ",b, " (absolute dev)")),list(a=round(res.ab["a"],dig2), b=round(res.ab["b"],dig2)))),
                            eval(substitute(expression(paste("a = ",a,", b = ",b," (RMS)")),list(a=round(res.ab3["a"],dig2), b=round(res.ab3["b"],dig2))))
                            ),                           
                            text.col=c("blue","red"),
                            bty="n", cex=0.9, y.intersp=1.2,
                            title="", title.col="black", title.adj=1.4) 
                            
}
#call
#betaoptim.plot(res.ab=res.ab, res.ab3=res.ab3, steps=steps, dig=dig, legpos=c("topleft","left"))
################################################################################

