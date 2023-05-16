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
# ptII_quan_classicstats_GandC_type-S-M-error.r

# location:
# chap. 4 [4.5.2.2]
# Richtung und Größe — zwei unterschätzte Fehlertypen

# HELPER FUNCTIONS

# based on R-Code and ideas from


# Gelman und Tuerlinckx (2000)


# Gelman und Carlin (2014)


# Alexander Etz 2015-05-21 [blog]



###### function incl. plot of effect size d_rep for retrodesign by Gelman and Carlin (2014)
retrodesign <- function(tes, se, alpha=.05, dfree=Inf, n.sims=10000, seed=9876, graph=FALSE)
{
 # after Gelman and Carlin 2014

 #p.649
 #exaggeration = typeM
 #changes to original version from Gelman & Carlin paper:
 #tes = hypothesized true effect size (original 'A')
 #se = standard error of the estimate (original 's')
 #dfree = degrees of freedom (original 'df')
 
 set.seed(seed)
 
 z <- qt(1-alpha/2, dfree) 
 p.hi <- 1 - pt(z-tes/se, dfree) 
 p.lo <- pt(-z-tes/se, dfree)  
 powr <- p.hi + p.lo
 typeS <- p.lo/powr
 #estimate based on d_rep model
 estimate <- tes + se*rt(n.sims,dfree)
 
 #plot
 if(graph)
 {
  fac <- 1.05
  est.ds <- density(estimate)
  est.max.y.ds <- max(est.ds$y)
  est.max.x.ds <- est.ds$x[which(est.ds$y == est.max.y.ds)]
  est.range <- range(est.ds$y)
  est.range[2] <- est.range[2]*fac
  hist(estimate, ylim=est.range, col="skyblue", border="white", prob=TRUE, xlab=expression(paste("d"[rep])), ylab="Dichte", main=eval(substitute(expression(paste("Simulation distribution effect d"[rep])),list())))
  lines(est.ds, col="red", lwd=2, lty=2)
  lines(x=c(rep(est.max.x.ds,2)), y=c(0,est.max.y.ds), col="red", lwd=1.9, lty=1)
  print(summary(estimate))
  require(Hmisc)
  print(describe(estimate))
 }
 
 significant <- abs(estimate) > se*z                  #number of 'significant' results under the d_rep model
 exaggeration <- mean(abs(estimate)[significant])/tes #mean expectation of 'significant' results under the d_rep model
 return(list(power=powr, typeS=typeS, exaggeration=exaggeration))
}
#call:
#retrodesign(tes, se, graph=TRUE)
########################## END OF FUNCTION


###### function to plot power retrodesign
plot.power.retrodesign <- function(typsm.res.ref=NA, D.range=NA, tes=NA, se=NA, digits=2)
{
 typsm.res <- do.call("rbind", lapply(seq_along(D.range), function(i)
 {
  unlist(retrodesign(tes=D.range[i], se=SE))
 }))
 head(typsm.res)
 tail(typsm.res)

 #plot Power within reasonable range of D (true effect size)
 typeS <- typsm.res.ref[["typeS"]]
 exagg <- typsm.res.ref[["exaggeration"]]
 pauer <- typsm.res.ref[["power"]]

 par(mfrow=c(2,1), mar=c(4,4,4,3), oma=c(2,1,1,1), "cex.axis"=0.8)
 
 #plot 1
 plot(x=c(0,1), y=range(typsm.res[,"typeS"]), xlab="Power", ylab="Typ S error rate", bty="l", type="n")
 typs.txt <- eval(substitute(expression(paste("Typ S error rate = ",typeS)),list(typeS=round(typeS,digits))))
 mtext(typs.txt, 4, line=1, cex=0.9, col="violet")
 grid(col="grey80", lwd=1, lty=2)
 lines(typsm.res[,"power"], typsm.res[,"typeS"], col="red", lwd=2)
 abline(h=typeS, col="violet", lwd=1.5)
 abline(v=pauer, col="brown", lwd=1.5)
 points(pauer, typeS, pch=23, bg="yellow", col="black", cex=1.3, lwd=1.5)

 #plot 2
 exag.range <- range(typsm.res[,"exaggeration"][-which(is.infinite(typsm.res[,"exaggeration"]))])
 plot(x=c(0,1), y=exag.range, xlab="Power", ylab="Exaggeration ratio (Typ M)", bty="l", type="n")
 typm.txt <- eval(substitute(expression(paste("Exaggeration Ratio = ",exagg)),list(exagg=round(exagg,digits))))
 mtext(typm.txt, 4, line=1, cex=0.9, col="violet")
 grid(col="grey80", lwd=1, lty=2)
 lines(typsm.res[,"power"], typsm.res[,"exaggeration"], col="blue", lwd=2)  
 abline(h=exagg, col="violet", lwd=1.5)
 abline(v=pauer, col="brown", lwd=1.5)
 points(pauer, exagg, pch=23, bg="yellow", col="black", cex=1.3, lwd=1.5)
 segments(0.05,1,1,1,col="gray")
 
 #title  
 power.txt <- eval(substitute(expression(paste("Power = ",pauer)),list(pauer=round(pauer,digits))))
 mtext(expression(paste("Designanalysis",sep="")), 3, line=-2, cex=1.25, outer=TRUE)  
 mtext(power.txt, 3, line=-3, cex=1, outer=TRUE)

return(typsm.res) 
}
#call:
#plot.power.retrodesign(typsm.res.ref, D.range, tes, se)
########################## END OF FUNCTION


###### function to plot sample distributions under H0 and Hypo=true effect size
plot.typ.sm <- function(typsm.res.ref=NA, range.dist=NA, mw=0, emp.mw=NA, tes=NA, se=NA, digits=2, crit.alpha=0.05)
{
#inspiration



 typeS <- typsm.res.ref[["typeS"]]
 exagg <- typsm.res.ref[["exaggeration"]]
 pauer <- typsm.res.ref[["power"]]
 sek <- seq(range.dist[1], range.dist[2], length.out=1000)
 dist.ds <- dnorm(sek, mw, se)
 max.dist.ds <- max(dist.ds)
 
 par(mfrow=c(1,2), mar=c(4,4,4,3), oma=c(2,1,1,1), "cex.axis"=0.8)
 #plot in case of H0=0 (test against zero)
 plot(sek, dist.ds, main="", xlab="estimated effect size", ylab="density", type="n", col="white", bty="l", lwd=1.5)
 
 #mtext(emp.mw.txt, 4, line=1, cex=0.9, col="black")
 grid(col="grey80", lwd=1, lty=2)
 
 #rejection area H0=0, two-sided
 tvalue <- qt(1-crit.alpha/2,df=Inf)
 boundary.low <- mw - tvalue*se
 boundary.up <- mw + tvalue*se
 id.low <- which(sek <= boundary.low)
 id.up <- which(sek >= boundary.up)
 polygon( c(sek[id.low],max(sek[id.low]),sek[id.low][1]), c(dist.ds[id.low],0,0), col="purple", border=NA)
 polygon( c(sek[id.up],max(sek[id.up]),sek[id.up][1]), c(dist.ds[id.up],0,0), col="darkorange", border=NA)
 lines(sek, dist.ds, col="darkcyan", lwd=1.5)
 points(emp.mw,0, pch=23, bg="yellow", col="black", cex=1.3, lwd=1.5)

 #vertical line
 #mean = 0
 lines(c(mw,mw), c(0,max.dist.ds), col="blue", lwd=2)
 #mean = tes
 tes.ds <- dist.ds[sek >= tes][1]
 lines(c(tes,tes), c(0,tes.ds), col="red", lwd=2)
 emp.mw.txt <- eval(substitute(expression(paste(bar(x)," = ",emp.mw)),list(emp.mw=emp.mw)))
 pop.mw.txt <- eval(substitute(expression(paste(mu," = ",tes)),list(tes=tes)))
 legend.cols <- c("red","blue","yellow","purple","darkorange")
 legend("topright", legend=c(paste("true effect size d = ",tes,sep=""),paste("Test H0 = ",mw,sep=""),emp.mw.txt,"rejection area H0","acceptance area H0"), fill=legend.cols, bty="n", cex=0.9)
 
 #plot in case of H0=2 (test against tes)
 sek2 <- sek + tes
 plot(sek2, dist.ds, main="", xlab="estimated effect size", ylab="density", type="n", col="white", bty="l", lwd=1.5)
 grid(col="grey80", lwd=1, lty=2)
 
 #rejection area H0=2, two-sided
 id.low.sek2 <- which(sek2 <= boundary.low)
 id.up.sek2 <- which(sek2 >= boundary.up)
 polygon( c(sek2[id.low.sek2],max(sek2[id.low.sek2]),sek2[id.low.sek2][1]), c(dist.ds[id.low.sek2],0,0), col="purple", border=NA)
 polygon( c(sek2[id.up.sek2],max(sek2[id.up.sek2]),sek2[id.up.sek2][1]), c(dist.ds[id.up.sek2],0,0), col="darkorange", border=NA)
 lines(sek2, dist.ds, col="darkcyan", lwd=1.5)
 points(emp.mw,0, pch=23, bg="yellow", col="black", cex=1.3, lwd=1.5)

 #vertical line
 #mean = 0
 lines(c(tes,tes), c(0,max.dist.ds), col="blue", lwd=2, lty=1)
 #mean = tes
 tes.ds.sek2 <- dist.ds[sek2 >= mw][1]
 lines(c(mw,mw), c(0,tes.ds.sek2), col="red", lwd=2, lty=2)
 legend("topright", legend=c(paste("previous test H0 = ",mw,sep=""),pop.mw.txt,emp.mw.txt,paste("Type S = ",round(typeS,digits),sep=""),paste("Type M = ",round(exagg,digits),sep="")), fill=legend.cols, bty="n", cex=0.9)
 
 #title
 power.txt <- eval(substitute(expression(paste("Power = ",pauer)),list(pauer=round(pauer,digits))))
 mtext(expression(paste("Design Analysis (Type S and Type M error)",sep="")), 3, line=-1.8, cex=1.25, outer=TRUE)  
 mtext(power.txt, 3, line=-3.25, cex=1, outer=TRUE)
}
#call:
#plot.typ.sm(typsm.res.ref, range.dist=c(-30,30), mw=0, emp.mw=d, tes, se)
########################## END OF FUNCTION

