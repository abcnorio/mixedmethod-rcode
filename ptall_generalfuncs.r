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



# 2020-20-02
# file:
# ptall_generalfuncs.r

# location:
# none
# general functions for various scripts


###### function to calculate Cohen's d(elta)
cohensd <- function(s1, s2, sd.theory=NA, OUT=FALSE)
{
 #s1 = sample 1
 #s2 = sample 2
 n1 <- length(s1)
 xbar1 <- mean(s1)
 sd1 <- sd(s1)
 if(length(s2) == 1)
 {
  if(OUT) cat(paste("\n Cohens d: calculate 'mu_2 vs. mu_1' (diff = mu_1-",s2,")\n\n",sep=""))
  delta <- xbar1 - s2
  res <- data.frame(delta/sd1, s2)
  names(res) <- c("d|one sample","s2 [scalar]")
  rownames(res) <- ""
  n2 <- 0
 } else
 {
  n2 <- length(s2)
  xbar2 <- mean(s2)
  sd2 <- sd(s2)
  # calculate delta based on sample2 minus sample1
  delta <- xbar2-xbar1
  #two different ways to calculate 'pooled' sd
  sd.pool1 <- sqrt((sd1^2 + sd2^2)/2)
  sd.pool2 <- sqrt(((n1-1)*sd1^2 + (n2-1)*sd2^2)/(n1+n2-2))
  res <- c(delta/sd.pool1, delta/sd.pool2)
  names(res) <- c("d|mean sd","d|pooled sd")  
  if(OUT)
  {
    cat("\n Cohens d: calculate 'mu_2 vs. mu_1' (diff = mu_2-mu_1)\n\n")
    cat(" Difference of mean sd vs. pooled sd:\t",all.equal(res[1], res[2]),"\n")
  }  
 }
 if(!is.na(sd.theory))
 {
  d3 <- delta/sd.theory
  res <- c(res,d3)
  names(res)[3] <- c("d|theory sd")
 }
 #https://www.statisticshowto.com/cohens-d/
 n <- n1+n2
 if(n <= 50)
 {
   res.l <- length(res)
   corr.fac <- ( (n-3)/(n-2.25) ) * sqrt( (n-2)/n ) 
   res <- c(res,res[res.l]*corr.fac)
   names(res)[res.l+1] <- c("d corrected|N<50")
 }
 if(OUT)
 {
   cat(" Sample sizes n1+n2  <= 50 (corrected d):",n <= 50,"\n\n")
   print(res)
 }   
return(res)
}
#call:
#cohensd(s1, s2)
#cohensd(s1, s2, sd.theory=NA)
#
#case: against scalar (one sample)
#n <- 30
#cohensd(s1=rnorm(n=n, mean=10, sd=2.5),s2=9)
#cohensd(s1=rnorm(n=n, mean=10, sd=2.5),s2=rnorm(n=n, mean=11, sd=2.8))
#n <- 20
#set.seed(0000)
#res1 <- cohensd(s1=rnorm(n=n, mean=10, sd=2.5),s2=rnorm(n=n, mean=11, sd=2.8))
#res2 <- cohensd(s1=rnorm(n=n, mean=10, sd=2.5),s2=rnorm(n=n, mean=11, sd=2.8), OUT=TRUE)
#res1
#res2
########################## END OF FUNCTION


########################## FUNCTION: calculate descriptive statistics
desc.stat <- function(x, ci=0.95)
{
# expects vector x
  nobs <- length(x)
  nas <- sum(is.na(x))
  x <- na.omit(x)
  nobs.onas <- length(x)
  summe <- sum(x)
  mw <- mean(x)
  gm <- prod(x)^(1/nobs.onas)               # geometrical mean
  hm <- nobs.onas/(sum(1/x))                # harmonical mean
  vari <- var(x)
  sabw <- sd(x)
  if(mw != 0) vc <- sabw/mw else vc <- NA   # >0??Bortz S.44, vc/variation coefficient
  med <- median(x)
  medad <- mad(x)
  mini <- min(x)
  maxi <- max(x)
  spann <- maxi-mini
  ad <- sum(abs(x-mean(x)))/nobs.onas              # average deviation
  schief <- sum((x - mw)^3)/(nobs.onas * sabw^3)   # skewness
  kurto <- sum((x - mw)^4)/(nobs.onas * sabw^4) -3 # curtosis #Bortz S.46
  quan1 <- quantile(x, prob=0.25)
  quan3 <- quantile(x, prob=0.75)
  iqr <- quan3-quan1
  se.mean <- sqrt(var(x)/nobs.onas)
  t.val <- qt((1 - ci)/2, nobs.onas - 1)
  lcl.mw <- mw + se.mean * t.val
  ucl.mw <- mw - se.mean * t.val
  v <- c(nobs, nas, nobs.onas, summe, mw, gm, hm, vari, sabw, vc,
         med, medad, mini, maxi, spann, ad, schief, kurto, quan1, quan3,
         iqr, se.mean, lcl.mw, ucl.mw)
res <- t(data.frame(v=v))
return(res)
}
#call:
#desc.stat(x=numericvector)
########################## END OF FUNCTION


########################## FUNCTION: descriptive statistics wrapper
describes <- function(d)			  
{
 vnames <- c("N (all)", "NA", "N (no NA)", "sum", "mean",
             "g mean", "h mean", "var", "sd", "vc",
             "med", "mad", "min", "max", "range", "mean dev",
             "skewness", "kurtosis", "1.Quantil", "3.Quantil", "IQR",
             "SE mean", "CI low mean", "CI up mean")
 if(is.matrix(d) || is.data.frame(d))
 {
  d.res <- apply(d, 2, desc.stat)
 } else
 if(is.numeric(d))
 {
   d.res <- t(desc.stat(d))
 } else
 {
   stop("neither matrix nor dataframe nor numeric vector...")
 }   
 rownames(d.res) <- vnames
return(t(d.res))
}
#call:
#describes(d=titanic.src[,c("age","fare")])
########################## END OF FUNCTION


###### function  calculate fivenum() and return it with names
fivenum.wn <- function(v)
{
  fivenumnams <- c("Min","1st Qu.","Median","3rd Qu.","Max")
  fn <- fivenum(v)
  names(fn) <- fivenumnams
return(fn)
}
# call:
# fivenum.wn(runif(100))
########################## END OF FUNCTION


########################## FUNCTION: wrapper for 'fivenum' with labels
fivenum2 <- function(d)
{
 res <- fivenum(d)
 names(res) <- c("minimum","lower-hinge","median","upper-hinge","maximum")
return(res)
}
#call:
#fivenum2(d=rnorm(n=100,mean=4,sd=3))
########################## END OF FUNCTION


########################## QCA related functions

### function AQUAD reverse writing style of truth tables
aquadstyle.rev <- function(ttab.aqd, pres.cnam=TRUE)
{
  rnam <- rownames(ttab.aqd)
  convback <- FALSE
  if(dim(ttab.aqd)[1] == 1)
  {
    convback <- TRUE
    ttab.aqd <- rbind(ttab.aqd,tolower(ttab.aqd),toupper(ttab.aqd))
  }  
  
  ttab.aqd[ttab.aqd == "--"] <- NA
  cnam <- colnames(ttab.aqd)
  ttab.aqd <- as.data.frame(ttab.aqd)
  res <- ttab.aqd == toupper(as.matrix(ttab.aqd))
  if(pres.cnam) colnames(res) <- cnam else colnames(res) <- LETTERS[1:dim(res)[2]]
  rownames(res) <- rnam
  return(as.data.frame(res)) 
}
#call:
#aquadstyle.rev(tab.aqd)


### function AQUAD writing style of truth tables
aquadstyle.tt <- function(ttab.ne, pres.cnam=TRUE)
{
  cn <- colnames(ttab.ne)
  cn.laenge <- length(cn)
  if(pres.cnam) cn.aqd <- cn else cn.aqd <- LETTERS[1:cn.laenge]
  cn.tab <- data.frame(Colnames=cn,Colnames.AQD=cn.aqd)
  res <- sapply(seq_along(cn), function(i)
  {
    ifelse(ttab.ne[,i] == 1, toupper(cn.aqd[i]), tolower(cn.aqd[i]))
  })
  if(pres.cnam) colnames(res) <- cn else colnames(res) <- cn.aqd
  rownames(res) <- rownames(ttab.ne)
  return(as.data.frame(res))
}
#call:
#aquadstyle.tt(tab.ne)
 
##################
aquadstyle.ne2tt <- function(ttab.ne, pres.cnam=TRUE)
{
  res <- ifelse(tab.ne == 1, TRUE, FALSE)
  if(!pres.cnam) colnames(res) <- LETTERS[1:dim(res)[2]]
  return(res)  
}  
# call:
# aquadstyle.ne2tt( aquadstyle.rev(krook.discuss)+0)
#################

### function extract and print prime implicants from QCA object and generate list
print.pis <- function(bm, caseandpims=TRUE)
{
 cat(paste("\n###################################\n### Results Boolean minimization\n",sep=""))
 cat(paste("\n### Outcome:\t",bm$tt$options$outcome,"\n",sep=""))
 cat(paste("### Criterium:\t",!bm$tt$options$neg.out,"\n",sep=""))
 cat(paste("### Conditions:\t", paste(bm$tt$options$conditions, collapse=" | "),"\n",sep=""))
 cat(paste("\n### Output of the created minimization object:\n",sep=""))
 print(bm)
 cat(paste("### Fundamental base products:\n\n",sep=""))
 print(bm$initials)
 cat(paste("\nsimplifying assumptions (if set):\n\n",sep="")) 
 print(bm$SA)

 pi.all <- paste(attr(bm$PIchart,"dimnames")[[1]],collapse=" + ") 
 cat(paste("### Primary implicants (charts, no reduction):\n\n",pi.all,"\n",sep=""))
 print(bm$PIchart)
 bmterm <- paste(colnames(bm$pims), collapse=" + ")
 cat(paste("### Solutions:\n\n",sep=""))
 print(lapply(bm$solution,function(i) paste(i,collapse=" + "))[[1]])
 if(caseandpims)
 {
  cat(paste("\n### Case and primary implicants:\n\n",bmterm,"\n\n",sep=""))
  print(bm$pims)
 }
 bmterm.ess <- paste(bm$essential, collapse=" + ")
 cat(paste("\n### Essential implicants:\n\n",bmterm.ess,"\n\n",sep=""))
 cat(paste("###################################\n\n",sep=""))
}
#call:
#print.pis(krook.mini)

##################
primtab.desc <- function(srctab, primtab, outcome, norownames=FALSE, pr.out=TRUE)
{  
  pichart <- primtab$PIchart
  pnamen <- colnames(srctab)
  # pichart
  # pnamen
  pnamen.l <- length(pnamen)
  pichart.d <- dim(pichart)
  pidisc.tab <- matrix(data=NA, ncol=pnamen.l, nrow=pichart.d[1])
  colnames(pidisc.tab) <- pnamen
  # pidisc.tab
  
  for(i in 1:pichart.d[1])
  {
    pi.sols <- dimnames(pichart)[[1]]
    # pi.sols
    pi.temp <- pi.sols[i]
    # pi.temp
    pi.temp.parts <- unlist(strsplit(pi.temp,c("\\*")))
    neg.caseIDs <- grep("~", pi.temp.parts, fixed=TRUE)
    pi.temp.parts <- gsub("~", "", pi.temp.parts, fixed=TRUE)
    present.IDs <- pnamen[pnamen %in% pi.temp.parts]
    pi.temp.parts <- toupper(pi.temp.parts)
    pi.temp.parts[neg.caseIDs] <- tolower(pi.temp.parts[neg.caseIDs])
    pidisc.tab[i,present.IDs] <- pi.temp.parts
    # pidisc.tab
  }  
  pidisc.tab[is.na(pidisc.tab)] <- "--"
  if(primtab$tt$options$neg.out) neg.out <- TRUE else neg.out <- FALSE # negative outcome?
  cat("\nnegative outcome: ",neg.out,"\n\n")
  outcome.n <- gsub("~", "", outcome, fixed=TRUE)
  outID <- which(outcome.n == pnamen)
  
  if(pichart.d[1] == 1) # in case of one row
  {
    pidisc.tab.res <- matrix(data=c(pidisc.tab[,-c(outID)],pidisc.tab[,outID]), nrow=pichart.d[1])
    colnames(pidisc.tab.res) <- c( pnamen[-c(outID)], "" )
    rownames(pidisc.tab.res) <- 1
  } else pidisc.tab.res <- cbind(pidisc.tab[,-c(outID)],pidisc.tab[,outID])
  colnames(pidisc.tab.res)[pnamen.l] <- paste("[ ",outcome.n," ]",sep="")
  if(neg.out)
  {
    pidisc.tab.res[,pnamen.l] <- tolower( rep(outcome.n, pichart.d[1]) )
  } else pidisc.tab.res[,pnamen.l] <- toupper( rep(outcome.n, pichart.d[1]) )
  if(norownames) rownames(pidisc.tab.res) <- 1:pichart.d[1] else rownames(pidisc.tab.res) <- pi.sols
  if(pr.out) print(pidisc.tab.res, quote=FALSE)
  return(pidisc.tab.res)
}
# call:
# primtab.desc(srctab=ttab.red.ne, primtab=t.mini, norownames=FALSE, pr.out=TRUE)
# primtab.desc(srctab=ttab.red.ne, primtab=t.mini, outcome=outcome, norownames=FALSE, pr.out=TRUE)
########################## END OF QCA





########################## HCA, MDS, prototypes, etc.

########################## FUNCTION: distfull
distfull <- function(dm)
{
# http://www.wiwi.uni-bielefeld.de/~frohn/Mitarbeiter/Handl/mvarfunktionen.html
# http://www.wiwi.uni-bielefeld.de/~frohn/Mitarbeiter/Handl/mvarfunktionen.html
# dm = distance matrix - output of dist()
# modified by LG 2013
 n <- attr(dm, "Size")
 full <- matrix(0, n, n)
 full[lower.tri(full)] <- dm
 full <- full + t(full)
 namen <- attr(dm, "Labels")
 dimnames(full) <- list(namen,namen)
 return(full)    
}
#call:
#distfull(distancematrix)
########################## END OF FUNCTION


########################## FUNCTION: optcut
optcut <- function(pm)
{
# first 2004, 2013
# references: Oldenbürger (1981)
  sp <- sort(pm[lower.tri(pm)])
  spcc <- matrix(nrow=length(sp), ncol=2)
  mat <- matrix(data=NA, nrow=length(sp), ncol=length(sp))
  mat[lower.tri(mat)] <- 1
  mat[upper.tri(mat)] <- 0
  diag(mat) <- 0
  cc <- apply(mat[,-dim(mat)[2]], 2, function(x) cor(x, sp))
  spcc <- data.frame(sp=sp[-length(sp)], cc=cc)
  return(spcc)
}
#call:
#vektorOpt <- optcut(rawd.d.full)
########################## END OF FUNCTION


########################## FUNCTION: makeZO
makeZO <- function(dm.full, vektorOpt, rigid=TRUE)
# first 2004, 2013
# references: Oldenbürger (1981)
# vektorOpt = output from optcut()
# dm.full = full distance matrix, output from distfull()
{
  maxcc <- max(vektorOpt[,"cc"])
  cutoff <- vektorOpt[(vektorOpt[,2] == maxcc),1]
  if(rigid) ids <- which(dm.full <= cutoff) else ids <- which(dm.full < cutoff)
  dm.full[ids] <- 1
  dm.full[-ids] <- 0
  protovec <- sort(apply(dm.full,2,sum),decreasing=TRUE)
  return(list(prototype.mat=dm.full,
              maxcc=maxcc,
              cutoff=cutoff,
              protovec=protovec))
} 
#call:
#outZO <- makeZO(dm.full=rawd.d.full, vektorOpt=vektorOpt)
########################## END OF FUNCTION


########################## FUNCTION: plot optimal cut vector
plot.optcut <- function(vektorOpt=NULL, outZO=NULL,
                        TITLE="",
						SUB="Optimal cut through a proximity matrix",
						marking=TRUE, texte=TRUE,
						...)
{
 xlim <- range(min(vektorOpt[,"sp"]),max(vektorOpt[,"sp"]))
 ylim <- range(0,1)

 par(mar=c(5,5,4,2))
 par(oma=c(2,1,1,1))
 par("cex.axis"=0.8)
 
 plot(xlim, ylim, type="n", xlab="", ylab="", axes=F, lty=2, bty="n", main="")
 grid(col="grey80", lwd=1.2, lty=1)

 axis(side = 1, pretty(xlim), tck =- .02, labels=NA, line=.6)
 axis(side = 2, pretty(ylim), tck = -.02, labels=NA, line=.6)
 axis(side = 1, lwd = 0, line = .4)
 axis(side = 2, lwd = 0, line = .4, las = 1)

 mtext((TITLE), 3, line=2, cex=1.5)
 mtext(SUB, 3, line=.48, cex=1.1)
 mtext("value", 1, line=3, cex=1)
 mtext("r", 2, line=4, cex=1)

 if(marking)
 {
  abline(v=outZO$cutoff, col="orange", lty=2)
  abline(h=outZO$maxcc, col="orange", lty=2)
 }
 
 redvektorOpt <- vektorOpt[!vektorOpt[,"cc"] == outZO["maxcc"],]
 if(texte) text(redvektorOpt, labels=round(redvektorOpt[,2],2),cex=0.6, pos=1, col="blue", font=1)

 points(vektorOpt, pch=21, col="blue", bg="orange")
 points(outZO$cutoff, outZO$maxcc, pch=22, col="darkred", bg="green", cex=1.6)

 mtext(paste("Optimal cut at value = ",round(outZO$cutoff,2),"with r = ",round(outZO$maxcc,2),sep=" "), 1, line=4.4, cex=.9, col="red", adj=0)
 text(outZO$cutoff, outZO$maxcc, labels=round(outZO$maxcc,2), cex=1, pos=3, col="violetred2", font=1)

}
#call:
#plot.optcut(vektorOpt, outZO, infile.short)
########################## END OF FUNCTION


########################## FUNCTION: plot MDS 2D with prototype(s) and relationships
plot.prototype2D <- function(rawd.d2=NULL, outZO=NULL, TITLE="", SUB="Multidimensional scaling (2D) + Prototype(s)",
                           expand=TRUE, fac=1.4, labbrev=6, pos=3,
                           linien=TRUE, punkte=TRUE, texte=TRUE,
                           prototypeplot=TRUE,
						   plinien=TRUE, ppunkte=TRUE, ptexte=TRUE,
                           labbrevMDSplot=FALSE, labelmds=NA,
                           axnam=c("axis I","axis II"), 
						   ...)
{
 rawd.d2 <- cmdscale(rawd.d, k=2, eig=FALSE, x.ret=TRUE, add=FALSE)
 if(is.na(labelmds)) labelmds <- dimnames(rawd.d2$points)[[1]]
 if(labbrevMDSplot) labelmds <- abbreviate(labelmds, labbrev)
 
 xlim <- range(rawd.d2$points[,1])
 ylim <- range(rawd.d2$points[,2])
 if(expand)
 {
  xlim <- xlim*fac
  ylim <- ylim*fac
 }

 par(mar=c(5,5,4,2))
 par(oma=c(2,1,1,1))
 par("cex.axis"=0.8)
 
 plot(xlim, ylim, type="n", xlab="", ylab="", axes=F, lty=2, bty="n", main="")
 rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col="grey90", border=NA)
 grid(col="white", lwd=1.2, lty=1)

 axis(side = 1, pretty(xlim), tck =- .02, labels=NA, line=.6)
 axis(side = 2, pretty(ylim), tck = -.02, labels=NA, line=.6)
 axis(side = 1, lwd = 0, line = .4)
 axis(side = 2, lwd = 0, line = .4, las = 1)

 mtext((TITLE), 3, line=2, cex=1.5)
 mtext(SUB, 3, line=.48, cex=1.1)
 mtext(axnam[1], 1, line=4, cex=1)
 mtext(axnam[2], 2, line=4, cex=1)

# insert relationships i.e. lines 
 if(linien)
 {
  combi <- t(combn(dimnames(rawd.d2$points)[[1]], 2))
  teall <- cbind(rawd.d2$points[combi[,1],], rawd.d2$points[combi[,2],])

  pmat <- outZO$prototype.mat
  pmat.lower <- pmat[lower.tri(pmat)]
  teall <- teall*pmat.lower 

  rownames(teall) <- 1:dim(teall)[1]
  teall <- data.frame(combi, pmat.lower, teall)
  colnames(teall) <- c("d1","d2","v","x","y","x.1","y.1")
  teall.red <- teall[teall$v != 0,]
# draw relationships between points
  segments(teall.red[,4],teall.red[,5],teall.red[,6],teall.red[,7], lty=3, col="olivedrab")
# mark points
  if(punkte) points(rawd.d2$points, col="darkblue", bg="orange", pch=21, cex=1.4)
# write names
  if(texte) text(rawd.d2$points[,1], rawd.d2$points[,2], labels=labelmds, cex=1.1, col="darkred", pos=pos)
 }

# relationships only of prototypes with others and with each other
 if(prototypeplot)
 {
  protovec <- outZO$protovec
  protonam <- names(protovec)[protovec == max(protovec)]
  pteall <- teall[unique(c(which(teall$d1 %in% protonam), which(teall$d2 %in% protonam))),]
  pteall.red <- pteall[pteall$v != 0,]
  
 # in case of more than one prototype
  mds.hprot.points <- rawd.d2$points[protonam,]

# draw relationships between points (only prototypes)
  if(plinien) segments(pteall.red[,4],pteall.red[,5],pteall.red[,6],pteall.red[,7], lty=3, col="blue")
# mark points (only prototypes)
  if(ppunkte) points(mds.hprot.points, col="red", bg="pink", pch=21, cex=1.6)
# write names (only prototypes)
  if(ptexte) text(mds.hprot.points[,1], mds.hprot.points[,2], labels=protonam, cex=1.1, col="blue", pos=pos)
 }
} 
#call:
#plot.prototype2D(rawd.d=rawd.d, outZO=outZO, TITLE="Leadership (8 dimensions)", fac=1.6)
########################## END OF FUNCTION


########################## FUNCTION: plot MDS 3D with prototype(s) and relationships 
plot.prototype3D <- function(rawd.d3=NULL, outZO=NULL, TITLE="", SUB="Multidimensional scaling (3D) + Prototype(s)",
                             # draw lines, points, labels 
							 linien=TRUE, punkte=TRUE, texte=TRUE,
						     # draw lines, points, labels (only prototypes)
							 prototypeplot=TRUE, plinien=TRUE, ppunkte=TRUE, ptexte=TRUE,
							 axnam=c("axis I","axis II","axis III"), 
							 ANGLE=45, box=FALSE, labelmds=NA,
							 ...)
{			   

 par(mar=c(5,5,4,2))
 par(oma=c(2,1,1,1))
 par("cex.axis"=0.8)
 
 #rawd.d3 <- cmdscale(rawd.d, k=3, eig=TRUE, add=TRUE, x.ret=TRUE)
 
 mds3D <- scatterplot3d(rawd.d3$points, main="", sub="",
						xlab=axnam[1], ylab=axnam[2], zlab=axnam[3],
						col.axis="blue", col.grid="lightblue",
						type="h", lwd=1, pch=21, bg="pink", color="darkred",
						angle=ANGLE, box=box)
 mds3.coords <- mds3D$xyz.convert(rawd.d3$points)              

 mtext(TITLE, 3, line=2, cex=1.5)
 mtext(SUB, 3, line=.252, cex=1.1)

 if(linien)
 {
  coordsxy <- as.data.frame(mds3.coords)
  rownames(rawd.d3$points) <- rownames(coordsxy) <- labelmds
  combi <- t(combn(dimnames(coordsxy)[[1]], 2))
  teall <- cbind(coordsxy[combi[,1],], coordsxy[combi[,2],])
# plot only lines based on prototype matrix ie. connections 0/1
  pmat <- outZO$prototype.mat
  pmat.lower <- pmat[lower.tri(pmat)]
  teall <- teall*pmat.lower 
# draw relationships between points
  segments(teall[,1],teall[,2],teall[,3],teall[,4], lty=3, col="olivedrab")
# mark points
  if(punkte) points(mds3.coords, col="darkblue", bg="orange", pch=21, cex=1.4)
# write names 
  if(texte) text(mds3.coords$x, mds3.coords$y, labels=labelmds, cex=.9, pos=3, col="darkblue")
 }
 
# relationships only of prototypes with others and with each other
 if(prototypeplot)
 {
  protovec <- outZO$protovec
  protonam <- names(protovec)[protovec == max(protovec)]
  proto.labelmds <- as.character(labelmds[labelmds %in% protonam])
  id <- which(labelmds %in% protonam)
 
  protorels <- outZO$prototype.mat[protonam,]
  as.vector(protorels)
  protorels.mat <- data.frame(d1=rep(rownames(protorels),dim(protorels)[2]),
                              d2=rep(colnames(protorels),each=dim(protorels)[1]),
 		  	                 v=as.vector(protorels))
  protorels.mat.red <- protorels.mat[which(protorels.mat[,"v"] != 0),]
  protorels.coordsxy <- do.call("rbind", lapply(seq_along(1:dim(protorels.mat.red)[1]), function(i)
     {
      data.frame(coordsxy[protorels.mat.red$d1[i] == labelmds,],
                 coordsxy[protorels.mat.red$d2[i] == labelmds,])
     }))
  protorels.mat.red <- data.frame(protorels.mat.red, protorels.coordsxy)
  protorels.mat.red
# draw relationships between points (only prototypes)
  if(plinien) segments(protorels.coordsxy[,1],protorels.coordsxy[,2],protorels.coordsxy[,3],protorels.coordsxy[,4], lty=3, col="orange")
# mark points (only prototypes)
  if(ppunkte) points(coordsxy[protonam,], col="red", bg="pink", pch=21, cex=1.6)
# write names (only prototypes)
  if(ptexte) text(mds3.coords$x[id], mds3.coords$y[id], labels=labelmds[id], cex=.9, pos=3, col="red") 
 }
}
#call:
#plot.prototype3D(rawd.d, outZO, TITLE="Leadership (8 dimensions)", axnam=axnam, ANGLE=26, box=FALSE)
########################## END OF FUNCTION


########################## FUNCTION: eigenvalues MDS with own theme
plot.eig.mds <- function(sdata, TITLE="", SUB="", ...)
{
 xlim <- range(1,length(sdata$eig))
 ylim <- range(sdata$eig)
 
 par(mar=c(5,5,4,2))
 par(oma=c(2,1,1,1))
 par("cex.axis"=0.8)
 
 plot(sdata$eig, main="", xlab="", ylab="", cex.lab=0.8, cex.axis=0.8, bty="n", axes=F)

 axis(side = 1, pretty(xlim), tck =- .02, labels=NA, line=.6)
 axis(side = 2, pretty(ylim), tck = -.02, labels=NA, line=.6)
 axis(side = 1, lwd = 0, line = .4)
 axis(side = 2, lwd = 0, line = .4, las = 1)
 
 mtext(TITLE, 3, line=2, cex=1.5)
 mtext(SUB, 3, line=.252, cex=1.1)

 #rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col="grey90", border=NA)
 #abline(v=pretty(xlim), col="white")
 #abline(h=pretty(ylim), col="white")
 grid(col="grey80", lwd=1.2, lty=1)
 mtext("index", 1, line=4, cex=1)
 mtext("value", 2, line=4, cex=1)
 
 lines(1:length(sdata$eig),sdata$eig, col="orange1")
 points(1:length(sdata$eig),sdata$eig, col="steelblue", pch=20)
}
#call:
#plot.eig.mds(sdata=rawd.d2, TITLE="Leadership (8 dimensions)", SUB="Eigenvalues (Multimensional scaling)")
########################## END OF FUNCTION

########################## END OF HCA, MDS, prototypes, etc.


########################## FUNCTION: Correlation and p-values
corp <- function(tab, metho="pearson", nk=4)       # expects data.frame
{
 res <- vector("list", length=2)
 names(res) <- c("r", "p")
 res[[1]] <- cor(tab, method=metho)
 n <- length(res[[1]])
 res[[2]] <- 2 * (1 - pt(abs(res[[1]]) / (sqrt(1-res[[1]]^2)) * sqrt(n-2), n-2,lower.tail=TRUE))
 diag(res[[2]]) <- NA
 return(res)
}
#call:
#corp(tab)
########################## END OF FUNCTION


########################## FUNCTION: print without "...e+/-..."
printcorp <- function(p, nk=3)
{
 p <- ifelse(p < 1e-04, 0, p)
 print( round(p, digits=nk) )
 invisible()
}
#call:
#printcorp(p)
########################## END OF FUNCTION

