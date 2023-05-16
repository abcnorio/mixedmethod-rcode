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



# file:
# ptII_quan_Bayes_Fisher_LadyBristol-BUGS_helpfuncs.r

# location:
# chap. 6 [6.13.5]
# Fisher reloaded — mehr Tee

# HELPER FUNCTIONS



###### function to do Lady Muriel experiment via Bayes-Theorem
ladymuriel.BT <- function(pr1, pr2, si, Ni, prout=TRUE)
{
  pr1pr2.sum <- pr1 + pr2
  if(pr1pr2.sum != 1)
  {
    pr1.adj <- pr1/pr1pr2.sum
    pr2.adj <- pr2/pr1pr2.sum
    adj <- TRUE
  } else
  {
    pr1.adj <- pr1
    pr2.adj <- pr2
    adj <- FALSE
  }  
  
  BF.pr12 <- pr1.adj/pr2.adj
  BF.pr21 <- pr2.adj/pr1.adj
  
  const <- choose(Ni,si)
  fs <- Ni - si
  
  L1 <- const * pr1.adj^si * (1-pr1.adj)^(fs)
  L2 <- const * pr2.adj^si * (1-pr2.adj)^(fs)
  
  evi <- pr1.adj*L1 + pr2.adj*L2
  
  LR12 <- L1/L2
  LR21 <- L2/L1
  
  post1 <- pr1.adj * L1 / evi
  post2 <- pr2.adj * L2 / evi
  
  postOR12 <- post1/post2
  postOR21 <- post2/post1
  
  # update post OR by BF and L
  postOR12.alt <- pr1.adj/pr2.adj * LR12
  stopifnot(postOR12.alt == BF.pr12*LR12)
  
  if(adj) note <- c("Prior values adjusted to sum up to 1") else note <- NULL
  res <- structure(list(prior1=pr1.adj,
                        prior2=pr2.adj,
                        BF.pr12=BF.pr12,
                        BF.pr21=BF.pr21,
                        const=const,
                        successes=si,
                        failures=fs,
                        trials=Ni,
                        L1=L1,
                        L2=L2,
                        evidence=evi,
                        LR12=LR12,
                        LR21=LR21,
                        post1=post1,
                        post2=post2,
                        postOR12=postOR12,
                        postOR21=postOR21,
                        note=note)
  )
  if(prout)
  {
    cat("\nLady Muriel via Bayes Theorem\n\n")
    res[c("method", "note")] <- NULL
    cat(paste(format(names(res), width = 15L, justify = "right"), 
              format(res, digits = digits), sep = " = "), sep = "\n")
    if (!is.null(note)) 
      cat("\n", "NOTE: ", note, "\n\n", sep = "")
    else cat("\n")
    invisible(x)    
  } else return(res)
}
# call:
# ladymuriel.BT(pr1=0.6, pr2=0.9, si=8, Ni=8)
# res <- ladymuriel.BT(pr1=0.6, pr2=0.9, si=8, Ni=8, prout=FALSE)
########################## END OF FUNCTION


###### function to plot s/N
plot.siNi <- function(theta, beta.dens, pbl.dens, pjc.dens, a.prior=NA, b.prior=NA, critbelo=1e-50, PRnos=1:3)
{
 par(oma=c(2,1,1,1), "cex.axis"=1, bty="l")
 plot(theta, beta.dens,type="l", col="red", pre.plot=grid(), bty="n", xlab=expression(paste(theta)), ylab="Density", cex.lab=1.2, lwd=2)
 lines(theta, pbl.dens, col="steelblue", lty=2, lwd=2)
 if(is.nan(mean(pjc.dens)) || is.infinite(mean(pjc.dens[!is.nan(pjc.dens)])))
 {
  cat("\npjc has not enough valid values and won't be printed, please see here:\n\n")
  print(pjc.dens)
  cat("\n")
  PRnos <- c(1,2)
 } else
 {
  lines(theta, pjc.dens, col="skyblue", lty=2, lwd=2)
 }
 if(!is.na(a.prior) & !is.na(b.prior))
 {
  lines(theta, dbeta(theta, shape1=a.prior, shape2=b.prior), col="violetred3", lwd=2, lty=3)
  PRnos <- c(PRnos,4)
 }
 mtext("Lady tea taste Beta update", outer=TRUE, line=-2, cex=1.5, side=3)
 
 par(fig=c(0,1,0,1), oma=c(1,0,0,0), mar=c(0,0,0,0), new=TRUE)
 plot(1, type="n", bty="n", xaxt="n", yaxt="n")
 categs <- c("Beta update", "UMS (pbl)", "UMS (pjc)","prior beta")
 colos <- c("red","steelblue","skyblue","violetred3")
 legend("bottom", legend=categs[PRnos], lty=1, lwd=2, xpd=TRUE, horiz=TRUE, col=colos[PRnos], bty="n", cex=.9)
}
# call:
# plot.siNi(theta, beta.dens=beta.dens, pbl.dens=muriel.pbl, pjc.dens=muriel.pjc)

# circumvent to write our own HDI code by creating a proper density object and use 'hdi' from 'HDInterval'
hdi.densi <- function(theta, densis)
{
 require(HDInterval)
 densi <- list(x=theta, y=densis, bw=NA, n=length(densis), call=NA, data.name="density over theta", has.na=FALSE)
 attr(densi, "class") <- "density"
 return(hdi(densi))
}
# call:
# hdi.densi(theta=theta, densis=beta.dens)
########################## END OF FUNCTION


###### function to calculate MAP
MAP <- function(theta, dens)
{
 theta[dens == max(dens)]
}
# call:
# MAP(theta, dens)
########################## END OF FUNCTION


###### function to collect steps to run a BUGs model from R with BRugs
# taken and adopted from
# http://www.di.fc.ul.pt/~jpn/r/bugs/bugs_tutorial.html  
run.model <- function(model, samps, dats=list(), chainL=1e+5, burning=0.10, 
                      initfun=NULL, numC=3, thin=1)
{
  # cL = chainLength
  # thin = thinning
  # burning = burn in steps
  # samps = samples
  
  writeLines(model, con="model.txt")
  writeLines(dats, con="data.txt")
  modelCheck("model.txt")
  modelData(fileName="data.txt")
  modelCompile(numChains=numC)
  
  if( is.null(initfun) )
  {
	# randomly inititalize model generation
    modelGenInits()
  } else {
    for(chain in 1:n.chains)
	{
	  # use user data als initial starting point
      modelInits(bugsInits(initfun))
    }
  }
  
  # drop burn ins
  modelUpdate(chainL * burning)
  # keep record
  samplesSet(samps)
  # thinning
  samplesSetThin(thin)
  # randomly initialize a chain
  modelUpdate(chainL)
  # thin = 1 = k = samples used of every k_th iteration for inference
}
# call:
# run.model(model=modelstrng, samples=c("post","y","p","n"), dats=dats)
########################## END OF FUNCTION

