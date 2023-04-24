# file:
# ptII_quan_classicstats_R-index_z-curve.r

# location:
# chap. 4 [4.6.4.1]
# R-Index

# HELPER FUNCTIONS


###### function to calculate R-Index
# origin: spreadsheet-coding1.xls by Schimmack
R.index <- function(dats=NA, pr=TRUE, note=NULL, digits=4)
{
 dats.src <- dats
 if(is.vector(dats)) dats <- data.frame(dats)
 dats.d <- dim(dats)
 if(dats.d[1] == 1) note <- c("only single study, no set of studies")
# alpha to z-score
# lowerbound
 crit.noncentral.z <- qnorm(1-dats[,"alpha"]/2)

# single studies
 pvals <- sapply(seq_along(1:nrow(dats)), function(x) calc.pval.v3(dats[x,]))
 z.scores <- qnorm(1-pvals/2, mean=0, sd=1)
 obs.powers <- pnorm(z.scores, mean=crit.noncentral.z, sd=1) # mean=1.96
 inflation.singles <- dats$success-obs.powers
 R.index.singles <- obs.powers-inflation.singles
 res.single <- data.frame(pvals,z.scores, alpha=dats[,"alpha"],crit.noncentral.z,obs.powers,inflation.singles,R.index.singles)

# aggregate over set of studies
 median.obs.power.set <- median(res.single$obs.powers)
 mean.success.rate.set <- mean(dats$success)
 inflation.set <- mean.success.rate.set-median.obs.power.set
 R.index.set <- median.obs.power.set-inflation.set
 res.set <- data.frame(median.obs.power.set,mean.success.rate.set,inflation.set,R.index.set)

# print
 if(pr)
 {
  res1 <- structure(list("Median (Observed Power)"=median.obs.power.set,
                         "Mean (Success Rate)"=mean.success.rate.set,
   		                   "Inflation"=inflation.set,
    		                 "R-Index"=R.index.set))
  cat("\n    ", "R-Index for a set of studies", "\n\n")
  cat(paste(format(names(res1), width = 15L, justify = "right"), 
            format(res1, digits = digits), sep = " = "), sep = "\n")
  cat("\n\tSide notes:\n")
  cat("\n\t- Statistical Test =",unique(dats.src[,"stat.test"]) ) #was:levels
  cat("\n\t- alpha =",unique(dats[,"alpha"]))
  cat("\n\t- effect =",unique(dats.src[,"type.test"])) #was:levels
  cat("\n\t- no. of studies =",dim(dats.src)[1])
  if (!is.null(note))
  {
   cat("\n\n    ", "NOTE: ", note, "\n\n", sep = "")		 
  } else cat("\n\n") 
 }

return(list(dats=dats.src,res.single=res.single, res.set=res.set))
}
# call:
# rindx <- R.index(dats=dats)
########################## END OF FUNCTION


###### helper function to calculate p-values for R-Index
calc.pval.v3 <- function(v)
{
  with(v, {
    if(type.test == "twoway") fac <- 1 else fac <- 2 #=oneway
    if(stat.test == "z") pval <- (1-pnorm(test.statistic))*fac
    if(stat.test == "t") pval <- ( (1-pt(test.statistic, df.nominator))*fac )
    if(stat.test == "chisq") pval <- 1-pchisq(test.statistic, df.nominator)
    if(stat.test == "F") pval <- 1-pf(test.statistic, df.nominator, df.denominator)
  return(pval)
  })
}
########################## END OF FUNCTION


###### helper function to calculate p-values for R-Index
#calc.pval.v <- function(v)
#{
#  type.test <- v["type.test"][,1]
#  test.statistic <- v["test.statistic"][,1]
#  df.nominator <- v["df.nominator"][,1]
#  df.denominator <- v["df.denominator"][,1]
#  stat.test <- v["stat.test"][,1]
#  if(type.test == "twoway") fac <- 1 else fac <- 2 #=oneway = default
#  calcs <- list("z" = quote( (1-pnorm(test.statistic))*fac ),
#                "t" = quote( (1-pt(test.statistic, df.nominator))*fac ),
#                "chisq" = quote( 1-pchisq(test.statistic, df.nominator) ),
#                "F" = quote( 1-pf(test.statistic, df.nominator, df.denominator) )
#  )
#  id <- which( stat.test == names(calcs) )
#  pval <- eval(calcs[[ id ]])
#  return(pval)  
#}
########################## END OF FUNCTION


###### function to calculate test of insufficient variance (TIVA)
# after Schimmack
TIVA <- function(z.scores=NA, pvals=NA, pr=TRUE, digits=4, type.test="oneway")
{
 if(type.test == "oneway") fac <- 1 else if(type.test == "twoway") fac <- 2 else stop("Neither 'one way' nor 'two way' effect.")
 if(!is.na(z.scores) && !is.na(pvals))
 {
  note <- c("z-score and p-values available. z-scores will be used.\n\n")
 }
 if(length(pvals) > 1 && !is.na(pvals) && is.na(z.scores))
 {
  z.scores <- qnorm(pvals/fac,lower.tail=FALSE)
  note <- c("p-values transformed to z-scores\n\n")
 }
 if(length(pvals) < 2 && is.na(pvals))
 {
   pvals <- (1-pnorm(z.scores))*fac
   note <- c("z-scores transformed to p-values\n\n")
 }   
 OV <- var(z.scores)
 k <- length(z.scores)
 tiva <- OV*(k-1)
 p <- pchisq(tiva, k-1)
 res <- data.frame(tiva,p,OV,k)
 colnames(res) <- c("TIVA","p","OV","k")
 rownames(res) <- ""
 if(pr)
 {
  res1 <- structure(list("Observed Variance"=OV,"TIVA (Chi^2 statistic)"=tiva,"p-value"=p,"df"=k-1,"k"=k))
  cat("\n    ", "TIVA Test for insufficient variance", "\n\n")
  cat(paste(format(names(res1), width = 15L, justify = "right"), 
         format(res1, digits = digits), sep = " = "), sep = "\n")
  if (!is.null(note))
  {
   cat("\n    ", "NOTE: ", note, "\n", sep = "")		 
  } else cat("\n") 
 }
return(list(res=res, scores=data.frame(pvals,z.scores)))
}
# call:
# tiva <- TIVA(pvals=pvals)
########################## END OF FUNCTION
