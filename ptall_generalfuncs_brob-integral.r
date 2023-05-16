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



# 2020-10-12
# file:
# ptall_generalfuncs_brob-integral.r

# location:
# none
# general functions for various scripts
# handle brob numbers easier
# calculate integrals for brob numbers


###### function to convert a brob list to a brob vector
#
list2vec.brob <- function(list.brob, check.list=FALSE)
{
  if(!is.list(list.brob)) stop("Input is not a list.")
  if(check.list)
  {
    if(any(unlist(lapply(list.brob, function(x) attr(x, "class"))) != "brob"))
    {
      stop("There are non-brob elements in the list. Stopping.")
    }  
  }
  vec.brob <- brob(unlist(lapply(list.brob,getX)),unlist(lapply(list.brob,getP)))
  return(vec.brob)
}
# call:
# list2vec.brob( lapply(c(1:10),as.brob))
########################## END OF FUNCTION


###### function to replace '%*%' scalarproduct that does not work for brob objects
#
#try:
#as.brob(1:10) %*% as.brob(11:20)
#
scalarprod.brob <- function(c1,c2)
{
  # check required?
  # if( any(c(attr(c1,"class"),attr(c2, "class")) != "brob") ) stop("Elements not of class 'brob'. Stopping.")
  return( sum(c1*c2) )
}
# call:
# as.brob(c(1:10) %*% c(11:20))
# scalarprod.brob(as.brob(1:10), as.brob(11:20))
# scalarprod.brob(as.brob(1:10), c(11:20))
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


