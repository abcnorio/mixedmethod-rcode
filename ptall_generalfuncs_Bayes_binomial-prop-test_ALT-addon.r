# Euler integral
# https://homepage.tudelft.nl/11r49/documents/wi4006/hyper.pdf
# Rec > Reb > 0

# Euler transformation
# https://mathworld.wolfram.com/EulerTransform.html
# https://mathworld.wolfram.com/EulersHypergeometricTransformations.html
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
  # http://dr-k-lo.blogspot.de/2014/03/the-simplest-way-to-plot-legend-outside.html  
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



###### function to calculate closed form Bayesian A/B Testing after Evan Miller
### closed form exact solution (Evan Miller)
# http://www.evanmiller.org/bayesian-ab-testing.html#binary_ab_derivation

h <- function(a1, b1, a2, b2, loga=FALSE)
{
  # PR(p_B=GR2 > P_A=GR1)
  
  ### closed form exact solution (Evan Miller)
  # http://www.evanmiller.org/bayesian-ab-testing.html#binary_ab_derivation
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


###### function to calculate loss function/ decision rule by Chris Stucchio
# after the closed form Bayesian A/B Testing after Evan Miller
bayes.prop.loss <- function(a1, b1, a2, b2, crit=0.05, loga=TRUE, pr.out=TRUE)
{
  # https://www.chrisstucchio.com/blog/2014/bayesian_ab_decision_rule.html
  # https://www.chrisstucchio.com/blog/2014/bayesian_asymptotics.html
  # http://www.evanmiller.org/bayesian-ab-testing.html#binary_ab_derivation
  
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


################################ old function - NOT RUN and NOT USED ANYMORE
pdf.theta.diff.OLDandOUTDATED <- function(theta, a1, b1, a2, b2, loga=FALSE, ...)
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
        F1.brob(a = b1, b = a1 + b1 + a2 + b2 - 2, b.prime = 1 - a1, c = a2 + b1, x = 1 + theta, y = 1 - theta^2, ...)@x
      #F1(a = 3, b = 4, b.prime = 5, c = 13, x = 0.2, y = 0.4)
    } 
    
    # theta > 0
    if(theta > 0) {
      res <- log(1) - A.l.const + lbeta(a1, b2) + (b1 + b2 - 1)*log(theta) + (a1 + b2 - 1)*log(1 - theta) +
        #log( abs(appellf1(a = b2, b1 = a1 + b1 + a2 + b2 - 2 , b2 = 1 - a2, c = a1 + b2, x = 1 - theta, y = 1 - theta^2)$val) )      
        F1.brob(a = b2, b = a1 + b1 + a2 + b2 - 2 , b.prime = 1 - a2, c = a1 + b2, x = 1 - theta, y = 1 - theta^2, ...)@x
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
################################ END OF FUNCTION

