# file:
# ptII_quan_Bayes_HMC_helpfuncs.r

# location:
# chap. 10 [10.1]
# Fallbeispiel quantitative Textanalyse

# HELPER FUNCTIONS


###### function to simulate bivariate normal distribution using rnorm()
bivariate_normal <- function(theta, n) {
  mu1 <- theta[1]
  sigma1 <- theta[2]
  mu2 <- theta[3]
  sigma2 <- theta[4]
  x <- rnorm(n/2, mean=mu1, sd=sigma1)
  y <- rnorm(n/2, mean=mu2, sd=sigma2)
  data.frame(x, y)
}
# call:
# bivariate_normal(theta, n)
########################## END OF FUNCTION


###### function to simulate bivariate normal distribution
bivarnorm <- function(Nsamptot=400,
                      mu1=0, mu2=0,
                      s1=2,  s2=3,
                      rho=-0.5,
                      method=c("manual"),# "MASS"
                      plotty=TRUE,
                      radius=sqrt(qchisq(.5,2)),
                      seed=9876)
{
  set.seed(seed)
  Nsamp <- Nsamptot/2
  sigma <- matrix(data=c(s1^2, s1*s2*rho, s1*s2*rho, s2^2),2)
  #sigma
  if(method=="manual")
  {
    M <- t(chol(sigma))
    #M
    M %*% t(M)
    Z <- matrix(data=c(rnorm(Nsamp,mean=0,sd=1),rnorm(Nsamp,mean=0,sd=1)),
                nrow=2,ncol=Nsamp, byrow=TRUE)
    #dim(Z)
    #apply(Z,1,mean)
    #apply(Z,1,sd)
    X <- t(M %*% Z)
    # add mu1 and mu2 to respective cols
    X[,1] <- X[,1]+mu1
    X[,2] <- X[,2]+mu2
    #X <- X + matrix(rep(mu,Nsamp), byrow=TRUE,ncol=2)
    colnames(X) <- c("v1","v2")
    #Xbar
    #apply(X,2,sd)
  } else
  {
    X <- mvrnorm(n=Nsamp, mu=c(mu1,mu2), Sigma=sigma)
  }  
  if(plotty)
  {
    plot(X, main=paste("Bivariate normal distribution [method=",method,"]",sep=""),
         pre.plot=grid(), type="p", bty="n",
         xlab="v1", ylab="v2", col="olivedrab")
    Xbar <- apply(X,2,mean)
    S <- cov(X)
    S
    ellipse(center=Xbar, shape=S, radius=radius, col="blue")
    ellipse(center=c(mu1,mu2), shape=sigma, radius=radius, col="darkred", lty=2)
  }
return(X)  
}
# calls:
# bivarnorm.res <- bivarnorm()
# bivarnorm(method="MASS")
# mu1 <- 1
# mu2 <- 2
# bivarnorm(mu1=1,mu2=2)
########################## END OF FUNCTION


###### function to calculate gradient of a function for HMC algorithm
# taken and changed from rhmc to allow to give over more parameters of the called function
num_grad2 <- function (f, x, ...) 
{
  d = length(x)
  g = numeric(d)
  for (i in 1:d) {
    h = sqrt(.Machine$double.eps) * if (x[i] != 0) 
      abs(x[i])
    else 1e-08
    xh = x[i] + h
    dx = xh - x[i]
    if (dx == 0) 
      next
    Xh = x
    Xh[i] = xh
    g[i] = (f(Xh, ...) - f(x, ...))/dx
  }
  g
}
########################## END OF FUNCTION


###### function to simulate bivariate normal distribution via HMC algorithm from rethinking
bivarnormdist.HMC.sim <- function(U, grad_U,
                    epsilon=0.1, L=11,
                    Qinitv=c(0,0),
                    nchains=5,
                    nsamp=1e3,
                    Qrescnams=c("q1","q2","a","dH"),
                    seeds,
                    ...
                    )
{
  
  require(rethinking)
  
  cat("\nnsamp:\t",nsamp,"\nEpsilon: ",epsilon,"\nL:\t",L,"\n\n",sep="")
  
  coln <- length(Qrescnams) #length(Qinit$q)+1+1
  Qres <- matrix(NA, nrow=nsamp, ncol=coln)
  colnames(Qres) <- Qrescnams
  Qres[1,c(1:2)] <- Qinitv
  OUTmcmc <- list()
  if(length(seeds) != nchains) stop("Number of chains and seeds differ!")
  
  for(z in 1:nchains)
  {
    cat("Chain:\t",z,"\nseed=\t",seeds[z],"\n\n",sep="")
    
    Q <- list()
    Q$q <- Qinitv
    set.seed(seeds[z])
    
    for (i in 1:nsamp)
    {
      # print(i)
      Q <- HMC2(U, grad_U, epsilon=step, L=L, current_q=Q$q, ...)
      Qres[i,"dH"] <- Q$dH
      Qres[i,"a"] <- Q$accept
      if(Q$a == 1) Qres[i,c(1:2)] <- Q$q
    }
    OUTmcmc[[z]] <- Qres
  }
return(OUTmcmc)
}
# call:
# OUTmcmc <- bivarnormdist.HMC.sim(U=U, gad_U=grad_U, seeds=seeds)
########################## END OF FUNCTION


###### function to calculate descriptive statistics per chain
MCMCout.desc.per.chain <- function(res, nchoose=c(1,2))
{
  cnams <- attr(res[[1]],"dimnames")[[2]]
  resi <- lapply(res, function(x)
  {
    t(apply(x,2,function(x) c(summary(x),sd=sd(x),var=var(x))))
  }
  )
  resi <- lapply(nchoose, function(y) lapply(resi, function(x) x[y,])) #1:2
  #str(resi)
  resi <- lapply(resi, function(x) do.call("rbind",x))
  names(resi) <- cnams[nchoose]
return(resi)
}
# call:
# MCMCout.desc.per.chain(OUTmcmc.nonas.onlyqs, nchoose=c(1,2))
########################## END OF FUNCTION


###### function to calculate descriptive statistics over all chains
MCMCout.desc.all.chain <- function(res, nchoose=c(1,2))
{
  #rnams <-  colnames(res[[1]])
  rnams <- attr(res[[1]],"dimnames")[[2]]
  resi <- lapply(1:2, function(y) unlist(lapply(res, function(x) x[,y])))
  resi <- t(sapply(resi, function(x) c(summary(x),sd=sd(x), var=var(x))))
  rownames(resi) <- rnams[nchoose]
return(resi)
}
# call:
# MCMCout.desc.all.chain(OUTmcmc.nonas.onlyqs)
########################## END OF FUNCTION


###### function to determine eps for passing Heidelberger-Welch diagnostics
# useful if it fails to get an impression of required eps
# could be done with some optim() algorithm
# here rough stepsize (precision!) is e.g. half of epsilon
heidel.eps.det <- function(mcmc.data, eps.init=0.01, steps=0.05, print.test=TRUE)
{
  itera <- 1
  passed <- FALSE
  heidel.s.h.test <- c(0,0)
  eps <- eps.init
  while(passed==FALSE)
  {
    heidel.diag.res <- heidel.diag(mcmc.data, eps=eps)
    heidel.s.h.test <- heidel.diag.res[,c(1,4)]
    if(sum(heidel.s.h.test)==2)
    {
      passed=TRUE
      res <- list(eps=eps, itera=itera, eps.init=eps.init, steps=steps)
    }  
    itera <- itera + 1
    eps <- eps + steps
  } 
  if(print.test)
  {
    print(heidel.diag.res)
    cat("\n\n")
  }  
  return(res)
}
# call:
# eps.det.OUTmcmc.list.q1 <- data.frame(t(sapply(1:nchains, function(x) heidel.eps.det(OUTmcmc.list[[x]][,"q1"]))))
# eps.det.OUTmcmc.list.q2 <- data.frame(t(sapply(1:nchains, function(x) heidel.eps.det(OUTmcmc.list[[x]][,"q2"]))))
########################## END OF FUNCTION


###### function to cumulative describe development of mean and covariance
# per chain
MCMCout.cumdesc.per.chain <- function(res)
{
#  res <- OUTmcmc.nonas[[1]]
  dims <- dim(res)
  divby <- 1:dims[1]
#  length(divby)
  mean.cs <- apply(res,2,cumsum)/divby
  cov.cs <- sapply(2:dims[1], function(x)
  {
    # we drop the vars on the diagonal and use only one cov,
    # because both are identical
    cov(res[1:x,])[1,2]
  })
  return(list(mean.cs,cov.cs))
}
# call:
# MCMCout.cs.descs <- lapply(OUTmcmc.nonas.onlyqs, MCMCout.cumdesc.per.chain)
########################## END OF FUNCTION


###### function to adjust limits to make comparison value appear on a plot
adj.limits <- function(daten, comp.v=0, fac=1.15)
{
  stopifnot(fac >= 1)
  daten.r <- range(daten, na.rm=TRUE)
  cat("\nrange:\t\t",daten.r[1],"\t",daten.r[2],sep="")
  cat("\ncomp.v:\t",comp.v,sep="") 
  cat("\nfac:\t",fac,sep="")
  if(sum((daten.r > comp.v)+0) == 2) daten.r[1] <- comp.v*(1-fac)
  if(sum((daten.r < comp.v)+0) == 2) daten.r[2] <- comp.v*fac
  if(sum((daten.r < comp.v)+0) != 2 & sum((daten.r > comp.v)+0) != 2)
  {
    #different sign?
    if(sum(sign(daten.r)) == 0)
    {
      daten.r <- daten.r*fac
    } else #same sign
    {  
      daten.r[1] <- daten.r[1]*(1-abs(1-fac))
      daten.r[2] <- daten.r[2]*fac
    }  
  }
  cat("\nnew range:\t",daten.r[1],"\t",daten.r[2],"\n",sep="")
return(invisible(daten.r))
}
# call:
# adj.limits(daten=MCMCout.cs.descs[[1]][[1]][,"q1"][outtake["start"]:outtake["end"]], comp.v=0)
# adj.limits(daten=MCMCout.cs.descs[[1]][[2]][outtake["start"]:outtake["end"]], comp.v=rho)
########################## END OF FUNCTION


###### function to simulate bivariate normal distribution via MH algorithm
# sample x times regardless whether success or not
bivarsim.MH <- function(nchains=5, nsamp=3e4, U, sd.param=1, current.values=c(0,0), seeds=seeds)
{  
  # create vectors for acceptance rate and post values
  a.MH <- rep(NA, nsamp)
  post.MH <- matrix(NA, nrow=nsamp, ncol=2)
  colnames(post.MH) <- c("q1", "q2")

  # initial prob
  prob.current <- U.MH(q=current.values, sigma=sigmamat)
  sd.param <- 1
  current.values <- mu

  OUTmcmc.MH.list <- list()
  for(j in 1:nchains)
  {  
    set.seed(seeds[j])
    for(i in 1:nsamp)
    {
      #print(current)
      #create proposed values for x and y
      proposed <- c(rnorm(1,current.values[1],sd.param), #x
                    rnorm(1,current.values[2],sd.param)) #y
  
      prop.proposed <- U.MH(q=proposed, sigma=sigmamat)
      H1minusH0 <- prop.proposed-prob.current
      # = min(1,exp(prop.proposed)/exp(prob.current))
      prob.accept <- min(1,exp(H1minusH0))
      testvalue <- runif(1)
      if(testvalue <= prob.accept)
      {
        current.values <- post.MH[i,] <- proposed
        a.MH[i] <- 1
        prob.current <- prop.proposed
      } else
      {
        #not required
        #post[i,] <- NA
      }
    }
  OUTmcmc.MH.list[[j]] <- list(post.MH, a.MH)
  }
return(OUTmcmc.MH.list)
}
# call:
# OUTmcmc.MH.list <- bivarsim.MH(U=U.MH, seeds=seeds)
########################## END OF FUNCTION


###### function to simulate bivariate normal distribution via MH algorithm
# sample till x successful samples are create
bivarsim.MH2 <- function(nchains=5, nsamp=3e4, U, sd.param=1, current.values.init=c(0,0),
                         seeds=seeds, initialfac=1.3, addon.fac=0.1)
{  
  # create vectors for acceptance rate and post values
  new.l <- nsamp*initialfac
  
  # extension for later if initial length is not enough to reach nsamp
  addon.l <- nsamp*addon.fac
  addon.mat <- matrix(NA, nrow=addon.l, ncol=2)
  addon.vec <- rep(NA,addon.l)
  # initial length = nsamp
  check.l <- nsamp
  
  OUTmcmc.MH.list <- list()
  
  for(j in 1:nchains)
  {  
    set.seed(seeds[j])
    #for(i in 1:nsamp)
    # do till no. of acceptances == nsamp
    # only in case of acceptance rate == 100% -> as == nsamp
    as <- 0
    # counter acceptance rate
    i <- 1
    extend.no <- 0

    # initialize anew with NAs
    a.MH <- rep(NA,new.l)
    post.MH <- matrix(NA, nrow=nsamp, ncol=2)
    colnames(post.MH) <- c("q1", "q2")
    print(head(a.MH))
    print(head(post.MH))

    # initial prob
    current.values <- current.values.init
    prob.current <- U.MH(q=current.values, sigma=sigmamat)
    
    # to the actual MH algorithm
    while(as < (nsamp+1))
    {
      #print(current)
      #create proposed values for x and y
      proposed <- c(rnorm(1,current.values[1],sd.param), #x
                    rnorm(1,current.values[2],sd.param)) #y
      
      prop.proposed <- U.MH(q=proposed, sigma=sigmamat)
      H1minusH0 <- prop.proposed-prob.current
      # = min(1,exp(prop.proposed)/exp(prob.current))
      prob.accept <- min(1,exp(H1minusH0))
      testvalue <- runif(1)
      if(testvalue <= prob.accept)
      {
        current.values <- post.MH[as,] <- proposed
        a.MH[i] <- 1
        prob.current <- prop.proposed
        as <- as + 1
      } else
      {
        #not required
        #post[i,] <- NA
      }
      i <- i + 1
      if(i == check.l)
      {
        extend.no <- extend.no + 1
        a.MH <- c(a.MH, addon.vec)
        check.l <- check.l + addon.l
        cat(extend.no," | extend chain by\t",addon.l,"\n",sep="")
      }  
    }
    # reduce a.MH so that no NAs are after the last entry
    a.MH <- a.MH[1:max(which(!is.na(a.MH)))]
    # add a.MH and post.MH to result list
    OUTmcmc.MH.list[[j]] <- list(post.MH, a.MH)
    cat("\nchain = ",j," | chain length = ",length(a.MH)," [nsamp = ",nsamp,"]\n\n",sep="")
  }
  print(str(OUTmcmc.MH.list))
return(OUTmcmc.MH.list)
}
# call:
# OUTmcmc.MH.list <- bivarsim.MH2(U=U.MH, seeds=seeds)
########################## END OF FUNCTION




