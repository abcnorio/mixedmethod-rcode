# file:
# ptII_quan_Bayes_MH-Gibbs_example_helpfuncs.r

# location:
# chap. 6 [6.13.4.1]
# Der Metropolis-Hastings Algorithmus im R

# HELPER FUNCTIONS


###### function to calculate MH algorithm
MH_norm <- function(y=c(9.37, 10.18, 9.16, 11.60, 10.33),
                    s2.prop=2, s2=1, t2=10, mu=5,
					theta0=0, nsim=1e+5, seed=1)
{
 set.seed(seed)
 cnamen <- c("thetaST","u.log","r.log","theta","acceptance")
 mat <- matrix(data=NA, nrow=nsim, ncol=length(cnamen))
 colnames(mat) <- cnamen
 mat[1,"theta"] <- theta0
 for (i in 2:nsim)
 {
  theta <- mat[i-1,"theta"]
  
  # draw candidate from normal distribution with mean=theta and sigma=VAR=+/-2 standard deviations)
  # that's the proposal distribution of thetaST given theta[i-1]
  thetaST <- rnorm(1, theta, sqrt(s2.prop))
  u.log <- log(runif(1))

  # thetaST(ar) = candidate
  # posterior thetaST(ar) on log scale
  llik.thetaST <- sum(dnorm(y,thetaST,sqrt(s2),log=TRUE))
  lprior.thetaST <- dnorm(thetaST,mu,sqrt(t2),log=TRUE)
  lpost.thetaST <- llik.thetaST + lprior.thetaST
  
  # theta = existent value
  # posterior theta on log scale
  llik.theta <- sum(dnorm(y,theta,sqrt(s2),log=TRUE))
  lprior.theta <- dnorm(theta,mu,sqrt(t2),log=TRUE)
  lpost.theta <- llik.theta + lprior.theta
  
  # calculate relationship of the posteriors of thetaST(=candidate) and theta
  # ratio of posteriors = difference on log scale (posteriors)
  r.log <- lpost.thetaST - lpost.theta
  
  # actual Metropolis Hastings algorithm decision part
  # set theta[i] <- thetaST with prob min(r.log,log(1)) else theta[i] <- theta[i-1] (= ie. no change)
  theta.new <- ifelse(u.log <= r.log, thetaST, theta)
  
  mat[i,c(1:4)] <- c(thetaST, u.log, r.log, theta.new)
 }
 mat[,"acceptance"] <- mat[,1] == mat[,4]
return(mat) 
}
# call:
# mat <- MH_norm(nsim=nsim)
########################## END OF FUNCTION


###### function to calculate MH algorithm
mu.sigma2.post <- function(y, mu.prior, sigma2.prior, sigma2.pop=NA, n=NA)
{
  # y = data
  if(is.na(n)) n <- length(y)
  if(is.na(sigma2.pop)) sigma2.pop <- var(y)
  mu.data <- mean(y)
  mu.post <- ( mu.prior/sigma2.prior + (n*mu.data/sigma2.pop) ) / ((1/sigma2.prior) + (n/sigma2.pop))
  # CHECK.... https://de.slideshare.net/BayesLaplace1/bayesian-statistics-using-r-intro -> slide 36 (error!)
  sigma2.post <- 1/ ( (1/sigma2.prior) + (n/sigma2.pop) )
  sigma.post <- sqrt(sigma2.post)
  tau <- 1/sigma2.post
  
  # Bolstad p.224
  # alternative: s2.post <- (n*sigma2.prior/(n*sigma2.prior + sigma2.pop))^2 * (sigma2.pop/n)
  # bias: sigma2.pop/(n*sigma2.prior + sigma2.pop)*(mu.data-mu.prior)
  
  # Gill p.76
  # normal model with variance known... and unknown mean
  # mu.post <- ( mu.prior/sigma2.prior + mu.data*n/sigma2.pop ) / (n/sigma2.pop + 1/sigma2.prior)
  # s2.post <- 1/(1/sigma2.prior + n/sigma2.pop)
  # s.post <- sqrt(s2.post)
  # tau.post <- 1/s2.post
  # s2.post
  # s.post
  # tau.post
  
  res <- data.frame(mu.prior, sigma2.prior, sigma2.pop, mu.post,sigma2.post, sigma.post, tau)
  colnames(res) <- c("mu.prior", "sigma2.prior", "sigma2.pop", "mu.post","s2.post","s.post","tau")
  return(res)
}
# call:
# mu.sigma2.post(y=y, mu.prior=5, sigma2.prior=10, sigma2.pop=1)
########################## END OF FUNCTION


###### function to perform Gibbs sampling with thinning...
# https://stats.stackexchange.com/questions/266665/gibbs-sampler-examples-in-r
gibbs <- function(daten, tau1=1, nsim=1e+3, seed=1, thin=1e+3)
{
  set.seed(seed)
  cnam <- c("no","mu","tau")
  outv <- matrix(data=NA, nrow=nsim, ncol=length(cnam))
  colnames(outv) <- cnam
  outv[,"no"] <- 1:nsim
  
  y <- daten$y
  mu.prior <- daten$mu.prior
  sigma2.prior <- daten$sigma2.prior
  sigma2.pop <- daten$sigma2.pop
  mu.post <- daten$mu.post
  sigma2.post <- daten$sigma2.post
  
  # initial values
  n <- length(y)
  mu.data <- mean(y)
  outv[,"tau"] <- tau1
  outv[1,"mu"] <- mu.data
  
  mu <- rep(NA,thin)
  taus <- rep(NA,thin)
  # initial value for tau = precision
  if(is.na(tau1)) tau1 <- rgamma(1,n/2,1/2)
  taus[1] <- tau1
  
  for(i in 2:nsim)
  { 
    for(j in 2:thin)
    {
      
      # https://stats.stackexchange.com/questions/266665/gibbs-sampler-examples-in-r
      # mu = pop mean
      # tau = 1/variance population = pop precision
      # n = sample size
      # ybar = sample mean
      # s^2 = sample variance
      # p(mu|tau,data) ~ N(ybar, 1/(n*tau)
      # p(tau|mu,data) ~ Ga(n/2, 2/ ((n-1)*s^2 + n*(mu.ybar)^2))
       
      # mu[j] <- rnorm( n=1, mean=mu.data, sd=sqrt(1 / (n*tau[j-1])) )
      # tau[j] <- rgamma( n=1, shape=n/2, scale=2/ ( (n-1)*sigma2.pop + n*(mu[j]-mu.data)^2 ) )
      
      # http://galton.uchicago.edu/~eichler/stat24600/Handouts/s09.pdf
      # mu_t+1 ~ N(ybar, (n*tau_t)^-1)
      # tau_t+1 ~ Ga(n/2, 1/2sum_i=1:n(y_i-mu_t+1)^2)
      # sigma2_t+1 = 1/tau_t+1
      
      mu[j] <- rnorm(n=1, mean=mu.data, sd=sqrt(1/(taus[j-1]*n)))
      taus[j] <- rgamma(n=1, shape=n/2, scale=sum((y-mu[j])^2)/2)
      
      # https://www4.stat.ncsu.edu/~reich/ABA/code/
      # https://www4.stat.ncsu.edu/~reich/ABA/code/NN2
      # Y_i|mu,sigma2 ~ N(mu, sigma2)
      # goal: estimate joint dist of (mu,sigma2)
      # mu ~ N(mu_prior, sigma2_prior)
      # sigma2 ~ InvGa(a,b)
      
      # a = prior a <- 0.01
      # b = prior b <- 0.01
      # mu.prior <- 0
      # sigma2.prior <- 1000
      
      # A <- sum(y)/sigma2.pop + mu.prior/sigma2.prior
      # B <- n/sigma2.pop + 1/sigma2.prior
      # mu[j] <- rnorm(n=1, mean=A/B, sd=1/sqrt(B))
      
      # A <- n/2 + a
      # B <- sum((y-mu.data)^2) / 2 + b
      # tau[j] <- rgamma(n=1, shape=A, scale=B)
      # sigma2.keep[j] <- 1/tau[j]
    }
    outv[i,"mu"] <- mu[j]
    outv[i,"tau"] <- taus[j]
    taus[1] <- taus[j]
    mu[1] <- mu[j]
  }
  return(outv) 
}
# call:
# gibbs(daten=daten, nsim=nsim)
########################## END OF FUNCTION


###### function to plot MCMC 
plot.mcmc <- function(mus, taus, credMass=0.87, compValmu=10, compValtau=1, ROPEmu=c(8,9.5), ROPEtau=c(0.5,1))
{
  # summarize MCMC/post plots
  mus.l <- length(mus)
  taus.l <- length(taus)
  require(BEST)
  par(oma=c(2,1,2,1), "cex.axis"=1, bty="l", mfrow=c(3,2))
  BEST:::plotPost(mus, credMass=credMass, compVal=compValmu, ROPE=ROPEmu, showMode=TRUE, xlab=expression(mu), main="")
  lines(density(mus), col="magenta3", lwd=3, lty=2)
  BEST:::plotPost(taus, credMass=credMass, compVal=compValtau, ROPE=ROPEtau, showMode=TRUE, xlab=expression(tau), main="")
  lines(density(taus), col="darkred", lwd=3, lty=2)
  plot(1:mus.l, mus, type="l", col="magenta3", bty="n", pre.plot=grid(), xlab="sim", main=expression(mu))
  plot(1:taus.l, taus, type="l", col="darkred", bty="n", pre.plot=grid(), xlab="sim", main=expression(tau))
  acf(mus, col="magenta3", bty="n", pre.plot=grid(), xlab="lag", main=expression(mu))
  acf(taus, col="darkred", bty="n", pre.plot=grid(), xlab="lag", main=expression(tau))
  mtext(expression(paste("Gibbs sampling for ",mu," and ",tau,sep="")), outer=TRUE, line=-2, cex=1.5, side=3)
}
# call:
# plot.mcmc(mus, taus)
########################## END OF FUNCTION



###### function to plot parts of MCMC
plot.mcmc.parts <- function(mus, taus, part=1:100, rylim=c(-1,1))
{
  # plot parts of the chain
  par(oma=c(2,1,3,1), "cex.axis"=1, bty="l", mfrow=c(2,2))
  plot(part,mus[part], col="magenta2", bty="n", pre.plot=grid(), type="l", ylab=expression(mu), xlab="sim", main=expression(paste(mu,sep="")))
  plot(part,taus[part], col="darkred", bty="n", pre.plot=grid(), type="l", ylab=expression(tau), xlab="sim", main=expression(paste(tau,sep="")))
  plot(mus[part],taus[part], col="black", pch=21, bg="orange", bty="n", pre.plot=grid(), type="b", xlab=expression(mu), ylab=expression(tau), main=expression(paste("Correlation of ",mu," and ",tau,sep="")))
  
  dpart <- diff(range(part))
  dpart
  r.cc <- rep(NA,dpart+1)
  r.cc
  part1 <- part[1]
  for(i in 1:(dpart+1)) r.cc[i] <- cor(mus[part1:(part1+i)], taus[part1:(part1+i)])
  r.cc
  plot(part, r.cc, ylim=rylim, col="violetred3", bty="n", pre.plot=grid(), type="l", xlab="sim", ylab=expression(paste("correlation (",mu,",",tau,")",sep="")), main=expression(paste("Correlation of ",mu," and ",tau," over time", sep="")))
  abline(h=0, col="steelblue", lty=2)
  mtext(expression(paste("Gibbs sampling for ",mu," and ",tau,sep="")), outer=TRUE, line=-0.5, cex=1.5, side=3)
}
# call:
# plot.mcmc.parts(mus,taus)
########################## END OF FUNCTION




# not run


# Metropolis example

# https://stats.stackexchange.com/questions/232824/bayesian-updating-with-conjugate-priors-using-the-closed-form-expressions
# First of all, the formulas are defined in terms of variance, not standard deviations.
# Second, the variance of the posterior is not a variance of your data but variance of estimated parameter μ.
# As you can see from the description ("Normal with known variance σ2"), this is formula for estimating μ when σ2 is known.
# The prior parameters μ0 and σ20 are parameters of distribution of μ, hence the assumed model is

############################# FUNCTION
# normal posterior predictive distribution
norm.pred.post <- function(n, mu.post, sigma2.post, sigma2.data)
{
 return( rnorm(n, mu.post, sqrt(sigma2.post+sigma2.data)) )
}
############################# END OF FUNCTION

############################# FUNCTION
# MH sample for normal distribution
MH.sampler <- function(daten=NA, nsim=1e+4, theta0=0, delta=2, seed=1, type="MH",
                       thresh=100, hist.cand.sigma=FALSE, loga=TRUE)
 {
  set.seed(seed)
  
  mu.prior <- daten[["mu.prior"]]
  sigma2.prior <- daten[["sigma2.prior"]]
  y <- daten[["y"]]
  sigma2.data <- daten[["sigma2.data"]]
  
  cnam <- c("no","curr","cand","cand.sigma","aprob","comp","theta")
  outv <- matrix(data=NA, nrow=nsim, ncol=length(cnam))
  colnames(outv) <- cnam
  outv[1,"no"] <- 1
  outv[1,"theta"] <- theta0
  head(outv)

  cand.sigma <- sqrt(delta)
  
  # MH part
  for(i in 2:nsim)
  {
  # current
   current <- outv[i-1,"theta"]
  # candidate
   candidate <- rnorm(1, current, cand.sigma)
   prior.cand <- dnorm(candidate, mu.prior, sqrt(sigma2.prior), log=loga)
   likeli.cand <- sum(dnorm(y,candidate,sqrt(sigma2.data), log=loga))
   prior.curr <- dnorm(current, mu.prior, sqrt(sigma2.prior), log=loga)
   likeli.curr <- sum(dnorm(y,current,sqrt(sigma2.data), log=loga))
   if(loga)
   {
    aratio.log <- (prior.cand+likeli.cand) - (prior.curr+likeli.curr)
   } else
   {
    aratio <- (prior.cand*likeli.cand)	/ (prior.curr*likeli.curr)
    aratio.log <- log(aratio)
   }   
   if(type == "MH")
   {
    compare <- runif(1, 0, 1)
    theta <- ifelse( log(compare) <= aratio.log, candidate, current )
   } else stop()
   outv[i,] <- c(i, current, candidate, cand.sigma, aratio.log, compare, theta)
   if(hist.cand.sigma)
   {
    if(i > thresh)
    {
    cand.sigma <- sd(outv[floor(i/2):i,"theta"])
	} else
    {
     cand.sigma <- cand.sigma + rnorm(1,0,1)
	}
   }	   
  }
return(outv)
}
# MH.sampler(daten=daten)
############################# END OF FUNCTION

