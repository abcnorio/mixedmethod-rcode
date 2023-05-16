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





# look at source code

# Hamiltonian Dynamics
rhmc:::hamiltonian_dynamics    
# Hamiltonian Monte Carlo
rhmc:::hmc                     
# Numerical Gradient
rhmc:::num_grad                
?hmc


# mu
mu <- 10
# sigma
sigma <- 5
# create some function with (negative) log posterior
hmcfun <- function(x) -dnorm(x, mu, sigma, log=TRUE)

# compare gradient functions
library(numDeriv)
hmcfun(100)
?grad
numDeriv:::grad(hmcfun, 100)
rhmc:::num_grad(hmcfun, 100)
all.equal(numDeriv:::grad(hmcfun, 100), rhmc:::num_grad(hmcfun, 100))


# do actual HMC mcmc
# number of chains
nchains <- 10
# iterations per chain
itera <- 1000
# initival value (prior value!)
initv <- 9
# leapfrog parameter (number of steps)
L <- 8
# leapfrog parameter (size of each step)
eps <- 0.3
# leapfrog parameter (mass vector)
mass <- 0.1
hmc.res.arr <- lapply(1:nchains, function(x) rhmc:::hmc(hmcfun, initv+(0.5-runif(1)), itera, L, eps, mass)) 

# have a look at results
str(hmc.res.arr)

# plot one chain
chain <- drop(hmc.res.arr[[1]]$chain)
U <- drop(hmc.res.arr[[1]]$U)
plot(chain, type="l")
plot(U[1:30],type="l")


# take only posterior values
hmc.res.arr.red <- lapply(hmc.res.arr, function(x) x[1])
str(hmc.res.arr.red)

# define dimensions for array
dimis <- dim(as.matrix(hmc.res.arr.red[[1]][[1]]))
# create array
res.arr <- array(dim=c(dimis[2],nchains,dimis[1]),
                 dimnames=list(iter=1:dimis[2],
                               chain=paste("chain",1:nchains,sep=""),
                               var=c("theta"))
)
# fill i values
for(i in 1:nchains) res.arr[,i,] <- t(as.matrix(hmc.res.arr.red[[i]][[1]]))

library(bayesplot)
# plot posteriors and chains

color_scheme_set("mix-blue-pink")
bayesplot:::mcmc_trace(res.arr, pars=c("theta"))
bayesplot:::mcmc_hist_by_chain(res.arr, pars=c("theta"))
color_scheme_set("green")
bayesplot:::mcmc_hist(res.arr, pars=c("theta"))
color_scheme_set("purple")
bayesplot:::mcmc_dens(res.arr, pars=c("theta"))
color_scheme_set("blue")
bayesplot:::mcmc_dens_overlay(res.arr, pars=c("theta"))
color_scheme_set("teal")
bayesplot:::mcmc_violin(res.arr, pars=c("theta"), probs=c(0.1, 0.5, 0.9))

# desriptive statistics
# per chain
str(unlist(hmc.res.arr.red[[1]]))
hm.res.red.desc <- do.call("rbind", lapply(hmc.res.arr.red, function(x) summary(unlist(x))))
hm.res.red.desc
hm.res.red.sd <- do.call("rbind", lapply(hmc.res.arr.red, function(x) sd(unlist(x))))
hm.res.red.desc <- data.frame(hm.res.red.desc,sd=hm.res.red.sd,var=hm.res.red.sd^2)
hm.res.red.desc
# over all chains
apply(hm.res.red.desc,2,mean)

# q<uantiles
hm.res.red.quan <- do.call("rbind", lapply(hmc.res.arr.red, function(x) quantile(unlist(x))))
apply(hm.res.red.quan,2,mean)

# all chains
summary(res.arr)
sd(res.arr)
var(res.arr)       

# summarize through array
theta.chains <- (res.arr[,,1])
str(theta.chains)
# descriptive statistics
t(apply(theta.chains, 2, function(x) c(summary(x),sd=sd(x),var=var(x))))

burnin.rem <- FALSE
burnin <- 100
dim(theta.chains)
if(burnin.rem) theta.chains <- theta.chains[-c(1:burnin),]
c(mean=mean(theta.chains),
  sd=esdes <- sd(theta.chains),
  var=esdes^2,
  SE=sqrt((esdes^2)/dim(theta.chains)[1])
)
quantile(theta.chains, probs=c(0,0.025,0.25,0.5,0.75,0.975,1))
# quantiles

# convert to mcmc object and mcmc list to use coda library
library(coda)
hmc.res.arr.red.mcmc <- as.mcmc.list(lapply(hmc.res.arr.red, function(x) mcmc(t(x$chain))))
summary(hmc.res.arr.red.mcmc)



#######

bivariate_normal <- function(theta, n) {
  mu1 <- theta[1]
  sigma1 <- theta[2]
  mu2 <- theta[3]
  sigma2 <- theta[4]
  x <- rnorm(n/2, mean=mu1, sd=sigma1)
  y <- rnorm(n/2, mean=mu2, sd=sigma2)
  data.frame(x, y)
}

theta <- c(5.0, 0.5, 2.0, 1.5)
names(theta) <- c("mu1","sd1","mu2","sd2")
theta
n <- 100
sims <- bivariate_normal(theta, n)
xs <- as.matrix(sims)
head(xs)
dim(xs)

log_likelihood <- function(xs, theta)
{
  sum(apply(xs, 1, function(x) dnorm(x[1], mean=theta[1], sd=theta[2], log=T) + 
              dnorm(x[2], mean=theta[3], sd=theta[4], log=T))
  )
}
log_likelihood(xs, theta)

log_likelihood <- function(xs, theta) {
  apply(xs, 1, function(x) dnorm(x[1], mean = theta[1], sd = theta[2], log = T) + 
          dnorm(x[2], mean = theta[3], sd = theta[4], log = T)) %>% sum()
}
log_likelihood(xs, theta)

#The prior distributions are chosen to be:
#p(μj)p(Σjj)=N(0,3),=Gamma(3,3),j=1,2.


log_prior <- function(theta)
{
  dnorm(theta[1], log=T) + 
    dnorm(theta[3], log=T) + 
    dgamma(theta[2], shape=3, rate=3, log=T) + 
    dgamma(theta[4], shape=3, rate=3, log=T)
}
log_prior(theta)

log_posterior <- function(xs, theta)
{
  log_likelihood(xs, theta) + log_prior(theta)
}  
log_posterior(xs,theta)

log_posterior <- function(xs) 
{
  function(theta) 
    log_likelihood(xs, theta) + log_prior(theta)
}
log_posterior(xs)


metropolis_step <- function(theta, log_posterior, proposal) {
  propTheta <- proposal(theta)
  a <- log_posterior(propTheta) - log_posterior(theta)
  u <- runif(1)
  if (log(u) < a) {
    propTheta
  } else {
    theta
  }
}

f = function(x) log_posterior(xs,theta)
hmc(f, init=theta, 1000, 10, 0.3, 0.1)

metropolis <- function(theta, log_posterior, proposal, m) {
  out = matrix(NA_real_, nrow = m, ncol = length(theta))
  out[1, ] = theta
  for (i in 2:m) {
    out[i, ] <- metropolis_step(out[i-1, ], log_posterior, proposal)
  }
  out
}

proposal <- function(x) {
  z = rnorm(4, sd = 0.05)
  c(x[1] + z[1], x[2] * exp(z[2]),
    x[3] + z[3], x[4] * exp(z[4]))
}


out = metropolis(theta, log_posterior(xs), proposal, 10000)

str(out)



numDeriv:::grad(log_posterior, 10)
rhmc:::num_grad(log_posterior, 100)



log_posterior(xs)
rhmc:::num_grad(log_posterior,xs)


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

bivarnorm.res <- bivarnorm()
bivarnorm(method="MASS")
mu1 <- 1
mu2 <- 2
bivarnorm(mu1=1,mu2=2)


##################


xs <- bivarnorm.res
dim(xs)
theta <- c(mu1,s1,mu2,s2)
names(theta) <- c("mu1","s1","mu2","s2")
theta

log_likelihood <- function(xs, theta=theta)
{
  sum( apply(xs, 1,
             function(x) dnorm(x[1], mean=theta["mu1"], sd=theta["s1"], log=TRUE) + 
               dnorm(x[2], mean=theta["mu2"], sd=theta["s2"], log=TRUE))
  )
}

log_prior <- function(theta=theta, gammavs=c(shape=3,rate=3))
{
  dnorm(theta["mu1"], log=TRUE) +
  dnorm(theta["mu2"], log=TRUE) + 
  dgamma(theta["s1"], shape=gammavs[1], rate=gammavs[2], log=TRUE) + 
  dgamma(theta["s2"], shape=gammavs[1], rate=gammavs[2], log=TRUE)
}

log_posterior <- function(xs)
{  
  function(theta)
  {  
    log_likelihood(xs, theta) + log_prior(theta)
  }
}  

lp <- log_posterior(xs[1:2,])(theta)
lp
numDeriv:::grad()

##########################

head(mvtnorm:::dmvnorm(xs))




# borrowed and taken from package "rhmc"
rhmc:::hmc(f=log_posterior,
           init=19,
           numit=1000,
           L=16,
           eps=0.3,
           mass=0.1)

rhmc:::hmc <- function (f, init, numit, L, eps, mass) 
{
  d = length(init)
  q = matrix(nrow = d, ncol = numit)
  U = numeric(numit)
  q[, 1] = init
  U[1] = f(init)
  ar = 0
  for (i in 2:numit) {
    p = rnorm(d, 0, sqrt(mass))
    K = sum(p^2/(2 * mass))
    prop = hamiltonian_dynamics(f, q[, i - 1], p, L, eps, 
                                mass)
    U_prop = f(prop$q)
    K_prop = sum(prop$p^2/(2 * mass))
    if (runif(1) < exp(K - K_prop + U[i - 1] - U_prop)) {
      q[, i] = prop$q
      U[i] = U_prop
      ar = ar + 1
    }
    else {
      q[, i] = q[, i - 1]
      U[i] = U[i - 1]
    }
  }
  list(chain = q, U = U, ar = ar/(numit - 1))
}

hamiltonian_dynamics <- function (U, q, p, L, eps, m) 
{
  p = p - 0.5 * eps * num_grad(U, q)
  for (i in 1:(L - 1)) {
    q = q + eps * p/m
    p = p - eps * num_grad(U, q)
  }
  q = q + eps * p/m
  p = p - 0.5 * eps * num_grad(U, q)
  list(q = q, p = p)
}

x <- q[, i - 1]


num_grad <- function (f, x) 
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
    g[i] = (f(Xh) - f(x))/dx
  }
  g
}
##################

# Linear regression example
set.seed(521)
X <- cbind(1, matrix(rnorm(300), ncol=3))
betavals <- c(0.5, -1, 2, -3)
y <- X%*%betavals + rnorm(100, sd=.2)
head(X)
head(y)

theta.init <- c(rep(0, 4), 1)
theta.init

f1_hmc <- hmclearn:::hmc(N = 500,
                         theta.init = theta.init,
                         epsilon = 0.01,
                         L = 10,
                         logPOSTERIOR = linear_posterior,
                         glogPOSTERIOR = g_linear_posterior,
                         varnames = c(paste0("beta", 0:3), "log_sigma_sq"),
                         param=list(y=y, X=X), parallel=FALSE, chains=1)

summary(f1_hmc, burnin=100)
str(f1_hmc)

linear_posterior(theta.init,y,X) #res = 1 value
g_linear_posterior(theta.init,y,X) #res = 5 values = 5 parameters (=length(theta.init))

########################
# R.M. Neal MCMC Handbook
# basic model taken from here:

# p.125

HMC2 <- function (U, grad_U, epsilon, L, current_q, ...) 
{
  q = current_q
  p = rnorm(length(q), 0, 1)
  current_p = p
  
  p = p - epsilon * grad_U(q, ...)/2
  
  #qtraj <- matrix(NA, nrow = L + 1, ncol = length(q))
  #ptraj <- qtraj
  #qtraj[1, ] <- current_q
  #ptraj[1, ] <- p
  
  for (i in 1:L) {
    q = q + epsilon * p
    if (i != L) {
      p = p - epsilon * grad_U(q, ...)
      #ptraj[i + 1, ] <- p
    }
    #qtraj[i + 1, ] <- q
  }
  p = p - epsilon * grad_U(q, ...)/2
  #ptraj[L + 1, ] <- p
  p = -p
  
  current_U = U(current_q, ...)
  current_K = sum(current_p^2)/2
  proposed_U = U(q, ...)
  proposed_K = sum(p^2)/2
  
  H0 <- current_U + current_K
  H1 <- proposed_U + proposed_K
  
  new_q <- q
  accept <- 0
  
  if (runif(1) < exp(current_U - proposed_U + current_K - proposed_K)) {
    new_q <- q
    accept <- 1
  }
  else new_q <- current_q
  #return(list(q = new_q, traj = qtraj, ptraj = ptraj, accept = accept, 
  #            dH = H1 - H0))
  return(list(q = new_q, accept = accept, dh = H1 - H0))
}

# HMC_2D_sample
# sampler  
n_samples <- n
a <- rep(NA, n_samples)
dH <- rep(NA, n_samples)
post <- matrix(NA, nrow = n, ncol = 2)
for (i in 1:n_samples) {
  Q <- HMC2(U, U_gradient, epsilon = step, L = L, current_q = Q$q)
  r <- min(abs(Q$dH), 1)
  dH[i] <- Q$dH
  a[i] <- Q$accept
  if (a[i] == 1) {
    post[i, ] <- Q$q
  }
}

#############################################################
# Kevin Shoemaker MH & MCMC


library(mvtnorm)
seed <- 9988776
set.seed(seed)

# iterations of HMC MCMC
nsamp <- 500

# correlation of the two normal dists
rho <- 0.6

# initial values for all parameters of interest
xinit <- 0
yinit <- 0

# matrix for storing the random samples
Qmat <- matrix(ncol=2, nrow=nsamp)

# initialize the markov chain
# Q stores temporarily as a list for each loop the relevant values
# first entry = initial values

# initial  
current_q <- c(xinit, yinit)

# sigma matrix - stays constant
#sigmamat <- matrix(c(4,2,2,3), ncol=2)
sigmamat <- matrix(c(1,rho,rho,1),ncol=2)

# probability density of the distribution at the starting values
# actually not really needed???
# otherwise start with n>1
#prev <- dmvnorm(x=Q$q, mean=c(0,0), sigma=sigmamat, log=TRUE)

# HMC_2D_sample
# sampler 

# define vectors, matrices, etc. for stored post values
# accept
a <- rep(NA, nsamp)
# diff H1-H0
dH <- rep(NA, nsamp)
# posterior values c(x,y)
post <- matrix(NA, nrow=nsamp, ncol=2)

# adapt to terminology from Neale & CO
# not needed as it is required for each trial...
#current_q <- Q$q

L <- 8
epsilon <- 0.03
step <- 0.03

# sd of rnorm to choose parameters
draw.param.sd <- 1#0.5 #1

# crawl through iterations
#for (i in 1:nsamp)
i <- 1
while(i <= nsamp)
{
  print(i)
  q <- current_q
  
  ### start of HMC2  
  #q = current_q
  #p = rnorm(length(q), 0, 1)  
  
  # make a jump. Note the symmetrical proposal distribution
  # draw both from rnorm
  
  p <- rnorm(length(q),0,draw.param.sd)
  current_p <- p
  
  q
  p
  
  # assess whether the new jump is good!
  
  # draw from target distribution (= U)
  
  
  # gradient from target distribution(= U_gradient)
  #newprob.grad <- numDeriv:::grad(f=dmvnorm, x=c(newx,newy), sigma=sigmamat, log=TRUE)
  
  #Q <- HMC2(U, U_gradient, epsilon = step, L = L, current_q = Q$q)
  # HMC2
  #function (U, grad_U, epsilon, L, current_q, ...) 
  #{
  #...
  #}
  
  #p = p - epsilon * grad_U(q, ...)/2
  p = p - epsilon * numDeriv:::grad(f=dmvnorm, x=q, sigma=sigmamat, log=TRUE) / 2
  
  #qtraj <- matrix(NA, nrow = L + 1, ncol = length(q))
  #ptraj <- qtraj
  #qtraj[1, ] <- current_q
  #ptraj[1, ] <- p
  
  # leapfrog integrator
  for (ii in 1:L)
  {
    q = q + epsilon * p
    if (ii != L) {
      #p = p - epsilon * grad_U(q, ...)
      p = p - epsilon * numDeriv:::grad(f=dmvnorm, x=q, sigma=sigmamat, log=TRUE)
      #ptraj[i + 1, ] <- p
    }
    #qtraj[i + 1, ] <- q
  }
  
  #p = p - epsilon * grad_U(q, ...)/2
  p = p - epsilon * numDeriv:::grad(f=dmvnorm, x=q, sigma=sigmamat, log=TRUE) / 2
  
  #ptraj[L + 1, ] <- p
  p = -p
  
  #current_U = U(current_q, ...)
  current_U = -dmvnorm(x=current_q, sigma=sigmamat, log=TRUE)
  current_K = sum(current_p^2)/2
  
  proposed_U = -dmvnorm(x=q, sigma=sigmamat, log=TRUE)
  proposed_K = sum(p^2)/2
  
  H0 <- current_U + current_K
  H1 <- proposed_U + proposed_K
  
  new_q <- NA
  accept <- NA
  
  if(runif(1) < exp(current_U - proposed_U + current_K - proposed_K))
  {
    new_q <- q
    accept <- 1
  } else
  {
    new_q <- current_q
    accept <- 0
  }
  #return(list(q = new_q, traj = qtraj, ptraj = ptraj, accept = accept, 
  #            dH = H1 - H0))
  #return(list(q = new_q, accept = accept, dh = H1 - H0))
  
  new_q
  accept
  
  dH.temp <- H1-H0
  
  ### end of HMC2
  
  # actual test routing with the outcome of the HMC proposal routine
  r <- min(abs(dH.temp), 1)
  dH[i] <- dH.temp
  a[i] <- accept
  if (a[i] == 1)
  {
    current_q <- post[i, ] <- new_q
    i <- i + 1
  }
}  
head(post)
head(a)
plot(post)

##########################


library(rethinking)

seed <- 9988776
seed <- 996 #with stepsize 0.03 bad example!
set.seed(seed)

rho <- 0.8
sigmamat <- matrix(c(1,rho,rho,1),ncol=2)
sigmamat
#sigmamat <- matrix(c(4,2,2,3), ncol=2)
#sigmamat

grad_U <- function(q, ...) -numDeriv:::grad(f=dmvnorm, x=q, sigma=sigmamat, log=TRUE)
U <- function(q, ...) -dmvnorm(x=q, sigma=sigmamat, log=TRUE)

L <- 11 #10
#WRONG
step <- 0.03 #0.03
step <- 0.1

# pretty exact
#L <- 10 #10
#step <- 0.03 #0.03

Q <- list()
Q$q <- c(0,0)

U(Q$q)
grad_U(Q$q)
Q

nsamp <- 3e4 #1e4
nsamp <- 3e3
nsamp <- 3e3
nsamp <- 1e3

coln <- 2+1+1
Qres <- as.data.frame(matrix(NA, nrow=nsamp, ncol=coln))
colnames(Qres) <- c("q1","q2","a","dH")
Qres[1,c(1:2)] <- Q$q
head(Qres)

for (i in 1:nsamp)
{
  # print(i)
  Q <- HMC2(U, grad_U, epsilon=step, L=L, current_q=Q$q)
  Qres[i,"dH"] <- Q$dH
  Qres[i,"a"] <- Q$accept
  if(Q$a == 1) Qres[i,c(1:2)] <- Q$q
}

head(Qres)
dim(Qres)
apply(Qres,2,mean, na.rm=TRUE)
cov(Qres[,c(1:2)], use="complete.obs")
length(is.na(Qres[,"a"]))/dim(Qres)[1]

post <- Qres[,c(1:2)]
## example McElreath rethinking HMC_2D_sample
#res <- HMC_2D_sample( n=1000 , U=U_funnel , U_gradient=U_funnel_gradient , 
#                      step=0.2 , L=10 , ylab="v"  , adj_lvls=1/12 )
#sum(!is.na(res)+0) / length(res)

dim(post)
head(post)
sum(!is.na(post)+0) / length(post)
method <- "dmvnorm"

#post <- post[-c(1:500),]
# plot bivariate posterior with ellipses
dim(post)
radius <- sqrt(qchisq(.5,2))
plot(post, main=paste("Bivariate normal distribution [method=",method,"]",sep=""),
     pre.plot=grid(), type="p", bty="n",
     xlab="v1", ylab="v2", col="olivedrab")
Xbar <- apply(post,2,mean, na.rm=TRUE)
Xbar
S <- cov(post, use="complete.obs")
S
apply(post,2,sd, na.rm=TRUE)
library(car)
ellipse(center=Xbar, shape=S, radius=radius, col="blue")
ellipse(center=c(0,0), shape=sigmamat, radius=radius, col="darkred", lty=2)

# compare with simulated from multivariate normal distribution
rmv.sim <- mvtnorm:::rmvnorm(n=1000, mean=c(0,0), sigma=sigmamat)
str(rmv.sim)
Xbar.sim <- apply(rmv.sim, 2, mean)
Xbar.sim
SD.sim <- apply(rmv.sim, 2, sd)
SD.sim
cov.sim <- cov(rmv.sim)
cov.sim
points(rmv.sim, col="red")
ellipse(center=Xbar.sim, shape=cov.sim, radius=radius, col="black")


# single run
#HMC2(U, grad_U, epsilon=0.03, L=8, current_q=c(0,0), sigmamat)

# plot MCMC

# diagnostics MCMC

# investigate acceptance rate

# 3d plot bivariate normal distribution

# investigate Q

# investigate leapfrogs

# investigate dH

# simulate and draw
post <- HMC_2D_sample(n=5, U=U, U_gradient=grad_U, step=0.1, L=11, start=c(0,0))
post
HMC_2D_sample(n=15, U=U, U_gradient=grad_U, step=0.1, L=11, start=c(0,0))
HMC_2D_sample(n=25, U=U, U_gradient=grad_U, step=0.1, L=11, start=c(0,0))
HMC_2D_sample(n=99, U=U, U_gradient=grad_U, step=0.1, L=11, start=c(0,0))
HMC_2D_sample(n=100, U=U, U_gradient=grad_U, step=0.1, L=11, start=c(0,0))
HMC_2D_sample(n=150, U=U, U_gradient=grad_U, step=0.1, L=11, start=c(0,0))
HMC_2D_sample(n=1000, U=U, U_gradient=grad_U, step=0.1, L=11, start=c(0,0))


############################
# HMC

# with Neal's code (s.a.) and modifications/ comments
# good to lean the basic principle

# U is a function that returns the potential energy given q
# grad_U returns the respective partial derivatives
# epsilon stepsize
# L number of leapfrog steps
# current_q current position

# kinetic energy is assumed to be sum(p^2/2) (mass == 1)
HMC <- function (U, grad_U, epsilon, L, current_q, ...)
{
  q <- current_q
  # independent standard normal variates
  p <- rnorm(length(q), 0, 1)  
  # Make a half step for momentum at the beginning
  current_p <- p 
  # Alternate full steps for position and momentum
  p <- p - epsilon * grad_U(q, ...) / 2 
  for (ii in 1:L) {
    # Make a full step for the position
    q <- q + epsilon * p
    # Make a full step for the momentum, except at end of trajectory
    if (ii != L) p <- p - epsilon * grad_U(q, ...)
  }
  # Make a half step for momentum at the end
  p <- p - epsilon * grad_U(q, ...) / 2
  # Negate momentum at end of trajectory to make the proposal symmetric
  p <- -p
  # Evaluate potential and kinetic energies at start and end of trajectory 
  current_U <- U(current_q, ...)
  current_K <- sum(current_p^2) / 2
  proposed_U <- U(q, ...)
  proposed_K <- sum(p^2) / 2
  # Accept or reject the state at end of trajectory, returning either
  # the position at the end of the trajectory or the initial position
  if (runif(1) < exp(current_U-proposed_U+current_K-proposed_K)) {
    return (q)  # accept
  } else {
    return (current_q)  # reject
  }
}


###
library(mvtnorm)
nsamp <- 1e4

rho <- 0.8
sigmamat <- matrix(c(1,rho,rho,1),ncol=2)
sigmamat
#sigmamat <- matrix(c(4,2,2,3), ncol=2)
#sigmamat

grad_U <- function(q, ...) -numDeriv:::grad(f=dmvnorm, x=q, sigma=sigmamat, log=TRUE)
U <- function(q, ...) -dmvnorm(x=q, sigma=sigmamat, log=TRUE)

L <- 11 #10
step <- 0.03 #0.03

Q <- list()
Q$q <- c(0,0)
postQ <- matrix(NA, nrow=nsamp, ncol=2)
postQ[1, ] <- c(0,0)
HMC(U, grad_U, epsilon=step, L=L, current_q=Q$q)
seed <- 9988776
set.seed(seed)
for (i in 1:nsamp)
{
  #print(i)
  #  print(Q$q)
  Q$q <- postQ[i, ] <- HMC(U, grad_U, epsilon=step, L=L, current_q=Q$q, sigmamat=sigmamat)
}
#######################
apply(postQ,2,mean)
apply(postQ,2,sd)
cov(postQ)
hist(postQ)
plot(postQ)






############### BOOK CODE


# bivariate probability density function

# N(x,mu,sigma) = 
ND.pdf <- function(x, mu, sigmamat)
{
  (2*pi)^(-1) * det(sigmamat)^(-1/2) * exp(-1/2 * t(x-mu) %*% solve(sigmamat) %*% (x-mu))
}

# correlation
rho <- 0.8
# sigmas
sigma1 <- 1
sigma2 <- 1
# Sigma matrix
sigmamat <- matrix(c(sigma1,rho*sigma1*sigma2,rho*sigma1*sigma2,sigma2),ncol=2)
sigmamat
#sigmamat <- matrix(c(4,2,2,3), ncol=2)
#sigmamat

# mu
mu <- c(1,1)

# point where to evaluate function
x <- c(0.5,0.4)

start_time <- Sys.time()
ND.pdf(x,mu,sigmamat)
end_time <- Sys.time()
end_time - start_time

start_time <- Sys.time()
dmvnorm(x,mean=mu,sigma=sigmamat)
end_time <- Sys.time()
end_time - start_time

# check gradient
numDeriv:::grad(f=dmvnorm, x=x, mean=mu, sigma=sigmamat)
numDeriv:::grad(f=ND.pdf, x=x ,mu=mu, sigmamat=sigmamat)
rhmc:::num_grad(f=ND.pdf, x=x, mu=mu, sigmamat=sigmamat)
num_grad2(f=ND.pdf, x=x, mu=mu, sigmamat=sigmamat)

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

### HMC with HMC2()
library(rethinking)

seed <- 9988776
seed <- 996 # with stepsize 0.03 bad example!

# we define two functions for the HMC sampling
# negative log likelihood for U
U <- function(q, ...) -dmvnorm(x=q, sigma=sigmamat, log=TRUE)
# gradient of U by making use of grad() from numDeriv
grad_U <- function(q, ...) -numDeriv:::grad(f=dmvnorm, x=q, sigma=sigmamat, log=TRUE)

# number of leapfrogs
L <- 11 #10

# step size epsilon
#WRONG step <- 0.03
step <- 0.1

# define a temporary list to store the Q results from HMC2()
Q <- list()

# initial value to start MCMC
Q$q <- c(0,0)

# try out functions whether they work
U(Q$q)
grad_U(Q$q)
Q

# MCMC repetitions ie. sample size
nsamp <- 3e4
nsamp <- 1e4
nsamp <- 1e3

# number of MCMC chains
nchains <- 5

#
coln <- length(Q$q)+1+1
Qres <- matrix(NA, nrow=nsamp, ncol=coln)
colnames(Qres) <- c("q1","q2","a","dH")
Qres[1,c(1:2)] <- Q$q
head(Qres)

# start random number generator initial value
set.seed(seed)

OUTmcmc <- list()

for(z in 1:nchains)
{
  print(z)
  
  Q <- list()
  Q$q <- c(0,0)
  
  for (i in 1:nsamp)
  {
    # print(i)
    Q <- HMC2(U, grad_U, epsilon=step, L=L, current_q=Q$q)
    Qres[i,"dH"] <- Q$dH
    Qres[i,"a"] <- Q$accept
    if(Q$a == 1) Qres[i,c(1:2)] <- Q$q
  }
  
  OUTmcmc[[z]] <- Qres
}

##########
# start random number generator initial value

# epsilon
step <- 0.1
# number of leapfrogs
L <- 11
#mu <- c(0,0)
# initial value for q
Qinitv <- c(0,0)
# number of samples per MCMC chain
nsamp <- 1e3
# number of MCMC chains
nchains <- 5
posty <- bivarnormdist.HMC.sim(U=U, grad_U=grad_U, epsilon=step, L=11,
                               nchains=nchains, Qinitv=Qinitv,
                               nsamp=nsamp,
                               seeds=seeds #c(9988776, 996, 345, 321, 12399)
)
str(posty)
OUTmcmc <- posty 

# compare with BAD starting values
step <- 0.03
L <- 11
step
L
Qinitv
nsamp <- 3e4
nsamp
seeds <- c(9988776, 996, 345, 321, 12399)
seeds
nchains <- 5
posty2 <- bivarnormdist.HMC.sim(U=U, grad_U=grad_U, epsilon=step, L=L,
                                nchains=5, Qinitv=Qinitv,
                                nsamp=nsamp,
                                seeds=seeds
)
#posty2.BP <- posty2
str(posty2)
head(posty2[[1]])
OUTmcmc <- posty2


# compare with GOOD starting values
step <- 0.1
L <- 11
step
L
Qinitv
nsamp <- 3e4
nsamp
seeds <- c(9988776, 996, 345, 321, 12399)
seeds
nchains <- 5
posty3 <- bivarnormdist.HMC.sim(U=U, grad_U=grad_U, epsilon=step, L=L,
                                nchains=nchains, Qinitv=Qinitv,
                                nsamp=nsamp,
                                seeds=seeds
)
post3.BP <- posty3
str(posty3)
head(posty3[[1]])
save(posty3, file="posty3_steps01.RDAta")

OUTmcmc <- posty3



# variability GOOD ONE
step <- 0.1
L <- 11
step
L
Qinitv
nsamp <- 1e3
nsamp
seeds <- c(9988776, 996, 345, 321, 12399,
           395, 350, 840, 382, 573,
           242, 891, 385, 680, 606,
           770, 913, 795, 670, 736
)
length(seeds)
seeds
nchains <- 20
nchains
posty4 <- bivarnormdist.HMC.sim(U=U, grad_U=grad_U, epsilon=step, L=L,
                                nchains=nchains, Qinitv=Qinitv,
                                nsamp=nsamp,
                                seeds=seeds
)
post4.BP <- posty4
str(posty4)
head(posty4[[1]])
save(posty4, file="posty4_steps01_nsamps-1e3_nchains-20.RDAta")

OUTmcmc <- posty4

# variability BAD ONE
step <- 0.03
L <- 11
step
L
Qinitv
nsamp <- 1e3
nsamp
seeds <- c(9988776, 996, 345, 321, 12399,
           395, 350, 840, 382, 573,
           242, 891, 385, 680, 606,
           770, 913, 795, 670, 736
)
length(seeds)
seeds
nchains <- 20
nchains
posty5 <- bivarnormdist.HMC.sim(U=U, grad_U=grad_U, epsilon=step, L=L,
                                nchains=nchains, Qinitv=Qinitv,
                                nsamp=nsamp,
                                seeds=seeds
)
post5.BP <- posty5
str(posty5)
head(posty5[[1]])
save(posty5, file="posty5_steps003_nsamps-1e3_nchains-20.RDAta")

OUTmcmc <- posty5


# variability BAD ONE
step <- 0.1
L <- 11
step
L
Qinitv
nsamp <- 3e3
nsamp
seeds <- c(9988776, 996, 345, 321, 12399,
           395, 350, 840, 382, 573,
           242, 891, 385, 680, 606,
           770, 913, 795, 670, 736
)
length(seeds)
seeds
nchains <- 20
nchains
posty6 <- bivarnormdist.HMC.sim(U=U, grad_U=grad_U, epsilon=step, L=L,
                                nchains=nchains, Qinitv=c(1,1),
                                nsamp=nsamp,
                                seeds=seeds, sigma=sigmamat
)
post6.BP <- posty6
str(posty6)
head(posty6[[1]])
save(posty6, file="posty6_steps003_nsamps-3e3_nchains-20_Qinitv-1-1.RDAta")

OUTmcmc <- posty6


############

#posty2 nsamp 3e4 chains 5 step 0.03 L 11
#posty3 nsamp 3e4 chains 5 step 0.1 L 11

#posty4 nsamp 1e3 chains 20 step 0.03 L 11
#posty5 nsamp 1e3 chains 20 step 0.1 L 11

############
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
############


#k = 2; n=3; m = 4
#array(NA, c(n,m,k))

# inspect results
str(OUTmcmc)

# bayesplot package requires chains to be of the same length
# so we delete all NA rows and delete from all chains
# the same rows which therefor is a random delete
# there may be a better way like sampling till the iterations
# are fully complete with full valid observations
# and delete then NAs so it comes out for all chains to have
# the same number of rows (iterations) if cols (= vars)
NA.IDs <- lapply(OUTmcmc, function(x) which(is.na(x), arr.ind=TRUE))
NA.IDs
#unlist(lapply(NA.IDs, length))
NA.rowIDs <- unique(unlist(lapply(NA.IDs, function(x) x[,1])))
NA.rowIDs
NA.rowIDs.l <- length(NA.rowIDs)
NA.rowIDs.l
NA.rowIDs.diffs <- NA.rowIDs[2:NA.rowIDs.l]-NA.rowIDs[1:(NA.rowIDs.l-1)]


library(unikn)
hist(NA.rowIDs.diffs, col=usecol(pal_bordeaux, n=6), main="Samples between NAs (= non-acceptance)", prob=TRUE, xlab="samples", ylab="prob")
lines(density(NA.rowIDs.diffs), col="blue", lwd=2)

c(summary(NA.rowIDs.diffs), sd=sd(NA.rowIDs.diffs))
# rate a
NA.rowIDs.l/nsamp

OUTmcmc.nonas <- list()

if(length(NA.rowIDs) > 0)
{
  for(i in 1:nchains)
  {
    #OUTmcmc.nonas[[i]] <- OUTmcmc[[i]][-NA.IDs[[i]][,1],c("q1","q2")]
    OUTmcmc.nonas[[i]] <- OUTmcmc[[i]][-NA.rowIDs,c("q1","q2")]
  } 
} else OUTmcmc.nonas <- OUTmcmc

lapply(OUTmcmc.nonas, function(x) which(is.na(x), arr.ind=TRUE))

head(OUTmcmc.nonas[[1]])
str(OUTmcmc.nonas)


# remove burnins if required
removeburnins <- FALSE
dim(OUTmcmc.nonas[[1]])
burnins <- 500
OUTmcmc.nonas.noburnins <- lapply(OUTmcmc.nonas, function(x) x[-c(1:burnins),]) 
dim(OUTmcmc.nonas.noburnins[[1]])
if(removeburnins) OUTmcmc <- OUTmcmc.nonas.noburnins else OUTmcmc <- OUTmcmc.nonas


# everything
OUTmcmc.nonas.onlyqs <- OUTmcmc.nonas

# only q's
OUTmcmc.nonas.onlyqs <- lapply(OUTmcmc.nonas, function(x) x[,c("q1","q2")])
str(OUTmcmc.nonas.onlyqs)


# convert to mcmc object and mcmc list to use coda library
library(coda)
#OUTmcmc.list <- as.mcmc.list(lapply(OUTmcmc.nonas, mcmc))
OUTmcmc.list <- as.mcmc.list(lapply(OUTmcmc.nonas.onlyqs, mcmc))
summary(OUTmcmc.list)

# output with only 3 digits
options(digits=3)

# descriptive statistics per chain
str(OUTmcmc)
str(OUTmcmc.nonas)

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

MCMCout.desc.per.chain(OUTmcmc.nonas.onlyqs, nchoose=c(1,2))
mcmc.perchain.means <- lapply(MCMCout.desc.per.chain(OUTmcmc.nonas.onlyqs, nchoose=c(1,2)), function(x) x[,"Mean"])
mcmc.perchain.means
lapply(mcmc.perchain.means, range)

# descriptive statistics over all chains
MCMCout.desc.all.chain <- function(res, nchoose=c(1,2))
{
  #rnams <-  colnames(res[[1]])
  rnams <- attr(res[[1]],"dimnames")[[2]]
  resi <- lapply(1:2, function(y) unlist(lapply(res, function(x) x[,y])))
  resi <- t(sapply(resi, function(x) c(summary(x),sd=sd(x), var=var(x))))
  rownames(resi) <- rnams[nchoose]
  return(resi)
}  

MCMCout.desc.all.chain(OUTmcmc.nonas.onlyqs)


# covariance matrices per chain
lapply(OUTmcmc.nonas.onlyqs, cov)

# covariance matrices over all chains
cov(do.call("rbind", OUTmcmc.nonas.onlyqs))


# quantiles per chain
quans <- c(0,0.05,0.1,0.25,0.5,0.75,0.87,0.9,0.95,0.99,1)
do.call("rbind", lapply(OUTmcmc.nonas.onlyqs, quantile, probs=quans))

# quantiles over all chains
apply(do.call("rbind", OUTmcmc.nonas.onlyqs),2,quantile, probs=quans)


library(bayesplot)
# plot posteriors and chains


# trace plot
color_scheme_set("mix-blue-pink")
bayesplot:::mcmc_trace(OUTmcmc.nonas)

# histogram per chain
bayesplot:::mcmc_hist_by_chain(OUTmcmc.nonas)

# histogram for all chains
color_scheme_set("green")
bayesplot:::mcmc_hist(OUTmcmc.nonas)

# density for all chains
color_scheme_set("purple")
bayesplot:::mcmc_dens(OUTmcmc.nonas)

# density lines for each chain
color_scheme_set("blue")
bayesplot:::mcmc_dens_overlay(OUTmcmc.nonas)

# violine plot
color_scheme_set("teal")
color_scheme_set("pink")
bayesplot:::mcmc_violin(OUTmcmc.nonas, probs=c(0.1, 0.5, 0.9))


# Gelman
gelman.plot(OUTmcmc.list)
gelman.diag(OUTmcmc.list)


# Geweke
geweke.plot(OUTmcmc.list)
geweke.diag(OUTmcmc.list)
geweke.OUTmcmc.list.z <- do.call("rbind",lapply(geweke.diag(OUTmcmc.list), function(x) x$z))
geweke.OUTmcmc.list.frac <- do.call("rbind",lapply(geweke.diag(OUTmcmc.list), function(x) x$frac))
colnames(geweke.OUTmcmc.list.frac) <- c("frac 1st", "frac 2nd")
geweke.OUTmcmc.list <- cbind(geweke.OUTmcmc.list.z, geweke.OUTmcmc.list.frac)
geweke.OUTmcmc.list


# Heidelberger-Welch
str(OUTmcmc.list)
out <- mcmc.list((lapply(OUTmcmc.list, function(x) mcmc(x[28000:29918,]) )))
str(out)
heidel.diag(out, eps=0.1)

heidelbergwelch.OUTmcmc.list <- cbind(chain=rep(1:nchains,each=2),do.call("rbind",lapply(heidel.diag(OUTmcmc.list), function(x) x[1:2,1:6])))
heidelbergwelch.OUTmcmc.list

summary(OUTmcmc.list)

# determine eps for passing
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

str(OUTmcmc.list)
eps.det.OUTmcmc.list.q1 <- data.frame(t(sapply(1:nchains, function(x) heidel.eps.det(OUTmcmc.list[[x]][,"q1"]))))
eps.det.OUTmcmc.list.q2 <- data.frame(t(sapply(1:nchains, function(x) heidel.eps.det(OUTmcmc.list[[x]][,"q1"]))))

eps.det.OUTmcmc.list.q1
eps.det.OUTmcmc.list.q2


# non-empirical example
# division by a value near zero
CI.width <- 0.1
mean.emp <- 0.003
epsilon <- 0.1
ratio.CI.mean <- 0.5*CI.width/mean.emp
ratio.CI.mean
ratio.CI.mean < epsilon


# simulate simple normal distribution
set.seed(seeds[1])
nd.data <- rnorm(n=1e6)
summary(nd.data)
sd(nd.data)
heidel.diag(nd.data)
eps.det.nd.data <- heidel.eps.det(nd.data)
eps.det.nd.data
# plot
hist(nd.data,prob=T, pre.plot=grid(),main="Simulation normal distribution", ylim=c(0,0.4))
lines(density(nd.data), col="darkred")
sek <- seq(-3,3,0.01)
lines(sek,dnorm(sek),col="blue")
# repeat wird ndata+1
heidel.diag(nd.data+1)

# repeat auf original data with +1
str(OUTmcmc.list)
OUTmcmc.nonas.onlyqs.plus1 <- lapply(OUTmcmc.nonas.onlyqs, function(x) x+1)
OUTmcmc.list.plus1 <- as.mcmc.list(lapply(OUTmcmc.nonas.onlyqs.plus1, mcmc))
summary(OUTmcmc.list.plus1)
heidelbergwelch.OUTmcmc.list.plus1 <- cbind(chain=rep(1:nchains,each=2),do.call("rbind",lapply(heidel.diag(OUTmcmc.list.plus1), function(x) x[1:2,1:6])))
heidelbergwelch.OUTmcmc.list.plus1


# Raftery-Lewis
raftery.diag(OUTmcmc.list)
raftery.OUTmcmc.list <- cbind(chain=rep(1:nchains,each=2),do.call("rbind",lapply(raftery.diag(OUTmcmc.list), function(x) x$resmatrix)))
raftery.OUTmcmc.list


# effective sample size
# per chain
# we have to deal with two parameters, so we are
# only interested in the number of rows
OUTmcmc.list.dim <- sapply(OUTmcmc.list, dim)
OUTmcmc.list.l <- OUTmcmc.list.dim[1,]
OUTmcmc.list.dim
OUTmcmc.list.l
# =
sapply(OUTmcmc.list, nrow)
# ESS per chain
ESS.per.chain <- sapply(OUTmcmc.list, effectiveSize)
ESS.per.chain
# ESS over all chains
apply(sapply(OUTmcmc.list, effectiveSize),1,sum)
# =
# overall
effectiveSize(OUTmcmc.list)
# ratio compared to actual empirical sample size
OUTmcmc.list.l/ESS.per.chain
# inverse
1/(OUTmcmc.list.l/ESS.per.chain)
# =
sapply(OUTmcmc.list, effectiveSize)/OUTmcmc.list.l
# = design effect
1/ ( effectiveSize(OUTmcmc.list)/sum(OUTmcmc.list.l) )
# ~
apply(OUTmcmc.list.l/ESS.per.chain,1,mean)


# autocorrelation
# first MCMC chain
autocorr.diag(OUTmcmc.list[[1]])
autocorr.plot(OUTmcmc.list[[1]])
# all MCMC chains
autocorr.diag(OUTmcmc.list)
autocorr.plot(OUTmcmc.list)


# cross correlation of values
# pre chain
lapply(OUTmcmc.list, cor)
# over all chains
cor(do.call("rbind",OUTmcmc.list))
crosscorr.plot(OUTmcmc.list)


# plot bivariate normal dist plot with ellipses
# draw ellipses

library(mixtools)

# take on MCMC chain
post <- OUTmcmc.nonas.onlyqs[[1]]
method <- "dmvnorm"
# calculate limits for the plot
limits <- ceiling(abs(apply(post,2,range)))
limits[1,] <- limits[1,]*(-1)
colnams <- colnames(post)
# theoretical mean
mu <- c(0,0)
# empirical mean and covariance matrix
Xbar <- apply(post,2,mean)
S <- cov(post)
plot(NULL, xlim=limits[,"q1"], ylim=limits[,"q2"], pre.plot=grid(), bty="n", xlab=colnams[1], ylab=colnams[2])
points(post, col="olivedrab", bg="orange", pch=21, cex=0.7)
# draw ellipse based on empirical values
mixtools:::ellipse(mu=Xbar, sigma=S, alpha=0.05, col="blue", lwd=2)
# draw ellipse based on theoretical values
mixtools:::ellipse(mu=mu, sigma=sigmamat, alpha=0.05, col="magenta", lwd=2)

# compare with simulated from multivariate normal distribution
nsim <- 1e3
rmv.sim <- mvtnorm:::rmvnorm(n=nsim, mean=mu, sigma=sigmamat)
# empirical mean and covariance matrix
Xbar.sim <- apply(rmv.sim, 2, mean)
S.sim <- cov(rmv.sim)
points(rmv.sim, col="skyblue", bg="yellow", pch=21, cex=0.7)
# draw ellipse based on empirical values
mixtools:::ellipse(mu=Xbar.sim, sigma=S.sim, alpha=0.05, col="black", lwd=2, lty=2)
mtext("Bivariate normal distribution (HMC simulation)", side=3, cex=2)


# single run
# simulate and draw
# review start values
# stepsize
step
# number of leapfrogs
L
# initial value
Qinitv
# functions
U <- function(q, ...) -mvtnorm:::dmvnorm(x=q, sigma=sigmamat, log=TRUE)
U(mu)
grad_U <- function(q, ...) -numDeriv:::grad(f=mvtnorm:::dmvnorm, x=q, sigma=sigmamat, log=TRUE)
# covar matrix
sigmamat

par(ask=TRUE)
samplesn <- c(5,15,25,99,100)
for(i in samplesn) HMC_2D_sample(n=i, U=U, U_gradient=grad_U, step=step, L=L, start=Qinitv, sigma=sigmamat)

HMC_2D_sample(n=5, U=U, U_gradient=grad_U, step=step, L=L, start=Qinitv, sigma=sigmamat)
HMC_2D_sample(n=15, U=U, U_gradient=grad_U, step=step, L=L, start=Qinitv, sigma=sigmamat)
HMC_2D_sample(n=25, U=U, U_gradient=grad_U, step=step, L=L, start=Qinitv, sigma=sigmamat)
HMC_2D_sample(n=99, U=U, U_gradient=grad_U, step=step, L=L, start=Qinitv, sigma=sigmamat)
HMC_2D_sample(n=100, U=U, U_gradient=grad_U, step=step, L=L, start=Qinitv, sigma=sigmamat)
HMC_2D_sample(n=150, U=U, U_gradient=grad_U, step=step, L=L, start=Qinitv, sigma=sigmamat)
HMC_2D_sample(n=1000, U=U, U_gradient=grad_U, step=step, L=L, start=Qinitv, sigma=sigmamat)


# investigate acceptance rate
# see NAs



# load huge dataset
load(file="posty2_mu-0-0_rho08_steps003_L11_nchains5_nsamp3e4.RData")
str(posty2)




# development of mean and covariance
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

#str( MCMCout.cumdesc.per.chain(OUTmcmc.nonas.onlyqs[[1]]) )

MCMCout.cs.descs <- lapply(OUTmcmc.nonas.onlyqs, MCMCout.cumdesc.per.chain)
str(MCMCout.cs.descs)


# plot development of HMC MCMC
#temp <- MCMCout.cs.descs
#str(temp)

# take only part of the MCMC chain
outtake <- c(start=1,end=nsamp)
outtake <- c(start=1,end=2500)
outtake <- c(start=1,end=5000)
outtake <- c(start=2e4,end=3e4)
outtake

daten <- MCMCout.cs.descs[[1]][[1]][,"q1"][outtake["start"]:outtake["end"]]
str(daten)
head(daten)

#### check for same sign and compare values

# range both < comp.v -> range2 = comp.v
# range both > comp.v -> range1 = comp.v
# range1 < comp.v < range2 -> all OK

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
  #if(comp.v == 0 & sum((daten.r != comp.v)+0) == 2)
  #{
  #  if(sum((daten.r > comp.v)+0) == 2) daten.r[1] <- comp.v
  #  if(sum((daten.r < comp.v)+0) == 2) daten.r[2] <- comp.v
  #} else
  #{
  #  IDtocompare <- which(sign(daten.r) %in% sign(c(comp.v)))
  #  if(daten.r[IDtocompare] < comp.v) daten.r[IDtocompare] <- comp.v*fac
  #}
  cat("\nnew range:\t",daten.r[1],"\t",daten.r[2],"\n",sep="")
  return(invisible(daten.r))
}

# examples
adj.limits(daten=MCMCout.cs.descs[[1]][[1]][,"q1"][outtake["start"]:outtake["end"]], comp.v=0)

adj.limits(daten=MCMCout.cs.descs[[1]][[2]][outtake["start"]:outtake["end"]], comp.v=rho)


par(mfrow=c(2,2))
mu
ylimits <- adj.limits(daten=MCMCout.cs.descs[[1]][[1]][,"q1"][outtake["start"]:outtake["end"]], comp.v=mu[1])
plot(outtake["start"]:outtake["end"],
     MCMCout.cs.descs[[1]][[1]][,"q1"][outtake["start"]:outtake["end"]],
     ylim=ylimits,
     type="l", bty="n", pre.plot=grid(), col="darkred", ylab="q1", xlab="nsamp")
abline(h=mu[1], col="darkgreen", lty=2)

plot(outtake["start"]:outtake["end"],
     MCMCout.cs.descs[[1]][[1]][,"q2"][outtake["start"]:outtake["end"]],
     ylim=adj.limits(daten=MCMCout.cs.descs[[1]][[1]][,"q2"][outtake["start"]:outtake["end"]], comp.v=mu[2]),
     type="l", bty="n", pre.plot=grid(), col="darkred", ylab="q2", xlab="nsamp")
abline(h=mu[2], col="darkgreen", lty=2)

#ylimits <- range(MCMCout.cs.descs[[1]][[2]])
#ylimits <- adj.limits(daten=MCMCout.cs.descs[[1]][[2]][outtake["start"]:outtake["end"]],
#                      comp.v=0)
#range.adj
#IDtocompare <- which(sign(ylimits) %in% sign(c(rho,rho)))
#fac <- 1.15
#if(ylimits[IDtocompare] < rho) ylimits[IDtocompare] <- rho*fac
#ylimits
#rho

plot(MCMCout.cs.descs[[1]][[2]][outtake["start"]:outtake["end"]],
     ylim=adj.limits(daten=MCMCout.cs.descs[[1]][[2]][outtake["start"]:outtake["end"]], comp.v=rho),
     type="l", bty="n", pre.plot=grid(), col="skyblue", ylab="cov(q1, q2)", xlab="nsamp")
abline(h=rho, col="darkgreen", lty=2)


# plot MCMCs - mean per chain and per variable and overall
fac <- 1
# define colors


library(RColorBrewer)
colos <- brewer.pal(nchains,"Oranges")

colos <- rainbow(nchains)
colos

colos1 <- usecol(pal_bordeaux, n=nchains)
colos2 <- usecol(pal_seegruen, n=nchains)
colos3 <- usecol(pal_petrol, n=nchains)


nsamp
outtake

nchains

str(MCMCout.cs.descs)
MCMCout.cs.descs


#
lengs <- sapply(MCMCout.cs.descs[[1]],length)
# length for q1, q2
q1q2.length <- lengs[1]/length(mu)
# length for cov matrix
cov.length <- lengs[2]
q1q2.length
cov.length

# take only part of the MCMC chain
#q1, q2
outtake.q1q2 <- c(start=1,end=nsamp)
outtake.q1q2 <- c(start=1,end=2500)
outtake.q1q2 <- c(start=500,end=2500)
outtake.q1q2 <- c(start=1,end=5000)
outtake.q1q2 <- c(start=2e4,end=q1q2.length)
outtake.q1q2 <- c(start=25000,end=q1q2.length)

#cov
outtake.cov <- c(start=1,end=nsamp)
outtake.cov <- c(start=1,end=2500)
outtake.cov <- c(start=500,end=2500)
outtake.cov <- c(start=1,end=5000)
outtake.cov <- c(start=2e4,end=cov.length)
outtake.cov <- c(start=25000,end=cov.length)

outtake.q1q2 <- c(start=1,end=q1q2.length)
outtake.cov <- c(start=1,end=cov.length)
outtake.q1q2
outtake.cov


par(mfrow=c(1,3))
#par(bg="gray97")
par(oma=c(2,2,5,2))

# q1
outtake <- outtake.q1q2
outtake
comp.value <- mu[1]
comp.value
listelement <- 1
daten <- unlist( lapply(MCMCout.cs.descs, function(x) x[[1]][,"q1"][outtake["start"]:outtake["end"]]) )
str(daten)
range(daten, na.rm=TRUE)
ylimits <- adj.limits(daten, comp.v=comp.value)
ylimits
plot(NULL, xlim=c(outtake["start"],outtake["end"]),
     ylim=ylimits,
     type="l", bty="n", pre.plot=grid(col="gray"),
     ylab="q1", xlab="nsamp", main=paste("start=",outtake["start"]," : end=",outtake["end"]," | comp.value=",comp.value,sep=""))
for(i in 1:nchains)
{
  lines(outtake["start"]:outtake["end"],
        MCMCout.cs.descs[[i]][[listelement]][,"q1"][outtake["start"]:outtake["end"]],
        col=colos1[i])
}  
abline(h=comp.value, col="black", lty=2)
mean.per.nsamp <- apply(sapply(MCMCout.cs.descs, function(x) x[[1]][,"q1"]),1,mean)
lines(outtake["start"]:outtake["end"],
      mean.per.nsamp[outtake["start"]:outtake["end"]], col="red", lwd=3)


# q2
outtake <- outtake.q1q2
outtake
comp.value <- mu[2]
comp.value
listelement <- 1
daten <- unlist( lapply(MCMCout.cs.descs, function(x) x[[1]][,"q2"][outtake["start"]:outtake["end"]]) )
str(daten)
range(daten, na.rm=TRUE)
ylimits <- adj.limits(daten, comp.v=comp.value)
ylimits
plot(NULL, xlim=c(outtake["start"],outtake["end"]),
     ylim=ylimits,
     type="l", bty="n", pre.plot=grid(col="gray"),
     ylab="q1", xlab="nsamp", main=paste("start=",outtake["start"]," : end=",outtake["end"]," | comp.value=",comp.value,sep=""))
for(i in 1:nchains)
{
  lines(outtake["start"]:outtake["end"],
        MCMCout.cs.descs[[i]][[listelement]][,"q2"][outtake["start"]:outtake["end"]],
        col=colos2[i])
}  
abline(h=comp.value, col="black", lty=2)
mean.per.nsamp <- apply(sapply(MCMCout.cs.descs, function(x) x[[1]][,"q2"]),1,mean)
lines(outtake["start"]:outtake["end"],
      mean.per.nsamp[outtake["start"]:outtake["end"]], col="red", lwd=3)


# plot MCMCs - cov per chain and per variable and overall
# cov
outtake <- outtake.cov
outtake
comp.value <- rho
comp.value
listelement <- 2
daten <- unlist( lapply(MCMCout.cs.descs, function(x) x[[2]][outtake["start"]:outtake["end"]]) )
str(daten)
range(daten, na.rm=TRUE)
ylimits <- adj.limits(daten, comp.v=comp.value)
ylimits
plot(NULL, xlim=c(outtake["start"],outtake["end"]),
     ylim=ylimits,
     type="l", bty="n", pre.plot=grid(col="gray"),
     ylab="cov", xlab="nsamp", main=paste("start=",outtake["start"]," : end=",outtake["end"]," | comp.value=",comp.value,sep=""))
for(i in 1:nchains)
{
  lines(outtake["start"]:outtake["end"],
        MCMCout.cs.descs[[i]][[2]][outtake["start"]:outtake["end"]],
        col=colos3[i])
}  
abline(h=comp.value, col="black", lty=2)
mean.per.nsamp <- apply(sapply(MCMCout.cs.descs, function(x) x[[2]]),1,mean)
lines(outtake["start"]:outtake["end"],
      mean.per.nsamp[outtake["start"]:outtake["end"]], col="red", lwd=3)

# main title
# outer margins

mtext("HMC simulation (bivariate normal distribution)", side=3, line=1.2, cex=2, outer=TRUE)
subtitletext <- paste("nsamp=",nsamp," | chains=",nchains," | epsilon=",step," | L=",L,sep="")
subtitletext
mtext(subtitletext, side=3, line=-0.5, cex=1.2, outer=TRUE)


# investigate dH
dH.list <- lapply(OUTmcmc, function(x) x[,"dH"])
str(dH.list)
colos.r <- rainbow(nchains)
plot(dH.list[[i]], type="l", bty="n",pre.plot=grid(),
     col=colos.r[1], xlab="nsamp", ylab="dH")
for(i in 2:nchains) points(dH.list[[i]], type="l", col=colos.r[i],)

do.call("rbind", lapply(dH.list, function(x) c(summary(x),sd=sd(x),var=var(x))))



###### comparison with Metropolis Hastings
# 9988776
# 996
seeds
set.seed(seeds[1])

# no neg log like HMC!
U.MH <- function(q, ...) mvtnorm:::dmvnorm(x=q, mean=c(0,0), sigma=sigmamat, log=TRUE)
U.MH(q=c(0.5,0.5))

nsamp <- 3e4

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


############
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
############
OUTmcmc.MH.list <- bivarsim.MH2(U=U.MH, seeds=seeds)
str(OUTmcmc.MH.list)

# single run
a.MH <- OUTmcmc.MH.list[[1]][[2]]
post.MH <- OUTmcmc.MH.list[[1]][[1]]
str(a.MH)
str(post.MH)

# investigate acceptance rate
head(a.MH)
head(post.MH)
length(which(is.na(a.MH)))/length(a.MH)
# =
sum(is.na(a.MH)+0)/length(a.MH)

# effective size ESS
# create coda object
str(OUTmcmc.MH.list)
OUTmcmc.MH.list.codaobj <- as.mcmc.list(lapply(OUTmcmc.MH.list, function(x) mcmc(x[[1]])))
summary(OUTmcmc.MH.list.codaobj)
OUTmcmc.MH.list.codaobj.dim <- sapply(OUTmcmc.MH.list.codaobj, dim)
# per chain
OUTmcmc.MH.list.codaobj.ESS.per.chain <- sapply(OUTmcmc.MH.list.codaobj, effectiveSize)
OUTmcmc.MH.list.codaobj.ESS.per.chain
OUTmcmc.MH.list.codaobj.ESS.per.chain/OUTmcmc.MH.list.codaobj.dim[1,]
apply(OUTmcmc.MH.list.codaobj.ESS.per.chain/OUTmcmc.MH.list.codaobj.dim[1,],1,mean)
# overall
effectiveSize(OUTmcmc.MH.list.codaobj)/sum(OUTmcmc.MH.list.codaobj.dim[1,])

effectiveSize(rnorm(1e7))

# plot acceptance rate
a.MH.nonas <- a.MH
a.MH.nonas[which(is.na(a.MH.nonas))] <- 0
plot(1:length(a.MH), cumsum(a.MH.nonas),
     cex=0.01, type="p", pch=4, col=1:length(a.MH), bty="n", pre.plot=grid(),
     xlab="nsamp", ylab="a")


# investigate post values
dim(post.MH)
head(post.MH)

# means
Xbar.MH <- apply(post.MH,2,mean, na.rm=TRUE)
names(Xbar.MH) <- c("q1","q2")
Xbar.MH
mu-Xbar.MH

# covariance matrix
S.MH <- cov(post.MH, use="complete.obs")
rownames(S.MH) <- colnames(S.MH) <- c("q1","q2")
S.MH
S.MH/sigmamat
# sd
apply(post.MH,2,sd, na.rm=TRUE)

# comparison HMC vs. MH

# means
# per chain
MCMCout.desc.per.chain(OUTmcmc.nonas.onlyqs)
# over all chains
MCMCout.desc.all.chain(OUTmcmc.nonas.onlyqs)
# MH
Xbar.MH

# cov matrices
# per chain
lapply(OUTmcmc.nonas.onlyqs, cov)
# all chains
cov(do.call("rbind", OUTmcmc.nonas.onlyqs))
# MH
S.MH
mu
Xbar.MH
S.MH


library(mixtools)
# take on MCMC chain
method <- "dmvnorm"
limits <- ceiling(abs(apply(post.MH,2,range, na.rm=TRUE)))
limits[1,] <- limits[1,]*(-1)
colnams <- c("q1","q2")
# theoretical mean
mu <- c(0,0)
# empirical mean and covariance matrix
Xbar <- apply(post,2,mean)
S <- cov(post)
plot(NULL, xlim=limits[,1], ylim=limits[,2], pre.plot=grid(), bty="n", xlab=colnams[1], ylab=colnams[2])
points(post.MH, col="blue", bg="yellow", pch=21, cex=0.7)
# draw ellipse based on empirical values
mixtools:::ellipse(mu=Xbar.MH, sigma=S.MH, alpha=0.05, col="orange", lwd=3)
# draw ellipse based on theoretical values
mixtools:::ellipse(mu=mu, sigma=sigmamat, alpha=0.05, col="magenta", lwd=3)
mtext("Bivariate normal distribution (MH simulation)", side=3, cex=2)


# Heidelberger-Welch Test
NA.IDs.MH <- which(is.na(post.MH))
heidel.diag(post.MH[-NA.IDs.MH])
heidel.eps.det(post.MH[-NA.IDs.MH])
# repeat wird post.MH + 1
heidel.diag(post.MH[-NA.IDs.MH] + 1)
# use plotPost from BEST
BEST:::plotPost(post.MH[-NA.IDs.MH], xlab="quantile", ROPE=c(-0.5,0.5), compVal=0, credMass=0.87)
lines(density(post.MH[-NA.IDs.MH]), col="magenta", lwd=3, lty=2)
HDInterval::hdi(post.MH[-NA.IDs.MH], credMass=0.87)


######## NOT RUN below this point
require(rgl)



jet.colors <- colorRampPalette( c("green", "skyblue", "orange", "yellow") ) 
pal <- jet.colors(100)
col.ind <- cut(z,100) # colour indices of each point
x <- seq(-3,3,length=1000)
y <- x

x <- sort(post.MH[,1])
y <- sort(post.MH[,2])
func <- function(x,y) dmvnorm(data.frame(x,y),mean=c(0,0), sigma=sigmamat)
z <- outer(x,y,func)
#persp3d(x,y,z,col=pal[col.ind])


##########################


t(chol(sigmamat)) %*% chol(sigmamat)
solve(sigmamat)
# =
chol2inv(chol(sigmamat))
# =
library(gear)
solve_chol(chol(sigmamat))
##########################


#### handle as ie. acceptance rate
str(OUTmcmc.MH.list)
as <- lapply(OUTmcmc.MH.list, function(x) x[[2]])
str(as)

# look at the tails whether there are NAs
lapply(as,tail)



############ POSSIBLY NOT RUN
# ONLY required if as vector has still NAs after the last successful entry
# get rid of all NAs after the last value!
# which is the last value NOT NAs
reducer.IDs <- sapply(as, function(x) max(which(!is.na(x))))
reducer.IDs

# reduce as by NAs after last value based on reducer.IDs
as.red.list <- lapply(1:nchains, function(x) as[[x]][1:reducer.IDs[x]])
str(as.red.list)
as.red.list.l <- sapply(as.red.list, length)
as.red.list.l

# from here only:
str(as.red.list)
# is relevant!

# length of each MCMC chain
mcmc.MH.l <- sapply(as.red.list, length)
mcmc.MH.l

# reduced factor
as.l <- sapply(as, length)
as.red.list.l/as.l

# number of NAs
NAs.anz <- sapply(as.red.list, function(x) sum(is.na(x)+0))
NAs.anz

# acceptance rate
accept.rate.MH <- 1-(NAs.anz/mcmc.MH.l)
accept.rate.MH
summary(accept.rate.MH, sd=sd(accept.rate.MH))

# factor length extension compared to required number of accepted values
as.l/nsamp
############ END OF POSSIBLY NOT RUN



# number of NAs
NAs.anz <- sapply(as, function(x) sum(is.na(x)+0))
NAs.anz

# acceptance rate
1-(NAs.anz/mcmc.MH.l)

# factor length extension compared to required number of accepted values
as.l <- sapply(as, length)
as.l/nsamp


# handle qs
str(OUTmcmc.MH.list)
OUTmcmc <- lapply(OUTmcmc.MH.list, function(x) x[[1]])
str(OUTmcmc)

# check for NAs - nothing should be there!
NA.IDs <- lapply(OUTmcmc, function(x) which(is.na(x), arr.ind=TRUE))
NA.IDs

OUTmcmc.nonas <- OUTmcmc
# = or immediately
OUTmcmc.nonas <- OUTmcmc.MH.list


############ NOT REQUIRED HERE
# remove burnins if required
removeburnins <- FALSE
dim(OUTmcmc.nonas[[1]])
burnins <- 500
OUTmcmc.nonas.noburnins <- lapply(OUTmcmc.nonas, function(x) x[-c(1:burnins),]) 
dim(OUTmcmc.nonas.noburnins[[1]])
if(removeburnins) OUTmcmc <- OUTmcmc.nonas.noburnins else OUTmcmc <- OUTmcmc.nonas
############ END OF NOT REQUIRED HERE


# everything = here only q's
# take first element of the list which contains the qs
# pre-check
str(OUTmcmc.nonas[[1]])
OUTmcmc.nonas.onlyqs <- lapply(OUTmcmc.nonas, function(x) x[[1]])
str(OUTmcmc.nonas.onlyqs)

# convert to mcmc object and mcmc list to use coda library
library(coda)
lapply(OUTmcmc.nonas.onlyqs, head)
lapply(OUTmcmc.nonas.onlyqs, tail)
OUTmcmc.list <- as.mcmc.list(lapply(OUTmcmc.nonas.onlyqs, mcmc))
summary(OUTmcmc.list)


# bayesplot
library(bayesplot)
# trace plot
color_scheme_set("mix-blue-pink")
bayesplot:::mcmc_trace(OUTmcmc.list)

# density lines for each chain
color_scheme_set("blue")
bayesplot:::mcmc_dens_overlay(OUTmcmc.list)

# violin plot
color_scheme_set("pink")
bayesplot:::mcmc_violin(OUTmcmc.list, probs=c(0.1, 0.5, 0.9))


# cumulative descriptive statistics
MCMCout.cs.descs <- lapply(OUTmcmc.list, MCMCout.cumdesc.per.chain)
str(MCMCout.cs.descs)

lengs <- sapply(MCMCout.cs.descs[[1]],length)
# length for q1, q2
q1q2.length <- lengs[1]/length(mu)
# length for cov matrix
cov.length <- lengs[2]
q1q2.length
cov.length

# take only part of the MCMC chain
#q1, q2
outtake.q1q2 <- c(start=1,end=q1q2.length)
#cov
outtake.cov <- c(start=1,end=cov.length)
outtake.q1q2
outtake.cov

# plot development / evolution of the MCMC chain for qs and covariance
par(mfrow=c(1,3), oma=c(2,2,5,2))

# q1
outtake <- outtake.q1q2
outtake
comp.value <- mu[1]
comp.value
listelement <- 1
daten <- unlist( lapply(MCMCout.cs.descs, function(x) x[[1]][,"q1"][outtake["start"]:outtake["end"]]) )
str(daten)
range(daten, na.rm=TRUE)
ylimits <- adj.limits(daten, comp.v=comp.value)
ylimits
plot(NULL, xlim=c(outtake["start"],outtake["end"]),
     ylim=ylimits,
     type="l", bty="n", pre.plot=grid(col="gray"),
     ylab="q1", xlab="nsamp", main=paste("start=",outtake["start"]," : end=",outtake["end"]," | comp.value=",comp.value,sep=""))
for(i in 1:nchains)
{
  lines(outtake["start"]:outtake["end"],
        MCMCout.cs.descs[[i]][[listelement]][,"q1"][outtake["start"]:outtake["end"]],
        col=colos1[i])
}  
abline(h=comp.value, col="black", lty=2)
mean.per.nsamp <- apply(sapply(MCMCout.cs.descs, function(x) x[[1]][,"q1"]),1,mean)
lines(outtake["start"]:outtake["end"],
      mean.per.nsamp[outtake["start"]:outtake["end"]], col="red", lwd=3)


# q2
outtake <- outtake.q1q2
outtake
comp.value <- mu[2]
comp.value
listelement <- 1
daten <- unlist( lapply(MCMCout.cs.descs, function(x) x[[1]][,"q2"][outtake["start"]:outtake["end"]]) )
str(daten)
range(daten, na.rm=TRUE)
ylimits <- adj.limits(daten, comp.v=comp.value)
ylimits
plot(NULL, xlim=c(outtake["start"],outtake["end"]),
     ylim=ylimits,
     type="l", bty="n", pre.plot=grid(col="gray"),
     ylab="q1", xlab="nsamp", main=paste("start=",outtake["start"]," : end=",outtake["end"]," | comp.value=",comp.value,sep=""))
for(i in 1:nchains)
{
  lines(outtake["start"]:outtake["end"],
        MCMCout.cs.descs[[i]][[listelement]][,"q2"][outtake["start"]:outtake["end"]],
        col=colos2[i])
}  
abline(h=comp.value, col="black", lty=2)
mean.per.nsamp <- apply(sapply(MCMCout.cs.descs, function(x) x[[1]][,"q2"]),1,mean)
lines(outtake["start"]:outtake["end"],
      mean.per.nsamp[outtake["start"]:outtake["end"]], col="red", lwd=3)


# plot MCMCs - cov per chain and per variable and overall
# cov
outtake <- outtake.cov
outtake
comp.value <- rho
comp.value
listelement <- 2
daten <- unlist( lapply(MCMCout.cs.descs, function(x) x[[2]][outtake["start"]:outtake["end"]]) )
str(daten)
range(daten, na.rm=TRUE)
ylimits <- adj.limits(daten, comp.v=comp.value)
ylimits
plot(NULL, xlim=c(outtake["start"],outtake["end"]),
     ylim=ylimits,
     type="l", bty="n", pre.plot=grid(col="gray"),
     ylab="cov", xlab="nsamp", main=paste("start=",outtake["start"]," : end=",outtake["end"]," | comp.value=",comp.value,sep=""))
for(i in 1:nchains)
{
  lines(outtake["start"]:outtake["end"],
        MCMCout.cs.descs[[i]][[2]][outtake["start"]:outtake["end"]],
        col=colos3[i])
}  
abline(h=comp.value, col="black", lty=2)
mean.per.nsamp <- apply(sapply(MCMCout.cs.descs, function(x) x[[2]]),1,mean)
lines(outtake["start"]:outtake["end"],
      mean.per.nsamp[outtake["start"]:outtake["end"]], col="red", lwd=3)

# main title
# outer margins

mtext("MH simulation (bivariate normal distribution)", side=3, line=1.2, cex=2, outer=TRUE)
subtitletext <- paste("nsamp=",nsamp," | chains=",nchains,sep="")
subtitletext
mtext(subtitletext, side=3, line=-0.5, cex=1.2, outer=TRUE)


# take single run for plotPost
str(OUTmcmc.MH.list)
sapply(OUTmcmc.MH.list, function(x) sum(is.na(x)+0))
BEST:::plotPost(OUTmcmc.MH.list[[1]][[1]][,"q1"], xlab="quantile", ROPE=c(-0.5,0.5), compVal=0, credMass=0.87)
lines(density(OUTmcmc.MH.list[[1]][[1]][,"q1"]), col="magenta", lwd=3, lty=2)
