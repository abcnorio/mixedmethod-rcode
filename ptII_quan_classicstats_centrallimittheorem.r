#file: ptII_quan_classicstats_centrallimittheorem.r
#location:
#Exkurs ? zentraler Grenzwertsatz

#load helper functions
source("ptII_quan_classicstats_centrallimittheorem_helpfuncs.r")


# define some samples as 'function to call'
fun1 <- quote(rnorm(n=30, mean=4, sd=2))
fun2 <- quote(rnorm(n=50, mean=4, sd=2))
fun3 <- quote(rnorm(n=100, mean=4, sd=2))

# simulation central limit theorem
clt.simulate(fun=fun1, trials=100)
clt.simulate(fun=fun2, trials=10000)
clt.simulate(fun=fun3, trials=100)

# larger sample
clt.simulate(fun=fun1, trials=10000)

# it works not 'just' with normal distribution!
# poisson
fun4 <- quote(rpois(n=30, lambda=2))
clt.simulate(fun4, trials=10000)
# chisquare
fun5 <- quote(rchisq(n=30, df=5, ncp=0))
clt.simulate(fun5, trials=10000)


# simulate CLT for different sample sizes, but each amount of trials
Ns <- c(15,30,50,100)
fun6 <- quote(rnorm(n=X, mean=4, sd=2))
c.clt.sim.res <- compare.clt.sim(fun=fun6, Ns=Ns, trials=1000)
str(c.clt.sim.res)


# simulate CLT for different Ns and standardize to mu=1, s=1 or not
Ns <- c(15, 30, 50, 100, 150, 1000, 10000)
trials <- 1000
seed <- 9182
res.clt.sim.Ns <- clt.sim.diffN(fun=fun6, Ns=Ns, norma=FALSE, seed=seed, trials=trials)  
str(res.clt.sim.Ns)
clt.sim.diffN(fun=fun6, Ns=Ns, norma=TRUE, seed=seed, trials=trials)  


# simulate for various N and various distribution families
set.seed(seed)
zusfssg <- function(x) return(data.frame(mw=mean(x),sd=sd(x),max=max(x),min=min(x)))
zusfssg(1:10)


# Normal distribution

# 10 * n=100 random numbers created from standard normal distribution
# mat1 <- matrix(data=rep(rnorm(100),10),ncol=10)
set.seed(seed)
mat <- matrix(data=replicate(10,rnorm(100)),ncol=10)
mat
# work according to vectors in R
res <- do.call(rbind,apply(mat,2,zusfssg)) 
res

# means and descriptive statistics over all samples
apply(res,2,mean)

# repeat the same with bigger N/ large sample size
mat3 <- matrix(data=replicate(10,rnorm(10000)),ncol=10)
tail(mat3)
res3 <- do.call(rbind,apply(mat3,2,zusfssg)) 
res3
res3.res <- apply(res3,2,mean)
res3.res

# differences are hardly visible
sequ <- seq(-4,+4,.1)
plot(x=sequ,dnorm(sequ,mean=0,sd=1),ylim=c(0,.5),type="l",col="blue",bty="n", pre.plot=grid())
lines(sequ,dnorm(sequ,mean=res3.res["mw"],sd=res3.res["sd"]),col="red")


# the same with more samples, although they are smaller now
# simulation of the central limit theorem
mat4 <- matrix(data=replicate(10000,rnorm(100)),ncol=10000)
tail(mat4)
res4 <- do.call(rbind,apply(mat4,2,zusfssg)) 
res4
res4.res <- apply(res4,2,mean)
res4.res


# differences are hardly visible
# sequ <- seq(-4,+4,.1)
plot(x=sequ,dnorm(sequ,mean=0,sd=1),ylim=c(0,.5),type="l",col="blue", bty="n", pre.plot=grid())
lines(sequ,dnorm(sequ,mean=res4.res["mw"],sd=res4.res["sd"]),col="red")


# Chisquare

# the same with more samples, althought they are smaller now
# simulation of the central limit theorem
n <- 100
dfree <- 50
set.seed(seed)
mat5 <- matrix(data=replicate(10000,rchisq(n,df=dfree)),ncol=10000)
tail(mat5)
res5 <- do.call(rbind,apply(mat5,2,zusfssg))
res5
res5.res <- apply(res5,2,mean)
res5.res

#plot
par(oma=c(2,1,1,1), "cex.axis"=1, bty="l", mfrow=c(2,2))
#plot one random draw from chi^2
hist(mat5[,1],prob=TRUE, xlab=expression(paste("random draw ",chi^2," values",sep="")), bty="n", pre.plot=grid(), main="")
lines(density((mat5[,1])),col="red")
#plot means of all samples
hist(res5[,"mw"],prob=TRUE, xlab=expression(paste("mean of ",chi^2," values",sep="")), bty="n", pre.plot=grid(), main="")
lines(density(res5[,"mw"]),col="red")

zentr <- (res5[,"mw"]-mean(res5[,"mw"]))/sd(res5[,"mw"])
zentr
c(mean(zentr), sd(zentr))
zentr.s <- zusfssg(zentr)

# differences are hardly visible
# sequ <- seq(-4,+4,.1)
#plot normal dist versus chi^2 sample means
plot(x=sequ,dnorm(sequ,mean=0,sd=1),ylim=c(0,.5),type="l", ylab="Density", xlab=expression(paste("comparison of ",chi^2," sample means vs. normal dist.",sep="")), col="green", bty="n", pre.plot=grid())
lines(sequ,dnorm(sequ,mean=zentr.s[,"mw"],sd=zentr.s[,"sd"]),col="red")
lines(density(zentr),col="darkblue")

mtext("Simulation Central limit theorem", outer=TRUE, line=-1.7, cex=1.5, side=3)
mtext(eval(substitute(expression(paste("base: ",chi^2," samples (n=",n,", df=",dfree,")",sep="")),list(dfree=dfree,n=n))), outer=TRUE, line=-3.4, cex=1.2, side=3)



# simulate
# one sample
# sampling distribution of the sum
# sampling distribution of the mean
# sampling distribution of variance

###########NOT RUN BELOW THIS POINT

#https://qualityandinnovation.com/2015/03/30/sampling-distributions-and-central-limit-theorem-in-r/
#http://www.rpubs.com/christopher_castle/137490
#https://stackoverflow.com/questions/40307510/central-limit-theorem-in-r
#https://consultglp.com/wp-content/uploads/2016/10/using-r-to-simulate-the-central-limit-theorem.pdf
#https://www.stat.ubc.ca/~john/305Last0607/lab2sol.pdf
#http://genomicsclass.github.io/book/pages/clt_in_practice.html

#todo
#http://adv-r.had.co.nz/Expressions.html
#https://stackoverflow.com/questions/8452002/plotting-a-grid-behind-data-not-in-front-in-r
