# file:
# ptII_quan_Bayes_MaximumEntropy_Jaynes-fair-dice.r

# location:
# chap. 6 [6.14.3]
# Der Klassiker — ist ein Würfel fair?

# load helper functions
source("ptII_quan_Bayes_MaximumEntropy_Jaynes-fair-dice_helpfuncs.r")


# Maximum Entropy


# side infos

# http://bjlkeng.github.io/posts/maximum-entropy-distributions/
# http://bjlkeng.github.io/posts/lagrange-multipliers/
# Eh voila! The usual (non-intuitive) definition of entropy we all know and love.
# Note: When any of the probabilities are pi=0, you replace 0log(0) with 0, which
# is consistent with the limit as p approaches to 0 from the right.
# Entropy, then, is the average amount of information or surprise for an event in
# a probability distribution. Going back to our example above, when transmitting only "A"s,
# the information transmitted is 0 (because P("A")=1
# and 0 for other letters), so the entropy is naturally 0. When transmitting English text,
# the entropy will be the average entropy using letter frequencies [1].

p <- 0.5
log2(1/p)
p*log2(1/p) + (1-p)*log2(1/p)

ps <- seq(0,1,0.01)
ps*(1-ps)

par(oma=c(2,1,2,1), "cex.axis"=1, bty="l", mfrow=c(2,2))
plot(log2(ps), ps*log2(1/ps) + (1-ps)*log2(1/ps),
     col="violetred3", type="l", pre.plot=grid(), bg="darkred",
     main="Shannon entropy", bty="n", xlab="p", ylab=expression(paste("p log(p)"),sep=""))

plot(log2(ps*(1-ps)), ps*log2(1/ps) + (1-ps)*log2(1/ps),
     col="violetred3", type="l", pre.plot=grid(), bg="darkred",
     main="Shannon entropy", bty="n", xlab="p", ylab=expression(paste("p log(p)"),sep=""))

plot(ps*(1-ps), exp(ps*log2(1/ps) + (1-ps)*log2(1/ps)),
     col="violetred3", type="l", pre.plot=grid(), bg="darkred",
     main="Shannon entropy", bty="n", xlab="p", ylab=expression(paste("p log(p)"),sep=""))

plot(ps, ps*(1-ps),
     col="violetred3", type="l", pre.plot=grid(), bg="darkred",
     main="Shannon entropy", bty="n", xlab="p", ylab="p*(1-p)")


# So one bit of information is transmitted with every observation of a fair coin toss.
# If we vary the value of p, we get a symmetric curve shown in Figure 1.
# The more biased towards H or T, the less entropy (information/surprise) we get (on average).
H <- function(p) -( p*log2(p) + (1-p)*log2(1-p) )
H(p=ps)
plot(ps, H(ps),#-ps*log2(ps)-(1-ps)*log2(1-ps),
     col="violetred3", type="l", pre.plot=grid(), bg="darkred",
     main="Entropy of a fair coin", bty="n", xlab="p", ylab="H(p)")

plot(ps, -ps*log2(ps)-(1-ps)*log2(1-ps),
     col="violetred3", type="l", pre.plot=grid(), bg="darkred",
     main="Shannon entropy", bty="n", xlab="p", ylab="H(p)")		 	 


# So given no information about a discrete distribution,
# the maximal entropy distribution is just a uniform distribution.
# This matches with Laplace's principle of indifference which states
# that given mutually exclusive and exhaustive indistinguishable possibilities,
# each possibility should be assigned equal probability of 1n.
# from numpy import exp
# from scipy.optimize import newton
#
# a, b, B = 1, 6, 4.5
#
## Equation 15
#def z(lamb):
#  return 1. / sum(exp(-k*lamb) for k in range(a, b + 1))
#
## Equation 16
#def f(lamb, B=B):
#  y = sum(k * exp(-k*lamb) for k in range(a, b + 1))
#return y * z(lamb) - B
#
## Equation 17
#def p(k, lamb):
#  return z(lamb) * exp(-k * lamb)
#
#lamb = newton(f, x0=0.5)
#print("Lambda = %.4f" % lamb)
#for k in range(a, b + 1):
#  print("p_%d = %.4f" % (k, p(k, lamb)))
#
# Output:
#   Lambda = -0.3710
#   p_1 = 0.0544
#   p_2 = 0.0788
#   p_3 = 0.1142
#   p_4 = 0.1654
#   p_5 = 0.2398
#   p_6 = 0.3475
################################

# critics on Jaynes and alternate...
# https://arxiv.org/pdf/1408.6803.pdf



# Jaynes' dice problem

# expectation fair dice (die)
k <- 1:6
prob <- 1/6
sum(k*prob)

# eq 15
#z <- function(x) 1/sum(exp(-k * x))

# eq 16
#foo <- function(x, B, k) return( abs( sum(k*exp(-k * x)) * ( 1/sum(exp(-k * x)) ) - B ) )
#foo2 <- function(x, B, k) return( abs( sum(k*exp(-k * x)) * ( z(x) ) - B ) )
#foo3 <- function(x, B, k) return( sum(k*exp(-k * x)) * ( z(x) ) - B )
# optimizations
optim(0.5, foo, method="BFGS", B=4.5, k=1:6)
optim(0.5, foo2, method="BFGS", B=4.5, k=1:6)
uniroot(foo3, c(-1, 1), tol = 0.0001, B = 4.5, k=1:6)

# eq 17
#probfun <- function(x)  1/z(x) * exp(-k * x)

# manual optimization
sek <- seq(-3,3,.001)
prob.zeros <- NA
for(i in 1:length(sek)) prob.zeros[i] <- foo3(x=sek[i], B=4.5, k=1:6)

# find value nearest zero
naid.sm.zero <- which(prob.zeros <= 0)
naid.gr.zero <- which(prob.zeros >= 0)
sm.zero <- prob.zeros[naid.sm.zero][1]
prob.zeros.temp <- prob.zeros[naid.gr.zero]
gr.zero <- prob.zeros.temp[length(prob.zeros.temp)]
MIN <- min(abs(c(sm.zero, gr.zero)))
MIN.id <- sek[which(abs(prob.zeros) == MIN)]

# shorter version
MIN <- min(abs(prob.zeros))
MIN.id <- sek[which(abs(prob.zeros) == MIN)]

# results
sm.zero
gr.zero
MIN
MIN.id

# plot
plot(prob.zeros, sek, type="l", pre.plot=grid(), col="darkred", bty="n",
     ylab="possible values", xlab="finding root ie. zero", main="Optimization problem")
abline(v=MIN, h=MIN.id, col="violetred3", lty=2)
text(MIN, MIN.id+1, pos=3,
     labels=eval(substitute(expression(paste("FUN at ",lambda,"=",MIN.id," with value=",MIN)), list(MIN.id=MIN.id, MIN=round(MIN,5)))),
     cex=1, col="blue")


# values from paper
lambda <- -0.371
ps <- probfun(x=lambda)
ps/sum(ps)
probs.ME <- t(data.frame(probfun(x=lambda)/sum(probfun(x=lambda))))
colnames(probs.ME) <- 1:6
rownames(probs.ME) <- c("probs")
probs.ME
#   Lambda = -0.3710
#   p_1 = 0.0544
#   p_2 = 0.0788
#   p_3 = 0.1142
#   p_4 = 0.1654
#   p_5 = 0.2398
#   p_6 = 0.3475


# number of faces
k <- 1:6
# expectation of the die roll
B <- 4.5
# B <- 3.5
lambda.opt <- optim(0.5, foo, method="BFGS", B=B, k=1:6)
lambda.opt
foo(x=lambda.opt$par, B=B, k=1:6)
ps <- probfun(x=lambda.opt$par)
ME.B45 <- ps/sum(ps)
ME.B45

# values from the paper/ blogpost
lambda.blog <- -0.371
ps <- probfun(x=lambda.blog)
ps/sum(ps)
all.equal(lambda.blog, lambda.opt$par)

# different opt algorithm
B
lambda.opt.alt <- optimize(foo, c(-0.5,0.5), B=B, k=1:6)
lambda.opt.alt
ps <- probfun(x=lambda.opt.alt$minimum)
ps/sum(ps)
all.equal(lambda.blog, lambda.opt.alt$minimum)

# uniform distribution
B <- 3.5
B
lambda.opt <- optim(0.5, foo, method="BFGS", B=B, k=1:6)
lambda.opt
foo(x=lambda.opt$par, B=4.5, k=1:6)
ps <- probfun(x=lambda.opt$par)
ME.B35 <- ps/sum(ps)
ME.B35

# B = 4
B <- 4
B
lambda.opt <- optim(0.5, foo, method="BFGS", B=B, k=1:6)
lambda.opt
foo(x=lambda.opt$par, B=4.5, k=1:6)
ps <- probfun(x=lambda.opt$par)
ME.B40 <- ps/sum(ps)
ME.B40

# B = 6
B <- 6
B
lambda.opt <- optim(0.5, foo, method="BFGS", B=B, k=1:6)
lambda.opt
foo(x=lambda.opt$par, B=4.5, k=1:6)
ps <- probfun(x=lambda.opt$par)
ME.B60 <- ps/sum(ps)
ME.B60

# B = 2
B <- 2
B
lambda.opt <- optim(0.5, foo, method="BFGS", B=B, k=1:6)
lambda.opt
foo(x=lambda.opt$par, B=4.5, k=1:6)
ps <- probfun(x=lambda.opt$par)
ME.B20 <- ps/sum(ps)
ME.B20

par(oma=c(2,1,4,1), "cex.axis"=1, bty="l", mfrow=c(3,2))
plot(k, ME.B35, col="violetred3", type="b", pch=21, pre.plot=grid(), bg="darkred",
     main="Entropy of a dice expectation B=3.5 (= fair)", bty="n", xlab="n-th face of the dice", ylab="MAXENT(p)")
plot(k, ME.B45, col="violetred3", type="b", pch=21, pre.plot=grid(), bg="darkred",
     main="Entropy of a dice expectation B=4.5 (= not fair)", bty="n", xlab="n-th face of the dice", ylab="MAXENT(p)")
plot(k, ME.B40, col="violetred3", type="b", pch=21, pre.plot=grid(), bg="darkred",
     main="Entropy of a dice expectation B=4 (= not fair)", bty="n", xlab="n-th face of the dice", ylab="MAXENT(p)")	 
plot(k, ME.B60, col="violetred3", type="b", pch=21, pre.plot=grid(), bg="darkred",
     main="Entropy of a dice expectation B=6 (= not fair)", bty="n", xlab="n-th face of the dice", ylab="MAXENT(p)")
plot(k, ME.B20, col="violetred3", type="b", pch=21, pre.plot=grid(), bg="darkred",
     main="Entropy of a dice expectation B=2 (= not fair)", bty="n", xlab="n-th face of the dice", ylab="MAXENT(p)")
mtext("Jaynes' dice problem", outer=TRUE, line=1, cex=1.5, side=3)


# perform Jaynes' dice problem
Bs <- c(0.5, 2, 3.5, 4, 4.5, 6)
J.dice.res <- lapply(Bs, function(i) Jaynes.dice(B=i))
J.dice.res
par(oma=c(2,1,4,1), "cex.axis"=1, bty="l", mfrow=c(3,2))
for(i in Bs)
{
  plot(k, J.dice.res[[i]]$probs, col="violetred3", type="b", pch=21, pre.plot=grid(), bg="darkred",
       main=paste("B=",i,sep=""), bty="n", xlab="n-th face of the dice", ylab="MAXENT(p)")
}
mtext("Jaynes' dice problem", outer=TRUE, line=1, cex=1.5, side=3)

# example from Jaynes
Jaynes.dice()$probs
Jaynes.dice(B=4.5, k=1:6)$probs
# faire dice
Jaynes.dice(B=3.5, k=1:6)$probs



# ratio
# dice (=6) versus coin (=2)	-> possibilities
log2(6) / log2(2)
# dice (=8) versus dice (=6)
log2(8) / log2(6)
log2(8) / log2(2)
log2(8)
log2(6)
log2(2)
# how many throws with a coin are equal to the possibilities of a dice (=8)
n <- 3
2^n



# not run

# simulate entropy after McElreath, p.277

sim.p()
sum(sim.p()$x)

replicate(10, sim.p(seed=NULL)$p)

reps <- 1e4
sim.out <- replicate(reps, sim.p(seed=NULL)$H)
head(sim.out)
par(oma=c(2,1,1,1), "cex.axis"=1, bty="l", mfrow=c(1,2))
hist(sim.out, prob=TRUE, border="white", breaks="Scott", col="violetred3", xlab="Entropy", bty="n", main="", pre.plot=grid())
lines(density(sim.out), col="green", lwd=3)
plot(1:reps,sim.out, col="violetred3", type="l", pre.plot=grid(), bg="darkred",
     main="", bty="n", xlab="p", ylab="H(p)")	
mtext("Simulating entropy", outer=TRUE, line=-2, cex=1.5, side=3)

max(sim.out)
sim.out[which(sim.out == max(sim.out))]

