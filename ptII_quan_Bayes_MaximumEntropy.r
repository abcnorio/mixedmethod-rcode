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
# ptII_quan_Bayes_MaximumEntropy.r

# location:
# chap. 6 [6.14]
# Maximum Entropy

# load necessary helper functions
source("ptII_quan_Bayes_MaximumEntropy_helpfuncs.r")



# How does Boltzmann and Shannon entropy looks like?
# information content = surprisal

# that's identical
# -log2 p(event) = log(1/ p(event))
ps <- seq(0,1,.01)
par(oma=c(2,1,2,1), "cex.axis"=1, bty="l", mfrow=c(2,2))
plot(ps,-log2(ps),col="violetred3", type="l", pre.plot=grid(), bg="darkred",
     main=expression(paste("-",log[2],"(p)",sep="")), bty="n", xlab="p(E)", ylab=expression(paste("-",log[2],"(p(E))"),sep=""))

plot(ps,log(1/ps),col="violetred3", type="l", pre.plot=grid(), bg="darkred",
     main=expression(paste("log(1/p)",sep="")), bty="n", xlab="p(E)", ylab=expression(paste(log,"(1/p(E))"),sep=""))

ps1 <- seq(0,3,0.01)
plot(ps1,ps1*log(ps1), col="violetred3", type="l", pre.plot=grid(), bg="darkred",
     main=expression(paste("p * log(p)",sep="")), bty="n", xlab="p", ylab=expression(paste("p * log(p)"),sep=""))

ps1 <- seq(0,3,0.01)
plot(ps1,ps1*log2(ps1), col="violetred3", type="l", pre.plot=grid(), bg="darkred",
     main=expression(paste("p * ",log[2],"(p)",sep="")), bty="n", xlab="p", ylab=expression(paste("p * ",log[2],"(p)"),sep=""))
mtext("Information content of an event E with p(E)", outer=TRUE, line=-0.5, cex=1.5, side=3)



# binary event Shannon entropy
# two events, same prob for each event to occur
outcomes <- 2
# on the bit scale
log2(outcomes)

# three events, same prob for each event to occur
outcomes <- 3
# on the bit scale
log2(outcomes)
# i.e. more possible events, the higher the entropy


# emtropy of a not so fair coin p + q = 1 and 0 <= p <= 1
# i.e,  p = 1-q ie. p != q
#-(p*log2(p)+(1-p)*log2(1-p))
H <- function(p) -p*log2(p) -(1-p)*log2(1-p)

# entropy H(X) of a perfect fair coin
H1 <- function(e=NA, base=2)
{
  #p = prob of event e - here binary, ie. e=2
  p <- 1/e
  H <- -sum(replicate(e,p*log(p, base=base))) 
  return(H)
} 


# fair coin
p <- 0.5
H(p=p)

# entropy of a pure fair coin with p1 = p2 = 0.5
H1(e=2,base=2)
H1(e=3,base=2)


# entropy of a pure fair coin with p1 = p2 = 0.5
ps <- seq(0,1,0.01)
par(oma=c(2,1,1,1), "cex.axis"=1, bty="l")
plot(ps, H(ps), col="violetred3", type="l", pre.plot=grid(), bg="darkred",
     main="", bty="n", xlab="p(E)", ylab=expression(paste("H = p(E) * log(p(E))"),sep=""))
mtext("Shannon entropy of a perfect fair coin", outer=TRUE, line=-2, cex=1.5, side=3)
points(p,Hp <- H(p=0.5), cex=1.5, col="darkred",bg="green", pch=21)
text(x=p, y=Hp*0.97, col="black", labels=paste("p = ",p," | H(p) = ",round(Hp,dig),sep=""), adj=1, offset=0.5, pos=1)


# entropy of a pure fair coin with p1 != p2 != 0.5
ps <- seq(0,1,0.01)	
par(oma=c(2,1,1,1), "cex.axis"=1, bty="l")
#H(ps) = -ps*log2(ps)-(1-ps)*log2(1-ps)
plot(ps, H(ps),
     col="violetred3", type="l", pre.plot=grid(), bg="darkred",
     main="", bty="n", xlab="p", ylab="H(p)")
mtext("Entropy of a coin with p != q", outer=TRUE, line=-2, cex=1.5, side=3)

# mark various ps
dig <- 2
p <- 0.6
Hp <- H(p=p)
Hp
abline(h=Hp, v=p, col="steelblue",lty=2)
points(p,Hp, cex=1.5, col="darkred",bg="magenta", pch=21)
text(x=p, y=Hp*0.97, col="black", labels=paste("p = ",p," | H(p) = ",round(Hp,dig),sep=""), adj=2, offset=1, pos=4)

p <- 0.3
Hp <- H(p=p)
Hp
abline(h=H(p=p), v=p, col="seagreen",lty=2)
points(p,Hp, cex=1.5, col="darkred",bg="magenta", pch=21)
text(x=p, y=Hp*0.97, col="black", labels=paste("p = ",p," | H(p) = ",round(Hp,dig),sep=""), adj=1, offset=0.5, pos=4)

p <- 0.5
Hp <- H(p=p)
Hp
abline(h=H(p=p), v=p, col="seagreen",lty=2)
points(p,Hp, cex=1.5, col="darkred",bg="green", pch=21)
text(x=p, y=Hp*0.97, col="black", labels=paste("p = ",p," | H(p) = ",round(Hp,dig),sep=""), adj=1, offset=0.5, pos=1)


# p not 0.5 but 0.7
p <- 0.7
qu <- 1-p
qu
H(p=p)
H(p=p) < H1(e=2,base=2)


# entropy and growing possibilities
n <- 1:10
log2(choose(6,k))
Hn <- log2(n)
par(oma=c(2,1,1,1), "cex.axis"=1, bty="l")
plot(n, Hn, col="violetred3", type="b", pch=21, pre.plot=grid(), bg="darkred",
     main="", bty="n", xlab="number n of possibilities", ylab="H= log2(n)",
     ylim=c(0,4.5), xlim=c(0,10), axes=FALSE)
axis(2, cex=0.8)
axis(1, c(0,n), labels=as.character(c(0,n)), cex=0.8)
text(2,log2(2),labels="coin",pos=4, cex=0.9, col="darkred") 
text(6,log2(6),labels="dice",pos=1, cex=0.9, col="darkred")
mtext("Entropy of growing possibilities", outer=TRUE, line=-2, cex=1.5, side=3)


# coin versus dice	 
log2(2)
log2(6)
log2(6)/log2(2)


# dice problem
# van Enk (2014) https://arxiv.org/pdf/1408.6803.pdf
# original ET Jaynes (1962)



# simulate two dices
set.seed(83345)
# number of areas
k <- 6
# number of simulations
N <- 100
# simulate from a uniform distribution
x1 <- round((k-1)*runif(N))+1
x2 <- round((k-1)*runif(N))+1
x1
x2
nk2 <- nk1 <- NA
for(k in 1:6) nk1[k] <- sum(x1 == k)
nk1
for(k in 1:6) nk2[k] <- sum(x2 == k)
nk2
# check the numbers = how many times an area was chosen
nk1/N
nk2/N


# simulate dice
d1 <- dice(k=6, N=100) 
d2 <- dice(k=6, N=100, seed=NULL) 
d1
d2
sum(d1$probs)
sum(d2$probs)
# shannon entropy
-sum(d1$probs*log(d1$probs))
-sum(d2$probs*log(d2$probs))


# simulate entropies and probs of a dice
seed <- 0987
set.seed(seed)
reps <- t(replicate(1e2,  unlist(dice(k=6, N=100, seed=NULL)[c("probs","H")])))
head(reps)


# summaries for faces
apply(reps,2,function(x) c(summary(x),Var=var(x),SD=sd(x)))
apply(reps,2,function(x) c(summary(x),Var=var(x),SD=sd(x),Sum=sum(x)))
# number of N trials for each dice
sum(apply(reps,2, sum)[1:6])


# plot
par(oma=c(2,1,2,1), "cex.axis"=1, bty="l", mfrow=c(2,2))
hist(reps[,"H"], prob=TRUE, border="white", breaks="Scott", col="violetred3", xlab="Entropy H(p)", bty="n", main="")
sum(is.na(reps))
which(is.na(reps))
lines(density(reps[,"H"], na.rm=TRUE), col="blue", lwd=3)
# plot Hs
plot(1:length(reps[,"H"]),reps[,"H"], col="violetred3", type="l", pre.plot=grid(), bg="darkred",
     main="", bty="n", xlab="replications", ylab="H(p)")
# plot sorted Hs
plot(1:length(reps[,"H"]), sort(reps[,"H"]), type="l", col="darkred", bty="n", xlab="sorted replications", ylab=" H(p)", pre.plot=grid())
# plot only those that are a number (!NAN) across all faces
reps1 <- reps[!is.nan(reps[,"H"])]
max(reps1)
reps[which(reps1 == max(reps1))]
reps.l <- length(reps1)
reps.l
plot(1:reps.l, sort(reps1), type="l", col="darkred", bty="n", xlab="sorted replications", ylab=" H(p)", pre.plot=grid())
mtext("Simulate a dice", outer=TRUE, line=-1, cex=1.5, side=3)


# plot all probs versus H(p)
par(oma=c(2,1,2,1), "cex.axis"=1, bty="l", mfrow=c(2,3))
for(i in 1:6)
{
 plot(reps[,i],reps[,"H"], main="", ylab="H(p)", xlab=paste("prob(face = ",i,")",sep=""), col="darkred")
}
mtext("Simulate a dice", outer=TRUE, line=-1, cex=1.5, side=3)


