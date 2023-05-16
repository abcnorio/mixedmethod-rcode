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



# file:
# ptII_quan_classicstats_N-P_example-soccer-sim.r

# location:
# chap.4 [4.5.4.1]
# Fallbeispiel Simulation — Fussball Sammelbilder

#load necessary libs
library(Hmisc)

# load helper functions
source("ptII_quan_classicstats_N-P_example-soccer-sim_helpfuncs.r")


seed <- 9876
set.seed(seed)
anzpp <- 5
Nkarten <- 294
trials <- 1e+4 #1e+5
maxIDs <- 1000
res.karten <- matrix(data=NA, nrow=trials, ncol=Nkarten)
res.IDs <- matrix(data=0, nrow=trials, ncol=maxIDs)
res.zaehl <- rep(0, trials)
for(i in 1:trials)
{
 # print(i)
 res <- BL.sim(Nkarten=Nkarten, anzpp=anzpp, doIDs=TRUE, total=FALSE)
 res.karten[i,] <- res$karten
 res.zaehl[i] <- res$zaehl
 res.IDs[i,] <- res$IDs
}

#
# number of trials (=number of packages à five cards) till album is full
str(res.zaehl)

# multiple cards - how many of each card are there at the end
dim(res.karten) #number of cards (double etc) for each card till zero

# number of cards "to go" (how many still required) till album is full
dim(res.IDs) #rows=trials, cols=IDs


# IDs
# distribution of necessary cards till album is full and nothing is missing
# extract number of necessary drawings for each trial/ case
laengeIDs <- apply(res.IDs,1, function(i) length(which(i != 0)))
par(oma=c(2,1,1,1), "cex.axis"=1, bty="l", mfrow=c(1,2))
hist(laengeIDs, panel.first=grid(), prob=TRUE, xlab="number of cards", ylab="Density", main="", col="skyblue", border="white")
lines(density(laengeIDs), col="red", lty=1, lwd=2)
# log() transfo of data
hist(log(laengeIDs), panel.first=grid(), prob=TRUE, xlab="log(number of cards)", ylab="Density", main="", col="skyblue", border="white")
lines(density(log(laengeIDs)), col="red", lty=1, lwd=2)
mtext("Simulation soccer collector cards", outer=TRUE, line=-2, cex=1.5, side=3)

fivenum.wn(res.zaehl)

# number of packages (5 cards within each package)
summary(res.zaehl)

# compare to .70-€ buying (each package)
summary(res.zaehl)*.7

# quantiles
probs <- c(0.01,0.025,0.05,c(seq(0.1,1,by=0.1)),0.95,0.975,0.99)
quantile(res.zaehl, probs=probs)
sd(res.zaehl)

# compare CI for single dataset, take the first one
seed <- 9876
set.seed(seed)
res.single <- BL.sim(Nkarten=Nkarten, anzpp=anzpp, doIDs=TRUE)
res.single

# TODO

# duration time if per week someone spends ~85,-€ in the discounter
# and we have to spend at least 10,-€ for each package
round(quantile(res.zaehl, probs=probs)*10/85)
# in years
round(quantile(res.zaehl, probs=probs)*10/85*7/365,2)

# double and triple, etc. cards (growing curve over time)
# uniform distribution of cards?
# number of double/ triple/ etc. of each card for each trial (typical)
str(res.karten) # rows=trials, cols=number of duplicates for each cards
res.karten.ratio <- apply(res.karten,2,sum)/trials
summary(res.karten.ratio)
sd(res.karten.ratio)
plot(res.karten.ratio, panel.first=grid(), main="Multiple cards", xlab="card number",ylab="average number of multiples",col="red", type="h", bty="l")
# heatmap of raw data
heatmap(res.karten, Rowv=NA, Colv=NA, labRow=NA, labCol=NA, xlab="card number", ylab="trials", main="heatmap")

# double/ triple/ etc. complete
res.karten.tab <- table(res.karten)
res.karten.tab
res.karten.tab/trials
# mean of multiple cards
sum(res.karten)/trials/Nkarten

# new cards
res.IDs.sum <- apply(res.IDs,2,function(x) sum(x[!is.na(x)]))
res.IDs.mean <- apply(res.IDs,2,function(x) mean(x[!is.na(x)]))
res.IDs.max <- apply(res.IDs,2,function(x) max(x[!is.na(x)]))
res.IDs.min <- apply(res.IDs,2,function(x) min(x[!is.na(x)]))
res.IDs.sd <- apply(res.IDs,2,function(x) sd(x[!is.na(x)]))

res.IDs.N <- apply(res.IDs,2,function(i) length(which(i > 0)))
res.IDs.SE <- res.IDs.sd/sqrt(res.IDs.N)
alpha <- 0.05
CI.fak <- qnorm(1-alpha/2, lower=TRUE)
res.IDs.CI.halfwidth <- (CI.fak*res.IDs.SE)
res.IDs.CI.halfwidth <- res.IDs.CI.halfwidth[res.IDs.N>0]
# work-around
res.IDs.CI.halfwidth <- res.IDs.CI.halfwidth[is.na(res.IDs.CI.halfwidth)] <- 0

res.IDs.ratio <- res.IDs.sum/trials
# only greater than zero
res.IDs.ratio.GZ <- res.IDs.ratio[res.IDs.ratio > 0]

#plot
par(oma=c(2,1,1,1), "cex.axis"=1, bty="l")
plot(res.IDs.mean[is.finite(res.IDs.mean)], panel.first=grid(), main="", xlab="drawing",ylab="missing cards",col="red", type="l", bty="l")
lines(res.IDs.max[res.IDs.max != Inf], col="blue")
lines(res.IDs.min[res.IDs.min != Inf], col="darkgreen")
abline(h=29,col="magenta", lty=2)
abline(h=10,col="brown", lty=3)
legend("topright", legend=c("missing cards (mean)","missing cards (max)","missing cards (min)","n=29 cards-to-go", "n=10 cards-to-go"),
       pch="---", pt.cex=2.5,col=c("red","blue","darkgreen","magenta","brown"), bty="n", bg="yellow")
mtext("Process cards", outer=TRUE, line=-2, cex=1.5, side=3)

# Min/Max
par(oma=c(2,1,1,1), "cex.axis"=1, bty="l")
errbar(x=1:sum(res.IDs.N>0), y=res.IDs.mean[res.IDs.N>0],
       yminus=res.IDs.max[res.IDs.N>0],
       yplus=res.IDs.min[res.IDs.N>0], panel.first=grid(),
       col="green", pch=2, bg="red",errbar.col="blue", cap=0, xlab="drawing", ylab="number of present cards", bty="l")
mtext("Process cards (max/ mean/ min))", outer=TRUE, line=-2, cex=1.5, side=3)

# CI
par(oma=c(2,1,1,1), "cex.axis"=1, bty="l")
errbar(x=1:sum(res.IDs.N>0), y=res.IDs.mean[res.IDs.N>0],
       yminus=res.IDs.mean[res.IDs.N>0] - res.IDs.CI.halfwidth,
       yplus=res.IDs.mean[res.IDs.N>0] + res.IDs.CI.halfwidth, panel.first=grid(),
       col="green", pch="|", bg="red",errbar.col="blue", cap=0, xlab="drawing", ylab="number of present cards", bty="l")
mtext("Process cards (95% CI_up/ mean/ 95% CI_low)", outer=TRUE, line=-2, cex=1.5, side=3)
 

# number of drawings necessary until album is full
anz.draw.n <- 1000-apply(apply(res.IDs,1,is.na),2,sum)
anz.draw.n

# single
# IDs
seed <- 9876
set.seed(seed)
res.single <- BL.sim(Nkarten=Nkarten, anzpp=anzpp, doIDs=TRUE, total=TRUE)
str(res.single)
waytoZ <- res.single$IDs
waytoZ <- waytoZ[!is.na(waytoZ)]
plot(waytoZ, panel.first=grid(), type="l", col="red", main="", xlab="Simulation (packages)", ylab="number of missing cards", bty="l")
mtext("Card collection process", outer=TRUE, line=-2, cex=1.5, side=3)

#
# 29 left
abline(h=29,col="blue", lty=2)
# 10 left
abline(h=10,col="darkblue", lty=3)
# duration time (in packages) for 29 and 10 missing cards left
# which means
Nkarten - 29
Nkarten - 10
# are already present
head(waytoZ)
tail(waytoZ)
waytoZ.l <- length(waytoZ)
waytoZ.l
which(waytoZ == 29)
waytoZ.l - which(waytoZ == 29)
which(waytoZ == 10)
waytoZ.l - which(waytoZ == 10)

# distribution of each card
karten.total <- res.single$karten.total
karten.total <- karten.total[!is.na(karten.total)]
plot(karten.total, panel.first=grid(), type="p", bty="n")
kt.tab <- table(karten.total)
table(kt.tab)
kt.tab.fn <- fivenum(kt.tab)
names(kt.tab.fn) <- c("Min","LQ","Median","UQ","Max")
kt.tab.fn
kt.tab
hist(as.vector(kt.tab), panel.first=grid(), prob=TRUE, col="skyblue", border="white",
     main="", xlab="number of multiple cards", ylab="Density")
lines(density(as.vector(kt.tab)), col="red", lwd=2)
mtext("Distribution of multiples (single simulation)", outer=TRUE, line=-2, cex=1.5, side=3)


