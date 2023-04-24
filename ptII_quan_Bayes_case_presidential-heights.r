# file:
# ptII_quan_Bayes_case_presidential-heights.r

# location:
# chap. 6 [6.15.1]
# Präsidiale Höhenflüge

# load necessary libs
library(BayesianFirstAid)
library(HDInterval)
library(BEST)
# library(Brobdingnag)
library(bayesboot)

# load helper functions
source("ptall_generalfuncs.r")
source("DiM_Bretthorst_UMS.r")
source("DiM_Bretthorst_PG.r")
source("ptII_quan_Bayes_case_presidential-heights_helpfuncs.r")

# read data

# do only once! preparation...
# pres.h <- data.frame(pres.h,
#                     WP=paste("W",factor(pres.h[,"W.party"]),sep=":"),
#                     OP=paste("O",factor(pres.h[,"O.party"]),sep=":")
#	           				  )
# write enhanced file, no need to reproduce diff.TF and diff below
# write.table(pres.h,"presidential-heights.tab",sep="\t", row.names=FALSE, col.names=TRUE)


# read enhanced file
pres.h <- read.table("presidential-heights.tab", sep="\t", header=TRUE)
dim(pres.h)
str(pres.h)
head(pres.h)

# add diffs
pres.h <- data.frame(pres.h, h.diff=with(pres.h, W.cm-O.cm))

# remove NAs
data.frame("na"=apply(pres.h,2,function(i) sum(is.na(i))))
naids <- which(is.na(pres.h),arr.ind=TRUE)
naids
naids.unique <- unique(naids[,"row"])
naids.unique
pres.h.nona <- pres.h[-naids.unique,]
pres.h.nona$h.diff.TF <- with(pres.h.nona, h.diff > 0)
head(pres.h.nona)


# identify appearances of persons more than once
# unless we want to make the diff for each election
# >>> then these are cases of winner vs. main.opponent <<<
W.double <- sort(table(pres.h.nona[,"winner"]) >1, dec=TRUE)
O.double <- sort(table(pres.h.nona[,"main.opponent"]) >1, dec=TRUE)
W.double[W.double]
O.double[O.double]

# use persons, not cases and reduce NAs 'en passent'
W.cm.RED <- data.frame(do.call("rbind",strsplit(unique(with(pres.h.nona, paste(winner,W.cm,sep=":"))),":")))
O.cm.RED <- data.frame(do.call("rbind",strsplit(unique(with(pres.h.nona, paste(main.opponent,O.cm,sep=":"))),":")))
colnames(W.cm.RED) <- c("winner","W.cm")
colnames(O.cm.RED) <- c("opponent","O.cm")
W.cm.RED[,"W.cm"] <- as.numeric(as.character(W.cm.RED[,"W.cm"]))
O.cm.RED[,"O.cm"] <- as.numeric(as.character(O.cm.RED[,"O.cm"]))
W.cm.RED <- W.cm.RED[,"W.cm"][!is.na(W.cm.RED[,"W.cm"])]
O.cm.RED <- O.cm.RED[,"O.cm"][!is.na(O.cm.RED[,"O.cm"])]
W.cm.RED
O.cm.RED

# combine - cases and persons
dats <- list(W=pres.h.nona$W.cm,
             O=pres.h.nona$O.cm,
             W.red=W.cm.RED,
             O.red=O.cm.RED)
dats

# descriptive statistics
summary(pres.h.nona$h.diff)
fivenum.wn(pres.h.nona$h.diff)
dats.desc <- do.call("rbind", lapply(dats, function(x) c(c(summary(x),VAR=var(x),SD=sd(x),fivenum.wn(x)))) )
dats.desc

# plot differences
h.dens <- density(pres.h.nona$h.diff, na.rm=TRUE)
par(oma=c(2,1,1,1), "cex.axis"=1, bty="l")
hist(pres.h.nona$h.diff, prob=TRUE, xlab="Difference (cm)", ylab="Density",
     breaks=20, col="skyblue", border="white",#"Scott"
     main="", ylim=c(0,max(h.dens$y)), pre.plot=grid())
lines(h.dens, col="violetred3", lwd=2, lty=1)
mtext("Heights differences", outer=TRUE, line=-1, cex=1.5, side=3)
mtext("POTUS vs. main opponents", outer=TRUE, line=-2.5, cex=1, side=3)

# boxplots
boxplot(dats, col=c("yellowgreen","yellow"), notch=TRUE, main="Heights (POTUS vs. main opponents)",
        las=1, frame.plot=FALSE, horizontal=TRUE, outly=2, lwd=2, medcol="darkblue")
rug(unlist(dats))


# tables
# library(BayesianFirstAid)

# who is taller?
h.diff.TF.tab <- table(pres.h.nona$h.diff.TF)
h.diff.TF.tab
h.W.successes <- h.diff.TF.tab["TRUE"]
h.N <- sum(h.diff.TF.tab)

# classic
binom.test(x=h.W.successes, n=h.N, p=0.5, alternative="greater")

# Bayes
# library(HDInterval)
h.bbt <- bayes.binom.test(x=h.W.successes, n=h.N,, comp.theta=0.5, cred.mass=0.87)
summary(h.bbt)
plot(h.bbt)
lapply(h.bbt$mcmc_samples, hdi, credMass=0.87)
diagnostics(h.bbt)
model.code(h.bbt)

# want to change prior?
BayesianFirstAid:::bayes.binom.test
BayesianFirstAid:::jags_binom_test
BayesianFirstAid:::binom_model_string
BayesianFirstAid:::run_jags
#[1] "model {\n  x ~ dbinom(theta, n)\n  theta ~ dbeta(1, 1)\n  x_pred ~ dbinom(theta, n)\n}"
bbinom.model.string <- "model {\n  x ~ dbinom(theta, n)\n  theta ~ dbeta(0.001, 0.001)\n  x_pred ~ dbinom(theta, n)\n}"
bbinom.model.string <- "model {\n  x ~ dbinom(theta, n)\n  theta ~ dbeta(1, 1)\n  x_pred ~ dbinom(theta, n)\n}"
bbinom.model.string <- "model {\n  x ~ dbinom(theta, n)\n  theta ~ dbeta(1, 6)\n  x_pred ~ dbinom(theta, n)\n}"

prior.a <- 3
prior.b <- 20
bbinom.model.string <- paste("model {\n  x ~ dbinom(theta, n)\n  theta ~ dbeta(",prior.a,", ",prior.b,")\n  x_pred ~ dbinom(theta, n)\n}",sep="")
bbinom.model.string

x <- h.W.successes
n <- h.N
x_name <-"W"
n_name <- "elections"

# use alternative version of bayes.binom() from 'BayesianFirstAid'
h.bbt.res <- bayes.binom.alt(x=x, n=n, cred.mass=0.87, comp.theta=0.5, x_name=x_name, n_name=n_name, model_string=bbinom.model.string)
#h.bbt.res.alt1 <- bayes.binom.alt(x=x, n=n, cred.mass=0.87, comp.theta=0.5, x_name=x_name, n_name=n_name)
#h.bbt.res.alt2 <- bayes.binom.alt(x=x, n=n, cred.mass=0.87, comp.theta=0.5, x_name=x_name, n_name=n_name, prior.a=2, prior.b=45)

# HDI and diagnostics of MCMC chain(s)
lapply(h.bbt$mcmc_samples, hdi, credMass=0.69)
diagnostics(h.bbt.res)
# output model etc.
summary(h.bbt.res)
plot(h.bbt.res)
# initial values
str(h.bbt.res)
h.bbt.res$x
h.bbt.res$n
# model
cat(h.bbt.res$model)


# comparison
summary(bayes.binom.test(x=h.W.successes, n=h.N,, comp.theta=0.5, cred.mass=0.87))

#plot(bfa_object)
#bfa_object$x
#bfa_object$n
#cat(bfa_object$model)

# change priors
prior.a <- 0.001
prior.b <- 0.001
comp.theta <- 0.5
cred.mass <- 0.87
h.bbt.res.1 <- bayes.binom.alt(x=x, n=n, cred.mass=cred.mass, comp.theta=comp.theta, prior.a=prior.a, prior.b=prior.b)
lapply(h.bbt.res.1$mcmc_samples, hdi, credMass=0.69)
diagnostics(h.bbt.res)


# not run
# different hypothesis - taller versus party
pres.h.tab <- with(pres.h.nona, table(W.party, h.diff.TF))
pres.h.tab
# select only Democrats and Republicans
select <- c("D","R")
ids <- dimnames(pres.h.tab)$W.party %in% select
pres.h.tab.RED <- pres.h.tab[ids,]
pres.h.tab.RED
# chisq.test
chisq.test(pres.h.tab.RED)
chisq.test(pres.h.tab.RED, sim=TRUE, B=10000)
# Bayesian proportion test
pres.h.tab.res <- bayes.prop.test(pres.h.tab.RED)
summary(pres.h.tab.res)
plot(pres.h.tab.res)
diagnostics(pres.h.tab.res)
model.code(pres.h.tab.res)
# end of not run


# test the difference in means

# data base
# cases / each election
W <- dats[["W"]]
O <- dats[["O"]]

# classic
with(dats, t.test(W, O, alternative="greater", paired=TRUE, var.equal=FALSE))
cohensd(W[!is.na(W)], O[!is.na(O)])

# persons (presidents vs. opponents)
W.red <- dats[["W.red"]]
O.red <- dats[["O.red"]]

# classic
with(dats, t.test(W.red, O.red, alternative="greater", paired=FALSE, var.equal=FALSE))
cohensd(W.red,O.red)


# library(BayesianFirstAid)

# cases / each election
pres.h.bt.res1 <- with(dats, bayes.t.test(W, O, paired=TRUE))
summary(pres.h.bt.res1)
plot(pres.h.bt.res1)
diagnostics(pres.h.bt.res1)
model.code(pres.h.bt.res1)

# persons (presidents vs. opponents)
pres.h.bt.res2 <- with(dats, bayes.t.test(W.red, O.red, paired=FALSE))
# not really different to BEST
summary(pres.h.bt.res2)
plot(pres.h.bt.res2)
diagnostics(pres.h.bt.res2)
model.code(pres.h.bt.res2)


# library(BEST)

# very wide prior (default), see ?BESTmcmc

# cases
W.nona <- pres.h.nona[,"W.cm"]
O.nona <- pres.h.nona[,"O.cm"]

# calculate model
pres.best.res <- BESTmcmc(W.nona, O.nona, rnd.seed=84445)
str(pres.best.res)
names(pres.best.res)

# plot and summaries
plot(pres.best.res)
summary(pres.best.res)
print(pres.best.res)
plotPost(pres.best.res[,"mu1"])
BEST:::pairs.BEST(pres.best.res)
plotAll(pres.best.res)
plotPostPred(pres.best.res)
hdi(pres.best.res)

# diff and ROPE
meanDiff.h <- pres.best.res$mu1 - pres.best.res$mu2
mean(meanDiff.h > 0)
mean(meanDiff.h < 0)
mean(meanDiff.h > 3)
mean(meanDiff.h < 3)

# BF_greater-vs-smaller(winner)
mean(meanDiff.h > 0) / mean(meanDiff.h < 0)
mean(meanDiff.h < 0) / mean(meanDiff.h > 0)
plotAreaInROPE(meanDiff.h, credMass=0.97, compVal=3, maxROPEradius=7)

# calculate posterior (Odds) Ratios
p1 <- mean(meanDiff.h > 0)
p2 <- mean(meanDiff.h < 3)
p1vsp2 <- post.comp(post1=p1 , post2=p2, RETURN=TRUE)

# for POWER
?makeData
?BESTpower


# not run

# difference
# very wide prior (default), see ?BESTmcmc
# cases
pres.best.diff.res <- BESTmcmc(pres.h.nona$h.diff, rnd.seed=84445)
plot(pres.best.diff.res)
summary(pres.best.diff.res)
hdi(pres.best.diff.res)

# diff and ROPE
meanDiff.diff <- pres.best.diff.res$mu
mean(meanDiff.diff > 0)
mean(meanDiff.diff < 0)
plotAreaInROPE(meanDiff.diff, credMass=0.95, compVal=0, maxROPEradius=5)

# calculate posterior (Odds) Ratios
p1 <- mean(meanDiff.diff > 0)
p2 <- mean(meanDiff.diff < 3)
p1vsp2 <- post.comp(post1=p1 , post2=p2, RETURN=TRUE)

# end of not run


# Bretthorst 1993
# implementation Mathematica by UM Studer 1999

# prepare data
W.cm <- pres.h.nona[,"W.cm"]
O.cm <- pres.h.nona[,"O.cm"]
Ni  <- length(W.cm)
Nii <- length(O.cm)
Di  <- mean(W.cm, na.rm=TRUE)
Dii <- mean(O.cm, na.rm=TRUE)
si  <- sd(W.cm, na.rm=TRUE)
sii <- sd(O.cm, na.rm=TRUE)
L   <- 170
H   <- 190
sL  <- 4
sH  <- 10

inval.base <- list(
  Si = NULL,	# UMS specific -> successes group1
  Ni = Ni,		# N group1
  Sii = NULL,	# UMS specific -> successes group2
  Nii = Nii,	# N group2
  smin = 0,		# UMS specific -> bounds on the mean -> only for SucRatesIntBounds()
  Di = Di,		# mean group1
  si = si,		# sd group1
  Dii = Dii,	# mean group2
  sii = sii,	# sd group2
  L = L,			# mean lower bound
  H = H,			# mean upper bound
  sL = sL,		# variance lower bound
  sH = sH,		# variance upper bound
  snames = c("president","main opponent")
)
inval.base
DiM.presheights <- DiffinMeans(inval=inval.base, out=FALSE)
UMSprint(DiM.presheights)

# different bounds -> smaller
inval.smaller <- inval.base
inval.smaller$L <- 175
inval.smaller$H <- 185
inval.smaller$sL <- 5
inval.smaller$sH <- 9  
inval.smaller
DiM.presheights.smaller <- DiffinMeans(inval=inval.smaller, out=FALSE)
UMSprint(DiM.presheights.smaller)

# different bounds -> wider
inval.wider <- inval.base
inval.wider$L <- 160
inval.wider$H <- 200
inval.wider$sL <- 3
inval.wider$sH <- 10 
inval.wider
DiM.presheights.wider <- DiffinMeans(inval=inval.wider, out=FALSE)
UMSprint(DiM.presheights.wider)



# implementation Mathematica by Phil Gregory

inputvalues <- list(snames = c("president","opponent"),
                    # sample 1
                    d1 = W.cm[!is.na(W.cm)],
                    # sample 2
                    d2 = O.cm[!is.na(O.cm)],
                    # Input priors and no. of steps in evaluation of p(r|D_1,D_2,I) & p(delta|D_1,D_2,I)
                    # ndelta = number of steps in delta parameter (mean difference)
                    ndelta = 1000, #100
                    # nr = number of steps in r parameter (ratio of the standard deviations)
                    nr = 1000, # 100
                    # Set prior limits (assumed the same for each data set) on mean (low,high),
                    # and prior limits (assumed the same for each data set) on the
                    # standard deviation (sigmalow, sigmahigh).
                    # upper mean
                    high = 200,
                    # lower mean
                    low = 160,
                    # upper sd
                    sigma.high = 10,
                    # lower sd
                    sigma.low = 3
)

# according to Phil Gregory scheme
inputvalues
DiM.PG.presheights <- DiM.pg(invtyp="pg", inputvalues=inputvalues, print.res=TRUE)
DiM.print.pg(DiM.PG.presheights)


# determine low + high from means
# determine scaleL and scaleH by experimenting

# check before
# change = TRUE -> change to new values
DiM.presheights.newlimits <- DiM.extract.limits(DiM.PG.presheights, low=175, high=182, scaleL=2, scaleH=2, change=TRUE) 
DiM.presheights.plotvalues <- DiM.plot.calc.pg(DiM.presheights.newlimits, BROB=FALSE)
DiM.plot.pg(DiM.presheights.plotvalues, filling=FALSE, BROB=FALSE)
# or directly
DiM.presheights.plotvalues <- DiM.plot.calc.pg(DiM.PG.presheights, low=175, high=182, scaleL=2, scaleH=2, BROB=FALSE)
DiM.plot.pg(DiM.presheights.plotvalues, filling=FALSE, BROB=FALSE)


# infinity is (not) the end...
# use BROB
# library(Brobdingnag)
# for very large numbers
print(10^(1:310))
print(as.brob(10)^(1:3100))



# bayesboot

# https://cran.r-project.org/web/packages/bayesboot/readme/README.html
# Rubin, D. B. (1981). The Bayesian bootstrap. The annals of statistics, 9(1), 130–134. link to paper
# https://projecteuclid.org/euclid.aos/1176345338

# library(bayesboot)

# not run
# cases
pres.Bboot.W <- bayesboot(W.cm[!is.na(W.cm)], mean)
pres.Bboot.O <- bayesboot(O.cm[!is.na(O.cm)], mean)

# persons
pres.Bboot.W <- bayesboot(dats$W.red, mean)
pres.Bboot.O <- bayesboot(dats$O.red, mean)
summary(pres.Bboot.W)
summary(pres.Bboot.O)
plot(pres.Bboot.W)
plot(pres.Bboot.O)

# boot via re-weighting original data set
pres.Bboot.W.1 <- bayesboot(dats$W.red, weighted.mean, use.weights=TRUE)
pres.Bboot.O.1 <- bayesboot(dats$O.red, weighted.mean, use.weights=TRUE)

# compare
difference <- pres.Bboot.W.1-pres.Bboot.O.1
pres.Bboot.diff <- as.bayesboot(difference)
summary(pres.Bboot.diff)
attr(pres.Bboot.diff,"statistic.label") <- "Difference"
par(oma=c(2,1,1,1), "cex.axis"=1, bty="l")
plot(pres.Bboot.diff)

# prob
prob.winner.is.larger <- mean(pres.Bboot.W.1[,1] > pres.Bboot.O.1[,1])
prob.winner.is.larger
# OR
prob.winner.is.larger / (1-prob.winner.is.larger)
1 / (prob.winner.is.larger / (1-prob.winner.is.larger))

str(pres.Bboot.W.1)
str(pres.Bboot.O.1)
str(pres.Bboot.diff)

# Given the model and the data, this is the probability that the mean
# heights of American presidents is above the mean heights of
# American males as given by www.cdc.gov/nchs/data/series/sr_11/sr11_252.pdf
mean(pres.Bboot.W[,1])
mean(pres.Bboot.O[,1])
mean(c(pres.Bboot.W[,1] > 175.9, TRUE, FALSE))
mean(c(pres.Bboot.O[,1] > 175.9, TRUE, FALSE))

# https://en.wikipedia.org/wiki/List_of_average_human_height_worldwide
# US
p1 <- mean(pres.Bboot.W[,1] > 176.4) #2011-2014
p2 <- mean(pres.Bboot.O[,1] > 176.4) #2011-2014
p1
p2
p1vsp2 <- post.comp(post1=p1 , post2=p2, RETURN=TRUE)

# Sweden
p1 <- mean(pres.Bboot.W[,1] > 181.5)
p2 <- mean(pres.Bboot.O[,1] > 181.5)
p1
p2
p1vsp2 <- post.comp(post1=p1 , post2=p2, RETURN=TRUE)

# check diff
hdi(pres.Bboot.W[,1])
hdi(pres.Bboot.O[,1])
hdi(pres.Bboot.diff[,1])


# prob that mean diff is above 2 cm
mean(pres.Bboot.diff[,1] >= 0)
mean(pres.Bboot.diff[,1] >= 2)
mean(pres.Bboot.diff[,1] <= -2)

# differences, secondary run, just example results
0.5325-0.52075 #>=0
0.112-0.1115   #>=2
0.099-0.08625  #<=-2

# sd that mean diff is above 2 cm
sd(pres.Bboot.diff[,1] >= 0)
sd(pres.Bboot.diff[,1] >= 2)
sd(pres.Bboot.diff[,1] <= -2)

# POTUS taller than main opponent
sek <- seq(-5,5,0.1)
prob.diff <- vector()
for(i in 1:length(sek))
{
  prob.diff[i] <- mean(pres.Bboot.diff[,1] > sek[i])
}

# main opponent taller than POTUS
prob.diff.O <- NA
for(i in 1:length(sek))
{
  prob.diff.O[i] <- mean(pres.Bboot.diff[,1] < sek[i])
}

# plot
par(mar=c(4,5,4,5), oma=c(2,1,1,1), cex.axis=0.8)
plot(sek, prob.diff, type="l", col="violetred3", pre.plot=grid(), bty="n",
     main="", ylab="", xlab="",
     axes=FALSE)
vh <- c(0,0.5)
abline(v=vh[1], h=vh[2], lty=3, col="yellowgreen")
points(x=vh[1], y=vh[2], pch=21, col="steelblue", bg="magenta", cex=1.4)
points(x=vh[1], y=prob.diff[sek == 0], pch=21, col="black", bg="yellow", cex=1.4)
points(x=vh[1], y=prob.diff.O[sek == 0], pch=21, col="black", bg="orange", cex=1.4)
lines(sek, prob.diff.O, col="steelblue")

# where meet both curves?
# alt1
#diffi <- abs(prob.diff - prob.diff.O)
#id <- which(diffi == min(diffi))
##points(x=sek[id], y=prob.diff[id], pch=21, col="steelblue", bg="green", cex=1.4)
#points(x=mean(sek[c(id-1,id,id+1)]), y=mean(prob.diff[c(id-1,id,id+1)]), pch=21, col="steelblue", bg="green", cex=1.4)
#
# a#t2
#ID1 <- which(prob.diff > prob.diff.O)
#ID1 <- ID1[length(ID1)]
#ID2 <- which(prob.diff < prob.diff.O)[1]
#xcoord <- mean(sek[c(ID1,ID2)])
#ycoord <- (prob.diff[ID1]+prob.diff.O[ID2])/2
#points(x=xcoord, y=ycoord, pch=21, col="steelblue", bg="green", cex=2)
# >>> both not exact enough

#use this instead
#library(devtools)
#install_github("andrewheiss/reconPlots")
# library(reconPlots)
curve1 <- data.frame(x=sek, y=prob.diff)
curve2 <- data.frame(x=sek, y=prob.diff.O)
xy.meet <- unlist(curve_intersect(curve1, curve2, empirical=TRUE))
# curves meet at
xy.meet
points(x=xy.meet["x"], y=xy.meet["y"], pch=21, col="steelblue", bg="green", cex=2)

# bottom
axis(side=1)

# left
axis(side=2, labels=seq(0,1,by=0.2), at=seq(0,1,by=0.2), col="violetred3", las=1)
mtext("p(winner > opponent)", side=2, line=3, cex.lab=1, las=0, col="violetred3")

# right
axis(side=4, labels=seq(1,0,by=-0.2), at=seq(0,1,by=0.2), col="steelblue", las=1)
mtext("p(opponent > winner)", side=4, line=3, cex.lab=1, las=0, col="steelblue")

mtext("Probability of difference in heights (cm)", outer=TRUE, line=-2, cex=1.5, side=3)
par(fig=c(0,1,0,1), oma=c(1,0,0,0), mar=c(0,0,0,0), new=TRUE)
plot(1, type="n", bty="n", xaxt="n", yaxt="n")
legend("bottom", legend=c("winner","opponent"), lty=1, lwd=2, xpd=TRUE, horiz=TRUE, col=c("violetred3","steelblue"), bty="n", cex=.9)


# compare results (precision)
all.equal(prob.diff, prob.diff.O)


# height ratio
# http://www.nicebread.de/a-compendium-of-clean-graphs-in-r/
# Presidential data up to and including 2008; data from Stulp et al. 2013

# cases ie. elections
pres.h.nona$W.O.ratio <- pres.h.nona$W.cm / pres.h.nona$O.cm
pres.h.nona$W.O.ratio.TF <- pres.h.nona$W.O.ratio > 1
head(pres.h.nona)
colos <- ifelse(pres.h.nona$W.O.ratio.TF, "blue","red")
# pres.h.nona.ratio = ratios without NAs
pres.h.ratio.nona <- pres.h.nona$W.O.ratio[!is.na(pres.h.nona$W.O.ratio)] # get rid of NAs
par(oma=c(2,1,1,1), "cex.axis"=1, bty="l", mfrow=c(1,2))
plotPost(pres.h.ratio.nona, xlab="ratio", ylab="Density", cex.lab=1.2)
lines(density(pres.h.ratio.nona), col="darkred", lty=1, lwd=2)
with(pres.h.nona, plot(year, W.O.ratio, pch=21, col=colos, bg=colos, pre.plot=grid(),
                       bty="n", xlab="year", ylab="Winner/ Main Opponent", type="b", cex.lab=1.2) )
abline(h=1, col="grey", lty=2)
mtext("Presidential heights (ratios)", outer=TRUE, line=-2, cex=1.5, side=3)


# proportion of POTUS larger than main opponent (and won the election...)
sum(pres.h.nona$W.O.ratio.TF) / dim(pres.h.nona)[1]

# now many times after each other
TF <- pres.h.nona$W.O.ratio.TF
TF
TF.n <- TF + 0
TF.n
TF.n.l <- length(TF.n)

# ratio of 'no change', ie. min. 2 subsequent W > 0 ie. chunks > 1
TF.n.tab <- data.frame(pre=TF.n[1:(TF.n.l-1)], post=TF.n[2:TF.n.l])
TF.n.tab
TF.n.2 <- sum(apply(TF.n.tab, 1, sum) > 1)
TF.n.2
TF.n.2/TF.n.l

# create one long vector with all TF values with TRUE = 1, FALSE = 0
TF.asone <- paste(as.character(TF.n),collapse="")
TF.asone
nchar(TF.asone)

# identify different chunk types
# 'bad' brute force approach

# number of characters = subsequent winner >>> main opponent
TF.no.chars.W <- sapply( as.list(unlist(strsplit(TF.asone,"0",fixed=FALSE))), nchar)
TF.no.chars.W
# table how many times in subsequent order the POUTS was LARGER than the main opponent
# 0 = opponent < winner (number of chunks ie. subsequent elections represented
#     in the other table, no mentioning of length ie. number of elections)
# 1, ... = number of times W > O
TF.no.chars.W.tab <- table(TF.no.chars.W)

# number of characters = subsequent winner <<< main opponent
TF.no.chars.O <- sapply( as.list(unlist(strsplit(TF.asone,"1",fixed=FALSE))), nchar)
TF.no.chars.O
# table how many times in subsequent order the POUTS was SMALLER than the main opponent
# 0 = ie. winner > opponent  (number of chunks ie. subsequent elections represented
#     in the other table, no mentioning of length ie. number of elections)
# 1, ... = number of times W < 0
TF.no.chars.O.tab <- table(TF.no.chars.O)

TF.no.chars.W.tab
TF.no.chars.O.tab

sum(as.numeric(names(TF.no.chars.W.tab))*TF.no.chars.W.tab)
sum(as.numeric(names(TF.no.chars.O.tab))*TF.no.chars.O.tab)

# without zero
sum(TF.no.chars.W.tab[-c(1)])
sum(TF.no.chars.O.tab[-c(1)])
# W - sequences (except the null which is not part of it, see above)
6+2+3+1+1
# O - sequences (except the null which is not part of it, see above)
6+3+2+2



