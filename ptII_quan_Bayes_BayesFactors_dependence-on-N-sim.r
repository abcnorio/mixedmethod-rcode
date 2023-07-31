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
# ptII_quan_Bayes_BayesFactors_dependence-on-N-sim.r

# location:
# chap. 6 [6.7.1.3]
# Aktualität von Bayes-Faktoren

# load necessary libs
library(BayesFactor)
library(lattice)
library(pCalibrate)
library(evidence)

# load necessary helper functions
source("ptall_generalfuncs.r")
source("ptII_quan_Bayes_BayesFactors_dependence-on-N-sim_helpfuncs.r")


# simulate BF01 for various N
mu1 <- 100
mu2 <- 100
sigma1 <- 10
sigma2 <- 10

par(oma=c(2,1,4,1), "cex.axis"=1, bty="l", mfrow=c(2,2))
ylim <- c(0,16)

n1 <- 30
n2 <- 30
res <- sim.p.bf(n1,n2,mu1,mu2,sigma1,sigma2, nsim=100)

n1 <- 90
n2 <- 90
res1 <- sim.p.bf(n1,n2,mu1,mu2,sigma1,sigma2, nsim=100)

n1 <- 250
n2 <- 250
res2 <- sim.p.bf(n1,n2,mu1,mu2,sigma1,sigma2, nsim=100)

n1 <- 1000
n2 <- 1000
res3 <- sim.p.bf(n1,n2,mu1,mu2,sigma1,sigma2, nsim=100)

mtext("Simulation Bayes Factors", outer=TRUE, line=2, cex=1.5, side=3)
mtext(expression(paste("dependence of ",BF[0][1]," on N",sep="")), outer=TRUE, line=0, cex=1, side=3)


# all in one as a loop
# growing sample size, same sample size
par(oma=c(2,1,4,1), "cex.axis"=1, bty="l", mfrow=c(2,2))
ens <- c(30,90,250,1000)
for(n in ens) res <- sim.p.bf(N1=n,N2=n,mu1,mu2,sigma1,sigma2,nsim=1000)
mtext("Simulation Bayes Factors", outer=TRUE, line=2, cex=1.5, side=3)
mtext(expression(paste("dependence of ",BF[0][1]," on N",sep="")), outer=TRUE, line=0, cex=1, side=3)


# calculate BFs from t-values
# Morey, R. D. & Rouder, J. N. (2011). Bayes Factor Approaches for Testing Interval Null Hypotheses. Psychological Methods, 16, 406-419
# Rouder, J. N., Speckman, P. L., Sun, D., Morey, R. D., & Iverson, G. (2009). Bayesian t-tests for accepting and rejecting the null hypothesis. Psychonomic Bulletin & Review, 16, 225-237 

# Classical example: Student's sleep data
data(sleep)
head(sleep)
dim(sleep)
plot(extra ~ group, data = sleep)
tres <- t.test(extra ~ group, data = sleep, paired=TRUE)
tv <- tres$statistic
# bf10 = in favor of the alternative hypothesis
bf10 <- ttest.tstat(t=tv, n1=dim(sleep)[1]/2)

# output
tres
tv
exp(bf10[['bf']])


# read in data from Gürtler (2005)
# two sample t-test
diss <- read.table("LG_school-words-raw.tab", header=TRUE, sep="\t")
head(diss)
tail(diss)
dim(diss)
diss.red <- subset(diss, stype %in% c("R","G"))
selvars <- c("ID","age","sex","stype")
diss.red <- diss.red[,selvars]
dim(diss.red)

naids <- which(is.na(diss.red), arr.ind=TRUE)
naids
diss.red.nona <- diss.red[-naids,]
age <- diss.red.nona$age
stype <- factor(diss.red.nona$stype)
sex <- diss.red.nona$sex
dats <- data.frame(age,sex,stype)
age.jit <- jitter(age)


# boxplot
TITLE <- "Study Gürtler"
SUB <- "age ~ sex"
bpx <- boxplot(age ~ stype, plot=FALSE)
bxp(bpx, notch=TRUE, ylab="age", xlab="age versus sex", main="", frame=FALSE,
    boxfill=rainbow(2), border=2/2)
mtext(TITLE, 3, line=2.5, cex=1.5)
mtext(SUB, 3, line=1, cex=1.1)


# age differences between school types G vs. R
# library 'lattice'
bwplot(age.jit ~ stype, data=dats)
tres.stype <- t.test(age.jit ~ stype, data=dats, var.equal=FALSE)
tres.stype
tv.stype <- tres.stype$statistic
pv.stype <- tres.stype$p.value
tv.stype
pv.stype
# bf10 = in favor of the alternative hypothesis
Ns.stype <- table(dats$stype)
Ns.stype
bf10.stype <- ttest.tstat(t=tv.stype, n1=Ns.stype[1], n2=Ns.stype[2])
bf10.stype
# BF10
exp(bf10.stype[['bf']])
# BF01
1/exp(bf10.stype[['bf']])
cohensd(age.jit[stype=="R"],age.jit[stype=="G"])


# age differences between sexes f vs. m
bwplot(age.jit ~ sex, data=diss.red)
tres.sex <- t.test(age.jit ~ sex, data=dats, var.equal=FALSE)
tres.sex
tv.sex <- tres.sex$statistic
pv.sex <- tres.sex$p.value
tv.sex
pv.sex
# bf10 = in favor of the alternative hypothesis
Ns.sex <- table(dats$sex)
bf10.sex <- ttest.tstat(t=tv.sex, n1=Ns.sex[1], n2=Ns.sex[2])
bf10.sex
# BF10
exp(bf10.sex[['bf']])
# BF01
1/exp(bf10.sex[['bf']])
cohensd(age.jit[sex=="m"],age.jit[sex=="w"])


# age differences between sexes f vs. m
bwplot(age ~ sex, data=diss.red)
tres.sex <- t.test(age ~ sex, data=dats, var.equal=FALSE)
tv.sex <- tres.sex$statistic
pv.sex <- tres.sex$p.value
# bf10 = in favor of the alternative hypothesis
Ns.sex <- table(dats$sex)
bf10.sex <- ttest.tstat(t=tv.sex, n1=Ns.sex[1], n2=Ns.sex[2])


tres.sex
# output
# Cohen's d
cohensd(age[sex=="m"],age[sex=="w"])
# t-value and p-value
tv.sex
pv.sex
# BF10
exp(bf10.sex[['bf']])
# BF01
1/exp(bf10.sex[['bf']])



# minimal BF / lower bound
# Sellke et al. 2001

# p-values from table 1 p.63
pv <- c(.2,.1,0.05,.01,0.005,0.001)
res <- sapply(pv, function(i) BF.calib(pv=i))
colnames(res) <- pv
print(res, digits=3)

# example above
dig <- 5
pv <- c(pv.stype,pv.sex)
res <- sapply(pv, function(i) BF.calib(pv=i))
colnames(res) <- signif(pv,dig)
print(res, digits=3)
BF.tp_base


# minimal BF
# library(pCalibrate)
BF.tp_base <- tCalibrate(p=c(pv.stype, pv.sex), n=c(Ns.stype[1],Ns.stype[2]), type="two.sided",alternative="normal")
colnames(BF.tp_base) <- c("p_stype","p_sex")
rownames(BF.tp_base) <- paste("N=",Ns.stype,sep="")
BF.tp_base
BF.tp_base.s <- tCalibrate(p=c(pv.stype, pv.sex), n=c(Ns.stype), type="two.sided",alternative="simple")
BF.tp_base.s
BF.tp_base.s1 <- tCalibrate(p=c(pv.stype, pv.sex), n=sum(Ns.stype), type="two.sided",alternative="simple")
BF.tp_base.s1


# library 'evidence'
# library 'agridat'
data(darwin)
?darwin
# Darwin, C.R. 1876. The effects of cross and self fertilisation in the vegetable kingdom. John Murray, London.
# Charles Darwin( 1876) provided data on the difference in the
# heights attained by selfed and crossed mother plants.
# A data frame with 15 observations on the following variable:
# ‘difference’ the difference in height in inches between each
# paired pair of offspring of a selfed and a crossed mother plant
darwin
B1Nsir(darwin$difference)
     
data(lightspeed)
lightspeed

# comparison of minBF(p) approaches
# Held & Ott 2018 p = 0.05 (table 3)
minbfs <- matrix(data= c(1/2.1,1/2.5,1/3.4,1/3.9,1/6.8,1/7.5,
                         1/6.5,1/8,1/14,1/15,1/28,1/37,
			                   1/11,1/14,1/26,1/28,1/51,1/74,
      			               1/41,1/53,1/112,1/118,1/224,1/368),
	               ncol=4, byrow=FALSE)
colnames(minbfs) <- c("0.05","0.01","0.005","0.001")	   
minbfs

par(oma=c(2,1,1,1), "cex.axis"=1, bty="l")
plot(minbfs[,1], ylim=c(0,1), type="b", bty="n", pch=20, xaxt="n", col="violetred3", pre.plot=grid(),xlab="Formula/ equation used to calculate BF", ylab="minBF(p)", main="")
for(i in 2:4) lines(minbfs[,i], col=i, type="b", pch=20)
axis(1, 1:6, paste("eq.",1:6,sep=""))
legend("top", legend=colnames(minbfs), col=c("violetred3",2:4), title="p value", bty="o", lty=1, lwd=2, cex=0.9, box.col="white", horiz=TRUE, bg="gray90")
mtext(expression(paste("Comparison of minBF(p) approaches",sep="")), outer=TRUE, line=-2.5, cex=1.5, side=3)
apply(minbfs, 2, summary)
apply(minbfs, 2, sd)
apply(minbfs, 2, fivenum.wn)

# ratio range max and minBF
apply(minbfs, 2, function(i) i[1]/i[6])
1/(apply(minbfs, 2, function(i) i[1]/i[6]))


# simulate dependence of p-values and N (sample size)
pes <- pvsN.sim()
pes
log(pes)


# plot cauchy for ES
# see how Wagenmakers et al. (2011) do it - critized by Schimmack (2015) on his blog
sek <- seq(-4,4,0.01)
par(oma=c(2,1,1,1), "cex.axis"=1, bty="l")
plot(sek, dcauchy(sek,scale=sqrt(2)/2), type="l", bty="n", col="darkred", pre.plot=grid(),
     xlab=expression(theta), ylab=expression(paste("density with r=",sqrt(2)/2,sep="")), cex.lab=1.2)
mtext(expression(paste("Cauchy Distribution with scale = ",sqrt(2)/2,sep="")), outer=TRUE, line=-3, cex=1.5, side=3)



