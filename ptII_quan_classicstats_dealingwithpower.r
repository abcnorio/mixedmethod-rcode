# file:
# ptII_quan_classicstats_dealingwithpower.r

# location:
# chap. 4 [4.6.3]
# Der Umgang mit Power

# load necessary libs
library(nlme)
library(lme4)
library(simr)
library(longpower)


# test for power
power.t.test(n=30, delta=1.1, sd=2.9, sig.level=0.01, power=NULL, type="paired", alternative="two.sided")

# test for required sample size
power.t.test(n=NULL, delta=1.1, sd=2.9, sig.level=0.01, power=0.9, type="paired", alternative="two.sided")


# read data Habil AAH (2007)
daten.i <- read.table(file="AAH_nachw_Habil.tab", sep="\t", header=TRUE)
head(daten.i)
tail(daten.i)

daten.i[,"zeit"] <- factor(daten.i[,"zeitn"])
daten.i[,"subject"] <- as.factor(daten.i[,"subject"])
daten.i[,"grpzugeh"] <- as.factor(daten.i[,"grpzugeh"])
daten.i[,"gru"] <- as.factor(daten.i[,"gru"])
str(daten.i)

# define contrasts
contrasts(daten.i[,"gru"])
contrasts(daten.i[,"gru"]) <- matrix(data=c(0.5,0.5,-0.5,-0.5,
                                            0.5,-0.5,0.5,-0.5,
                                            0.5,-0.5,-0.5,0.5),nrow=4)
contrasts(daten.i[,"gru"])


# plot
daten.i.grp <- groupedData(nachw ~ zeitn | subject, outer=~gru, inner=~subgr, data=daten.i)
# one person per row
plot(daten.i.grp,layout=c(14,8), main="Huber (2007)", xlab="time points", ylab="post-hoc test after training")
# lmList
fit1lm <- lmList(nachw ~ zeitn | subject, data=daten.i.grp)
fit1lm


# simr
# https://github.com/pitakakariki/simr/issues/96
# on missing fixed effects, test argument, along argument, etc.
m0 <- lmer(nachw ~ poly(zeitn,2) + gru + (1|subject) + (1|grpzugeh), data=daten.i)
m1 <- lmer(nachw ~ poly(zeitn,2) + gru + (zeitn-1|subject) + (1|grpzugeh), data=daten.i)
m2 <- lmer(nachw ~ poly(zeitn,2) + gru + (zeitn-1|subject) + (1|subject) + (1|grpzugeh), data=daten.i)
# fails on R 3.6.3 (Linux, Debian buster, install from CRAN), not on R 4.0.2 (Windows)
m3 <- lmer(nachw ~ poly(zeitn,2) + gru + (zeitn|subject) + (1|grpzugeh), data=daten.i)
anova(m0,m1,m2,m3)
anova(m0,m1,m3)
anova(m0,m1,m2)
#m2 > m3 > m1 > m0

#> sessionInfo()
#R version 4.0.2 (2020-06-22)
#Platform: x86_64-w64-mingw32/x64 (64-bit)
#
#> anova(m0,m1,m2,m3)
#refitting model(s) with ML (instead of REML)
#Data: daten.i
#Models:
#    m0: nachw ~ poly(zeitn, 2) + gru + (1 | subject) + (1 | grpzugeh)
#m1: nachw ~ poly(zeitn, 2) + gru + (zeitn - 1 | subject) + (1 | grpzugeh)
#m2: nachw ~ poly(zeitn, 2) + gru + (zeitn - 1 | subject) + (1 | subject) + 
#    m2:     (1 | grpzugeh)
#m3: nachw ~ poly(zeitn, 2) + gru + (zeitn | subject) + (1 | grpzugeh)
#npar    AIC    BIC  logLik deviance   Chisq Df Pr(>Chisq)    
#m0    9 2475.4 2515.7 -1228.7   2457.4                          
#m1    9 2476.7 2516.9 -1229.3   2458.7  0.0000  0  1.0000000    
#m2   10 2465.5 2510.2 -1222.7   2445.5 13.2078  1  0.0002788 ***
#    m3   11 2467.2 2516.4 -1222.6   2445.2  0.2349  1  0.6278806    
#---
#    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

summary(m2)
anova(m2)
plot(m2)

# best model
# nachw ~ poly(zeitn,2) + gru + (zeitn-1|subject) + (1|subject) + (1|grpzugeh)
doTest(m2, fcompare(~ poly(zeitn,2)))
# failed to converge
#doTest(m2, fcompare(~ gru))
doTest(m2, fcompare(~ poly(zeitn,2) + gru))

doTest(m2, rcompare(~ (1|subject)))
doTest(m2, rcompare(~ (1|grpzugeh)))
doTest(m2, rcompare(~ (zeitn-1|subject)))

doTest(m2, compare(~ poly(zeitn,2) + (zeitn-1|subject) + (1|grpzugeh),"lr"))
doTest(m2, compare(~ poly(zeitn,2) + gru + (1|subject) + (1|grpzugeh),"lr"))
#doTest(m2, compare(~ poly(zeitn,2) + gru + (zeitn-1|subject) + (1|subject) + (1|grpzugeh),"pb"))

fixef(m2)
ranef(m2)

# various types of tests
# does not work
doTest(m2, fixed("gru","z"))
doTest(m2, fixed("gru","t"))
# therefor use for categorial terms
doTest(m2, fixed("gru","lr"))
doTest(m2, fixed("gru","f"))

# simulate test to leave out unique level for each subject
p1 <- powerSim(m2, test=rcompare(~ (1|subject)), nsim=50)
print(p1)
pc1 <- powerCurve(m2, along="zeitn", test=rcompare(~ (1|subject)), nsim=50)
plot(pc1)

# we test in dependence of the measurement points on 'zeitn' (total of n=6)
pc1.1 <- powerCurve(m2, along="zeitn", test=rcompare(~ (1|subject)), nsim=50, breaks=1:6)
print(pc1.1)
plot(pc1.1)

# wrong call, see manpage ?extend
# reduce number of levels and see how power drops down into the cellar
tp.a <- 6
# factor levels
6*(1:6)
# extend model by this new configuration
m2.a <- extend(m2, within="zeitn", n=tp.a)
pc2.a <- powerCurve(m2.a, along="zeitn", test=rcompare(~ (1|subject)), nsim=50, breaks=1:tp.a)
print(pc2.a)
plot(pc2.a)


tp.b <- 25
# factor levels
length(levels(daten.i$subject))
112*(1:25)
# increase 'within' - wrong!
m2.b <- extend(m2, within="zeitn", n=tp.b)
pc2.b <- powerCurve(m2.b, along="zeitn", test=rcompare(~ (1|subject)), nsim=50, breaks=1:tp.b)
print(pc2.b)
plot(pc2.b)

# DO IT BETTER!
# now the proper call
tp.c <- 25
# factor levels
length(levels(daten.i$subject))
112*(1:25)
# increase 'along' - correct!
m2.c <- extend(m2, along="zeitn", n=tp.c)
pc2.c <- powerCurve(m2.c, along="zeitn", test=rcompare(~ (1|subject)), nsim=50, breaks=1:tp.c)
print(pc2.c)
plot(pc2.c)

# increase levels again and see power rising high
tp.d <- 6
dim(daten.i)
# factor levels
length(daten.i$subject) * (1:tp.d)
672*(1:6)
# increase 'within' - correct!
m2.d <- extend(m2, within="zeitn+subject", n=tp.d)
pc2.d <- powerCurve(m2.d, along="zeitn", test=rcompare(~ (1|subject)), nsim=50, breaks=1:tp.d)
print(pc2.d)
plot(pc2.d)
# here large power is already with zeitn >=3 (power=.7) and zeitn >=4 (power >0.95)


# does not work
# do not extend by 'along' AND 'within'
m2.e <- extend(m2, along="zeitn", within="zeitn+subject", n=tp.d)


# test for a specific effect against a value
# we use a simple model out of didactic reasons
# with only one fixed effect and one random effect
mx <- lmer(nachw ~ gru + (1|subject), data=daten.i)
summary(mx)
fixef(mx)
mx1 <- mx
levels(daten.i[,"gru"])
contrasts(daten.i[,"gru"])
# see second column
# we test 'PAm+PPm' vs. 'PAo+PPo'
fixef(mx1)["gru2"]
fixef(mx1)["gru2"] <- 0.8
fixef(mx1)["gru2"]
mx2 <- mx
fixef(mx2)["gru2"]
fixef(mx2)["gru2"] <- 0.5
fixef(mx2)["gru2"]
ps.mx <- powerSim(mx, nsim=50)
pc.mx <- powerCurve(mx, nsim=50)
ps.mx1 <- powerSim(mx1, nsim=50)
pc.mx1 <- powerCurve(mx1, nsim=50)
ps.mx2 <- powerSim(mx2, nsim=50)
pc.mx2 <- powerCurve(mx2, nsim=50)

# be aware that the 'lower' estimate is detected more difficult than the 'larger' estimate
# i.e. different power with growing number of levels in gru (= group)
# this is congruent with an assumption that large effects are detected more
# easily than small effects
# outputs
print(ps.mx)
print(ps.mx1)
print(ps.mx2)
#
print(pc.mx)
print(pc.mx1)
print(pc.mx2)
# plots
plot(pc.mx)
plot(pc.mx1)
plot(pc.mx2)



# long power
# does not work out because of too many grouping levels
lmmpower(m2, n=NULL, parameter=6, delta=-0.2, t=seq(1,6,1), power=0.8)
# take a reduced model
m0.red <- lmer(nachw ~ poly(zeitn, 2) + gru + (1+zeitn|subject), data=daten.i)
summary(m0.red)
lmmpower(m0.red, n=NULL, parameter=6, delta=-0.2, t=seq(1,6,1), power=0.8)
lmmpower(m0.red, n=NULL, parameter=6, pct.change=0.2, t=seq(1,6,1), power=0.8)

# analyse for n based on delta sequence and percentage change
ds <- seq(-0.4,-0.01,0.005)
ds.l <- length(ds)
pct <- seq(0.1,0.4, length.out=ds.l)
m0.red.lmmpwr <- matrix(data=NA, nrow=ds.l, ncol=2)
colnames(m0.red.lmmpwr) <- c("N(delta)","N(pct)")
for(i in 1:ds.l)
{
  m0.red.lmmpwr[i,"N(delta)"] <- lmmpower(m0.red, n=NULL, parameter=6, delta=ds[i], t=seq(1,6,1), power=0.8)[["N"]]
  m0.red.lmmpwr[i,"N(pct)"] <- lmmpower(m0.red, n=NULL, parameter=6, pct.change=pct[i], t=seq(1,6,1), power=0.8)[["N"]]
  # N = 2*n (output of lmpower) = total sample size
  # n = sample size for each group
}
head(m0.red.lmmpwr)
dim(m0.red.lmmpwr)

# plot results
par(oma=c(2,1,1,1), "cex.axis"=1, bty="l", mfrow=c(2,1))
plot(ds, log(m0.red.lmmpwr[,"N(delta)"]), main="", pre.plot=grid(), bty="l", xlab=expression(delta), ylab="log(n)", col="magenta", type="l")
plot(pct, (m0.red.lmmpwr[,"N(pct)"]), main="", pre.plot=grid(), bty="l", xlab="percentage change", ylab="n", col="magenta", type="l")
mtext("research study Huber (2007)", outer=TRUE, line=-1.7, cex=1.5, side=3)
mtext("Power simulation with lmmpower() - required sample sizes", outer=TRUE, line=-3.1, cex=1, side=3)


# check via binomial test
# p-value = probability of empirical value or extreme!
binom.test(x=9, n=10, p=.05)

# exact probability for 9 of 10 with p=0.05
1/( choose(10,9) * 0.05^9 * (1-0.05)^(10-9) )
# exact probability for 10 out of 10 with p=0.05
1/( choose(10,10) * 0.05^10 * (1-0.05)^(10-10) )
# the same, but we have to sum up for the p-value
1/ (dbinom(x=9, prob=0.05, size=10) + dbinom(x=10, prob=0.05, size=10) )
# equals
1/ sum(dbinom(x=9:10, prob=0.05, size=10))
# equals
1/binom.test(x=9, n=10, p=.05, alternative="greater")$p.value
1/binom.test(x=9, n=10, p=.05, alternative="less")$p.value
1/binom.test(x=9, n=10, p=.05, alternative="two.sided")$p.value

# exact cum. prob. for 0 to 9 of 10 or less with p=0.05
# that's a different question compare to '9 or 10 out of 10' (= p-value)
binoms <- sapply(seq_along(0:9), function(i) ( choose(10,i)*(0.05)^i*(1-0.05)^(10-i) ) )
binoms
sum(binoms)
lbinoms <- sapply(seq_along(0:9), function(i) ( lchoose(10,i)+i*log(0.05)+(10-i)*log(1-0.05) ) )
lbinoms
exp(lbinoms)
sum(exp(lbinoms))


# number of research studies required to get 9 'significant' on 5% crit. level
100/5*9
# number of participants required if one study has n=100
100/5*9 * 100


# calculate probabilities
# Bem study
# assumed power=0.6 -> get 9 out of 10 successful
1-pbinom(9,10,.6) + dbinom(9,10,.6)
1-cumsum(dbinom(0:(10-1),10,.6))

