# file:
# ptII_quan_Bayes_BayesFactors_test-hypos.r

# location:
# chap. 6 [6.7.1]
# Bayes-Faktoren und Bayes-Hypothesentesten
# [6.7.1.1]
# Bayesischer Determinationskoeffizient R^{2}

# load necessary libs
library(BayesFactor)
library(arm)
library(lme4)
library(brms)

# load necessary libraries
source("ptall_generalfuncs.r")


# Lindleys Paradox
# https://www.aarondefazio.com/adefazio-bayesfactor-guide.pdf
# p.3
# "The key issue is the handling of small-effects. As n increases and s^2 stays fixed, a fixed
# t-value of say 2 represents a smaller and smaller measured effect, namely:

# xbar propr to 2/sqrt(n)

# Detecting small effects requires a large sample size, and the Bayesian t-test greatly prefers the null hypothesis
# over that of a small effect. This is usually described positively as a Occam’s razor effect, but it does have real
# consequences when we are attempting to find true small effects. Essentially, the BF test will require much more
# data pick up a small effect then a corresponding frequentist test. Often, this is viewed from the opposite side; that
# frequentist tests too easily reject the null hypothesis for large sample sizes. This is more of an issue when testing
# more complex models such as linear regressions with multiple coefficients. Unless the data was actually generated
# from a linear equation with Gaussian noise, large samples will inevitably reject simple null models.  This is in
# contrast on simple point null hypothesis testing, where we can appeal to asymptotic normality."

# Consequences for Lindley’s paradox
# As discussed above, when restricting ourselves to well behaved proper priors, “Lindley’s paradox” as such can not
# be rectified by a careful choice of a proper prior on the alternative hypothesis. When one takes a step back and
# considers the consequences of this, Lindley’s paradox becomes much clearer, as a statement about objective priors
# rather than BF tests in general.
# The central idea is this: It requires a lot of evidence to detect small effects when you use vague priors. The
# idea of running a long experiment when you use a vague prior is nonsensical from an experimental design point of
# view: large amounts of data are useful for picking up small effects, and if you expect such a small effect you should
# encode this sensibly in your prior. The default Cauchy prior used in the BF t-test essentially says that you expect
# a 50% chance that the absolute effect size (Cohen’s d)islargerthan1, which is a completely implausible in many
# settings. For instance, if you are trying to optimize conversion rates on a website with say a 10% base conversion
# rate, an effect size of “1” is a 4x increase in sales! Some software defaults to 0.707 instead of 1 for the Cauchy scale
# parameter, but the effect is largely the same

# Experimental design considerations are important in the choice of a prior, and the Bayesian justification of
# the Bayes factor depends on our priors being reasonable. The choice of using a particular BF test is not a full
# experimental design, just as for a frequentist it is not sufficient to just decide on particular test without regard
# to the eventual sample size. It is necessary to examine other properties of the test, so determine if it will behave
# reasonably in the experimental setting in which we will apply it. So in effect, Lindley’s paradox is just the statement
# that a poorly designed Bayesian experiment won’t agree with a less poorly designed frequentist experiment.
# Put another way, if the Bayes factor is comparing two hypothesis, both of which are unlikely under the data,
# you won’t get reasonable results. You can’t identify this from examining the Bayes factor, you have to look at the
# experiment holistically. The extreme case of this is when neither hypothesis includes the true value of theta. In such
# cases, the BF will generally not converge to any particular value, and may show strong evidence in either direction.


# Lindleys Paradox
set.seed(66755)
n <- 5e+4
daten <- rnorm(n, mean=2/sqrt(n), sd=1)
hist(daten, prob=T, pre.plot=grid())
lines(density(daten), col="darkred")
summary(daten)
sd(daten)

# Cohen's d
(mean(daten)-0)/1

# classical t-test
t.test(daten)

# unspecific null hypothesis
bf1 <- ttestBF(daten)
# one-sided test delta < 0 versus point null
bf2 <- ttestBF(daten, nullInterval=c(-Inf,0))
bf1
bf2
# delta < 0 versus delta > 0
bf2[2]/bf2[1]
# same from opposite direction
bf3 <- ttestBF(daten, nullInterval=c(0,Inf))
bf3
bf3[1]/bf3[2]



bfall <- c(bf1,bf2)
bfall
plot(bfall)

bfmat <- bfall/bfall
bfmat



# two sample t-test

# read data from school study
diss <- read.table(file="LG_school-words-raw.tab", sep="\t", header=TRUE)
dim(diss)
head(diss)
tail(diss)

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
dats$age.jit <- age.jit <- jitter(age)
head(dats)


# tables
table(age)
table(stype)
# not shown
# table(age.jit)
tapply(age,stype, summary)
tapply(age.jit,stype, summary)
tapply(age,stype, sd)
tapply(age.jit,stype, sd)


# boxplots
TITLE <- "Study Gürtler"
SUB <- "age ~ sex"
par(mfrow=c(1,2))
bpx <- boxplot(age ~ stype, plot=FALSE)
bxp(bpx, notch=TRUE, ylab="age", xlab="age versus sex", main="", frame=FALSE,
    boxfill=c("green","darkorange"), border=2/2)
mtext(TITLE, 3, line=2.5, cex=1.5)
mtext(SUB, 3, line=1, cex=1.1)

bpx <- boxplot(age.jit ~ stype, plot=FALSE)
SUB <- "jitter(age) ~ sex"
bxp(bpx, notch=TRUE, ylab="age", xlab="age versus sex", main="", frame=FALSE,
    boxfill=c("green","darkorange"), border="darkblue")
mtext(TITLE, 3, line=2.5, cex=1.5)
mtext(SUB, 3, line=1, cex=1.1)



# age
# simple t-test classical
t.test(age ~ stype, var.eq=FALSE)
# effect size
cohensd(age[stype == "G"], age[stype == "R"])

# Bayes Factor t-test age versus school type
bf2s.1 <- ttestBF(formula=age ~ stype, data=dats)
bf2s.1
bf2s.2 <- ttestBF(formula=age ~ stype, data=dats, nullInterval=c(-Inf,0))
bf2s.2
bf2s.2[1]/bf2s.2[2]
# check MCMC chains
samps1 <- posterior(bf2s.1, iterations=1e+5)
plot(samps1, bty="n", col="violetred3")
plot(bf2s.1)
summary(samps1)


# age.jit
# simple t-test classical
t.test(age.jit ~ stype, var.eq=FALSE)
# effect size
cohensd(age.jit[stype == "G"], age[stype == "R"])

# Bayes Factor t-test age.jit versus school type
bf2s.1.jit <- ttestBF(formula=age.jit ~ stype, data=dats)
bf2s.1.jit
bf2s.2.jit <- ttestBF(formula=age.jit ~ stype, data=dats, nullInterval=c(-Inf,0))
bf2s.2.jit
bf2s.2.jit[1]/bf2s.2.jit[2]
# check MCMC chains
samps1.jit <- posterior(bf2s.1.jit, iterations=1e+5)
plot(samps1.jit, bty="n", col="violetred3")
plot(bf2s.1.jit)
summary(samps1.jit)


dats$sex <- as.factor(dats$sex)
str(dats)

# Bayes Factor t-test age versus sex
ttestBF(formula=age.jit ~ sex, data=dats)

# Bayes Factor anova age versus (school type + sex)
anovaBF(formula=age.jit ~ stype + sex, data=dats)

# with random effect = sex
anovaBF(formula=age.jit ~ stype + sex, data=dats, whichRandom="sex")

# plot posterior samples
bf.best <- ttestBF(formula=age.jit ~ sex, data=dats)
bf.best.post <- posterior(bf.best, iterations=1e+5)
plot(bf.best.post, bty="n", col="violetred3")



# Bayes Factor t-test age versus sex
bf2s.3 <- ttestBF(formula=age ~ sex, data=dats)
bf2s.3
# boxplot
plot(age ~ sex)
t.test(age ~ sex, var.eq=FALSE)
tapply(age, sex, summary)
tapply(age, sex, sd)
# change to sex == "f" instead of sex == "w"
cohensd(age[sex == "w"], age[sex == "m"])
plot(bf2s.3)


# Bayes Factor anova age versus (school type + sex)
bff <- anovaBF(formula=age ~ stype + sex, data=dats)
bff
bff[2]/bff[3]
bff[2]/bff[4]


# with random effect = sex
bf2s.4 <- anovaBF(formula=age ~ stype + sex, data=dats, whichRandom="sex")
bf2s.4


# classical anova age versus (school type + sex)
anova(lm(age ~ stype + sex, data=dats))
display(diss.lmer <- lmer(age ~ stype + (1|sex), data=dats))
summary(diss.lmer)


# Bayes Factor linear model age versus school type
bf2s.5 <- lmBF(formula=age ~ stype, data=dats)
# Bayes Factor linear model age versus sex
bf2s.6 <- lmBF(formula=age ~ sex, data=dats, whichRandom="sex")
bf2s.5
bf2s.6
bf2s.7 <- bf2s.5/bf2s.6
bf2s.8 <- c(bf2s.4,bf2s.7)
bf2s.8
1/(bf2s.4/bf2s.7)
plot(bf2s.8)


# check against Stan with brms
# full Bayesian
# library(brms)

# not really good...
brm1 <- brm(age ~ stype + (stype|sex), data=dats)
summary(brm1)
plot(brm1)
pp_check(brm1, nsamples=100) # horrible with age as a category!

# not really good...
brm2 <- brm(age ~ stype + (1|sex), data=dats)
summary(brm2)
plot(brm2)
pp_check(brm2, nsamples=100) # horrible with age as a category!

# problematic brm1 and brm2 is the age variable
# therefor: add the jitter to 'age'
# because actually 'age' is not categorial but continuous
# but one documents mostly the year, not year-month-date
brm3 <- brm(jitter(jitter(age)) ~ stype + (1|sex), data=dats)
summary(brm3)
plot(brm3)
pp_check(brm3, nsamples=100) # less horrible due to jitter()

# not better...
# change to a poisson model
brm4 <- brm(age ~ stype, data=dats, family=poisson(), save_all_pars=TRUE)
summary(brm4)
plot(brm4)
pp_check(brm4, nsamples=100) #GAU!

# still the same...
# simple model
brm5 <- brm(age ~ stype, data=dats, family=gaussian(), save_all_pars=TRUE)
summary(brm5)
plot(brm5)
pp_check(brm5, nsamples=100) # like initial case, horrible!

# not convincing till now...

# create a more continous age variable...
age.l <- length(age)
age.enh <- age + runif(age.l,0,1)*sample(c(-1,1),age.l,replace=TRUE)
dats$age.enh <- age.enh

# comparisons summary statistics
mean(age-age.jit)
mean(age-age.enh)
mean(age.jit-age.enh)
t(apply(cbind(age,age.jit,age.enh),2,fivenum.wn))
#fivenum.wn(age)
#fivenum.wn(age.jit)
#fivenum.wn(age.enh)

# compare densities
hist(age, prob=TRUE, col="skyblue", pre.plot=grid(), main="Study Gürtler (2005)")
lines(density(age), col="darkred")
lines(density(age.jit), col="blue")
lines(density(age.enh), col="olivedrab")
legend("topright", legend=c("age","age.jit","age.enh"),
       col=c("darkred","blue","olivedrab"),
       lwd=2, bty="n", bg="yellow", cex=1.2)

lapply(list(age,age.enh), summary)
lapply(list(age,age.enh), sd)


# that looks pretty well!!!!
brm6 <- brm(age.enh ~ stype, data=dats, family=gaussian(), save_all_pars=TRUE)
summary(brm6)
plot(brm6)
pp_check(brm6, nsamples=100)
# much better due to introduction of general uncertainty!
# do not take the uncertainty serious on the level of the single case, but in general it is much better
marginal_effects(brm6)
plot(marginal_effects(brm6), points=TRUE)

# pp looks better for brm6 compared to brm5
pp_check(brm5, nsamples=100)
pp_check(brm6, nsamples=100)
# but the BF and other criteria show a clear preference for brm5 over brm6
# ie. the introduced uncertainty is not covered as good as the original data by the factor

bayes_factor(brm6, brm5)
# Estimated Bayes factor in favor of bridge1 over bridge2: 0.00000
# not really meaningful, because models are different, ie. outcome

bayes_factor(brm5, brm6)
# Estimated Bayes factor in favor of bridge1 over bridge2: 63486721668608555378828842.00000
# not really meaningful, because models are different, ie. outcome

# Bayes R^2
bayes_R2(brm5)
bayes_R2(brm6)

# comparisons effects
fixef(brm5)
fixef(brm6)
# compare estimates
(1-fixef(brm6)/ fixef(brm5))*100
bayes_factor(brm5, brm6)

# age vs. stype
table(age,stype)


# compare brm5 and brm6 -> differences for sigma, not fixed effects...
brm5 <- add_criterion(brm5, criterion=c("loo","waic","kfold","R2","marglik"))
brm6 <- add_criterion(brm6, criterion=c("loo","waic","kfold","R2","marglik"))
# should work if not brm5 and brm6 would have a different dependent variable
for(i in c("loo","waic","kfold"))
{
 cat("\n",i)
 print(brms:::loo_compare.brmsfit(brm5,brm6, criterion=i))
}
# compare estimates
summary(brm5)
summary(brm6)


# add counts instead of age -> agecounts + vars (counts) + log(...) + (1|stype)
# prepare data with ftable()
ftable(age ~ sex,data=dats)
ftable(age ~ sex + stype,data=dats)
count.age <- as.data.frame(ftable(age ~ sex + stype,data=dats))
colnames(count.age)[colnames(count.age) == "Freq"] <- "Freq.age.cat"
count.age

# model with categ age
brm.p1.0 <- brm(Freq.age.cat ~ age + stype, data=count.age, family=poisson(), save_all_pars=TRUE)

# summary
summary(brm.p1.0)
brm6

# model check
plot(brm.p1.0)
pp_check(brm.p1.0, nsamples=100)
pp_check(brm6)

# R2
bayes_R2(brm6)
bayes_R2(brm.p1.0)

# BF
bayes_factor(brm.p1.0, brm6)

# plot effects
plot(marginal_effects(brm.p1.0), points=TRUE)

# does not work, because models are too different
loo(brm6, brm.p1.0, reloo=TRUE)

# interaction plots
# https://bookdown.org/rdpeng/exdata/plotting-and-color-in-r.html
colos <- colorRampPalette(c("blue","darkred"))
colos(6)
with(count.age, interaction.plot(sex, stype,Freq.age.cat, legend=TRUE, col=colos(2)))
with(count.age, interaction.plot(stype, sex,Freq.age.cat, legend=TRUE, col=colos(2)))
with(count.age, interaction.plot(age, sex,Freq.age.cat, legend=TRUE, col=colos(2)))
with(count.age, interaction.plot(age, stype,Freq.age.cat, legend=TRUE, col=colos(2)))
with(count.age, interaction.plot(stype, age,Freq.age.cat, legend=TRUE, col=colos(6)))
with(count.age, interaction.plot(sex, age,Freq.age.cat, legend=TRUE, col=colos(6)))

# further models to test ...
# Freq.age.cat ~ age + stype
# Freq.age.cat ~ age + stype + sex
# Freq.age.cat ~ age + stype * sex
# Freq.age.cat ~ (1|age) + stype * sex
# ...



# some more on the wordfreq study
# we change the dependend variable from age to word frequencies...

head(diss)
selvars1 <- c("ID","age","sex","stype","W.noSC","uW.noSC","cc.noSC","W.wSC","uW.wSC")
diss.red1 <- diss[,selvars1]
dim(diss.red1)
naids1 <- which(is.na(diss.red1), arr.ind=TRUE)
naids1
diss.red.nona1 <- diss.red1[-naids1,]
which(is.na(diss.red.nona1))
head(diss.red.nona1)
# reduce number of unused levels
diss.red.nona1$stype <- factor(diss.red.nona1$stype)
diss.red.nona1$age.log <- log(diss.red.nona1$age)
diss.red.nona1$age.cat <- factor(diss.red.nona1$age)
diss.red.nona1$W.noSC.log <- log(diss.red.nona1$W.noSC)
head(diss.red.nona1)
str(diss.red.nona1)
BFout <- lmBF(W.noSC.log ~ age.log + age.cat + sex + stype, data=diss.red.nona1, iterations=1e+3)
BFout.post <- lmBF(W.noSC.log ~ age.log + age.cat + sex + stype, data=diss.red.nona1, posterior=TRUE, iterations=1e+3)
summary(BFout)
summary(BFout.post)

samps <- posterior(BFout, iterations=1e+3) 
summary(samps)
plot(samps, col="violetred3", bty="n")

BFout2 <- lmBF(W.noSC.log ~ age.log + age.cat + sex + stype + sex:stype, data=diss.red.nona1)
BFout / BFout2

BFout3 <- lmBF(W.noSC.log ~ age.log + sex + stype + sex:stype, data=diss.red.nona1)
BFout / BFout3

c(BFout, BFout2, BFout3)

R2 <- summary(lm(W.noSC.log ~ age.log + age.cat + sex + stype, data=diss.red.nona1))["r.squared"]
R2 <- as.numeric(R2)
linearReg.R2stat(dim(diss.red.nona1)[1], p=4, R2=R2)

# with random effects
BFout.M <- lmBF(W.noSC.log ~ age.log + age.cat, data=diss.red.nona1)
BFout.M1 <- lmBF(W.noSC.log ~ sex + stype, data=diss.red.nona1, whichRandom=c("sex","stype"))
BFout.M2 <- lmBF(W.noSC.log ~ age.log + age.cat + sex + stype, data=diss.red.nona1, whichRandom=c("sex","stype"))

BFout.M
BFout.M1
BFout.M2

bfall <- c(BFout.M, BFout.M1, BFout.M2)
bfall

# random effect
BFout.M1/BFout.M

summary(BFout.M)
plot(BFout.M)

BFout.G <- generalTestBF(W.noSC.log ~ age.log + age.cat + sex + stype + sex:stype, data=diss.red.nona1)
BFout.G
plot(BFout.G)

BFout.G1 <- generalTestBF(W.noSC.log ~ age.log + age.cat + sex + stype + sex:stype, data=diss.red.nona1, whichRandom=c("sex","stype"))
BFout.G1
plot(BFout.G1)
BFout.G/max(BFout.G)





