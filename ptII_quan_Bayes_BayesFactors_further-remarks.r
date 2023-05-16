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
# ptII_quan_Bayes_BayesFactors_further-remarks.r

# location:
# chap. 6 [6.7.1.6]
# Bayes-Faktoren — und nun?

# load necessary libs
library(brms)


# KRUSCHKE

# BF mean and ES different



# BF and prior probs

# choose not just by usings BF's without the priors, ie. different models = different priors
# not true: all different models have the same prior probs

# detection rate / false-alarm rate = BF
# BF * prior odds = posterior odds ie BF converts prior odds to attain posterior odds by seeing the data

# low prior probs even with high BF's is still a low posterior prob!!!
# BF = change in opinion by seeing the data does not mean it is for real!

# lesson learned: if doing random screening or testing, it is not ok to use only the BF alon to interpret results
# of the diagnostic test. The prior probs must be included, and the decision should be made based on posterior probs.

# M = Model
# D = Data

# prior prob for having a disease
# random draw from population with prior probs
p_disease <- 0.01				      #M=1
p_nodisease <- 1-p_disease		#M=2

# diagnostic test
# binary outcomes
test.positive <- 1				#D=1
test.negative <- 0				#D=0

# correct detection = sensitivity
p_test.positive_disease <- 0.97	#p(D=1|M=1)
p_test.negative_disease <- 1-p_test.positive_disease	#p(D=0|M=1)

# false rate alarms
p_test.positive_nodisease <- 0.05	#p(D=1|M=2)
# specifity
p_test.negative_nodisease <- 1-p_test.positive_nodisease	#p(D=0|M=2)

# posteriors
# p(M=1|D=1) = p(D=1|M=1)p(M=1)/p(D=1)
# p(M=2|D=1) = p(D=1|M=2)p(M=2)/p(D=1)
# posterior odds
# p(M=1|D=1)/p(M=2|D=1) =  p(D=1|M=1)/p(D=1|M=2) * p(M=1)/p(M=2)

# detection vs false alarm
post_detvsfa <- p_test.positive_disease/p_test.positive_nodisease * p_disease/p_nodisease
BF_detvsfa <- p_test.positive_disease/p_test.positive_nodisease
prior.detvsfa <- p_disease/p_nodisease

# outputs
# prior
prior.detvsfa
# BF as incdication of change of expectations / prior believe
BF_detvsfa
# posterior
post_detvsfa

# data LG word frequency schooltype age sex 
# read data
diss <- read.csv("LG_school-words-raw.tab", header=TRUE, sep="\t")
str(diss)
head(diss)
namen <- names(diss)
diss$SS <- paste(diss$sex,diss$schooltype,sep="")
diss$stype <- factor(diss$stype)
diss$sex <- factor(diss$sex)
dim(diss)

# create categories for age with different breaks
diss$age.cat <- cut(as.numeric(diss$age), breaks=c(13,16,19,25), include.lowest=TRUE, right=TRUE, labels=c("(14-16]","(17-19]","(20-25]"))
diss$age.cat <- factor(diss$age.cat)
diss$age.cat1 <- cut(as.numeric(diss$age), breaks=c(13,19,25), include.lowest=TRUE, right=TRUE, labels=c("(14-19]","(20-25]"))
diss$age.cat1 <- factor(diss$age.cat1)

diss$W.noSC <- as.integer(diss$W.noSC)
diss$W.noSC.log <- log(diss$W.noSC)
diss$age.log <- log(diss$age)

diss$SS <- factor(diss$SS)
diss$schooltype <- factor(diss$schooltype)

diss$SSn <- as.numeric(factor(diss$SS)) #as.numeric(factor(paste(diss$sex,diss$schooltype,sep="")))
diss$sexn <- as.numeric(factor(diss$sex))
diss$stypen <- as.numeric(factor(diss$schooltype))
head(diss)

yName <- "W.noSC.log"
xName <- c("age.log","sexn","stypen")
fileNameRoot <- "Diss-"
numSavedSteps <- 15000
thinSteps <- 50


# get it from Kruschke...
source("DBDA2E-utilities.R")
source("Jags-Ymet-XmetMulti-Mrobust.R")

# prepare what we want to analyze and remove NAs
diss1 <- diss[,c(yName,xName)]
naid <- which(is.na(diss1), arr.ind=TRUE)[,1]
diss.nona <- diss1[-naid,]

# library(brms)
# t11
diss.res.t11 <- brm(bf(log(W.noSC) ~ log(age) + age.cat1 + stype + sex, sigma ~ 0 + stype * sex), data=diss, family=student(), save_all_pars=TRUE)
# t11 diff priors
# just a model
model.stan.t11 <- bf(log(W.noSC) ~ log(age) + age.cat1 + stype + sex, sigma ~ 0 + stype * sex)
model.stan.t11
prior.t11 <- get_prior(model.stan.t11, data=diss)

# define our own priors, no theoretical background intended...
prior.t11.pr1 <- c(
	prior(normal(0,3), class=b),
	prior(normal(0,3), class=b, coef="age.cat120M25"),
	prior(normal(0,3), class=b, coef="logage"),
	prior(normal(0,3), class=b, coef="sexw"),
	prior(normal(0,3), class=b, coef="stypeG"),
	prior(normal(0,3), class=b, coef="stypeR"),
	prior(student_t(3,5,10), class=Intercept),
	prior(cauchy(0,3), class=b, dpar=sigma),
	prior(cauchy(0,3), class=b, coef="sexw", dpar=sigma),
	prior(cauchy(0,3), class=b, coef="stypeB", dpar=sigma),
	prior(cauchy(0,3), class=b, coef="stypeG", dpar=sigma),
	prior(cauchy(0,3), class=b, coef="stypeG:sexw", dpar=sigma),
	prior(cauchy(0,3), class=b, coef="stypeR", dpar=sigma),
	prior(cauchy(0,3), class=b, coef="stypeR:sexw", dpar=sigma)
)
prior.t11.pr1
identical(prior.t11,prior.t11.pr1)

diss.res.t11.pr1 <- brm(bf(log(W.noSC) ~ log(age) + age.cat1 + stype + sex, sigma ~ 0 + stype * sex),
                        data=diss, family=student(),
                        prior=prior.t11.pr1, save_all_pars=TRUE)

prior_summary(diss.res.t11, data=diss)
prior_summary(diss.res.t11.pr1, data=diss)
prior.t11.pr1

# nonsense! almost same results... but different update of the prior knowledge!
# be aware that this is enough to change a Bayes Factor EVEN if the model basically is identical!
# and even estimations are basically identical...
BF <- bayes_factor(diss.res.t11, diss.res.t11.pr1)
BF # clearly PRO 'diss.res.t11'
format(BF$bf, scientific=TRUE)
# just an example
format(27621598260.94, scientific=TRUE)

# re-check coefficients percent
(1-fixef(diss.res.t11) / fixef(diss.res.t11.pr1))*100
round((1 - fixef(diss.res.t11) / fixef(diss.res.t11.pr1))*100,3)

bayes_R2(diss.res.t11)
bayes_R2(diss.res.t11.pr1)
# re-check with percent
(1-bayes_R2(diss.res.t11) / bayes_R2(diss.res.t11.pr1))*100

# model checks
pp_check(diss.res.t11, nsamples=100)
pp_check(diss.res.t11.pr1, nsamples=100)


summary(diss.res.t11)
summary(diss.res.t11.pr1)

#~identical!
LOO1 <- LOO(diss.res.t11, reloo=TRUE)
LOO2 <- LOO(diss.res.t11.pr1, reloo=TRUE)

WAIC1 <- WAIC(diss.res.t11)
WAIC2 <- WAIC(diss.res.t11.pr1)

LOO1
LOO2
WAIC1
WAIC2

loo_compare(x=list(LOO1, LOO2))
loo_compare(x=list(WAIC1, WAIC2))

# refit models for LOO
loo1 <- brms:::loo.brmsfit(diss.res.t11, reloo=TRUE)
loo2 <- brms:::loo.brmsfit(diss.res.t11.pr1, reloo=TRUE)
loo1
loo2
# compare
loo_compare(x=list(loo1, loo2))

# plot LOO
plot(loo1)
plot(loo2)

# information criteria, R2 corrected for LOO
brms::loo_R2(diss.res.t11)
brms::loo_R2(diss.res.t11.pr1)

# plot posteriors
plot(diss.res.t11)
plot(diss.res.t11.pr1)

# plot marginal effects
conditional_effects(diss.res.t11)
conditional_effects(diss.res.t11.pr1)


