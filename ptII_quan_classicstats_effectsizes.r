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
# ptII_quan_classicstats_effectsizes.r

# location:
# chap. 4 [4.6.10]
# Effektstärken — Größe, Häufigkeit und Bezug zur Originalskala

# load necessary libraries
library(arm)
library(effsize)
library(effectsize)
library(dabestr)
library(pwr)
library(sjstats)
library(displayHTS)

# load necessary helper functions
source("ptall_generalfuncs.r")
source("ptII_quan_classicstats_effectsizes_helpfuncs.r")


# call function to relate N, pv and Cohen's d
# without the same seed
res.notsameseed <- pv.vs.cd()
# with the same seed
res.sameseed <- pv.vs.cd(usesameseed=TRUE)
#pv.vs.cd(usesameseed=TRUE, log.pv=FALSE)
str(res.notsameseed)
str(res.sameseed)


describes(res.notsameseed$pv)
describes(res.notsameseed$cd)

arm:::display(res.notsameseed$lm.pv)
arm:::display(res.notsameseed$lm.cd)


# different effect sizes, different n, approx. same p-values
seed <- 9876
set.seed(seed)

n1 <- 80
sigma1 <- 1.5
s1 <- rnorm(n=n1, mean=10, sd=sigma1)
s2 <- rnorm(n=n1, mean=10.4956458, sd=sigma1)
t1 <- t.test(s1,s2)
ES.v <- (mean(s2)-mean(s1))/sigma1
ES.v
cohensd(s1, s2, sd.theory=sigma1)

n2 <- 1000
sigma2 <- 10
w1 <- rnorm(n=n2, mean=100, sd=sigma2)
w2 <- rnorm(n=n2, mean=100.57567, sd=sigma2)
t2 <- t.test(w1,w2)
ES.w <- (mean(w2)-mean(w1))/sigma2
ES.w
cohensd(w1, w2, sd.theory=sigma2)

# compare values
t1
t2
all.equal(t1$pv, t2$pv)
ES.v
ES.w


# various packages

# library 'effsize'
cliff.delta(w1,w2)
VD.A(w1,w2)
effsize:::cohen.d(w1,w2)
effsize:::cohen.d(w1,w2,hedges.correction=TRUE)

# library 'effectsize'
# lm and effect size
lm.w2.w1 <- lm(w2~w1)
arm:::display(lm.w2.w1)
eta_squared(lm.w2.w1)
eta_squared(lm.w2.w1, partial=FALSE)
eta_squared(lm.w2.w1, partial=FALSE)
epsilon_squared(lm.w2.w1)
omega_squared(lm.w2.w1)
effectsize:::cohens_f(lm.w2.w1)

# transform from one size to the other
# just an example
lm.w2.w1.anova <- anova(lm.w2.w1)
# F value to eta^2
F_to_eta2(lm.w2.w1.anova$`F value`[1], lm.w2.w1.anova$Df[1], lm.w2.w1.anova$Df[1])
# Cohens' d to r
d_to_r(cohensd(w1,w2)[2])
#d_to_r <- function (d, ...) 
#{
#  d/(sqrt(d^2 + 4))
#}

# library 'pwr'
# effect size for proportions
ES.h(0.2,0.3)

# effect size for chi^2 GOF
prob.0 <-rep(1/3,4)
prob.1 <- c(0.4,rep((1-0.4)/3,3))
prob.0
prob.1
ES.w1(prob.0,prob.1)

# effect size for chi^2 association
fisher.tea <- matrix(c(4,0,0,4), nrow=2, dimnames=list(guess=c("milk","tea"),truth=c("milk","tea")))
fisher.tea
ES.w2(prop.table(fisher.tea))
fisher.wrong.tea <- matrix(c(3,1,1,3), nrow=2, dimnames=list(guess=c("milk","tea"),truth=c("milk","tea")))
fisher.wrong.tea
ES.w2(prop.table(fisher.wrong.tea))


# library 'sjstats'
# odds ratio and risk ratio
#
# Data on whether or not households in Bangladesh changed the wells that they were using.
library(carData)
?Wells
data(Wells)
head(Wells)
tail(Wells)
summary(Wells)
wells.glm <- glm(switch~arsenic+distance+education+association, data=Wells, family=binomial(link="logit"))
# summary(wells.glm)
arm:::display(wells.glm)
# RR from glm
wells.glm.OR.RR <- odds_to_rr.v2(wells.glm)

# output
wells.glm.OR.RR

# prop table
wells.tab <- with(Wells, table(switch,association))
wells.tab.p <- prop.table(wells.tab)
wells.tab
wells.tab.p

# identical
wells.or <- wells.glm.OR.RR$OR[5,"OR"]
wells.p0 <- well.glm.RR$P0
# from odds_to_rr.v2()
rr1 <- wells.glm.OR.RR$RR["associationyes","RR"]
# from or_to_rr()
rr2 <- or_to_rr( or=wells.or, p0=sum(wells.tab.p[2,]) )
# from or_to_rr()
rr3 <- or_to_rr(or=wells.or, p0=wells.p0)

# from or_to_rr()
rr4 <- wells.or/(1 - p0 + (p0 * wells.or))

# the same?
well.glm.RR$P0 == sum(wells.tab.p[2,])
all.equal(rr1,rr2,rr3,rr4)

