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
# ptII_quan_Bayes_intro-BayesTheorem_medicaldiagnosis.r

# location:
# chap. 6 [6.2.3.4]
# [6.2.3 Fallbeispiel — Medizindiagnostik]
# Anwendung Bayes-Theorem

# load necessary helper functions
source("ptII_quan_Bayes_intro-BayesTheorem_medicaldiagnosis_helpfuncs.r")


# artificial medicine test

# data given

# persons tested
sampleN <- 15000
illness.pos <- 9
sensitivity <- 99/100
false.positive <- 3/100

# calculations
prevalence <- illness.pos/sampleN
healthy <- 1-prevalence
false.negative <- 1-sensitivity
specifity <- 1-false.positive

prevalence
healthy
false.negative
specifity

# 0.99*6e-04 / (0.99*6e-04 + (1-0.97)*(1-6e-04))
BayesTheorem(p.A=9/15000, p.BcondA=0.99, p.NOTBcondNOTA=0.97)
BayesTheorem(p.A=illness.pos/sampleN, p.BcondA=sensitivity, p.NOTBcondNOTA=1-false.positive)
BT.res1.medtest <- BayesTheorem(p.A=prevalence, p.BcondA=sensitivity, p.NOTBcondNOTA=specifity)
BT.res1.medtest

# lower the base rate by factor 10
fac <- 10
BayesTheorem(p.A=9/15000*fac, p.BcondA=0.99, p.NOTBcondNOTA=0.97)
BT.res2.medtest <- BayesTheorem(p.A=prevalence*fac, p.BcondA=sensitivity, p.NOTBcondNOTA=specifity)
BT.res2.medtest

diff.fac <- BT.res2.medtest["p(A|B)"]/BT.res1.medtest["p(A|B)"]
names(diff.fac) <- "fac.diff"
diff.fac
# =
.1661074/.019427
# 8.550337

0.99*6e-04 / (0.99*6e-04 + (1-0.97)*(1-6e-04))
#0.019427

# persons ill (=correct) if diagnosis = positive
0.019427*15000
# 291.405


# AIDS example
# ELISA test

# data given
population <- 81390400

AIDS.pos.abs <- 84700
prevalence <- AIDS.pos.abs/population
healthy <- 1-prevalence

prevalence
healthy

# ELISA test
sensitivity <- .999
specifity <- .995

# calculations
false.negative <- 1-sensitivity
false.positive <- 1-specifity

false.negative
false.positive

BT.res.aids <- BayesTheorem(p.A=prevalence, p.BcondA=sensitivity, p.NOTBcondNOTA=specifity)
BT.res.aids

# persons based on population
BT.res.aids["p(A|B)"] * population #actually sample required, not population, ie. sample of ELISA test
BT.res.aids["p(A|B)"] * prevalence


# cancer example


# data given
prevalence <- 6/1000
sensitivity <- .98
specifity <- .96

# calculations
healthy <- 1-prevalence
false.positive <- 1-sensitivity
false.negative <- 1-specifity

healthy
false.negative
false.positive

BT.res.cancer <- BayesTheorem(p.A=prevalence, p.BcondA=sensitivity, p.NOTBcondNOTA=specifity)
BT.res.cancer

BT.res.cancer["p(A|B)"] * prevalence
1-BT.res.cancer["p(A|B)"]

# in case of positive test, what is more probable?
# cancer is real
cancer.real <- sensitivity * prevalence

# no cancer, cancer is not real
false.negative * (1-prevalence)
# =
cancer.notreal <- false.negative * healthy
# = MAP hypothesis = maximum a posteriori

cancer.real
cancer.notreal

# compare both terms
cancer.real > cancer.notreal
cancer.real / cancer.notreal


# normalising
cancer.real / (cancer.real+cancer.notreal)
cancer.notreal / (cancer.real+cancer.notreal)
# =
1-cancer.real / (cancer.real+cancer.notreal)
# compare with results of Bayes-Theorem
BT.res.cancer["p(A|B)"] == cancer.real / (cancer.real+cancer.notreal)

