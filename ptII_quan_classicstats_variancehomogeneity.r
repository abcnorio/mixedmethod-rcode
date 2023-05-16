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
# ptII_quan_classicstats_variancehomogeneity.r

# location:
# chap. 4 [4.6.8.2]
# Varianzhomogenität (Homoskedastizität)

# load necessary libs
library(car)
library(lmtest)

# same data
# example from '?lm' (Annette Dobson)
ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
group <- gl(2, 10, 20, labels = c("Ctl","Trt"))
weight <- c(ctl, trt)
lm.fit <- lm(weight ~ group)


# variance homogeneity/ Heteroscedasticity
# levene test
car::leveneTest(weight, group)
# breusch-pagan test
lmtest::bptest(lm.fit)


# non-constant error variance
spreadLevelPlot(lm.fit)
# non-constant variance score test
# also breusch-pagan test, extensions by cook and weisberg
ncvTest(lm.fit)
# identical to
# studentize	= logical. If set to TRUE Koenker's studentized version of the test statistic will be used.
lmtest::bptest(lm.fit, studentize=FALSE)

