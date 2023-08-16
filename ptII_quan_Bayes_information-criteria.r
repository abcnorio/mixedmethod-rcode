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
# ptII_quan_Bayes_information-criteria.r

# location:
# chap. 6 [6.7.2]
# Informationskriterien

# load necessary libs
library(pscl)
library(lme4)
library(wiqid) # exIC

# load helper functions
source("ptII_quan_Bayes_information-criteria_helpfuncs.r")


# marginal likelihood
plik <- function(theta) {
  dbinom(x=40, size=150, prob=theta) * dbeta(x=theta, shape1=3, shape2=1)
}
margLik <- integrate(f=plik, lower=0, upper=1)$value
margLik


# Dobson (1990) Page 93: Randomized Controlled Trial
dobson.D93 <- data.frame(counts=c(18,17,15,20,10,20,25,13,12),
                         outcome=gl(3,1,9),
                         treatment=gl(3,3)
                        )
dobson.D93
glm.D93 <- glm(counts ~ outcome + treatment, family=poisson(), data=dobson.D93)

# lm
dobson.D9 <- data.frame(weight=c(ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14),
                                 trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)),
                        group=gl(2, 10, 20, labels = c("Ctl","Trt"))
                       )
dobson.D9
lm.D9 <- lm(weight ~ group, data=dobson.D9)

# example of huge penalty with AICcc ... n=9 observations, k=5 (= 2 factors with 2 + 3 categories)
glm.D93.exIC <- exIC(glm.D93)
# less penalty
lm.D9.exIC <- exIC(lm.D9)

# lmer
fm1 <- lmer(Reaction ~ Days + (Days | Subject), data=sleepstudy)
fm1.exIC <- exIC(fm1)

# glmer
gm1 <- glmer(cbind(incidence, size-incidence) ~ period + (1|herd), data=cbpp, family=binomial)
gm1.exIC <- exIC(gm1)


# DO NOT RUN THIS CODE BELOW
# it is an excerpt from exIC()
cat(paste(format(names(res), width = 17L, justify = "right"),  #15L
          format(res, digits = digits, nsmall=2), sep = " = "), sep = "\n")
if(!is.null(NOTE)) cat("\n", "NOTE: ", NOTE, "\n\n", sep = "")

