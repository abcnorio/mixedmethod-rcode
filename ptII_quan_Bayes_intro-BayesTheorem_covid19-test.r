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
# ptII_quan_Bayes_intro-BayesTheorem_covid19-test.r

# location:
# chap. 6 [6.2.4]
# Fallbeispiel — Zur Zuverlässigkeit eines COVID-19 Tests

# load necessary helper functions
source("ptII_quan_Bayes_intro-BayesTheorem_medicaldiagnosis_helpfuncs.r")


# function to calculate posterior prob
posterior.test <- function(sensi, speci, p) sensi*p / (sensi*p + (1-speci)*(1-p))

# data

# characteristics of the test
# specifity
speci1 <- 0.996
# sensitivity
sensi1 <- 0.944

# overall probability in population
p1 <- 0.0025
post1 <- posterior.test(sensi=sensi1, speci=speci1, p=p1)
post1
# =
0.944*.0025/(.944*.0025+(1-.996)*(1-.0025))
# 0.3716535
# =
BayesTheorem(p.A=.0025, p.BcondA=0.944, p.NOTBcondNOTA=0.996)

sek <- seq(0,1, length.out=100+1)
probs <- posterior.test(sensi=sensi1, speci=speci1, p=sek)
probs

plot(sek*100,probs*100, bty="n", col="darkred", type="l", pre.plot=grid(), main="COVID-19 in case of positive test result", xlab="Distribution in population (%)", ylab="COVID-19 p(%)")
#plot(sek*100,log(probs*100), bty="n", col="darkred", type="l", pre.plot=grid(), main="COVID-19 in vase of positive test result", xlab="Distribution in population (%)", ylab="COVID-19 p(%)")

# percent population required to acquire a certainty of x% in the test
perc.sec <- c(0.5, 0.9, 0.99)
N.total <- 83200000
for(i in perc.sec)
{
  perc.prop <- sek[probs >= i][1]*100
  cat("\n% security (COVID-19 test) = ",i,"\n% in population required (base rate): = ",perc.prop,"\n",sep="")
  cat("people required = ",perc.prop/100*N.total,"\n",sep="")
  abline(v=perc.prop, col="blue", lty=2)
  points(perc.prop, i*100, pch=23, bg="yellow", col="blue", cex=2)
  text(perc.prop+1, i*100, pos=4, labels=paste(perc.prop,"%",sep=""), cex=2)
}  


# replicate with initial post1 value
post2 <- posterior.test(sensi=sensi1, speci=speci1, p=post1)
post2
# 0.9928871
# =
posterior.test2 <- function(sensi2, speci2, p2) sensi2^2*p2 / (sensi2^2*p2 + (1-speci2)^2*(1-p2))
# =
posterior.test2(sensi2=sensi1, speci2=speci1, p2=p1)
# =
BayesTheorem(p.A=post1, p.BcondA=0.944, p.NOTBcondNOTA=0.996)


