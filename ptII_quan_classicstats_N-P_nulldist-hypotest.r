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
# ptII_quan_classicstats_N-P_nulldist-hypotest.r

# location:
# chap. 4 [4.5.2.5]
# Die Vollkostenrechnung

#load necessary libs
library(pwr)

# load helper functions
source("ptII_quan_classicstats_N-P_nulldist-hypotest_helpfuncs.r")


#normal case
#two sided
plot.H0(mu0=0, sigma0=1, alternative="two.sided")

#one sided, less 
plot.H0(mu0=2, sigma0=2, alternative="less")

#one sided, greater
plot.H0(mu0=3, sigma0=3, alternative="greater")
plot.H0(mu0=99, sigma0=6, alternative="greater")


#t case
#two sided
plot.H0(mu0=0, N=30, type="t", alternative="two.sided")

#one sided, less 
plot.H0(mu0=2, N=30, type="t", alternative="less")

#one sided, greater
plot.H0(mu0=3, N=30, type="t", alternative="greater")
plot.H0(mu0=90, N=9000, type="t", alternative="greater")

plot.H0(mu0=-2, N=4, type="t", alternative="greater")

plot.H0(mu0=90, N=4, type="t", alternative="greater")
plot.H0(mu0=90, N=4, type="t", alternative="less")
plot.H0(mu0=90, N=4, type="t", alternative="two.sided")



#different calls 
alpha.err <- 0.05
n1 <- 50
n2 <- 60
mu1 <- 0
sigma1 <- 1
mu2 <- 2
sigma2 <- 1

#t
type <- "t"

delta <- NA
beta.err <- NULL

alternative <- "two.sided"
plot.ab.err(n1=n1, n2=n2, mu1=mu1, sigma1=sigma1, sigma2=sigma2, delta=delta, alpha.err=alpha.err, beta.err=beta.err, type=type, alternative=alternative)

alternative <- "greater"
plot.ab.err(n1=n1, n2=n2, mu1=mu1, sigma1=sigma1, sigma2=sigma2, delta=delta, alpha.err=alpha.err, beta.err=beta.err, type=type, alternative=alternative)

alternative <- "less"
plot.ab.err(n1=n1, n2=n2, mu1=mu1, sigma1=sigma1, sigma2=sigma2, delta=delta, alpha.err=alpha.err, beta.err=beta.err, type=type, alternative=alternative)


#beta given, no mu1/ mu2/ delta
#delta = ?
beta.err <- 0.2
mu1 <- mu2 <- NA
delta <- NULL

alternative <- "two.sided"
plot.ab.err(n1=n1, n2=n2, mu1=mu1, sigma1=sigma1, sigma2=sigma2, delta=delta, alpha.err=alpha.err, beta.err=beta.err, type=type, alternative=alternative)

alternative <- "greater"
plot.ab.err(n1=n1, n2=n2, mu1=mu1, sigma1=sigma1, sigma2=sigma2, delta=delta, alpha.err=alpha.err, beta.err=beta.err, type=type, alternative=alternative)

alternative <- "less"
plot.ab.err(n1=n1, n2=n2, mu1=mu1, sigma1=sigma1, sigma2=sigma2, delta=delta, alpha.err=alpha.err, beta.err=beta.err, type=type, alternative=alternative)


#
beta.err <- 0.001

alternative <- "two.sided"
plot.ab.err(n1=n1, n2=n2, mu1=mu1, sigma1=sigma1, sigma2=sigma2, delta=delta, alpha.err=alpha.err, beta.err=beta.err, type=type, alternative=alternative)

alternative <- "greater"
plot.ab.err(n1=n1, n2=n2, mu1=mu1, sigma1=sigma1, sigma2=sigma2, delta=delta, alpha.err=alpha.err, beta.err=beta.err, type=type, alternative=alternative)

alternative <- "less"
plot.ab.err(n1=n1, n2=n2, mu1=mu1, sigma1=sigma1, sigma2=sigma2, delta=delta, alpha.err=alpha.err, beta.err=beta.err, type=type, alternative=alternative)


#
alpha.err <- 0.3

alternative <- "two.sided"
plot.ab.err(n1=n1, n2=n2, mu1=mu1, sigma1=sigma1, sigma2=sigma2, delta=delta, alpha.err=alpha.err, beta.err=beta.err, type=type, alternative=alternative)

alternative <- "greater"
plot.ab.err(n1=n1, n2=n2, mu1=mu1, sigma1=sigma1, sigma2=sigma2, delta=delta, alpha.err=alpha.err, beta.err=beta.err, type=type, alternative=alternative)

alternative <- "less"
plot.ab.err(n1=n1, n2=n2, mu1=mu1, sigma1=sigma1, sigma2=sigma2, delta=delta, alpha.err=alpha.err, beta.err=beta.err, type=type, alternative=alternative)


# created error ("strict" mode in power.t.test())
beta.err <- 0.7

alternative <- "two.sided"
plot.ab.err(n1=n1, n2=n2, mu1=mu1, sigma1=sigma1, sigma2=sigma2, delta=delta, alpha.err=alpha.err, beta.err=beta.err, type=type, alternative=alternative)

alternative <- "greater"
plot.ab.err(n1=n1, n2=n2, mu1=mu1, sigma1=sigma1, sigma2=sigma2, delta=delta, alpha.err=alpha.err, beta.err=beta.err, type=type, alternative=alternative)

alternative <- "less"
plot.ab.err(n1=n1, n2=n2, mu1=mu1, sigma1=sigma1, sigma2=sigma2, delta=delta, alpha.err=alpha.err, beta.err=beta.err, type=type, alternative=alternative)



