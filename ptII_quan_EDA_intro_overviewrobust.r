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
# ptII_quan_EDA_intro_overviewrobust.r

# location:
# chap. 5 [5.2], [5.3]
# Typische Verfahren der EDA in R
# Robuste Datenanalysen als Teil der EDA

# load necessary libraries
library(faraway)
library(foreign)
library(data.table)
library(MASS)

# load necessary helper functions
source("ptall_generalfuncs.r")
source("ptII_quan_EDA_intro_overviewrobust_helpfuncs.r")


# lm standard plot
example(lm)


# median vs. mean
mu <- 6
sigma <- 2.34
N <- 1e4
seed <- 54321
reps <- 1e3
res.1 <- mwmed.sim(N=N, mu=mu, sigma=sigma, reps=reps, seed=seed, abso=FALSE, pr=TRUE)
res.2 <- mwmed.sim(N=N, mu=mu, sigma=sigma, reps=reps, abso=FALSE, pr=TRUE, usesameseed=FALSE)
res.3 <- mwmed.sim(N=N, mu=mu, sigma=sigma, reps=reps, abso=FALSE, pr=TRUE, usesameseed=TRUE)
str(res.1)
head(res.1[[1]])
tail(res.1[[1]])
apply(res.1[[1]],2,fivenum.wn)
apply(res.1[[1]],2,range)
apply(res.1[[1]],2,summary)
apply(res.1[[1]],2,function(x) c(sd(x),var(x)))
# plot all three different calls
mwmed.sim.plot(res.1,mu=mu)
mwmed.sim.plot(res.2,mu=mu)
mwmed.sim.plot(res.3,mu=mu)



# functions to plot psi functions


andrewswave <- function(sek, a=1.34*pi)
{
  sek.abs <- abs(sek)
  sek.mal.pi <- pi*sek
  a / (sek.mal.pi) * sin(sek.mal.pi/a) * ( sek.abs <= a )
} 
# graphical plot of different weighting functions
sek <- seq(-10,10, length.out=1e3)
digs <- 3
par(oma=c(2,2,2,2), mar=c(2,2,4,2), "cex.axis"=1, bty="l", mfrow=c(2,2))
# Huber k
plot(sek, MASS:::psi.huber(sek), type="l", col="darkred", bty="n", pre.plot=grid(), main="Huber k", ylab="weight")
# Hampel
plot(sek, MASS:::psi.hampel(sek), type="l", col="darkred", bty="n", pre.plot=grid(), main="Hampel", ylab="weight")
# Andrews Wave
a <- 1.34*pi
plot(sek, andrewswave(sek, a), type="l", col="darkred", bty="n", pre.plot=grid(), ylab="weight", main=expression(paste("Andrews Wave (a = 1.34 * ",pi,")",sep="")))
# Tukey's biweight
plot(sek, MASS:::psi.bisquare(sek, a), type="l", col="darkred", bty="n", pre.plot=grid(), main="Tukey's biweight", ylab="weight")
mtext(expression(paste("Various weighting functions",sep="")), outer=TRUE, line=-1, cex=1.5, side=3)


# lm vs. rlm

# Crime data
# explanations

# read from the net
# crimedata <- read.dta("https://stats.idre.ucla.edu/stat/data/crime.dta")
# write.table(crimedata, file="crimedata.tab", sep="\t", row.names=FALSE, col.names=TRUE)
crimedata <- read.csv(file="crimedata.tab", sep="\t", header=TRUE)
head(crimedata)
tail(crimedata)

crimedata.lm <- lm(crime ~ poverty + single, data = crimedata)
crimedata.rlm <- rlm(crime ~ poverty + single, data = crimedata, psi = psi.bisquare)

summary(crimedata.lm)
summary(crimedata.rlm)

# relative efficiency
summary(crimedata.lm)$sigma / summary(crimedata.rlm)$sigma
# residual plot
resid.plot(lmfit=crimedata.lm, rlmfit=crimedata.rlm)
# ratios of coefficients lm vs. rlm
coef(crimedata.lm)/coef(crimedata.rlm)
coef(crimedata.lm)/( coef(crimedata.lm) + coef(crimedata.rlm) )
# the higher the residual, the lower the weight
data.frame(crimedata.rlm$residuals, crimedata.rlm$w)
# how are weights distributed? vs. residuals
if(dev.cur() != 1) dev.off()
with(crimedata.rlm, plot(residuals, w, bty="n", pre.plot=grid(), col="darkred", ylab="Weights", xlab="Residuals"))
abline(h=1, v=0, col="blue", lty=2)
mtext("Distribution of weights vs. residuals", outer=TRUE, line=-3, cex=1.5)



# BUPA data liver disorders
# read from the net
# bupa <- fread("ftp://ftp.ics.uci.edu/pub/machine-learning-databases/liver-disorders/bupa.data")
# colnames(bupa) <- c("mcv","alkphos","sgpt","sgot","gammagt","drinks","selector")
# bupa$gammagt.log <- log(bupa$gammagt)
# bupa$sgpt.log <- log(bupa$sgpt)
# write.table(bupa, file="bupa.tab", sep="\t", row.names=FALSE, col.names=TRUE)
bupa <- read.csv(file="bupa.tab", sep="\t", header=TRUE)
head(bupa)
tail(bupa)

# OLS
bupa.lm <- lm( sgpt.log ~ gammagt.log, data=bupa)
# RLM
bupa.rlm <- rlm( sgpt.log ~ gammagt.log, data=bupa)
# output
summary(bupa.lm)
summary(bupa.rlm)

# scatterplot
with(bupa, plot( gammagt.log, sgpt.log, col="purple", bg="grey", xlab="log(gammagt)", ylab="log(sgpt)", pch=21, lwd=2, pre.plot=grid(), bty="n" ) )
abline(bupa.lm, col="green", lwd=2)
abline(bupa.rlm, col="blue", lwd=2, lty=2)
legend( "topleft", legend=c("OLS","RLM"), col=c("green","blue"), bty="n", lty=c(1,2), lwd=c(2,2) )
mtext("log(sgpt) vs. log(gammagt)", outer=TRUE, line=-3, cex=1.5)

# relative efficiency
# the smaller the better for the first one
# in case of one = same efficiency
summary(bupa.lm)$sigma / summary(bupa.rlm)$sigma
# residual plot to identify outliers
resid.plot(lmfit=bupa.lm, rlmfit=bupa.rlm)



# Chicago data
data(chicago)
head(chicago)
tail(chicago)

chic.lm <- lm(involact ~ race + fire + theft + age + log(income), data=chicago)
chic.rlm <- rlm(involact ~ race + fire + theft + age + log(income), data=chicago)
summary(chic.lm)
summary(chic.rlm)
# ratio lm vs. rlm
coef(chic.lm) / coef(chic.rlm)
# relative efficiency lm vs rlm
summary(chic.lm)$sigma / summary(chic.rlm)$sigma





