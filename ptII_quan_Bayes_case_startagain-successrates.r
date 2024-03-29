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
# ptII_quan_Bayes_case_startagain-successrates.r

# location:
# chap. 6 [6.15.2]
# Durchlaufquoten in der Drogensuchttherapie

# load necessary helper functions
source("ptall_generalfuncs_Bayes_binomial-prop-test.r")


# UM Studer (1996) success rates / binomial model

# original values by UMS
si <- 23
Ni <- 27

steps <- 1000
theta <- seq(0,1,length.out=steps)

pbl.res <- pbl(theta=theta, si=si, Ni=Ni, loga=TRUE, reexp=TRUE)
pjc.res <- pjc(theta=theta, si=si, Ni=Ni, loga=TRUE, reexp=TRUE)
head(pbl.res)
tail(pbl.res)
head(pjc.res)
tail(pjc.res)
sN.ME.res <- data.frame(pbl.res, pjc.res)
head(sN.ME.res)
tail(sN.ME.res)
plot.bl.jc(theta, sN.ME.res=sN.ME.res, si=si, Ni=Ni, filling=FALSE)
plot.bl.jc(theta, sN.ME.res=sN.ME.res, si=si, Ni=Ni, filling=TRUE)
sN.ME.post.summary <- sN.post.su(Ni=Ni, si=si)

# single values
# theta.quer.BL +/- sigma.BL (studer nimmt 75%)
# BL
# calculate without logs
pbl(theta=0.5, si=si, Ni=Ni, loga=FALSE)
# calculate with logs and give out log value
exp( pbl(theta=0.5, si=si, Ni=Ni, loga=TRUE) )
# calculate with logs and give out re-exp non-log value
pbl(theta=0.5, si=si, Ni=Ni, loga=TRUE, reexp=TRUE)

# JC
# calculate without logs
pjc(theta=0.5, si=si, Ni=Ni, loga=FALSE)
# calculate with logs and give out log value
exp( pjc(theta=0.5, si=si, Ni=Ni, loga=TRUE) )
# calculate with logs and give out re-exp non-log value
pjc(theta=0.5, si=si, Ni=Ni, loga=TRUE, reexp=TRUE)


# no success / extreme case
si <- 0
Ni <- 6
pbl.res <- pbl(theta=theta, si=si, Ni=Ni, loga=TRUE, reexp=TRUE)
pjc.res <- pjc(theta=theta, si=si, Ni=Ni, loga=TRUE, reexp=TRUE)
plot.bl.jc(theta, sN.ME.res=data.frame(pbl.res, pjc.res), si=si, Ni=Ni, filling=TRUE)

# only success / extreme case
si <- 6
Ni <- 6
pbl.res <- pbl(theta=theta, si=si, Ni=Ni, loga=TRUE, reexp=TRUE)
pjc.res <- pjc(theta=theta, si=si, Ni=Ni, loga=TRUE, reexp=TRUE)
plot.bl.jc(theta, sN.ME.res=data.frame(pbl.res, pjc.res), si=si, Ni=Ni, filling=TRUE)

# one success / less extreme case
si <- 1
Ni <- 6
pbl.res <- pbl(theta=theta, si=si, Ni=Ni, loga=TRUE, reexp=TRUE)
pjc.res <- pjc(theta=theta, si=si, Ni=Ni, loga=TRUE, reexp=TRUE)
plot.bl.jc(theta, sN.ME.res=data.frame(pbl.res, pjc.res), si=si, Ni=Ni, filling=TRUE)

# almost everything a success / less extreme case
si <- 5
Ni <- 6
pbl.res <- pbl(theta=theta, si=si, Ni=Ni, loga=TRUE, reexp=TRUE)
pjc.res <- pjc(theta=theta, si=si, Ni=Ni, loga=TRUE, reexp=TRUE)
plot.bl.jc(theta, sN.ME.res=data.frame(pbl.res, pjc.res), si=si, Ni=Ni, filling=TRUE)

# not everything a success / less extreme case
si <- 4
Ni <- 6
pbl.res <- pbl(theta=theta, si=si, Ni=Ni, loga=TRUE, reexp=TRUE)
pjc.res <- pjc(theta=theta, si=si, Ni=Ni, loga=TRUE, reexp=TRUE)
plot.bl.jc(theta, sN.ME.res=data.frame(pbl.res, pjc.res), si=si, Ni=Ni, filling=TRUE)


