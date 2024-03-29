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
# ptII_quan_Bayes_Entropy_KullbackLeibler.r

# location:
# chap. 6 [6.14.1]
# „I, we, and nation“ — präsidiale Eigenwerbung

# load necessary libraries
library(entropy)

# load helper functions
source("ptII_quan_Bayes_Entropy_KullbackLeibler_helpfuncs.r")


# presidential debates and entropy
pres <- t(matrix(c(16,101,91, 32,131,88), nrow=2,
                 byrow=TRUE,dimnames=list(c("Bush","Kerry"),c("nation","I","we"))))
pres

counts.bk <- c(16,101,91, 32,131,88)
president <- gl(2,3, labels=c("Bush","Kerry"))
term <- gl(3,1,6, labels=c("nation","I","we"))
pres.dat <- data.frame(counts.bk,president,term)


# table used terms for all
bush <- pres[,"Bush"]
kerry <- pres[,"Kerry"]
counts.all <- apply(pres,1,sum)

pres.dat
counts.all


# Shannon entropy


Hc.all <- H.counts(counts=counts.all)#pres.dat[,"counts.bk"])
Hc.bush <- H.counts(counts=bush)
Hc.kerry <- H.counts(counts=kerry)

# combined
ratio <- apply(pres,2,sum)/sum(pres)

Hc.comb <- c(sum(ratio * c(Hc.bush["Hc"], Hc.kerry["Hc"])),
             sum(ratio * c(Hc.bush["Hc.Bayes"], Hc.kerry["Hc.Bayes"]))
) 
names(Hc.comb) <- c("Hc.comb","Hc.comb.Bayes")

# information gain by distinction (partition)
# of Bush vs. Kerry
info.gain <- Hc.all - Hc.comb

ratio
info.gain
Hc.comb

# empirical Kullback-Leibler divergence
KL.counts(bush,kerry)

# comparison
Hc.all
Hc.bush
Hc.kerry

# via package
# library(entropy)
apply(pres,2,entropy)
entropy(counts.all)

