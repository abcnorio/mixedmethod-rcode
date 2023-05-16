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
# ptIV_qual_Boole_logical-minimization.r

# location:
# chap. 12.3 []
# Typenbildung als Prinzip des Vergleichs mittels logischer Minimierung

# load necessary libraries
library(combinat)


# combinations comparisons Boolean minimization
noquote(t(combn(paste("k",1:4,sep=""),2)))

# minimal example logic minimization
k1 <- c(T,F,T)
k4 <- c(T,T,T)
names(k1) <- names(k4) <- c("U","B","K")

k1 & k4
red.l <- length(id <- which(!(k1 & k4)))
id
red.l

if(red.l == 1) cat(paste("reduction, because there is only one difference at:\t",names(k1[id]),"\n",sep=""))
k1[id] <- NA
str(k1)
k1

