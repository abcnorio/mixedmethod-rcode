# file:
# ptIV_qual_Boole_case_school-success.r

# location:
# chap. 12 [12.6]
# Kriteriumsanalyse — positiver und negativer Ausgang

# load necessary libs
library(QCA)
library(venn)

# load helper functions
source("ptall_generalfuncs.r")


# artificial school success example
# library(QCA)
SE <- read.table("school-success.tab", header=TRUE, sep="\t")
SE
SE.ne <- SE[,c("E","U","B","K")]

# outcome
outcome <- "E"

# positive case
neg.out <- FALSE
# superSubset
SE.susu <- superSubset(SE.ne, outcome=outcome)
print(SE.susu)
# truth table
SE.TT <- truthTable(data=SE.ne, outcome=outcome, neg.out=neg.out, complete=TRUE, show.cases=TRUE, sort.by="incl")
print(SE.TT)
# logic minimization
SE.mini <- minimize(input=SE.TT, outcome=outcome, neg.out=neg.out, details=TRUE, show.cases=TRUE)
print(SE.mini)
print.pis(SE.mini)
# extract primary implicants
paste(attr(SE.mini$PIchart,"dimnames")[[1]],collapse=" + ") 
# extract essential implicants
paste(SE.mini$essential, collapse=" + ")

# Venn-diagram
# library(venn)
venn(SE.mini, ilabels=TRUE, col="blue", zcolor="pink, yellow, green", ellipse=TRUE, borders=FALSE, box=FALSE)


# negative case
neg.out <- TRUE
# superSubset
SE.susu.NEG <- superSubset(SE.ne, outcome=outcome)
print(SE.susu.NEG)
# truth table
SE.TT.NEG <- truthTable(data=SE.ne, outcome=outcome, neg.out=neg.out, complete=TRUE, show.cases=TRUE, sort.by="incl")
print(SE.TT.NEG)
# logic minimization
SE.mini.NEG <- minimize(input=SE.TT.NEG, outcome=outcome, neg.out=neg.out, details=TRUE, show.cases=TRUE)
print(SE.mini.NEG)
print.pis(SE.mini.NEG)
# extract primary implicants
paste(attr(SE.mini.NEG$PIchart,"dimnames")[[1]],collapse=" + ") 
# extract essential implicants
paste(SE.mini.NEG$essential, collapse=" + ")

# Venn-diagram
venn(SE.mini.NEG, ilabels=TRUE, col="blue", zcolor="darkred, yellow, green", ellipse=TRUE, borders=FALSE, box=FALSE)

# simplifying assumptions
str(SE.mini)
SE.mini$SA

str(SE.mini.NEG)
SE.mini.NEG$SA


# example factorize
# library(QCA)
# library(admisc)
?factorize
factorize("ac + aD + bc + bD", pos=FALSE)
factorize("ac + aD + bc + bD", pos=TRUE)


