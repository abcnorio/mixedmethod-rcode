# file:
# ptIV_qual_Boole_case_Krook_women-in-parliament.r

# location:
# chap. 12 [12.11.1]
# Die Repräsentativität von Frauen in Parlamenten

# load necessary libraries
library(QCA)
library(venn)

# load helper functions
source("ptall_generalfuncs.r")


# read source
krook.raw <- read.table("Krook_raw_QCA.tab", sep="\t", header=TRUE, check.names=FALSE)
krook.ne <- read.table("Krook_NE_QCA.tab", sep="\t", header=TRUE, check.names=FALSE)

krook.raw
krook.ne

# conversions for readability
aquadstyle.tt(krook.ne)
aquadstyle.rev(aquadstyle.tt(krook.ne))
aquadstyle.tt(krook.ne, pres.cnam=FALSE)
aquadstyle.rev(aquadstyle.tt(krook.ne))+0


# QCA

# define parameters
# superSubset 
outcome <- "WomenP"
incl.cut <- 1
cov.cut <- 0

# truthTable
n.cut <- 1
incl.cut1 <- 1
incl.cut0 <- 1
complete <- TRUE
show.cases <- TRUE
sort.by <- "incl"

# minimize
explain <- 1
include <- ""
all.sol <- TRUE
rowdom <- FALSE
details <- TRUE
ttab.ne <- krook.ne

# positive case
neg.out <- FALSE

# superSubset
krook.susu <- superSubset(ttab.ne, outcome=outcome, incl.cut=incl.cut, cov.cut=cov.cut)
krook.susu
             
# truth table
# rows = unique configurations !!NOT cases
krook.TT <- truthTable(data=ttab.ne, outcome=outcome, neg.out=neg.out, n.cut=n.cut, incl.cut1=incl.cut1, incl.cut0=incl.cut0,
                       complete=complete, show.cases=show.cases, sort.by="incl")
krook.TT

# logic minimization
# do not use eqmcc anymore, call minimize with same parameters
# short call
minimize(krook.TT)
krook.mini <- minimize(input=krook.TT, outcome=outcome, neg.out=neg.out, n.cut=n.cut, incl.cut1=incl.cut1, incl.cut0=incl.cut0, explain=explain,
                       include=include, all.sol=all.sol, rowdom=rowdom, details=details, show.cases=show.cases)
krook.mini
print.pis(krook.mini)

# extract primary implicants
paste(attr(krook.mini$PIchart,"dimnames")[[1]],collapse=" + ") 

# extract essential implicants
paste(krook.mini$essential, collapse=" + ")

# plot
venn(krook.mini)
venn(krook.mini, ilabels=TRUE, col="blue", zcolor="steelblue, yellow, green", ellipse=TRUE, borders=FALSE, box=FALSE)


# all possible Venn diagrams
par(ask=TRUE)
for(i in 1:7) venn(i, ilabels=TRUE, col="blue", zcolor="darkred, yellow, green", ellipse=TRUE, borders=FALSE, box=FALSE)


# negative case
neg.out <- TRUE

# superSubset
krook.susu.NEG <- superSubset(ttab.ne, outcome=outcome, incl.cut=incl.cut, cov.cut=cov.cut)
krook.susu.NEG
             
# truth table
# rows = unique configurations !!NOT cases
krook.TT.NEG <- truthTable(data=ttab.ne, outcome=outcome, neg.out=neg.out, n.cut=n.cut, incl.cut1=incl.cut1, incl.cut0=incl.cut0,
                       complete=complete, show.cases=show.cases, sort.by="incl")
krook.TT.NEG

# logic minimization
# do not use eqmcc anymore, call minimize with same parameters
# short call
minimize(krook.TT.NEG)
krook.mini.NEG <- minimize(input=krook.TT.NEG, outcome=outcome, neg.out=neg.out, n.cut=n.cut, incl.cut1=incl.cut1, incl.cut0=incl.cut0, explain=explain,
                       include=include, all.sol=all.sol, rowdom=rowdom, details=details, show.cases=show.cases)
krook.mini.NEG
print.pis(krook.mini.NEG)

# extract primary implicants
paste(attr(krook.mini.NEG$PIchart,"dimnames")[[1]],collapse=" + ") 

# extract essential implicants
paste(krook.mini.NEG$essential, collapse=" + ")

# plot
venn(krook.mini.NEG)
venn(krook.mini.NEG, , ilabels=TRUE, col="blue", zcolor="magenta, green, yellow", ellipse=TRUE, borders=FALSE, box=FALSE)

# check for consistency
krook.pis <- attr(krook.mini$PIchart,"dimnames")[[1]]
krook.pis
krook.pis.NEG <- attr(krook.mini.NEG$PIchart,"dimnames")[[1]]
krook.pis.NEG
krook.pis %in% krook.pis.NEG
krook.pis.NEG %in% krook.pis

# prepare positive and negative case for discussion
krook.discuss <- primtab.desc(srctab=krook.ne, primtab=krook.mini, outcome=outcome, norownames=FALSE)
krook.discuss.NEG <- primtab.desc(srctab=krook.ne, primtab=krook.mini.NEG, outcome=outcome, norownames=FALSE)

# convert between different way to show a truth table
aquadstyle.rev(krook.discuss)
aquadstyle.rev(krook.discuss)+0
t(aquadstyle.rev(krook.discuss))
t(aquadstyle.rev(krook.discuss)+0)

t(aquadstyle.rev(krook.discuss.NEG))
t(aquadstyle.rev(krook.discuss.NEG)+0)


##################
# not run below this point

# Ragin original data
ragin <- matrix(data=c(
  "A","B","C","D",
  "A","b","C","D",
  "a","B","c","D",
  "A","B","c","D",
  "A","B","C","D",
  "A","b","c","d",
  "a","b","C","d",
  "a","B","C","d",
  "a","b","c","d"),
  ncol=4, byrow=1)
ragin



