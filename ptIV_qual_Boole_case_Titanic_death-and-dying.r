# file:
# ptIV_qual_Boole_case_Titanic_death-and-dying.r

# location:
# chap. 12 [12.11.2]
# Leben und Sterben auf der Titanic Teil II

# load necessary libraries
library(QCA)
library(venn)

# load necessary helper functions
source("ptall_generalfuncs.r")


# read data
ttab.red.ne.all <- read.table("Titanic_data_TF-all.tab", sep="\t", header=TRUE)
head(ttab.red.ne.all)
tail(ttab.red.ne.all)
dim(ttab.red.ne.all)
t(apply(ttab.red.ne.all,2,table))


# not run
# using more predictors and it will be intractable (probably)
# at least one more and the Venn diagram does not work anymore
# alternative
colsofinterest <- c(
  "crew.c.L",
  "firstclass.1st.L",
  "lowerclass.l.L",
  "travelalone.a.L",
  "sex.f.L",
  "survived.s.L",
  "notjoinedinS.nS.L",
  "possible.mother.pm.L",
  "possible.child.c.L"
  #"travel.hugegroup.4min.L"
)
ttab.red.ne <- ttab.red.ne.all[,colsofinterest]
# end of not run


#colsofinterest <- c(
#  "crew.c.L",
#  "firstclass.1st.L",
#  #"lowerclass.l.L",
#  "travelalone.a.L",
#  "sex.f.L",
#  "survived.s.L",
#  "notjoinedinS.nS.L",
#  "possible.mother.pm.L",
#  "possible.child.c.L"
#  #"travel.hugegroup.4min.L"
#)
#ttab.red.ne <- ttab.red.ne.all[,colsofinterest]
head(ttab.red.ne)
tail(ttab.red.ne)
dim(ttab.red.ne)
t(apply(ttab.red.ne,2,table))


# define parameters
# superSubset 
incl.cut <- 1
cov.cut <- 0

# truthTable
n.cut <- 1
incl.cut1 <- 1
incl.cut0 <- 0
incl.cut <- c(1,0)
complete <- TRUE
show.cases <- TRUE
sort.by <- "incl"

# minimize
explain <- 1
include <- ""
all.sol <- TRUE
rowdom <- FALSE
details <- TRUE


# positive case
outcome <- "survived.s.L"
neg.out <- FALSE

# superSubset
ttab.susu <- superSubset(ttab.red.ne, outcome=outcome, incl.cut=incl.cut, cov.cut=cov.cut)
ttab.susu
             
# truth table
# rows = unique configurations !!NOT cases
t.TT <- truthTable(data=ttab.red.ne, outcome=outcome, n.cut=n.cut, incl.cut1=incl.cut1, incl.cut0=incl.cut0,
                   complete=complete, show.cases=show.cases, sort.by="incl")
t.TT

# logic minimization
# do not use eqmcc anymore, call minimize with same parameters
# short call
t.mini <- minimize(input=ttab.red.ne, outcome=outcome, n.cut=n.cut, incl.cut1=incl.cut1, incl.cut0=incl.cut0, explain=explain,
                       include=include, all.sol=all.sol, rowdom=rowdom, details=details, show.cases=show.cases)
t.mini
print.pis(t.mini, caseandpims=FALSE)

# extract primary implicants
t.PIs <- paste(attr(t.mini$PIchart,"dimnames")[[1]],collapse=" + ") 
t.PIs
strsplit(t.PIs," + ", fixed=TRUE)

# extract essential implicants
t.EIs <- paste(t.mini$essential, collapse=" + ")
t.EIs
strsplit(t.EIs," + ", fixed=TRUE)

# newer version
# library(venn)
venn(t.mini)
venn(t.mini, ilabels=TRUE, zcolor="style")


# negative case
outcome <- "survived.s.L"
neg.out <- TRUE

# NO SOLUTION WITHOUT VARIABLE CHILD IN THIS SCENARIO
# try out, remove from ttab.red.ne the 'child.c.L' variable
# but only if joined in Southampton is part of the table

# truth table
# rows = unique configurations !!NOT cases
t.TT.NEG <- truthTable(data=ttab.red.ne, outcome=outcome, n.cut=n.cut, incl.cut1=incl.cut1, incl.cut0=incl.cut0,
                       complete=complete, show.cases=show.cases, sort.by="incl", neg.out=neg.out)
str(t.TT.NEG)
head(t.TT.NEG$tt)

# logic minimization
# do not use eqmcc anymore, call minimize with same parameters
# short call

# minimize(t.TT.NEG)
t.mini.NEG <- minimize(input=t.TT.NEG, outcome=outcome, n.cut=n.cut, incl.cut1=incl.cut1, incl.cut0=incl.cut0, explain=explain,
                       include=include, all.sol=all.sol, rowdom=rowdom, details=details, show.cases=show.cases)
t.mini.NEG
print.pis(t.mini.NEG, caseandpims=FALSE)

# extract primary implicants
t.NEG.PIs <- paste(attr(t.mini.NEG$PIchart,"dimnames")[[1]],collapse=" + ") 
t.NEG.PIs
strsplit(t.NEG.PIs," + ", fixed=TRUE)

# extract essential implicants
t.NEG.EIs <- paste(t.mini.NEG$essential, collapse=" + ")
t.NEG.EIs
strsplit(t.NEG.EIs," + ", fixed=TRUE)

# newer version
venn(t.mini.NEG)
venn(t.mini.NEG, ilabels=TRUE, zcolor="style")

# check
t.pis <- attr(t.mini$PIchart,"dimnames")[[1]]
t.pis.NEG <- attr(t.mini.NEG$PIchart,"dimnames")[[1]]
t.pis %in% t.pis.NEG
t.pis.NEG %in% t.pis


# prepare for discussion
t.discuss <- primtab.desc(srctab=ttab.red.ne, primtab=t.mini, outcome="survived.s.L", norownames=FALSE)
t.discuss.NEG <- primtab.desc(srctab=ttab.red.ne, primtab=t.mini.NEG, outcome="~survived.s.L", norownames=FALSE)

t.discuss <- primtab.desc(srctab=ttab.red.ne, primtab=t.mini, outcome="survived.s.L", norownames=TRUE)
t.discuss.NEG <- primtab.desc(srctab=ttab.red.ne, primtab=t.mini.NEG, outcome="survived.s.L", norownames=TRUE)

t.discuss
t.discuss.NEG

print.pis(t.mini)
print.pis(t.mini.NEG)


# outcome and predictors in rows
t(aquadstyle.rev(t.discuss))
t(aquadstyle.rev(t.discuss)+0)

t(aquadstyle.rev(t.discuss.NEG))
t(aquadstyle.rev(t.discuss.NEG)+0)

t.full.tab <- cbind( t(aquadstyle.rev(t.discuss)+0), t(aquadstyle.rev(t.discuss.NEG)+0) )
t.full.tab
# write.table(t.full.tab,file="Titanic_TF_boolean-mini_POS+NEG.tab", sep="\t", row.names=TRUE, col.names=TRUE)

