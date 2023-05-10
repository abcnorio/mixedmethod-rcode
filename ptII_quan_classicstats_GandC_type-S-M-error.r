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
# ptII_quan_classicstats_GandC_type-S-M-error.r

# location:
# chap. 4 [4.5.2.2]
# Richtung und Größe — zwei unterschätzte Fehlertypen


# based on R-Code and ideas from
# http://andrewgelman.com/2014/11/17/power-06-looks-like-get-used/

# Gelman und Tuerlinckx (2000)
# http://www.stat.columbia.edu/~gelman/research/published/francis8.pdf

# Gelman und Carlin (2014)
# http://www.stat.columbia.edu/~gelman/research/published/PPS551642_REV2.pdf

# Alexander Etz 2015-05-21 [blog]
# https://alexanderetz.com/2015/05/21/type-s-and-type-m-errors/

#load necessary libs
library(Hmisc)

# load helper functions
source("ptII_quan_classicstats_GandC_type-S-M-error_helpfuncs.r")


#examples
# true effect size of 0.1, standard error 3.28, alpha=0.05  
retrodesign(tes=.1, se=3.28)

#Gelman and Carlin (2014, p.646)
#example
#Psychological Science
#Durante, Arsena, and Griskevicius (2013) 
#true effect size of 2, standard error 8.1, alpha=0.05  
tes <- 2
#d difference = empirical mean
d <- 17
#empirical reported two-sided p-value = 0.35
zvalue <- qnorm(1-0.035/2)
zvalue
#standard error of difference
#d/s = zvalue
SE <- d/zvalue
SE
#values based on literature recherche (tes) and empirical p-value
typsm.res.ref <- retrodesign(tes=tes, se=SE, graph=TRUE)
typsm.res.ref
#empirical values based on author's empirical results
retrodesign(tes=17, se=SE)
#what a difference!!!


#retrodesign power plotting
D.range <- seq(0,50,0.1)
#call
typsm.res <- plot.power.retrodesign(typsm.res.ref=typsm.res.ref, D.range=D.range, tes=tes, se=SE)
#pure "blind" empirically: tes = zvalue

max(typsm.res[typsm.res[,"typeS"]>0.1,"power"])
max(typsm.res[typsm.res[,"exaggeration"]>2,"power"])


#plot curves
plot.typ.sm(typsm.res.ref=typsm.res.ref, range.dist=c(-30,30), mw=0, emp.mw=d, tes=tes, se=SE)

