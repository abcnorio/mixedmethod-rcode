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
# ptII_quan_classicstats_varianceestimation.r

# location:
# chap. 4 [4.6.5]
# (Selbst-)Täuschungen

# load helper functions
source("ptII_quan_classicstats_varianceestimation_helpfuncs.r")


# if sigma is known
varest(x=rnorm(n=50, mean=100, sd=10), sigma=10, LOG=FALSE)

# if sigma is unknown
varest(x=rnorm(n=50, mean=100, sd=10), sigma=NA, LOG=FALSE)

#dev.off()
plot.varest(plotwhat="s2")
#dev.off()
plot.varest(plotwhat="s")




