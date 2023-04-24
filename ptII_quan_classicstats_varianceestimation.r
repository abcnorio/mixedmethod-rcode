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




