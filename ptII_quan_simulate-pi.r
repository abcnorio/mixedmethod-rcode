# file:
# ptII_quan_simulate-pi.r

# location:
# chap. 6 [6.13]
# Marko Chain Monte Carlo Simulationen — MCMC

# load necessary helper functions
source("ptII_quan_simulate-pi_helpfuncs.r")


seed <- 5
set.seed(seed)
for(i in 10^(1:7))
{
 cat("i = ",i,"\t",piR(i),"\n")
}

# piR(1e10)
# error -> uses too much RAM
# everything above piR(7) can block your computer...


