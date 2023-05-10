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


