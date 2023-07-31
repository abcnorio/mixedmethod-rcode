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
# ptII_quan_Bayes_simulate-pi_helpfuncs.r

# location:
# chap. 6 [6.13]
# Marko Chain Monte Carlo Simulationen — MCMC


# HELPER FUNCTIONS

###### function to calculate pi
# original code from D. Eddelbüttel
piR <- function(N)
{
  x <- runif(N) # numbers between 0 and 1
  y <- runif(N)
  d <- sqrt(x^2 + y^2)
  # = area of the circle
  # area of the quadratic form
  # = x^2 + y^2 >= 1
  # area of the circle
  # = x^2 + y^2 <1
  
  # sum(area <1 )
  return(4 * sum(d < 1.0) / N)  
}
# call:
# piR(1e3)
########################## END OF FUNCTION

# inefficient, but works...
