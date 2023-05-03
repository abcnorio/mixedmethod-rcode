# file:
# ptII_quan_simulate-pi_helpfuncs.r

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

