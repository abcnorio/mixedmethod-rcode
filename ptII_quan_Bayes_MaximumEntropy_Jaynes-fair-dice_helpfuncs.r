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
# ptII_quan_Bayes_MaximumEntropy_Jaynes-fair-dice_helpfuncs.r

# location:
# chap. 6 [6.14.3]
# Der Klassiker — ist ein Würfel fair?

# HELPER FUNCTIONS


###### function to calculate Jaynes' dice problem
# requires optimizations...

# eq 15
z <- function(x) 1/sum(exp(-k * x))

# eq 16
foo <- function(x, B, k) return( abs( sum(k*exp(-k * x)) * ( 1/sum(exp(-k * x)) ) - B ) )
foo2 <- function(x, B, k) return( abs( sum(k*exp(-k * x)) * ( z(x) ) - B ) )
foo3 <- function(x, B, k) return( sum(k*exp(-k * x)) * ( z(x) ) - B )

# eq 17
probfun <- function(x)  1/z(x) * exp(-k * x)
########################## END OF FUNCTION


###### function to perform Jaynes' dice problem for various B's
Jaynes.dice <- function(B=4.5, k=1:6, startv=0.5, method="BFGS")
{
  foo <- function(x, B, k) return( abs( sum(k*exp(-k * x)) * ( 1/sum(exp(-k * x)) ) - B ) )
  lambda.opt <- optim(startv, foo, method=method, B=B, k=k)
  ps <- probfun(x=lambda.opt$par)
  return(list(ps=ps, probs=ps/sum(ps)))
}
# call:
# example from Jaynes
# Jaynes.dice()$probs
# Jaynes.dice(B=4.5, k=1:6)$probs
########################## END OF FUNCTION


###### function from McElreath  p.277 simulation of entropy
sim.p <- function(G=1.4, seed=38383)
{
  if(!is.null(seed)) set.seed(seed)
  x123 <- runif(3)
  x4 <- ( G*sum(x123) - x123[2] - x123[3] ) / (2-G)
  z <- sum(c(x123,x4))
  p <- c(x123, x4)/ z
  return(list(H=-sum(p*log(p)), p=p, x=c(x123,x4))) 
}
# call:
# sim.p()
# sum(sim.p()$x)
# replicate(10, sim.p(seed=NULL)$p)
########################## END OF FUNCTION

