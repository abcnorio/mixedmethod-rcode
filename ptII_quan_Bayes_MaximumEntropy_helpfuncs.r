# file:
# ptII_quan_Bayes_MaximumEntropy_helpfuncs.r

# location:
# chap. 6 [6.14]
# Maximum Entropy

# HELPER FUNCTIONS


###### function to calculate entropy H(X) of a perfect fair coin
H1 <- function(e=NA, base=2)
{
 #p = prob of event e - here binary, ie. e=2
 p <- 1/e
 H <- -sum(replicate(e,p*log(p, base=base))) 
return(H)
} 
# call:
# H1(e=2,base=2)
# H1(e=3,base=2)
########################## END OF FUNCTION


###### function to calculate entropy H(X) of a not-perfect fair coin
H2 <- function(p) -( p*log2(p) + (1-p)*log2(1-p) )
# call:
# p <- 0.5
# q <- 1-p
# H2(p=p)
########################## END OF FUNCTION


###### function to simulate a dice and the entropy
dice <- function(k, N, seed=83345)
{
 if(!is.null(seed)) set.seed(seed)
 x <- round((k-1)*runif(N))+1
 v <- rep(NA,k)
 nk <- sapply(seq_along(1:k), function(i) sum(x == i))
 probs <- nk/N
 names(probs) <- names(nk) <- 1:k
 H <- -sum(probs*log(probs))
return(list(faces=x, nk=nk, probs=probs, H=H))
}
# call:
# d1 <- dice(k=6, N=100) 
########################## END OF FUNCTION

