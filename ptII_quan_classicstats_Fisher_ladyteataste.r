# 2020-10-02
# file:
# ptII_quan_classicstats_Fisher_ladyteataste.r
#
# location:
# chap. 4 [4.5.1.1]
# Vom Tee trinken und Milch erkennen — ein Beispielexperiment nach Fisher


# correct classification possibilities for the four "tea first" variants
# all possible four group variants

#n=8
#k=4
#n!/(k!*(n-k)!)
(8*7*6*5*4*3*2*1) / (4*3*2*1)
#=
factorial(8)/factorial(8-4)
#=1680

# number of different sequences for the four group
4*3*2*1

prod(4:1)
#=24

# all variants divided by different sequences of the four group
1680/24
#70

# 8-over-4 Binomialkoeffizient -> possible ways to classify 4 of 8 cups properly as tea first or v.v.
# binomial coefficient
# possible ways to classify 4 of 8 cups of tea properly with "tea first" (=four group)
choose(8,4)

# chance to get all correct classified in case no discrimination abilities are present
# prob per cent
1/70
#0.01428571 

# small function just to calculate this
teataste <- function(teeproben, positive)
{
  return(factorial(teeproben)/(factorial(positive)*factorial(teeproben-positive)))
}
# call:
teataste(8,4)

# lotto
# 6 right out of 49 (= "lotto gambling")
teataste(49,6)

# 1 of 13983816
choose(8,4)

# use Fisher test to calculate prob for '8 right out of 8' with fixed margins
tea.test <- matrix(c(4, 0, 0, 4), nrow = 2, dimnames = list(Guess = c("Milk", "Tea"), Truth = c("Milk", "Tea")))
tea.test
tea.ftest <- fisher.test(tea.test, alternative = "greater")
tea.ftest
#check
all.equal(tea.ftest$p.value,1/70)
#= 1/70


###### end of R-script from book


###### add-on - only R script

#hypergeoemtric distribution (density function)
#
#draw WITHOUT replacement
#
#to get the various probabilities
#function calculates probabilities, not combinations
dhyper.comb <- dhyper(0:4,4,4,4)
names(dhyper.comb) <- 0:4

#combinations
dhyper.comb * choose(8,4)
#cumulated combinations
cumsum(dhyper.comb * choose(8,4))

#probs
dhyper.comb
#cumsum probs
cumsum(dhyper.comb)

#combined prob -> 3 or 4 correct
(dhyper(3,4,4,4) + dhyper(4,4,4,4))
#=
sum(dhyper.comb[c("3","4")])

#do it manually
hyperg <- function(K,N,n,k)
{
 # K = number of possible successes in population
 # N = population size
 # n = number of draws (trials)
 # k = number of empirical successes
 choose(K,k)*choose(N-K,n-k)/choose(N,n)
}

#calls
#
#all correct
hyperg(K=4, N=8, n=4, k=4)

#compare to dhyper() results
#prob for 0 to 4 successes
hyperg(K=4, N=8, n=4, k=0:4)

#combinations
hyperg(K=4, N=8, n=4, k=0:4) * choose(8,4)

#all four right (what Lady Bristol was able to achieve!)
#exact Fisher Test
tea.4.0 <- matrix(c(4, 0, 0, 4), nrow=2, dimnames=list(guess=c("milk first", "tea first"), truth=c("milk first", "tea first")))
tea.4.0
fisher.test(tea.4.0, alternative="greater")

#chi^2 test is an approximation to the exact test
#not to be be used for very small sample sizes
#chi^2 test
chisq.test(tea.4.0)

#3 right, 1 wrong
tea.3.1 <- matrix(c(3, 1, 1, 3), nrow=2, dimnames=list(guess=c("milk first", "tea first"), truth=c("milk first", "tea first")))
tea.3.1
fisher.test(tea.3.1, alternative="greater")
chisq.test(tea.3.1)


