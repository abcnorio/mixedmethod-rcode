# file:
# ptII_quan_Bayes_Entropy_KullbackLeibler_helpfuncs.r

# location:
# chap. 6 [6.14.1]
# „I, we, and nation“ — präsidiale Eigenwerbung

# HELPER FUNCTIONS


###### function to calculate H
H.counts <- function(counts, type="Jeffreys", priorN=0, ein="log")
{
  # cell frequency priors
  # http://www.jmlr.org/papers/volume10/hausser09a/hausser09a.pdf
  # type priors
  # = 0 = no prior
  # = 0.5 = Jeffreys 
  # = 1 = uniform Bayes Laplace
  # = 1/length(y) = SG
  # = sqrt(sum(y))/length(y) = minimax
  #
  priors <- c("0","0.5","1","1/length(counts)","sqrt(sum(counts))/length(counts)")
  priors.nam <- c("noprior","Jeffreys","BayesLaplace","SG","minimax")
  prior.id <- which(priors.nam %in% type)
  prior <- priors[prior.id]
  
  Bayes.Dirichlet <- function(counts, prior, priorN=0)
  {
    cp <- counts + prior
    #priorN = paper p.1471 chap. 2.3 "A may be interpreted as the a priori sample size"
    total <- sum(cp) + priorN
    freqs <- cp/total
    return(freqs)
  }
  #call:
  freqs.Bayes <- Bayes.Dirichlet(counts, prior=eval(parse(text=prior)), priorN=priorN)
  freqs <- counts/sum(counts)
  Hc <- -sum(freqs * log(freqs))
  Hc.Bayes <- -sum(freqs.Bayes * log(freqs.Bayes))
  res <- c(Hc, Hc.Bayes)
  names(res) <- c("Hc","Hc.Bayes")
  if(ein == "log2") res <- res/log(2)
  if(ein == "log10") res <- res/log(10)
  return(res)
}
########################## END OF FUNCTION


###### function to calculate Kullback-Leibler distance based on counts (discrete case)
KL.counts <- function(counts1, counts2, ein="log")
{
  # Hausser, J., and K. Strimmer. 2009. Entropy inference and the James-Stein estimator,
  # with application to nonlinear gene association networks. J. Mach. Learn. Res. 10: 1469-1484. (arXiv:0811.3579)
  # http://strimmerlab.org/software/entropy/
  # http://www.jmlr.org/papers/volume10/hausser09a/hausser09a.pdf
  # http://jmlr.csail.mit.edu/papers/v10/hausser09a.html
  freqs1 <- counts1/sum(counts1)
  freqs2 <- counts2/sum(counts2)
  # expectation of log(freqs1 / freqs2)
  KL <- sum(freqs1 * log(freqs1/freqs2))
  # observed vs expected (prob mass functions)
  CHI2 <- sum((freqs1 - freqs2)^2/freqs2)
  res <- c(KL, CHI2)
  names(res) <- c("KL","CHI^2")
  if(ein == "log2") res <- res/log(2)
  if(ein == "log10") res <- res/log(10)
  return(res) 
}
# call:
# KL.counts(count1,counts2)
########################## END OF FUNCTION


