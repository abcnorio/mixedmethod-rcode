# file:
# ptII_quan_Bayes_case_presidential-heights_helpfuncs.r

# location:
# chap. 6 [6.15.1]
# Präsidiale Höhenflüge

# HELPER FUNCTIONS


###### alternative function to bayes.binom.test from BayesianFirstAid
bayes.binom.alt <- function(x=NA, n=NA, n.chains=3, n.iter=15000, cred.mass=0.87, comp.theta=0.5,
                            x_name="", n_name="",
                            model_string=NA,
                            printout=TRUE, plotout=TRUE, prior.a=1, prior.b=1)
{
  require(BayesianFirstAid)
  if(is.na(model_string)) bbinom.model.string <- paste("model {\n  x ~ dbinom(theta, n)\n  theta ~ dbeta(",prior.a,", ",prior.b,")\n  x_pred ~ dbinom(theta, n)\n}",sep="")
  DNAME <- x_name
  mcmc_samples <- BayesianFirstAid:::run_jags(
    model_string=bbinom.model.string,
    data=list(x=x, n=n), inits=list(theta = (n + 1) / (n + 2)), 
    params=c("theta", "x_pred"), n.chains=n.chains, n.adapt=0,
    n.update=0, n.iter=n.iter, thin=1, progress.bar="none"
  )
  mcmc_samples
  stats <- BayesianFirstAid:::mcmc_stats(mcmc_samples, cred_mass = cred.mass, comp_val = comp.theta)
  bfa_object <- list(x = x, n = n, comp_theta = comp.theta, cred_mass = cred.mass,
                     x_name = x_name, n_name = n_name, data_name = DNAME,
                     mcmc_samples = mcmc_samples, stats = stats,
                     model=bbinom.model.string) 
  class(bfa_object) <- c("bayes_binom_test", "bayesian_first_aid")
  if(printout) print(summary(bfa_object))
  if(plotout) plot(bfa_object)
  return(bfa_object)
}
# call:
# h.bbt.res <- bayes.binom.alt(x=x, n=n, cred.mass=0.87, comp.theta=0.5, x_name=x_name, n_name=n_name, model_string=bbinom.model.string)
# h.bbt.res <- bayes.binom.alt(x=x, n=n, cred.mass=0.87, comp.theta=0.5, x_name=x_name, n_name=n_name)
# h.bbt.res <- bayes.binom.alt(x=x, n=n, cred.mass=0.87, comp.theta=0.5, x_name=x_name, n_name=n_name, prior.a=2, prior.b=45)
########################## END OF FUNCTION


###### function to calculate posterior (Odds) Ratios
post.comp <- function(post1, post2, digits=3, RETURN=FALSE)
{
  RR <- p1/p2
  RR.inv <- 1-RR
  OR <- (post1 * (1-post2)) / (post2 * (1-post1))
  OR.inv <- 1-OR
  res <- structure(list("post[1]"=post1, "post[2]"=post2,
                        "Ratio (p1 vs. p2)"=RR,
                        "Ratio (p2 vs. p1)"=1/RR,
                        "Odds Ratio (p1 vs. p2)"=OR,
                        "Odds Ratio (p2 vs. p1)"=1/OR,
                        WHATIS="Comparison of two posterior values", note=NULL), class="post.comp")
  # taken from stats:::print.power.htest()
  cat("\n    ", res$WHATIS, "\n\n")
  note <- res$note
  res[c("WHATIS", "note")] <- NULL
  cat(paste(format(names(res), width = 15L, justify = "right"), 
            format(res, digits = digits), sep = " = "), sep = "\n")
  if(!is.null(note)) 
  {
    cat("\n", "NOTE: ", note, "\n\n", sep = "")
  } else cat("\n")
  
  if(RETURN) return(res) else invisible()
}
# call:
# p1 <- mean(meanDiff.diff > 0)
# p2 <- mean(meanDiff.diff < 0)
# p1vsp2 <- post.comp(post1=p1 , post2=p2, RETURN=TRUE)
########################## END OF FUNCTION

