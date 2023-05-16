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
# ptII_quan_classicstats_effectsizes_helpfuncs.r

# location:
# chap. 4 [4.6.10]
# Effektstärken — Größe, Häufigkeit und Bezug zur Originalskala

# HELPER FUNCTIONS

# previously on sjstats package - not anymore (!)


# convert odds (OR=) to RR
# taken from sjstats
# Version:            0.17.5
# Date:               2019-06-04
odds_to_rr <- function (fit) 
{
  fitinfo <- get_glm_family(fit)
  if (!fitinfo$is_bin && !fitinfo$is_logit) 
    stop("`fit` must be a binomial model with logit-link (logistic regression).", 
         call. = F)
  est <- exp(stats::coef(summary(fit))[, 1])
  if (is_merMod(fit)) 
    ci <- stats::confint(fit, method = "Wald", parm = "beta_")
  else ci <- stats::confint(fit)
  or.dat <- data.frame(est, exp(ci))
  colnames(or.dat) <- c("OR", "lower.ci", "upper.ci")
  modfram <- stats::model.frame(fit)
  P0 <- mean(sjlabelled::as_numeric(modfram[[1]], start.at = 0, 
                                    keep.labels = F), na.rm = T)
  rr.dat <- or.dat/((1 - P0) + (P0 * or.dat))
  colnames(rr.dat) <- c("RR", "lower.ci", "upper.ci")
  rr.dat
}

# from previous sjstats
#

get_glm_family <- function(fit) {
  c.f <- class(fit)
  
  # do we have glm? if so, get link family. make exceptions
  # for specific models that don't have family function
  if (any(c.f %in% c("lme", "plm"))) {
    fitfam <- ""
    logit_link <- FALSE
  } else {
    fitfam <- stats::family(fit)$family
    logit_link <- stats::family(fit)$link == "logit"
  }
  
  # create logical for family
  binom_fam <- fitfam %in% c("binomial", "quasibinomial")
  poisson_fam <- fitfam %in% c("poisson", "quasipoisson") ||
    sjmisc::str_contains(fitfam, "negative binomial", ignore.case = T)
  
  list(is_bin = binom_fam, is_pois = poisson_fam, is_logit = logit_link)
}


# from previous sjstats
# 

is_merMod <- function(fit) {
  inherits(fit, c("lmerMod", "glmerMod", "nlmerMod", "merModLmerTest"))
}

# convert RR to odds (OR=)
# taken from sjstats
# Version:            0.17.5
# Date:               2019-06-04
or_to_rr <- function (or, p0) 
{
  or/(1 - p0 + (p0 * or))
}
 
  
  
odds_to_rr.v2 <- function (fit) 
{
  fitinfo <- get_glm_family(fit)
  if (!fitinfo$is_bin && !fitinfo$is_logit) 
    stop("`fit` must be a binomial model with logit-link (logistic regression).", 
         call. = F)
  est <- exp(stats::coef(summary(fit))[, 1])
  if (is_merMod(fit)) 
    ci <- stats::confint(fit, method = "Wald", parm = "beta_")
  else ci <- stats::confint(fit)
  or.dat <- data.frame(est, exp(ci))
  colnames(or.dat) <- c("OR", "lower.ci", "upper.ci")
  modfram <- stats::model.frame(fit)
  P0 <- mean(sjlabelled::as_numeric(modfram[[1]], start.at = 0, 
                                    keep.labels = F), na.rm = T)
  #print(or.dat)
  #print(P0)
  rr.dat <- or.dat/((1 - P0) + (P0 * or.dat))
  colnames(rr.dat) <- c("RR", "lower.ci", "upper.ci")
  return( list( OR=or.dat, RR=rr.dat, P0=P0) )
}

