# file:
# ptII_quan_Bayes_information-criteria_helpfuncs.r

# location:
# chap. 6 [6.7.2]
# Informationskriterien

# HELPER FUNCTIONS

# load necessary libs
library(pscl)
library(lme4)


###### function to calculate and extract information criteria from lm/ glm/ lmer/ glmer objects
exIC <- function(model, alpha=0.05, digits=4, k=2, pr.out=TRUE, CHECK=FALSE, NOTE=NULL)
{
  # !! k = number of parameters
  require(pscl)
  require(wiqid)
  
  modelclass <- class(model)[1]
  
  if(modelclass %in% c("glm","lm"))
  {
    nobs <- nobs(model) #dim(model$model)[1]
    # number of parameters
    fixef.anz <- dim(model.matrix(model))[2]
    k <- fixef.anz
  } else if(modelclass %in% c("lmerMod","glmerMod"))
  {
    nobs <- nobs(model) #dim(model@frame)[1]
    # number of parameters
    fixef.anz <- dim(model.matrix(model))[2]
    # https://stackoverflow.com/questions/30896540/extract-raw-model-matrix-of-random-effects-from-lmer-objects-lme4-r
    ranef.anz <- dim(do.call(cbind,getME(model,"mmList")))[2]
    if(modelclass == "lmerMod")
    {
      remlTF <- model@resp$REML == 2
      if(!remlTF) k <- fixef.anz + ranef.anz else k <- ranef.anz
    } else if(modelclass == "glmerMod")
    {
      k <- fixef.anz + ranef.anz
    }  
    
  }  
  
  df.resid <- df.residual(model)
  
  AIC <- AIC(model)
  # = GOF
  
  if(CHECK)
  {
    # checks
    # AIC = extractAIC
    cat("\ncheck for AIC and k\n")
    print( extractAIC(model)[2] == AIC(model) )
    # edf = equivalent degrees of freedom == k
    print( extractAIC(model)[1] == k )
  }
  if(modelclass == "lmerMod" && remlTF)
  {
    DIC <- deviance(model, REML=FALSE)
  } else DIC <- deviance(model)
  
  lLH <- logLik(model)
  BIC <- BIC(model)
 
  # AICv2 <- extractAIC(model)
  # https://en.wikipedia.org/wiki/Akaike_information_criterion
  AICc <- AIC + (2*k^2 + 2*k) / (nobs - k -1)
  #AICcc <- AIC(model)
  # https://en.wikipedia.org/wiki/Hannan-Quinn_information_criterion
  HQC <- -2*lLH[1] + 2*k*log(log(nobs))
 
  PearsonChi2 <- sum(resid(model,"pearson")^2)
  # phi = variance vs. mean (=intercept?)
  phi <- PearsonChi2/df.resid
 
  # library 'pscl'
  # ?pR2
  # llh = The log-likelihood from the fitted model
  # llhNull = The log-likelihood from the intercept-only restricted model
  # G2 = Minus two times the difference in the log-likelihoods
  # McFadden = McFadden's pseudo r-squared
  # r2ML= Maximum likelihood pseudo r-squared
  # r2CU = Cragg and Uhler's pseudo r-squared
  if(modelclass %in% c("lmerMod","glmerMod"))
  {
    NOTE <- c("Info: No Pseudo-R2 measures for (g)lmer models.")
    pR2s <- rep("none",6)
  } else pR2s <- pR2(model)
  
  res <- structure(list(
              model=modelclass,
              N=nobs,
              k=k,
              df.resid=df.resid,
              AIC=AIC,
	            AICc=AICc,
	            BIC=BIC,
	            DIC=DIC,
	            lLH=lLH,
	            HQC=HQC,
              
              "crit Chi^2"=qchisq(1-alpha, df.residual(model)), #pchisq??? qchisq???
              "Pearson Chi^2"=PearsonChi2,
              
	            phi=phi,
              "sqrt(phi)"=sqrt(phi),
              
              "llh [pR2s]"=pR2s[1],
              "llhNull [pR2s]"=pR2s[2],
              "G2 [pR2s]"=pR2s[3],
              "McFadden [pR2s]"=pR2s[4],
              "r2ML [pR2s]"=pR2s[5],
              "r2CU [pR2s]"=pR2s[6]
              ))
  
  if(pr.out)
  {
    TITLE <- c("Information Criteria for model type = ")
    cat("\n    ", TITLE, modelclass,"\n\n",sep="")
    cat(paste(format(names(res), width = 17L, justify = "right"),  #15L
              format(res, digits = digits, nsmall=2), sep = " = "), sep = "\n")
    if(!is.null(NOTE)) cat("\n", "NOTE: ", NOTE, "\n\n", sep = "")
    cat("\n")
  }  
return(res)
}
# call:
# glm.D93.IC <- exIC(glm.D93)
########################## END OF FUNCTION


