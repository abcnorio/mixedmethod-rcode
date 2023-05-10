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
# ptII_quan_Bayes_case_wordcounts-PPC_helöpfuncs.r

# location:
# chap. 6 [6.7.4.6]
# Forschungsbeispiel — Wortproduktion Humor

# HELPER FUNCTIONS
# adjusted from DBDA (Kruschke)


###### function to summarize MCMC chains from Kruschke (adjusted to match requirements here)
smryMCMC2 <- function(codaSamples, saveName=NULL)
{
 summaryInfo <- NULL
 mcmcMat <- as.matrix(codaSamples,chains=TRUE)
 paramName <- colnames(mcmcMat)
 for(pName in paramName)
 {
  summaryInfo <- rbind(summaryInfo, summarizePost2(mcmcMat[,pName]))
 }
 rownames(summaryInfo) <- paramName
 summaryInfo <- rbind(summaryInfo, "log10(nu)"=summarizePost2(log10(mcmcMat[,"nu"])))
 if(!is.null(saveName))
 {
  write.csv( summaryInfo , file=paste(saveName,"SummaryInfo.csv",sep=""))
 }
return(summaryInfo)
}
# call:
# smryMCMC2(codaSamples=model)
########################## END OF FUNCTION


###### function to summarize posterior from Kruschke (adjusted to match requirements here)
summarizePost2 <- function(paramSampleVec, compVal=NULL, ROPE=NULL, credMass=0.95)
{
 meanParam <- mean(paramSampleVec)
 medianParam <- median(paramSampleVec)
 dres <- density(paramSampleVec )
 modeParam <- dres$x[which.max(dres$y)]
 sdParam <- sd(paramSampleVec)
 varParam <- var(paramSampleVec)
 mcmcEffSz = round(effectiveSize(paramSampleVec),1)
 names(mcmcEffSz) <- NULL
 hdiLim <- HDIofMCMC(paramSampleVec, credMass=credMass)
 if (!is.null(compVal))
 {
   pcgtCompVal <- (100 * sum(paramSampleVec > compVal) / length(paramSampleVec))
 } else
 {
  compVal=NA
  pcgtCompVal=NA
 }
 if(!is.null(ROPE))
 {
   pcltRope <- (100 * sum(paramSampleVec < ROPE[1]) / length(paramSampleVec))
   pcgtRope <- (100 * sum(paramSampleVec > ROPE[2]) / length(paramSampleVec))
   pcinRope <- 100 - (pcltRope+pcgtRope)
 } else { 
   ROPE <- c(NA,NA)
   pcltRope <- NA 
   pcgtRope <- NA 
   pcinRope <- NA 
 }  
res <- c(Mean=meanParam, Median=medianParam, Mode=modeParam, 
         SD=sdParam, VAR=varParam,
	     ESS=mcmcEffSz,
         HDImass=credMass, HDIlow=hdiLim[1], HDIhigh=hdiLim[2], 
         CompVal=compVal, PcntGtCompVal=pcgtCompVal, 
         ROPElow=ROPE[1], ROPEhigh=ROPE[2],
         PcntLtROPE=pcltRope, PcntInROPE=pcinRope, PcntGtROPE=pcgtRope
         )
return(res)
}
# call:
# summarizePost2(paramSampleVec, compVal=NULL, ROPE=NULL, credMass=0.95)
########################## END OF FUNCTION


###### function to prepare MCMC diagnostics from Kruschke (adjusted to match requirements here)
mcmc.diag.kruschke <- function(model, dats, xName, yName, PR=FALSE, PLOTdens=TRUE, PLOTmult=FALSE, PLOThist=FALSE, saveName=NULL, graphFileType="jpg")
{
 # MCMC diagnostic
 summaryInfo <- smryMCMC2(model, saveName=saveName)
 if(PR) show(summaryInfo)
 if(PLOTdens)
 {
  parameterNames <- varnames(model) # get all parameter names
  for(parName in parameterNames )
  {
   diagMCMC(codaObject=model, parName=parName, saveName=saveName, saveType=graphFileType)
  }
 }
 if(PLOThist)
 {
    plotMCMC(model, data=dats, xName=xName, yName=yName, pairsPlot=TRUE, showCurve=FALSE,
             saveName=saveName, saveType=graphFileType)
 }
 return(summaryInfo)
}
# call:
# mcmc.diag.kruschke(model=mcmc.dm1, dats=diss.nona, xName=xName, yName=yName)
########################## END OF FUNCTION

