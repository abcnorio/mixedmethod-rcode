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
# ptII_quan_Bayes_PPC_model-check_helpfuncs.r

# location:
# chap. 6 [6.7.4.4]
# Praxis — posterior predictive check

# HELPER FUNCTIONS


###### function to calculate and plot two-sided and one-sided test with both directions
plot.comp.bpv <- function(sek, postps, compcrit=0, Trep=NA, colo="skyblue", dig=5, PR=TRUE, type="one-sided", direct="biggerthan")
{
 # plot Bayesian p-value cumulative probs
 # BE CAREFUL! one sided test here...
 if(sum(sek == compcrit) == 0)
 {
  # differ <- mean(Trep) - compcrit
  differ <- compcrit
  bigger.id <- min(which(sek > differ))
  smaller.id <- max(which(sek < differ))
  compcrit <- mean(sek[c(min(bigger.id),smaller.id)])
  postps.value <- mean(postps[c(min(bigger.id),smaller.id)])
 } else
 {
  postps.value <- postps[sek == compcrit]
 } 
 # vertical
 lines(c(compcrit,compcrit),c(0,postps.value),col=colo, lty=2)
 # horizontal
 lines(c(sekstart,compcrit),c(postps.value,postps.value),col=colo, lty=2)
 TEXT <- paste("Mass p = ",postps.value," at crit = ",signif(compcrit,5),sep="")
 text(compcrit,postps.value,TEXT, col=colo, pos=4)
 res <- data.frame(compcrit,postps.value,"Bayes pv (one-sided test)"=1-postps.value, check.names=FALSE)
return(res) 
}
#call:
# zero line
# plot.comp(sek,postps,compcrit=0)
# empirical posterior mode
# plot.comp(sek,postps,compcrit=Ty,Trep, colo="red")
########################## END OF FUNCTION

