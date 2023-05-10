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
# ptII_quan_EDA_case_Chiro_heartrate-variability_helpfuncs.r

# location:
# chap. 5 [5.5.6]
# Ein Experiment zur Herzratenvariabilität

# HELPER FUNCTIONS


###### function to plot interactions
ia.plot <- function(dframe=NA, facs=NA, f1nam=NA, f2nam=NA, vars=NA,
                    TITLE="", SUB="", colo=c("violetred3","blue","yellowgreen","orange"), ylab="mean",
					trace.label="xxx", ...)
{
 par(mar=c(5,5,4,2), oma=c(2,1,5,1), cex.axis=0.8, mfrow=c(2,2))
 for(i in vars)
 {
  interaction.plot(facs[,f1nam], facs[,f2nam], dframe[,i], pch=21, cex=1.5, bg=colo, type="b", xtick=TRUE,
                   trace.label=trace.label, bty="n", main=i, col=colo, ylab=ylab, xlab=f1nam)
 }
 mtext(TITLE, 3, line=2, cex=1.5, outer=TRUE)
 mtext(SUB, 3, line=.35, cex=1.1, outer=TRUE)
}
# call:
# ia.plot(dframe=daten.analy, f1=timepoint, f2=sex, vars=vars, TITLE=TITLE, SUB=SUB, colo=colo)
########################## END OF FUNCTION

