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
# ptII_quan_classicstats_N-P_example-soccer-sim.r

# location:
# chap.4 [4.5.4.1]
# Fallbeispiel Simulation — Fussball Sammelbilder

# HELPER FUNCTIONS


###### function to simulate how many cards are necessary until the album is full and nothing is missing
#drawing from pool with replacement
#modified to contain full process
BL.sim <- function(Nkarten, anzpp, zaehl=0, IDsl=1000, doIDs=FALSE, zaehlonly=FALSE, total=FALSE)
{
 karten <- rep(0, Nkarten)
 names(karten) <- 1:Nkarten
 pool <- 1:Nkarten
 IDs <- rep(NA, IDsl)
 if(total) karten.total <- rep(NA, IDsl*anzpp) else karten.total <- NA
 while(TRUE)
 {
  ran.karten <- sample(pool, anzpp, replace=FALSE)
  if(total) karten.total[(zaehl*anzpp+1):(zaehl*anzpp+anzpp)] <- ran.karten
  karten[ran.karten] <- karten[ran.karten] +1
  zaehl <- zaehl + 1
  ids <- which(karten == 0)
  id.l <- length(ids)
  if(doIDs == TRUE) IDs[zaehl] <- id.l
  if(id.l == 0) break
 }
 if(zaehlonly == FALSE) return(list(zaehl=zaehl, karten=karten, IDs=IDs, karten.total=karten.total)) else
 {
  return(zaehl)
 }
}
# call:
# BL.sim(Nkarten=Nkarten, anzpp=anzpp, doIDs=TRUE, total=FALSE)
########################## END OF FUNCTION



