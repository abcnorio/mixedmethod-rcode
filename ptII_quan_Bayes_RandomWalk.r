# file:
# ptII_quan_Bayes_RandomWalk.r

# location:
# chap. 6 [6.13]
# Marko Chain Monte Carlo Simulationen — MCMC

# load necessary libraries
library(diagram)

# load necessary helper functions
source("ptII_quan_Bayes_RandomWalk_helpfuncs.r")

# simple random walk
walk.2d <- randomwalk(seed=667)
walk.3d <- randomwalk(seed=766, D=3)
head(walk.3d)
tail(walk.3d)
 
# Markov transition matrix
states <- c("playful","depressed","hopeful")
states.l <- length(states)
transmat <- matrix(data=c(0.4,0.4,0.2,
                          0.3,0.5,0.2,
                          0.3,0.1,0.6),
                   ncol=states.l, nrow=states.l, byrow=TRUE)
rownames(transmat) <- colnames(transmat) <- states
transmat
par(oma=c(2,1,1,1), "cex.axis"=1, bty="l")
plotmat(transmat, relsize=0.8,
        self.shiftx = c(0.1,-0.1,0.1), 
        self.shifty = c(-0.1,-0.1,0.10), 
        self.arrpos = c(4.9,4.9,4.9),
        self.cex = c(0.5),
        box.col="darkred",
        box.lcol="orange",
        lcol="blue",
        txt.col="white")
mtext(expression(paste("Markov chain transition matrix")),outer=TRUE,line=-4,cex=1.5, side=3)

