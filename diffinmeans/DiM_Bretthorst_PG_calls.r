################################################################################
# ON THE DIFFERENCE OF MEANS
################################################################################

# original Mathematica code by Phil Gregory
# http://www.phas.ubc.ca/~gregory/gregory.html
# Ch. 9: Bayesian analysis of two independent samples
# Introduction
# This is a Mathematica implementation of the probability calculations discussed in the book in the section entitled,     
# "Bayesian Comparison of Two Samples?". 
#
# based on a paper from G.L. Bretthorst (1993) "on the difference of means"
# http://bayes.wustl.edu/glb/diff.pdf
#
# see also Mathematica code by UM Studer (1996 + 1998) on the same problem (paper)
# taken code from there to allow calculations based only on summary statistics
# and not on raw data (see also Bretthorst, 1993, for an example taken from Jaynes)

# R code by Leo Gürtler 2017-
# first = 2017-04-19
# last = 2020-10-22

# notes:
# *- introduce logs to integral calculations, but probably that won't help...
# *- very small numbers are slightly different from Mathematica -> e.g. e-230

# call PG scheme
inputvalues <- list(snames = c("riverB.1","riverB.2"),
                    # sample 1
                    d1 = c(13.2,13.8,8.7,9,8.6,9.9,14.2,9.7,10.7,8.3,8.5,9.2),
                    # sample 2
                    d2 = c(8.9,9.1,8.3,6,7.7,9.9,9.9,8.9),
                    
                    # Input priors and no. of steps in evaluation of p(r|D_1,D_2,I) & p(delta|D_1,D_2,I)
                    # ndelta = number of steps in delta parameter (mean difference)
                    ndelta = 1000, #100
                    # nr = number of steps in r parameter (ratio of the standard deviations)
                    nr = 1000, # 100
                    
                    # Set prior limits (assumed the same for each data set) on mean (low,high),
                    # and prior limits (assumed the same for each data set) on the
                    # standard deviation (sigmalow, sigmahigh).
                    # upper mean
                    high = 12,
                    # lower mean
                    low = 7,
                    # upper sd
                    sigma.high = 4,
                    # lower sd
                    sigma.low = 1)
inputvalues
dim.res <- DiM.pg(invtyp="pg", inputvalues, print.res=TRUE)
dim.res.newlimits <- DiM.extract.limits(dim.res, scaleL=30, scaleH=4, change=TRUE)
DiM.extract.limits(dim.res.newlimits, change=FALSE)
dim.res.calc <- DiM.plot.calc.pg(dim.res.newlimits, BROB=FALSE)
DiM.plot.pg(dim.res.calc, filling=FALSE, BROB=FALSE)



# call according to UMS scheme
inputvalues <- list(snames=c("Jaynes.1","Jaynes.2"), si=6.48, Ni=4, sii=7.48, Nii=9, Di=50, Dii=42, L=34, H=58, sL=3, sH=10, ndelta=1000, nr=1000)
inputvalues <- ums2pg(inputvalues)
inputvalues
dim.res <- DiM.pg(invtyp="ums", inputvalues=inputvalues, print.res=TRUE)



#works
res.SIB <- SucRatesIntBounds(Si=11, Ni=15, Sii=10, Nii=16, smin=0, snames=c("voluntary","non-voluntary"))
res.SIB
DIM.pg.res <- DiM.pg(invtyp="ums", inputvalues=res.SIB, print.res=TRUE, BROB=FALSE)
DIM.pg.res
DiM.res <- DIM.pg.res
DiM.print.pg(DIM.pg.res)
DiM.plotvalues.res.nonbrob <- DiM.plot.calc.pg(DIM.pg.res, BROB=FALSE)
DiM.plot.pg(DiM.plotvalues.res.nonbrob, filling=TRUE, BROB=FALSE)
#as brob
DIM.pg.res <- DiM.pg(invtyp="ums", inputvalues=res.SIB, print.res=TRUE, BROB=TRUE)
DIM.pg.res
DiM.print.pg(DIM.pg.res)
DiM.plotvalues.res.nonbrob.brob <- DiM.plot.calc.pg(DIM.pg.res, BROB=TRUE)
DiM.plot.pg(DiM.plotvalues.res.nonbrob.brob, filling=TRUE, BROB=TRUE)


#normally does not work
res.SIB.NRFtotal <- SucRatesIntBounds(Si=(20+13), Ni=(47+28), Sii=(338 %/% 4), Nii=338, smin=0, snames=c("male","female"))
res.SIB.NRFtotal
DIM.pg.res.brob <- DiM.pg(invtyp="ums", inputvalues=res.SIB.NRFtotal, print.res=TRUE, BROB=TRUE)
DiM.print.pg(DIM.pg.res.brob)
#try several limits...
DiM.extract.limits(DIM.pg.res.brob, scaleL=10, scaleH=1, change=FALSE)
DiM.extract.limits(DIM.pg.res.brob, scaleL=10, scaleH=2, change=FALSE)
DiM.extract.limits(DIM.pg.res.brob, scaleL=5, scaleH=2, change=FALSE)
DiM.extract.limits(DIM.pg.res.brob, scaleL=100, scaleH=4, change=FALSE)
#aplly new limits
DiM.newlimits <- DiM.extract.limits(DIM.pg.res.brob, scaleL=10, scaleH=1, change=TRUE)
DiM.newlimits <- DiM.extract.limits(DIM.pg.res.brob, scaleL=100, scaleH=4, change=TRUE)
#check
DiM.extract.limits(DiM.newlimits, change=FALSE)
#calc plot values
DiM.newlimits.calc.plot <- DiM.plot.calc.pg(DiM.newlimits, BROB=TRUE)
#plot
DiM.plot.pg(DiM.newlimits.calc.plot, filling=FALSE, by1=TRUE, BROB=TRUE)

