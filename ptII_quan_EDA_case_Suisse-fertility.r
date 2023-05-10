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
# ptII_quan_EDA_case_Suisse-fertility.r

# location:
# chap. 5 [5.5.2]
# Fruchtbarkeit und Fertilität

# load necessary libs
library(lattice)
library(hexbin) 
library(rockchalk)
library(arm)

library(fields)
library(corrgram)

# load necessary helper functions
source("ptall_generalfuncs.r")
source("ptII_quan_EDA_case_Suisse-fertility_helpfuncs.r")


# Suisse fertility data
# Mosteller, F. and Tukey, J. W. (1977) Data Analysis and Regression: A Second Course in Statistics. Addison-Wesley, Reading Mass. 

# data description
# 1. Fertility: common standardized fertility measure
# 2. Agriculture: % of males involved in agriculture as occupation
# 3. Examination: % draftees receiving highest mark on army examination
# 4. Education: % education beyond primary school for draftees.
# 5. Catholic: % ‘catholic' (as opposed to ‘protestant’).
# 6. Infant.Mortality: live births who live less than 1 year.

?swiss
head(swiss)
tail(swiss)

# first look on numbers
describes(swiss)

# alternatives for descriptive statistics
t(apply(swiss,2,fivenum.wn))
t(apply(swiss,2,summary))

# #first look on plots
par(mfrow=c(2,1))
hist(swiss$Fertility, prob=TRUE)
lines(density(swiss$Fertility))
hist(swiss$Catholic, prob=TRUE)
lines(density(swiss$Catholic))

# log and sqrt are not that promising
hist(log(swiss$Catholic), prob=TRUE)
lines(density(log(swiss$Catholic)))
hist(sqrt(swiss$Catholic), prob=TRUE)
lines(density(sqrt(swiss$Catholic)))

# could be an U-shape
stem(swiss$Fertility)
stem(swiss$Catholic)

# various plots
boxplot(swiss$Fertility)
boxplot(swiss$Fertility ~ swiss$Catholic)
boxplot(swiss$Fertility ~ swiss$Catholic > 50)
boxplot(swiss$Fertility ~ swiss$Catholic < 50)
with(swiss, plot(Fertility, Catholic))
with(swiss, plot(Catholic, Infant.Mortality))
with(swiss, plot(Fertility, Catholic))
pairs(swiss)


# library 'lattice'
splom(~swiss, groups=Catholic > 50, data=swiss, type=c("p","smooth"), xlab="", ylab="", main="Swiss data (SPLOM/scatterplotmatrix)", pscale=0)

# plot
# library(hexbin)
scaplot.cont(x=swiss$Catholic, y=swiss$Fertility,
             TITLE="Swiss data on Fertility from 1888", SUB="Fertility versus Catholic",
             legendTITLE="Catholic Index", xtext="Catholic", ytext="Fertility")

	   
# correlations fertility and catholic index
# general r = .46
with(swiss, cor(Fertility, Catholic))
 
# add new variables Fertility and Catholic split at 50%
# correlations for F & C hi versus lo
nk <- 2
swiss2 <- data.frame(swiss, F.hi=swiss$Fertility > 50, C.hi=swiss$Catholic > 50)
swiss2
F.hilo <- factor(swiss2$F.hi, labels=c("F.lo","F.hi"))
C.hilo <- factor(swiss2$C.hi, labels=c("C.lo","C.hi"))
swiss2$FC <- factor(paste(F.hilo, C.hilo, sep=":"))
head(swiss2)
r <- NULL
for(i in levels(swiss2$FC))
{
 r[i] <- round(with(swiss2[swiss2$FC == i,], cor(Fertility, Catholic)),nk)
 cat(paste(i, " --- r = ", r[i],"\n",sep=""))
}


# further perspectives

# no Catholic r = -.29
with(subset(swiss2, Catholic < 50), cor(Fertility, Catholic))
# yes Catholic r = .757
with(subset(swiss2, Catholic > 50), cor(Fertility, Catholic))
# Catholic > 60 r = -2.5
with(subset(swiss2, Catholic > 60), cor(Fertility, Catholic))
# low Catholic level r = .19
with(subset(swiss2, Catholic < 40), cor(Fertility, Catholic))

# high Fertility r = .64
with(subset(swiss2, Fertility > 50), cor(Fertility, Catholic))
# low Fertility r = .76
with(subset(swiss2, Fertility < 50), cor(Fertility, Catholic))


#r=-1
subset(swiss2, Fertility < 50 & Catholic > 50)
#r=NA
subset(swiss2, Fertility < 50 & Catholic < 50)


# high F & high C r = -.25
r.hFhC <- with(subset(swiss2, Fertility > 50 & Catholic > 50), cor(Fertility, Catholic))
r.hFhC
# high F & low C r = .19
r.hFlC <- with(subset(swiss2, Fertility > 50 & Catholic < 50), cor(Fertility, Catholic))
r.hFlC
# just 2 obs
subset(swiss2, Fertility < 50 & Catholic > 50)
# low F and high C r = -1
r.lFhC <- with(subset(swiss2, Fertility < 50 & Catholic > 50), cor(Fertility, Catholic))
r.lFhC
# low F and low C r = NA
r.lFlC <- with(subset(swiss2, Fertility < 50 & Catholic < 50), cor(Fertility, Catholic))
r.lFlC
# just 1 obs
subset(swiss2, Fertility < 50 & Catholic < 50)


# two color split
# have a closer look on the various possibilities
plot(swiss$Catholic, swiss$Fertility, pch=21, bg=c("violetred2","blue")[(swiss$Catholic > 50)+1], bty="n", main="Swiss data (1888)")
abline(v=50, lty=2, col="red")
abline(h=50, lty=2, col="steelblue")
r.xy <- data.frame(x=c(70,20,70,20), y=c(90,90,40,40), colo=c("violetred2","blue","olivedrab","brown"))
for(i in 1:length(r))
{
 text(r.xy[i,c("x","y")],paste(names(r)[i]," = ",r[i],sep=""), col=as.character(r.xy[i,"colo"]))
} 

# Catholic - only lo Fertility
range(subset(swiss2, Fertility < 50)$Catholic)

# boxplot
# same data, same result, different perspective
str(swiss2)
TITLE <- "Swiss data on Fertility from 1888"
SUB <- "Fertility high/low vs. Catholic high/low"
boxplot(Fertility ~ FC, data=swiss2, notch=TRUE, col=c("olivedrab3","steelblue","orange","yellow"),
        boxwex=1, lex.order=TRUE, varwidth=TRUE, outline=TRUE, frame.plot=FALSE, border=2:5)
rug(swiss$Fertility, side=2, col="black")
mtext(TITLE, 3, line=2, cex=1.5)
mtext(SUB, 3, line=.6, cex=1.1)


# combine factor levels for F
# library(rockchalk)
levels(swiss2$FC)
swiss2$FC.adj <- combineLevels(swiss2$FC, levs=c("F.lo:C.lo","F.lo:C.hi"), newLabel="F.lo.C")
# check
swiss2[,c("FC","FC.adj")]
# boxplot
SUB <- "Fertility vs. Catholic high/low"
boxplot(Fertility ~ FC.adj, data=swiss2, notch=TRUE, col=c("olivedrab3","steelblue","orange"),
        boxwex=1, lex.order=TRUE, varwidth=TRUE, outline=TRUE, frame.plot=FALSE, border=2:5)
rug(swiss$Fertility, side=2, col="magenta3")
mtext(TITLE, 3, line=2, cex=1.5)
mtext(SUB, 3, line=.6, cex=1.1)


# descriptive
do.call("cbind",with(swiss2, tapply(Fertility, FC.adj, fivenum2)))
# linear model
seed <- 9876
table(swiss2$FC.adj)
# library 'arm'
arm:::display(swiss2.lm <- lm(Fertility ~ FC.adj, data=swiss2))
set.seed(seed)
swiss2.sim <- arm:::sim(swiss2.lm, n.sims=1000)
str(swiss2.sim)
apply(coef(swiss2.sim), 2, quantile)
quantile(sigma.hat(swiss2.sim))


# smooth scatterplot
# library 'fields'
# function 'fudgeit()' taken from
# https://stackoverflow.com/questions/14271584/r-legend-for-color-density-scatterplot-produced-using-smoothscatter
fudgeit <- function(){
  xm <- get('xm', envir = parent.frame(1))
  ym <- get('ym', envir = parent.frame(1))
  z  <- get('dens', envir = parent.frame(1))
  colramp <- get('colramp', parent.frame(1))
  fields::image.plot(xm,ym,z, col = colramp(256), legend.only = T, add =F)
}

par(mar=c(5,4,4,6) + .1)
with(swiss, smoothScatter(Fertility, Catholic, nrpoints=0, postPlotHook=fudgeit, colramp=colorRampPalette(c("white", "darkorange"))))
title("")
SUB <- "Fertility vs. Catholic index"
mtext(TITLE, 3, line=2, cex=1.5)
mtext(SUB, 3, line=.6, cex=1.1)


# pairs plot
# extensions due to additional functions, see ?pairs
# histogram on the diagonal
panel.hist <- function(x, colo="orange", ...)
{
   usr <- par("usr"); on.exit(par(usr))
   par(usr = c(usr[1:2], 0, 1.5) )
   h <- hist(x, plot = FALSE)
   breaks <- h$breaks; nB <- length(breaks)
   y <- h$density; y <- y/max(y)
   rect(0,0,max(h$breaks), max(h$breaks), col="grey90")
	 rect(breaks[-nB], 0, breaks[-1], y, col=colo, border="blue" , ...)#col="violetred2"
	 xdens <- density(x)
	 lines(xdens$x,max(y)/max(xdens$y)*xdens$y, col="green3", lwd=2, lty=2)
}
# correlations
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor=1, ...)
{
   usr <- par("usr"); on.exit(par(usr))
   par(usr = c(0, 1, 0, 1))
   r <- round(abs(cor(x, y)),digits)
   txt <- format(c(r, 0.123456789), digits = digits)[1]
   txt <- paste0(prefix, txt)
   if(missing(cex.cor)) cex.cor <- 0.7/strwidth(txt)
   text(0.5, 0.5, txt, cex = cex.cor )
}
# actual pairs plots
pairs(~ Fertility + Agriculture + Examination + Education + Catholic + Infant.Mortality + FC.adj, data=swiss2,
      diag.panel=panel.hist, upper.panel=panel.smooth, lower.panel=panel.cor,
  	  pch=21, bg=c("violetred2","skyblue")[unclass(swiss2$Catholic >50)+1],
	    main="Swiss data (high vs. low <-> Fertility vs. Catholic)")
 

# correlogram
# library(corrgram)
corrgram(swiss, order=TRUE,
    		 lower.panel=corrgram::panel.ellipse,
		     upper.panel=corrgram::panel.cor,
		     diag.panel=corrgram::panel.density,
    		 text.panel=corrgram::panel.txt,
		     main="Swiss data",
		     col.regions=colorRampPalette(c("darkgoldenrod4", "burlywood1","darkkhaki", "darkgreen"))
		    )


# additional perspectives for interested readers 
pairs(swiss, panel=panel.smooth, main="Swiss data from 1888", col=12+(swiss$Catholic > 50),
      upper.panel=panel.cor, diag.panel=panel.hist)
	  
pairs(~ Fertility + Agriculture + Examination + Education + Catholic + Infant.Mortality, data=swiss,
      diag.panel=panel.hist, upper.panel=panel.smooth, lower.panel=panel.cor,
  	  pch=21, bg=c("violetred2","skyblue")[unclass(swiss$Catholic >50)+1],
	    main="Swiss data (Catholic > 50)")
		 


