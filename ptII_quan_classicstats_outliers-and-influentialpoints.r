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
# ptII_quan_classicstats_outliers-and-influentialpoints.r

# location:
# chap. 4 [4.6.9.3]
# Ausreisser und einflussreiche Datenpunkte

# load necessary libs
library(car)
library(psych)
library(arm)

# load necessary helper functions
source("ptII_quan_classicstats_outliers-and-influentialpoints_helpfuncs.r")
source("ptall_generalfuncs.r")


# outlier and influential data points
seed <- 9876
set.seed(seed)
a <- c(1:20,100)
# add some noise
b <- a + rnorm(21,mean=5,sd=2)
data.frame(a, b=round(b,2))
# define outlier 21
weg21 <- c(21)
# simple plots
plot.outlier(a=a, b=b, weg=weg21)

plot(a.LP, b.LP, col="steelblue", panel.first=grid(), pch=21, cex=1.4, bg="skyblue", bty="l", main="", xlab="a", ylab="b")
abline(lm(b~a),col="red", lty=1, lwd=2)
#local regression
lines(lowess(b~a),col="darkblue",lty=2,lwd=2)

# with outlier w21, without leverage point w22
lmfit1 <- lm(b~a)
summary(lmfit1)
# without any outlier w21 or leverage point w22
lmfit2 <- lm(b[-c(weg21)]~a[-c(weg21)])
summary(lmfit2)
coef(lmfit1)
coef(lmfit2)

# descriptive statistics with and without outlier 21
# with outlier
describes(data.frame(a,b))
summary(data.frame(a,b))
# without outlier 21
describes(data.frame(a[-weg21],b[-weg21]))
summary(data.frame(a[-weg21],b[-weg21]))

# t-test unpaired with outlier
t.test(a,b)
# t-test unpaired without outlier 21
t.test(a[-weg21],b[-weg21])

# t-test paired with outlier
t.test(a,b,paired=TRUE)
# t-test paired without outlier 21
t.test(a[-weg21],b[-weg21],paired=TRUE)

# correlation with outlier
cor.test(a,b)
# correlation without outlier 21
cor.test(a[-weg21],b[-weg21])

# outlier test - for residuals
outlierTest(lmfit1)
outlierTest(lmfit2)

# leverage plots
leveragePlots(lmfit1, panel.first=grid())
leveragePlots(lmfit2, panel.first=grid())

# now with a real leverage point, "not an outlier"
a.LP <- c(a,80)
b.LP <- c(b,10)
ab.l <- length(a.LP)
# define leverage point 22
weg22 <- c(22)
# simple plots
plot.outlier(a=a.LP, b=b.LP, weg=weg22)


# plot various outliers and their influeces
#par(oma=c(2,1,1,1), "cex.axis"=1, bty="l", mfrow=c(1,1))
dev.off()
plot(a.LP, b.LP, col="steelblue", panel.first=grid(), pch=21, cex=1.4, bg="skyblue", bty="l", main="", xlab="a", ylab="b")
# outlier
points(a.LP[weg21],b.LP[weg21], col="black", pch=21, cex=1.4, bg="green")
# leverage point
points(a.LP[weg22],b.LP[weg22], col="black", pch=21, cex=1.4, bg="red")
# mark with text
text(a.LP[c(weg21,weg22)],b.LP[c(weg21,weg22)],c("21","22"), pos=1)

# complete with w22 outlier and w22 leverage point
lmfit3 <- lm(b.LP~a.LP)
summary(lmfit3)
abline(lmfit3,col="purple",lty=1, lwd=1)

# with outlier w21, without leverage point w22
abline(lmfit1,col="green",lty=2, lwd=1)
# without any outlier w21 or leverage point w22
abline(lmfit2,col="skyblue",lty=2, lwd=1)

# without outlier w21, with leverage point w22
lmfit4 <- lm(b.LP[-c(weg21)]~a.LP[-c(weg21)])
summary(lmfit4)
abline(lmfit4,col="red",lty=2, lwd=1)

mtext("Outliers and different influences", outer=TRUE, line=-2, cex=1.5, side=3)

par(fig=c(0,1,0,1), oma=c(1,0,0,0), mar=c(0,0,0,0), new=TRUE)
plot(1, type="n", bty="n", xaxt="n", yaxt="n")
legend("bottom", legend=c("complete", "w/o [22]", "w/o [21]", "w/o [21,22]"),
       lty=c(1,2,2,2), lwd=1, col=c("purple","green","red","skyblue"), bty="n", cex=.9, xpd=TRUE, horiz=TRUE)


# test autocorrelated errors
durbinWatsonTest(lmfit1)
durbinWatsonTest(lmfit2)
durbinWatsonTest(lmfit3)
durbinWatsonTest(lmfit4)

# compare linear models
display(lmfit1)
display(lmfit2)
display(lmfit3)
display(lmfit4)

# compare models with each other
# beta coefficient, but also intercept
comp.beta <- rbind(lmfit3$coef, lmfit1$coef, lmfit4$coef, lmfit2$coef)
rownames(comp.beta) <- c("complete", "w/o [22]", "w/o [21]", "w/o [21,22]")
colnames(comp.beta) <- c("(Intercept)","beta")

comp.se <- rbind(display(lmfit3)$se, display(lmfit1)$se, display(lmfit4)$se, display(lmfit2)$se)
rownames(comp.se) <- c("complete", "w/o [22]", "w/o [21]", "w/o [21,22]")
colnames(comp.se) <- c("(Intercept)SE","beta(SE)")

comp.beta
comp.se

# compare outier tests and don't trust them 'blindly'

# w/o [22]
outlierTest(lmfit1) # with outlier w21, without leverage point w22

# w/o [21,22]
outlierTest(lmfit2) # without any outlier w21 or leverage point w22

# complete
outlierTest(lmfit3) # complete with w21 outlier and w22 leverage point

# w/o [21]
outlierTest(lmfit4) # without outlier w21, with leverage point w22




