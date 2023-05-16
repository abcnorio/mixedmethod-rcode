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
# ptII_quan_EDA_case_Anderson_iris-species-in-biology.r

# location:
# chap. 5 [5.5.3]
# Spezies unterscheiden in der Biologie

# load necessary libs
library(MASS)

# load necessary helper functions
source("ptall_generalfuncs.r")


# iris data set
# by Anderson and made famous by Fisher
data(iris)
head(iris)
tail(iris)

# lda iris data
di <- dim(iris)
apply(iris[,-5],2,fivenum.wn)
table(iris$Species)


# plot

# no color
pairs(iris[1:4], main="Iris data set (from Edgar Anderson)", pch=21, bg="olivedrab")
# from ?pairs
# with color
pairs(iris[1:4], main="Iris data set (from Edgar Anderson)", pch=21, bg=c("violetred2", "greenyellow", "blue")[unclass(iris$Species)])


# lda
# create train vs. test data subset from iris
seed <- 5642
set.seed(seed)
train.ids <- sample(di[1], di[1]/2)
train.iris <- iris[train.ids,]
test.iris <- iris[-train.ids,]
table(train.iris$Species)
table(test.iris$Species)

# prior = 1/3 for each species
# all
lda.iris <- lda(Species ~ ., iris, prior=rep(1/3,3))
# train data set
trainlda.iris <- lda(Species ~ ., train.iris, prior=rep(1/3,3))
# predict train data set
pred.iris <- predict(object=trainlda.iris, newdata=test.iris)

# check
lda.iris
trainlda.iris 
str(pred.iris)

# all
table(REAL=(actual <- iris$Species), CLASSIFIED=(model.response(model.frame(lda.iris))))
# train data set
table(REAL=(actual <- train.iris$Species), CLASSIFIED=(model.response(model.frame(trainlda.iris))))
# predict for test data set - that is relevant
pred.tab <- table(REAL=(actual <- test.iris$Species), CLASSIFIED=(pred.iris$class))
pred.tab

# correct classified
cclass <- sum(diag(pred.tab))/sum(pred.tab)
# incorrect classified
wclass <- 1- cclass
cclass
wclass


# further alternative plots
pairs(lda.iris, pch=21, col=c("violetred2", "green3", "steelblue")[model.response(model.frame(lda.iris))])
pairs(trainlda.iris, pch=21, col=c("violetred2", "green3", "steelblue")[model.response(model.frame(trainlda.iris))])

# unfortunately not wrongly classified marked
pairs(trainlda.iris, pch=21, col=c("violetred2", "green3", "steelblue")[train.iris$Species])


# compare -- create tables
pred.iris.tab <- data.frame(pred.iris$x, test=test.iris$Species, pred=pred.iris$class)
comp.ids.wc <- with(pred.iris.tab, which(test != pred))
comp <- as.character(pred.iris.tab$test)
replacetext <- do.call(paste, c(pred.iris.tab[comp.ids.wc,c("test","pred")],sep=":"))
comp[comp.ids.wc] <- replacetext
pred.iris.tab <- data.frame(pred.iris.tab, comp)
pred.iris.tab
pred.iris.tab$compTF <- TRUE
pred.iris.tab$compTF[comp.ids.wc] <- FALSE
head(pred.iris.tab)
tail(pred.iris.tab)
pred.iris.tab[comp.ids.wc,]


# nice pairs plot
comp.fac <- as.factor(pred.iris.tab$comp)
levels(comp.fac)
bgcolo <- c("orange", "yellowgreen", "red", "steelblue","darkred")[comp.fac]
bocolo <- c("yellow", "yellow", "blue", "yellow","lightblue")[comp.fac]
pairs(pred.iris$x, pch=21, cex=1.7, col=bocolo, bg=bgcolo, oma=c(4,4,8,13), main="")
par(xpd=TRUE)
legend(0.87, 0.59, as.vector(levels(comp.fac)),
       fill=c("orange", "yellowgreen", "red", "steelblue","darkred"), title="LDA categorisation", cex=0.85, bty="n", title.col="black")
TITLE <- "Iris data set (from Edgar Anderson)"
SUB <- "LDA comparison"
mtext(TITLE, 3, line=2, cex=1.5)
mtext(SUB, 3, line=.4, cex=1.1)


# plot alone is sufficient
levels(comp.fac)
bgcolo <- c("orange", "yellowgreen", "red", "steelblue","darkred")[comp.fac]
bocolo <- c("yellow", "yellow", "blue", "yellow","lightblue")[comp.fac]
xlim <- range(pred.iris$x[,"LD1"])
ylim <- range(pred.iris$x[,"LD2"])
par(mar=c(5,5,4,9), oma=c(2,1,1,1), "cex.axis"=0.8)
plot(pred.iris$x, pre.panel=grid(), col.lab="violetred3", cex.lab=0.85, pch=21, cex=1.7, cex.lab=0.8, cex.axis=0.8, col=bocolo, bg=bgcolo, axes=F, main="", bty="n")
axis(side = 1, pretty(xlim), tck =- .02, labels=NA, line=.6)
axis(side = 2, pretty(ylim), tck = -.02, labels=NA, line=.6)
axis(side = 1, lwd = 0, line = .4)
axis(side = 2, lwd = 0, line = .4, las = 1)
mtext(TITLE, 3, line=2.5, cex=1.5)
mtext(SUB, 3, line=1, cex=1.1)
legend(12,1.2, as.vector(levels(comp.fac)), horiz=FALSE, xpd=TRUE,
       fill=c("orange", "yellowgreen", "red", "steelblue","darkred"), title="LDA categorisation", cex=0.85, bty="n", title.col="black")


# text plotten
fac <- 1.2
xlim <- range(pred.iris$x[,"LD1"])*fac
ylim <- range(pred.iris$x[,"LD2"])*fac
textplots <- pred.iris.tab$comp
par(mar=c(5,5,4,9), oma=c(2,1,1,1), "cex.axis"=0.8)
plot(0,0, xlim=xlim, ylim=ylim, pre.panel=grid(), xlab="", ylab="", pch=21, cex=1.7, cex.lab=0.8, cex.axis=0.8, col="white", bg="white", axes=F, main="", bty="n")
text(pred.iris$x, labels=pred.iris.tab[,"comp"], col=bgcolo) 
axis(side = 1, pretty(xlim), tck =- .02, labels=NA, line=.6)
axis(side = 2, pretty(ylim), tck = -.02, labels=NA, line=.6)
axis(side = 1, lwd = 0, line = .4)
axis(side = 2, lwd = 0, line = .4, las = 1)
mtext(TITLE, 3, line=2.5, cex=1.5)
mtext(SUB, 3, line=1, cex=1.1)
title(xlab="LD1",ylab="LD2", col.lab="violetred3", cex.lab=0.85)
legend(15,2.4, as.vector(levels(comp.fac)), horiz=FALSE, xpd=TRUE,
       fill=c("orange", "yellowgreen", "red", "steelblue","darkred"), title="LDA categorisation", cex=0.85, bty="n", title.col="black")

	   
# abbreviate names	   
abbreviate(as.vector(levels(comp.fac)))

# hclust
plot(hclust(dist(iris[,1:4])),labels=abbreviate(iris$Species,2), col="darkred")



# not run below this point - further plots and abbreviations
comp.irisW <- comp.iris <- as.character(pred.iris$class)
comp.ids.cc <- which(test.iris$Species == pred.iris$class)
comp.ids.wc <- which(test.iris$Species != pred.iris$class)
comp.iris[comp.ids.wc] <- toupper(test.iris$Species[comp.ids.wc])
comp.irisW[comp.ids.wc] <- rep("WRONG",length(comp.ids.wc))
pred.iris.tab <- data.frame(pred.iris.tab, comp.iris, comp.irisW)

bgcolo <- c("orange", "green3", "steelblue","red")[as.factor(pred.iris.tab$comp.irisW)]
bocolo <- c("yellow", "yellow", "yellow","blue")[as.factor(pred.iris.tab$comp.irisW)]


pairs(pred.iris$x, pch=21, cex=1.7, col=bocolo, bg=bgcolo, oma=c(4,4,8,13), main="")
par(xpd=TRUE)
legend(0.87, 0.59, as.vector(levels(comp.fac)),   #0.78,0.7 -> depends on device?
       fill=c("orange", "yellowgreen", "red", "steelblue","red"), title="LDA categorisation", cex=0.85, bty="n", title.col="black")
TITLE <- "Iris data set (from Edgar Anderson)"
SUB <- "LDA comparison"
mtext(TITLE, 3, line=2, cex=1.5)
mtext(SUB, 3, line=.4, cex=1.1)


# plot just what is wrongly categorized
plot(pred.iris$x, pch=21, cex=1.7, col=bocolo, bg=bgcolo, oma=c(4,4,8,4), main="", bty="n")
par(xpd=TRUE)
legend(2, 4, as.vector(levels(as.factor(pred.iris.tab$comp.irisW))),  
       fill=c("orange", "green3", "steelblue","red"), title="LDA categorisation", cex=0.85, bty="n", title.col="black")
TITLE <- "Iris data set (from Edgar Anderson)"
SUB <- "LDA comparison"
mtext(TITLE, 3, line=2, cex=1.5)
mtext(SUB, 3, line=.6, cex=1.1)

