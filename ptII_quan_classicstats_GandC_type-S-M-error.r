# file:
# ptII_quan_classicstats_GandC_type-S-M-error.r

# location:
# chap. 4 [4.5.2.2]
# Richtung und Größe — zwei unterschätzte Fehlertypen


#based on R-Code and ideas from
#http://andrewgelman.com/2014/11/17/power-06-looks-like-get-used/

#Gelman und Tuerlinckx (2000)
#http://www.stat.columbia.edu/~gelman/research/published/francis8.pdf

#Gelman und Carlin (2014)
#http://www.stat.columbia.edu/~gelman/research/published/PPS551642_REV2.pdf

#Alexander Etz 2015-05-21 [blog]
#https://alexanderetz.com/2015/05/21/type-s-and-type-m-errors/

#load necessary libs
library(Hmisc)

# load helper functions
source("ptII_quan_classicstats_GandC_type-S-M-error_helpfuncs.r")


#examples
# true effect size of 0.1, standard error 3.28, alpha=0.05  
retrodesign(tes=.1, se=3.28)

#Gelman and Carlin (2014, p.646)
#example
#Psychological Science
#Durante, Arsena, and Griskevicius (2013) 
#true effect size of 2, standard error 8.1, alpha=0.05  
tes <- 2
#d difference = empirical mean
d <- 17
#empirical reported two-sided p-value = 0.35
zvalue <- qnorm(1-0.035/2)
zvalue
#standard error of difference
#d/s = zvalue
SE <- d/zvalue
SE
#values based on literature recherche (tes) and empirical p-value
typsm.res.ref <- retrodesign(tes=tes, se=SE, graph=TRUE)
typsm.res.ref
#empirical values based on author's empirical results
retrodesign(tes=17, se=SE)
#what a difference!!!


#retrodesign power plotting
D.range <- seq(0,50,0.1)
#call
typsm.res <- plot.power.retrodesign(typsm.res.ref=typsm.res.ref, D.range=D.range, tes=tes, se=SE)
#pure "blind" empirically: tes = zvalue

max(typsm.res[typsm.res[,"typeS"]>0.1,"power"])
max(typsm.res[typsm.res[,"exaggeration"]>2,"power"])


#plot curves
plot.typ.sm(typsm.res.ref=typsm.res.ref, range.dist=c(-30,30), mw=0, emp.mw=d, tes=tes, se=SE)




#################################do not use below, copied from original website from ETZ

#gelman original
D_range <- c(seq(0,1,.01),seq(1,10,.1),100)
n <- length(D_range)  
power <- rep(NA, n)  
typeS <- rep(NA, n)
exaggeration <- rep(NA, n)
for (i in 1:n){
 a <- retrodesign(D_range[i], 1)
 power[i] <- a$power 
 typeS[i] <- a$typeS 
 exaggeration[i] <- a$exaggeration
}
#

#The first plot with the null value and the proposed true value
x <- seq(-35,35,.001) #set up for plotting the curve
y <- dnorm(x,0,8.1) #y values for plotting curve
plot(x=x,y=y, main="Type-S and Type-M error example", xlab="Estimated effect size", 
      ylab="Relative frequency", type="l", cex.lab=1.5, axes=F, col="white")
axis(1,at=seq(-30,30,10),pos=0) #make the axis nice
axis(2,at=seq(0,.05,.01),pos=-35,las=1) #make the axis nice
lines(c(0,0),c(0,.05),col="red",lwd=3) ##Add line at null value
lines(c(2,2),c(0,.05),col="blue",lwd=3) ##Add line at population mean
points(17, .001, pch=23, bg="grey",col="black",cex=1.5) ##Add sample mean

#######################################################################################################
##The second and third plots with the null sampling distribution and significance areas under the curve
x <- seq(-35,35,.001) #set up for plotting the curve
y <- dnorm(x,0,8.1) #y values for plotting curve
plot(x,y, main="Type-S and Type-M error example", xlab="Estimated effect size",
     ylab= "Relative frequency", type="l",cex.lab=1.5, las=1, lwd=3, axes = F)
axis(1,at=seq(-30,30,10),pos=0) #make the x axis nice
axis(2,at=seq(0,.05,.01),pos=-35,las=1) #make the y axis nice

lines(c(0,0),c(0,dnorm(0,0,8.1)),col="red",lwd=3) ##adds null line
lines(c(2,2),c(0,dnorm(2,0,8.1)),col="blue",lwd=3) ##adds true pop mean line
points(17, .001, pch=23, bg="grey",col="black",cex=1.5) ##adds sample mean

##Adds shaded area
cord.x <- c(-35, seq(-35,-15.9,.01),-15.9) ##set up for shading
cord.y <- c(0,dnorm(seq(-35,-15.9,.01),0,8.1),0) ##set up for shading
polygon(cord.x,cord.y,col='red') ##shade left tail
cord.xx <- c(35, seq(35,15.9,-.01),15.9) 
cord.yy <- c(0,dnorm(seq(35,15.9,-.01),0,8.1),0)
polygon(cord.xx,cord.yy,col='red') ##shade right tail
points(17, .001, pch=23, bg="grey",col="black",cex=1.5) ##replots the sample mean over the shading

#######################################################################################################
##The fourth plot with the alternative sampling distribution and significance areas under the curve
x <- seq(-35,35,.001) #set up for plotting the curve
y <- dnorm(x,2,8.1) #y values for plotting curve
plot(x,y, main="Type-S and Type-M error example", xlab="Estimated effect size",
     ylab= "Relative frequency", type="l", cex.lab=1.5, las=1, lwd=3, axes = F)
axis(1,at=seq(-30,30,10),pos=0) #make the x axis nice
axis(2,at=seq(0,.05,.01),pos=-35, las=1) #make the y axis nice

lines(c(0,0),c(0,dnorm(0,2,8.1)),col="red",lwd=3) ##add vertical line at null value
lines(c(2,2),c(0,dnorm(2,2,8.1)),col="blue",lwd=3) ##add vertical line at population mean

cord.x <- c(-35, seq(-35,-15.9,.01),-15.9) ##set up for shading
cord.y <- c(0,dnorm(seq(-35,-15.9,.01),2,8.1),0) ##set up for shading
polygon(cord.x,cord.y,col='red') ##shade left tail
cord.xx <- c(35, seq(35,15.9,-.01),15.9) 
cord.yy <- c(0,dnorm(seq(35,15.9,-.01),2,8.1),0)
polygon(cord.xx,cord.yy,col='red') ##shade right tail
points(17, .001, pch=23, bg="grey",col="black", cex=1.5) ##replots sample mean over shading

