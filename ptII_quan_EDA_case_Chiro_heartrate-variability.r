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
# ptII_quan_EDA_case_Chiro_heartrate-variability.r

# location:
# chap. 5 [5.5.6]
# Ein Experiment zur Herzratenvariabilität

# load necessary libs
library(robustlmm)
library(corrgram)
library(vcd)

# load necessary helper functions
source("ptall_generalfuncs.r")
source("ptII_quan_EDA_case_Chiro_heartrate-variability_helpfuncs.r")

# load data via stored environment
# not available online due to being a private data set
load("HW_FINAL_status-quo-161216_complete-environment.RData")


# rownames = treatment vs. control and timepoint
table(gd)

# select vars
relvars <- c("SDNNms","RMSSDms","SD2durchSD1")
daten.analy <- data.frame(daten.red[,relvars],log(daten.red[,"SD2durchSD1"]))
cnams <- c("SDNNms","RMSSDms","sd2/sd1","log(sd2/sd1)")
colnames(daten.analy) <- cnams
rownames(daten.analy) <- paste(as.character(gd),rownames(daten.analy),sep=".")
daten.analy

# correlations
cor.red.tab <- corp(daten.analy)
# correlations: -1 < r < +1
print(cor.red.tab$r, digits=2)
printcorp(cor.red.tab$p)

# corrgram
# library(corrgram)
# variables of interest
# file:	HW_Corrgram_variables-of-interest.emf
corrgram(daten.analy, lower.panel="panel.ellipse", upper.panel="panel.pie", diag.panel="panel.density",
          col.regions=colorRampPalette(pal), main="Corrgram", order=FALSE)



#### NOT RUN
# Cohens delta
cohensd2 <- function(x,y,n1=0,n2=0)
{
## Cohens d
 n1 <- length(x)
 n2 <- length(y)
 spooled <- sqrt( ((n1-1)*var(x) + (n2-1)*var(y)) / (n1+n2-2) )
 d <- (mean(x)-mean(y))/spooled
# Glass delta
# y must be control group!!!
 delta <- (mean(x)-mean(y))/sd(y)
 return(data.frame(cohensd=d,glassd=delta))
}
# group1 = treatment
# group2 = control
# d ~ treatment - control = diff from perspective of treatment
comps.trti <- combn(length(levels(gd)),2) # each 2er comparisons
comps.trti <- t(comps.trti)
cd.treattime1 <- list()
compnameN <- names(table(gd))
comparisons <- vector()
for(i in 1:nrow(comps.trti))
{
 compname <- paste(compnameN[comps.trti[i,2]],"-minus-",compnameN[comps.trti[i,1]],sep="")
 compname
 v1.all <- daten.analy[which(gd == compnameN[comps.trti[i,1]]),]
 v2.all <- daten.analy[which(gd == compnameN[comps.trti[i,2]]),]
 cnamen <- colnames(v1.all)
 stopifnot(cnamen == colnames(v2.all))
 tmp <- unlist(lapply(seq_along(v1.all),function(x)
 {
  # perspective >>> Cohens d: v2 - v1 = treat - control
  # d = x-y
  # comparison is col-2 (treat) minus col-1 (contol)
  # so    x = x = treat = v2
  # and   y = y = control = v1
  cohensd2(x=v2.all[,x], y=v1.all[,x])[2]  
 }
 ))
 cd.treattime1[[i]] <- tmp
 comparisons[i] <- compname
}
cd.treattime1 <- (do.call("rbind",cd.treattime1))
colnames(cd.treattime1) <- c(cnams)
rownames(cd.treattime1) <- comparisons
cd.treattime1
#### END OF NOT RUN



# Diffs MEAN and SD

#Cohens delta
#group1=treatment
#group2=control
#d ~ treatment - control = diff from perspective of treatment = group_1
cohensd <- function(mean1,var1,mean2,var2,n1,n2)
{
  spooled <- sqrt( ((n1-1)*var1 + (n2-1)*var2) / (n1+n2-2) )
  d <- (mean1-mean2)/spooled
  return(d)
}

MW.trti <- with(daten.analy, aggregate(daten.analy[,cnams], data.frame(gd), mean))
VAR.trti <- with(daten.analy, aggregate(daten.analy[,cnams], data.frame(gd), var))
SD.trti <- with(daten.analy, aggregate(daten.analy[,cnams], data.frame(gd), sd))
LE.trti <- with(daten.analy, aggregate(daten.analy[,cnams], data.frame(gd), length))
comps.trti <- combn(length(levels(gd)),2) # each 2er comparisons
comps.trti <- t(comps.trti)
cd.treattime <- list()

for(i in 1:nrow(comps.trti))
{
  compname <- paste(MW.trti[comps.trti[i,2],"gd"],"-minus-",MW.trti[comps.trti[i,1],"gd"],sep="")
  
  tmp <- cohensd(mean1=MW.trti[comps.trti[i,2],cnams],
                 var1=VAR.trti[comps.trti[i,2],cnams],
                 n1=LE.trti[comps.trti[i,2],cnams],
                 mean2=MW.trti[comps.trti[i,1],cnams],
                 var2=VAR.trti[comps.trti[i,1],cnams],
                 n2=LE.trti[comps.trti[i,1],cnams])
  
  cd.treattime[[i]] <- data.frame(comp=compname,tmp, check.names=FALSE)
}
cd.treattime <- (do.call("rbind",cd.treattime))
rownames(cd.treattime) <- cd.treattime[,"comp"]
cd.treattime <- cd.treattime[,2:5]
cd.treattime


# age x sex structure distribution
table(sex,age)
# library 'vcd'
mosaic(table(age,sex), shade=TRUE, legend=TRUE)

# all data
pal <- terrain.colors(4)
# diaglabel <- as.character(abbreviate(names(rawd),labbrev))
# library(corrgram)
corrgram(daten.cor, lower.panel="panel.ellipse", upper.panel="panel.pie", diag.panel="panel.density",
         col.regions=colorRampPalette(pal), main="Corrgram", order=FALSE)

# does not work, some error...
#corrgram(daten.cor, lower.panel="panel.cor", upper.panel="panel.pie", diag.panel="panel.density",
#         main="Corrgram", order=FALSE)


# histograms for sd2/sd1
# comparison no transfo, log(), sqrt()
par(mar=c(5,5,4,2), oma=c(2,1,1,1), cex.axis=0.8, mfrow=c(2,2))
hist(daten.analy[,"sd2/sd1"], prob=TRUE, main="", xlab="sd2/sd1", col="skyblue", border="white", pre.plot=grid())
lines(density(daten.analy[,"sd2/sd1"]), col="violetred2", lwd=2, lty=2)

sd2.sd1.log <- log(daten.analy[,"sd2/sd1"])
hist(sd2.sd1.log, prob=TRUE, main="", xlab="log(sd2/sd1)", col="skyblue", border="white", pre.plot=grid())
lines(density(sd2.sd1.log), col="violetred2", lwd=2, lty=2)

sd2.sd1.sqrt <- sqrt(daten.analy[,"sd2/sd1"]) 
hist(sd2.sd1.sqrt, prob=TRUE, main="", xlab="sqrt(sd2/sd1)", col="skyblue", border="white", pre.plot=grid())
lines(density(sd2.sd1.sqrt), col="violetred2", lwd=2, lty=2)

mtext("Study Wipfler / chiropractic (SD2/SD1)", 3, line=-1, cex=1.5, outer=TRUE)
mtext("Histogram and density", 3, line=-2.6, cex=1.1, outer=TRUE)


# vars and specs
colo <- c("violetred3","blue","yellowgreen","orange")
group.s <- factor(group, labels=c("C","T"))
time.sex <- paste(timepoint, sex, sep=" | ")
vars <- c("SDNNms","RMSSDms","sd2/sd1","log(sd2/sd1)")
facs <- data.frame(timepoint,sex,group,group.s,time.sex)
TITLE <- "Study Wipfler (interaction plot)"

# HW_interactionplot_xaxis-timepoint_yaxis-dependentvar_within-treatment_sd2sd1-log(sd2sd1)
# by treatment x timepoint
SUB <- "treatment (C/T) x timepoint (pre/post)"
ia.plot(dframe=daten.analy, facs=facs, f1nam="timepoint", f2nam="group.s", vars=vars, TITLE=TITLE, SUB=SUB, colo=colo, trace.label="group.s")

# by sex x treatment
SUB <- "treatment (C/T) x sex (m/f)"
ia.plot(dframe=daten.analy, facs=facs, f1nam="sex", f2nam="group.s", vars=vars, TITLE=TITLE, SUB=SUB, colo=colo, trace.label="group.s")

# by sex x timepoint
SUB <- "sex (m/f) x timepoint (pre/post)"
ia.plot(dframe=daten.analy,facs=facs,  f1nam="timepoint", f2nam="sex", vars=vars, TITLE=TITLE, SUB=SUB, colo=colo, trace.label="sex")

# HW_interactionplot_xaxis-treatment_yaxis-dependentvar_within-timepoint-sex
# by (timepoint x sex) x treatment
SUB <- "[timepoint (pre/post) x sex (m/f)] x treatment (C/T)"
ia.plot(dframe=daten.analy, facs=facs, f1nam="group", f2nam="time.sex", vars=vars, TITLE=TITLE, SUB=SUB, colo=colo, trace.label="time.sex")


