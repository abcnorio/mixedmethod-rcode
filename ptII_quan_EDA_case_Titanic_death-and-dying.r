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
# ptII_quan_EDA_case_Titanic_death-and-dying.r

# location:
# chap. 5 [5.5.4.6]
# Leben und Sterben auf der Titanic

# load necessary libraries
library(vcd)
#library(xlsx)
library(Hmisc)
library(corrgram)
library(MASS)
library(vcdExtra)
library(heatmap3)
library(car)
library(rockchalk)
library(arm)

# load necessary helper functions
source("ptall_generalfuncs.r")
source("ptII_quan_EDA_case_Titanic_death-and-dying_helpfuncs.r")


# TITANIC - death and dying explorative

# sources about the Titanic and associated information
# passenger list
# https://titanicfacts.net/titanic-passenger-list/
#
# https://www.historyonthenet.com/r-m-s-titanic/
# https://www.historyonthenet.com/wp-content/uploads/2014/08/MGY_Cutaway2.JPG
# https://en.wikipedia.org/wiki/RMS_Titanic
# http://www.ultimatetitanic.com/design/
# https://i.pinimg.com/originals/a1/03/ba/a103ba89965693be01953404d4b582df.jpg

# https://www.google.com/search?q=titanic+layout+ship&client=firefox-b&prmd=ivns&source=lnms&tbm=isch&sa=X&ved=0ahUKEwiZrYDypcXdAhXDIcAKHXNZA8EQ_AUIBQ

# https://www.youtube.com/watch?v=FSGeskFzE0s
# https://www.youtube.com/watch?v=c5cFoRLeVZw

# sinking in real time
# https://www.youtube.com/watch?v=rs9w5bgtJC8
# https://www.youtube.com/watch?v=nI8WvWI0_5s


# explanations data kaggle.com/c/titanic/data
# Variable	Definition	Key
# survival 	Survival 	0 = No, 1 = Yes
# pclass 	Ticket class 	1 = 1st, 2 = 2nd, 3 = 3rd
# sex 	Sex 	
# Age 	Age in years 	
# sibsp 	# of siblings / spouses aboard the Titanic 	
# parch 	# of parents / children aboard the Titanic 	
# ticket 	Ticket number 	
# fare 	Passenger fare 	
# cabin 	Cabin number 	
# embarked 	Port of Embarkation 	C = Cherbourg, Q = Queenstown, S = Southampton
# Variable Notes

# pclass: A proxy for socio-economic status (SES)
# 1st = Upper
# 2nd = Middle
#  3rd = Lower

# age: Age is fractional if less than 1. If the age is estimated, is it in the form of xx.5

# SibSp - Number of Sibling/Spouse on bord
# sibsp: The dataset defines family relations in this way...
# Sibling = brother, sister, stepbrother, stepsister
# Spouse = husband, wife (mistresses and fiancés were ignored)

# Parch - Number of Parent/Child on bord
# parch: The dataset defines family relations in this way...
# Parent = mother, father
# Child = daughter, son, stepdaughter, stepson
# Some children travelled only with a nanny, therefore parch=0 for them.

# https://en.wikipedia.org/wiki/File:Titanic_casualties.svg

# crew
# https://en.wikipedia.org/wiki/Crew_of_the_RMS_Titanic

# https://www.encyclopedia-titanica.org/?q=1
# https://titanicfacts.net/titanic-orchestra/

# fare for each class
# http://titanicdatabase.wikia.com/wiki/Passengers_of_the_RMS_Titanic

# https://titanicfacts.net/titanic-crew/

# dataset from R
?Titanic
Titanic

# more datasets
help.search("titanic")

# just have a look on the 'Titanic' dataset and forget this immediately!
# library(vcd)
mosaic(Titanic, shade=TRUE)
assoc(Titanic, shade=TRUE)

# everything there?
sum(Titanic)
# for classes and crew
apply(Titanic, c(1), sum)
# classes
sum(apply(Titanic, c(1), sum)[-4])

sum(Titanic)

#library(xlsx)
library(openxlsx)
#t.src <- read.xlsx("TITANIC_disaster.xlsx", "combwithvanderbilt")

# import data
t.src <- read.table(file="Titanic_data.tab", header=TRUE, sep="\t")
dim(t.src)
str(t.src)

# extract vars
sex <- t.src$sex
age <- as.numeric(t.src$age)
sibsp <- t.src$sibsp
parch <- t.src$parch
fare <- as.numeric(as.character(t.src$fare))
cabin <- t.src$cabin
embarked <- t.src$embarked
pclass <- t.src$pclass
survived <- t.src$survived
fullname <- as.character(t.src$fullname)
lastname <- as.character(t.src$lastname)
fulltitle <- as.character(t.src$title)
firstname <- as.character(t.src$firstname)
secondfirstname <- as.character(t.src$secondfirstname)

# create new var
famsize <- sibsp + parch + 1

# not run
table(pclass)
table(sex)
table(embarked)
table(survived)
table(sibsp)
table(parch)
table(sibsp.parch)
table(fulltitle)
describes(data.frame(age,fare))
# end of not run


# NA analysis
t.src.dim <- dim(t.src)
nas <- apply(data.frame(t.src,famsize),2, function(i) sum(is.na(i)))
nk <- 2
data.frame(nas,ratio=round(nas/t.src.dim[1]*100,nk))


# NA analysis of boat
boat <- as.character(t.src$boat)
boat[is.na(boat)] <- "NONE"
boat <- as.factor(boat)
str(boat)
table(boat,survived)


# analysis boat x pclass
table(boat, pclass)


# survived but no boat
survived.noboat.ids <- which(survived == TRUE & boat == "NONE")
length(survived.noboat.ids)
survived.noboat <- t.src[survived.noboat.ids,]
table(survived.noboat$pclass)


# to replace NAs for e.g. age, first investigate age structures for select subgroups
quantile(age, na.rm=TRUE)
quantile(age[sex=="male"], na.rm=TRUE)
quantile(age[sex=="female"], na.rm=TRUE)
quantile(age[sex=="female" & pclass=="Crew"], na.rm=TRUE)


# boxplot
bp <- boxplot(age ~ pclass + sex, plot=FALSE)
str(bp)
bp$names
bp$names <- paste(rep(c("1st","2nd","3rd","C"),length(bp$names)/4), rep(c("f","m"),each=length(bp$names)/2),sep=":")
TITLE <- "Titanic"
SUB <- "age ~ pclass + sex"
bxp(bp, notch=TRUE, ylab="age", xlab="class versus sex", main="", frame=FALSE,
    boxfill=rainbow(length(bp$names)/2), border=length(bp$names)/2)
mtext(TITLE, 3, line=2.5, cex=1.5)
mtext(SUB, 3, line=1, cex=1.1)


# base rates
tab.class.sex <- table(pclass,sex)
tab.class.sex[,2]/tab.class.sex[,1]


# NAs body
sum(is.na(t.src$body))/length(t.src$body)


# reduce titles
head(fulltitle)
t(table(sex,fulltitle))

upperclass.men <- c("Capt","Col","Commander","Don","Dr","Jonkheer","Lieutenant","Major","Rev","Sir","Sub-Lieutenant")
upperclass.women <- c("Dona","Dr","Lady","the Countess")
women.titles.young <- c("Miss","Mlle","Ms")
women.titles.mature <- c("Mme","Mrs")
men.titles <- c("Master","Mr") # we leave both as they are
fulltitle.red <- fulltitle
fulltitle.red[fulltitle.red %in% upperclass.women] <- "Lady" #before men due to "Dr" present in both strings
fulltitle.red[fulltitle.red %in% upperclass.men] <- "Sir"
fulltitle.red[fulltitle.red %in% women.titles.young] <- "Miss"
fulltitle.red[fulltitle.red %in% women.titles.mature] <- "Mrs"
table(fulltitle.red,sex)
table(fulltitle.red,survived)


# create index possible mother
table(sex == "female" & parch > 0 & age > 18)
table(sex == "female" & parch > 0 & age > 18 & fulltitle.red != "Miss")
possible.mother <- sex == "female" & parch > 0 & age > 18 & fulltitle.red != "Miss"
table(possible.mother,sex)


# cross-checks that variable was created properly
sum(possible.mother[parch < 1],na.rm=TRUE)
sum(possible.mother[age < 19],na.rm=TRUE)
sum(possible.mother[fulltitle.red == "Miss"],na.rm=TRUE)
sum(possible.mother[sex == "male"],na.rm=TRUE)


# more mothers survived?
table(possible.mother,survived)
round(prop.table(table(possible.mother,survived))*100,nk)
round(prop.table(table(possible.mother,survived),m=1)*100,nk)
round(prop.table(table(possible.mother,survived),m=2)*100,nk)


# create child index truth table
child <- age < 18 
table(child,sex)


# sanity check
(possible.mother & age < 18) + 0


# create senior index
oldage <- age > 60
table(oldage,sex)


# age ranges
# create asymmetric but content relevant age dimensions
quantile(age,probs=seq(0,1,.1),na.rm=T)
quantile(age,probs=seq(0,1,.05),na.rm=T)
# child - young adult - midterm + senior
maxx <- max(age)
age.bin.1 <- factor(.bincode(age, breaks=c(0,17,31,61, maxx), right=TRUE, include.lowest=TRUE))
levels(age.bin.1) <- c("child","youngadult","adult","senior")
age.bin.1 <- factor(age.bin.1)
table(age.bin.1,sex)


# not run
xxx <- table(age.bin.1,survived)
xxx
data.frame(survival.ratio=(xxx[,2]/xxx[,1]))

# child - youth/ puberty - adult - midterm + senior
age.bin.2 <- factor(.bincode(age, breaks=c(0,11,18,31,61, maxx), right=TRUE, include.lowest=TRUE))
levels(age.bin.2) <- c("child","youth","youngadult","adult","senior")
age.bin.2 <- factor(age.bin.2)
table(age.bin.2,sex)
xxx <- table(age.bin.2,survived)
xxx
data.frame(survival.ratio=(xxx[,2]/xxx[,1]))

# baby + small child - child - youth/ puberty - adult - midterm + senior
age.bin.3 <- factor(.bincode(age, breaks=c(0,6,11,18,31,61, maxx), right=TRUE, include.lowest=TRUE))
levels(age.bin.3) <- c("baby","child","youth","youngadult","adult","senior")
age.bin.3 <- factor(age.bin.3)
table(age.bin.3,sex)
xxx <- table(age.bin.3,survived)
xxx
data.frame(survival.ratio=(xxx[,2]/xxx[,1]))
# end of not run


# create index family name + family size
family.IDsize <- paste(lastname,famsize,sep=":")
head(sort(table(family.IDsize),dec=TRUE))
tail(sort(table(family.IDsize),dec=TRUE))


# create adjusted family size variable for people sharing cabins but not registered as family members
noccur <- data.frame(table(cabin))
head(noccur)
tail(noccur)
dim(noccur)
# remove if only letter is known, but no cabin number...
noccur.cc <- subset(noccur, nchar(as.character(cabin)) > 1)
dim(noccur.cc)

sharedcabins <- noccur.cc$cabin[noccur.cc$Freq > 1]
# replace shared cabins but freq=1 by freq=2 for famsize.adj
famsize.adj <- famsize
sharedcabins.id <- which((famsize.adj == 1) & (cabin %in% sharedcabins))
sum(length(sharedcabins.id))
# check whether really n=1
sum(famsize.adj[sharedcabins.id] != 1)
famsize.adj[sharedcabins.id] <- 2
table(famsize,sex)
table(famsize.adj,sex)
# for staff use family size = 1 (=one, i.e. alone with oneself)
length(famsize.adj[pclass=="Crew"])
is.na.ids <- which(is.na(famsize.adj))
famsize.adj[is.na.ids] <- 1
table(famsize.adj)
sum(is.na(famsize.adj))


# create small family size index
maxx <- max(as.numeric(names(table(famsize.adj))))


# 0 | 1 alone | 2 duo | group 3+4 | >=5
travelno.bin.1 <- factor(.bincode(famsize.adj, breaks=c(0,1,2,4,maxx), right=TRUE, include.lowest=TRUE))
levels(travelno.bin.1) <- c("alone","duo","group3+4","group>4")
travelno.bin.1 <- factor(travelno.bin.1)
table(travelno.bin.1,sex)


# check for survival and famsize
round(prop.table(table(travelno.bin.1))*100,nk)
table(travelno.bin.1,survived)
round(prop.table(table(travelno.bin.1,survived))*100,nk)
round(prop.table(table(travelno.bin.1,survived),m=1)*100,nk)
round(prop.table(table(travelno.bin.1,survived),m=2)*100,nk)


# drop crew who did not pay but was paied
fare.p <- fare[fare > 0]
describes(fare.p)
quantile(fare.p,probs=seq(0,1,.1),na.rm=T)


# https://www.kaggle.com/rahulpalnitkar/titanic-eda-with-basic-predictive-modelling/notebook
# bins: 0 - 0.7854 - 10.5 - 21.679 - 39.688 - 512.329
maxx <- max(fare, na.rm=TRUE)
fare.bin.1 <- factor(.bincode(fare, breaks=c(0,1,8,12,30,maxx), right=TRUE, include.lowest=TRUE))
levels(fare.bin.1) <- c("Crew","Third","Second","First","Suite")
fare.bin.1 <- factor(fare.bin.1)
table(fare.bin.1)
table(fare.bin.1,sex)
table(fare.bin.1,pclass)


# number of categories
length(table(fare))
sum(fare[pclass != "Crew" & t.src$crew != "guarantee.group"] == 0, na.rm=TRUE)
sum(fare[child == TRUE] == 0, na.rm=TRUE)
sum(fare[child == TRUE] != 0, na.rm=TRUE)


# table analyses
structable(age.bin.1 ~ pclass + sex, split_vertical = c(TRUE, TRUE, FALSE, FALSE))

addmargins(table(pclass,sex))
addmargins(table(age.bin.1,sex))
addmargins(table(embarked,sex))

# survival
mastertable <- table(pclass, age.bin.1, sex, survived)
ftable(mastertable)

# is identical to
mastertable
apply(mastertable,c(1,2,3,4),sum)

# identical to
margin.table(mastertable)
sum(mastertable)
sum(apply(mastertable,1,sum))

# summed over var 1 in the order of appearance str(mastertable)
margin.table(mastertable,1) # pclass
margin.table(mastertable,2) # age.bin.1
margin.table(mastertable,3) # sex 
margin.table(mastertable,4) # survived

# summed over var 1 and then var 2
ftable(apply(mastertable,c(1,4),sum)) # pclass x survival
ftable(apply(mastertable,c(2,4),sum)) # age.bin.1 x survival
ftable(apply(mastertable,c(3,4),sum)) # sex x survival

# summed over var 1 and then var 2 and then var 3
ftable(apply(mastertable,c(1,2,4),sum)) # pclass x age.bin.1 x survival
ftable(apply(mastertable,c(1,3,4),sum)) # pclass x sex x survival

# survial rates, no more sums
op.orig <- options(digits=2)
tx <- table(sex,survived)
MARGIN <- 1
sweep(tx, MARGIN, margin.table(tx, MARGIN), "/", check.margin=FALSE)
sweep(tx, MARGIN, apply(tx, MARGIN,sum), "/", check.margin=FALSE)
prop.table(table(sex,survived),m=1)

prop.table(table(sex))
prop.table(table(sex,survived))

prop.table(table(sex,survived),m=1)
apply(prop.table(table(sex,survived),m=1),1,sum)
prop.table(table(sex,survived),m=2)
apply(prop.table(table(sex,survived),m=2),2,sum)


# further tables
# analysis based on two variables
prop.table(table(sex,survived))
prop.table(table(sex,survived),m=1)
prop.table(table(sex,survived),m=2)

prop.table(table(pclass,survived))
prop.table(table(pclass,survived),m=1)
prop.table(table(pclass,survived),m=2)

prop.table(table(age.bin.1,survived))
prop.table(table(age.bin.1,survived),m=1)
prop.table(table(age.bin.1,survived),m=2)

# analysis based on three variables
ftable(prop.table(table(pclass,sex,survived)))
ftable(prop.table(table(pclass,sex,survived),m=1)) # pclass x survival
ftable(prop.table(table(pclass,sex,survived),m=2)) # sex x survival

# analysis based on two variables - but table contains four variables
ftable(prop.table(table(pclass,age.bin.1,sex,survived),m=c(1,4))) # pclass x survival
ftable(prop.table(table(pclass,age.bin.1,sex,survived),m=c(2,4))) # age.bin.1 x survival
ftable(prop.table(table(pclass,age.bin.1,sex,survived),m=c(3,4))) # sex x survival

# analysis based on three variables - but table contains four variables
ftable(prop.table(table(pclass,age.bin.1,sex,survived),m=c(1,2,4))) # pclass x age.bin.1 x survival
ftable(prop.table(table(pclass,age.bin.1,sex,survived),m=c(1,3,4))) # pclass x sex x survival
ftable(prop.table(table(pclass,age.bin.1,sex,survived),m=c(2,3,4))) # age.bin.1 x sex x survival

# more difficult to understand, but contrasts each possible sub-combination for survival TRUE vs. FALSE
ftable(prop.table(table(pclass,age.bin.1,sex,survived),m=c(1,2,3))) # pclass x age.bin.1 x sex


# possible mothers
table(possible.mother)
prop.table(table(possible.mother))
prop.table(table(possible.mother,survived))
prop.table(table(possible.mother,survived),m=1)
prop.table(table(possible.mother,survived),m=2)

# traveling alone versus in group
table(famsize.adj)
prop.table(table(famsize.adj))
prop.table(table(famsize.adj,survived))
prop.table(table(famsize.adj,survived),m=1)
prop.table(table(famsize.adj,survived),m=2)

# reverse options (digits)
options(op.orig)

# power set count
m <- 4
potlist <- list()
for(i in 1:m) potlist[[i]] <- combn(m,i)
potlist 

str(potlist)


# graphical analyses
survived.TF <- factor(survived, labels=c("F","T"))
sex.FM <- factor(sex, labels=c("F","M"))
child.AC <- factor(child, labels=c("A","C"))

doubledecker(survived.TF ~ pclass + sex.FM + child.AC, gp=gpar(fill=c("violetred3","greenyellow")) )

mosaic(~ survived.TF + pclass + sex.FM + child.AC,  pop=FALSE,
       shade=TRUE, legend=TRUE, split_vertical=c(TRUE,TRUE,FALSE,FALSE),gp=shading_hcl,
	   gp_args=list(h=c(130,43), c=100, l=c(90,70)))


# hist plot
hist.titanic(daten=age, TITLE="Titanic dataset", SUB="age (separated by surviving status)",
             xaxtext="age")

hist.titanic(daten=fare.p, TITLE="Titanic dataset", SUB="fare (separated by surviving status)",
             xaxtext="fare.p (fare > 0)")

hist.titanic(daten=fare.p[fare.p <100], TITLE="Titanic dataset", SUB="fare (separated by surviving status)",
             xaxtext="fare.p (0 < fare < 100)")

#add values to cells/ tiles
mosaic(~ survived.TF + pclass + sex.FM + child.AC,  pop=FALSE, labeling=labeling_residuals,
       shade=TRUE, legend=TRUE, gp=shading_hcl,
	   gp_args=list(h=c(130,43), c=100, l=c(90,70)))
	   
mosaic(~ survived.TF + pclass + sex.FM + child.AC,  pop=FALSE, labeling=labeling_residuals,
       shade=TRUE, legend=TRUE, split_vertical=c(TRUE,TRUE,FALSE,FALSE),gp=shading_hcl,
	   gp_args=list(h=c(130,43), c=100, l=c(90,70)))

age.bin.1.CYAAS <- factor(age.bin.1, labels=c("C","YA","A","S"))
mtab <- table(pclass, age.bin.1, sex.FM, survived.TF)	   
mosaic(mtab,pop=FALSE)
labeling_cells(text=mtab, margin=0)(mtab)

stab <- structable(survived.TF ~ child.AC + sex.FM + pclass)
stab
tab.prop.sex <- round(prop.table(table(pclass,sex.FM,survived.TF),m=c(2,3))*100,nk) #sex
ftable(tab.prop.sex)

mosaic(stab, pop=FALSE, shade=TRUE, legend=TRUE, gp=shading_hcl, gp_args=list(h=c(130,43), c=100, l=c(90,70)))
labeling_cells(text=tab.prop.sex)(stab)


# tables for final discussion
prop.table(table(survived))

# travelers alone vs. group
fsize.tab <- prop.table(table(famsize.adj))
fsizeXsurv.tab <- prop.table(table(famsize.adj,survived),m=2)
fsize.tab
fsizeXsurv.tab
# survival impact factor
sort(fsizeXsurv.tab[,"TRUE"]/fsize.tab,dec=TRUE)
# not run: death impact factor
sort(fsizeXsurv.tab[,"FALSE"]/fsize.tab,dec=TRUE)
# one liner
# sort(prop.table(table(famsize.adj,survived),m=2)[,2]/prop.table(table(famsize.adj)),dec=TRUE)


# example survival 1st vs 2nd class
table(pclass,survived)
prop.table(table(pclass))
prop.table(table(pclass,survived),m=2)
prop.table(table(pclass,survived),m=1)


##############################################
# not run
# tables

# expected
# .8 = how many places used for each boat

saved.exp <- 2208 - 1178*(18/20) * .8
saved.exp
saved.exp/2208*100
2208-saved.exp
# real places used
(1178*18/20 - 702)/1178*100 #702=survived

# real

# variables alone
table(survived)
prop.table(table(survived))

table(sex)
prop.table(table(sex))

table(pclass)
prop.table(table(pclass))

table(child)
prop.table(table(child))

table(famsize.adj)
prop.table(table(famsize.adj))

table(embarked)
prop.table(table(embarked))

table(age.bin.1)
prop.table(table(age.bin.1))

# use travelno.bin.1 instead of travelno.1 (not available....)
table(travelno.bin.1)
prop.table(table(travelno.bin.1))

table(fare.bin.1)
prop.table(table(fare.bin.1))


# variables alone
table(sex, pclass)
prop.table(table(sex, pclass))
prop.table(table(sex, pclass), m=1)
prop.table(table(sex, pclass), m=2)

table(child, sex)
prop.table(table(child, sex))
prop.table(table(child, sex), m=1)
prop.table(table(child, sex), m=2)

table(sex, age.bin.1)
prop.table(table(sex, age.bin.1))
prop.table(table(sex, age.bin.1), m=1)
prop.table(table(sex, age.bin.1), m=2)

table(sex, travelno.bin.1)
prop.table(table(sex, travelno.bin.1))
prop.table(table(sex, travelno.bin.1), m=1)
prop.table(table(sex, travelno.bin.1), m=2)

table(sex, famsize.adj)
prop.table(table(sex, famsize.adj))
prop.table(table(sex, famsize.adj), m=1)
prop.table(table(sex, famsize.adj), m=2)

table(sex, pclass, travelno.bin.1)
prop.table(table(sex, pclass, travelno.bin.1))
prop.table(table(sex, pclass, travelno.bin.1), m=1)
prop.table(table(sex, pclass, travelno.bin.1), m=2)

table(sex, pclass, age.bin.1)
prop.table(table(sex, pclass, age.bin.1))
prop.table(table(sex, pclass, age.bin.1), m=1)
prop.table(table(sex, pclass, age.bin.1), m=2)

addmargins(table(sex, pclass, age.bin.1, travelno.bin.1))
addmargins(ftable(sex, pclass, age.bin.1, travelno.bin.1))
prop.table(ftable(sex, pclass, age.bin.1, travelno.bin.1))
prop.table(ftable(sex, pclass, age.bin.1, travelno.bin.1), m=1)
prop.table(ftable(sex, pclass, age.bin.1, travelno.bin.1), m=2)

ftable(pclass, age.bin.1, travelno.bin.1)
prop.table(ftable(pclass, age.bin.1, travelno.bin.1))
prop.table(ftable(pclass, age.bin.1, travelno.bin.1), m=1)
prop.table(ftable(pclass, age.bin.1, travelno.bin.1), m=2)


# survival rates
ftable(survived, sex)
ftable(survived, pclass)
ftable(survived, child)
ftable(survived, age.bin.1)
prop.table(addmargins(table(survived, travelno.bin.1)))

prop.table(addmargins(table(survived,child,pclass)))

ftable(survived, sex, travelno.bin.1, age.bin.1, pclass)
prop.table(ftable(survived, sex, travelno.bin.1, age.bin.1, pclass), m=1)
prop.table(ftable(survived, sex, travelno.bin.1, age.bin.1, pclass), m=2)

ftable(survived, sex, travelno.bin.1, child, pclass)
prop.table(ftable(survived, sex, child, age.bin.1, pclass), m=1)
prop.table(ftable(survived, sex, child, age.bin.1, pclass), m=2)


# mastertable <- table(pclass, sex, child, survived, embarked)
# child.AC <- factor(child, labels=c("A","C")) #a=adult, c=child
sex.FM <- factor(sex, labels=c("F","M"))
sex.FM
mastertable <- table(pclass, child.AC, sex.FM, survived)
mastertable

margin.table(mastertable)
# summed over var 1 in the order of appearance str(mastertable)
margin.table(mastertable,1) #pclass #1
margin.table(mastertable,2) #child #2
margin.table(mastertable,3) #sex #3 
margin.table(mastertable,4) #survived #5
# summed over var 1 and then var 2
margin.table(mastertable,c(1,2)) #pclass x child
margin.table(mastertable,c(1,3,4)) #pclass x sex x survived
margin.table(mastertable,c(1,2,3,4)) #pclass x child x sex x survived

prop.table(mastertable)
prop.table(mastertable, m=1) # row %
prop.table(mastertable, m=2) # col %

aggregate(age ~ sex, FUN=function(x) sum(x)/length(x))
aggregate(age ~ child.AC + sex + pclass + survived, FUN=function(x) sum(x)/length(x))
format(aggregate(fare ~ pclass, FUN=function(x) sum(x)/length(x)), justify="left", digits=nk+2)


# hypotheses

#- children and women better than man in dependance of class
prop.table(ftable(survived, sex, child.AC, pclass), m=1) #with age & sex but between classes
prop.table(ftable(survived, sex, child.AC, pclass), m=2) #between age & sex but within classes

#- classes
prop.table(ftable(survived, sex, pclass), m=1)

structable(sex + pclass + child.AC ~ survived)
structable(sex + survived ~ pclass + child.AC)
structable(sex + survived ~ pclass + child.AC, split_vertical = c(TRUE, TRUE, FALSE, FALSE))
structable(sex + survived ~ pclass + child.AC, direction = c("h","h","v","v"))

structable(sex ~ pclass + age.bin.1)


# beware of the base rates ...
# Simpson Paradox
prop.table(table(sex,age.bin.1))
prop.table(table(sex,age.bin.1),m=1)
prop.table(table(sex,age.bin.1),m=2)

prop.table(table(sex,pclass))
prop.table(table(sex,pclass),m=1)
prop.table(table(sex,pclass),m=2)
# absolute numbers or relative?

# cabin split
cabin.split <- sapply(as.character(cabin),function(x) strsplit(x,NULL)[[1]][1])
cabin.split
# too many NAs to use something meaningful!!!
sum(is.na(cabin))/length(cabin)*100
table(cabin.split)

data.frame(pclass,cabin.split)
class.cabin.tab <- table(paste(as.character(pclass),as.character(cabin.split),sep=":"))
class.cabin.tab[sort(names(class.cabin.tab),dec=TRUE)]
table(pclass)


#0 | 1 alone | 2 duo + group 3+4 | >=5
travelno.2 <- factor(.bincode(famsize.adj, breaks=c(0,1,4,maxx), right=TRUE, include.lowest=TRUE))
levels(travelno.2) <- c("alone","duo+group3+4","group>4")
travelno.2 <- factor(travelno.2)
xxx <- table(travelno.2,survived)
xxx
data.frame(survival.ratio=(xxx[,2]/xxx[,1]))

#0 | 1alone | 2duo + group 3 | group4 | >=5
travelno.3 <- factor(.bincode(famsize.adj, breaks=c(0,1,3,4,maxx), right=TRUE, include.lowest=TRUE))
levels(travelno.3) <- c("alone","duo+group3","group4","group>4")
travelno.3 <- factor(travelno.3)
xxx <- table(travelno.3,survived)
xxx
data.frame(survival.ratio=(xxx[,2]/xxx[,1]))

# alone travelers
sum(sibsp==0 & parch==0,na.rm=TRUE)
# equal to
sum((famsize-1) == 0, na.rm=TRUE)
travel.no <- as.numeric(names(table(famsize.adj)))
travel.no
# travelers: 1 - 2 - 3/4 - >4/5+
travelno.4 <- factor(.bincode(famsize.adj, breaks=c(0,1,2,4,maxx), right=TRUE, include.lowest=TRUE))
levels(travelno.4) <- c("alone","duo","group<5","group>4")
travelno.4 <- factor(travelno.4)
xxx <- table(travelno.4,survived)
xxx
data.frame(survival.ratio=(xxx[,2]/xxx[,1]))


# fare
# https://titanicfacts.net/life-on-the-titanic/
# $150 – the average cost of a First Class berth on the Titanic (£30); a First Class parlour suite on the other hand cost $4,350 (£875).
# $60 – the average cost of a Standard Class berth (£12).
# $15-$40 – the average cost of a Third class berth (£3-£8).

# https://www.quora.com/What-were-the-ticket-prices-to-board-the-Titanic
# https://www.quora.com/topic/RMS-Titanic-ship
#    First Class (parlor suite) — £870/$4,350 ($50,000 today).
#    First Class (berth)— £30/$150 ($1724 today).
#    Second Class — £12/$60 ($690 today).
#    Third Class — £3 to £8/$40 ($172 to $460 today).

# end of not run



# graphical analyses
fac <- 1.05
fare.dens <- density(fare.p, na.rm=TRUE)
hist(fare.p, col="steelblue", border="white", prob=TRUE, main="Titanic (fare)", panel.first=grid(), ylim=range(fare.dens$y)*fac, breaks="FD")
lines(fare.dens, lty=1, lwd=2, col="violetred2")

fac <- 1.05
age.dens <- density(age, na.rm=TRUE)
hist(age, col="steelblue", border="white", prob=TRUE, main="Titanic (age)", panel.first=grid(), ylim=range(age.dens$y)*fac, breaks="FD")
lines(age.dens, lty=1, lwd=2, col="violetred2")

hist.titanic(daten=age, TITLE="Titanic dataset", SUB="age (separated by surviving status)", xaxtext="age")
hist.titanic(daten=fare.p, TITLE="Titanic dataset", SUB="fare (separated by surviving status)", xaxtext="fare.p (fare > 0)")
hist.titanic(daten=fare.p[fare.p <150], TITLE="Titanic dataset", SUB="fare (separated by surviving status)", xaxtext="fare.p (0 < fare < 150)")
hist.titanic(daten=fare.p[fare.p <100], TITLE="Titanic dataset", SUB="fare (separated by surviving status)", xaxtext="fare.p (0 < fare < 100)")

 
# mosaicplot survivor and gender and class
TITLE <- c("Titanic and surviving...")
SUB <- c("Influence of sex")
mcolos <- c("greenyellow","orange","darkred","violetred2","olivedrab")

mosaicplot(~ sex + survived, color=mcolos, las=1, main="", sub="")
mtext(TITLE, 3, line=2, cex=1.5)
mtext(SUB, 3, line=.7, cex=1.1)
 
SUB <- c("Influence of class")
mosaicplot(~ pclass + survived, color=mcolos, las=1, main="", sub="")
mtext(TITLE, 3, line=2, cex=1.5)
mtext(SUB, 3, line=.7, cex=1.1)
 
SUB <- c("Influence of sex + class")
mosaicplot(~ sex + pclass + survived, color=mcolos, shade=TRUE, las=1, main="", sub="")
mtext(TITLE, 3, line=2, cex=1.5)
mtext(SUB, 3, line=.7, cex=1.1)

mosaic(~ sex + pclass + survived, color=mcolos, shade=TRUE, legend=TRUE, las=1, main="", sub="")
mtext(TITLE, 3, line=2, cex=1.5)
mtext(SUB, 1, line=1.7, cex=1.1)

ftable(sex, pclass, survived)
assoc(ftable(sex, pclass, survived) , color=mcolos, shade=TRUE, legend=TRUE, las=1, main=TITLE, sub=SUB)

SUB <- c("Influence of family size")
mosaic(table(famsize.adj,survived), shade=TRUE, legend=TRUE)
mtext(TITLE, 3, line=2, cex=1.5)
mtext(SUB, 1, line=1.7, cex=1.1)

mosaic(table(sex,child,survived,pclass), shade=TRUE, legend=TRUE)
mosaic(table(pclass,sex,child,survived), shade=TRUE, legend=TRUE)

SUB <- c("Influence of sex and class")
mosaic(~sex+pclass, data=table(pclass,sex,child,survived), shade=TRUE, legend=TRUE)
mtext(TITLE, 3, line=2, cex=1.5)
mtext(SUB, 1, line=1.7, cex=1.1)

mastertable
mosaic(~ pclass + sex.FM + child.AC + survived, data=mastertable, shade=TRUE, legend=TRUE)
mosaic(~ pclass + sex.FM + child.AC + survived, data=mastertable, shade=TRUE, legend=FALSE)
mosaic(~ pclass + sex.FM + child.AC + survived, data=mastertable, shade=FALSE, legend=TRUE)
mosaic(~ pclass + sex.FM + child.AC + survived, data=mastertable, shade=TRUE, legend=TRUE, gp=shading_Friendly) #fixed cutoffs +-2/+-4 for individual significant cells at 5%/1%
mosaic(~ pclass + sex.FM + child.AC + survived, data=mastertable, highlighting="sex.FM")

mosaic(~ pclass + sex.FM + survived, data=mastertable, shade=TRUE, legend=TRUE)
# colors
# Hue (h), Chroma (c), Luminance (l)
mosaic(~ pclass + sex.FM + survived, data=mastertable, shade=TRUE, legend=TRUE, gp=shading_hcl, gp_args=list(h=c(130,43), c=100, l=c(90,70)))


# output text
# labeling with residuals
mosaic(mastertable,pop=FALSE,labeling=labeling_residuals, shade=TRUE, legend=TRUE, gp=shading_hcl, gp_args=list(h=c(130,43), c=100, l=c(90,70)))
mosaic(~ survived + pclass + sex + child,pop=FALSE,labeling=labeling_residuals, shade=TRUE, legend=TRUE, split_vertical=c(TRUE,TRUE,FALSE,FALSE),gp=shading_hcl, gp_args=list(h=c(130,43), c=100, l=c(90,70)))

# labeling with original freqs
mosaic(mastertable,pop=FALSE)
labeling_cells(text=mastertable, margin=0)(mastertable)

# according to selected variables
survived.alt <- factor(survived, labels=c("N","Y"))
# child.AC <- factor(child, labels=c("A","C"))
# stab <- structable(survived.alt ~ pclass + child.alt + sex)
stab <- structable(survived.alt ~ child.AC + sex + pclass)
stab
tab.prop.sex <- round(prop.table(table(pclass,sex,survived.alt),m=c(2,3))*100,nk) #sex
tab.prop.sex

mosaic(stab, pop=FALSE, shade=TRUE, legend=TRUE, gp=shading_hcl, gp_args=list(h=c(130,43), c=100, l=c(90,70)), split=TRUE)
labeling_cells(text=tab.prop.sex)(stab)

mosaic(stab, pop=FALSE, shade=TRUE, legend=TRUE, gp=shading_hcl, gp_args=list(h=c(130,43), c=100, l=c(90,70)), split=FALSE)
labeling_cells(text=tab.prop.sex)(stab)

assoc(stab, pop=FALSE, shade=TRUE, legend=TRUE, gp=shading_hcl,
      gp_args=list(h=c(130,43), c=100, l=c(90,70)),
      split=TRUE, keep=TRUE)

assoc(stab, pop=FALSE, shade=TRUE, legend=TRUE, gp=shading_hcl,
      gp_args=list(h=c(130,43), c=100, l=c(90,70)),
      split=FALSE, keep=TRUE)

cotabplot(stab)

mosaic(~ pclass + sex.FM + survived, data=mastertable, pop=FALSE)


# colors and numbers/ freqs in cells
tab.struct0 <- structable(survived ~ pclass + sex + child)
mosaic(tab.struct0, pop=FALSE, shade=TRUE, legend=TRUE, gp=shading_hcl, gp_args=list(h=c(130,43), c=100, l=c(90,70)))
labeling_cells(text=tab.struct0, margin=0)(tab.struct0) #margin=0 for no numbers if too small the tile

# overall
round(prop.table(tab.struct0)*100,nk)
round(prop.table(as.table(tab.struct0),m=c(1,2))*100,nk)
round(prop.table(as.table(tab.struct0),m=1)*100,nk)
round(prop.table(as.table(tab.struct0),m=2)*100,nk)

# all
tab.struct1 <- structable(survived ~ pclass + sex + child)
mosaic(tab.struct1, pop=FALSE, shade=TRUE, legend=TRUE, gp=shading_hcl, gp_args=list(h=c(130,43), c=100, l=c(90,70)))
labeling_cells(text=tab.struct1)(tab.struct1)

# according to selected variables
mtable <- table(pclass,sex,survived)
mtable
tab.struct2 <- structable(survived ~ pclass + sex + child)
tab.prop.sex <- round(prop.table(mtable,m=2)*100,nk) #sex
tab.prop.sex
mosaic(tab.struct2, pop=FALSE, shade=TRUE, legend=TRUE, gp=shading_hcl, gp_args=list(h=c(130,43), c=100, l=c(90,70)))
labeling_cells(text=tab.prop.sex)(tab.struct2)


# paranoid

# original
# http://www.titanicinquiry.org/BOTInq/BOTReport/botRep01.php
# http://www.titanicinquiry.org/BOTInq/BOTReport/botRepSaved.php
# https://www.encyclopedia-titanica.org/titanic-survivors/
# http://www.icyousee.org/titanic.html

# simpsons paradox
# http://www.anesi.com/titanic.htm

# http://www.icyousee.org/titanic.html#analysis -> dataset?
# facts on historical context! e.g. child depended on class 1st class 14 yrs=child, different class -> adult
# facts on behavior of people while boarding or not!

# dataset
# http://campus.lakeforest.edu/frank/pages/bio150_datasets.html
# http://campus.lakeforest.edu/frank/FILES/MLFfiles/Bio150/Titanic/TitanicV3_DataOnly.xlsx.zip
# http://campus.lakeforest.edu/frank/FILES/MLFfiles/Bio150/Titanic/TitanicMETA.pdf
# Frank Harrel

# http://www.public.iastate.edu/~hofmann/data/titanic.html

# NAs
# Hmisc http://campus.lakeforest.edu/frank/FILES/MLFfiles/Bio150/Titanic/TitanicMETA.pdf
# library(Hmisc)
plot(t.src.naclus <- naclus(t.src))
t.src.naclus
summary(survived~age+sex+pclass+sibsp+parch)



# not run
# does not work properly...
plsmo(age,as.numeric(survived),group=pclass, datadensity=FALSE)



table(survived,sex)
prop.table(table(survived,sex))
prop.table(table(survived,sex),m=1)
prop.table(table(survived,sex),m=2)

prop.table(table(survived,pclass))
prop.table(table(survived,pclass),m=1)
prop.table(table(survived,pclass),m=2)

prop.table(table(sex,pclass))
prop.table(table(sex,pclass),m=1)
prop.table(table(sex,pclass),m=2)
# clarification of variances between variables without causing it!

# colors
fill_colors <- matrix(c("dark cyan","greenyellow","orange","dark magenta"), ncol = 2)
mosaic(~ pclass + sex.FM + survived, data=mastertable, shade=TRUE, gp=gpar(fill=fill_colors, col="violetred3")) #col=0 -> no border around tiles

mosaic(~ pclass + sex.FM + survived, data=mastertable, shade=TRUE, legend=FALSE, split_vertical=c(TRUE,FALSE,TRUE))
mosaic(~ pclass + sex.FM + survived + child.AC, data=mastertable, shade=TRUE, legend=FALSE, split_vertical=c(TRUE,FALSE,TRUE,FALSE))

#
mosaic(~ survived + pclass + sex.FM + child.AC, data=mastertable, shade=TRUE, legend=FALSE, split_vertical=c(TRUE,TRUE,FALSE,FALSE))
pairs(mastertable, highlighting=2, diag_panel=pairs_diagonal_mosaic, diag_panel_args = list(fill = rainbow))
 

#mosaic(~ pclass + sex + survived, data=mastertable, shade=TRUE, legend=TRUE, expected=~pclass*sex*survived)
# library 'MASS'
tmod1 <- loglm(~ (pclass * child.AC * sex.FM) + survived * (pclass + child.AC * sex.FM), data=(mastertable+0.5))
summary(tmod1)
plot(tmod1)
doubledecker(survived ~ sex.FM + child.AC + pclass, data=mastertable)
doubledecker(survived ~ sex.FM + pclass, data=mastertable)
doubledecker(survived ~ child.AC + pclass, data=mastertable)
doubledecker(survived ~ sex.FM + child.AC, data=mastertable)
doubledecker(survived ~ pclass + sex.FM + child.AC, data=mastertable)
structable(survived ~ pclass + sex.FM + child.AC, data=mastertable)
# Admit ~ Dept + Gender, data=UCBAdmissions[2:1,,])

# identical:
# summary( loglm(~ (pclass * child * sex.FM) + survived * (pclass + child * sex.FM), data=(mastertable+0.5)))
# summary( loglm(~ (pclass * child * sex.FM) + survived * pclass + survived * child * sex.FM, data=(mastertable+0.5)))


fourfold(table(sex,survived, pclass, child), mfrow=c(3,3), col="darkred")

# Survival on the Titanic:  2201 passengers, classified by Class, Gender, Age, survived.
# Data from: Mersey (1912), Report on the loss of the “Titanic” S.S. Dawson (1995)

# Mosaic displays allow a detailed explanation:
# Regardless of Age and Gender, lower economic status → increased mortality.
# Differences due to Class were moderated by both Age and Gender.
# Women more likely overall to survive than men, but:

# Class × Gender:
# women in 3rd class did not have a significant advantage
# men in 1st class did compared to men in other classes.

# Class × Age:
# no children in 1st or 2nd class died, but
# nearly two-thirds of children in 3rd class died.
# For adults, mortality as economic class

# Summary statement:
# “women and children (according to class), then 1st class men”.


mosaic(~sex+child+survived, shade=TRUE, legend=TRUE)
mosaic(~sex+survived+child, shade=TRUE, legend=TRUE)


corrgram(t.src, order=TRUE, lower.panel=panel.shade, upper.panel=panel.pie,
		 diag.panel=panel.density, text.panel=panel.txt, main="Titanic dataset")		 

fulltitle.num <- as.factor(fulltitle)
fulltitle.red.num <- as.factor(fulltitle.red)

daten1 <- cbind(pclass,survived,sex,age,sibsp,parch,fare,embarked,
               child,oldage,famsize,famsize.adj,fulltitle.num,fulltitle.red.num,
			   age.bin.1,age.bin.2,age.bin.3,travelno.bin.1,travelno.2,travelno.3,travelno.4)
daten2 <- cbind(pclass,survived,sex,age,sibsp,parch,fare,embarked,
               child,oldage,famsize,famsize.adj,fulltitle.num,fulltitle.red.num)
    
head(daten1)
head(daten2)

barplot(table(pclass, survived))
barplot(table(sex, survived))



# with best prediction according to different predictors of the tree forrest (rpart)
# Large families not good for Survival jasonm
 https://www.kaggleusercontent.com/kf/109040/eyJhbGciOiJkaXIiLCJlbmMiOiJBMTI4Q0JDLUhTMjU2In0..stYun0XEzhQ8IV1WEZ5_yw.jkZ4Oe-waV-QVa1nNOGRiUL56QzEJ0DpArW8qBU-60EAcvg4ZBuuPP-W_b-nbSDgdpeOxrbDW4ACOxWJnD0FW287NaxsJyBjWH86EXlUm4qPQ1USvRitodmc7HT2McuHXSDlebOjfppfYLWIhfoWhGXmxiAV0QUGg4jNkxOpHQQ.DHQgGAhH7au7lB1JBuPtnA/output.html
# formula 	measured accuracy
# 1. Survived ~ Age + Sex 	0.845
# 2. Survived ~ Age + Sex + Pclass 	0.838
# 3. Survived ~ Age + Sex + Fare 	0.807
# 4. Survived ~ Age + Sex + Pclass + Fare 	0.820
# 5. Survived ~ Age + Sex + Pclass + Fare + SibSp + Parch 	0.838
# 6. Survived ~ Age + Sex + Pclass + SibSp + Parch + Fare + Deck 	0.807
# 7. Survived ~ Age + Sex + Pclass + FamilySize 	0.872 (best)
# 8. Survived ~ Age + Sex + FamilySize 	0.854
# 9. Survived ~ Age + Sex + Pclass + FamilySizeAdj 	0.872 (no change)
# 10. Survived ~ Age + Sex + Pclass + TravelAlone 	0.843
# 11. Survived ~ Age + Sex + Pclass + FamilySize + Embarked 	0.858


xxx <- table(famsize.adj, survived)
xxx
data.frame(famsize.adj=attr(xxx,"dimnames")[[1]],survival.ratio=xxx[,2]/xxx[,1])
#!
mosaic(table(famsize.adj,survived), shade=TRUE)
#!


famsize.index <- paste(as.character(famsize.adj),lastname,sep=":")
# famID[famsize.adj <=3] <- "0:SMALL"
# famID.tab <- data.frame(table(famID))
# famID.tab
# comp.tab <- sapply(strsplit(as.character(famID.tab[,"famID"]),split=c(":"), fixed=TRUE),"[",1)
# famID.tab$famID.DROP <- comp.tab != famID.tab[,"Freq"]
# famID.tab
# head(famID.tab)
# sum(famID.tab$famID.DROP == FALSE)
# wrongly classified
# famID.nam2del <- famID.tab[famID.tab$Freq <= 2,]
# #correctedfamID
# famID.cc <- famID
# famID.cc[which(famID %in% famID.nam2del$famID)] <- "0:SMALL"
# famID.cc


# number of rooms/cabins per class

# embarkement
table(embarked)
table(embarked,survived)


# fare range ~ pclass
cor(as.numeric(pclass),fare,use="pairwise")


# stem - not used as much as in the past due to graphical capabilities
stem(age)


# descriptive
describes(data.frame(fare,age))
describes(fare.p)


# relationship class and parents/children/families
# children with parents
# cor.plot heatmap all vars

# plot age over survival status
daten <- age
daten <- fare.p
daten <- fare.p[fare.p <150]


#library(arm)
arm:::display(lm(age ~ survived))
arm:::display(lm(fare.p ~ survived[fare > 0]))



# correlations
nk <- 2
fulltitle.num <- as.numeric(factor(fulltitle))
fulltitle.red.num <- as.numeric(factor(fulltitle.red))
cor.tab <- round(with(t.src, cor(cbind(pclass,survived,sex,age,sibsp,parch,fare,embarked,
                                      child,oldage,famsize,famsize.adj,fulltitle.num,fulltitle.red.num,
									  age.bin.1,age.bin.2,age.bin.3,
									  travelno.bin.1,travelno.2,travelno.3,travelno.4),use="pairwise")),nk)
cor.tab[upper.tri(cor.tab)] <- ""
print(formatC(cor.tab, format="fg"), quote=FALSE)


#library(heatmap3)
heatmap3(table(survived,pclass), scale="row", balanceColor=TRUE, showRowDendro=TRUE, ColSideCut=.1)


ftable(embarked,pclass,sex,survived)
ftable(embarked,pclass,sex,survived)


###proportion tables
prop.table(table(survived))
prop.table(table(pclass))
prop.table(table(sex))


# general titanic and kaggle
#
# 1 http://trevorstephens.com/kaggle-titanic-tutorial/r-part-2-the-gender-class-model/
# 2 https://www.kaggle.com/patkakou/titanic-challenge-r/notebook
# 2 https://www.kaggle.com/abinavramesh/titanic-analysis-no-extra-features/notebook
#
# BUT:
# http://www.clayford.net/statistics/tag/simpsons-paradox/

prop.table(table(sex, survived))
prop.table(table(sex, survived),m=1)
prop.table(table(sex, survived),m=2)


prop.table(table(pclass, survived))
prop.table(table(pclass, survived),m=1)
prop.table(table(pclass, survived),m=2)

prop.table(table(sex, pclass, survived))
# row-wise on the most detailled level: for sex across pclass
prop.table(table(sex, pclass, survived),c(1,3))


# MR and MRS -> title
# family size and family ID

# age dimensions
table(age)
describes(age)
sum(is.na(age))
sum(is.na(age))/length(age)*100

# unique values for each variable
apply(t.src, 2, function(i) length(unique(i)))


# normal plot
#library(car)
qqPlot(age)
qqPlot(fare)

qqnorm(age)
qqline(age, col="violetred2", lty=2, lwd=2)

qqnorm(fare)
qqline(fare, col="violetred2", lty=2, lwd=2)

# smooth plot
smoothScatter(age, bty="n")
smoothScatter(fare, bty="n")

# survivors
child.num <- as.numeric(child)
sex
sex.num <- as.numeric(sex)-1
sex.num #fem=0, male=1
survived.num <- as.numeric(survived)

tab <- aggregate(survived.num ~ age.bin.1 + sex + pclass + travelno.bin.1, FUN=function(x) sum(x)/length(x))
with(tab, tab[order(age.bin.1,pclass,sex,-survived.num),])
with(tab, tab[order(age.bin.1,pclass,sex,survived.num),])
head(tab)

aggregate(survived.num ~ child.num + sex.num, FUN=sum)
# all
aggregate(survived.num ~ child.num + sex.num, FUN=length)
# ratio
aggregate(survived.num ~ child.num + sex.num, FUN=function(x) sum(x)/length(x))

aggregate(age ~ survived + sex + child, FUN=function(x) sum(x)/length(x))
aggregate(survived ~ age.bin1, FUN=function(x) sum(x)/length(x))
aggregate(survived ~ age.bin1 + sex, FUN=function(x) sum(x)/length(x))

# tab <- aggregate(survived ~ age.bin + sex + pclass, FUN=function(x) sum(x)/length(x))
# overall survival rate
# with(tab, tab[order(-survived,age.bin.1,pclass,sex),])

# with(tab, tab[order(age.bin,pclass,sex,-survived),])
# with(tab, tab[order(pclass,age.bin,sex,-survived),])
# with(tab, tab[order(sex,pclass,age.bin,-survived),])
# with(tab, tab[order(sex,pclass,-survived),])
# with(tab, tab[order(pclass,age.bin,-survived),])
# with(tab, tab[order(pclass,-survived),])


# boxplot(age)
# boxplot(age ~ sex)
# boxplot(age ~ sex + pclass)
# boxplot(age ~ sex + pclass + survived)


# comb.asp1 <- paste(age.bin,sex,pclass,sep=":")
# comb.asp2 <- paste(age.bin,sex,survived,sep=":")

# table(comb.asp1)
# table(comb.asp2)

# boxplot(age ~ comb.asp1)
# boxplot(age ~ comb.asp2)



##Boolean analysis
#dependent = survived
#predicted

ttab.ne <- data.frame(
age,
sex,
survived,
pclass,
embarked,
fare,
sibsp,
parch,
#sibsp.parch,
famsize,
possible.mother,
child,
oldage,

travelno.bin.1,
travelno.2,
travelno.3,
travelno.4,

#fare.p,
fare.bin.1,

famsize.adj
)
dim(ttab.ne)
head(ttab.ne)
tail(ttab.ne)


# QCA
#library(QCA)

# NAs
titanic.NAs <- which(is.na(ttab.ne),arr.ind=TRUE)
table(titanic.NAs[,2])
ttab.red <- ttab.ne[,-c(1,5,6,7,8,9,10,11,12,17)]
head(ttab.red)
apply(ttab.red,2,table)
apply(ttab.ne,2,table)
apply(ttab.ne,2,function(x) length(which(is.na(x))))


# relabel levels
#library(rockchalk)

# Crew or not
pclasses <- as.factor(pclass)
crew <- combineLevels(pclasses, levs = c("1st","2nd","3rd"), newLabel = "notCrew")
levels(crew)

# 1st class or not
firstclass <- combineLevels(pclasses, levs = c("2nd","3rd","Crew"), newLabel = "not1stclass")
levels(firstclass)

# lower class or not
lowerclass <- combineLevels(pclasses, levs = c("1st","2nd","Crew"), newLabel = "notlowerclass")
levels(lowerclass)

# travel alone or not
travelalone <- combineLevels(travelno.bin.1, levs = c("duo","group>4","group3+4"), newLabel = "notalone")
levels(travelalone)

# joined not in Southampton
notjoinedinS <- combineLevels(as.factor(embarked), levs = c("B","C","Q"), newLabel = "notS")
levels(notjoinedinS)

# familysize
famsize.adj.ne <- famsize.adj
famsize.adj.ne[famsize.adj > 1] <- 2
table(famsize.adj.ne)
# 1 = alone, 2 = group


# convert to logical
crew.c.L <- crew == "Crew"
firstclass.1st.L <- firstclass == "1st"
lowerclass.l.L <- lowerclass == "3rd"
travelalone.a.L <- travelalone == "alone"
sex.f.L <- sex.FM == "F" #2-as.numeric(sex.FM)
survived.s.L <- survived == "TRUE"


# just replace NAs for not joined in Southampton
notjoinedinS.nS.L <- notjoinedinS == "notS"
njiS.NA <- which(is.na(notjoinedinS.nS.L))
njiS.NA
notjoinedinS.nS.L[njiS.NA] <- TRUE


# create rough logical child var
# unknown age = NA
# travelling alone
# sibling != 0
# not crew
# age < 18
child.AC == "C" & possible.mother == TRUE
which(is.na(child.AC) & age < 18 & travelno.bin.1 != "alone" & sibsp != "1" & possible.mother == FALSE)
possible.child.c.L <- child == TRUE
possible.child.c.L & possible.mother == TRUE
which(is.na(possible.child.c.L) & age < 18 & travelno.bin.1 != "alone" & sibsp != "1" & possible.mother == FALSE)
child.NA.IDs <- which(is.na(possible.child.c.L))
possible.child.c.L[child.NA.IDs] <- FALSE
which(is.na(possible.child.c.L))
table(possible.child.c.L)


# replace NAs for 'possible mother' with "FALSE"
sum(possible.mother == TRUE & possible.child.c.L == TRUE)
pm.NA.IDs <- which(is.na(possible.mother))
pm.NA.IDs
possible.mother.pm.L <- possible.mother
possible.mother.pm.L[pm.NA.IDs] <- FALSE
which(is.na(possible.mother.pm.L))


# family size >3
table(famsize.adj)
travel.hugegroup.4min.L <- famsize.adj > 3
travel.hugegroup.4min.L


# sanity checks
sum(crew.c.L == TRUE & firstclass.1st.L == TRUE)
sum(lowerclass.l.L == TRUE & firstclass.1st.L == TRUE)
#
sum(possible.mother.pm.L == TRUE & possible.child.c.L == TRUE) #NAs!!!
sum(travelalone.a.L == TRUE & travel.hugegroup.4min.L == TRUE)

# create ne = NullEins tabel = zero-ones = truth table numeric (integer)
ttab.red.ne <- data.frame(
crew.c.L,
firstclass.1st.L,
lowerclass.l.L,
travelalone.a.L,
sex.f.L,
survived.s.L,
notjoinedinS.nS.L,
possible.mother.pm.L,
possible.child.c.L,
travel.hugegroup.4min.L
)

head(ttab.red.ne)
tail(ttab.red.ne)
t(apply(ttab.red.ne, 2, table))

# write.table(ttab.red.ne, "ttab.red.ne1.csv", sep="\t", col.names=TRUE)

ttab.red.ne.num <- ttab.red.ne+0
head(ttab.red.ne.num)
tail(ttab.red.ne.num)
t(apply(ttab.red.ne.num, 2, table))


# in-depth Boolean analysis done in
# see ptIV_qual_Boolean_case_Titanic_death-and-dying.r




