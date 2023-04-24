# file:
# ptII_quan_classicstats_Simpsonparadox.r

# location:
# chap. 4 [4.6.11.1]
# Simpson Paradox

# load necessary libs
library(vcd)
library(reshape2)
library(arm)
library(Simpsons)

# load necessary helper functions
source("ptII_quan_classicstats_Simpsonparadox_helpfuncs.r")


# Berkeley university case
applications <- c(m=2691, f=1835)
admissions <- c(m=1198, f=557)
rejections <- applications-admissions
admissionrates <- admissions/applications
admissionrates
admissionrates["m"]/admissionrates["f"]
# odds ratio
AR <- admissions/rejections
AR[1]/AR[2]
chisq.test(apply(UCBAdmissions, c(1, 2), sum))

# better original dataset!!!
# raw
UCBAdmissions

# assoc plot
# that almost shows everything ...
# library(vcd)
assoc(aperm(UCBAdmissions), expected = ~ (Admit + Gender) * Dept, compress = FALSE,
      labeling_args = list(abbreviate = c(Gender = TRUE), rot_labels = 0))

# margin tables...
margin.table(UCBAdmissions, margin=c(1,2,3))
# global addmission vs rejected - baserate
margin.table(UCBAdmissions, margin=c(1))
 #global male versus female - baserate
margin.table(UCBAdmissions, margin=c(2))
# not run
#apply(UCBAdmissions[,,1:6],2,sum)
# global total for each department
margin.table(UCBAdmissions, margin=c(3))
# admission versus rejected for male versus female
margin.table(UCBAdmissions, margin=c(1,2))
# admission versus rejected for each department
margin.table(UCBAdmissions, margin=c(1,3))
# male versus female for each department
margin.table(UCBAdmissions, margin=c(2,3))

# admission versus rejected for male versus female
ucbd.tab <- margin.table(UCBAdmissions, margin=c(1,2))
# not run
#apply(UCBAdmissions, c(1, 2), sum)
ucbd.tab
chisq.test(ucbd.tab)

mosaicplot(UCBAdmissions, border="darkblue", shade=TRUE)
mosaicplot(ucbd.tab, main = "Berkeley UC acceptance rates", border="darkblue", shade=TRUE)
# not run, just grey
#mosaicplot(apply(UCBAdmissions, c(1, 2), sum), main = "Student admissions at UC Berkeley")

# not run
fm <- loglin(UCBAdmissions, list(1,2,3))
fm
1-pchisq(fm$lrt, fm$df)

# for each department
par(mfrow=c(2,3), oma=c(0,0,3,0), "cex.axis"=1, bty="l")
for(i in 1:6) mosaicplot(UCBAdmissions[,,i], xlab="admission", ylab="sex", main=paste("department", LETTERS[i]), border="darkblue", shade=TRUE)
mtext(expression("Student admissions at UC Berkeley"), outer=TRUE, line=0, cex=1.6)

# handle as dataframe
ucbadm <- as.data.frame(UCBAdmissions)
ucbadm
# add some more cols ie. level combinations
ucbadm <- data.frame(ucbadm, deptsex=paste(ucbadm$Dept,ucbadm$Gender,sep="-"),
				     admitdeptsex=paste(ucbadm$Admit,ucbadm$Dept,ucbadm$Gender,sep="-"),
				     admitdept=paste(ucbadm$Admit,ucbadm$Dept,sep="-"),
				     admitsex=paste(ucbadm$Admit,ucbadm$Gender,sep="-"))
ucbadm

# corrected base rate
xxx <- data.frame(totalN=rep(tapply(ucbadm$Freq, ucbadm$Dept, sum), each=4), ucbadm)
xxx2 <- data.frame(totalNbysex=rep(tapply(xxx$Freq, xxx$deptsex, sum), 2),
                   xxx[order(xxx$admitdeptsex),])
xxx2 <- data.frame(fac.C.bysex=xxx2$Freq/xxx2$totalNbysex,xxx2)
xxx2

xxx <- data.frame(fac.C=xxx$Freq/xxx$totalN,xxx)
xxx <- data.frame(Freq.C=(xxx$fac.C*xxx$Freq), xxx)
xxx
xxx1 <- droplevels(subset(xxx, Admit == "Admitted"))
xxx1
xx1.med <- tapply(xxx1$fac.C, xxx1$Gender, median)
xx1.mean <- tapply(xxx1$fac.C, xxx1$Gender, mean)
xx1.med
xx1.mean

# extract only 'admitted'
ucbadm.adm <- subset(ucbadm, Admit == "Admitted")
ucbadm.adm
# sort
ucbadm.adm.sort <- data.frame(ucbadm.adm[order(ucbadm.adm$admitdeptsex),],
                              deptsex.sum=tapply(ucbadm$Freq, ucbadm$deptsex, sum))
ucbadm.adm.sort
# add admission rate
# with(ucbadm.adm.sort.rate, tapply(adm.rate, droplevels(admitdeptsex), mean))
ucbadm.adm.sort.rate <- droplevels(data.frame(ucbadm.adm.sort,adm.rate=with(ucbadm.adm.sort, Freq/deptsex.sum)))
ucbadm.adm.sort.rate

# reshape2 lib
ucbadm.ratebydept.sex <- dcast(ucbadm.adm.sort.rate, Dept ~ Gender, value.var="adm.rate")
ucbadm.ratebydept.sex
# for which departments are the admission rates higher for males
with(ucbadm.ratebydept.sex, which(Male > Female))
# for which departments are the admission rates higher for females
with(ucbadm.ratebydept.sex, which(Female > Male))
# ratio female versus male addmission rates
data.frame(dept=ucbadm.ratebydept.sex$Dept, ratio.f.vs.m=with(ucbadm.ratebydept.sex, Female/Male))

# check with model
glm.fit <- display(glm(adm.rate ~ Dept + Gender , data=ucbadm.adm.sort.rate), detail=TRUE)
# summary(glm(adm.rate ~ Dept + Gender , data=ucbadm.adm.sort.rate))
# t-values
glm.fit$coef/glm.fit$se

display(glm(Freq ~ Dept + Gender , data=ucbadm.adm.sort.rate, family=poisson()), detail=TRUE)
summary(glm(Freq ~ admitdept + Gender , data=ucbadm, family=poisson()))
display(glm(adm.rate ~ Dept + Gender, data=ucbadm.adm.sort.rate), detail=TRUE)

# not run
#display(glm(adm_rate ~ dpt + gen, data=df2))
#display(lm(adm_rate ~ dpt + gen, data=df2))

# HLM
# as binomial model
display(glmer(Admit == "Admitted" ~ (1|Dept) + Gender , data=ucbadm, family=binomial(link="logit")), detail=TRUE)
display(glmer(Admit == "Admitted" ~ (1|Dept) + Gender , data=ucbadm, weight=Freq, family=binomial(link="logit")), detail=TRUE)
# as poisson model
display(glmer(Freq ~ (1|Dept) + Gender , data=ucbadm, family=poisson()), detail=TRUE)

#
chisq.test(apply(ucbadm.ratebydept.sex[,c(2,3)],2,sum))


# Simpson Paradox
# simulation
seed <- 99883
set.seed(seed)

# create some artifical data that represent the Simpson Paradox
v <- data.frame(x=c(c(1:10)+rnorm(10), c(11:20)+rnorm(10)),
                y=c(c(10:1)+rnorm(10), c(20:11)+rnorm(10)),
        	group=gl(2,10))
v
# plot two groups
SP.sim(v)

# detect subgroups
out.s <- Simpsons(x,y,data=v)
summary(out.s)
print(out.s)

# check clustering
coef(out.s)
str(out.s)
out.tab <- cbind(v, mclust=out.s$mclustanalysis$classification)
cbind(out.tab, comp=out.tab[,"group"] == out.tab[,"mclust"])

