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
# ptII_quan_classicstats_missingdata.r

# location:
# chap. 4 [4.6.7]
# Fehlende Daten

# load necessary libs
library(lme4)
library(longpower)
library(mice)
library(Amelia)
library(brms)
library(future)
library(mi)


###### function to simulate missing data
na.sim <- function(n=1000, MW=0, SD=1, pct.rem=0.1, seed=09877)
{
  set.seed(seed)
  v <- rnorm(n=n, mean=MW, sd=SD)
  v.descstat <- c(N=length(v),summary(v), var=var(v), sd=sd(v))
  rem <- sample(1:n, size=n*pct.rem, replace=FALSE)
  v.red <- v[-rem]
  v.red.descstat <- c(N=length(v.red),summary(v.red), var=var(v.red), sd=sd(v.red))
  res <- t(data.frame(v=v.descstat, v.red=v.red.descstat, v.ratio=v.descstat/ v.red.descstat))
return(res) 
}
#call:
#na.sim()
########################## END OF FUNCTION


na.sim()
# repeat the same with
seed <- 464677
na.sim(seed=seed)
na.sim(n=1e6, seed=seed)
na.sim(n=1e7, seed=seed)

# use iris data
data(iris)
head(iris)
tail(iris)
dim(iris)
# check whether there are NAs
which(is.na(iris))


# linear model
iris.lm <- lm(Sepal.Length ~ Sepal.Width + Petal.Width + Petal.Length + Species, data=iris)
summary(iris.lm)
iris.lm.res <- summary(iris.lm)$coefficients
iris.lm.res

# not run - the same as HLM
iris.lmer <- lmer(Sepal.Length ~ Sepal.Width + Petal.Width + Petal.Length + (1|Species), data=iris)
summary(iris.lmer)
#
# power for rate of change = longitudinal model
lmmpower(iris.lmer, pct.change=0.3, t=seq(0,9,1), power=0.8)


# percent to delete = 10% = create NAs
pc <- 0.1
# data points
datps <- prod(dim(iris[,1:4]))

seed <- 464645577
set.seed(seed)
del.IDs <- sample(datps, datps*pc, replace=FALSE)
del.IDs
iris.m <- as.matrix(iris[,1:4])
iris.m
iris.m[del.IDs] <- NA
iris.NA <- iris.m
iris.NA

NA.IDs <- which(is.na(iris.NA), arr.ind=FALSE) 
# check whether the proper values were deleted (=NA)
NA.IDs == sort(del.IDs)

iris.NA.c <- data.frame(iris.NA, Species=iris[,5])
fluxplot(iris.NA.c)
flux(iris.NA.c)

md.pairs(iris.NA.c)
md.pattern(iris.NA.c)


# estimate without imputation
iris.lm.NA <- lm(Sepal.Length ~ Sepal.Width + Petal.Width + Petal.Length + Species, data=iris.NA.c)
summary(iris.lm.NA)
iris.lm.NA.res <- summary(iris.lm.NA)$coefficients
iris.lm.NA.res
# compare
iris.lm.res
# compare as ratio
iris.lm.res/iris.lm.NA.res


# imputation via mice
# pmm = predictive mean matching
iris.imp <- mice(iris.NA.c, method=c("pmm"))
iris.imp.fit <- lm.mids(Sepal.Length ~ Sepal.Width + Petal.Width + Petal.Length + Species, data=iris.imp)
iris.imp.fit
iris.imp.mice.pooled <- mice:::pool(iris.imp.fit)
summary(iris.imp.mice.pooled)
# compare
iris.lm.NA.res
iris.lm.res
summary(iris.imp.mice.pooled)
# compare as ratio
# just estimates
iris.lm.res[,"Estimate"]/iris.imp.mice.pooled$pooled[,c("estimate")]
iris.lm.NA.res[,"Estimate"]/iris.imp.mice.pooled$pooled[,c("estimate")]
# compare all as ratio
iris.lm.res/summary(iris.imp.mice.pooled)[,c("estimate","std.error","statistic","p.value")]
iris.lm.NA.res/summary(iris.imp.mice.pooled)[,c("estimate","std.error","statistic","p.value")]


# via Amelia
# https://www.r-bloggers.com/how-to-perform-a-logistic-regression-in-r/
missmap(iris.NA.c)
iris.ame <- amelia(iris.NA)
str(iris.ame)
summary(iris.ame)
plot(iris.ame)
le <- length(iris.ame$imputations)
betas <- vector()
ses <- vector()
for(i in 1:le)
{
 dats <- data.frame(iris.ame$imputations[[i]], Species=iris$Species)
 iris.ame.lm <- lm(Sepal.Length ~ Sepal.Width + Petal.Width + Petal.Length + Species, data=dats)
 betas <- rbind(betas, iris.ame.lm$coef)
 ses <- rbind(ses, coef(summary(iris.ame.lm))[,2])
}
betas
ses
iris.imp.amelia.pooled <- mi.meld(q=betas, se=ses)
iris.imp.amelia.pooled
iris.imp.amelia.pooled.tab <- t(do.call("rbind",iris.imp.amelia.pooled))
colnames(iris.imp.amelia.pooled.tab) <- c("Estimate","Std. Error")
iris.imp.amelia.pooled.tab
# compare as ratio
iris.lm.res[,c(1,2)]/iris.imp.amelia.pooled.tab
iris.lm.NA.res[,c(1,2)]/iris.imp.amelia.pooled.tab


# Bayes
# library 'brms'
# https://cran.r-project.org/web/packages/brms/vignettes/brms_missings.html
#
# gives out warnings with Rhats >1 (we just ignore that...)
# in reality one should consider better priors, more iterations, etc.
iris.imp.brmsfit <- brm_multiple(Sepal.Length ~ Sepal.Width + Petal.Width + Petal.Length + Species, data=iris.imp, chains=2)
# does not give a warning
iris.brmsfit <- brm(Sepal.Length ~ Sepal.Width + Petal.Width + Petal.Length + Species, data=iris, chains=2)
# output
summary(iris.imp.brmsfit)
plot(iris.imp.brmsfit, pars = "^b_")
iris.imp.brmsfit$rhats
# fixed effects export
fe.iris.imp.brms <- fixef(iris.imp.brmsfit)
fe.iris.imp.brms

summary(iris.brmsfit)

# library 'future' for parallelization
future:::plan(strategy="multicore")
iris.imp.brmsfit <- brm_multiple(Sepal.Length ~ Sepal.Width + Petal.Width + Petal.Length + Species, data=iris.imp, chains=2)
iris.imp.brmsfit
plot(iris.imp.brmsfit, pars="^b")
round(iris.imp.brmsfit$rhats, 2)
# https://stackoverflow.com/questions/46985425/is-it-possible-to-plot-the-coefficients-from-multiple-brms-models-on-a-single-gr
# plot all, see link above
marginal_effects(iris.imp.brmsfit)


# library 'mi'
#library(mi)
iris.NA.c
mdf <- missing_data.frame(iris.NA.c)
show(mdf)
image(mdf)
hist(mdf)
iris.imp.mi <- mi:::mi(mdf, n.iter=30, n.chains=4, max.minutes=20)
show(iris.imp.mi)
round(mipply(iris.imp.mi, mean, to.matrix = TRUE), 3)
Rhats(iris.imp.mi)
plot(iris.imp.mi)
image(iris.imp.mi)
summary(iris.imp.mi)
iris.imp.mi.fit.pooled <- pool(Sepal.Length ~ Sepal.Width + Petal.Width + Petal.Length + Species, data=iris.imp.mi)
display(iris.imp.mi.fit.pooled)
iris.imp.mi.fit.pooled.tab <- data.frame("Estimate"=iris.imp.mi.fit.pooled@coefficients,
                                         "Std. Error"=iris.imp.mi.fit.pooled@ses, check.names=FALSE)
iris.imp.mi.fit.pooled.tab
# compare as ratio
iris.lm.res[,c(1,2)]/iris.imp.mi.fit.pooled.tab
iris.lm.NA.res[,c(1,2)]/iris.imp.mi.fit.pooled.tab


# deviances
# compare mice with amelia and brms | perspective: mice, brms
# estimates
ests <- cbind(original=iris.lm.res[,1],
              amelia=as.numeric(iris.imp.amelia.pooled$q.mi),
              mice=summary(iris.imp.mice.pooled)[,1],
              brms=fe.iris.imp.brms[,1],
              mi=iris.imp.mi.fit.pooled@coefficients,
	      NA.rem=iris.lm.NA.res[,1]
         )

ses <- cbind(original=iris.lm.res[,2],
             amelia.SE=as.numeric(iris.imp.amelia.pooled$se.mi),
             mice.SE=summary(iris.imp.mice.pooled)[,2],
             brms.SE=fe.iris.imp.brms[,2],
             mi.SE=iris.imp.mi.fit.pooled@ses,
             NA.SE.rem=iris.lm.NA.res[,2]
		) 

# comparison over all different methods
# estimates
ests

# simple percent over-/underestimation for each variable over each method
apply(ests,2,function(x) 1 - iris.lm.res[,1]/x)*100

# compare methods over variables
# mean
apply(apply(ests,2,function(x) 1 - iris.lm.res[,1]/x),2,mean)*100
# median
apply(apply(ests,2,function(x) 1 - iris.lm.res[,1]/x),2,median)*100
# sd
apply(apply(ests,2,function(x) 1 - iris.lm.res[,1]/x),2,sd)*100

# standard errors
ses

# simple percent over-/underestimation for each variable over each method
apply(ses,2,function(x) 1 - iris.lm.res[,2]/x)*100

# compare methods over variables
# mean
apply(apply(ses,2,function(x) 1 - iris.lm.res[,2]/x),2,mean)*100
# median
apply(apply(ses,2,function(x) 1 - iris.lm.res[,2]/x),2,median)*100
# sd
apply(apply(ses,2,function(x) 1 - iris.lm.res[,2]/x),2,sd)*100


iris.lm.res
iris.lm.NA.res
display(iris.lm)
display(iris.lm.NA)

-0.32/-0.04 # beta ratio
0.15/0.19   # se ratio

-0.32/0.15  # t-value without NAs
-0.04/0.19  # t-value with NAs



###NOT RUN BELOW THIS POINT
# summary(iris.imp.mice.pooled)[,1] / iris.imp.amelia.pooled$q.mi
# fe.iris.imp.brms[,1] / summary(iris.imp.mice.pooled)[,1]
# fe.iris.imp.brms[,1] / iris.imp.amelia.pooled$q.mi
# fe.iris.imp.brms[,1] / iris.imp.mi.fit@coefficients
#
##variances
# summary(iris.imp.mice.pooled)[,2] / iris.imp.amelia.pooled$se.mi
# fe.iris.imp.brms[,2] / summary(iris.imp.mice.pooled)[,2]
# fe.iris.imp.brms[,2] / iris.imp.amelia.pooled$se.mi
# fe.iris.imp.brms[,2] / iris.imp.mi.fit@ses

