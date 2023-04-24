# file:
# ptII_quan_Bayes_PPC_model-check-graph.r

# location:
# chap. 6 [6.7.4.5]
# Graphische Begutachtung von Modellen im Dienste des Modellfittings

# load necessary libraries
library(arm)
library(brms)
library(ggplot2)
library(BEST)


# load necessary helper functions
source("ptall_generalfuncs.r")


# https://vuorre.netlify.com/post/2017/bayes-factors-with-brms/
# https://mjskay.github.io/tidybayes/articles/tidy-brms.html
# https://cran.r-project.org/web/packages/bayesplot/vignettes/graphical-ppcs.html
# https://www.rensvandeschoot.com/tutorials/brms-priors/
# color_scheme_set("viridis")


# case 1 - linear regression - almost ideal fit
n <- 100
x <- 1:n
set.seed(887766)
y <- x + rnorm(100,0,10)

plot(x,y)

xymodel <- data.frame(x, y)
head(xymodel)

cor(x,y)

# frequentist analysis
lm.xymodel <- lm(y ~ x, data=xymodel)
summary(lm.xymodel)
display(lm.xymodel)

# plot
plot(xymodel$x, xymodel$y, pch=21, cex=1.1, xlab="x", ylab="y", bg="yellow", col="steelblue", pre.plot=grid(), bty="n", main="")
abline(lm.xymodel, lty=2, col="red", lwd=2)
lines(lowess(x,y), lty=1, col="darkgreen", lwd=2)
par(mfrow=c(2,2))
plot(lm.xymodel, col="purple", main="")


# Bayesian analysis with Stan and brms package
P <- c(prior(normal(1,5), class="b", coef="x"),
    # prior(student_t(2,0,10), class="Intercept"),
	  # prior(student_t(2,0,10), class="sigma"))
       prior(student_t(3,50,29), class="Intercept"),
  	   prior(student_t(3,0,29), class="sigma"))
brm.xymodel <- brm(y ~ x, data=xymodel, family="gaussian", save_all_pars=TRUE, prior=P)						   			   

summary(brm.xymodel)
plot(brm.xymodel)
pairs(brm.xymodel)

brms:::pp_check.brmsfit(brm.xymodel, nsamples=100)
brms:::pp_check.brmsfit(brm.xymodel, nsamples=100, type="ecdf_overlay")
brms:::pp_check.brmsfit(brm.xymodel, nsamples=100, type="error_scatter_avg")
brms:::pp_check.brmsfit(brm.xymodel, nsamples=100, type="stat")

conditional_effects(brm.xymodel)

# hypotheses
hypo0.int.greater.zero <- hypothesis(brm.xymodel, "Intercept > 0")
hypo0.int.smaller.zero <- hypothesis(brm.xymodel, "Intercept < 0")
hypo1.b.eq.1 <- hypothesis(brm.xymodel, "x = 1")
hypo1.b.eq.1
plot(hypo1.b.eq.1)


# case 2 - treatment design
set.seed(1172233)
ngroup <- 2
n <- 100
mu1 <- 100
mu2 <- 105
# same variances
sigma1 <- 10
sigma2 <- 10
yC <- rnorm(n,mu1,sigma1)
yT <- rnorm(n,mu2,sigma2)
y <- c(yC,yT)

group <- rep(c("Control","Treatment"),each=n)
xymodel <- data.frame(y=y,group=factor(group))
head(xymodel)
tail(xymodel)

# descriptive statistics
do.call("rbind",with(xymodel, tapply(y, INDEX=group, FUN=function(x) c(summary(x),SD=sd(x),VAR=var(x),fivenum2(x)))))
cohensd(xymodel$y[xymodel$group == "Control"], xymodel$y[xymodel$group == "Treatment"])

fac <- 1.15
densC <- density(xymodel$y[xymodel$group == "Control"])
densT <- density(xymodel$y[xymodel$group == "Treatment"])
ylim <- range(c(densC$y, densT$y))*fac
xlim <- range(c(densC$x, densT$x))*fac

# descriptive plot
par(oma=c(2,1,1,1), "cex.axis"=1, bty="l", mfrow=c(1,2))
hist(xymodel$y, ylim=ylim, xlim=xlim, prob=TRUE, breaks="Scott", border="white", col="steelblue", bty="n", pre.plot=grid(), main="", xlab="y")
lines(densC, col="orange", lwd=2)
lines(densT, col="red", lwd=2)
# boxplot
plot(y ~ group, data=xymodel, pch=21, cex=1.1, bg="darkred", pre.plot=grid(), bty="n", main="", notch=TRUE, col=c("orange","yellow"))
mtext("Treatment design with two groups", outer=TRUE, line=-2, cex=1.5, side=3)


# frequentist solution
lm.xymodel.CT <- lm(y ~ group, data=xymodel)
summary(lm.xymodel.CT)
display(lm.xymodel.CT)

par(mfrow=c(2,2))
plot(lm.xymodel.CT, col="purple")


# Bayesian analysis with Stan and brms package
P <- c(prior(normal(1,5), class="b", coef="groupTreatment"),
	   prior(student_t(2,0,10), class="Intercept"),
	   prior(student_t(2,0,10), class="sigma"))
brm.xymodel.CT <- brm(y ~ group, data=xymodel, family="gaussian", save_all_pars=TRUE, prior=P)							   			   

summary(brm.xymodel.CT)
plot(brm.xymodel.CT)
pairs(brm.xymodel.CT)
brms:::pp_check.brmsfit(brm.xymodel.CT, nsamples=100)
brms:::pp_check.brmsfit(brm.xymodel.CT, nsamples=100, type="ecdf_overlay")
brms:::pp_check.brmsfit(brm.xymodel.CT, nsamples=100, type="error_scatter_avg")
brms:::pp_check.brmsfit(brm.xymodel.CT, nsamples=100, type="stat")
marginal_effects(brm.xymodel.CT)


# case 3 - heterogenous variances between groups
set.seed(1172233)
ngroup <- 2
n <- 100
mu1 <- 100
sigma1 <- 14#12
mu2 <- 110
sigma2 <- 10
yC <- rnorm(n,mu1,sigma1)
yT <- rnorm(n,mu2,sigma1)
yT.alt <- rnorm(n,mu2,sigma2)
# different mu, same variance
y <- c(yC,yT)
# different mu, different variance
y.alt <- c(yC,yT.alt)

group <- rep(c("Control","Treatment"),each=n)
xymodel <- data.frame(y=y,y.alt=y.alt,group=factor(group))
head(xymodel)
tail(xymodel)

# descriptive plot
dens.yC <- density(yC)
dens.yT <- density(yT)
dens.yT.alt <- density(yT.alt)
ylim <- c(0, max(dens.yC$y, dens.yT$y, dens.yT.alt$y))

par(oma=c(2,1,1,1), "cex.axis"=1, bty="l", mfrow=c(2,1))
# same variance
boxplot(y ~ group, data=xymodel, pch=21, cex=1.1, horizontal=TRUE, bg="darkred", bty="n", main="", notch=TRUE, col=c("orange","yellow"))
# different variance
boxplot(y.alt ~ group, data=xymodel, pch=21, cex=1.1, horizontal=TRUE, bg="darkred", bty="n", main="", notch=TRUE, col=c("orange","green"))
mtext("Treatment design with two groups", outer=TRUE, line=-2, cex=1.5, side=3)

par(oma=c(2,1,1,1), "cex.axis"=1, bty="l")
hist(xymodel$y, prob=TRUE, ylim=ylim, breaks="Scott", border="white", col="steelblue", bty="n", pre.plot=grid(), main="", xlab="y")
lines(dens.yC, col="orange", lwd=2)
lines(dens.yT, col="red", lwd=2)
lines(dens.yT.alt, col="green", lwd=2)
mtext("Treatment design with two groups", outer=TRUE, line=-2, cex=1.5, side=3)
par(fig=c(0,1,0,1), oma=c(1,0,0,0), mar=c(0,0,0,0), new=TRUE)
plot(1, type="n", bty="n", xaxt="n", yaxt="n")
cols <- c("orange","red","green")
legend("bottom", legend=c("control","treat | same var","treat | diff var"), lty=1, lwd=2, xpd=TRUE, horiz=TRUE, col=cols, bty="n", cex=.9)


# descriptive statistics

# different mu, same variance
do.call("rbind",with(xymodel, tapply(y, INDEX=group, FUN=function(x) c(summary(x),SD=sd(x),VAR=var(x),fivenum2(x)))))
cohensd(xymodel$y[xymodel$group == "Control"], xymodel$y[xymodel$group == "Treatment"])

# different mu, different variance
do.call("rbind",with(xymodel, tapply(y.alt, INDEX=group, FUN=function(x) c(summary(x),SD=sd(x),VAR=var(x),fivenum2(x)))))
cohensd(xymodel$y.alt[xymodel$group == "Control"], xymodel$y.alt[xymodel$group == "Treatment"])

# frequentist
lm.hetv <- lm(y.alt ~ group, data=xymodel)
summary(lm.hetv)
display(lm.hetv)
anova(lm.hetv)


# Bayesian analysis with Stan and brms package

# PRIORS

# different mu, different variance
# non-heterogenous variances between groups
get_prior(bf(y.alt ~ group), data=xymodel)

# define priors for non-heterogenous variances
P.nonhet <- c(prior(normal(1,5), class="b", coef="groupTreatment"),
              prior(student_t(2,0,10), class="Intercept"),
	            prior(student_t(2,0,10), class="sigma"))
	  
# different mu, different variance
# heterogenous variances between groups
# one prior for the variances

# different priors
# group factor tests also SD of residual of the response
get_prior(bf(y.alt ~ group, sigma ~ group), data=xymodel)

# model different sigmas for each group
get_prior(bf(y.alt ~ group, sigma ~ 0 + group), data=xymodel)

# https://vuorre.netlify.com/post/2017/03/21/bayes-factors-with-brms/
# model the intercept and allow to specify a prior on it
# the '0 +' takes out the default intercept (= like "-1' in other R models)
# the reserved term 'intercept' shows you mean the regular/ real intercept
# ie. you model the intercept as a unique factor in the model
# ?brmsformula
get_prior(bf(y.alt ~ group, sigma ~ 0 + Intercept + group), data=xymodel)
# + 0 = make one group as the intercept, ie. shift model to this group etc. as a base
# summary(with(xymodel, lm(y~group)))
# summary(with(xymodel, lm(y~0+group)))

# define priors for heterogenous variances
P.hetv <- c(prior(normal(1,5), class="b", coef="groupTreatment"),
            prior(student_t(2,0,10), class="Intercept"),
	          # one prior on sigma
	          prior(student_t(2,0,10), class="b", dpar="sigma", coef="groupTreatment"),
	          prior(student_t(3,0,10), class="Intercept", dpar="sigma")
        	 )

# models

# robust estimation with family="student"
# use gaussian() for other family
brm.xymodel.CT.nonhetv <- brm(bf(y.alt ~ group), data=xymodel, family="student",
                              sample_prior=TRUE, save_all_pars=TRUE, prior=P.nonhet)

# models the effect of group on the mean AND the residual standard deviation of the response distribution
# ie. it is like a group-test how the group influences the SD of the residuals on the response
# it is not modelling the variances differently, ie. different variances for each group
# the latter uses 'sigma ~ 0 + group'
brm.xymodel.CT.hetv <- brm(bf(y.alt ~ group, sigma ~ group), data=xymodel, family="student",
                              sample_prior=TRUE, save_all_pars=TRUE, prior=P.hetv)		   
brm.xymodel.CT.hetv.NV <- brm(bf(y.alt ~ group, sigma ~ group), data=xymodel, family="gaussian",
                              sample_prior=TRUE, save_all_pars=TRUE, prior=P.hetv)	

brm.xymodel.CT.hetv
brm.xymodel.CT.hetv.NV

# all effects
fixef(brm.xymodel.CT.hetv)
# normal effects
fixef(brm.xymodel.CT.hetv)[c(1,3),]
# sigmas have to be exp()
# intercept is the sigma (standard deviation) of group 0
# groupTreatment is the effect of group 1 on sigma (of group 0)
# standard deviation of groupTreatment is sigma_Intercept + sigma_groupTreatment
# first we check the direction of the effect
sigmaef <- fixef(brm.xymodel.CT.hetv)[c(2,4),]
sigmaef
# now exp()
sigmaef.exp <- exp(sigmaef[,"Estimate"])
sigmaef.exp
# group Control
sigmaef.exp[1]
# group Treatment
# see above - negative effect of groupTreatment on SD
ifelse(sigmaef[2,"Estimate"] > 0, sum(sigmaef.exp), sigmaef.exp[1]-sigmaef.exp[2])
sigmaef.exp[1] + sign(sigmaef[2,1])*sigmaef.exp[2]
# compare to SDs original data
with(xymodel, tapply(y.alt, group, sd))



# MCMC samples
mcmcs <- as.data.frame(brm.xymodel.CT.hetv, pars="groupTreatment")[,1]
str(mcmcs)
plot(mcmcs, type="l", pre.plot=grid(), col="darkred", bty="n")
mcmc.l <- length(mcmcs)
# MCMCs > 9
mcmcs.above <- sapply(seq_along(0:14), function(x) sum(mcmcs > x)/mcmc.l)
mcmcs.above
plot(mcmcs.above, type="l", pre.plot=grid(), col="darkred", bty="n")


# https://vuorre.netlify.app/post/2017/01/02/how-to-compare-two-groups-with-robust-bayesian-estimation-using-r-stan-and-brms/#unequal-variances-model
# The model’s output contains our 4 parameters. Intercept is the mean for group 0, Group 1 is
# the “effect of group 1”. The sigma_Intercept is the standard deviation of Group 0, sigma_Group
# is the effect of group 1 on the standard deviation (the SD of Group 1 is sigma_Intercept + sigma_Group).
# The sigmas are implicitly modeled through a log-link (because they must be positive).
# To convert them back to the scale of the data, they need to be exponentiated.
# After taking the exponents of the sigmas, the results look like this:

# analyses

# assumed same variance
prior_summary(brm.xymodel.CT.nonhetv)
summary(brm.xymodel.CT.nonhetv)
plot(brm.xymodel.CT.nonhetv)
pairs(brm.xymodel.CT.nonhetv)

# posterior predictive checks
brms:::pp_check.brmsfit(brm.xymodel.CT.nonhetv, nsamples=100)
plot(marginal_effects(brm.xymodel.CT.nonhetv), points=TRUE)

# assumed different variance
prior_summary(brm.xymodel.CT.hetv)
summary(brm.xymodel.CT.hetv)
plot(brm.xymodel.CT.hetv)
pairs(brm.xymodel.CT.hetv)
# posterior predictive checks
brms:::pp_check.brmsfit(brm.xymodel.CT.hetv, nsamples=100)
plot(marginal_effects(brm.xymodel.CT.hetv), points=TRUE)

# test hypothesis about different variances
# Evid.Ratio is a ratio (BF_01)
hypothesis( brm.xymodel.CT.hetv, "sigma_Intercept + sigma_groupTreatment = 2")
hypothesis( brm.xymodel.CT.hetv, "sigma_Intercept + sigma_groupTreatment = 2.5")
h1.hetv <- hypothesis( brm.xymodel.CT.hetv, c("sigma_Intercept = 0", "sigma_Intercept + sigma_groupTreatment = 0"))
h1.hetv
plot(h1.hetv, ignore_prior=TRUE)

# plot only posterior
h1 <- hypothesis(brm.xymodel.CT.hetv, c("sigma_Intercept > 20", "sigma_Intercept + sigma_groupTreatment - 50 = 0"))
h1
plot(h1, ignore_prior=TRUE)

# plot posterior with prior
h2 <- hypothesis(brm.xymodel.CT.hetv, c("Intercept + 70 = 0"))
plot(h2)
# library 'ggplot2'
h2p1 <- plot(h2, ignore_prior=FALSE, theme = theme_get(), plot = F)[[1]] +
  theme(legend.position = "top")
h2p1


# https://vuorre.netlify.com/post/2017/03/21/bayes-factors-with-brms/
hypo1.1 <- c("sigma_Intercept = 0")
h1.1 <- hypothesis(brm.xymodel.CT.hetv, hypo1.1)
plot(h1.1, ignore_prior=FALSE)
plot(h1.1, ignore_prior=TRUE)

# reproduce manually
h2p2 <- plot(h1.1, ignore_prior=TRUE, theme = theme_get(), plot = F)[[1]] + 
             coord_cartesian(ylim = c(0, 0.7), xlim = c(-2, 19)) +
             theme(legend.position = "top")
gridExtra::grid.arrange(h2p1, h2p2, nrow = 2)


# test that the treatment group variance is larger than the control group variance
hypo2 <- c("exp(sigma_Intercept + sigma_groupTreatment) < exp(sigma_Intercept)")
h2.hetv <- hypothesis(brm.xymodel.CT.hetv, hypo2)
h2.hetv
BF01 <- h2.hetv$hypothesis$Evid.Ratio
BF01
# BF10
1/BF01
plot(h2.hetv, ignore_prior=TRUE)


# run only if the model is modelled that way:
get_prior(bf(y.alt ~ group, sigma ~ group), data=xymodel)
get_prior(bf(y.alt ~ group, sigma ~ 0 + group), data=xymodel)
# this result in sigma for each group
# the sigma population-level intercept is dropped
# see also https://cran.r-project.org/web/packages/brms/brms.pdf
# "Parameterization of the population-level intercept"
brm.xymodel.CT.hetv1 <- brm(bf(y.alt ~ group, sigma ~ 0 + group), data=xymodel, family="student",
                               sample_prior=TRUE, save_all_pars=TRUE)
brm.xymodel.CT.hetv2 <- brm(bf(y.alt ~ group, sigma ~ 0 + intercept + group), data=xymodel, family="student",
                               sample_prior=TRUE, save_all_pars=TRUE)

brms:::pp_check.brmsfit(brm.xymodel.CT.hetv1, nsamples=100)

summary(brm.xymodel.CT.hetv)
summary(brm.xymodel.CT.hetv1)
summary(brm.xymodel.CT.hetv2)

# see differences between the models
prior_summary(brm.xymodel.CT.hetv)
prior_summary(brm.xymodel.CT.hetv1)
prior_summary(brm.xymodel.CT.hetv2)
# >>> first model: different sigmas for each group
# >>> second model: one sigma for the group, one for the intercept

# then you can plot both sigmas of the treatments against each other
# plot sigmas
str(posterior_samples(brm.xymodel.CT.hetv1))
sigmas <- exp(posterior_samples(brm.xymodel.CT.hetv1, "^b_sigma_"))
# library 'ggplot2'
ggplot(stack(sigmas), aes(values)) + geom_density(aes(fill = ind))

hypo3 <- c("exp(sigma_groupTreatment) < exp(sigma_groupControl)")
hypo3.hetv <- hypothesis(brm.xymodel.CT.hetv1, hypo3)
hypo3.hetv
plot(hypo3.hetv, ignore_prior=TRUE)
plot(hypo3.hetv, ignore_prior=FALSE)

pp_check(brm.xymodel.CT.hetv1, nsamples=100, type="error_scatter_avg")

# further PPC plots
brms:::pp_check.brmsfit(brm.xymodel.CT.hetv, nsamples=100, type="ecdf_overlay")
brms:::pp_check.brmsfit(brm.xymodel.CT.hetv, nsamples=100, type="error_scatter_avg")
brms:::pp_check.brmsfit(brm.xymodel.CT.hetv, nsamples=100, type="stat")

# on the log scale - use exp() to handle it later
samps0 <- posterior_samples(brm.xymodel.CT.hetv)
samps1 <- posterior_samples(brm.xymodel.CT.hetv, "^b")
samps2 <- posterior_samples(brm.xymodel.CT.hetv, "^b_sigma")
str(samps0)
str(samps1)
str(samps2)
head(samps0)
head(samps1)
head(samps2)

str(samps0)
samps0.d <- dim(samps0)
d12 <- ceiling(sqrt(samps0.d[2]))
par(oma=c(2,1,1,1), "cex.axis"=1, bty="l", mfrow=c(d12,d12))
fac <- 1.15
for(i in 1:samps0.d[2])
{
 print(i)
 dens <- density(samps0[,i])
 ylim <- c(0,max(dens$y)*fac)
 hist(samps0[,i], prob=TRUE, pre.plot=grid(), bg="grey", ylim=ylim, xlab=colnames(samps0)[i],main="")
 lines(dens, col="darkred", lwd=2)
} 
mtext("Samples of posterior of model parameters", outer=TRUE, line=-2, cex=1.5, side=3)


#compare models
brm.xymodel.CT.nonhetv.ic <- brms:::add_criterion(brm.xymodel.CT.nonhetv, criterion=c("loo","waic","kfold","R2","marglik"))
brm.xymodel.CT.hetv.ic <- brms:::add_criterion(brm.xymodel.CT.hetv, criterion=c("loo","waic","kfold","R2","marglik"))
brm.xymodel.CT.hetv.ic1 <- brms:::add_criterion(brm.xymodel.CT.hetv1, criterion=c("loo","waic","kfold","R2","marglik"))


brms:::compare_ic(brm.xymodel.CT.nonhetv.ic, brm.xymodel.CT.hetv.ic, brm.xymodel.CT.hetv.ic1)
loo_compare(brm.xymodel.CT.nonhetv.ic, brm.xymodel.CT.hetv.ic, brm.xymodel.CT.hetv.ic1)

# compare models                  
for(i in c("loo","waic","kfold"))
{
 cat("\n",i)
 print(brms:::loo_compare.brmsfit(brm.xymodel.CT.nonhetv.ic, brm.xymodel.CT.hetv.ic, brm.xymodel.CT.hetv.ic1, criterion=i))
} 

# R2
brms:::bayes_R2.brmsfit(brm.xymodel.CT.nonhetv)
brms:::bayes_R2.brmsfit(brm.xymodel.CT.hetv)
brms:::bayes_R2.brmsfit(brm.xymodel.CT.hetv1)
brms:::bayes_R2.brmsfit(brm.xymodel.CT.hetv2)

# extract infos
brms:::residuals.brmsfit(brm.xymodel.CT.hetv)
brms:::predict.brmsfit(brm.xymodel.CT.hetv)
brms:::pp_average.brmsfit(brm.xymodel.CT.nonhetv, brm.xymodel.CT.hetv)
brms:::post_prob.brmsfit(brm.xymodel.CT.nonhetv, brm.xymodel.CT.hetv, prior_prob=c(0.8,0.2)) 

# extract general information
# fitted values
fitted(brm.xymodel.CT.hetv)
# fixed effects
fixef(brm.xymodel.CT.hetv)
# random effects - here not present
ranef()
# log likelihood
log_lik(brm.xymodel.CT.hetv)
# log posterior
log_posterior(brm.xymodel.CT.hetv)
# posterior model probabilities from marginal likelihoods
post_prob(brm.xymodel.CT.hetv, brm.xymodel.CT.nonhetv)
# posterior intervals Q2.5 and Q97.5
posterior_interval(brm.xymodel.CT.hetv)
# posterior table per model paramter
posterior_table(brm.xymodel.CT.hetv)
# residuals
residuals(brm.xymodel.CT.hetv)
# variance-covariance matrix
vcov(brm.xymodel.CT.hetv)

# hypos
hx1 <- hypothesis(brm.xymodel.CT.hetv, "Intercept = 100")
plot(hx1)
hypothesis(brm.xymodel.CT.hetv, "sigma_groupTreatment > 0")
hx2 <- hypothesis(brm.xymodel.CT.hetv, "groupTreatment - 10 > 0", alpha=0.08)
plot(hx2)

# hp <- plot(h0, plot=F, theme=theme_get())[[1]]
# hp + scale_x_continuous(breaks = seq(-100, 100, by = 10),
#                       labels = seq(-100, 100, by = 10))		   
#h1
plot(h1)
h2
plot(h2)


# manual tests
# https://vuorre.netlify.app/post/2017/01/02/how-to-compare-two-groups-with-robust-bayesian-estimation-using-r-stan-and-brms/
# posterior distribution of group effect
# test posterior
crit1 <- 5
treat.post <- as.data.frame(brm.xymodel.CT.hetv, pars = "groupTreatment")[,1]
mean(treat.post > crit1)

crit2 <- 2.3
ctrl.post <- as.data.frame(brm.xymodel.CT.hetv, pars = "groupControl")[,1]
mean(ctrl.post)
mean(ctrl.post < crit2)

# classic, same test
t.test(xymodel$y[group == "Treatment"], xymodel$y[group == "Control"], data=xymodel, mu=5, alternative="greater")


# Residualplot
# https://stackoverflow.com/questions/4357031/qqnorm-and-qqline-in-ggplot2
resi1 <- residuals(brm.xymodel.CT.nonhetv)[,"Estimate"]
resi2 <- residuals(brm.xymodel.CT.hetv)[,"Estimate"]
resi3 <- residuals(brm.xymodel.CT.hetv1)[,"Estimate"]
par(mfrow=c(2,2))
qqnorm(resi1,main="assumed same variance", col="blue")
qqline(resi1, col="red",lwd=2)
qqnorm(resi2,main="group factor on group sigma", col="blue")
qqline(resi2, col="red",lwd=2)
qqnorm(resi3,main="separate variances for groups", col="blue")
qqline(resi3, col="red",lwd=2)

# https://mvuorre.github.io/pdf/2018-heino-vuorre-hankonen.pdf


# summary models
# no handling of variances
brm.xymodel.CT.nonhetv <- brm(bf(y.alt ~ group), data=xymodel, family="student",
                                 sample_prior=TRUE, save_all_pars=TRUE, prior=P.nonhet)
# group factor for sigma
brm.xymodel.CT.hetv <- brm(bf(y.alt ~ group, sigma ~ group), data=xymodel, family="student",
                              sample_prior=TRUE, save_all_pars=TRUE, prior=P.hetv)		   
# separate variances for groups
brm.xymodel.CT.hetv1 <- brm(bf(y.alt ~ group, sigma ~ 0 + group), data=xymodel, family="student",
                               sample_prior=TRUE, save_all_pars=TRUE)
# unique estimation of intercept with prior
brm.xymodel.CT.hetv2 <- brm(bf(y.alt ~ group, sigma ~ 0 + intercept + group), data=xymodel, family="student",
                               sample_prior=TRUE, save_all_pars=TRUE)





