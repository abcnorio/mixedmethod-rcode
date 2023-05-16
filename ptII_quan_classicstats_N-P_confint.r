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
# ptII_quan_classicstats_N-P_confint.r

# location:
# chap. 4 [4.5.2.6]
# Konfidenzintervalle

# load helper functions
source("ptII_quan_classicstats_N-P_confint_helpfuncs.r")


# 95% N-P CI empirical mean
n1 <- 50
xbar1 <- 100
sd1 <- 10

ci.mean.res <- ci.mean(n1=n1, xbar1=xbar1, sd1=sd1, printshort=TRUE)
ci.mean.res
ci.mean(n1=n1, xbar1=xbar1, sd1=sd1, printshort=FALSE)

# rounded
digits <- 2
sapply(ci.mean(n1=n1, xbar1=xbar1, sd1=sd1), round, digits)

# caclulate t-value
prob <- .95
alpha <- 1-prob
qt(1-alpha/2, df=Inf, lower.tail=TRUE)

# 95% N-P CI empirical difference in means
n2 <- 48
xbar2 <- 104.8
sd2 <- 12.5

# sd1 = sd2
ci.diff.in.means(n1=n1, xbar1=xbar1, sd1=sd1, n2=n2, xbar2=xbar2, sd2=sd1, equal.var=TRUE)
# sd1 != sd2
ci.diff.in.means(n1=n1, xbar1=xbar1, sd1=sd1, n2=n2, xbar2=xbar2, sd2=sd2, equal.var=FALSE)

#
9.109996/8.022219


# just check for known values, originally taken from
# https://onlinecourses.science.psu.edu/stat414/node/203
#n1 <- 10
#n2 <- 10
#xbar1 <- 10.26
#xbar2 <- 9.02
#sd1 <- 2.51
#sd2 <- 1.90
#ci.diff.in.means(n1=n1, xbar1=xbar1, sd1=sd1, n2=n2, xbar2=xbar2, sd2=sd2, equal.var=FALSE)

# values
n1 <- 50
xbar1 <- 100
sd1 <- 10
n2 <- 48
xbar2 <- 104.8
sd2 <- 12.5

ci.diff.in.means(n1=n1, xbar1=xbar1, sd1=sd1, n2=n2, xbar2=xbar2, sd2=sd1, equal.var=TRUE)
ci.diff.in.means(n1=n1, xbar1=xbar1, sd1=sd1, n2=n2, xbar2=xbar2, sd2=sd2, equal.var=FALSE)

# simulation mean differences between two samples
n1 <- 50
mu1 <- 100
sigma1 <- 10
n2 <- 48
mu2 <- 104.8
sigma2 <- 12.5
trials <- 1000
min.n1 <- 5
min.n2 <- 5
max.n1 <- 100
max.n2 <- 100

random.n <- FALSE

seed <- 9876
set.seed(seed)

# actual simulation
res <- do.call("rbind", lapply(seq_along(1:trials), function(i)
{
 # create random sample sizes
 if(random.n == TRUE)
 {
  n1 <- sample(min.n1:max.n1, size=1)
  n2 <- sample(min.n2:max.n2, size=1)
 } 
 samp1 <- rnorm(n=n1, mean=mu1, sd=sigma1)
 samp2 <- rnorm(n=n2, mean=mu2, sd=sigma2)
 xbar1 <- mean(samp1)
 sd1 <- sd(samp1)
 xbar2 <- mean(samp2)
 sd2 <- sd(samp2)
 ci.dim.res <- ci.diff.in.means(n1=n1, xbar1=xbar1, sd1=sd1, n2=n2, xbar2=xbar2, sd2=sd2, equal.var=FALSE)
 data.frame(n1, n2, "xbar1"=xbar1, "xbar2"=xbar2, "s1"=sd1, "s2"=sd2, ci.dim.res[3,][-6], check.names=FALSE)
}))
rownames(res) <- 1:dim(res)[1]

# results
head(res)
tail(res)

# show plot and results
res.plot <- plot.CI(res=res, trials=trials)
# only results
res.plot

#compare simulation with single trial
digits <- 3
cat(paste("\n",round(res.plot$delta.mean,digits)," [",round(res.plot$ci.low,digits),"; ",round(res.plot$ci.up,digits),"] [",round(res.plot$ci.up-res.plot$ci.low,digits),"]\n\n",sep=""))
ci.diff.in.means(n1=n1, xbar1=xbar1, sd1=sd1, n2=n2, xbar2=xbar2, sd2=sd2, equal.var=FALSE)

