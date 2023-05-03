#file: ptII_quan_classicstats_N-P_nulldist-hypotest.r
#location:
#Die Vollkostenrechnung

#load necessary libs
library(pwr)

# load helper functions
source("ptII_quan_classicstats_N-P_nulldist-hypotest_helpfuncs.r")


#normal case
#two sided
plot.H0(mu0=0, sigma0=1, alternative="two.sided")

#one sided, less 
plot.H0(mu0=2, sigma0=2, alternative="less")

#one sided, greater
plot.H0(mu0=3, sigma0=3, alternative="greater")
plot.H0(mu0=99, sigma0=6, alternative="greater")


#t case
#two sided
plot.H0(mu0=0, N=30, type="t", alternative="two.sided")

#one sided, less 
plot.H0(mu0=2, N=30, type="t", alternative="less")

#one sided, greater
plot.H0(mu0=3, N=30, type="t", alternative="greater")
plot.H0(mu0=90, N=9000, type="t", alternative="greater")

plot.H0(mu0=-2, N=4, type="t", alternative="greater")

plot.H0(mu0=90, N=4, type="t", alternative="greater")
plot.H0(mu0=90, N=4, type="t", alternative="less")
plot.H0(mu0=90, N=4, type="t", alternative="two.sided")



#different calls 
alpha.err <- 0.05
n1 <- 50
n2 <- 60
mu1 <- 0
sigma1 <- 1
mu2 <- 2
sigma2 <- 1

#t
type <- "t"

delta <- NA
beta.err <- NULL

alternative <- "two.sided"
plot.ab.err(n1=n1, n2=n2, mu1=mu1, sigma1=sigma1, sigma2=sigma2, delta=delta, alpha.err=alpha.err, beta.err=beta.err, type=type, alternative=alternative)

alternative <- "greater"
plot.ab.err(n1=n1, n2=n2, mu1=mu1, sigma1=sigma1, sigma2=sigma2, delta=delta, alpha.err=alpha.err, beta.err=beta.err, type=type, alternative=alternative)

alternative <- "less"
plot.ab.err(n1=n1, n2=n2, mu1=mu1, sigma1=sigma1, sigma2=sigma2, delta=delta, alpha.err=alpha.err, beta.err=beta.err, type=type, alternative=alternative)


#beta given, no mu1/ mu2/ delta
#delta = ?
beta.err <- 0.2
mu1 <- mu2 <- NA
delta <- NULL

alternative <- "two.sided"
plot.ab.err(n1=n1, n2=n2, mu1=mu1, sigma1=sigma1, sigma2=sigma2, delta=delta, alpha.err=alpha.err, beta.err=beta.err, type=type, alternative=alternative)

alternative <- "greater"
plot.ab.err(n1=n1, n2=n2, mu1=mu1, sigma1=sigma1, sigma2=sigma2, delta=delta, alpha.err=alpha.err, beta.err=beta.err, type=type, alternative=alternative)

alternative <- "less"
plot.ab.err(n1=n1, n2=n2, mu1=mu1, sigma1=sigma1, sigma2=sigma2, delta=delta, alpha.err=alpha.err, beta.err=beta.err, type=type, alternative=alternative)


#
beta.err <- 0.001

alternative <- "two.sided"
plot.ab.err(n1=n1, n2=n2, mu1=mu1, sigma1=sigma1, sigma2=sigma2, delta=delta, alpha.err=alpha.err, beta.err=beta.err, type=type, alternative=alternative)

alternative <- "greater"
plot.ab.err(n1=n1, n2=n2, mu1=mu1, sigma1=sigma1, sigma2=sigma2, delta=delta, alpha.err=alpha.err, beta.err=beta.err, type=type, alternative=alternative)

alternative <- "less"
plot.ab.err(n1=n1, n2=n2, mu1=mu1, sigma1=sigma1, sigma2=sigma2, delta=delta, alpha.err=alpha.err, beta.err=beta.err, type=type, alternative=alternative)


#
alpha.err <- 0.3

alternative <- "two.sided"
plot.ab.err(n1=n1, n2=n2, mu1=mu1, sigma1=sigma1, sigma2=sigma2, delta=delta, alpha.err=alpha.err, beta.err=beta.err, type=type, alternative=alternative)

alternative <- "greater"
plot.ab.err(n1=n1, n2=n2, mu1=mu1, sigma1=sigma1, sigma2=sigma2, delta=delta, alpha.err=alpha.err, beta.err=beta.err, type=type, alternative=alternative)

alternative <- "less"
plot.ab.err(n1=n1, n2=n2, mu1=mu1, sigma1=sigma1, sigma2=sigma2, delta=delta, alpha.err=alpha.err, beta.err=beta.err, type=type, alternative=alternative)


# created error ("strict" mode in power.t.test())
beta.err <- 0.7

alternative <- "two.sided"
plot.ab.err(n1=n1, n2=n2, mu1=mu1, sigma1=sigma1, sigma2=sigma2, delta=delta, alpha.err=alpha.err, beta.err=beta.err, type=type, alternative=alternative)

alternative <- "greater"
plot.ab.err(n1=n1, n2=n2, mu1=mu1, sigma1=sigma1, sigma2=sigma2, delta=delta, alpha.err=alpha.err, beta.err=beta.err, type=type, alternative=alternative)

alternative <- "less"
plot.ab.err(n1=n1, n2=n2, mu1=mu1, sigma1=sigma1, sigma2=sigma2, delta=delta, alpha.err=alpha.err, beta.err=beta.err, type=type, alternative=alternative)






######################### not run below this point

###not run, for reference
pwr.t.test(d=NULL, n=55, sig.level=.3, power=.3, alternative="two.sided", type="two.sample")
pwr.t2n.test(d=NULL, n1=50,n2=60, sig.level=.3, power=.3, alternative="two.sided")
power.t.test(delta=NULL, n=55, sig.level=.3, sd=1, power=.3, alternative="two.sided", type="two.sample")
power.t.testX(delta=NULL, n1=50, n2=60, sig.level=.3, sd1=1, sd2=1, power=.3, alternative="two.sided", type="two.sample")


plot.ab.err(n1=n1, n2=n2, mu1=mu1, sigma1=sigma1, sigma2=sigma2, delta=NULL, alpha.err=alpha.err, beta.err=0.0001, type="t", alternative="two.sided")

beta.err <- 0.001
#delta = ?
plot.ab.err(n1=n1, n2=n2, mu1=mu1, sigma1=sigma1, sigma2=sigma2, delta=NULL, alpha.err=alpha.err, beta.err=beta.err, type="t", alternative="two.sided")

#n
plot.ab.err(n1=n1, n2=n2, mu1=mu1, sigma1=sigma1, sigma2=sigma2, delta=NULL, alpha.err=alpha.err, beta.err=beta.err, type="n", alternative="two.sided")

#one-sided
beta.err <- 0.2
plot.ab.err(n1=n1, n2=n2, mu1=mu1, sigma1=sigma1, sigma2=sigma2, delta=NULL, alpha.err=alpha.err, beta.err=beta.err, type="t", alternative="greater")


#change sigma
sigma2 <- 2
plot.ab.err(N=N, mu1=mu1, sigma1=sigma1, sigma2=sigma2, delta=delta)

#change mu2 via delta (=non-central parameter)
delta <- 7
plot.ab.err(N=N, mu1=mu1, sigma1=sigma1, sigma2=sigma2, delta=delta)
   
#
z <- ES/(sigma/sqrt(n))
z <- ES/SE
SE <- sigma/sqrt(n)


############# tweaked power.t.test() taken from R
power.t.testX <- function (n1 = NULL, n2 = NULL, delta = NULL, sd1 = 1, sd2 = NULL, sig.level = 0.05, power = NULL, 
    type = c("two.sample", "one.sample", "paired"), alternative = c("two.sided", 
        "one.sided"), strict = FALSE, tol = .Machine$double.eps^0.25, var.eq = TRUE)
{
    if (sum(sapply(list(n1, n2, delta, sd1, sd2, power, sig.level), is.null)) != 
        1) 
        stop("exactly one of '[n1, n2]', 'delta', '[sd1, sd2]', 'power', and 'sig.level' must be NULL")
    if (!is.null(sig.level) && !is.numeric(sig.level) || any(0 > 
        sig.level | sig.level > 1)) 
        stop("'sig.level' must be numeric in [0, 1]")
    type <- match.arg(type)
    alternative <- match.arg(alternative)
    tsample <- switch(type, one.sample = 1, two.sample = 2, paired = 1)
    tside <- switch(alternative, one.sided = 1, two.sided = 2)
    
	if(is.null(sd2)) sd2 <- sd1
	if(is.null(n2)) n2 <- n1
	if(sd1 != sd2)
	{
	 var.eq <- FALSE
	 sd.pooled <- sqrt((sd1^2 + sd2^2)/2)
	 nu <- (sd1^2/n1 + sd2^2/n2)^2 / ( 1/(n1-1)*(sd1^2/n1)^2 + 1/(n2-1)*(sd2^2/n2)^2 )
	} else sd.pooled <- sd1
	
	if(n1 != n2)
	{
	 n <- (n1 + n2)/2
	 nu <- (sd1^2/n1 + sd2^2/n2)^2 / ( 1/(n1-1)*(sd1^2/n1)^2 + 1/(n2-1)*(sd2^2/n2)^2 )
	} else
    {
     n <- n1
	 nu <- (n - 1) * tsample#n1 + n2 - 2
	} 
	
	if (tside == 2 && !is.null(delta)) 
        delta <- abs(delta)
    p.body <- if (strict && tside == 2) 
        quote({
            #nu <- (n - 1) * tsample
            qu <- qt(sig.level/tside, nu, lower.tail = FALSE)
            pt(qu, nu, ncp = sqrt(n/tsample) * delta/sd.pooled, lower.tail = FALSE) + 
                pt(-qu, nu, ncp = sqrt(n/tsample) * delta/sd.pooled, 
                  lower.tail = TRUE)
        })
    else quote({
        #nu <- (n - 1) * tsample
        pt(qt(sig.level/tside, nu, lower.tail = FALSE), nu, ncp = sqrt(n/tsample) * 
            delta/sd.pooled, lower.tail = FALSE)
    })
    if (is.null(power)) 
        power <- eval(p.body)
    else if (is.null(n)) 
        n <- uniroot(function(n) eval(p.body) - power, c(2, 1e+07), 
            tol = tol, extendInt = "upX")$root
    else if (is.null(sd.pooled)) 
        sd.pooled <- uniroot(function(sd.pooled) eval(p.body) - power, delta * 
            c(1e-07, 1e+07), tol = tol, extendInt = "downX")$root
    else if (is.null(delta)) 
        delta <- uniroot(function(delta) eval(p.body) - power, 
            sd.pooled * c(1e-07, 1e+07), tol = tol, extendInt = "upX")$root
    else if (is.null(sig.level)) 
        sig.level <- uniroot(function(sig.level) eval(p.body) - 
            power, c(1e-10, 1 - 1e-10), tol = tol, extendInt = "yes")$root
    else stop("internal error", domain = NA)
    NOTE <- switch(type, paired = "n is number of *pairs*, sd.pooled is std.dev. of *differences* within pairs", 
        two.sample = "n is number in *each* group", NULL)
    METHOD <- paste(switch(type, one.sample = "One-sample", two.sample = "Two-sample", 
        paired = "Paired"), "t test power calculation")
    structure(list("n [mean/sample]" = n, n1 = n1, n2 = n2, nu = nu, delta = delta, "sd [pooled]" = sd.pooled, sig.level = sig.level, 
        power = power, alternative = alternative, "var [equal]" = var.eq, note = NOTE, 
        method = METHOD), class = "power.htest")
}



