# file:
# ptII_quan_Bayes_simple-estimation-mean-post_helpfuncs.r
# location:
# chap. 6 [6.5.1]
# Intuitives Verständnis von Wahrscheinlichkeit

# load necessary libs
library(Bolstad)

# HELPER FUNCTIONS


###### function to plot mean posterior along with prior and likelihood
plot.mean.post <- function(res=NULL, TITLE="Bayes mean estimation", SUB="prior, likelihood, posterior",
                           xtext=expression(mu), ytext=expression(probability(mu)), colo=c("olivedrab","violetred3","blue"),
						               xlim=NULL, ylim=NULL, scalelikeli=TRUE, scaleprior=TRUE,
            						   add=FALSE, refacX=FALSE, refacY=TRUE, fac=1.2)
{
 stopifnot(attr(res, "class") == "Bolstad")
 
 if(scalelikeli) res$likelihood <- res$likelihood * (max(res$posterior)/max(res$likelihood))
 if(scaleprior) res$prior <- res$prior * (max(res$posterior)/max(res$prior))
 
 if(add == FALSE)
 {
  par(mar=c(5,5,4,2))
  par(oma=c(2,1,1,1))
  par("cex.axis"=0.8)
 }

 if(is.null(xlim)) xlim <- range(res$param.x)
 if(refacX) xlim <- xlim*fac
 if(is.null(ylim)) ylim <- range(c(res$prior,res$likelihood,res$post))
 if(refacY) ylim <- ylim*fac
 
 plot(0,0, xlim=xlim, ylim=ylim, main="", xlab="", ylab="", cex.lab=0.8, cex.axis=0.8, bty="n", axes=F, col="white")

 axis(side = 1, pretty(xlim), tck =- .02, labels=NA, line=.6)
 axis(side = 2, pretty(ylim), tck = -.02, labels=NA, line=.6)
 axis(side = 1, lwd = 0, line = .4)
 axis(side = 2, lwd = 0, line = .4, las = 1)

 grid(col="grey90", lwd=1.2, lty=2)
 mtext(xtext, 1, line=4, cex=1)
 mtext(ytext, 2, line=4, cex=1) 
 
 mtext(TITLE, 3, line=2, cex=1.5)
 mtext(SUB, 3, line=.4, cex=1.1)
 
 lines(res$param.x, res$prior, col=colo[1], lty=3)
 lines(res$param.x, res$likelihood, col=colo[2], lty=2)
 lines(res$param.x, res$posterior, col=colo[3], lty=1)

 legend("topright", c("prior","likelihood","posterior"),
        col=colo, lty=c(3,2,1), text.col=colo, bty="n", bg="white", xjust=0, lwd=2)
}
# call:
# plot.mean.post(bayes.mean.res)
########################## END OF FUNCTION


###### function to reproduce original from 'Bolstad' ->> without bug in first line
# seems not to be necessary anymore with newer versions of 'Bolstad'
normgcp.alt <- function (x, sigma.x = NULL, density = c("normal", "uniform", 
    "user"), params = NULL, n.mu = 50, mu = NULL, mu.prior = NULL, 
    plot = TRUE) 
{
    mean.x = mean(x)
    if (n.mu < 3) 
        stop("Number of prior values of mu must be greater than 2")
    if (is.null(sigma.x)) {
        sigma.x = sd(x - mean.x)
        cat(paste("Standard deviation of the residuals :", signif(sigma.x, 
            4), "\n", sep = ""))
    }
    else {
        cat(paste("Known standard deviation :", signif(sigma.x, 
            4), "\n", sep = ""))
    }
    density = match.arg(density)
    if (grepl("^n(orm(al)*)*$", density)) {
        if (is.null(params) | length(params) < 1) 
            stop("You must supply a mean for a normal prior")
        mx = params[1]
        if (length(params) == 2) 
            s.x = params[2]
        else s.x = sigma.x
        mu = seq(mx - 3.5 * s.x, mx + 3.5 * s.x, length = n.mu)
        mu.prior = dnorm(mu, mx, s.x)
    }
    else if (grepl("^u(nif(orm)*)*$", density)) {
        if (is.null(params)) {
            params = c(mean.x - 3.5 * sigma.x, mean.x + 3.5 * 
                sigma.x)
        }
        if (length(params) < 2) 
            stop("You must supply a minimum and a maximum to use a uniform prior")
        minx = params[1]
        maxx = params[2]
        if (maxx <= minx) 
            stop("The maximum must be greater than the minimum for a uniform prior")
        mu = seq(minx, maxx, length = n.mu)
        mu.prior = dunif(mu, minx, maxx)
    }
    else {
        if (is.null(mu) | is.null(mu.prior)) 
            stop("If you wish to use a non-uniform continuous prior then you must supply a mean vector, mu, and an associated density vector, mu.prior")
        if (is.function(mu.prior)) 
            mu.prior = mu.prior(mu)
    }
    if (any(mu.prior < 0)) 
        stop("Prior densities must be >=0")
    crude.int = sum(diff(mu) * mu.prior[-1])
    if (round(crude.int, 3) != 1) {
        warning("The prior probabilities did not sum to 1, therefore the prior has been normalized")
        mu.prior = mu.prior/crude.int
        print(crude.int)
    }
    n.mu = length(mu)
    mx = mean(x)
    nx = length(x)
    snx = sigma.x^2/nx
    likelihood = exp(-0.5 * (mx - mu)^2/snx)
    f.x.mu = likelihood * mu.prior
    ap = approx(mu, f.x.mu, n = 513)
    integral = sum(ap$y[2 * (1:256) - 1] + 4 * ap$y[2 * (1:256)] + 
        ap$y[2 * (1:256) + 1])
    integral = (ap$x[2] - ap$x[1]) * integral/3
    posterior = likelihood * mu.prior/integral
    if (plot) {
        plot(mu, posterior, ylim = c(0, 1.1 * max(posterior, 
            mu.prior)), type = "l", lty = 1, col = "blue", xlab = expression(mu), 
            ylab = expression(Probabilty(mu)))
        lines(mu, mu.prior, lty = 2, col = "red")
        legend("topleft", bty = "n", cex = 0.7, lty = 1:2, col = c("blue", 
            "red"), legend = c("Posterior", "Prior"))
    }
    results = list(name = "mu", param.x = mu, prior = mu.prior, 
        likelihood = likelihood, posterior = posterior, mu = mu, 
        mu.prior = mu.prior)
    class(results) = "Bolstad"
    invisible(results)
}
# call:
# see ?normgcp
########################## END OF FUNCTION


