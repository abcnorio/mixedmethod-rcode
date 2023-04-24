# file:
# ptII_quan_classicstats_N-P_simulation.r

# location:
# chap. 4 [4.5.4]
# Exkurs — Simulationen


seed <- 9876
set.seed(seed)
sim <- function(N1, N2, mu1, mu2, sd1, sd2)
{
 x1 <- rnorm(N1, mean=mu1, sd=sd1)
 x2 <- rnorm(N2, mean=mu2, sd=sd2)
 mean(x1) - mean(x2)
}
boot.means <- replicate(trials, sim(N1=N1, N2=N2, mu1=mu1, mu2=mu2, sd1=sd1, sd2=sd2))
summary(boot.means)

# plot
par(oma=c(2,1,1,1), "cex.axis"=1, bty="l")
hist(boot.means, prob=TRUE, pre.plot=grid(), col="steelblue", border="white", main="", xlab="Mean Differences", ylab="Density")
lines(density(boot.means), type="l", col="red", lwd=2)
abline(v=mu1-mu2, col="darkred", lty=2, lwd=2)
abline(v=mean(boot.means), col="red", lty=1, lwd=2)
legend("topright", legend=c("TRUE DiM", "Simulation DiM"),
       pch="---", pt.cex=2.5,col=c("red","darkred"), bty="o", bg="white", box.lty=1, box.col="white")
mtext("Simulation Difference in Means", outer=TRUE, line=-2, cex=1.5, side=3)

# true mean difference
mu1 - mu2
# mean simulation
mean(boot.means)
# ratio
abs(1-(mu1-mu2)/mean(boot.means))

set.seed(seed)
trials <- 1e+6
boot.means1 <- replicate(trials, sim(N1=N1, N2=N2, mu1=mu1, mu2=mu2, sd1=sd1, sd2=sd2))
abs(1-(mu1-mu2)/mean(boot.means1))

# not run
boot.means2 <- replicate(trials, mean(sample(sim.diff.means[,"DiM"], size=length(sim.diff.means[,"DiM"]), replace=TRUE)))
abs(1-mean(boot.means1)/mean(boot.means2))

