# file:
# ptII_quan_classicstats_N-P_confint-errorbars.r

# location:
# chap. 4 [4.5.2.6]
# Konfidenzintervalle

#load necessary libs
library(psych)
library(Hmisc)

# load helper functions
source("ptII_quan_classicstats_N-P_confint-errorbars_helpfuncs.r")


trials <- 100
pop.mean <- 4.5
pop.sd <- 1.7
sim.res <- CI.evolve(N=30, trials=trials, pop.mean=pop.mean, pop.sd=pop.sd, prob=(prob <- 0.95))

#inspect data
str(sim.res)
head(sim.res$simulation)
tail(sim.res$simulation)
sim.res[-1]

#descriptive statistics
psych:::describe(sim.res$simulation)


#repeat for 100 trials
trials <- 100
sim.res1 <- CI.evolve(N=30, trials=trials, pop.mean=pop.mean, pop.sd=pop.sd, seed=9876)
CI.cover.mean(sim.res=sim.res1)

#repeat for 1000 trials
trials <- 1000
sim.res1 <- CI.evolve(N=30, trials=trials, pop.mean=pop.mean, pop.sd=pop.sd, seed=9876)
CI.cover.mean(sim.res=sim.res1)

#repeat for 10000 trials
digits <- 3
trials <- 10000
sim.res2 <- CI.evolve(N=30, trials=trials, pop.mean=pop.mean, pop.sd=pop.sd, seed=9876)
CI.cover.mean(sim.res=sim.res2, digits=digits, id.out=FALSE)

#repeat for 1 trials
trials <- 1
sim.res1 <- CI.evolve(N=30, trials=trials, pop.mean=pop.mean, pop.sd=pop.sd, seed=9876)
CI.cover.mean(sim.res=sim.res1)


