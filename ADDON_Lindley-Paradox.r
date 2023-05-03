# source
# https://en.wikipedia.org/wiki/Lindley%27s_paradox

gb <- c(boys=49581, girls=48870)
n <- sum(gb)

theta <- 0.5 #H0
#theta != 0.5 #H1

prior.H0 <- 0.5
prior.H1 <- 0.5

k <- gb["boys"]
p.k.H0 <- exp(lchoose(n,k) + log(theta)*(k) + log(1-theta)*(n-k))
p.k.H0

k <- gb["girls"]
p.k.H1 <- exp(lchoose(n,k) + lbeta(k+1,n-k+1))
p.k.H1

totalevid <- p.k.H0*prior.H0 + p.k.H1*prior.H1
names(totalevid) <- "totalevid"
totalevid

post.H0.k <- p.k.H0*prior.H0/totalevid
post.H1.k <- p.k.H1*prior.H1/totalevid

# boys
post.H0.k
# girls
post.H1.k
# compare
post.H0.k > post.H1.k
# post OR
post.H0.k/post.H1.k