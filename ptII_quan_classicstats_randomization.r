# file:
# ptII_quan_classicstats_randomization.r

# location:
# chap. 4 [4.6.6]
# Randomisierung

# load required libraries
library(randomizr)
library(randomizeR)

# sample
nsamples <- 2
npersample <- 30
stotal <- nsamples*npersample

seed <- 07987
set.seed(seed)

# randomize sample to create two arbitrary samples
# replicate a random draw from a uniform distribution
rnd1.s <- replicate(stotal, round(runif(1),0))
# or that way (sample with replacement)
rnd2.s <- sample(c(0,1), stotal, replace=TRUE)

# or that way (permutation, sample without replacement)
rnd3.perm <- sample(stotal, stotal, replace=FALSE)
# see sorted
sort(rnd3.perm)
group <- rep(c(0,1),each=npersample)
# rnd3.s <- data.frame(order(rnd3.perm, group),group)
rnd3.s <- group[order(rnd3.perm,group)]

# all together
rnd123.tab <- data.frame(rnd1.s, rnd2.s, rnd3.s)
head(rnd123.tab)
tail(rnd123.tab)

# check probs
# zeros
apply(rnd123.tab,2,function(x) abs(sum(x-1)/(stotal)))
# ones
apply(rnd123.tab,2,function(x) sum(x)/(stotal))

# sample with different probs
probs <- c(0.25, 0.5, 0.25)
items <- c("1|low","2|middle","3|high")
data.frame(probs,items)
reps <- 1e5
rnd4.s <- sample(items, reps, replace=TRUE, prob=probs)
head(rnd4.s)
tail(rnd4.s)
prop.table(table(rnd4.s))
1-(prop.table(table(rnd4.s))/probs)


# library 'randomzr'
# https://declaredesign.org/r/randomizr/
# https://declaredesign.org/r/randomizr/articles/randomizr_vignette.html

#library(randomizr)
# full randomization according to probs / percentages
rand1 <- complete_ra(N=100, m=33)
table(rand1)
rand2 <- complete_ra(N=100, m=50)
table(rand2)

# clusters
clust.num <- rep((letters)[26:17], times=1:10)
clust.num
table(clust.num)
length(clust.num)
rand2 <- cluster_ra(cluster=clust.num, m_each=c(2,5,2,1), conditions=c("control","placebo","treat1","treat2"))
table(rand2, clust.num)
# add margins
addmargins(table(rand2, clust.num),c(1,2))
sum(1:10)
sum(c(2,5,2,1))
c(2,5,2,1)/sum(c(2,5,2,1))
rowSums(table(rand2, clust.num))
rowSums(table(rand2, clust.num))/sum(rowSums(table(rand2, clust.num)))


#library('randomizeR)
n1 <- 100
n2 <- 10

# complete randomization
params.cr <- crPar(n1)
# permuted block randomization
params.pbr <- pbrPar(n2)
params.cr
params.pbr

# set reference size
rsize <- 1e4

# generate sequences
cr.seq <- genSeq(params.cr, r=rsize, seed=seed)
cr.seq
str(cr.seq)

pbr.seq <- genSeq(params.pbr, r=rsize, seed=seed)
pbr.seq
str(pbr.seq)
# for small sizes complete is possible
# = power set count
pbr.all.seq <- getAllSeq(params.pbr)
pbr.all.seq
str(pbr.all.seq)
pbr.all.seq@M

cr.all.seq <- getAllSeq(crPar(n2))
cr.all.seq
str(cr.all.seq)
cr.all.seq@M

getRandList(pbr.all.seq)

# dimensions
dim(pbr.all.seq@M)
dim(cr.all.seq@M)




###### function to probabilies of sequences
seqAprobs <- function(seque)
{
 seqscollapse <- apply(getRandList(seque),1,function(x) paste(x,collapse=""))
 ps <- getProb(seque)
 return(data.frame(sequences=seqscollapse, probs=ps))
}
#call:
# seqAprobs(seque=pbr.all.seq)
########################## END OF FUNCTION


seqAprobs.pbr <- seqAprobs(seque=pbr.all.seq)
head(seqAprobs.pbr)
tail(seqAprobs.pbr)

comp.res0 <- compare(issue=corGuess("CS"), cr.all.seq, pbr.all.seq)
comp.res0

# expected responses and standard deviations in treatmant groups of clinical trials
endpoints <- normEndp(mu=c(2,3), sigma=c(1,1))
comp.res1 <- compare(issue=selBias(type="CS", method="exact", eta=0.2, alpha=0.03), cr.all.seq, pbr.all.seq, endp=endpoints)
comp.res1

plot(comp.res0)
plot(comp.res1)

plot(comp.res0, y="boxplot")
plot(comp.res1, y="boxplot")

