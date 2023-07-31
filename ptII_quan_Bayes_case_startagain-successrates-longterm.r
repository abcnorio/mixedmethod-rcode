### (C) 2005-2023 by Leo Guertler 
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
# ptII_quan_Bayes_case_startagain-successrates-longterm.r

# location:
# chap. 6 [6.15.2.1]
# Langzeitevaluation (Durchlaufquoten)

# load necessary helper functions
source("ptall_generalfuncs_Bayes_binomial-prop-test.r")


# read source tab
sa <- read.table("startagain_statistics_1992-2017.tab",sep="\t",header=TRUE)
dim(sa)
sa

# correct for parts of a year (first and last year, ie. 1992 and 2017)
mpy <- 12 # months per year
sa.cor <- cbind(year=sa[,"year"],sa[,c("IN","OUT","s","f","indiff")]/sa[,"mpyear"]*mpy)
sa.cor

# conservative: failure = failure + indiff
f.indiff <- rowSums(sa.cor[,c("f","indiff")])
# less conservative: success = success + indiff
s.indiff <- rowSums(sa.cor[,c("s","indiff")])

# N per year
N <- rowSums(sa.cor[,c("s","f","indiff")])
sa.cor.enh <- data.frame(sa.cor, f.indiff, s.indiff, N)
sa.cor.enh

# cumsums
sa.cor.enh.cs <- apply(sa.cor.enh[,-1], 2, cumsum)
colnames(sa.cor.enh.cs) <- paste(colnames(sa.cor.enh.cs),".cs", sep="")
sa.cor.enh.cs
sa.all <- data.frame(sa.cor.enh, sa.cor.enh.cs)
sa.all


# here: FIRST always use the same PRIOR!!!
# SECOND: learning from experience!
# conservative perspective: we use 's.cs' and 'N.cs'

# sa.l <- dim(sa.cor.enh.cs.all)
# prepare lists
sa.l <- dim(sa.all)
sa.l
res.bino.hdi.EXP <- res.bino.sum.EXP <- res.bino.hdi <- res.bino.sum <- res.sum <- res.hdi.BL  <- res.hdi.JC <- list()

theta.prior <- 0.5
nprior <- 2
# calculate everything
for(i in 1:sa.l[1])
{
  #print(i)
  temp <- sN.post.su(Ni=sa.all[i,"N.cs"], si=sa.all[i,"s.cs"], rn=sa.all[i,"year"], printout=FALSE)
  res.sum[[i]] <- temp[["res"]]
  res.hdi.BL[[i]] <- temp[["hdi.BL"]]
  res.hdi.JC[[i]] <- temp[["hdi.JC"]]
  #uniform prior = identical to JC
  temp <- bino.abs(si=sa.all[i,"s.cs"], Ni=sa.all[i,"N.cs"], theta.prior=theta.prior, nprior=nprior, rn=sa.all[i,"year"], graph=FALSE)
  res.bino.sum[[i]] <- temp[["res"]]
  res.bino.hdi[[i]] <- temp[["hdi"]]
  #learning from experience ie. prior = posterior[i-1]
  if(i==1)
  {
    temp.EXP <- bino.abs(si=sa.all[i,"s.cs"], Ni=sa.all[i,"N.cs"], theta.prior=theta.prior, nprior=nprior, rn=sa.all[i,"year"], graph=FALSE)
  } else
  {
    theta.prior <- NULL
    nprior <- NULL
    a.prior <- res.bino.sum.EXP[[i-1]][,"a.post"]
    b.prior <- res.bino.sum.EXP[[i-1]][,"b.post"]
    temp.EXP <- bino.abs(si=sa.all[i,"s"], Ni=sa.all[i,"N"], a.prior=a.prior, b.prior=b.prior, rn=sa.all[i,"year"], graph=FALSE)
  }  
  res.bino.sum.EXP[[i]] <- temp.EXP[["res"]]
  res.bino.hdi.EXP[[i]] <- temp.EXP[["hdi"]]  
}

# create tables
sa.res <- do.call("rbind",res.sum)
sa.hdi.BL <- do.call("rbind",res.hdi.BL)
sa.hdi.JC <- do.call("rbind",res.hdi.JC)
sa.bino <- do.call("rbind",res.bino.sum)
sa.bino.EXP <- do.call("rbind",res.bino.sum.EXP)
sa.bino.hdi.EXP <- do.call("rbind",res.bino.hdi.EXP)

#... have a look
head(sa.res)
tail(sa.res)

head(sa.hdi.BL)
tail(sa.hdi.BL)

head(sa.hdi.JC)
tail(sa.hdi.JC)

head(sa.bino)
tail(sa.bino)

head(sa.bino.hdi)
tail(sa.bino.hdi)

head(sa.bino.EXP)
tail(sa.bino.EXP)

head(sa.bino.hdi.EXP)
tail(sa.bino.hdi.EXP)

# plot change/ development of mean, sd, and ratio mean/sd
sN.sum.plot(tab=sa.res, TITLE="Development success rates (start again 1992-2017)", xlab="year", type="l")


# write down everything into tables
rownames(sa.all) <- 1:dim(sa.all)[1]
rownames(sa.res) <- 1:dim(sa.res)[1]
rownames(sa.hdi.BL) <- 1:dim(sa.hdi.BL)[1]
rownames(sa.hdi.JC) <- 1:dim(sa.hdi.JC)[1]
rownames(sa.bino) <- 1:dim(sa.bino)[1]
rownames(sa.bino.hdi) <- 1:dim(sa.bino.hdi)[1]
rownames(sa.bino.EXP) <- 1:dim(sa.bino.EXP)[1]
rownames(sa.bino.hdi.EXP) <- 1:dim(sa.bino.hdi.EXP)[1]

# write everything in tables...
write.table(sa.all,"sa.all.csv",sep="\t",col.names=TRUE)
write.table(sa.res,"sa.res.csv",sep="\t",col.names=TRUE)
write.table(sa.hdi.BL,"sa.hdi.BL.csv",sep="\t",col.names=TRUE)
write.table(sa.hdi.JC,"sa.hdi.JC.csv",sep="\t",col.names=TRUE)
write.table(sa.bino,"sa.bino.csv",sep="\t",col.names=TRUE)
write.table(sa.bino.hdi,"sa.bino.hdi.csv",sep="\t",col.names=TRUE)
write.table(sa.bino.EXP,"sa.bino.EXP.csv",sep="\t",col.names=TRUE)
write.table(sa.bino.hdi.EXP,"sa.bino.hdi.EXP.csv",sep="\t",col.names=TRUE)

# demonstrate changes in prior, likelihood, posterior
# start again data 1992-2017
sa.all
sa.res

# pre year
sa.res.py <- matrix(data=NA, nrow=dim(sa.res)[1], ncol=6,
                   dimnames=list(rownames(sa.res),
				 c("a.prior","b.prior","a.lik","b.lik","a.post","b.post"))
				 )
sa.res.py <- data.frame(sa.res.py)
head(sa.res.py)
dim(sa.res.py)

# 1,1 = uniform
# .5,.5 = Jeffrey's
# .35 = 7,20 = informed weaker success prior
# .35*20=7
# .55 = 11,20 = informed stronger success prior
# .55*20 = 11
# .75 = 15,20 = unreal stronger success prior
# .75*20 = 15
# total domination: 1900,2000
# sa.res.py[1,"a.prior"] <- 1
# sa.res.py[1,"b.prior"] <- 1
ab.likelis <- do.call("cbind", bino.ab.lik(sa.all["s"],sa.all["N"]))
ab.likelis
sa.res.py[,c("a.lik","b.lik")] <- ab.likelis
sa.res.py

# choose different priors
sa.res.py[1,c("a.prior","b.prior")] <- c(1,1)
sa.res.py
sa.res.py[1,c("a.prior","b.prior")] <- c(0.01,0.01)
sa.res.py
sa.res.py[1,c("a.prior","b.prior")] <- c(0.5,0.5)
sa.res.py

# calculate following priors and posteriors
# same prior
sa.res.sp <- betabinomial.lbyxp(sa.res.py=sa.res.py, prior=list(a=1,b=1), sameprior=TRUE)
# prior[i] = posterior[i-1]
sa.res.dp <- betabinomial.lbyxp(sa.res.py=sa.res.py, prior=list(a=1,b=1))

# collect and merge infos
abc <- do.call("cbind", beta.summary(a=sa.res.dp[,"a.post"], b=sa.res.dp[,"b.post"]))
colnames(abc) <- c("a.post","b.post","mode.post","mean.post","sd.post","var.post")
res.sa <- data.frame(sa.all, sa.res.dp, abc)

# same prior
sa.res.sp
# different prior
sa.res.dp
# all results
res.sa

# one plot after each other
par(ask=TRUE)
for(i in 1:26)
{
 beta.triplot(si=res.sa[i,"s"], Ni=res.sa[i,"N"], v=res.sa[i,], multiplot=FALSE, rn=res.sa[i,"year"])
}

# everything on one plot
par(mfrow=c(5,6))  # not 6,5 -> margins too large... error
for(i in 1:26)
{
 beta.triplot(si=res.sa[i,"s"], Ni=res.sa[i,"N"], v=res.sa[i,], multiplot=TRUE, rn=res.sa[i,"year"])
}
plot.new()
legend("center",legend=c("prior","likelihood","posterior"),
       xpd=TRUE, horiz=FALSE, inset=c(0,0), y.intersp=1,
       col=c("blue","green","red"), lty=c(2,3,1), lwd=1.9, bty="n", cex=2) 
plot.new()
mtext(side=1,"Development\nsuccess rates\nstart again\n\n1992-2017", cex=1.2)

