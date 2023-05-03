# file:
# ptII_quan_classicstats_nullritual.r

# location:
# chap. 4 [4.5.7]
# Der Ablauf eines statistischen Tests — das Nullritual


# null ritual
# simple and dumb version of NULL ritual
test.sig.NULLritual <- function(p.value, crit.sig)
{
 if(p.value < crit.sig) cat(paste("\nWELCOME TO THE NULL RITUAL\n\nThe test against NULL (= NHST) is statistically significant with p < ",crit.sig,".\nYou can reject H0.\n\n",sep=""))
 else cat(paste("\nWELCOME TO THE NULL RITUAL\n\nThe test against NULL (= NHST) is *not* statistically significant with p < ",crit.sig,".\nYou cannot reject H0.\n\n",sep=""))
}

# define level of significance in accordance to lobbyism and bad habits
crit.sig <- 0.05
# create some data
a <- c(1,1,4,7,3,6,2,5,3,3)
b <- c(6,9,4,8,4,3,12,1,11,6)
ttest.ab.res <- t.test(a,b)
ttest.ab.res
test.sig.NULLritual(p.value=ttest.ab.res$p.value, crit.sig=crit.sig)

cor.ab.res <- cor.test(a,b)
cor.ab.res
test.sig.NULLritual(p.value=cor.ab.res$p.value, crit.sig=crit.sig)

# Fisher's null hypothesis testing
# same data as above
# late Fisher, report exact p-value
cat(paste("\nTHE LATE FISHER\n\nThe test resulted in an exact p-value of p = ",ttest.ab.res$p.value,".\nNow hopefully you have learned something about the research study,\nthe design, the data, and your theory --- Use your brain!\n\n",sep=""))

# Neyman-Pearson
digits <- 2
alpha <- 0.05
seed <- 9876
set.seed(seed)
aprioripw.N <- power.t.test(n=NULL, delta=1.6, sd=2, sig.level=alpha, power=0.8, type=c("two.sample"), alternative=c("two.sided"))
aprioripw.N

a <- round(rnorm(n=25, mean=6.5, sd=2))
b <- round(rnorm(n=25, mean=7.8, sd=2))
describes(data.frame(a,b))
t.test(a,b)
DiM <- mean(a)-mean(b)
DiM
cohensd(b,a)

# simple and dumb N-P version of a significance test
test.sig.NP <- function(p.value, alpha)
{
 cat(paste("\nNEYMAN-PEARSON DECISION THEORY\n\nThe test decides based on a priori power analysis:\n",sep=""))
 print(aprioripw.N)
 if(p.value < alpha) note <- paste("\np < alpha, therefor accept H2.\n\n",sep="") else note <- paste("\np >= alpha, therefor accept **H1**.\n\n",sep="")
 method <- c("Hypothesis testing according to Neyman-Pearson")
 print(structure(list("Cohen's delta"=round(cohensd(b,a)[2],digits),alpha=alpha,"p < alpha"=p.value < alpha,
       method=method, note=note), class="power.htest"))
}
test.sig.NP(p.value=ttest.ab.res$p.value, alpha=alpha)



