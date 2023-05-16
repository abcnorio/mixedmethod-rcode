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
# ptII_quan_Bayes_Bem-study-aspects.r

# location:
# chap. 6 [6.7.1.5]
# Designkritik — die Studie von Bem


k <- 1000
# replication ability
r <- 0.6
B.med.NOT <- 0.9
B.med <- 1-B.med.NOT
B.spon <- 0.8
qu <- 0.5

N.world <- 7e9

# case meditation
1000*0.6*0.5*(B.med)
n.med <- k*r * qu * B.med
p.med <- n.med/N.world
p.med

# case spontaneous
n.spon <- k*r * qu * B.spon
n.spon
p.spon <- n.spon/N.world
p.spon

# world
n.total <- n.med + n.spon
n.total
p.total <- n.total/N.world
p.total

1/p.total
# = 25'925'925

# USA
N.USA <- 312e6
pc.USA <- N.USA/N.world
pc.USA
n.USA <- pc.USA*n.total
n.USA
p.USA <- pc.USA*n.total/N.USA
p.USA

1/p.USA
# = 25'925'925

# chance to win in lottery
choose(49,6)
p.lottery <- 1/choose(49,6)

p.lottery/p.USA


# Bem study

# N
N.female <- c(107,69,57,63,87,140,64,64)
N.male <- c(43,31,43,37,63,60,36,36)
N.unknown <- 100
N.Bem <- sum(N.female,N.male,N.unknown)
N.Bem
# number of experiments
k <- 9

# required sample for Bem
p.Bem <- (1/p.USA)/N.Bem
p.Bem
1/p.Bem

# meet one person in the sample that fits to criteria
1/choose(p.Bem,1)*100 #percent

# meet all persons in the sample that fit to criteria
1/choose(p.Bem,N.Bem)*100 #percent
# = 0
log(1)-lchoose(p.Bem,N.Bem)+100 #percent
#-4340.611
log(1)-lchoose(p.Bem,N.Bem)
#-4440.611

# meet n=10 persons in the sample that fit to criteria
1/choose(p.Bem,10)
#6.873476e-38
log(1)-lchoose(p.Bem,10)
#-85.57056

