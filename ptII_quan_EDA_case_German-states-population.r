# file:
# ptII_quan_EDA_case-German-states-population.r

# location:
# chap. 5 [5.5.1]
# Die Population im Bundesländervergleich

# load necessary libs
library(arm)

# load necessary helper functions
source("ptall_generalfuncs.r")

# prepare data
einw2017 <- c(17912,12997,11023,7963,6243,4081,4074,3613,2890,2504,2223,2151,1831,1611,994,681)

bland <- c("Nordrhein-Westfalen","Bayern","Baden-Württemberg","Niedersachsen","Hessen","Sachsen","Rheinland-Pfalz","Berlin","Schlwesig-Holstein","Brandenburg","Sachsen-Anhalt","Thüringen","Hamburg","Mecklenburg-Vorpommern","Saarland","Bremen")
eastwest <- c("W","W","W","W","W","E","W","B","W","E","E","E","W","E","W","W")

# 2016
# km^2 in 1000
area2016 <- c(34097,70551,35751,47634,21114,18415,19853,891,15799,29485,20446,16172,755,23180,2569,419)
# population
einw2016 <- c(17877808,12887133,10915756,7936142,6194630,4083317,4059428,3547431,2870320,2489737,2240861,2164421,1798923,1611518,996124,675121)
# people per km^2
ppkm2.2016 <- c(524,183,306,167,294,221,205,4012,182,84,109,133,2397,69,388,1617)
# deaths
death2016 <- c(202250,129552,106630,92368,64081,53330,45863,34050,33879,30790,31453,28312,17267,20445,12897,7732)
# births
births2016 <- c(173274,125686,107479,75215,60731,37940,37518,41086,25420,20934,18092,18475,21480,13442,8215,7136)
# Nordrhein-Westfalen,Bayern,Baden-Württemberg,Niedersachsen,Hessen,Sachsen,Rheinland-Pfalz,Berlin,Schlwesig-Holstein,Brandenburg,Sachsen-Anhalt,Thüringen,Hamburg,Mecklenburg-Vorpommern,Saarland,Bremen
# mean age
mage2016 <- c(44,43.6,43.3,44.4,43.7,46.7,44.6,42.6,45,46.9,47.5,47,42.1,46.7,46.1,43.6)
poptab <- data.frame(einw2016,area2016,ppkm2.2016,death2016,births2016,mage2016)
rownames(poptab) <- bland
colnames(poptab) <- c("population","km^2","ppkm^2","death","birth","age (mean)")

#write.table(poptab,file="German_states_population.tab", sep="\t", row.names=TRUE, col.names=TRUE)
poptab <- read.table(file="German_states_population.tab", header=TRUE, sep="\t")
poptab


# show tab
poptab
# order along minimum births
poptab[order(poptab$birth),]

# barplot
# https://stackoverflow.com/questions/10286473/rotating-x-axis-labels-in-r-for-barplot
par(mar=c(7,4,2,2)+0.2)
bpl <- barplot(einw2017, col=rainbow(length(einw2017)), border="blue", main="Population German Federal Lands", xlab="")
text(bpl, par("usr")[3], srt=60, adj=1, xpd=TRUE, labels=bland, cex=0.65)


# plot according to east/west/both
par(mar=c(5,4,2,2)+0.2)
fac <- 1.9
bcolo <- c("violetred2","orange","blue")
bpl2 <- barplot(einw2017,col=bcolo[as.factor(eastwest)], border="green", main="Population German Federal Lands",
        xlab="east/ west/ both", legend.text=c("(B)oth","(E)ast","(W)est"),
    		ylab="N (in 1000)",
		    args.legend=list(x="topright", title="Location", cex=.85, bty="n",
    		fill=bcolo, title.col="black",
		    text.col=c("violetred2","orange","blue")))
text(bpl2, par("usr")[3]*fac, srt=0, adj=1, xpd=TRUE, labels=eastwest, cex=.9)


# Tukey's fivenum descriptive statistics
fivenum(einw2017)
# no labes, better use one's own function
fivenum.wn(einw2017)
mean(einw2017)


# plot population for each German Federal Land
bl <- 1:length(bland)
einw2017.lm <- lm(einw2017~bl)
einw2017.log.lm <- lm(log(einw2017) ~ bl)

par(mfrow=c(1,2))
plot(bl,einw2017, bty="n", pre.plot=grid(), main="Population per German Federal Land", pch=21, bg="darkred", col="blue", ylab="population", xlab="German Federal Land")
abline(einw2017.lm, col="violetred2", lty=2)

# plot log(population) for each German Federal Land
plot(bl,log(einw2017), bty="n", pre.plot=grid(), main="Population per German Federal Land", pch=21, bg="violetred2", col="blue", ylab="log(population)", xlab="German Federal Land")
abline(einw2017.log.lm, col="darkred", lty=2, lwd=1.8)


# plot 'lm' next to each other (normal and log-version)
par(mfrow=c(2,4))
plot(einw2017.lm, col="violetred3", pch=21, bg="darkred")
plot(einw2017.log.lm, col="blue", pch=21, bg="blue")


# check linear models
einw2017.lm.sum <- arm:::display(einw2017.lm)
einw2017.log.lm.sum <- arm:::display(einw2017.log.lm)
t.wo.log <- with(einw2017.lm.sum, coef/se)
t.w.log <- with(einw2017.log.lm.sum, coef/se)
t.wo.log
t.w.log
t.w.log/t.wo.log

# compare R^2
1-einw2017.log.lm.sum$r.squared/einw2017.lm.sum$r.squared

# do sqrt and compare to the other solutions
arm:::display(lm(sqrt(einw2017)~bl))

# plot residuals
par(mfrow=c(2,2))
plot(residuals(einw2017.lm), col="violetred2", bty="n")
plot(residuals(einw2017.lm), col="white", bty="n")
text(residuals(einw2017.lm), labels=bland, col="darkred")
plot(residuals(einw2017.log.lm), col="violetred2", bty="n")
plot(residuals(einw2017.log.lm), col="white", bty="n")
text(residuals(einw2017.log.lm), labels=bland, col="darkred")


