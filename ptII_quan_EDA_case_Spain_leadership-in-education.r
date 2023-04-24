# file:
# ptII_quan_EDA_case_Spain_leadership-in-education.r

# location:
# chap. 5 [5.5.5]
# Führungsverhalten in Bildungskontexten

# load necessary libs
library(scatterplot3d)
library(heatmap3)
library(RColorBrewer)
library(heatmap.plus)
library(lattice)
library(corrgram)

# load necessary helper functions
source("ptall_generalfuncs.r")


# read data
rawd <- read.table("Spain_leadership-in-education_data.tab", header=TRUE, sep="\t")
rawd <- t(rawd) # transpose for later analysis
rawd
str(rawd)
dim(rawd)
# cols = cases/ interviews
# rows = dimensions content leadership

# abbreviate dimension names
dnams <- rownames(rawd)
dnams.tab.abbrev <- data.frame(abbrev=paste("D",1:length(dnams),sep=""),dnams.short=substr(dnams, 1, 4),
                               dnams.abbrev=abbreviate(dnams, named=FALSE),categories=dnams)
dnams.tab.abbrev
rownames(rawd) <- dnams.tab.abbrev[,"dnams.short"]

# scale before calculating distance matrix
scaling <- FALSE
if(scaling) rawd.scaled <- sweep(rawd,2,sqrt(apply(rawd,2,var)),"/")

# distance matrix
rawd.d <- dist(rawd, method="euclidean")
rawd.d
rawd.d.full <- distfull(rawd.d)
rawd.d.full
print(rawd.d.full,digit=3)

# opt
vektorOpt <- optcut(rawd.d.full)
str(vektorOpt)

# max correlation
vektorOpt[with(vektorOpt, order(-cc)),]
vektorOpt[vektorOpt[,"cc"] == (max(vektorOpt[,"cc"])),]

# plot correlations
plot.optcut(vektorOpt=vektorOpt, outZO=outZO, TITLE="Leadership (8 dimensions)")

# create prototype matrix (0,1) based on optimal cut
outZO <- makeZO(dm.full=rawd.d.full, vektorOpt=vektorOpt)
outZO

# prototype
outZO$protovec

# calculate MDS 2D/ 3D
rawd.d2 <- cmdscale(rawd.d, k=2, eig=TRUE, add=TRUE, x.ret=TRUE)
rawd.d3 <- cmdscale(rawd.d, k=3, eig=TRUE, add=TRUE, x.ret=TRUE)

# plot MDS with prototypes
plot.prototype2D(rawd.d2, outZO=outZO, TITLE="Leadership (8 dimensions)", fac=1.6)
# library 'scatterplot3d'
plot.prototype3D(rawd.d3, outZO, TITLE="Leadership (8 dimensions)", ANGLE=26, labelmds=rownames(rawd), box=FALSE)

# plot eigenvalues
plot.eig.mds(sdata=rawd.d2, TITLE="Leadership (8 dimensions)", SUB="Eigenvalues (Multimensional scaling)")



# not run
# scatterplot with pairs (MDS 2D + 3D)
axnam <- c("axis I","axis II","axis III")
pairs(rawd.d2$points, labels=axnam, main=paste("Scatterplot MDS (2D)",sep=""), pch=21, bg="orange1")
pairs(rawd.d3$points, labels=axnam, main=paste("Scatterplot MDS (3D)",sep=""), pch=21, bg="orange1")
# end of not run


# calculate hierarchical clusters
dists <- c("manhattan","euclidean","euclidean","euclidean")
agglos <- c("ward.D2","single","average","complete")
methoden <- data.frame(dists, agglos)
d.methoden <- dim(methoden)
methoden

# scaling if required
rawd.s <- scale(rawd)

# distance matrices
dists.res <- lapply(seq_along(1:d.methoden[1]), function(i) dist(rawd, method=methoden[i,"dists"]))
dists.res.full <- lapply(dists.res, distfull)
names(dists.res.full) <- methoden$dists
dists.res.full

# agglomeration
clusts.res <- lapply(seq_along(1:d.methoden[1]), function(i) hclust(dists.res[[i]], method=methoden[i,"agglos"]))
comb.nam <- paste(methoden[,1],methoden[,2],sep=" / ")
names(clusts.res) <- comb.nam
clusts.res

# plot dendrograms
par(mar=c(5,5,4,2), oma=c(2,1,1,1), "cex.axis"=0.8, mfrow=c(2,2))
for(i in 1:d.methoden[1])
{
 plot(clusts.res[[i]], main="", axes=TRUE, sub="", xlab=comb.nam[i], ylab="height", col="violetred2")
 rect.hclust(clusts.res[[i]], k=3, border="green")
}
mtext("Leadership (8 dimensions)", 3, line=-1.5, cex=1.5, outer=TRUE)
mtext("Hierarchical Cluster Analysis", 3, line=-3, cex=1.1, outer=TRUE)

# check whether plot() and stats:::plot.hclust() are the same
par(mar=c(5,5,4,2), oma=c(2,1,1,1), "cex.axis"=0.8, mfrow=c(1,2))
plot(clusts.res[[1]], col="darkred")
stats:::plot.hclust(clusts.res[[1]], col="steelblue")
attr(clusts.res[[1]],"class")

str(clusts.res)
str(clusts.res[[1]])
clusts.res[[1]][["method"]]

hclust
stats:::hclust

methods("lm")
methods("plot")

# S3 get functions
getAnywhere(plot.hclust)

# heatmap
# https://silico-sciences.com/2016/02/cluster-and-plot-gene-expression-data/
# library 'heatmap3'
hc <- hclust(dist(rawd, method="manhattan"),method="ward.D2")
hr <- hclust(dist(t(rawd), method="manhattan"),method="ward.D2")
#pdf("heatmap.pdf", width = 20, height = 25)
heatmap3(t(rawd), Rowv=as.dendrogram(hr), Colv=as.dendrogram(hc), scale="row",
         balanceColor=TRUE, showRowDendro=TRUE, ColSideCut=50)
#dev.off()

# heatmap
# library(RColorBrewer)
coul <- colorRampPalette(brewer.pal(8, "PiYG"))(25)
heatmap(rawd, xlab="", ylab="", main="Leadership", col=coul)


# not run
# heatmap
# library 'heatmap.plus'
heatmap.plus(rawd)

# levelplot
# library 'lattice'
par(mar=c(3,4,2,2))
levelplot(t(rawd))

#####
#https://cran.r-project.org/web/packages/corrgram/vignettes/corrgram_examples.html
#Friendly, Michael. 2002. “Corrgrams: Exploratory Displays for Correlation Matrices.” The American Statistician 56: 316–24. doi:10.1198/000313002533.

# library 'corrgram'
corrgram(t(rawd), order=TRUE,
    		 lower.panel=panel.shade,
		     upper.panel=panel.pie,
		     diag.panel=panel.minmax,
		     text.panel=panel.txt,
    		 main="Leadership (8 dimensions)"
		    )

corrgram(t(rawd), order=TRUE,
    		 lower.panel=panel.ellipse,
		     upper.panel=panel.pts,
    		 diag.panel=panel.density,
		     text.panel=panel.txt,
    		 main="Leadership (8 dimensions)"
		    )

corrgram(t(rawd), order=TRUE,
      	 lower.panel=panel.ellipse,
		     upper.panel=panel.conf,
    		 diag.panel=panel.density,
		     text.panel=panel.txt,
		     main="Leadership (8 dimensions)"
		    )

corrgram(t(rawd), order=TRUE,
    		 lower.panel=panel.ellipse,
		     upper.panel=panel.bar,
    		 diag.panel=panel.minmax,
    		 text.panel=panel.txt,
    		 main="Leadership (8 dimensions)",
    		 col.regions=colorRampPalette(c("darkgoldenrod4", "burlywood1","darkkhaki", "darkgreen"))
    		)

corrgram(cor(t(rawd)),
         lower.panel=panel.bar,
         main="Leadership (8 dimensions)"
      	)
# end of not run


