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
# ptII_quan_Bayes_RandomWalk_helpfuncs.r

# location:
# chap. 6 [6.13]
# Marko Chain Monte Carlo Simulationen — MCMC

# HELPER FUNCTIONS


###### function to calculate simple random walk
randomwalk <- function(nsim=1000, seed=NA, p=c(0.5,0.5,0.5), disti=c(10,10,10), initial=c(0,0,0), graph=TRUE, D=2,
                       phi=30, theta=45, type3d="l", ...)
{
  require(scatterplot3d)
  
  if(!is.na(seed)) set.seed(seed)
  walk <- matrix(data=NA, nrow=nsim, ncol=4)
  colnames(walk) <- c("no","x","y","z")
  walk[,"no"] <- 1:nsim
  # initial value
  walk[1,c("x","y","z")] <- initial
  for(i in 2:nsim)
  {
    ux <- runif(1)
    uy <- runif(1)
    uz <- runif(1)
    udx <- round(runif(1,0,disti[1]))
    udy <- round(runif(1,0,disti[2]))
    udz <- round(runif(1,0,disti[3]))
    ifelse(ux <= p[1], walkx <- +1, walkx <- -1)
    ifelse(uy <= p[2], walky <- +1, walky <- -1)
    ifelse(uz <= p[3], walkz <- +1, walkz <- -1)
    walk[i,"x"] <- walk[i-1,"x"] + walkx*udx
    walk[i,"y"] <- walk[i-1,"y"] + walky*udy
    walk[i,"z"] <- walk[i-1,"z"] + walkz*udz
  }
  if(graph)
  {
    if(D == 3)
    {
      # taken from ?persp
      nrz <- nrow(walk)
      ncz <- ncol(walk)
	    jet.colors <- colorRampPalette( c("blue", "green") )
      # Generate the desired number of colors from this palette
      nbcol <- dim(walk)[1]
      color <- jet.colors(nbcol)
      # Compute the z-value at the facet centres
      z <- outer(walk[,"x"],walk[,"y"], function(a,b) a*b^2)
	    zfacet <- z[-1, -1] + z[-1, -ncz] + z[-nrz, -1] + z[-nrz, -ncz]
      # Recode facet z-values into color indices
      facetcol <- cut(zfacet, nbcol)
	    
      # persp(walk[,c("x","y","z")], phi=phi, theta=theta, shade=.1, col=color[facetcol], border="yellow")
    	scatterplot3d(walk[,c("x","y","z")], col.axis="blue", col.grid="lightblue", main="Random walk (3D)", pch=20,
	                  angle=30, highlight.3d=FALSE, grid=TRUE, color=color, box=FALSE, type=type3d, bg="violetred3")
    } else #D = 2
    {
      plot(walk[,c("x","y")], bty="n", type="l", col="violetred3", pre.plot=grid(), main="Random walk (2D)")
      points(initial[1:2], pch=8, cex=1.5, col="blue")   
    }  
  } 
return(walk)
}
# call:
# walk <- randomwalk()
# walk <- randomwalk(D=3)
########################## END OF FUNCTION

