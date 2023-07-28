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
# ptII_quan_Bayes_problem-local-minima.r

# location:
# chap. 6 [6.13.1.4]
# Zusammenfassung MCMC-Algorithmus

# load necessary libraries 
library(plot3D)
library(plotly)


# problem local minima

# example 1
local.min.ex1 <- function(r, n, m, s)
{
  ( sin(r) - sin(2*r)/2 + sin(3*r)/3 - sin(4*r)/4 + 4) *(r^2/(r+1)) *
    ifelse(runif(1) <0.5, 1, -1) * cumsum(rnorm(n,m,s))
}  

sek1 <- seq(0,40,0.1)
sek1

par(mfrow=c(1,2))
par(oma=c(2,1,1,1), "cex.axis"=1, bty="l")
plot(sek1,local.min.ex1(sek1,n=length(sek1),m=-5,s=4), xlab="v",ylab="", type="l", col="darkred", bty="n", pre.plot=grid())

# example 2
local.min.ex2 <- function(t)
{
  2 + cos(t) + cos(2*t-1/2)/2 + sin(t/2)
}  
sek2 <- seq(0,2*pi,0.1)

plot(sek2,local.min.ex2(sek2), xlab="v",ylab="", type="l", col="darkred", bty="n", pre.plot=grid())
mtext(expression(paste("Problem of local minima")),outer=TRUE,line=-2,cex=1.5, side=3)



# local minima problem in 3d
himmelblaus <- function(x,y)
{
  (x^2+y-11)^2 + (x+y^2-7)^2
}

himmelblaus(3,2) #0
himmelblaus(-2.805,3.131) #0

n <- 100
sek <- seq(-4,4,length=n)
dats <- unlist(lapply(seq_along(sek), function(y)
  {
  lapply(seq_along(sek), function(x)
    {
      #print(c(x,y))
      himmelblaus(sek[x],sek[y])
    }
    )
  }
  ))
head(dats)
h.sek <- matrix(data=dats, nrow=n, ncol=n, byrow=TRUE)

dim(h.sek)
head(h.sek)

#library(plot3D)
persp3D(x=sek, y=sek, z=h.sek)

par(oma=c(2,1,1,1), "cex.axis"=1, bty="l")
persp3D(x=sek, y=sek, z=h.sek, phi=55, theta=50)
mtext(expression(paste("Himmelblaus' function (local minima)")),outer=TRUE,line=-2,cex=1.5, side=3)

#library(plotly)
fig <- plot_ly(z=~h.sek)
fig <- fig %>% add_surface()
fig


# with contours

fig <- plot_ly(z = ~h.sek) %>% add_surface(
  contours = list(
    z = list(
      show=TRUE,
      usecolormap=TRUE,
      highlightcolor="#ff0000",
      project=list(z=TRUE)
    )
  )
)
fig <- fig %>% layout(
  scene = list(
    camera=list(
      eye = list(x=1.87, y=0.88, z=-0.64)
    )
  )
)

fig
