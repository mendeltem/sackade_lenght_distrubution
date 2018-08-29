setwd("E:/CS/Project Mathe/PointProcesses")
rm(list=ls())

library(mgcv)
library(jpeg)
library(spatstat)
library(ggplot2)
library(grid)

pdfout = F

# read experimental data & image
raw <- read.table("fixloc_4.dat",header=TRUE)

# image metrics
imrange <- read.table("range.dat",header=TRUE)
xrange <- imrange$xrange
yrange <- imrange$yrange

# define point patterns
X <- ppp(raw$xpos, raw$ypos, xrange, yrange)
bw <- bw.scott(X)
d <- density(X,bw)

# visualize point pattern
plot(d)
points(X,col="white",cex=0.2)
contour(d)

N <- dim(raw)[1]
rX <- rpoint(N,d)
points(rX,col="white",cex=0.2)


