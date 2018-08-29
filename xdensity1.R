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
imgname = sprintf("Set1_4.jpg")
x <- readJPEG(imgname)
g <- rasterGrob(x, interpolate=TRUE)

# image metrics
imrange <- read.table("range.dat",header=TRUE)
xrange <- imrange$xrange
yrange <- imrange$yrange

# experimental trial 
selected_trial <- 7
exp.df <- raw[raw$trial==selected_trial,]
N = length(exp.df$xpos)

# plot image and trial
pa <- ggplot(exp.df,aes(x=xpos,y=ypos)) + 
  annotation_custom(g, xmin=xrange[1], xmax=xrange[2], ymin=yrange[1], ymax=yrange[2]) +
  geom_path(size=0.3,colour="red") + geom_point(size=1,colour="red") + 
  ylim(yrange[1],yrange[2]) + xlim(xrange[1],xrange[2]) + 
  xlab("x") + ylab("y") + coord_fixed(ratio = 1) 

# define point patterns
X <- ppp(raw$xpos, raw$ypos, xrange, yrange)
bw <- bw.scott(X)

# create density plot
pb <- ggplot(raw, aes(x=xpos, y=ypos)) + 
  stat_density2d(aes(alpha=..level.., fill=..level..),bins=20,size=0.1,geom="polygon",h=bw) + 
  scale_fill_gradient(low = "yellow", high = "red",guide=FALSE) +
  scale_alpha(guide = FALSE) +
  geom_density2d(colour="black", bins=20, size=0.1) +
  geom_point(data=raw,size=0.1) +
  ylim(yrange[1],yrange[2]) + xlim(xrange[1],xrange[2]) + 
  xlab("x") + ylab("y") + coord_fixed(ratio = 1)

# finalize plot
if ( pdfout ) pdf("Figure.pdf",paper='a4r',width=8,height=6)
grid.newpage()
pushViewport(viewport(layout=grid.layout(1,2)))
print(pa, vp=viewport(layout.pos.row=1,layout.pos.col=1))
print(pb, vp=viewport(layout.pos.row=1,layout.pos.col=2))
if ( pdfout ) dev.off()

