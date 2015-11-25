
## ----setup3da,echo=FALSE,message=FALSE,results='hide',cache=FALSE--------
opts_chunk$set(message=FALSE,fig.pos='H',fig.lp='fig:',fig.align='center',tidy=FALSE,warning=FALSE,cache=TRUE,dev='pdf',dpi=300,size='small',fig.height=6,fig.width=10.5)


## ----preamble,echo=FALSE,results='hide',message=FALSE--------------------
library(sp)
library(maptools)
library(RColorBrewer)
library(GISTools)


## ----setup3d,echo=FALSE,message=FALSE,results='hide'---------------------
require(rgl)
require(misc3d)
opts_chunk$set(message=FALSE,fig.pos='H',fig.lp='fig:',fig.align='center',tidy=FALSE,warning=FALSE,cache=TRUE,dev='png',dpi=150)
opts_knit$set(verbose = TRUE, concordance=TRUE,width=60)


## ----kde3dfig,results='hide',echo=FALSE----------------------------------
bx2 <- seq(-5,9,l=60)
by2 <- seq(-5,9,l=60)
floormat <- matrix(0,length(bx2),length(by2))

open3d(windowRect=c(0,0,1000,1000))
x <- seq(-3,3,l=60)
y <- x
z <- outer(x,y,function(x,y) ifelse(x^2 + y^2 < 9,dnorm(x)*dnorm(y),NA))
set.seed(4499864)
xs <- c(rnorm(24),rnorm(12)/2+4.5)
ys <- c(rnorm(24),rnorm(12)/2+4.5)

spheres3d(xs,ys,xs*0+dnorm(0),r=0.15,col='darkolivegreen',alpha=0)
spheres3d(xs,ys,xs*0,r=0.15,col='darkolivegreen')
persp3d(bx2,by2,floormat,box=FALSE,axes=FALSE,col='gray',alpha=0.4,xlab='',ylab='',zlab='',add=TRUE)
aspect3d(1,1,0.5)
rgl.snapshot(file='balls.png')
clear3d()

adder <- FALSE
for (i in 1:36) {
  persp3d(x + xs[i],y + ys[i],z,box=FALSE,axes=FALSE,col='aquamarine1',alpha=0.5,add=adder,xlab='',ylab='',zlab='') 
  adder <- TRUE
}
persp3d(bx2,by2,floormat,box=FALSE,axes=FALSE,col='gray',alpha=0.4,xlab='',ylab='',zlab='',add=TRUE)
aspect3d(1,1,0.5)
rgl.snapshot(file='bumps.png')

clear3d()
x2 <- seq(-5,7.5,l=90)
y2 <- seq(-5,7.5,l=90)
kde <- function(x,y,x2,y2) {
  mean(dnorm(x2-xs)*dnorm(y2-ys))
}
z2 <- matrix(0,length(x2),length(y2))
for (i in 1:length(x2))
  for (j in 1:length(y2))
    z2[i,j] <- kde(x,y,x2[i],y2[j])

z2[z2 < 0.001] <- NA

persp3d(x2,y2,z2,box=FALSE,axes=FALSE,col='tomato1',xlab='',ylab='',zlab='')
persp3d(bx2,by2,floormat,box=FALSE,axes=FALSE,col='gray',alpha=0.4,xlab='',ylab='',zlab='',add=TRUE)
aspect3d(1,1,0.5)
rgl.snapshot(file='kde.png')


## ----varbw,results='hide',echo=FALSE-------------------------------------

open3d(windowRect=c(0,0,1000,1000))

x2 <- seq(-5,7.5,l=90)
y2 <- seq(-5,7.5,l=90)
kdeh <- function(x,y,x2,y2,h) {
  mean(dnorm(x2-xs,sd=h)*dnorm(y2-ys,sd=h))
}

z2.1 <- matrix(0,length(x2),length(y2))
for (i in 1:length(x2))
  for (j in 1:length(y2))
    z2.1[i,j] <- kdeh(x,y,x2[i],y2[j],0.4)

z2.1[z2.1 < 0.001] <- NA

persp3d(x2,y2,z2.1,box=FALSE,axes=FALSE,col='salmon',xlab='',ylab='',zlab='')
persp3d(bx2,by2,floormat,box=FALSE,axes=FALSE,col='gray',alpha=0.4,xlab='',ylab='',zlab='',add=TRUE)
aspect3d(1,1,0.5)
rgl.snapshot(file='kde_04.png')

z2.2 <- matrix(0,length(x2),length(y2))
for (i in 1:length(x2))
  for (j in 1:length(y2))
    z2.2[i,j] <- kdeh(x,y,x2[i],y2[j],1)

z2.2[z2.2 < 0.001] <- NA

persp3d(x2,y2,z2.2,box=FALSE,axes=FALSE,col='gold',xlab='',ylab='',zlab='')
persp3d(bx2,by2,floormat,box=FALSE,axes=FALSE,col='gray',alpha=0.4,xlab='',ylab='',zlab='',add=TRUE)
aspect3d(1,1,0.5)
rgl.snapshot(file='kde_10.png')

z2.3 <- matrix(0,length(x2),length(y2))
for (i in 1:length(x2))
  for (j in 1:length(y2))
    z2.3[i,j] <- kdeh(x,y,x2[i],y2[j],3)

z2.3[z2.3 < 0.001] <- NA

persp3d(x2,y2,z2.3,box=FALSE,axes=FALSE,col='azure',xlab='',ylab='',zlab='')
persp3d(bx2,by2,floormat,box=FALSE,axes=FALSE,col='gray',alpha=0.4,xlab='',ylab='',zlab='',add=TRUE)
aspect3d(1,1,0.5)
rgl.snapshot(file='kde_30.png')


## ----kde,fig.cap='KDE Map for Breaches of the Peace'---------------------
# R Kernel Density
require(GISTools)
data(newhaven)
# Compute Density
breach.dens <- kde.points(breach,lims=tracts)
# Create a level plot
level.plot(breach.dens)
# Use 'masking' to clip around blocks
masker <- poly.outer(breach.dens,tracts,extend=100)
add.masking(masker)
# Add the tracts again
plot(tracts,add=TRUE)


## ----dens,fig.cap="The overplotting problem - Point plot (left) and KDE plot (right)",echo=FALSE----
xd <- matrix(rnorm(8000),4000,2)
par(mfrow=c(1,2))
plot(xd,asp=1,xlab='',ylab='',axes=FALSE,pch=16)
kd <- kde2d(xd[,1],xd[,2])
plot(xd,asp=1,xlab='',ylab='',axes=FALSE,pch=16,type='n')
.filled.contour(kd$x,kd$y,kd$z,levels=seq(0,0.165,l=7),brewer.pal(7,'Blues'))


## ----kdecmp,fig.cap='KDE Maps to Compare Forced and Non-Forced Burglary Patterns'----
# R Kernel Density comparison
require(GISTools)
data(newhaven)
# Set up parameters to create two plots side by side, 
# with 2 line margin at the top,  no margin to bottom, left 
# or right
par(mfrow=c(1,2),mar=c(0,0,2,0))
# Compute Density for forced entry burglaries and create plot
brf.dens <- kde.points(burgres.f,lims=tracts)
level.plot(brf.dens)
# Use 'masking' as before
masker <- poly.outer(brf.dens,tracts,extend=100)
add.masking(masker)
plot(tracts,add=TRUE)
# Add a title
title("Forced Burglaries")

# Compute Density for non-forced entry burglaries and create plot
brn.dens <- kde.points(burgres.n,lims=tracts)
level.plot(brn.dens)
# Use 'masking' as before 
masker <- poly.outer(brn.dens,tracts,extend=100)
add.masking(masker)
plot(tracts,add=TRUE)
# Add a title
title("Non-Forced Burglaries")


## ----hb1-----------------------------------------------------------------
# Load the package with Hex Binning
require(fMultivar)
# Create the hexagonal bins
hbins <- hexBinning(coordinates(breach))


## ----hb2-----------------------------------------------------------------
head(hbins$x)
head(hbins$y)
head(hbins$z)


## ----hb3,fig.cap="Hexagonal Binning of residential burglaries"-----------
# Hex binning code block
# Set up the hexagons to plot,  as polygons
u <- c(1, 0, -1, -1, 0, 1)
u <- u * min(diff(unique(sort(hbins$x))))
v <- c(1,2,1,-1,-2,-1)
v <- v * min(diff(unique(sort(hbins$y))))/3


## ----hb3a----------------------------------------------------------------
max(hbins$z)


## ----hb4, fig.cap="Hexagonal Binning of Breach of Peace events"----------
# Draw a map with blocks
plot(blocks)

# Obtain a shading scheme
shades <- brewer.pal(9,"Greens")

# Draw each polygon, with an appropriate shade 
for (i in 1:length(hbins$x)) {
  polygon(u + hbins$x[i], v + hbins$y[i], 
          col = shades[hbins$z[i]], 
          border = NA)}

# Re-draw the blocks
plot(blocks,add=TRUE)


## ----hb5, fig.cap="Hexagonal Binning of Breach of Peace events - proportional symbolism"----
# Draw a map with blocks
plot(blocks)

# Obtain a shading scheme
scaler <- sqrt(hbins$z/9)

# Draw each polygon, with an appropriate shade 
for (i in 1:length(hbins$x)) {
  polygon(u*scaler[i] + hbins$x[i], v*scaler[i] + hbins$y[i], 
          col = 'indianred', 
          border = NA)}

# Re-draw the blocks
plot(blocks,add=TRUE)


## ----blobbo,fig.cap='A spatial process with clustering and dispersion',echo=FALSE----
set.seed(290162)
rcirc <- function(n) {
  samp <- matrix(runif(2*n,-1,1),n,2)
  samp <- samp[samp[,1]^2 + samp[,2]^2 < 1,]
  repeat {
    if (nrow(samp) >= n) {
      samp <- samp[sample(nrow(samp),n),]
      break
    }
    samp <- rbind(samp,matrix(runif(2*n,-1,1),n,2))
    samp <- samp[samp[,1]^2 + samp[,2]^2 < 1,]
  }
  return(samp)
}

u1 <- runif(10,-1,1)
v1 <- runif(10,-1,1)

u2 <- numeric(length(u1)*10)
v2 <- numeric(length(v1)*10)
stepper <- 1:10

#plot(u1,v1,type='n',xlim=c(-1.2,1.2),ylim=c(-1.2,1.2),axes=FALSE,xlab='',ylab='',asp=1)
for (i in 1:length(u1)) {
  spray <- rcirc(10)/10
#  points(u1[i]+spray[,1],v1[i]+spray[,2],pch=16,col='indianred')
  u2[stepper] <- u1[i]+spray[,1]
  v2[stepper] <- v1[i]+spray[,2]
  stepper <- stepper + 10
}

plot(u2,v2,xlim=c(-1.2,1.2),ylim=c(-1.2,1.2),axes=FALSE,xlab='',ylab='',asp=1,pch=16,col='indianred')
lines(c(0.3,1.1),c(-0.95,-0.95),col='indianred')
text(0.7,-0.90,expression(d[1]))
lines(c(0.6,0.8),c(-1.15,-1.15),col='indianred')
text(0.7,-1.1,expression(d[2]))
box()


## ----CSRK,echo=FALSE,fig.cap='Sample $K$-functions under CSR'------------
kh <- function(x,y) {
  if (!missing(y)) {
    X <- cbind(x,y)
  }
  else {
    X <- x
  }
  d <- sort(dist(X))
  return(cbind(d,(1:length(d))/length(d)))
}
par(mar=c(4,6,1,1))
plot(kh(u2,v2),type='s',xlab=expression(d),ylab=expression(hat(K)(d)))
for (i in 1:100) lines(kh(runif(100,-1,1),runif(100,-1,1)),type='s',col=rgb(1,0,0,0.02))


## ----bc,fig.cap="Bramble cane locations",results='hide'------------------
# K-function code block
# Load the spatstat package
require(spatstat)
# Obtain the bramble cane data
data(bramblecanes)
plot(bramblecanes)


## ----kfun,fig.cap="Ripley's $k$ function plot",results='hide'------------
kf <- Kest(bramblecanes,correction='border')
# Plot it
plot(kf)


## ----Kenv,fig.cap="$k$ function with envelope",results='hide'------------
# Code block to produce k-function with envelope
# Envelope function
kf.env <- envelope(bramblecanes,Kest,correction="border")
# Plot it
plot(kf.env)


## ----mad-----------------------------------------------------------------
mad.test(bramblecanes,Kest)


## ----dclf----------------------------------------------------------------
dclf.test(bramblecanes,Kest)


## ----Lenv,fig.cap="$L$ function with envelope",results='hide'------------
# Code block to produce k-function with envelope
# Envelope function
lf.env <- envelope(bramblecanes,Lest,correction="border")
# Plot it
plot(lf.env)


## ----madlfun-------------------------------------------------------------
mad.test(bramblecanes,Lest)


## ----Genv,fig.cap="$G$ function with envelope",results='hide'------------
# Code block to produce G-function with envelope
# Envelope function
gf.env <- envelope(bramblecanes,Gest,correction="border")
# Plot it
plot(gf.env)


## ----sp2ppp,eval=FALSE,tidy=FALSE----------------------------------------
## require(maptools)
## require(spatstat)
## 
## # Bramblecanes is a data set in ppp format from spatstat
## data(bramblecanes)
## 
## # Convert the data to SpatialPoints, and plot them
## bc.spformat <- as(bramblecanes,"SpatialPoints")
## plot(bc.spformat)
## 
## # It is also possible to extract the study polygon
## # referred to as a window in spatstat terminology
## # Here it is just a rectangle...
## 
## bc.win <- as(bramblecanes$win,"SpatialPolygons")
## plot(bc.win,add=TRUE)


## ----ppp2sp,eval=FALSE,tidy=FALSE----------------------------------------
## # convert burgres.n to a ppp object
## br.n.ppp <- as.ppp(coordinates(burgres.n),
##                    W=as.owin(gUnaryUnion(blocks)))
## br.n.gf <- Gest(br.n.ppp)
## plot(br.n.gf)


## ----bcmarks-------------------------------------------------------------
marks(bramblecanes)


## ----markassign,eval=FALSE-----------------------------------------------
## marks(x) <- ...


## ----crossK,fig.cap='Cross-$L$ function for levels 0 and 1 of the bramble cane data',results='hide'----
ck.bramble <- Lcross(bramblecanes,i=0,j=1,correction='border')
plot(ck.bramble)


## ----crossKenv,fig.cap='Cross-$L$ function envelope for levels 0 and 1 of the bramble cane data',results='hide'----
ckenv.bramble <- envelope(bramblecanes,Lcross,i=0,j=1,correction='border')
plot(ckenv.bramble)


## ----crossKdclf----------------------------------------------------------
dclf.test(bramblecanes,Lcross,i=0,j=1,correction='border')


## ----carsonvoro,cache=TRUE-----------------------------------------------
#
# Original code from Carson Farmer
# http://www.carsonfarmer.com/2009/09/voronoi-polygons-with-r/
# Subject to minor stylistic modifications
#
require(deldir)
require(sp)

voronoipolygons = function(layer) {
    crds <- layer@coords
    z <- deldir(crds[,1], crds[,2])
    w <- tile.list(z)
    polys <- vector(mode='list', length=length(w))
    for (i in seq(along=polys)) {
        pcrds <- cbind(w[[i]]$x, w[[i]]$y)
        pcrds <- rbind(pcrds, pcrds[1,])
        polys[[i]] <- Polygons(list(Polygon(pcrds)),
                               ID=as.character(i))
    }
    SP <- SpatialPolygons(polys)
    voronoi <- SpatialPolygonsDataFrame(SP, 
        data=data.frame(x=crds[,1], 
        y=crds[,2], 
        layer@data,
        row.names=sapply(slot(SP, 'polygons'), 
          function(x) slot(x, 'ID'))))
    return(voronoi)
}


## ----transects,fig.cap='Fulmar Sighting Transects (LHS=Points; RHS=Voronoi Diagram)',results='hide',cache=TRUE----
library(gstat)
library(maptools)
data(fulmar)
fulmar.spdf <- SpatialPointsDataFrame(cbind(fulmar$x,fulmar$y),
                                      fulmar)
fulmar.spdf <- fulmar.spdf[fulmar.spdf$year==1999,]
fulmar.voro <- voronoipolygons(fulmar.spdf)
par(mfrow=c(1,2),mar=c(0.1,0.1,0.1,0.1))
plot(fulmar.spdf,pch=16)
plot(fulmar.voro)


## ----nni,fig.cap='Nearest Neighbour Estimate of Fulmar Density',results='hide',cache=TRUE----
library(gstat)
library(GISTools)
sh <- shading(breaks=c(5,15,25,35),
              cols=brewer.pal(5,'Purples'))
par(mar=c(0.1,0.1,0.1,0.1))
choropleth(fulmar.voro,fulmar.voro$fulmar,shading=sh,border=NA)
plot(fulmar.voro,border='lightgray',add=TRUE,lwd=0.5)
choro.legend(px='topright',sh=sh)


## ----setupIDW,results='hide',cache=FALSE---------------------------------
library(maptools) # Required package
library(GISTools) # Required package
library(gstat) # Set up the gstat package
# Define a sample grid then use it as a set of points 
# to estimate fulmar density via IDW, with alpha=1
s.grid <- spsample(fulmar.voro,type='regular',n=6000)
idw.est <- gstat::idw(fulmar~1,fulmar.spdf,
                      newdata=s.grid,idp=1.0)


## ----reshapeIDW,cache=FALSE----------------------------------------------
# Extract the distinct x and y coordinates of the grid
# Extract the predicted values and form into a matrix
# of gridded values
ux <- unique(coordinates(idw.est)[,1])
uy <- unique(coordinates(idw.est)[,2])
predmat <- matrix(idw.est$var1.pred,length(ux),length(uy))


## ----setupIDW2,results='hide',cache=FALSE--------------------------------
idw.est2 <- gstat::idw(fulmar~1,fulmar.spdf,
                      newdata=s.grid,idp=2.0)
predmat2 <- matrix(idw.est2$var1.pred,length(ux),length(uy))


## ----idw_p1,fig.cap='IDW Interpolation (LHS:$\\alpha = 1$, RHS:$\\alpha=2$)',message=FALSE,results='hide',cache=FALSE----
# Draw the map. The first plot command draws nothing,  

par(mar=c(0.1,0.1,0.1,0.1),mfrow=c(1,2))
plot(fulmar.voro,border=NA,col=NA)
.filled.contour(ux,uy,predmat,col=brewer.pal(5,'Purples'),
                levels=c(0,2,4,6,8,30))

# Draw the legend
sh <- shading(breaks=c(2,4,6,8),
              cols=brewer.pal(5,'Purples'))
choro.legend(px='topright',sh=sh,bg='white')

plot(fulmar.voro,border=NA,col=NA)
.filled.contour(ux,uy,predmat2,col=brewer.pal(5,'Purples'),
                levels=c(0,2,4,6,8,30))
choro.legend(px='topright',sh=sh,bg='white')


## ----idw3dflaws,fig.cap="3D Plots of IDW (LHS:$\\alpha=1$; RHS:$\\alpha=2$)",cache=FALSE----
par(mfrow=c(1,2),mar=c(0,0,2,0))
persp(predmat,box=FALSE)
persp(predmat2,box=FALSE)


## ----krige,fig.cap='Kriging semivariogram',message=FALSE,results='hide',cache=FALSE----
evgm <- variogram(fulmar~1,fulmar.spdf,
                  boundaries=seq(0,250000,l=51))
fvgm <- fit.variogram(evgm,vgm(3,"Mat",100000,1))
plot(evgm,model=fvgm)


## ----krig2,fig.cap='Kriging estimates of fulmar density (RHS),  and associated variance (LHS)',message=FALSE,results='hide',cache=FALSE----
krig.est <- krige(fulmar~1,fulmar.spdf,newdata=s.grid,model=fvgm)
predmat3 <- matrix(krig.est$var1.pred,length(ux),length(uy))
par(mar=c(0.1,0.1,0.1,0.1),mfrow=c(1,2))
plot(fulmar.voro,border=NA,col=NA)
.filled.contour(ux,uy,pmax(predmat3,0),col=brewer.pal(5,'Purples'),
                levels=c(0,8,16,24,32,40))
sh <- shading(breaks=c(8,16,24,32),
              cols=brewer.pal(5,'Purples'))
choro.legend(px='topright',sh=sh,bg='white')
errmat3 <- matrix(krig.est$var1.var,length(ux),length(uy))
plot(fulmar.voro,border=NA,col=NA)
.filled.contour(ux,uy,errmat3,col=rev(brewer.pal(5,'Purples')),
                levels=c(0,3,6,9,12,15))
sh <- shading(breaks=c(3,6,9,12),
              cols=rev(brewer.pal(5,'Purples')))
choro.legend(px='topright',sh=sh,bg='white')


## ----onk,fig.cap="3D Plot Kriging-based interpolation.",cache=FALSE------
persp(predmat3,box=FALSE)


## ----kdestq,fig.cap='KDE Map for Breaches of the Peace - With Roads and Map Scale'----
# R Kernel Density
require(GISTools)
data(newhaven)
# Compute Density
breach.dens <- kde.points(breach,lims=tracts)
# Create a level plot
level.plot(breach.dens)
# Use 'masking' to clip around blocks
masker <- poly.outer(breach.dens,tracts,extend=100)
add.masking(masker)
# Add the tracts again
plot(tracts,add=TRUE)
# Add the roads
plot(roads,col='grey',add=TRUE)
# Add the scale
map.scale(534750,152000,miles2ft(2),"Miles",4,0.5,sfcol='red')


## ----krige2,fig.cap='Kriging semivariogram (Exponential Model)',message=FALSE,results='hide',cache=FALSE----
evgm <- variogram(fulmar~1,fulmar.spdf,
                  boundaries=seq(0,250000,l=51))
fvgm <- fit.variogram(evgm,vgm(3,"Exp",100000,1))
plot(evgm,model=fvgm)


