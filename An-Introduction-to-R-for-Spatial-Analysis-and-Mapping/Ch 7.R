
## ----setup3d,echo=FALSE,message=FALSE,results='hide',cache=FALSE---------
opts_chunk$set(message=FALSE,fig.pos='H',fig.lp='fig:',fig.align='center',tidy=FALSE,warning=FALSE,cache=FALSE,dev='png',dpi=150,size='small')


## ----install_it,eval=FALSE-----------------------------------------------
## install.packages('SpatialEpi',depend=TRUE)


## ----canchoro,fig.cap='Penn State Smoking Rates',tidy=FALSE,fig.height=8.75,fig.width=8.75----
# Make sure the necessary packages have been loaded
require(SpatialEpi)
require(GISTools)
require(rgdal)

# Read in the Penn State lunfg cancer data
data(pennLC)

# Extract the SpatialPolygon info
penn.state.latlong <- pennLC$spatial.polygon

# Convert to UTM zone 17N
penn.state.utm <- spTransform(penn.state.latlong,
                              CRS("+init=epsg:3724 +units=km"))

# Obtain the smoking rates
smk <- pennLC$smoking$smoking * 100

# Set up a shading object, draw choropleth and legend
shades <- auto.shading(smk,n=6,cols=brewer.pal(5,'Blues'))
choropleth(penn.state.utm,smk,shades)
choro.legend(538.5336,4394,shades,title='Smoking Uptake (% of popn.)')


## ----seeding,echo=FALSE,results='hide'-----------------------------------
set.seed(10082007)


## ----ransmk,fig.cap='Randomisation of smoking uptake rates',dependson='seeding'----
# Set up the parameters - six plots in 3 rows by 2 cols
# set margins as smaller than usual to allow bigger maps
par(mfrow=c(3,2),mar=c(1,1,1,1)/2)

# Which one will be the real data
real.data.i <- sample(1:6,1) 

# Draw six plots.  Five will be random one will be the real data
for (i in 1:6) {
  if (i == real.data.i) {
    choropleth(penn.state.utm,smk,shades)}
  else {
    choropleth(penn.state.utm,sample(smk),shades)}
}


## ----reveal,dependson='ransmk'-------------------------------------------
real.data.i


## ----rookqueen,fig.cap='Rook\'s vs Queen\'s case neighbours: Zones 1 and 2 are neighbours only under Queen\'s case. Zone pairs 1,3 and 2,3 are neighbours under both cases.',echo=FALSE,out.width='9cm'----
par(mar=c(1,1,1,1)/2)
plot(c(-1,1),c(-1,1),type='n',axes=FALSE,xlab='',ylab='')
rect(0,0,0.8,0.9,border='black')
rect(-0.8,-0.7,0,0,border='black')
rect(0,-0.6,0.4,0,border='black')
text(0.4,0.45,'Zone 1')
text(-0.4,-0.35,'Zone 2')
text(0.2,-0.3,'Zone 3')


## ----bgspdep,cache=FALSE,echo=FALSE--------------------------------------
require(spdep)


## ----getnb---------------------------------------------------------------
require(spdep)
penn.state.nb <- poly2nb(penn.state.utm)
penn.state.nb


## ----plotnetwork,fig.cap='Depiction of Neighbouring Counties of Penn State as a network (Queen\'s case.)'----
plot(penn.state.utm,border='lightgrey')
plot(penn.state.nb,coordinates(penn.state.utm),add=TRUE,col='red')


## ----plotnetwork2,fig.cap='Comparison of Neighbouring Counties of Penn State (Rook\'s vs. Queen\'s case).'----
# Calculate the Rook's case neighbours
penn.state.nb2 <- poly2nb(penn.state.utm,queen=FALSE)
# Plot the counties in background
plot(penn.state.utm,border='lightgrey')
# Plot the Queen's case neighbourhoods info as a network
plot(penn.state.nb,coordinates(penn.state.utm),add=TRUE,col='blue',lwd=2)
# Now overlay the Rook's case neighbours
plot(penn.state.nb2,coordinates(penn.state.utm),add=TRUE,col='yellow')


## ----nb2lw---------------------------------------------------------------
# Convert the neighbour list to a listw object - use Rook's case...
penn.state.lw <- nb2listw(penn.state.nb2)
penn.state.lw


## ----lagmean,fig.cap='Lagged means of smoking uptake rates'--------------
smk.lagged.means <- lag.listw(penn.state.lw,smk)
choropleth(penn.state.utm,smk.lagged.means,shades)


## ----nbplot,fig.cap='Lagged Mean plot for smoking uptake'----------------
plot(smk,smk.lagged.means,asp=1,xlim=range(smk),ylim=range(smk))
abline(a=0,b=1)
abline(v=mean(smk),lty=2)
abline(h=mean(smk.lagged.means),lty=2)


## ----msp,fig.cap='Lagged Mean plot for smoking uptake - alternative method.'----
moran.plot(smk,penn.state.lw)


## ----morancomp-----------------------------------------------------------
moran.test(smk,penn.state.lw)


## ----Irange,echo=FALSE,cache=FALSE---------------------------------------
wm <- listw2mat(penn.state.lw)
ev <- eigen((wm + t(wm))/2)$values
evh <- max(ev)
evl <- min(ev)


## ----Iminmax-------------------------------------------------------------
moran.range <- function(lw) {
  wmat <- listw2mat(lw)
  return(range(eigen((wmat + t(wmat))/2)$values))
}
moran.range(penn.state.lw)


## ----morancomp2----------------------------------------------------------
moran.test(smk,penn.state.lw,randomisation=FALSE)


## ----RandomMoran---------------------------------------------------------
moran.mc(smk,penn.state.lw,10000)


## ----sardemo-------------------------------------------------------------
sar.res <- spautolm(smk~1,listw=penn.state.lw)
sar.res


## ----getLambdaSESAR------------------------------------------------------
sar.res$lambda.se


## ----getLambdaCISAR------------------------------------------------------
sar.res$lambda + c(-2,2)*sar.res$lambda.se


## ----cardemo,eval=FALSE--------------------------------------------------
## car.res <- spautolm(smk~1,listw=penn.state.lw,family='CAR')
## car.res


## ----seedata-------------------------------------------------------------
head(pennLC$data)


## ----makedata1-----------------------------------------------------------
require(plyr)
totcases <- ddply(pennLC$data,c("county"),numcolwise(sum))


## ----seeplyr-------------------------------------------------------------
head(totcases)


## ----makedata2-----------------------------------------------------------
totcases <- transform(totcases,rate=10000*cases/population)


## ----ratesee,fig.width=5,fig.height=3,fig.cap="Boxplot of Cancer Rates (Penn State 2002)"----
head(totcases)
# Check the distribution of rates
boxplot(totcases$rate,horizontal=TRUE,
        xlab='Cancer Rate (Cases per 10,000 Popn.)')


## ----makemodels----------------------------------------------------------
sar.mod <- spautolm(rate~sqrt(smk),listw=penn.state.lw,
                    weight=population,data=totcases)
summary(sar.mod)


## ----columin,fig.cap='Shapefile of neighbourhoods in Columbus, Ohio, with labels'----
columbus <- readShapePoly(system.file("etc/shapes/columbus.shp",
 package="spdep")[1])

# Create a plot of columbus
plot(columbus, col = "wheat")
# Add labels for each of the zones
text(coordinates(columbus), as.character(1:49), cex = 0.8)
# The box just makes things look neater
box(which = "outer", lwd = 2)


## ----qcrc----------------------------------------------------------------
# Extract a 'queen's case' adjacency object and print it out
col.queen.nb <- poly2nb(columbus,queen=TRUE)
col.queen.nb
# Extract a 'rooks's case' djacency object and print it out
col.rook.nb <- poly2nb(columbus,queen=FALSE)
col.rook.nb


## ----defcov--------------------------------------------------------------
covmat <- function(lambda,adj) {
  solve(tcrossprod(diag(length(adj)) - lambda* listw2mat(nb2listw(adj))))
}


## ----covcor--------------------------------------------------------------
cormat <- function(lambda,adj) {
  cov2cor(covmat(lambda,adj))
}


## ----lambdamess,fig.cap='Relationship between $\\lambda$ and the correlation between zones 41 and 47'----
# Create a range of valid lambda values
lambda.range <- seq(-1.3,0.99,l=101)
# Create an array to store the corresponding correlations
cor.41.47 <- lambda.range*0
# ... store them
for (i in 1:101) cor.41.47[i] <- cormat(lambda.range[i],col.rook.nb)[41,47]
# ... plot the relationship
plot(lambda.range,cor.41.47,type='l')


## ----lambdamess2,fig.cap='Relationship between $\\lambda$ and the correlation between zones 41 and 47'----
# First, add the line from the previous figure for reference
plot(lambda.range,cor.41.47,type='l',xlab=expression(lambda),ylab='Correlation',lty=2)
# Now compute the correlation between zones 40 and 41.
cor.40.41 <- lambda.range*0
for (i in 1:101) cor.40.41[i] <- cormat(lambda.range[i],col.rook.nb)[40,41]
# ... and add these to the plot
lines(lambda.range,cor.40.41)


## ----bat1,fig.cap='Parametric plot of correlations between two polygon pairs (40,41) and (41,47)'----
# First,  plot the empty canvas (type='n)
plot(c(-1,1),c(-1,1),type='n',xlim=c(-1,1),ylim=c(-1,1),xlab='Corr1',ylab='Corr2')
# Then the quadrants
rect(-1.2,-1.2,1.2,1.2,col='pink',border=NA)
rect(-1.2,-1.2,0,0,col='lightyellow',border=NA)
rect(0,0,1.2,1.2,col='lightyellow',border=NA)
# Then the x=y reference line
abline(a=0,b=1,lty=3)
# Then the curve
lines(cor.40.41,cor.41.47)


## ----bat2,fig.cap='Parametric Plots of 100 Sampled Correlations',cache=TRUE----
# First,  plot the empty canvas (type='n)
plot(c(-1,1),c(-1,1),type='n',xlim=c(-1,1),ylim=c(-1,1),
     xlab='Corr1',ylab='Corr2')
# Then the quadrants
rect(-1.2,-1.2,1.2,1.2,col='pink',border=NA)
rect(-1.2,-1.2,0,0,col='lightyellow',border=NA)
rect(0,0,1.2,1.2,col='lightyellow',border=NA)
# Then the x=y reference line
abline(a=0,b=1,lty=3)
# Then the curves
# First, set a seed for reproducibility
set.seed(310712)
for (i in 1:100) {
  r1 <- sample(1:length(col.rook.nb),1)
  r2 <- sample(col.rook.nb[[r1]],2)
  cor.ij1 <- lambda.range*0
  cor.ij2 <- lambda.range*0
  for (k in 1:101) 
    cor.ij1[k] <- cormat(lambda.range[k],col.rook.nb)[r1,r2[1]]
  for (k in 1:101) 
    cor.ij2[k] <- cormat(lambda.range[k],col.rook.nb)[r1,r2[2]]
  lines(cor.ij1,cor.ij2)
}


## ----bootsmk,fig.cap='Bootstrap Randomisation of smoking uptake rates',dependson='seeding'----
# Set up the parameters - six plots in 3 rows by 2 cols
# set margins as smaller than usual to allow bigger maps
par(mfrow=c(3,2),mar=c(1,1,1,1)/2)

# Which one will be the real data
real.data.i <- sample(1:6,1) 

# Draw six plots.  Five will be random one will be the real data
for (i in 1:6) {
  if (i == real.data.i) {
    choropleth(penn.state.utm,smk,shades)}
  else {
    choropleth(penn.state.utm,sample(smk,replace=TRUE),shades)}
}


