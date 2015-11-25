
## ----setup3d,echo=FALSE,message=FALSE,results='hide',cache=FALSE---------
opts_chunk$set(message=FALSE,fig.pos='H',fig.lp='fig:',fig.align='center',tidy=FALSE,warning=FALSE,cache=FALSE,dev='pdf',dpi=300,size='small',fig.height=6,fig.width=10.5)
opts_knit$set(verbose = TRUE, concordance=TRUE,width=60)


## ----getdat,fig.cap='North Carolina SIDS Data, County Map'---------------
# Load spdep and rgdal packages
require(maptools)
require(spdep)
require(rgdal)
# Read in the North Carolina shapefile
nc.sids <- readShapePoly(
  system.file("etc/shapes/sids.shp",
  package="spdep")[1],
  ID="FIPSNO", 
  proj4string=CRS("+proj=longlat +ellps=clrk66"))
# Transform to EPSG 2264 (with distances in miles) 
nc.sids.p <- spTransform(nc.sids,CRS("+init=epsg:2264 +units=mi"))
# Plot North Carolina
plot(nc.sids.p)
# add a scale
lines(c(480,480,530,530),c(25,20,20,25))
text(505,10,"50 Miles")


## ----scalemaker----------------------------------------------------------
add.scale <- function() {
  lines(c(480,480,530,530),c(25,20,20,25))
  text(505,10,"50 Miles")
}


## ----localmoran,fig.cap="Standardised local Moran's-$I$"-----------------
# Use GISTools to draw maps
require(GISTools)
# Compute the listw object for the North Carolina polygons
nc.lw <- nb2listw(poly2nb(nc.sids.p))
# Compute the SIDS rates (per 1000 births) for 1979
sids79 <- 1000*nc.sids.p$SID79/nc.sids.p$BIR79
# Compute the local Moran's I
nc.lI <- localmoran(sids79,nc.lw)
# Compute a shading scheme
sids.shade <- auto.shading(c(nc.lI[,1],-nc.lI[,1]),
                           cols=brewer.pal(5,"PRGn")) 
# Draw the map
choropleth(nc.sids.p,nc.lI[,1],shading=sids.shade)
# Add legends + title + scale
choro.legend(120.3,54.9,sids.shade,fmt="%6.2f")
title("Sudden Infant Death Syndrome (Local Moran's I)",cex.main=2)
add.scale()


## ----localpvalraw,fig.cap="Local Moran's-$I$ $p$-values"-----------------
# Create a manual shading scheme
pval.shade <- shading(c(0.01,0.05,0.1),
                      cols=rev(brewer.pal(4,'PuRd')))
# Draw the map
choropleth(nc.sids.p,nc.lI[,5],shading=pval.shade)
# Add legends + title + scale
choro.legend(120.3,54.9,pval.shade,fmt="%6.2f")
title("Sudden Infant Death Syndrome (Local p-value)",cex.main=2)
add.scale()


## ----bonfR---------------------------------------------------------------
1 - (1 - 0.05)^(1/100)


## ----localpvalbonf,fig.cap="Local Moran's-$I$ Bonferroni Adjusted $p$-values"----
# Create a manual shading scheme
pval.shade <- shading(c(0.01,0.05,0.1),
                      cols=rev(brewer.pal(4,'PuRd')))
# Draw the map - note the p.adjust function
choropleth(nc.sids.p,
           p.adjust(nc.lI[,5],
           method='bonferroni'),
           shading=pval.shade)
# Add legends + title + scale
choro.legend(120.3,54.9,pval.shade,fmt="%6.2f")
title("Sudden Infant Death Syndrome (Bonferroni Adjusted  p-value)",
      cex.main=2)
add.scale()


## ----localpvalholm,fig.cap="Local Moran's-$I$ Holm Adjusted $p$-values"----
# Create a manual shading scheme
pval.shade <- shading(c(0.01,0.05,0.1),
                      cols=rev(brewer.pal(4,'PuRd')))
# Draw the map - note the p.adjust function -
# this time using Holm's approach
choropleth(nc.sids.p,
           p.adjust(nc.lI[,5],
               method='holm'),
           shading=pval.shade)
# Add legends + title + scale
choro.legend(120.3,54.9,pval.shade,fmt="%6.2f")
title("Sudden Infant Death Syndrome (Holm Adjusted  p-value)",
      cex.main=2)
add.scale()


## ----localpvalfdr,fig.cap="Local Moran's-$I$ FDR Adjusted $p$-values"----
# Create a manual shading scheme
pval.shade <- shading(c(0.01,0.05,0.1),
                      cols=rev(brewer.pal(4,'PuRd')))
# Draw the map - note the p.adjust function - 
# this time using the FDR approach
choropleth(nc.sids.p,
           p.adjust(nc.lI[,5],
                method='fdr'),
           shading=pval.shade)
# Add legends + title + scale
choro.legend(120.3,54.9,pval.shade,fmt="%6.2f")
title("Sudden Infant Death Syndrome (FDR Adjusted  p-value)",
      cex.main=2)
add.scale()


## ----ranmorani,cache=FALSE-----------------------------------------------
# Create a matrix to place the simulated local Moran's I
sim.I <- matrix(0,1000,100)
# Run the simulations - use column 4 - standardised local Moran's i -
# to evaluate the simulated distributions.
for (i in 1:1000) sim.I[i,] <- localmoran(sample(sids79),nc.lw)[,4]


## ----lmqq,fig.cap="$QQ$-Plot used to evaluate deviation from normality assumption of standardised $I_i$"----
qqnorm(sim.I[,1],main="Alamance County")
qqline(sim.I[,1])


## ----qqmat,fig.cap="Matrix of $QQ$-plots for 9 randomly selected counties."----
# Set up the 3 x 3 multiple window
par(mfrow=c(3,3))
# Create a random sample of 9 counties from 100
samp <- sample(100,9)
# For county in the each sample,  create a qq-plot
for (cty in samp) {
  place <- nc.sids.p@data$NAME[cty] # County name
  qqnorm(sim.I[,cty],main=place) # QQ-plot
  qqline(sim.I[,cty]) # Reference line
}


## ----mcpv----------------------------------------------------------------
mc.pvals <- (colSums(sweep(sim.I,2,nc.lI[,4],'>=')) + 1) /
            (nrow(sim.I) + 1)


## ----eval=FALSE----------------------------------------------------------
## sweep(X,margin,sweepvals,fun)


## ----eval=FALSE----------------------------------------------------------
## sweep(X,2,colMeans(X),'-')


## ----eval=FALSE----------------------------------------------------------
## sweep(X,1,rowSums(X),'/')


## ----eval=FALSE----------------------------------------------------------
## sweep(sim.I,2,nc.lI[,4],'>=')


## ----localpsimfdr,fig.cap="Local Moran's-$I$ FDR Adjusted simulated $p$-values"----
# Create a manual shading scheme
pval.shade <- shading(c(0.01,0.05,0.1),
                      cols=rev(brewer.pal(4,'PuRd')))
# Draw the map - note the p.adjust function - 
# this time using the FDR approach
choropleth(nc.sids.p,
           p.adjust(mc.pvals,
                method='fdr'),
           shading=pval.shade)
# Add legends + title + scale
choro.legend(120.3,54.9,pval.shade,fmt="%6.2f")
title("Sudden Infant Death Syndrome (FDR Adjusted  p-value)",
      cex.main=2)
add.scale()


## ----getlistw,fig.cap="Connectivity between counties for $d = 30$ miles."----
nc.nb.g <- dnearneigh(coordinates(nc.sids.p),0,30) # Create the nb object
nc.lw.g <- nb2listw(nc.nb.g,style='B') # Create the associated listw object
plot(nc.sids.p) # Plot the counties
plot(nc.nb.g,coordinates(nc.sids.p),add=TRUE,col='red') # Add the v[i,j] information
add.scale() # Add scale


## ----lgtest,fig.cap='Map of standardised $G_i$'--------------------------
nc.lG <- localG(sids79,nc.lw.g) # Create local G
# Create a shading scheme
sids.shade <- auto.shading(c(nc.lG,-nc.lG),cols=brewer.pal(5,"PRGn")) 
choropleth(nc.sids.p,nc.lG,shading=sids.shade) # Draw choropleth map
choro.legend(120.3,54.9,sids.shade,fmt="%5.2f") # Add legend
title("Sudden Infant Death Syndrome (Cases per 1000 Popn)",
      cex.main=2) # Add title
add.scale() # Add scale


## ----rangetis,cache=FALSE------------------------------------------------
# Create a matrix to place the simulated local G
sim.G <- matrix(0,1000,100)
# Run the simulations 
# to evaluate the simulated distributions.
for (i in 1:1000) sim.G[i,] <- localG(sample(sids79),nc.lw.g)


## ----qqmatG,fig.cap="Matrix of $QQ$-plots for $G_i$ for 9 randomly selected counties."----
# Set up the 3 x 3 multiple window
par(mfrow=c(3,3))
# Create a random sample of 9 counties from 100
samp <- sample(100,9)
# For county in the each sample,  create a qq-plot
for (cty in samp) {
  place <- nc.sids.p@data$NAME[cty] # County name
  qqnorm(sim.G[,cty],main=place) # QQ-plot
  qqline(sim.G[,cty]) # Reference line
}


## ----mcpvG---------------------------------------------------------------
mc.pvals.g <- (colSums(sweep(sim.G,2,nc.lG,'>=')) + 1) /
            (nrow(sim.G) + 1)


## ----gpvals,fig.cap='FDR-adjusted local $p$-values for local $G$-statistics'----
nc.lpv.g <- p.adjust(mc.pvals.g,method="fdr")
lpv.shade <- shading(c(0.001,0.01,0.05,0.10),cols=rev(brewer.pal(5,"BuGn"))) 
choropleth(nc.sids.p,nc.lpv.g,shading=lpv.shade)
choro.legend(120.3,54.9,lpv.shade,fmt="%5.3f")
title("Sudden Infant Death Syndrome (Local p-values)",cex.main=2)
add.scale()


## ----truestuff-----------------------------------------------------------
all(c(TRUE,FALSE,TRUE,TRUE))
all(c(TRUE,TRUE,TRUE))


## ----scanfor-------------------------------------------------------------
all(nc.lpv.g >= 0.05)


## ----scanfor2------------------------------------------------------------
any(nc.lpv.g < 0.05)


## ----loopdist,cache=FALSE------------------------------------------------
g.test <- function(d,spdf, var) {
 spdf.nb <- dnearneigh(coordinates(spdf),0,d) 
 spdf.lw <- nb2listw(spdf.nb,style='B')
 true.G <- localG(var,spdf.lw)
 sim.G <- matrix(0,10000,length(var))
 for (i in 1:10000) sim.G[i,] <- localG(sample(var),spdf.lw)
 return((colSums(sweep(sim.G,2,nc.lI[,4],'>=')) + 1) /
            (nrow(sim.I) + 1))
} 


## ----dists,cache=FALSE---------------------------------------------------
dists <- seq(30,100,by=5) # 30 to 100 miles,  in steps of 5
p.results <- matrix(0,100,15) 
i <- 1 # Counter for p.results vector
for (d in dists) {
  p.results[,i] <- p.adjust(
    g.test(d,nc.sids.p,sids79),method='fdr')
  i <- i + 1 }
flag.p <- p.results < 0.05
apply(flag.p,2,any)


## ----whichdist-----------------------------------------------------------
dists[apply(flag.p,2,any)]


## ----gpvals2,fig.cap='FDR-adjusted local $p$-values for local $G$-statistics, $d=100$ miles'----
choropleth(nc.sids.p,p.results[,dists==100],shading=lpv.shade)
choro.legend(120.3,54.9,lpv.shade,fmt="%5.3f")
title("Sudden Infant Death Syndrome d=100 miles (Local p-values)",cex.main=2)
add.scale()


## ----gethp,cache=FALSE---------------------------------------------------
library(GWmodel)
data(EWHP)
head(ewhp)


## ----div1000-------------------------------------------------------------
ewhp$PurPrice <- ewhp$PurPrice / 1000


## ----hpfa,fig.cap='Purchase Price vs. Floor Area>'-----------------------
plot(ewhp$FlrArea,ewhp$PurPrice, xlab='Floor Area',
     ylab='Purchase Price (1000\'s UK Pounds)')


## ----sumhp---------------------------------------------------------------
mean(ewhp$PurPrice)
sd(ewhp$PurPrice)


## ----gete1071,message=FALSE,cache=FALSE----------------------------------
library(e1071)
skewness(ewhp$PurPrice)


## ----hphist,fig.cap='Histogram of UK House Prices'-----------------------
hist(ewhp$PurPrice,xlab='Purchase Price',
     main='Housing Cost Distribution')


## ----makespdf------------------------------------------------------------
houses.spdf <- SpatialPointsDataFrame(ewhp[,1:2],ewhp)


## ----holoc,fig.cap='Locations of Houses in 1991 Data Set'----------------
data(EWOutline) 
plot(ewoutline)
plot(houses.spdf,add=T,pch=16)


## ----cls-----------------------------------------------------------------
localstats1 <- gwss(houses.spdf,vars=c("PurPrice","FlrArea"),bw=50000)


## ----lookaihp------------------------------------------------------------
head(data.frame(localstats1$SDF))


## ----lrcb----------------------------------------------------------------
require(RColorBrewer)


## ----qmg-----------------------------------------------------------------
quick.map <- function(spdf,var,legend.title,main.title) {
 x <- spdf@data[,var]
 cut.vals <- pretty(x)
 x.cut <- cut(x,cut.vals)
 cut.levels <- levels(x.cut)
 cut.band <- match(x.cut,cut.levels)
 colors <- brewer.pal(length(cut.levels),'Reds')
 par(mar=c(1,1,1,1))
 plot(ewoutline,col='grey85')
 title(main.title)
 plot(spdf,add=TRUE,col=colors[cut.band],pch=16)
 legend('topleft',cut.levels,col=colors,pch=16,bty='n',title=legend.title)
}


## ----qmx,eval=FALSE------------------------------------------------------
## quick.map(gwss.object,variable.name,legend.title,main.map.title)


## ----gwmeanmap,fig.cap="Geographically Weighted Mean"--------------------
quick.map(localstats1$SDF,"PurPrice_LM",
          "1000's Uk Pounds","Geographically Weighted Mean")


## ----sdsk,fig.cap="Geographically Weighted Standard Deviation and skewness"----
par(mfrow=c(1,2))
quick.map(localstats1$SDF,"PurPrice_LSKe",
          "Skewness Level","Local Skewness")
quick.map(localstats1$SDF,"PurPrice_LSD",
          "1000's Pounds","Local Standard Deviation")


## ----qmrho,fig.cap="Geographically Weighted Pearson Correlation"---------
quick.map(localstats1$SDF,"Corr_PurPrice.FlrArea",
          expression(rho),"Geographically Weighted Pearson Correlation")


## ----cocomp,fig.cap="Comparison of Geographically Weighted Correlation Measures"----
par(mfrow=c(1,2))
quick.map(localstats1$SDF,"Corr_PurPrice.FlrArea",
          expression(rho),
          "Geographically Weighted Pearson Correlation")
quick.map(localstats1$SDF,"Spearman_rho_PurPrice.FlrArea",
          expression(rho),
          "Geographically Weighted Spearman Correlation")


## ----gwmedian,fig.cap="Geographically Weighted Medians"------------------
localstats2 <- gwss(houses.spdf,vars=c("PurPrice","FlrArea"),
                    bw=50000,quantile=TRUE)
quick.map(localstats2$SDF,"PurPrice_Median","1000\'s UK Pounds",
          "Geographically Weighted Median House Price")


## ----iqrqi,fig.cap="Geographically Weighted Interquartile Range and Quantile Imbalance"----
par(mfrow=c(1,2))
quick.map(localstats2$SDF,"PurPrice_IQR","1000\'s UK Pounds",
          "Geographically Weighted Interquartile Range")
quick.map(localstats2$SDF,"PurPrice_QI","1000\'s UK Pounds",
          "Geographically Weighted Quantile Imbalance")


## ----dogwr,cache=TRUE----------------------------------------------------
gwr.res <- gwr.basic(PurPrice~FlrArea,
                     data=houses.spdf,bw=50000)


## ----infogwr,size='tiny'-------------------------------------------------
gwr.res


## ----gwrmap,fig.cap="Geographically Weighted Regression Coefficient"-----
quick.map(gwr.res$SDF,"FlrArea",
         "1000's Uk Pounds per Square meter",
          "Geographically Weighted Regression Coefficient")


## ----nevrest,fig.cap="Comparison of north east England Against Other Areas"----
# Get coordinates
xy <- coordinates(gwr.res$SDF)
# Compute distance from a point in north east england
dne <- sqrt(rowSums(sweep(xy,2,c(452300,517200),"-")^2))
# Is each location less than 75km away?
in.ne <- dne < 75000
# Compare scatter plots for north east and rest of data
plot(houses.spdf$FlrArea,houses.spdf$PurPrice,type='n',
     xlab='Floor area (sq. m.)',ylab="Purchase Price (1000's pounds)")
points(houses.spdf$FlrArea[!in.ne],
       houses.spdf$PurPrice[!in.ne],col='grey85',pch=16)
points(houses.spdf$FlrArea[in.ne],
       houses.spdf$PurPrice[in.ne],col='black',pch=16)


## ----dogwrrob,cache=TRUE-------------------------------------------------
gwr.res.ad <- gwr.basic(PurPrice~FlrArea,
                       data=houses.spdf,adaptive=TRUE,bw=100)
quick.map(gwr.res.ad$SDF,"FlrArea",
         "1000's Uk Pounds per Square meter",
          "Geographically Weighted Regression Coefficient")


## ----pickout-------------------------------------------------------------
nc.sids.p@data[nc.lI[,5] < 0.05,c(11,18,19)]


