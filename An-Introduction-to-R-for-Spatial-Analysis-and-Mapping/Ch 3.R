##
## Code for Chapter 3
##
## Handling Spatial data in R  
## 

## 3.1 OVERVIEW

## 3.2 INTRODUCTION: GISTools

## 3.2.1 Installing and Loading GISTools

library(GISTools)

help(GISTools)
?GISTools

## 3.2.2 Spatial Data in GISTools

data(newhaven)
ls()

plot(roads)

head(data.frame(blocks))
plot(blocks)
plot(roads)

par(mar = c(0,0,0,0)) 
plot(blocks)plot(roads, add=TRUE, col="red")

plot(blocks)plot(breach, col = "red", add = TRUE)

plot(blocks, lwd = 0.5, border = "grey50") 
plot(breach, col = "red", pch = 1, add = TRUE)

colors()

## 3.2.3 Embellishing the Map

map.scale(534750,152000,miles2ft(2),"Miles",4,0.5)
north.arrow(534750,154000,miles2ft(0.25),col= "lightblue")
title('New Haven, CT.' )

## 3.2.4 Saving Your Map

# load package and datalibrary(GISTools)data(newhaven)# plot spatial dataplot(blocks)plot(breach,add=TRUE,col= 'red', pch = 1)# embellish the mapmap.scale(534750,152000,miles2ft(2), "Miles",4,0.5) 
north.arrow(530650,154000,miles2ft(0.25),col= 'lightblue') 
title('New Haven, CT')

source("newhavenmap.R")

plot(roads,add=TRUE,col= 'blue')

plot(blocks, lwd=3)

plot(roads,add=TRUE,col= "red",lwd=2)

pdf(file= 'map.pdf')
dev.off()

png(file= 'map.png')
dev.off()

## 3.3 MAPPING SPATIAL OBJECTS

## 3.3.1 Introduction

## 3.3.2 Data

rm(list=ls())

library(GISTools)data(georgia)

plot(georgia, col = "red", bg = "wheat")

# do a mergegeorgia.outline <- gUnaryUnion(georgia, id = NULL) 
# plot the spatial layersplot(georgia, col = "red", bg = "wheat", lty = 2, 
	border = "blue")plot(georgia.outline, lwd = 3, add = TRUE)# add titlestitle(main = "The State of Georgia", font.main = 2, cex.main = 1.5,	sub = "and its counties", font.sub = 3, 
	col.sub = "blue")

# set some plot parameterspar(mfrow=c(1,2))par(mar = c(2,0,3,0))# 1st plotplot(georgia, col = "red", bg = "wheat") 
title("georgia")# 2nd plotplot(georgia2, col = "orange", bg = "lightyellow3") 
title("georgia2")# reset par(mfrow)par (mfrow=c(1,1))

data.frame(georgia)[,13]

# assign some coordinatesLat <- data.frame(georgia)[,1] #Y or North/SouthLon <- data.frame(georgia)[,2] #X or East/West# assign some labelNames <- data.frame(georgia)[,13]# set plot parameters, plot and labelpar(mar = c(0,0,0,0))plot(georgia, col = NA)pl <- pointLabel(Lon, Lat, Names, offset = 0, cex =.5)

# the county indices below were extracted from the data. framecounty.tmp <- c(81, 82, 83, 150, 62, 53, 21, 16, 124, 121, 17)georgia.sub <- georgia[county.tmp,]
par(mar = c(0,0,3,0))plot(georgia.sub, col = "gold1", border = "grey")plot(georgia.outline, add = TRUE, lwd = 2)title("A subset of Georgia", cex.main = 2, font.main = 1) 
pl <- pointLabel(Lon[county.tmp], Lat[county.tmp],	Names[county.tmp], offset = 3, cex = 1.5)

plot(georgia, border = "grey", lwd = 0.5) 
plot(georgia.sub, add = TRUE, col = "lightblue") 
plot(georgia.outline, lwd = 2, add = TRUE) 
title("Georgia with a subset of counties")

## 3.3.4 Adding Context

install.packages(c("OpenStreetMap"),depend=T)library(OpenStreetMap)

# define upper left, lower right cornersul <- as.vector(cbind(bbox(georgia.sub)[2,2], bbox(georgia.sub)[1,1]))
lr <- as.vector(cbind(bbox(georgia.sub)[2,1], bbox(georgia.sub)[1,2]))# download the map tileMyMap <- openmap(ul,lr,9, 'mapquest')# now plot the layer and the backdroppar(mar = c(0,0,0,0))plot(MyMap, removeMargin=FALSE) 
plot(spTransform(georgia.sub, osm()), add = TRUE, lwd = 2)

install.packages(c("RgoogleMaps"), depend=T)
# load the packagelibrary(RgoogleMaps)

# convert the subsetshp <- SpatialPolygons2PolySet(georgia.sub)# determine the extent of the subsetbb <- qbbox(lat = shp[,"Y"], lon = shp[,"X"]) 
# download map data and store itMyMap <- GetMap.bbox(bb$lonR, bb$latR, destfile = "DC.jpg") 
# now plot the layer and the backdroppar(mar = c(0,0,0,0))PlotPolysOnStaticMap(MyMap, shp, lwd=2,col = rgb(0.25,0.25,0.25,0.025), add = F) 
# reset the plot marginspar(mar=c(5,4,4,2))

## 3.4 MAPPING SPATIAL DATA ATTRIBUTES

## 3.4.1 Introduction

## 3.4.2 Attributes and Data Frames

# load & list the datadata(newhaven)ls()# have a look at the attributes 
summary(blocks)summary(breach)summary(tracts)

data.frame(blocks)
head(data.frame(blocks))
colnames(data.frame(blocks))
data.frame(blocks)$P_VACANT
blocks$P_VACANT
attach(data.frame(blocks))
hist(P_VACANT)
detach(data.frame(blocks))

# use kde.points to create a kernel density surfacebreach.dens = kde.points(breach,lims=tracts) 
summary(breach.dens)
head(data.frame(breach.dens))

# use 'as' to coerce this to a SpatialGridDataFramebreach.dens.grid <- as(breach.dens, "SpatialGridDataFrame") 
summary(breach.dens.grid)

choropleth(blocks, blocks$P_VACANT)
vacant.shades = auto.shading(blocks$P_VACANT)
vacant.shades
choro.legend(533000,161000,vacant.shades)

# set the shadingvacant.shades = auto.shading(blocks$P_VACANT,n=7)# plot the map 
choropleth(blocks,blocks$P_VACANT,shading=vacant.shades) 
choro.legend(533000,161000,vacant.shades)

display.brewer.all()
brewer.pal(5,'Blues')

vacant.shades = auto.shading(blocks$P_VACANT, cols=brewer.pal(5,"Greens"))choropleth(blocks, blocks$P_VACANT,shading=vacant.shades)
choro.legend(533000,161000,vacant.shades)

vacant.shades = auto.shading(blocks$P_VACANT, n=5, cols=brewer.pal(5,"Blues"), cutter=rangeCuts)choropleth(blocks,blocks$P_VACANT,shading=vacant.shades)choro.legend(533000,161000,vacant.shades)

choropleth
auto.shading

## 3.4.4 Mapping Points and Attributes

plot(breach)
plot(blocks) 
plot(breach, add=TRUE)

plot(blocks)plot(breach,add=TRUE,pch='@')

plot(blocks)plot(breach,add=TRUE,pch=16)

plot(blocks)plot(breach,add=TRUE,pch=1,col='red')

# examine the Brewer "Reds" colour palettebrewer.pal(5, "Reds")# then add a 50% transparency 
add.alpha(brewer.pal(5, "Reds"),.50)

par(mar= c(0,0,0,0))# plot the blocks and then the breaches of the peace 
plot(blocks, lwd = 0.7, border = "grey40") 
plot(breach,add=TRUE, pch=1, col= "#DE2D2680")

# load the datadata(quakes)# look at the first 6 records 
head(quakes)

# define the coordinatesdata(quakes)coords.tmp <- cbind(quakes$long, quakes$lat)# create the SpatialPointsDataFramequakes.spdf <- SpatialPointsDataFrame(coords.tmp,	data = data.frame(quakes))# set the plot parameters to show 2 maps 
par(mar = c(0,0,0,0))

par(mfrow=c(1,2))# 1st plot with default plot character 
plot(quakes.spdf)# then with a transparency term 
plot(quakes.spdf, pch = 1, col = '#FB6A4A80') 
# reset par(mfrow)par(mfrow=c(1,1))

##Not Run### you could load the 'maps' package for some context 
# install.packages("maps", dep = T)# library(maps)# map('world2', fill = F, add = TRUE)##End Not Run##

help("SpatialPolygons-class")

data(georgia)# select the polys of interesttmp <- georgia.polys[c(1,3,151,113)]# convert to Polygon and the Polygons objectt1 <- Polygon(tmp[1]); t1 <- Polygons(list(t1), "1") 
t2 <- Polygon(tmp[2]); t2 <- Polygons(list(t2), "2") 
t3 <- Polygon(tmp[3]); t3 <- Polygons(list(t3), "3") 
t4 <- Polygon(tmp[4]); t4 <- Polygons(list(t4), "4") 
# create a SpatialPolygons objecttmp.Sp <- SpatialPolygons(list(t1,t2,t3,t4), 1:4) 
plot(tmp.Sp, col = 2:5)# create an attributenames <- c("Appling", "Bacon", "Wayne", "Pierce")# now create an SPDF objecttmp.spdf <- SpatialPolygonsDataFrame(tmp.Sp,	data=data.frame(names))# plot the data to examine (code not run) 
# data.frame(tmp.spdf)# plot(tmp.spdf, col = 2:5)

# set some plot parameterspar(mfrow=c(2,2))par(mar = c(0,0,0,0)) # set margins 
## 1. Plot using choropleth 
choropleth(quakes.spdf, quakes$mag)
## 2. Plot with a different shading scheme & pchshades = auto.shading(quakes$mag,n=6, cols=brewer.pal(6,'Greens'))choropleth(quakes.spdf, quakes$mag, shades, pch = 1) 
## 3. Plot with a transparencyshades$cols <- add.alpha(shades$cols, 0.5) 
choropleth(quakes.spdf, quakes$mag, shading = shades, pch = 20)## 4. Plot character size determined by attribute magnitude 
tmp <- quakes$mag # assign magnitude to tmptmp <- tmp - min(tmp) # remove minimumtmp <- tmp / max(tmp) # divide by maximum 
plot(quakes.spdf, cex = tmp*3, pch = 1, col = '#FB6A4A80')

# Set the plot parameterspar(mfrow=c(1,2))par(mar = c(0,0,0,0))## 1. Apply a threshold to categorise the datatmp2 <- cut(quakes$mag, fivenum(quakes$mag), include.lowest = T)class <- match(tmp2,levels(tmp2))# specify 4 plot characters to usepch.var <- c(0,1,2,5)# Plot the classesplot(quakes.spdf, pch = pch.var[class], cex = 0.7,	col = "#252525B3")## 2. Thresholds for classes can be specified# logical operations help to define 3 classes# note the terms such as '+ 0' convert TRUE / FALSE to numbersindex.1 <- (quakes$mag >= 4 & quakes$mag < 5) + 0index.2 <- (quakes$mag >=5 & quakes$mag < 5.5) * 2index.3 <- (quakes$mag >=5.5) * 3class <- index.1 + index.2 + index.3# specify 3 plot colours to usecol.var <- (brewer.pal(3, "Blues"))plot(quakes.spdf, col = col.var[class], cex = 1.4, pch = 20) 
# reset par(mfrow)par(mfrow=c(1,1))

## Info Box
data <- c(3, 6, 9, 99, 54, 32, -102) 
index <- (data == 32 | data <= 6) 
data[index]

library(RgoogleMaps)Lat <- as.vector(quakes$lat)Long <- as.vector(quakes$long)MyMap <- MapBackground(lat=Lat, lon=Long, zoom = 10)# note the use of the tmp variable defined earlier to set the cex value
PlotOnStaticMap(MyMap,Lat,Long,cex=tmp+0.3,pch=1,	col= '#FB6A4A80')

MyMap <- MapBackground(lat=Lat, lon=Long, zoom = 10, maptype = "satellite")
PlotOnStaticMap(MyMap,Lat,Long,cex=tmp+0.3,pch=1,      col='#FB6A4A80')

## 3.4.5 Mapping Lines and Attributes

data(newhaven)# 1. create a clip areaxmin <- bbox(roads)[1,1]ymin <- bbox(roads)[2,1]xmax <- xmin + diff(bbox(roads)[1,]) / 2ymax <- ymin + diff(bbox(roads)[2,]) / 2xx = as.vector(c(xmin, xmin, xmax, xmax, xmin)) 
yy = as.vector(c(ymin, ymax, ymax, ymin, ymin)) 
# 2. create a spatial polygon from thiscrds <- cbind(xx,yy)
Pl <- Polygon(crds)ID <- "clip"Pls <- Polygons(list(Pl), ID=ID)SPls <- SpatialPolygons(list(Pls))df <- data.frame(value=1, row.names=ID)clip.bb <- SpatialPolygonsDataFrame(SPls, df)# 3. clip out the roads and the data frameroads.tmp <- gIntersection(clip.bb, roads, byid = T) 
tmp <- as.numeric(gsub("clip", "", names(roads.tmp))) 
tmp <- data.frame(roads)[tmp,]# 4. finally create the SLDF objectroads.tmp <- SpatialLinesDataFrame(roads.tmp,data = tmp, match.ID = F)

par(mfrow=c(1,3)) # set plot order 
par(mar = c(0,0,0,0)) # set margins# 1. simple mapplot(roads.tmp)# 2. mapping an attribute variableroad.class <- unique(roads.tmp$AV_LEGEND)# specify a shading scheme from the road typesshades <- rev(brewer.pal(length(road.class), "Spectral")) 
tmp <- roads.tmp$AV_LEGENDindex <- match(tmp, as.vector(road.class)) 
plot(roads.tmp, col = shades[index], lwd = 3)# 3. using an attribute to specify the line width 
plot(roads.tmp, lwd = roads.tmp$LENGTH_MI * 10)# reset par(mfrow)par(mfrow=c(1,1))

## 3.4.6 Mapping Raster Attributes

data(meuse.grid)class(meuse.grid)summary(meuse.grid)

plot(meuse.grid$x, meuse.grid$y, asp = 1)

meuse.grid = SpatialPixelsDataFrame(points =	meuse.grid[c("x", "y")], data = meuse.grid)

par(mfrow=c(1,2)) # set plot orderpar(mar = c(0.25, 0.25, 0.25, 0.25)) # set margins 
# map the dist attribute using the image function 
image(meuse.grid, "dist", col = rainbow(7)) 
image(meuse.grid, "dist", col = heat.colors(7))

# using spplot from the sp packagepar(mar = c(0.25, 0.25, 0.25, 0.25)) # set marginsp1 <- spplot(meuse.grid, "dist", col.regions=terrain.colors(20))# position in c(xmin, ymin, xmax, ymax)print(p1, position = c(0,0,0.5,0.5), more = T)p2 <- spplot(meuse.grid, c("part.a", "part.b", "soil", "ffreq"),	col.regions=topo.colors(20))print(p2, position = c(0.5,0,1,0.5), more = T)

## 3.5 SIMPLE DESCRIPTIVE STATISTICAL ANALYSES

## 3.5.1 Histograms and Boxplots

data(newhaven)hist(blocks$P_VACANT, breaks = 20, col = "cyan",	border = "salmon",	main = "The distribution of vacant property percentages",	xlab = "percentage vacant", xlim = c(0,40))

index <- blocks$P_VACANT > 10 
high.vac <- blocks[index,] 
low.vac <- blocks[!index,]

# set plot parameters and shadescols = rev(brewer.pal(3, "Blues")) 
par(mfrow = c(1,2))par(mar = c(2.5,2,3,1))# attach the data frame 
attach(data.frame(high.vac))# create a boxplot of 3 variablesboxplot(P_OWNEROCC,P_WHITE,P_BLACK, 
	names=c("OwnerOcc", "White", "Black"),	col=cols, cex.axis = 0.7, main = "High Vacancy")# detach the data framedetach(data.frame(high.vac))# do the same for the second boxplot & variables 
attach(data.frame(low.vac))boxplot(P_OWNEROCC,P_WHITE,P_BLACK, 
	names=c("OwnerOcc","White", "Black"), 
	col=cols, cex.axis = 0.7, main = "Low Vacancy")
detach(data.frame(low.vac)) 
# reset par(mfrow) 
par(mfrow=c(1,1))# reset the plot margins 
par(mar=c(5,4,4,2))

## 3.5.2 Scatter Plots and Regressions

plot(blocks$P_VACANT/100, blocks$P_WHITE/100) 
plot(blocks$P_VACANT/100, blocks$P_BLACK/100)

# assign some variablesp.vac <- blocks$P_VACANT/100 
p.w <- blocks$P_WHITE/100 
p.b <- blocks$P_BLACK/100# fit regressionsmod.1 <- lm(p.vac ~ p.w) 
mod.2 <- lm(p.vac ~ p.b)
summary(mod.1)

# not run below# summary(mod.2)# summary(lm(p.vac ~ p.w + p.b))

# define a factor for the jitter functionfac = 0.05# define a colour palettecols = (brewer.pal(6, "Spectral"))# plot the points with small random term added 
# this is to help show densities# 1st properties vacant against p.w 
plot(jitter(p.vac, fac), jitter(p.w, fac),	xlab= "Proportion Vacant",
	ylab = "Proprtion White / Black",
	col = cols[1], xlim = c(0, 0.8))# then properties vacant against p.bpoints(jitter(p.vac, fac), jitter(p.b, fac), col = cols[6]) 
# fit some trend lines from the 2 regression model coefficientsabline(a = coef(mod.1)[1], b= coef(mod.1)[2],	lty = 1, col = cols[1]); #whiteabline(a = coef(mod.2)[1], b= coef(mod.2)[2],	lty = 1, col = cols[6]); #black# add some legend itemslegend(0.71, 0.19, legend = "Black", bty = "n", cex = 0.8) 
legend(0.71, 0.095, legend = "White", bty = "n", cex = 0.8)

## 3.5.3 Mosaic Plots

# populations of each group in each census blockpops <- data.frame(blocks[,14:18]) * data.frame(blocks)[,11] 
pops <- as.matrix(pops/100)colnames(pops) <- c("White", "Black", "Ameri", "Asian", "Other")# a true / false for vacant propertiesvac.10 <- (blocks$P_VACANT > 10) + 0 
# the crosstabulationsmat.tab <- xtabs(pops ~vac.10)

# mosaic plotttext = sprintf("Mosaic Plot of Vacant Properties with ethnicty")mosaicplot(t(mat.tab),xlab='',
	ylab= 'Vacant Properties > 10 percent', 
	main=ttext,shade=TRUE,las=3,cex=0.8)

## 3.6 SELF-TEST QUESTIONS

## Self-Test Question 1.
# Hintsdisplay.brewer.all() # to show the Brewer palettes?locator # to identify coordinates in the plot windowcex = 0.75 # sets the character size in choro.legend # Toolslibrary(GISTools) # for the mapping tools
data(georgia) 	# to load the Georgia data# choropleth() 	# to create the maps# choro.legend()	# to display the legend

## Self-Test Question 2. 
# Hints?auto.shading # the help for autoshading tool 
?par # the help for plot parameters 
par(mfrow = c(1,2)) # set the plot order to be 1 row & 2 columns# run the code below to specify a 10 by 8 inch plot windowif (.Platform$GUI == "AQUA") { 
	quartz(w=10,h=8) } else {	x11(w=10,h=8) } 
# Toolslibrary(GISTools) # for the mapping tools 
data(newhaven) # to load the New Haven data

## Self-Test Question 3. 
# Hints# locator() # to identify locations in the plot window 
# rect() # to draw a rectangle for a legend# legend() # to indicate the rural and non-rural areas
help("!") # to examine logic operators# Tools 
library(GISTools) # for the mapping tools 
data(georgia) # use georgia2 as it has a geographic projection
library(rgeos) # you may need to use install.packages()# gArea() # a function in rgeos

## Self-Test Question 4. 
library(GISTools)library(rgdal)data(quakes)coords.tmp <- cbind(quakes$long, quakes$lat)# create the SpatialPointsDataFramequakes.spdf <- SpatialPointsDataFrame(coords.tmp,data = data.frame(quakes))

summary(quakes.spdf)
proj4string(quakes.spdf) <- CRS("+proj=longlat +ellps=WGS84")

# Hints# use the help and the example code they include 
?PlotOnStaticMap # for the points?PlotPolysOnStaticMap # for the census block# adjust the polygon shading using rgb and transparency 
?rgb
# Tools
library(GISTools) # for the mapping tools
library(rgdal) # this has the spatial reference tools
library(RgoogleMaps)
library(PBSmapping)data(newhaven) # for the breach point dataset

## ANSWERS TO SELF-TEST QUESTIONS

## Q1.
# load the data and the packagelibrary(GISTools)data(georgia)# open the tif file and give it a name 
tiff("Quest1.tiff", width=7,height=7,units='in',res=300) 
# define the shading schemeshades <- auto.shading(georgia$MedInc/1000,n=11,	cols=brewer.pal(11, "Spectral")) # plot the mapchoropleth(georgia,georgia$MedInc/1000,shading=shades) 
# add the legend & keyschoro.legend(-81.7, 35.1, shades,	title ="Median Income (1000s $)", cex = 0.75) 
# close the filedev.off()

## Q2.
# Codelibrary(GISTools)data(newhaven)attach(data.frame(blocks))# 1. Initial investigation# You could start by having a look at the data 
hist(HSE_UNITS, breaks = 20)# You should notice that it has a normal distribution 
# but with some large outliers# have a look at the impacts of different cut schemes 
quantileCuts(HSE_UNITS, 5)rangeCuts(HSE_UNITS, 5) 
sdCuts(HSE_UNITS, 5)# 2. Do the task# define the plot windowif (.Platform$GUI == "AQUA") {	quartz(w=10,h=6) } else { x11(w=10,h=6) }# set the plot parameterspar(mar = c(0.25,0.25,2, 0.25))par(mfrow = c(1,3))par(lwd = 0.7)# a) mapping classes defined by quantilesshades <- auto.shading(HSE_UNITS, cutter = quantileCuts,	n = 5, cols = brewer.pal(5, "RdYlGn")) 
choropleth(blocks,HSE_UNITS,shading=shades) 
choro.legend(533000,161000,shades) 
title("Quantile Cuts", cex.main = 2)# b) mapping classes defined by absolute rangesshades <- auto.shading(HSE_UNITS, cutter = rangeCuts, 
	n = 5, cols = brewer.pal(5, "RdYlGn"))choropleth(blocks,HSE_UNITS,shading=shades) 
choro.legend(533000,161000,shades)title("Range Cuts", cex.main = 2)# c) mapping classes defined by standard deviations 
shades <- auto.shading(HSE_UNITS, cutter = sdCuts,	n = 5, cols = brewer.pal(5, "RdYlGn")) 
choropleth(blocks,HSE_UNITS,shading=shades) 
choro.legend(533000,161000,shades)
title("St. Dev. Cuts", cex.main = 2) 
# 3. Finally detach the data frame 
detach(data.frame(blocks))# reset par(mfrow)par(mfrow=c(1,1))

## Q3. 
# attach the data frameattach(data.frame(georgia2))# calculate rural populationrur.pop <- PctRural * TotPop90 / 100# calculate county areas in km^2areas <- gArea(georgia2, byid = T)areas <- as.vector(areas / (1000* 1000))# calculate rural densityrur.pop.den <- rur.pop/areas# detach the data framedetach(data.frame(georgia2))# select counties with density > 20index <- rur.pop.den > 20# plot themplot(georgia2[index,], col = "chartreuse4")# plot the non-rural countiesplot(georgia2[!index,], col = "darkgoldenrod3", add = TRUE) # add some fancy bitstitle("Counties with a rural population density	of >20 people per km^2", sub = "Georgia, USA") 
rect(850000, 925000, 970000, 966000, col = "white") 
legend(850000, 955000, legend = "Rural",
	bty = "n", pch = 19, col = "chartreuse4") 
legend(850000, 975000, legend = "Not Rural",	bty = "n", pch = 19, col = "darkgoldenrod3")

## Q4.
library(GISTools) # for the mapping tools 
library(rgdal) # this has the spatial reference tools 
library(RgoogleMaps)library(PBSmapping) 
data(newhaven)# define a new projectionnewProj <- CRS("+proj=longlat +ellps=WGS84")# transform blocks and breachbreach2 <- spTransform(breach, newProj)blocks2 <- spTransform(blocks, newProj)# extract coordinates to pass to Googlecoords <- coordinates(breach2)Lat <- coords[,2]
Long <- coords[,1]# download mapMyMap <- MapBackground(lat=Lat, lon=Long, zoom = 20) 
# convert polys to PBS formatshp <- SpatialPolygons2PolySet(blocks2)# plot polys on map with shading 
PlotPolysOnStaticMap(MyMap, shp, lwd=0.7,	col = rgb(0.75,0.25,0.25,0.15), add = F)# now plot points 
PlotOnStaticMap(MyMap,Lat,Long,pch=1,col='red', add = TRUE)





