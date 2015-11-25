##
## Code for Chapter 5
##
## This shows R functions to perform 
## GIS like operations 
## 


## 5.1 INTRODUCTION

## 5.2 SPATIAL INTERSECTION OR CLIP OPERATIONS

library(GISTools)data(tornados)
  
# set plot parameters and initial plot for map extent
par(mar=c(0,0,0,0))
plot(us_states)
# plot the data using a shading with a transparency term 
# see the add.alpha() function for thisplot(torn, add = T, pch = 1, col = "#FB6A4A4C", cex = 0.4) 
plot(us_states, add = T)

summary(torn)

index <- us_states$STATE_NAME == "Texas" | 
	us_states$STATE_NAME == "New Mexico" | 
	us_states$STATE_NAME == "Oklahoma" | 
	us_states$STATE_NAME == "Arkansas"AoI <- us_states[index,]

plot(AoI)plot(torn, add = T, pch = 1, col = "#FB6A4A4C")

AoI.torn <- gIntersection(AoI, torn, byid = TRUE) 
par(mar=c(0,0,0,0))plot(AoI)plot(AoI.torn, add = T, pch = 1, col = "#FB6A4A4C")

AoI.torn <- gIntersection(AoI, torn, byid = TRUE)

head(data.frame(AoI.torn))head(rownames(data.frame(AoI.torn)))tail(rownames(data.frame(AoI.torn)))

rownames(data.frame(us_states[index,]))

us_states$STATE_NAME[index]

# assign rownames to tmp and split the data by spaces " " 
tmp <- rownames(data.frame(AoI.torn))tmp <- strsplit(tmp, " ")# assign the first and second parts of the split
torn.id <- (sapply(tmp, "[[", 2))state.id <- (sapply(tmp, "[[", 1))# use torn.id to subset the torn data and assign to df1 
torn.id <- as.numeric(torn.id)df1 <- data.frame(torn[torn.id,])

## Info box

# set up some variables
state.list <- rownames(data.frame(us_states[index,])) 
tmp <- rownames(data.frame(AoI.torn))
# loop through these, removing the state.list variable
for (i in 1: length(state.list)) {
	replace.val <- sprintf("%s ", state.list[i])
	tmp <- gsub(replace.val, "", tmp)
}
# again use torn.id to subset the torn data and assign to df1
torn.id <- as.numeric(tmp)
df1 <- data.frame(torn[torn.id,])

df2 <- us_states$STATE_NAME[as.numeric(state.id)] 
df <- cbind(df2, df1)names(df)[1] <- "State"

AoI.torn <- SpatialPointsDataFrame(AoI.torn, data = df) 
# write out as a shapefile if you wish# writePointsShape(AoI.torn, “AoItorn.shp”)

# match df2 defined above to us_states$STATE_NAMEindex2 <- match(df2, us_states$STATE_NAME)# use this to select from the data frame of us_states 
df3 <- data.frame(us_states)[index2,]# bind together and rename the attributedf3 <- cbind(df2, df1, df3)names(df3)[1] <- "State"# create spatial dataAoI.torn2 <- SpatialPointsDataFrame(AoI.torn, data = df3)

## 5.3 BUFFERS

# select an Area of Interest and apply a bufferAoI <- us_states2[us_states2$STATE_NAME == "Texas",] 
AoI.buf <- gBuffer(AoI, width = 25000)# map the buffer and the original area par(mar=c(0,0,0,0))plot(AoI.buf)plot(AoI, add = T, border = "blue")

data(georgia)# apply a buffer to each objectbuf.t <- gBuffer(georgia2, width = 5000, byid = T,id = georgia2$Name)# now plot the dataplot(buf.t)plot(georgia2, add = T, border = "blue")

plot(buf.t[1,])plot(georgia2[1,], add = T, col = "blue")

## 5.4 MERGING SPATIAL FEATURES

AoI.merge <- gUnaryUnion(us_states)# now plotpar(mar=c(0,0,0,0))plot(us_states, border = "darkgreen", lty = 3) 
plot(AoI.merge, add = T, lwd = 1.5)

## 5.5 POINT-IN-POLYGON AND AREA CALCULATIONS 

## 5.5.1 Point-in-Polygon

## Info Box
poly.counts

torn.count <- poly.counts(torn, us_states) 
head(torn.count)

names(torn.count)

## 5.5.2 Area Calculations

proj4string(us_states2)

poly.areas(us_states2)
# hectarespoly.areas(us_states2) / (100 * 100)# square kilometres 
poly.areas(us_states2) / (1000 * 1000)

## Self-Test Question 1.

library(GISTools)data(newhaven)

ft2miles(ft2miles(poly.areas(blocks)))

## 5.5.3 Point and Areas Analysis Exercise

data(newhaven)densities= poly.counts(breach,blocks) /      ft2miles(ft2miles(poly.areas(blocks)))cor(blocks$P_OWNEROCC,densities)

plot(blocks$P_OWNEROCC,densities)

breaches ~ Poisson(exp(a + b * blocks$P_OWNEROCC+log(AREA)))

# load and attach the datadata(newhaven)attach(data.frame(blocks))# calculate the breaches of the peace in each block 
n.breaches = poly.counts(breach,blocks)area = ft2miles(ft2miles(poly.areas(blocks)))# fit the model 
model1=glm(n.breaches~P_OWNEROCC,offset=log(area),family= poisson)# detach the datadetach(data.frame(blocks))

model1
summary(model1)

s.resids = rstandard(model1)
resid.shades = shading(c(-2,2),c("red","grey","blue"))

par(mar=c(0,0,0,0)) 
choropleth(blocks,s.resids,resid.shades) 
# reset the plot marginspar (mar=c(5,4,4,2))

attach(data.frame(blocks))n.breaches = poly.counts(breach,blocks)area = ft2miles(ft2miles(poly.areas(blocks))) 
model2=glm(n.breaches~P_OWNEROCC+P_VACANT,	offset=log(area),family=poisson) 
s.resids.2 = rstandard(model2) 
detach(data.frame(blocks))

s.resids.2 = rstandard(model2) 
par(mar=c(0,0,0,0)) 
choropleth(blocks,s.resids.2,resid.shades) 
# reset the plot marginspar (mar=c(5,4,4,2))

## 5.6 CREATING DISTANCE ATTRIBUTES

data(newhaven)proj4string(places) <- CRS(proj4string(blocks)) 
centroids. <- gCentroid(blocks, byid = T, id = rownames(blocks))distances <- ft2miles(gDistance(places, centroids., byid = T))

distances <- gWithinDistance(places, blocks, byid = T, dist = miles2ft(1.2))

## 5.6.1 Distance Analysis/Accessibility Exercise

distances <- ft2miles(gDistance(places, centroids., byid = T))min.dist <- as.vector(apply(distances,1, min))access <- min.dist < 1# and this can be mapped# plot(blocks, col = access)

# extract the ethnicity data from the blocks variableethnicity <- as.matrix(data.frame(blocks[,14:18])/100) 
ethnicity <- apply(ethnicity, 2, function(x) (x * blocks$POP1990))ethnicity <- matrix(as.integer(ethnicity), ncol = 5) 
colnames(ethnicity) <- c("White", "Black",	"Native American", "Asian", "Other")

# use xtabs to generate a crosstabulationmat.access.tab = xtabs(ethnicity~access) 
# then transpose the data
data.set = as.data.frame(mat.access.tab)#set the column namescolnames(data.set) = c("Access","Ethnicity", "Freq")

modelethnic = glm(Freq~Access*Ethnicity, data=data.set,family=poisson)# the full model can be printed to the console 
# summary(modelethnic)

summary(modelethnic)$coef
mod.coefs = summary(modelethnic)$coef

tab <- 100*(exp(mod.coefs[,1]) - 1) 
tab <- tab[7:10]names(tab) <- colnames(ethnicity)[2:5] 
tab

mosaicplot(t(mat.access.tab),xlab='',ylab='Access to Supply', 
	main="Mosaic Plot of Access",shade=TRUE,las=3,cex=0.8)

## Self-Test Question 2. 

plot(blocks,border='red')plot(tracts,lwd=2,add=TRUE)

blocks$OCCUPIED
blocks2 = blocks[blocks$OCCUPIED > 0,]

attach(data.frame(blocks2))forced.rate = 2000*poly.counts(burgres.f,blocks2)/OCCUPIED 
notforced.rate = 2000*poly.counts(burgres.n,blocks2)/ OCCUPIEDdetach(data.frame(blocks2))

model1 = lm(forced.rate~notforced.rate)
summary(model1)coef(model1)

## 5.7 COMBINING SPATIAL DATASETS AND THEIR ATTRIBUTES

data(newhaven)# define sample grid in polygonsbb <- bbox(tracts)grd <- GridTopology(cellcentre.offset=      c(bb[1,1]-200,bb[2,1]-200),cellsize=c(10000,10000), cells.dim = c(5,5)) 
int.layer <- SpatialPolygonsDataFrame(      as.SpatialPolygons.GridTopology(grd),data = data.frame(c(1:25)), match.ID = FALSE) 
names(int.layer) <- "ID"

int.res <- gIntersection(int.layer, tracts, byid = T)

# set some plot parameterspar(mfrow = c(1,2))par(mar=c(0,0,0,0))# plot and label the zonesplot(int.layer, lty = 2)Lat <- as.vector(coordinates(int.layer)[,2])Lon <- as.vector(coordinates(int.layer)[,1])Names <- as.character(data.frame(int.layer)[,1])# plot the tractsplot(tracts, add = T, border = "red", lwd =2)pl <- pointLabel(Lon, Lat, Names, offset = 0, cex =.7) 
# set the plot extentplot(int.layer, border = "white")# plot the intersectionplot(int.res, col=blues9, add = T)

names(int.res)

tmp <- strsplit(names(int.res), " ") 
tracts.id <- (sapply(tmp, "[[", 2)) 
intlayer.id <- (sapply(tmp, "[[", 1))

# generate area and proportionsint.areas <- gArea(int.res, byid = T) 
tract.areas <- gArea(tracts, byid = T)# match this to the new layerindex <- match(tracts.id, row.names(tracts)) 
tract.areas <- tract.areas[index]tract.prop <- zapsmall(int.areas/tract.areas, 3)# and create data frame for the new layerdf <- data.frame(intlayer.id, tract.prop)houses <- zapsmall(tracts$HSE_UNITS[index] * tract.prop, 1) 
df <- data.frame(df, houses, int.areas)

int.layer.houses <- xtabs(df$houses~df$intlayer.id) 
index <- as.numeric(gsub("g", "", names(int.layer.houses)))# create temporary variabletmp <- vector("numeric", length = dim(data.frame(int.layer))[1])tmp[index] <- int.layer.housesi.houses <- tmp

int.layer <- SpatialPolygonsDataFrame(int.layer, 
	data = data.frame(data.frame(int.layer), i.houses), 
	match.ID = FALSE)

# set the plot parameters and the shading variablepar(mar=c(0,0,0,0))shades = auto.shading(int.layer$i.houses,	n = 6, cols = brewer.pal(6, "Greens")) 
# map the datachoropleth(int.layer, int.layer$i.houses, shades) 
plot(tracts, add = T)choro.legend(530000, 159115, bg = "white", shades,	title = "No. of houses", under = "") 
# reset the plot marginspar (mar=c(5,4,4,2))

## Self-Test Question 3. 
install.packages("rgdal", dep = T)library(rgdal)ct <- proj4string(blocks)proj4string(int.layer) <- CRS(ct)blocks <- spTransform(blocks, CRS(proj4string(int.layer)))

## 5.8 CONVERTING BETWEEN RASTER AND VECTOR

## 5.8.1 Raster to Vector

library(GISTools) 
library(raster) 
data(tornados)

# Points
r <- raster(nrow = 180 , ncols = 360, ext = extent(us_states2))
t2 <- as(torn2, "SpatialPoints")
r <- rasterize(t2, r, fun=sum)
# set the plot extent by specifying the plot colour 'white'plot(r, col = "white")plot(us_states2, add = T, border = "grey") 
plot(r, add = T)

# Linesus_outline <- as(us_states2 , "SpatialLinesDataFrame") 
r <- raster(nrow = 180 , ncols = 360, ext = extent(us_states2)) 
r <- rasterize(us_outline , r, "STATE_FIPS") 
plot(r)

# Polygonsr <- raster(nrow = 180 , ncols = 360, ext = extent(us_states2))
r <- rasterize(us_states2, r, "POP1997")
plot(r)
r

## Info box

d <- 50000 
dim.x <- d 
dim.y <- d

bb <- bbox(us_states2)# work out the number of cells needed 
cells.x <- (bb[1,2]-bb[1,1]) / dim.x 
cells.y <- (bb[2,2]-bb[2,1]) / dim.y 
round.vals <- function(x){	if(as.integer(x) < x) { 
		x <- as.integer(x) + 1	} else {x <- as.integer(x) 
		}}# the cells cover the data completelycells.x <- round.vals(cells.x)cells.y <- round.vals(cells.y)# specify the raster extentext <- extent(c(bb[1,1], bb[1,1]+(cells.x*d),bb[2,1],bb[2,1]+(cells.y*d)))# now run the raster conversionr <- raster(ncol = cells.x,nrow =cells.y) 
extent(r) <- extr <- rasterize(us_states2, r, "POP1997") 
# and examine the resultsr
plot(r)

## 5.8.2 Converting to sp Classes

r <- raster(nrow=60,ncols=120,ext = extent(us_states2)) 
r <- rasterize(us_states2, r, "STATE_FIPS")
g <- as(r, 'SpatialGridDataFrame')p <- as(r, 'SpatialPixelsDataFrame')# not run# image(g, col = topo.colors(51))# points(p, cex = 0.5)par(mar=c(0,0,0,0))plot(p, cex = 0.5, pch = 1, col = p$layer)

head(data.frame(g)) 
head(data.frame(p))

# set up and create the rasterr <- raster(nrow = 60 , ncols = 120, ext = extent(us_states2)) 
r <- rasterize(us_states2 , r, "POP1997") 
r2 <- r
# seubset the data
r2[r < 10000000] <- NA
g <- as(r2, 'SpatialGridDataFrame')
p <- as(r2, 'SpatialPixelsDataFrame')
# not run
# image(g, bg = "grey90")
par(mar=c(0,0,0,0))
plot(p, cex = 0.5, pch = 1)

## 5.8.3 Vector to Raster

# load the data and convert to rasterdata(newhaven)# set up the raster, rr <- raster(nrow = 60 , ncols = 60, ext = extent(tracts)) # convert polygons to rasterr <- rasterize(tracts , r, "VACANT")
poly1 <- rasterToPolygons(r, dissolve = T)
# convert to pointspoints1 <- rasterToPoints(r)# plot the points, rasterized polygons & orginal polygons 
par(mar=c(0,0,0,0))plot(points1, col = "grey", axes = FALSE, xaxt='n', ann=FALSE)plot(poly1, lwd = 1.5, add = T)plot(tracts, border = "red", add = T)# reset the plot marginspar(mar=c(5,4,4,2))

## 5.9 INTRODUCTION TO RASTER ANALYSIS

## 5.9.1 Raster Data Preparation

library(GISTools) 
library(raster) 
library(sp)# load the meuse.grid data 
data(meuse.grid)# create a SpatialPixelsDataFrame objectcoordinates(meuse.grid) <- ~x+ymeuse.grid <- as(meuse.grid, "SpatialPixelsDataFrame") 
# create 3 raster layersr1 <- raster(meuse.grid, layer = 3) #distr2 <- raster(meuse.grid, layer = 4) #soilr3 <- raster(meuse.grid, layer = 5) #ffreq

image(r1, asp = 1) 
image(r2, asp = 1) 
image(r3, asp = 1)

## 5.9.2 Raster Reclassification

Raster_Result <- r2 + (r3 * 10)table(as.vector(Raster_Result$values))
spplot(Raster_Result, col.regions=brewer.pal(9, "Spectral"), cuts=8)

r1a <- r1 > 0.5 
r2a <- r2 >= 2 
r3a <- r3 < 3

Raster_Result <- r1a * r2a * r3a 
table(as.vector(Raster_Result$values))
plot(Raster_Result, legend = F, asp = 1)# add a legendlegend(x='bottomright', legend = c("Suitable", "Not Suitable"),	fill = (terrain.colors(n = 2)), bty = "n")

Raster_Result <- r1a + r2a + r3a 
table(as.vector(Raster_Result$values))
image(Raster_Result, col = heat.colors(3), asp = 1) 
legend(x='bottomright',	legend = c("1 Condition", "2 Conditions", "3 Conditions"),fill = (heat.colors(n = 3)), bty = "n")

## 5.9.3 Other Raster Calculations

Raster_Result <- sin(r3) + sqrt(r1) 
Raster_Result <- ((r1 * 1000 ) / log(r3) ) * r2 
image(Raster_Result)

my.func <- function(x) {log(x)} 
Raster_Result <- calc(r3, my.func) 
# this is equivalent to 
Raster_Result <- calc(r3, log)

Raster_Result <- overlay(r2,r3,fun = function(x, y) {return(x + (y * 10))} )# alternatively using a stackmy.stack <- stack(r2, r3)Raster_Result <- overlay(my.stack, fun = function(x, y) (x + (y * 10)) )

# load meuse and convert to pointsdata(meuse)coordinates(meuse) <- ~x+y# select a point layersoil.1 <- meuse[meuse$soil == 1,]# create an empty raster layer# this is based on the extent of meuse 
r <- raster(meuse.grid)dist <- distanceFromPoints(r, soil.1) 
plot(dist)plot(soil.1, add = T)

## ANSWERS TO SELF-TEST QUESTIONS

## Q1.
densities= poly.counts(breach,blocks) / 
	ft2miles(ft2miles(poly.areas(blocks)))density.shades <- auto.shading(densities, 
	cols=brewer.pal(5, "Oranges"), cutter=rangeCuts)choropleth(blocks,densities,shading=density.shades)
choro.legend(533000,161000,density.shades) title("Incidents per Sq. Mile")

## Q2. 
# Analysis with blocksblocks2 = blocks[blocks$OCCUPIED > 0,] 
attach(data.frame(blocks2))forced.rate = 2000*poly.counts(burgres.f,blocks2)/OCCUPIED 
notforced.rate = 2000*poly.counts(burgres.n,blocks2)/ OCCUPIEDmodel1 = lm(forced.rate~notforced.rate)coef(model1)
cat("expected(forced rate)= ",coef(model1)[1], "+", coef(model1)[2], "* (not forced rate) ")## expected(forced rate)= 5.467 + 0.379 * (not forced rate)detach(data.frame(blocks2))

# Analysis with tractstracts2 = tracts[tracts$OCCUPIED > 0,] 
# align the projectionsct <- proj4string(burgres.f) 
proj4string(tracts2) <- CRS(ct)# now do the analysisattach(data.frame(tracts2))forced.rate = 2000*poly.counts(burgres.f,tracts2)/OCCUPIED 
notforced.rate = 2000*poly.counts(burgres.n,tracts2)/ OCCUPIEDmodel2 = lm(forced.rate~notforced.rate)coef(model2)## (Intercept) notforced.rate ## 5.2435 0.4133cat("expected(forced rate) = ",coef(model2)[1], "+", coef(model2)[2], "* (not forced rate) ")## expected(forced rate) = 5.243 + 0.4133 * (not forced rate)detach(data.frame(tracts2))

cat("expected(forced rate) = ",coef(model1)[1], "+", 
	coef(model1)[2], "* (not forced rate) ")
cat("expected(forced rate) = ",coef(model2)[1], "+",	coef(model2)[2], "* (not forced rate)")

## Q3. 

int.poly.counts <- function(int.layer, tracts, 
	tracts.var, var.name) {int.res <- gIntersection(int.layer, tracts, byid = T) 
# split the intersection referencestmp <- strsplit(names(int.res), " ")tracts.id <- (sapply(tmp, "[[", 2))intlayer.id <- (sapply(tmp, "[[", 1)) 
# calculate areasint.areas <- gArea(int.res, byid = T) 
tract.areas <- gArea(tracts, byid = T) 
# match this to the new layerindex <- match(tracts.id, row.names(tracts)) 
tract.areas <- tract.areas[index]tract.prop <- zapsmall(int.areas/tract.areas, 3) 
# and create data frame for the new layerdf <- data.frame(intlayer.id, tract.prop)houses <- zapsmall(tracts.var[index] * tract.prop, 1)df <- data.frame(df, houses, int.areas)# Finally, link back to the original areas 
int.layer.houses <- xtabs(df$houses~df$intlayer.id)index <- as.numeric(gsub("g", "", names(int.layer.houses))) 
# create temporary variabletmp <- vector("numeric", length = dim(data.frame(int.layer))[1])tmp[index] <- int.layer.housesi.houses <- tmp# create output dataint.layer2 <- SpatialPolygonsDataFrame(int.layer,	data = data.frame(data.frame(int.layer), i.houses),	match.ID = FALSE) 
names(int.layer2) <- c("ID", var.name) 
return(int.layer2)}

int.layer2 <- int.poly.counts(int.layer, 
	tracts,tracts$HSE_UNITS, "i.house" )
	
int.poly.counts <- function(int.layer1, int.layer2, int.layer2.var, var.name) {	int.res <- gIntersection(int.layer1, int.layer2, byid = T) 
	tmp <- 	strsplit(names(int.res), " ")	int.layer2.id <- (sapply(tmp, "[[", 2)) 
	intlayer.id <- (sapply(tmp, "[[", 1))
	int.areas <- gArea(int.res, byid = T)
	tract.areas <- gArea(int.layer2, byid = T)
	index <- match(int.layer2.id, row.names(int.layer2)) 
	tract.areas <- tract.areas[index]	tract.prop <- zapsmall(int.areas/tract.areas, 3)	df <- data.frame(intlayer.id, tract.prop)	var <- zapsmall(int.layer2.var[index] * tract.prop, 1) 
	df <- data.frame(df, var, int.areas)	int.layer1.var <- xtabs(df$var~df$intlayer.id)	index <- as.numeric(gsub("g", "", names(int.layer1.var))) 
	tmp <- vector("numeric", length = dim(data.frame(int.layer1))[1])	tmp[index] <- int.layer1.var	i.var <- tmp	int.layer.out <- SpatialPolygonsDataFrame(int.layer1,		data = data.frame(data.frame(int.layer1), i.var),		match.ID = FALSE) 
	names(int.layer.out) <- c("ID", var.name) 
	return(int.layer.out)}

# Set up the packages and data
library(GISTools)library(rgdal)data(newhaven)# define the intersection layer just to make sure 
bb <- bbox(tracts)grd <- GridTopology(cellcentre.offset = c(bb[1,1]-200,bb[2,1]-200), 	
	cellsize=c(10000,10000), cells.dim = c(5,5))int.layer <- SpatialPolygonsDataFrame( as.SpatialPolygons.GridTopology(grd),data = data.frame(c(1:25)), match.ID = FALSE)names(int.layer) <- "ID"# now run with some data# match prj4stringsct <- proj4string(blocks) 
proj4string(int.layer) <- CRS(ct) 
int.layer <- spTransform(int.layer, CRS(proj4string(blocks)))# now run the functionint.result <- int.poly.counts(int.layer, blocks, blocks$POP1990, "i.pop" )# set plot parameterspar(mar=c(0,0,0,0))# map the resultsshades = auto.shading(int.result$i.pop,n = 5, cols = brewer.pal(5, "OrRd"))choropleth(int.result, int.result$i.pop, shades) 
plot(blocks, add = T, lty = 2, lwd = 1.5) 
choro.legend(530000, 159115, bg = 	"white", shades, title = "Count", under = "")

matrix(data.frame(int.result)[,2], nrow = 5, ncol = 5, byrow = T)
