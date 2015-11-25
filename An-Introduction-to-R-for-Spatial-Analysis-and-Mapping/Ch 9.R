
## ----setup3d,echo=FALSE,message=FALSE,results='hide',cache=FALSE---------
opts_chunk$set(message=FALSE,fig.pos='H',fig.lp='fig:',fig.align='center',tidy=FALSE,warning=FALSE,cache=FALSE,size='small')


## ----public,cache=TRUE---------------------------------------------------
fpe <- read.table("http://data.princeton.edu/wws509/datasets/effort.dat")
head(fpe)


## ----splom,fig.cap="Scatterplot Matrix of Princeton Data"----------------
pairs(fpe,panel=panel.smooth)


## ----biocinst,eval=FALSE-------------------------------------------------
## source("http://bioconductor.org/biocLite.R")
## biocLite()


## ----rgvinst,eval=FALSE--------------------------------------------------
## source("http://bioconductor.org/biocLite.R")
## biocLite("Rgraphviz")


## ----urlsrce,eval=FALSE--------------------------------------------------
## source("http://bioconductor.org/biocLite.R")


## ----grdem,fig.cap='Illustration of Rgraphviz',out.height='8cm',out.width='8cm'----
# The following two packages are required:
require(Rgraphviz)
require(datasets)
# Load the state.x77 data
data(state)
# Which ones are 'connected' - ie abs correlation above 0.3
connected <- abs(cor(state.x77)) > 0.5 
# Create the graph - node names are the column names
conn <- graphNEL(colnames(state.x77))
# Populate with edges - join variables that are TRUE in 'connected'
for (i in colnames(connected)) {
  for (j in colnames(connected)) {
    if (i < j) {
      if (connected[i,j]) {
        conn <- addEdge(i,j,conn,1)}}}}
# Create a layout for the graph
conn <- layoutGraph(conn)
# Specify some drawing parameters
attrs <- list(node=list(shape="ellipse", 
                        fixedsize=FALSE, fontsize=12))
# Plot the graph
plot(conn,attrs=attrs)


## ----lrcurl--------------------------------------------------------------
library(RCurl) # Load RCurl


## ----curly,cache=FALSE---------------------------------------------------
library(RCurl) # Load RCurl
# Get the content of the URL and store it into 'temp'
stem <- 'https://www.gov.uk/government/uploads/system/uploads'
file1 <- '/attachment_data/file/15240/1871702.csv'
temp <- getURL(paste0(stem,file1))
# Use textConnection to read the content of temp
# as though it were a CSV file
imd <- read.csv(textConnection(temp))
# Check - this gives the first 10 column names of the data frame
head(colnames(imd),n=10)


## ----csvfun--------------------------------------------------------------
read.csv.https <- function(url) {
  temp <- getURL(url)
  return(read.csv(textConnection(temp)))
}


## ----boxplot,fig.cap="Boxplot of IMDs by Goverment Office Regions",cache=TRUE----
# Download the csv dataand put it into 'imd2'
imd2 <- read.csv.https(paste0(stem,file1))
# Modify the margins around the plot, to fit the GOR
# names into the left-hand margin
par(mar=c(5,12,4,2) + 0.1)
# Create the boxplot. The 'las' parameter specificies y-axis 
# labelling is horizontal, x-axis is vertical
boxplot(IMD.SCORE~GOR.NAME,data=imd,horizontal=TRUE,las=2)


## ----pukurl,eval=FALSE---------------------------------------------------
## http://data.police.uk/api/crimes-street/all-crime?lat=<latitude>
##   &lng=<longitude>&date=<date>


## ----pukurl2,eval=FALSE--------------------------------------------------
## http://data.police.uk/api/crimes-street/all-crime?lat=53.401422
##   &lng=-2.965075&date=2013-04


## ----pukurl3-------------------------------------------------------------
crimes.buf <- getForm(
  "http://data.police.uk/api/crimes-street/all-crime",
  lat=53.401422,
  lng=-2.965075,
  date="2013-04")


## ----pukapi--------------------------------------------------------------
require(rjson)
crimes <- fromJSON(crimes.buf)


## ----checkjs,size='footnotesize'-----------------------------------------
crimes[[1]]


## ----extractor1----------------------------------------------------------
getLonLat <- function(x) as.numeric(c(x$location$longitude,
                                      x$location$latitude))
crimes.loc <- t(sapply(crimes,getLonLat))
head(crimes.loc)


## ----extractor2----------------------------------------------------------
getAttr <- function(x) c(
  x$category,
  x$location$street$name,
  x$location_type)
crimes.attr <- as.data.frame(t(sapply(crimes,getAttr)))
colnames(crimes.attr) <- c("category","street","location_type")
head(crimes.attr)


## ----makespdf------------------------------------------------------------
library(GISTools)
crimes.pts <- SpatialPointsDataFrame(crimes.loc,crimes.attr)
# Specify the projection - in this case just geographical coordinates
proj4string(crimes.pts) <- CRS("+proj=longlat")
# Note that 'head' doesn't work on SpatialPointsDataFrames
crimes.pts[1:6,] 


## ----getasb--------------------------------------------------------------
asb.pts <- crimes.pts[crimes.pts$category=="anti-social-behaviour",]


## ----getcda--------------------------------------------------------------
cda.pts <- crimes.pts[crimes.pts$category=="criminal-damage-arson",]


## ----ctmap,fig.cap="Locations of anti-social behaviour and criminal damage/arson incidents"----
plot(asb.pts,pch=16,col='grey70')
plot(cda.pts,pch=16,col='black',add=TRUE)
legend('bottomright',c("Antisocial behaviour","Criminal damage"),
       col=c("grey70","black"),pch=16)


## ----nest,warning=TRUE---------------------------------------------------
terr3bed.buf <- getForm("http://api.nestoria.co.uk/api",
  action='search_listings',
  place_name='liverpool',
  encoding='json',
  listing_type='buy',
  number_of_results=50,
  bedroom_min=3,bedroom_max=3,
  keywords='terrace')
terr3bed <- fromJSON(terr3bed.buf)


## ----hpdf----------------------------------------------------------------
getHouseAttr <- function(x) {
  as.numeric(c(x$price/1000,x$longitude,x$latitude))
}

terr3bed.attr <- as.data.frame(t(sapply(terr3bed$response$listings,
                                        getHouseAttr)))

colnames(terr3bed.attr) <- c("price","longitude","latitude")
head(terr3bed.attr)


## ----crimeinfo,cache=TRUE------------------------------------------------
# Create an extra column to contain burglary rates
terr3bed.attr <- transform(terr3bed.attr,burgs=0)

# For each house in the data frame
for (i in 1:50) {
  # Firstly obtain crimes in a 1-mile radius of the houses
  # latitude and longitude and decode it from JSON form
  crimes.near <- getForm(
  "http://data.police.uk/api/crimes-street/all-crime",
  lat=terr3bed.attr$latitude[i],
  lng=terr3bed.attr$longitude[i],
  date="2013-04")
  crimes.near <- fromJSON(crimes.near)
  crimes.near <- as.data.frame(t(sapply(crimes.near,getAttr)))
  # Then from the 'category' column count the number of burlaries
  # and assign it to the burgs column
  terr3bed.attr$burgs[i] <- sum(crimes.near[,1] == 'burglary')
  # Pause before running next API request - to avoid overloading
  # the server
  Sys.sleep(0.7)
  # Note this stage may cause the code to take a minute or two to run
}


## ----relationship,fig.cap="Scatter plot of Burglary Rate vs. House Price"----
plot(price~burgs,data=terr3bed.attr,log='y',
     xlab='Burglaries in a 1-mile radius',
     ylab='House Price (1000s pounds)')


## ----regr----------------------------------------------------------------
summary(lm(log(price)~burgs,data=terr3bed.attr))


## ----getgmap,results='hide'----------------------------------------------
require(RgoogleMaps)
LivMap <- GetMap(c(53.401422,-2.965075),zoom=14)


## ----backdrop------------------------------------------------------------
backdrop <- function(gmt) {
  # Set x and y plot limits
  limx <- c(-320,320) 
  limy <- c(-320,320)
  # Make the map fill rhe entire window
  par(mar=c(0,0,0,0))
  # Create the empty plot
  plot(limx,limy,type='n',asp=1,
       xlab='',ylab='',xaxt='n',yaxt='n',bty='n')
  # Draw a box around it
  box()
  # Fill it with the raster map
  rasterImage(gmt$myTile,-320,-320,320,320)
  }


## ----bdmap,fig.cap="Google Maps backfdrop for Liverpool"-----------------
backdrop(LivMap)


## ----getXYs--------------------------------------------------------------
asb.XY <- LatLon2XY.centered(LivMap,
                             coordinates(asb.pts)[,2],
                             coordinates(asb.pts)[,1])


## ----asbmap,fig.cap="Locations of Anti-Social Behaviour Incidents"-------
backdrop(LivMap)
points(asb.XY$newX,asb.XY$newY,
       pch=16,col='darkred',cex=1.5)
points(asb.XY$newX,asb.XY$newY,
       pch=16,col='white',cex=0.75)


## ----asbmap2,fig.cap="Locations of Anti-Social Behaviour Incidents Representing Imprecision"----
backdrop(LivMap)
points(asb.XY$newX,asb.XY$newY,
       pch=16,col=rgb(0.7,0,0,0.15),cex=3)


## ----distns--------------------------------------------------------------
# Compute distance along side of map tile
# components BBOX$ur and BBOX$ll give lat/long of upper right and 
# lower left of the map tile
dist.ns <- (3959 * pi / 180) * (LivMap$BBOX$ur[,1] - LivMap$BBOX$ll[,1])
dist.ns


## ----verify,fig.cap="A test of the accuracy of the 1-mile circle radius estimate"----
# Set conversion factor between pixels and miles
cf <-  282.8126  
theta <- seq(0,pi*2,l=100) # Angles for points on a circle
# (sin,cos) Circle is of radius 1 so multiply by cf
# to get 1 mile in pixel units
circle <- cbind(sin(theta),cos(theta)) * cf 
backdrop(LivMap) # So draw it on the backdrop map...
# Draw the circle - note that (0,0) in pixel coords 
# is the centre of the map.
polygon(circle,col=rgb(0.5,0,1,0.1)) 
# Plot the crimes as well,  to compare with the 1 mile radius circle
points(asb.XY$newX,asb.XY$newY,pch=16,col='blue')  


## ----greyhole,fig.cap="Anti-Social Behaviour Map with Non-Study Area Region Greyed Out"----
hole.bg <- rbind(circle,
                 1.02*cbind(c(0,-320,-320,320,320,0),
                            c(320,320,-320,-320,320,320)))
# The above creates a polygon shape equivalent to a 
# rectangular tile with a circular hole. 
backdrop(LivMap) # So draw the backdrop map...
points(asb.XY$newX,asb.XY$newY,pch=16,
       col=rgb(1,0,0,0.1),cex=3)  # Plot the crimes
 # next draw the tile-with-a-hole - 
polygon(hole.bg,col=rgb(0,0,0,0.2),border=NA)


## ----grepper1------------------------------------------------------------
grepl('Chris[0-9]*',c('Chris','Lex','Chris1999','Chris Brunsdon'))


## ----grepper2------------------------------------------------------------
grep('Chris[0-9]*',c('Chris','Lex','Chris1999','Chris Brunsdon'))


## ----grepper3------------------------------------------------------------
grep('Chris[0-9]*',c('Chris','Lex','Chris1999','Chris Brunsdon'),
     value=TRUE)


## ----grabbit-------------------------------------------------------------
web.buf <- readLines(
  "http://traintimes.org.uk/durham/leicester/00:00/monday")


## ----findtimes-----------------------------------------------------------
times <- grep("[0-2][0-9]:[0-5][0-9].*[0-2][0-9]:[0-5][0-9]",
              web.buf,value=TRUE)


## ----gettimes------------------------------------------------------------
locs <- gregexpr("[0-2][0-9]:[0-5][0-9]",times)
# Show the match information for times[1]
locs[[1]]


## ----makedata------------------------------------------------------------
timedata <- matrix(0,length(locs),4)
ptr <- 1
for (loc in locs) {
  timedata[ptr,1] <- as.numeric(substr(times[ptr],loc[1],loc[1]+1))
  timedata[ptr,2] <- as.numeric(substr(times[ptr],loc[1]+3,loc[1]+4))
  timedata[ptr,3] <- as.numeric(substr(times[ptr],loc[2],loc[2]+1))
  timedata[ptr,4] <- as.numeric(substr(times[ptr],loc[2]+3,loc[2]+4))
  ptr <- ptr + 1
}
colnames(timedata) <- c('h1','m1','h2','m2')
timedata <- transform(timedata,duration = h2 + h2/60 - h1 - m1/60 )
timedata


