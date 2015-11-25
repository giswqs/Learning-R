##
## Code for Chapter 2 
##
## This introduces data types and classes  
## 

## 2.1 INTRODUCTION

## 2.2 THE BASIC INGREDIENTS OF R: VARIABLES AND ASSIGNMENT

# examples of simple assignmentx <- 5y <- 4# the variables can be used in other operations 
x+y# including defining new variablesz <- x + y 
z# which can then be passed to other functionssqrt(z)

# example of vector assignmenttree.heights <- c(4.3,7.1,6.3,5.2,3.2,2.1) 
tree.heights
tree.heights**2
sum(tree.heights)
mean(tree.heights)
max.height <- max(tree.heights) 
max.height

tree.heightstree.heights [1] # first elementtree.heights[1:3] # a subset of elements 1 to 3sqrt(tree.heights[1:3]) # square roots of the subsettree.heights[c(5,3,2)] # a subset of elements 5,3,2: note the ordering

# examples of character variable assignmentname <- "Lex Comber"
name
# these can be assigned to a vector of character variablescities <- c("Leicester","Newcastle","London","Durham", "Exeter")citieslength(cities)

# an example of a logical variablenorthern <- c(FALSE, TRUE, FALSE, TRUE, FALSE) 
northern# this can be used to subset other variablescities[northern]

## Info Box
##### Example Script #####
## these commands will not do anything  
## Load libraries 
library(GISTools)## Load functions 
source("My.functions.R") 
## load some data
my.data <- read.csv(file = "my.data.csv")## apply a function written in My.functions.R 
cube.root.func(my.data)## apply another R functionrow.tot <- rowSums(my.data)

## 2.3 DATA TYPES AND DATA CLASSES

## 2.3.1 Data Types in R

character(8)# conversionas.character("8")# testsis.character(8) 
is.character("8")

numeric(8)# conversions
as.numeric(c("1980","-8","Geography")) 
as.numeric(c(FALSE,TRUE))# testsis.numeric(c(8, 8))is.numeric(c(8, 8, 8, "8"))

logical(7)# conversionas.logical(c(7, 5, 0, -4,5))# TRUE and FALSE can be converted to 1 and 0as.logical(c(7,5,0,-4,5)) * 1 ## [1] 1 1 0 1 1 as.logical(c(7,5,0,-4,5)) + 0 ## [1] 1 1 0 1 1# different ways to declare TRUE and FALSEas.logical(c("True","T","FALSE","Raspberry","9","0", 0))

data <- c(3, 6, 9, 99, 54, 32, -102) 
# a logical testindex <- (data > 10)index# used to subset datadata[index]sum(data)
sum(data[index])

## 2.3.2 Data Classes in R

# defining vectorsvector(mode = "numeric", length = 8)vector(length = 8)# testing and conversiontmp <- data.frame(a=10:15, b=15:20) 
is.vector(tmp)
as.vector(tmp)

# defining matricesmatrix(ncol = 2, nrow = 0) 
matrix(1:6)
matrix(1:6, ncol = 2)
as.matrix(6:3)
is.matrix(as.matrix(6:3))

flow <- matrix(c(2000, 1243, 543, 1243, 212, 545, 654, 168, 109), c(3,3), byrow=TRUE)# Rows and columns can have names, not just 1,2,3,...colnames(flow) <- c("Leicester", "Liverpool"," Elsewhere") 
rownames(flow) <- c("Leicester", "Liverpool", "Elsewhere") 
# examine the matrixflow
# and functions exist to summariseoutflows <- rowSums(flow) 
outflows
z <- c(6,7,8)names(z) <- c("Newcastle","London","Manchester") 
z

## Info box
?sumhelp(sum)# Create a variable to pass to other summary functions 
x <- matrix(c(3,6,8,8,6,1,-1,6,7),c(3,3),byrow=TRUE)# Sum over rowsrowSums(x)# Sum over columnscolSums(x)# Calculate column meanscolMeans(x)# Apply function over rows (1) or columns (2) of x 
apply(x,1,max)# Logical operations to select matrix elements 
x[,c(TRUE,FALSE,TRUE)]# Add up all of the elements in xsum(x)# Pick out the leading diagonaldiag(x)# Matrix inversesolve(x)# Tool to handle roundingzapsmall(x %*% solve(x))

# a vector assignmenthouse.type <- c("Bungalow", "Flat", "Flat", "Detached", "Flat", "Terrace", "Terrace")# a factor assignmenthouse.type <- factor(c("Bungalow", "Flat","Flat", "Detached", "Flat", "Terrace", "Terrace"), levels=c("Bungalow","Flat","Detached","Semi","Terrace"))house.type# table can be used to summarisetable(house.type)# 'levels' control what can be assigned 
house.type <- factor(c("People Carrier", "Flat","Flat", "Hatchback", "Flat", "Terrace", "Terrace"),levels=c("Bungalow","Flat","Detached","Semi","Terrace")) 
house.type

income <-factor(c("High", "High", "Low", "Low", "Low", "Medium", "Low", "Medium"), levels=c("Low", "Medium", "High"))income > "Low"# ‘levels’ in ‘ordered’ defines a relative orderincome <-ordered (c("High", "High", "Low", "Low", "Low", "Medium", "Low", "Medium"), levels=c("Low", "Medium", "High"))income > "Low"

tmp.list <- list("Lex Comber",c(2005, 2009), "Lecturer", matrix(c(6,3,1,2), c(2,2))) 
tmp.list
# elements of the list can be selectedtmp.list[[4]]
employee <- list(name="Lex Comber", start.year = 2005, position="Professor")employee
append(tmp.list, list(c(7,6,9,1)))
lapply(tmp.list[[2]], is.numeric)
lapply(tmp.list, length)

employee <- list(name="Lex Comber", start.year = 2005, position="Professor")
class(employee) <- "staff"
print.staff <- function(x) {	cat("Name: ",x$name, "\n")	cat("Start Year: ",x$start.year, "\n") 
	cat("Job Title: ",x$position, "\n")}# an example of the print class  print(employee)

print.staff <- function(x) {cat("Name: ",x$name, "\n")cat("Start Year: ",x$start.year, "\n") cat("Job Title: ",x$position, "\n")}# an example of the print classprint(employee)
print(unclass(employee))

new.staff <- function(name,year,post) {
	result <- list(name=name, start.year=year,
	position=post)
	class(result) <- "staff"
	return(result)}

leics.uni <- vector(mode='list',3)# assign values to elements in the listleics.uni[[1]] <- new.staff("Fisher, Pete", 1991, "Professor")leics.uni[[2]] <- new.staff("Comber, Lex", 2005, "Lecturer")leics.uni[[3]] <- new.staff("Burgess, Robert", 1998, "VC")
leics.uni

## 2.3.3 Self-Test Questions

colours <- factor(c("red","blue","red","white", "silver","red","white","silver", "red","red","white","silver","silver"), levels=c("red","blue","white","silver","black"))

## Self-Test Question 1. 
colours[4] <- "orange"colours

colours <- factor(c("red","blue","red","white", "silver", "red", "white", "silver", "red","red","white","silver","silver"), levels=c("red","blue","white","silver","black"))table(colours)

colours2 <-c("red","blue","red","white", "silver","red","white","silver", "red","red","white","silver")# Now, make the tabletable(colours2)

## Self-Test Question 2.
car.type <- factor(c("saloon","saloon","hatchback", "saloon","convertible","hatchback","convertible", "saloon", "hatchback","saloon", "saloon", "saloon", "hatchback"), levels=c("saloon","hatchback","convertible"))
table(car.type, colours)
crosstab <- table(car.type,colours)

## Self-Test Question 3.
engine <- ordered(c("1.1litre","1.3litre","1.1litre", "1.3litre","1.6litre","1.3litre","1.6litre", "1.1litre","1.3litre","1.1litre", "1.1litre", "1.3litre","1.3litre"), levels=c("1.1litre","1.3litre","1.6litre"))
engine > "1.1litre"

## Self-Test Question 4. 
dim(crosstab) # Matrix dimensionsrowSums(crosstab) # Row sumscolnames(crosstab) # Column names
apply(crosstab,1,max)
apply(crosstab,2,max)
example <- c(1.4,2.6,1.1,1.5,1.2) 
which.max(example)

## Self-Test Question 5.
 
## Self-Test Question 6. 
levels(engine)
levels(colours)[which.max(crosstab[,1])]
colnames(crosstab)[which.max(crosstab[,1])]
colnames(crosstab)
crosstab[,1]
which.max(crosstab[,1])

# Defines the functionwhich.max.name <- function(x) { 
	return(names(x)[which.max(x)])}# Next, give the variable ‘example’ names for the valuesnames(example) <- c("Leicester","Nottingham", "Loughborough","Birmingham","Coventry")example
which.max.name(example)

## Self-Test Question 7. 

## Self-Test Question 8.

new.sales.data <- function(colours, car.type) {
	xtab <- table(car.type,colours)
	result <- list(colour=apply(xtab,1,which.max.name),
		type=apply(xtab,2,which.max.name),
		total=sum(xtab)) 
	class(result) <- "sales.data"	return(result)}
this.week <- new.sales.data(colours,car.type) 
this.week

## 2.4 PLOTS

## 2.4.1 Basic Plot Tools

x1 <- rnorm(100) 
y1 <- rnorm(100) 
plot(x1,y1)
plot(x1,y1,pch=16, col='red')

x2 <- seq(0,2*pi,len=100)y2 <- sin(x2)plot(x2,y2,type='l')plot(x2,y2,type='l', lwd=3, col='darkgreen')

plot(x2,y2,type='l', col='darkgreen', lwd=3, ylim=c(-1.2,1.2))y2r <- y2 + rnorm(100,0,0.1)points(x2,y2r, pch=16, col='darkred')

y4 <- cos(x2)plot(x2, y2, type='l', lwd=3, col='darkgreen') 
lines(x2, y4, lwd=3, lty=2, col='darkblue')

## Info Box
par(mfrow = c(1,2)) 
# resetpar(mfrow = c(1,1))

x2 <- seq(0,2*pi,len=100)y2 <- sin(x2)y4 <- cos(x2)# specify the plot order: see ?par for more information 
par(mfrow = c(1,2))# plot #1plot(y2,y4)polygon(y2,y4,col='lightgreen')# plot #2: this time with ‘asp’ to set the aspect ratio of the axesplot(y2,y4, asp=1, type='n') 
polygon(y2,y4,col='lightgreen')

# you may have done this in Chapter 1
install.packages("GISTools", depend = T)
# if this fails because of versioning eg with R 3.1.2 try
install.packages("GISTools", type = "source") 
library(GISTools)
# library(GISTools)data(georgia)# select the first element 
appling <- georgia.polys[[1]]
# set the plot extentplot(appling, asp=1, type='n', xlab="Easting", ylab="Northing")# plot the selected features with hatching 
polygon(appling, density=14, angle=135)

## 2.4.2 Plot Colours

plot(appling, asp=1, type='n', xlab="Easting", ylab="Northing")polygon(appling, col=rgb(0,0.5,0.7))
polygon(appling, col=rgb(0,0.5,0.7,0.4))

# set the plot extentplot(appling, asp=1, type='n', xlab="Easting", ylab="Northing")# plot the pointspoints(x = runif(500,126,132)*10000,	y = runif(500,103,108)*10000, pch=16, col='red') 
# plot the polygon with a transparency factor 
polygon(appling, col=rgb(0,0.5,0.7,0.4))

plot(appling, asp=1, type='n', xlab="Easting", ylab="Northing")polygon(appling, col="#B3B333")# add text, sepcifying its placement, colour and size 
text(1287000,1053000, "Appling County",cex=1.5) 
text(1287000,1049000, "Georgia",col='darkred')

plot(c(-1.5,1.5),c(-1.5,1.5),asp=1, type='n')# plot the green/blue rectanglerect(-0.5,-0.5,0.5,0.5, border=NA, col=rgb(0,0.5,0.5,0.7))# then the second onerect(0,0,1,1, col=rgb(1,0.5,0.5,0.7))

# load some grid datadata(meuse.grid)# define a SpatialPixelsDataFrame from the datamat = SpatialPixelsDataFrame(points = meuse.grid[c("x","y")], data = meuse.grid)# set some plot parameters (1 row, 2 columns) 
par(mfrow = c(1,2))# set the plot marginspar(mar = c(0,0,0,0))# plot the points using the default shading 
image(mat, "dist")# load the packagelibrary(RColorBrewer)# select and examine a colour palette with 7 classes 
greenpal <- brewer.pal(7,'Greens')# and now use this to plot the dataimage(mat, "dist", col=greenpal)

## 2.5 READING, WRITING, LOADING AND SAVING DATA

## 2.5.1 Text Files

# display the first six rowshead(appling)# display the variable dimensions 
dim(appling)
colnames(appling) <- c("X", "Y")

write.csv(appling, file = "test.csv")
write.csv(appling, file = "test.csv", row.names = F)
tmp.appling <- read.csv(file = "test.csv")

## 2.5.2 R Data Files

# this will save everything in the workspacesave(list = ls(), file = "MyData.RData")# this will save just applingsave(list = "appling", file = "MyData.RData")# this will save appling and georgia.polys 
save(list = c("appling", "georgia.polys"), file = "MyData.RData")
load("MyData.RData")

## 2.5.3 Spatial Data Files

data(georgia)writePolyShape(georgia, "georgia.shp",)
new.georgia <- readShapePoly("georgia.shp")

## ANSWERS TO SELF-TEST QUESTIONS

## Q4
# Undo the colour[4] <- "orange" line used abovecolours <- factor(c("red","blue","red","white", "silver","red","white","silver", "red","red","white","silver"),levels=c("red","blue","white","silver","black")) 
colours[engine > "1.1litre"]
table(car.type[engine < "1.6litre"])
table(colours[(engine >= "1.3litre") & (car.type == "hatchback")])

## Q6
apply(crosstab,1,which.max)

## Q7apply(crosstab,1,which.max.name)
apply(crosstab,2,which.max.name)

## Q8
most.popular <- list(colour=apply(crosstab,1,which.max.name),type=apply(crosstab,2,which.max.name)) 
most.popular

## Q9
print.sales.data <- function(x) { 
	cat("Weekly Sales Data:\n") 
	cat("Most popular colour:\n") 
	for (i in 1:length(x$colour)) {
		cat(sprintf("%12s:%12s\n",names(x$colour)[i], 
		x$colour[i]))}	cat("Most popular type:\n") 
	for (i in 1:length(x$type)) {
		cat(sprintf("%12s:%12s\n",names(x$type)[i],
		x$type[i]))}	cat("Total Sold = ",x$total)}
