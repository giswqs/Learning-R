##
## Code for Chapter 4 
##
## This describes different methods for 
## repeatedly doing something and how to 
## control such processes  
## 

## 4.1 OVERVIEW

## 4.2 INTRODUCTION

tree.heights <- c(4.3,7.1,6.3,5.2,3.2)
tree.heights
if (tree.heights[1] < 6) { cat('Tree is small\n') } else { cat('Tree is large\n')}

for (i in 1:3) {	if (tree.heights[i] < 6) { cat('Tree',i,' is small\n') }	else { cat('Tree',i, 'is large\n')} }

assess.tree.height <- function(tree.list, thresh)	{ for (i in 1:length(tree.list))	{ if(tree.list[i] < thresh) {cat('Tree',i, ' is small\n')} else { 
	cat('Tree',i, ' is large\n')}	}
} 
assess.tree.height(tree.heights, 6)

tree.heights2 <- c(8,4.5,6.7,2,4) 
assess.tree.height(tree.heights2, 4.5)

## 4.3 BUILDING BLOCKS FOR PROGRAMS

## 4.3.1 Conditional Statements

x <- -7if (x < 0) cat("x is negative")
x <- 8if (x < 0) cat("x is negative")

x <- -7if (x < 0) cat("x is negative") else cat("x is positive")x <- 8if (x < 0) cat("x is negative") else cat("x is positive")

??is.

x <- c(1,3,6,8,9,5)if (all(x > 0)) cat("All numbers are positive")

x <- c(1,3,6,-8,9,5)if (any(x > 0)) cat("Some numbers are positive")

any(x==0)

## 4.3.2 Code Blocks

x <- c(1,3,6,8,9,5) 
if (all(x > 0)) {	cat("All numbers are positive\n") 
	total <- sum(x)	cat("Their sum is",total) }

x <- c(1,3,6,8,9,-5) 
if (all(x > 0)) {	cat("All numbers are positive\n")	total <- sum(x)	cat("Their sum is",total) } else { 
		cat("Not all numbers are positive\n") 
		cat("This is probably an error\n") 
		cat("as numbers are rainfall levels") }

## 4.3.3 Functions

mean.rainfall <- function(rf)  
{ if (all(rf> 0))                 #open Function
  { mean.value <- mean(rf)        #open Consequent
    cat("The mean is ",mean.value) 
  } else                          #close Consequent
    { cat("Warning: Not all values are positive\n")  #open Alternative
    }                             #close Alternative
  }                               #close Function
mean.rainfall(c(8.5,9.3,6.5,9.3,9.4))  

mean.rainfall2 <- function(rf) { 
	if (all(rf> 0)) {
		return( mean(rf))} else { 
		return(NA)}}mr <- mean.rainfall2(c(8.5,9.3,6.5,9.3,9.4)) 
mr

rf <- "Tuesday" 
mean.rainfall2(c(8.5,9.3,6.5,9.3,9.4))

for (i in 1:5) {	i.cubed <- i * i * i	cat("The cube of",i, "is",i.cubed, "\n")}

for (val in seq(0,1,by=0.25)) {	val.squared <- val * val	cat("The square of",val, "is",val.squared, "\n")}

i <- 1; n <- 654 
repeat{i.squared <- i * iif (i.squared > n) break 
i <- i + 1 }cat("The first square number exceeding",n, "is ",i.squared, "\n")

first.bigger.square <- function(n) { 
	i <- 1	repeat{		i.squared <- i * i		if (i.squared > n) break 
		i <- i + 1 }
	return(i.squared)}first.bigger.square(76987)

## 4.3.5 Debugging

debug(mean.rainfall2)
mean.rainfall2(c(8.5,9.3,6.5,9.3,9.4))

## 4.4 WRITING FUNCTIONS

## 4.4.1 Introduction

cube.root <- function(x) { 
	result <- x ^ (1/3) 
	return(result)}cube.root(27)

# source("functions.R")

cube.root <- function(x) { 
	result <- x ^ (1/3) 
	return(result)}

source('functions.R')cube.root(343)cube.root(99)

circle.area <- function(r) { 
	result <- pi * r ^ 2 
	return(result)}

source('functions.R')cube.root(343)circle.area(10)

## 4.4.2 Data Checking

cube.root(-343)

cube.root <- function(x) { 
	if (x >= 0) {	result <- x ^ (1/3) } else {	result <- -(-x) ^ (1/3) } 
	return(result)}

cube.root(3)
cube.root(-3)

debug(cube.root)
cube.root(-50)
undebug(cube.root)

help(debug)

## 4.4.3 More Data Checking

cube.root("Leicester")

is.numeric(77) 
is.numeric("Lex") 
is.numeric("77")v <- "Two Sevens Clash" 
is.numeric(v)

cube.root <- function(x) { 
	if (is.numeric(x)) {		if (x >= 0) { result <- x^(1/3) } 
		else { result <- -(-x)^(1/3) } 
		return(result) }	else {		cat("WARNING: Input must be numerical, not character\n")		return(NA)}}

result <- sign(x)*abs(x)^(1/3)

## 4.4.4 Loops Revisited

gcd <- function(a,b){ 
	divisor <- min(a,b) 
	dividend <- max(a,b) 
	repeat		{ remainder <- dividend %% divisor 
			dividend <- divisor
			divisor <- remainder
			if (remainder == 0) break
		}      return(dividend)}

gcd(6,15)gcd(25,75)gcd(31,33)

cube.root.table <- function(n) 
	{ 
		for (x in 1:n)
		{ 
			cat("The cube root of ", x, " is", cube.root(x), "\n") 
		}
	}

## 4.4.5 Further Activity

cube.root.table <- function(n) 
	{ 
		for (x in 1:n)
		{ 
			cat(sprintf("The cube root of %4.0f is %8.4f \n",x,cube.root(x)))
		}
	}
## 4.5 WRITING FUNCTIONS FOR SPATIAL DATA

rm(list = ls())
library(GISTools)data(georgia)
ls()

plot(georgia.polys[[1]],asp=1,type='l')

## 4.5.1 Drawing Polygons in a List

plot(c(939200,1419420),c(905510,1405900),asp=1,type='n')invisible(lapply(georgia.polys,polygon))

plot(c(939200,1419420),c(905510,1405900),asp=1,      type='n',xlab='',ylab='',xaxt='n',yaxt='n',bty='n')
      
## 4.5.2 Automatically Choosing the Bounding Box

poly1 <- georgia.polys[[1]] 
min(poly1[,1])

most.eastern <- function(poly) {return(min(poly[,1]))}
most.eastern.list <- lapply(georgia.polys,most.eastern)
most.eastern.list <- lapply(georgia.polys, function(poly) {return(min(poly[,1]))} )
most.eastern.list <- lapply(georgia.polys, function(poly) return(min(poly[,1])) )
min(unlist(most.eastern.list))

# Function definitionmost.eastern.point <- function(polys) { 
	# Most eastern points
	most.eastern.list <- lapply(polys,		function(poly) return(min(poly[,1])))
		# Return the smallest		return(min(unlist(most.eastern.list)))}

most.eastern.point(georgia.polys)

## Self-Test Question 6.

# when you have created the functions
c(most.eastern.point(georgia.polys),      most.western.point(georgia.polys))c(most.southern.point(georgia.polys),      most.northern.point(georgia.polys))

## 4.5.3 Shaded Maps

data(georgia)
names(georgia)georgia$PctRural
classifier <- factor(ifelse(georgia$PctRural > 50,      "rural","urban"))

fill.cols <- vector(mode="character", length=length(classifier))
fill.cols[classifier=="urban"] <- "yellow" 
fill.cols[classifier=="rural"] <- "darkgreen"

# NB. ew is east/west, ns is north/south# apply functions to determine bounding coordinates 
ew <- c(most.eastern.point(georgia.polys),	most.western.point(georgia.polys)) 
ns <- c(most.southern.point(georgia.polys),	most.northern.point(georgia.polys)) 
# set the plot parameterspar(mar = c(0,0,0,0)) 
plot(ew,ns,asp=1, type='n',xlab='',ylab='',xaxt='n',yaxt='n',bty='n') 
invisible(mapply(polygon,georgia.polys,col=fill.cols))

## Self-Test Question 8.
hatching <- vector(mode="numeric", length=length(georgia.polys))

## ANSWERS TO SELF-TEST QUESTIONS

## Q1. 
cube.root.2 <- function(x) 
 { if (is.numeric(x)) 
   { result <- sign(x)*abs(x)^(1/3)
      return(result)  
   } else
 { cat("WARNING: Input must be numerical, not character\n") 
   return(NA) }
}

## Q2.
gcd <- function(a,b)
  {
     divisor <- min(a,b) # line 1
     dividend <- max(a,b) # line 1
     repeat #line 5
       { remainder <- dividend %% divisor #line 2
         dividend <- divisor # line 3
         divisor <- remainder # line 4
         if (remainder == 0) break #line 6 
        }
     return(dividend)
 }

## Q3.
gcd.60 <- function(a)
  {
    for(i in 1:a) 
    { divisor <- min(i,60)
       dividend <- max(i,60)
     	repeat 
      	{ remainder <- dividend %% divisor
         	dividend <- divisor
         	divisor <- remainder
         	if (remainder == 0) break
         }
      cat(dividend, "\n")	
     }
  }

gcd.60 <- function(a)
  {  for(i in 1:a) 
    { dividend <- gcd(i,60)
      cat(i,":", dividend, "\n")	
    }
  }

gcd.all <- function(x,y)
  {  for(n1 in 1:x) 
    {  for (n2 in 1:y) 
        { dividend <- gcd(n1, n2)
       	  cat("when x is",n1,"& y is",n2,"dividend =",dividend,"\n")
     		}
     }
  }

## Q4.
cube.root.table <- function(n)
  { for (x in seq(0.5, n, by = 0.5)) 
    { cat("The cube root of ",x," is", 
          sign(x)*abs(x)^(1/3),"\n")  }
  }

cube.root.table <- function(n)
  { if (n < 0 ) by.val = 0.5
    if (n < 0 ) by.val =-0.5
    for (x in seq(0.5, n, by = by.val)) 
    { cat("The cube root of ",x," is", 
      sign(x)*abs(x)^(1/3),"\n") }
  }

## Q5.
draw.polys <- function(poly.list) 
    { plot(c(939200,1419420),c(905510,1405900),asp=1,
        type='n',xlab='',ylab='',xaxt='n',yaxt='n',bty='n')
      invisible(lapply(poly.list,polygon)) 
    }
# Test it
draw.polys(georgia.polys)   

## Q6.
# The function definitions 
most.western.point <- function(polys) {
    most.western.list <- lapply(georgia.polys, 
        function(poly) return(max(poly[,1]))) 
    return(max(unlist(most.western.list)))} 
#
most.southern.point <- function(polys) {
    most.southern.list <- lapply(georgia.polys, 
        function(poly) return(min(poly[,2]))) 
    return(min(unlist(most.southern.list)))} 
#
most.northern.point <- function(polys) {
    most.northern.list <- lapply(georgia.polys, 
        function(poly) return(max(poly[,2]))) 
    return(max(unlist(most.northern.list)))} 
# Test the functions
c(most.eastern.point(georgia.polys), 
  most.western.point(georgia.polys))
c(most.southern.point(georgia.polys), 
  most.northern.point(georgia.polys))

## Q7.
# NB. ew = east/west ns=north/south
draw.polys <- function(poly.list) {  
    ew <- c(most.eastern.point(poly.list), 
            most.western.point(poly.list))
    ns <- c(most.southern.point(poly.list), 
            most.northern.point(poly.list))        
    plot(ew,ns,asp=1,
         type='n',xlab='',ylab='',xaxt='n',yaxt='n',bty='n')
    invisible(lapply(poly.list,polygon)) }
#
# Test it - it should look the same as before!
#
draw.polys(georgia.polys)   

hatch.densities <- vector(mode="numeric",length=length(georgia.polys))
hatch.densities[classifier=="urban"] <- 40
hatch.densities[classifier=="rural"] <- 0
# This assumes ew and ns were defined earlier
plot(ew,ns,asp=1,
     type='n',xlab='',ylab='',xaxt='n',yaxt='n',bty='n')
invisible(mapply(polygon,georgia.polys,density=hatch.densities))







