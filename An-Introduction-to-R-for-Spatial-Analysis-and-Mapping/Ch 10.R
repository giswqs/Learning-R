##
## Code for Chapter 10
##
## 

library(plyr)# Create a data frame with three columns (x, y and z)
test.set <- data.frame(x = rnorm(100, 1, 1), 
	y = rnorm(100, 2, 1), 
	z = rnorm(100, 3, 1))median.by.col <- colwise(median) 
median.by.col(test.set)

h.mean <- function(x) 1/mean(1/x) 
h.mean.by.col <- colwise(h.mean) 
h.mean.by.col(test.set)

x <- 5f <- function(x) {	x <- x + 1 
	return(x*x)
}f(x)
x