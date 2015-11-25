## newhaven.R
## created for Chapter 3
##
## Sourcing code in R scripts  
##

# load package and data
library(GISTools)
data(newhaven)
# plot spatial data
plot(blocks)
plot(roads,add=TRUE,col= 'red')
# embellish the map
map.scale(534750,152000,miles2ft(2), "Miles",4,0.5) 
north.arrow(530650,154000,miles2ft(0.25),col= 'lightblue') 
title('New Haven, CT')