##
## Code for Chapter 1 
##
## These are introductory code examples for  
## installing and loading packages 
## highlighting depreciation
## 

install.packages("GISTools", dependencies = T) 
# if this fails because of versioning eg with R 3.1.2 try
install.packages("GISTools", type = "source") 
 
library(GISTools)

data(newhaven)# the variables can be used in other operations 
overlay(places, blocks)
