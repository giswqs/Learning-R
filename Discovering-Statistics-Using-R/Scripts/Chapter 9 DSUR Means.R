#---------------------------------------------------------------------------------------------------------
#R Code for Chapter 9 of:
#
#Field, A. P. & Miles, J. N. V. (2012). Discovering Statistics Using R: and Sex and Drugs and Rock 'N' Roll. London Sage
#
#(c) 2011 Andy P. Field & Jeremy N. V. Miles
#-----------------------------------------------------------------------------------------------------------

#----Set the working directory------

setwd("~/Documents/Academic/Data/DSU_R/Chapter 09 (t-tests)")
imageDirectory<-"~/Documents/Academic/Books/Discovering Statistics/DSU R/DSU R I/DSUR I Images"

setwd("~/Public/Academic/Data/DSU_R/Chapter 09 (t-tests)")
imageDirectory<-"~/Public/Academic/Books/Discovering Statistics/DSU R/DSU R I/DSUR I Images"

#----Set the working directory for Zoe ------
setwd("~/Dropbox/Team Field/DSUR/DSUR II/Chapter 09 (t-tests)")
imageDirectory<-"~/Dropbox/Team Field/DSUR/DSUR II/DSUR II Images"


#----Install Packages-----
install.packages("ggplot2")
install.packages("pastecs")
install.packages("WRS")
install.packages("Hmisc")


#------And then load these packages, along with the boot package.-----
library(ggplot2)
library(pastecs)
library(reshape)
library(Hmisc)

library(Rcmdr)
library(WRS)
source("http://dornsife.usc.edu/assets/sites/239/docs/Rallfun-v21")


#load the data

invisibility<-read.delim("invisibility.dat", header = TRUE)
invisibility$Cloak<-factor(invisibility$Cloak, levels = c(0:1), labels = c("No Cloak", "Cloak"))


#Self test:
#Enter data into a dataframe called CloakLong:

Group<-gl(2, 12, labels = c("No Cloak", "Cloak"))
Mischief<-c(3, 1, 5, 4, 6, 4, 6, 2, 0, 5, 4, 5, 4, 3, 6, 6, 8, 5, 5, 4, 2, 5, 7, 5)
CloakLong<-data.frame(Group, Mischief)

#draw an Error Bar chart of the Invisibility data:

cloakBar <- ggplot(CloakLong, aes(Group, Mischief))
cloakBar + stat_summary(fun.y = mean, geom = "bar", fill = "White", colour = "Black") + stat_summary(fun.data = mean_cl_normal, geom = "pointrange") + labs(x = "Cloak of Invisibility", y = "Mischief") + scale_y_continuous(limits = c(0, 6), breaks = seq(from = 0, to = 6, by = 1))
ggsave(file = paste(imageDirectory,"09 cloak bar.png",sep="/"))

#Self test- produce a boxplot with error bars showing confidence intervals for the cloak of invisibility data:

cloakBoxplot<-ggplot(CloakLong, aes(Group, Mischief))
cloakBoxplot + geom_boxplot() + labs(x = "Cloak", y = "Mischief") + scale_y_continuous(limits = c(0, 8), breaks = seq(from = 0, to = 8, by = 1))
ggsave(file = paste(imageDirectory,"09 cloak boxplot.png",sep="/"))

#describe the data
by(CloakLong$Michief, CloakLong$Group, stat.desc, basic = FALSE, norm = TRUE)

stat.desc(CloakWide$Cloak, basic = FALSE, norm = TRUE)
stat.desc(CloakWide$No Cloak, basic = FALSE, norm = TRUE)


#adjust the repeated measures data

rmMeanAdjust<-function(dataframe)
{
	varNames<-names(dataframe)
	
	pMean<-(dataframe[,1] + dataframe[,2])/2
	grandmean<-mean(c(dataframe[,1], dataframe[,2]))
	adj<-grandmean-pMean
	varA_adj<-dataframe[,1] + adj
	varB_adj<-dataframe[,2] + adj
	
	output<-data.frame(varA_adj, varB_adj)
	names(output)<-c(paste(varNames[1], "adj", sep = "_"), paste(varNames[2], "_adj", sep = "_"))
	return(output)
}


rmMeanAdjust(spiderWide)


spiderWide$pMean<-(spiderWide$picture + spiderWide$real)/2
grandMean<-mean(c(spiderWide$picture, spiderWide$real))
spiderWide$adj<-grandMean-spiderWide$pMean
head(spiderWide)

spiderWide$picture_adj<-spiderWide$picture + spiderWide$adj
spiderWide$real_adj<-spiderWide$real + spiderWide$adj

spiderWide$pMean2<-(spiderWide$picture_adj + spiderWide$real_adj)/2

#plot the adjusted means

spiderWide$id<-gl(12, 1, labels = c(paste("P", 1:12, sep = "_")))
adjustedData<-melt(spiderWide, id = c("id", "picture", "real", "pMean", "adj", "pMean2"), measured = c("picture_adj", "real_adj"))
adjustedData <-adjustedData[, -c(2:6)]
names(adjustedData)<-c("id", "Group", "Anxiety_Adj")
adjustedData$Group<-factor(adjustedData$Group, labels = c("Spider Picture", "Real Spider"))

bar <- ggplot(adjustedData, aes(Group, Anxiety_Adj))
bar + stat_summary(fun.y = mean, geom = "bar", fill = "White", colour = "Black") + stat_summary(fun.data = mean_cl_normal, geom = "pointrange") + labs(x = "Type of Stimulus", y = "Anxiety") + scale_y_continuous(limits = c(0, 60), breaks = seq(from = 0, to = 60, by = 10))
ggsave(file = paste(imageDirectory,"09 spider RM bar.png",sep="/"))



#Self-Test: t-test as a GLM

t.test.GLM<-lm(Anxiety ~ Group, data = spiderLong)
summary(t.test.GLM)


#computing t-test from means and SDs

x1 <- mean(spiderLong[spiderLong$Group=="Real Spider", ]$Anxiety)
x2 <- mean(spiderLong[spiderLong $Group=="Picture", ]$Anxiety)
sd1 <- sd(spiderLong[spiderLong $Group=="Real Spider", ]$Anxiety)
sd2 <- sd(spiderLong[spiderLong $Group=="Picture", ]$Anxiety)
n1 <- length(spiderLong[spiderLong $Group=="Real Spider", ]$Anxiety)
n2 <- length(spiderLong[spiderLong $Group=="Picture", ]$Anxiety)

ttestfromMeans<-function(x1, x2, sd1, sd2, n1, n2)
{	df<-n1 + n2 - 2
	poolvar <- (((n1-1)*sd1^2)+((n2-1)*sd2^2))/df
	t <- (x1-x2)/sqrt(poolvar*((1/n1)+(1/n2)))
	sig <- 2*(1-(pt(abs(t),df)))
	paste("t(df = ", df, ") = ", t, ", p = ", sig, sep = "")

}

ttestfromMeans(x1, x2, sd1, sd2, n1, n2)


# t-test

t.test(x, y = NULL, alternative = c("two.sided", "less", "greater"),
       mu = 0, paired = FALSE, var.equal = FALSE, conf.level = 0.95, na.action = na.exclude)



ind.t.test<-t.test(Anxiety ~ Group, data = spiderLong)
ind.t.test

ind.t.test<-t.test(spiderWide$real, spiderWide$picture)
ind.t.test


#Effect sizes

t<-ind.t.test$statistic[[1]]
df<-ind.t.test$parameter[[1]]
r <- sqrt(t^2/(t^2+df))
round(r, 3)

#Robust tests

#Independent groups
yuen(spiderWide$real, spiderWide$picture, tr=.2, alpha=.05)
yuenbt(spiderWide$real, spiderWide$picture, tr=.2, alpha=.05, nboot = 2000)
#CI
pb2gen(spiderWide$real, spiderWide$picture, alpha=.05, nboot=2000, est=mom)


#normality of differences

spiderWide$diff<-spiderWide$real-spiderWide$picture
stat.desc(spiderWide$diff, basic = FALSE, desc = FALSE, norm = TRUE)


#Dependent t test

stat.desc(spiderWide, basic = FALSE, norm = TRUE)

dep.t.test2<-t.test(Anxiety ~ Group, data = spiderLong, paired = TRUE)
dep.t.test2

dep.t.test<-t.test(spiderWide$real, spiderWide$picture, paired = TRUE)
dep.t.test

#Effect sizes:
t<-dep.t.test$statistic[[1]]
df<-dep.t.test$parameter[[1]]
r <- sqrt(t^2/(t^2+df))
round(r, 3)

#Robust Dependent t test:
yuend(spiderWide$real, spiderWide$picture, tr=.2, alpha=.05)
ydbt(spiderWide$real, spiderWide$picture, tr=.2, alpha=.05, nboot = 2000)
CI
bootdpci(spiderWide$real, spiderWide$picture, est=tmean, nboot=2000)

#----------------------Labcoat Leni------------------------------------

#load in the data:
madData<-read.delim("Board&Fritzon(2005).dat", header = TRUE)

#run the t-tests:
ttestfromMeans(madData$x1, madData$x2, madData$sd1, madData$sd2, madData$n1,madData$n2)

#----------------------Smart Alex Task 1------------------------------------

penisData<-read.delim("Penis.dat", header = TRUE)

penisData$book<-factor(penisData$book, levels = c(1:2), labels = c("Women are from Bras, Men are from Penis", "Marie Claire"))


penis.t.test<-t.test(happy ~ book, data = penisData)
penis.t.test


#Effect sizes

t<-penis.t.test$statistic[[1]]
df<-penis.t.test$parameter[[1]]
r <- sqrt(t^2/(t^2+df))
round(r, 3)

#----------------------Smart Alex Task 2------------------------------------

Field_Hole<-read.delim("Field&Hole.dat", header = TRUE)

fieldHole.t.test<-t.test(Field_Hole$women, Field_Hole$statbook, paired = TRUE)
fieldHole.t.test

t<-fieldHole.t.test$statistic[[1]]
df<-fieldHole.t.test$parameter[[1]]
r <- sqrt(t^2/(t^2+df))
round(r, 3)

#Robust Dependent t test:
yuend(Field_Hole$women, Field_Hole$statbook, tr=.2, alpha=.05)
ydbt(Field_Hole$women, Field_Hole$statbook, tr=.2, alpha=.05, nboot = 2000)
CI
bootdpci(Field_Hole$women, Field_Hole$statbook, est=tmean, nboot=2000)
