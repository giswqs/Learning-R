#---------------------------------------------------------------------------------------------------------
#R Code for Chapter 15 of:

#Field, A. P., Miles, J. N. V., & Field, Z. C. (2012). Discovering Statistics Using R: and Sex and Drugs and Rock 'N' Roll. #London Sage
#
#(c) 2011 Andy P. Field, Jeremy N. V. Miles & Zoe C. Field
#-----------------------------------------------------------------------------------------------------------




#----Set the working directory------
setwd("~/Dropbox/Team Field/DSUR/DSUR_2/DSUR2 Data Files/Chapter 15 (Nonparametric)")
setwd("~/Documents/Academic/Data/DSU_R/Chapter 15 (Nonparametric)")
imageDirectory<-"~/Documents/Academic/Books/Discovering Statistics/DSU R/DSU R I/DSUR I Images"

setwd("~/Public/Academic/Data/DSU_R/Chapter 15 (Nonparametric)")
imageDirectory<-"~/Public/Academic/Books/Discovering Statistics/DSU R/DSU R I/DSUR I Images"


#----Install Packages-----

install.packages("car")
install.packages("clinfun")
install.packages("ggplot2")install.packages("pastecs")
install.packages("pgirmess")



#------And then load these packages, along with the boot package.-----

library(car)
library(clinfun)
library(ggplot2)
library(pastecs)
library(pgirmess)

#********************* Wilcoxon Rank Sum ********************

#Enter raw data

sundayBDI<-c(15, 35, 16, 18, 19, 17, 27, 16, 13, 20, 16, 15, 20, 15, 16, 13, 14, 19, 18, 18)
wedsBDI<-c(28, 35, 35, 24, 39, 32, 27, 29, 36, 35, 5, 6, 30, 8, 9, 7, 6, 17, 3, 10)
drug<-gl(2, 10, labels = c("Ecstasy", "Alcohol"))
drugData<-data.frame(drug, sundayBDI, wedsBDI)


drugData<-read.delim("Drug.dat", header = TRUE)


#exploratory analysis

by(drugData[,c(2:3)], drugData$drug, stat.desc, basic=FALSE, norm=TRUE)
leveneTest(drugData$sundayBDI, drugData$drug, center = "mean")
leveneTest(drugData$wedsBDI, drugData$drug, center = "mean")

wilcox.test(x, y = NULL, alternative = c("two.sided", "less", "greater"),
       mu = 0, paired = FALSE, exact = FALSE, correct = FALSE, conf.level = 0.95, na.action = na.exclude)


sunModel<-wilcox.test(sundayBDI ~ drug, data = drugData)
sunModel
wedModel<-wilcox.test(wedsBDI ~ drug, data = drugData)
wedModel


sunModel<-wilcox.test(sundayBDI ~ drug, data = drugData, exact = FALSE, correct= FALSE)
sunModel
wedModel<-wilcox.test(wedsBDI ~ drug, data = drugData, exact = FALSE, correct= FALSE)
wedModel

#Jane superbrain:
g1 <- drugData$sundayBDI[drugData$drug == "Alcohol"]g2 <- drugData$sundayBDI[drugData$drug == "Ecstasy"]n1 <- length(g1); n2 <- length(g2)w <- rank(c(g1, g2)) r1 <- w[1:n1]; r2 <- w[(n1+1):(n1+n2)]w1 <- sum(r1); w2 <- sum(r2)wilc1 <- w1-n1*(n1+1)/2; wilc2 <- w2-n2*(n2+1)/2wilc = min(wilc1, wilc2)wilcm1 <- mean(r1); m2 <- mean(r2)m1; m2

rFromWilcox<-function(wilcoxModel, N){
	z<- qnorm(wilcoxModel$p.value/2)
	r<- z/ sqrt(N)
	cat(wilcoxModel$data.name, "Effect Size, r = ", r)
}

rFromWilcox(sunModel, 20)
rFromWilcox(wedModel, 20)


#********************* Wilcoxon Signed Rank ********************

drugData$BDIchange<-drugData$wedsBDI-drugData$sundayBDI
by(drugData$BDIchange, drugData$drug, stat.desc, basic = FALSE, norm = TRUE)

boxplot<-ggplot(drugData, aes(drug, BDIchange)) + geom_boxplot()
boxplot

alcoholData<-subset(drugData, drug == "Alcohol")
ecstasyData<-subset(drugData, drug == "Ecstasy")


alcoholModel<-wilcox.test(alcoholData$wedsBDI, alcoholData$sundayBDI,  paired = TRUE, correct= FALSE)
alcoholModel
ecstasyModel<-wilcox.test(ecstasyData$wedsBDI, ecstasyData$sundayBDI, paired = TRUE, correct= FALSE)
ecstasyModel

rFromWilcox(alcoholModel, 20)
rFromWilcox(ecstasyModel, 20)



#********************* Kruskal-Wallis ********************


Sperm<-c(0.35, 0.58, 0.88, 0.92, 1.22, 1.51, 1.52, 1.57, 2.43, 2.79, 3.40, 4.52, 4.72, 6.90, 7.58, 7.78, 9.62, 10.05, 10.32, 21.08, 0.33, 0.36, 0.63, 0.64, 0.77, 1.53, 1.62, 1.71, 1.94, 2.48, 2.71, 4.12, 5.65, 6.76, 7.08, 7.26, 7.92, 8.04, 12.10, 18.47, 0.40, 0.60, 0.96, 1.20, 1.31, 1.35, 1.68, 1.83, 2.10, 2.93, 2.96, 3.00, 3.09, 3.36, 4.34, 5.81, 5.94, 10.16, 10.98, 18.21, 0.31, 0.32, 0.56, 0.57, 0.71, 0.81, 0.87, 1.18, 1.25, 1.33, 1.34, 1.49, 1.50, 2.09, 2.70, 2.75, 2.83, 3.07, 3.28, 4.11)
Soya<-gl(4, 20, labels = c("No Soya", "1 Soya Meal", "4 Soya Meals", "7 Soya Meals"))
soyaData<-data.frame(Sperm, Soya)

soyaData<-read.delim("Soya.dat", header = TRUE)
soyaData$Soya<-factor(soyaData$Soya, levels = levels(soyaData$Soya)[c(4, 1, 2, 3)])

by(soyaData$Sperm, soyaData$Soya, stat.desc, basic=FALSE)
by(soyaData$Sperm, soyaData$Soya, stat.desc, desc = FALSE, basic=FALSE, norm=TRUE)

leveneTest(soyaData$Sperm, soyaData$Soya)

kruskal.test(Sperm ~ Soya, data = soyaData)
soyaData$Ranks<-rank(soyaData$Sperm)
by(soyaData$Ranks, soyaData$Soya, mean)

ggplot(soyaData, aes(Soya, Sperm)) + geom_boxplot() +
  labs(y = "Sperm Count", x = "Number of Soya Meals Per Week")
  
ggsave(file = paste(imageDirectory, "15 sperm boxplot.png", sep = "/"))

kruskalmc(Sperm ~ Soya, data = soyaData)
kruskalmc(Sperm ~ Soya, data = soyaData, cont = 'two-tailed')

jonckheere.test(soyaData$Sperm, as.numeric(soyaData$Soya))


#********************* Friedman's ANOVA ********************

dietData<-read.delim("Diet.dat", header = TRUE)

Start<-c(63.75, 62.98, 65.98, 107.27, 66.58, 120.46, 62.01, 71.87, 83.01, 76.62)
Month1<-c(65.38, 66.24, 67.70, 102.72, 69.45, 119.96, 66.09, 73.62, 75.81, 67.66)Month2<-c( 81.34, 69.31, 77.89, 91.33, 72.87, 114.26, 68.01, 55.43, 71.63, 68.60)
dietData<-data.frame(Start, Month1, Month2)


stat.desc(dietData, basic = FALSE, norm = TRUE)

library(reshape)

friedman.test(as.matrix(dietData))
friedmanmc(as.matrix(dietData))

#---------Labcoat Leni 1---------------------------

#Load in the data:
matthewsData<-read.delim("Matthews et al. (2007).dat", header = TRUE)

#Wilcoxon Signed Rank 
matthewsModel<-wilcox.test(matthewsData$Signaled, matthewsData$Control,  exact = FALSE, correct= FALSE)
matthewsModel


#---------Labcoat Leni 2---------------------------
quailData<-read.delim("Cetinkaya & Domjan (2006).dat", header = TRUE)
quailData$Groups<-factor(quailData$Groups, levels = levels(quailData$Groups)[c(1, 3, 2)])

#Box-plot for Eggs_Percent:

ggplot(quailData, aes(Groups, Egg_Percent)) + geom_boxplot() +
  labs(y = "Percentage of Eggs Fertilised", x = "Group")

ggplot(quailData, aes(Groups, Latency)) + geom_boxplot() +
  labs(y = "Time Taken to Initiate Copulation", x = "Group")
  
kruskal.test(Egg_Percent ~ Groups, data = quailData)
quailData$Ranks<-rank(quailData$Egg_Percent)
by(quailData$Ranks, quailData$Groups, mean)

kruskal.test(Latency ~ Groups, data = quailData)
quailData$Ranks<-rank(quailData$Latency)
by(quailData$Ranks, quailData$Groups, mean)

#Comparisons:
kruskalmc(Egg_Percent ~ Groups, data = quailData)
kruskalmc(Latency ~ Groups, data = quailData)



#---------Smart Alex Task 1---------------------------

MenDogs<-read.delim("MenLikeDogs.dat", header = TRUE)

#exploratory analysis
by(MenDogs[,2], MenDogs$species, stat.desc, basic=FALSE, norm=TRUE)

#Wilcoxon rank sum test
MenDogsModel<-wilcox.test(behaviou ~ species, data = MenDogs)
MenDogsModel


#Calculating the effect size (make sure you have executed the function first):
rFromWilcox(MenDogsModel, 40)


#---------Smart Alex Task 2---------------------------

darkLord<-read.delim("DarkLord.dat", header = TRUE)

#exploratory analysis
stat.desc(darkLord, basic=FALSE, norm=TRUE)

#Wilcoxon signed-rank test:
darkModel<-wilcox.test(darkLord$message, darkLord$nomessag,  paired = TRUE, correct= FALSE)
darkModel

#Effect size (make sure that you have executed the function from the book chapter first)
rFromWilcox(darkModel, 64)



#---------Smart Alex Task 3---------------------------

eastendersData<-read.delim("Eastenders.dat", header = TRUE)
stat.desc(eastendersData, basic = FALSE, norm = TRUE)

library(reshape)

friedman.test(as.matrix(eastendersData))
friedmanmc(as.matrix(eastendersData))


#---------Smart Alex Task 4---------------------------
clownData<-read.delim("coulrophobia.dat", header = TRUE)

#Relevel the variable infotype:
clownData$infotype<-factor(clownData$infotype, levels = levels(clownData$infotype)[c(3, 1, 2, 4)])

#Descriptives:
by(clownData$beliefs, clownData$infotype, stat.desc, basic=FALSE)


kruskal.test(beliefs ~ infotype, data = clownData)
clownData$Ranks<-rank(clownData$beliefs)
by(clownData$Ranks, clownData$infotype, mean)

#Compare each group against the control
kruskalmc(beliefs ~ infotype, data = clownData, cont = 'two-tailed')

jonckheere.test(clownData$beliefs, as.numeric(clownData$infotype))

#Relevel the variable infotype so that Adverts is level 1:
clownData$infotype<-factor(clownData$infotype, levels = levels(clownData$infotype)[c(2, 1, 3, 4)])

#Compare each group against Adverts:
kruskalmc(beliefs ~ infotype, data = clownData, cont = 'two-tailed')



