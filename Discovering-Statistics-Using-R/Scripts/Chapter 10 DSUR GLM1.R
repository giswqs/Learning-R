#---------------------------------------------------------------------------------------------------------
#R Code for Chapter 10 of:
#
#Field, A. P., Miles, J. N. V., & Field, Z. C. (2012). Discovering Statistics Using R: and Sex and Drugs and Rock 'N' Roll. #London Sage
#
#(c) 2011 Andy P. Field, Jeremy N. V. Miles & Zoe C. Field
#-----------------------------------------------------------------------------------------------------------

#Set the working directory (you will need to edit this to be the directory where you have stored the data files for this Chapter)

setwd("~/Dropbox/Team Field/DSUR/DSUR_2/DSUR2 Data Files/Chapter 10 (ANOVA)")
setwd("~/Documents/Academic/Data/DSU_R/Chapter 10 (ANOVA)")
imageDirectory<-"~/Documents/Academic/Books/Discovering Statistics/DSU R/DSU R I/DSUR I Images"

######Install packages

install.packages("granova")
install.packages("car")
install.packages("pastecs")
install.packages("multcomp")
install.packages("compute.es")
install.packages("Rcmdr", dependencies = TRUE)
install.packages("WRS", repos="http://R-Forge.R-project.org")

#Initiate packages
library(ggplot2)
library(granova)
library(car)
library(Rcmdr)
library(pastecs)
library(multcomp)
library(compute.es)
library(WRS)


#--------Self Test----------

dummyData<-read.delim("Dummy.dat", header = TRUE)
dummyData$dose<-factor(dummyData$dose, levels = c(1:3), labels = c("Placebo", "Low Dose", "High Dose"))

dummy.model<-lm(libido~dummy1 + dummy2, data = dummyData)
summary(dummy.model)


#--------Viagra data----------

id<-(1:15)
libido<-c(3,2,1,1,4,5,2,4,2,3,7,4,5,3,6)
#dose<-c(rep(1,5),rep(2,5), rep(3,5))
#dose<-factor(dose, levels = c(1:3), labels = c("Placebo", "Low Dose", "High Dose"))
dose<-gl(3,5, labels = c("Placebo", "Low Dose", "High Dose"))
viagraData<-data.frame(dose, libido)

#Graph
line <- ggplot(viagraData, aes(dose, libido))
line + stat_summary(fun.y = mean, geom = "line", size = 1, aes(group=1), colour = "#FF6633") + stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2, size = 0.75, colour = "#990000") + stat_summary(fun.y = mean, geom = "point", size = 4, colour = "#990000") + stat_summary(fun.y = mean, geom = "point", size = 3, colour = "#FF6633") + labs(x = "Dose of Viagra", y = "Mean Libido")
imageFile <- paste(imageDirectory,"10 Viagra Line.png",sep="/")
ggsave(file = imageFile)


#Descriptives
by(viagraData$libido, viagraData$dose, stat.desc)

#Levene's test
leveneTest(viagraData$libido, viagraData$dose, center = median)
#levene.test(viagraData$libido, viagraData$dose)


#ANOVA
viagraModel<-aov(libido~dose, data = viagraData)

#viagraModel<-lm(libido~dose, data = viagraData)

summary(viagraModel)
summary.lm(viagraModel)
plot(viagraModel)


# Welch Test
oneway.test(libido~dose, data = viagraData)


#--------Robust methods--------
# source("http://www-rcf.usc.edu/~rwilcox/Rallfun-v13")

viagraWide<-unstack(viagraData, libido~dose)
granova.1w(viagraWide)

#Trimmed mean:
t1way(viagraWide, tr = .1)
t1way(v.mat, MAT=TRUE, var.col = 2, lev.col = 1, tr = .1)

#compare medians:
med1way(viagraWide)

#bootstrap:
t1waybt(viagraWide, tr = .2, nboot = 599)

#M estimator bootstrap:
b1way(viagraWide, nboot = 599)


#--------Planned Contrasts--------
summary.lm(viagraModel)



contrast1<-c(-2,1,1)
contrast2<-c(0,-1,1)

contrasts(viagraData$dose)<-cbind(contrast1, contrast2)

contrasts(viagraData$dose)<-cbind(c(-2,1,1), c(0, -1, 1))
viagraPlanned<-aov(libido~dose, data = viagraData)
summary.lm(viagraPlanned)

contrasts(viagraData$dose)<-contr.poly(3)
viagraTrend<-aov(libido~dose, data = viagraData)
summary.lm(viagraTrend)

contrasts(viagraData$dose)<-contr.helmert(3)
viagraModel2<-aov(libido~dose, data = viagraData)
summary.lm(viagraModel2)


contrasts(viagraData$dose)<-contr.treatment(3, base = 2)
viagraModel3<-aov(libido~dose, data = viagraData, )
summary.lm(viagraModel3)

#--------Post Hoc Tests----------

pairwise.t.test(viagraData$libido, viagraData$dose, p.adjust.method = "bonferroni")
pairwise.t.test(viagraData$libido, viagraData$dose, p.adjust.method = "BH")


postHocs<-glht(viagraModel, linfct = mcp(dose = "Tukey"))
summary(postHocs)
confint(postHocs)

postHocs<-glht(viagraModel, linfct = mcp(dose = "Dunnett"), base = 1)
summary(postHocs)
confint(postHocs)

lincon(viagraWide, tr = .1)
mcppb20(viagraWide, tr = .2, nboot = 2000)

lincon(viagraWide)
mcppb20(viagraWide)

#--------Effect Sizes----------

mes(2.2, 3.2, 1.3038405, 1.3038405, 5, 5)
mes(2.2, 5, 1.3038405, 1.5811388, 5, 5)
mes(3.2, 5, 1.3038405, 1.5811388, 5, 5)



rcontrast<-function(t, df)
{r<-sqrt(t^2/(t^2 + df))
	print(paste("r = ", r))
	}
	
rcontrast(2.474, 12)
rcontrast(2.029, 12)



omega<-function(SSm, SSr, dfm, MSr)
{
	SSt = SSm + SSr
	omega = (SSm-(dfm*MSr))/(SSt+MSr)
	print(paste("Omega-Squared: ", omega))
	}
	
omega(20.133, 23.600, 2, 1.9667)
omega(450.66,38.09, 5, 0.334)
omega(4180.6, 4356.6, 3, 167.56)



#----Superhero data-----

heroData<-read.delim("Superhero.dat", header = TRUE)
heroData$hero<-factor(heroData$hero, levels = c(1:4), labels = c("Spiderman", "Superman", "The Hulk", "Ninja Turtle"))

pairwise.t.test(heroData$injury, heroData$hero, p.adjust.method = "none")



#--------Self Test----------

contrastData<-read.delim("Contrast.dat", header = TRUE)
contrastData$dose<-factor(contrastData$dose, levels = c(1:3), labels = c("Placebo", "Low Dose", "High Dose"))

contrast.model<-lm(libido~dummy1 + dummy2, data = contrastData)
summary(contrast.model)


#--------Oliver Twisted: Levene's Test--------


libido<-c(3,2,1,1,4,5,2,4,2,3,7,4,5,3,6)
dose<-c(rep(1,5),rep(2,5), rep(3,5))
dose<-factor(dose, levels = c(1:3), labels = c("Placebo", "Low Dose", "High Dose"))
viagraData<-data.frame(dose, libido)
by(viagraData$libido, viagraData$dose, summary)
viagraData$median<-c(rep(2,5),rep(3,5), rep(5,5))
viagraData$difference = viagraData$libido-viagraData$median
viagraData$abs.diff = abs(viagraData$difference)
levenes<-aov(abs.diff~dose, data = viagraData)
summary(levenes)


viagraData$mean<-c(rep(2.2,5),rep(3.2,5), rep(5,5))
viagraData$difference = viagraData$libido-viagraData$mean
viagraData$abs.diff = abs(viagraData$difference)
levenes<-aov(abs.diff~dose, data = viagraData)
summary(levenes)





#-------labcoat leni-----
#load in the Gallup et al.csv data:

GallupData = read.csv("Gallup et al.csv", header = TRUE)

#Error bar graph
#Create a bar object:
bar <- ggplot(GallupData, aes(Phallus, Displacement))

#Create the bar graph:
bar + stat_summary(fun.y = mean, geom = "bar", fill = "White", colour = "Black") + stat_summary(fun.data = mean_cl_normal, geom = "pointrange") + labs(x = "Phallus Type", y = "Mean Semen Displacement (%)")

#One way ANOVA:

#Conduct a Levene's test:
leveneTest(GallupData$Displacement, GallupData$Phallus, center = "median")

#Run the one-way ANOVA:

GallupModel<-aov(Displacement~Phallus, data = GallupData)
summary(GallupModel)

#Trend analysis:

contrasts(GallupData$Phallus)<-contr.poly(3)

GallupTrend<-aov(Displacement~Phallus, data = GallupData)

summary.lm(GallupTrend)

#Contrasts:

contrast1<-c(-2,1,1)
contrast2<-c(0,-1,1)

contrasts(GallupData$Phallus)<-cbind(contrast1, contrast2)
GallupData$Phallus

gallupPlanned<-aov(Displacement~Phallus, data = GallupData)

summary.lm(gallupPlanned)




#----------------Smart Alex Task 1---------------------

teachData = read.delim("Teach.dat", header = TRUE)

#Descriptives:

by(teachData$exam, teachData$group, stat.desc)

#Set group to be a factor:

teachData$group<-factor(teachData$group, levels = c(1:3), labels = c("Punish","Indifferent", "Reward"))

#Conduct a Levene's test:

leveneTest(teachData$exam, teachData$group, center = "median")

#Run the one-way ANOVA:

teachModel<-aov(exam~group, data = teachData)

summary(teachModel)

#Contrasts:

contrast1<-c(1,1,-2)
contrast2<-c(1,-1,0)

contrasts(teachData$group)<-cbind(contrast1, contrast2)
teachData$group

teachPlanned<-aov(exam~group, data = teachData)

summary.lm(teachPlanned)


#----------------Smart Alex Task 2---------------------

#Load in the data:

superData = read.delim("Superhero.dat", header = TRUE)

#Set hero to be a factor:

superData$hero<-factor(superData$hero, levels = c(1:4), labels = c("Spiderman","Superman", "Hulk", "Ninja Turtle"))

#Descriptives:

by(superData$injury, superData$hero, stat.desc)

#Create a bar object:
bar <- ggplot(superData, aes(hero, injury))

#Create the bar graph:
bar + stat_summary(fun.y = mean, geom = "bar", fill = "White", colour = "Black") + stat_summary(fun.data = mean_cl_normal, geom = "pointrange") + labs(x = "Superhero Costume", y = "Mean Severity of Injury")

#Conduct a Levene's test:

leveneTest(superData$injury, superData$hero, center = "median")

#Run the one-way ANOVA:

superModel<-aov(superData$injury~superData$hero, data = superData)

summary(superModel)

#Posthoc tests:

pairwise.t.test(superData$injury, superData$hero, p.adjust.method = "BH")





#----------------Smart Alex Task 3---------------------

soyaData = read.delim("Soya.dat", header = TRUE)

#Descriptives:

by(soyaData$Sperm, soyaData$Soya, stat.desc)

#Set soya to be a factor:

soyaData$Soya<-factor(soyaData$Soya, levels = c(1:4), labels = c("No Soya Meals","1 Soya Meal Per Week", "4 Soya Meals Per Week", "7 Soya Meals Per Week"))

#Conduct a Levene's test:

leveneTest(soyaData$Sperm, soyaData$Soya, center = "median")

#Run the one-way ANOVA:

soyaModel<-aov(soyaData$Sperm~soyaData$Soya, data = soyaData)

summary(soyaModel)

oneway.test(Sperm~Soya, data = soyaData)



#----------------Smart Alex Task 4---------------------

tumourData = read.delim("Tumour.dat", header = TRUE)

tumourData$usage<-factor(tumourData$usage, levels = c(0:5), labels = c("0 Hours", "1 Hour", "2 Hours", "3 Hours", "4 Hours", "5 Hours"))

#Error bar graph

#Create a bar object:
bar <- ggplot(tumourData, aes(usage, tumour))

#Create the bar graph:
bar + stat_summary(fun.y = mean, geom = "bar", fill = "White", colour = "Black") + stat_summary(fun.data = mean_cl_normal, geom = "pointrange") + labs(x = "Mobile Phone Use (Hours Per Day)", y = "Mean Size of Tumour (MM cubed)")

#Descriptives:

by(tumourData$tumour, tumourData$usage, stat.desc)

#Conduct a Levene's test:

leveneTest(tumourData$tumour, tumourData$usage, center = "median")

#Run the one-way ANOVA:

tumourModel<-aov(tumourData$tumour~tumourData$usage, data = tumourData)

summary(tumourModel)

oneway.test(tumour~usage, data = tumourData)

pairwise.t.test(tumourData$tumour, tumourData$usage, p.adjust.method = "BH")


#----------------Smart Alex Task 5---------------------

#Load the data into R:

festivalData<-read.delim("GlastonburyFestivalRegression.dat", header = TRUE)

#check levels of factor:

levels(festivalData$music)

#Conduct a Levene's test:

leveneTest(festivalData$change, festivalData$music, center = "median")

#Run the one-way ANOVA:

festivalModel<-aov(festivalData$change~festivalData$music, data = festivalData)

summary(festivalModel)

#Or you could just type:

festivalModel<-aov(change~music, data = festivalData)

summary(festivalModel)

#Contrasts:
contrasts(festivalData$music)<-contr.SAS(4)

festivalContrast<-aov(change~music, data = festivalData)

summary.lm(festivalContrast)

#Post Hoc tests:

pairwise.t.test(festivalData$change, festivalData$music, p.adjust.method = "BH")


#----------------Smart Alex Task 6---------------------

#Load the data into R:

quailData<-read.delim("Cetinkaya & Domjan.dat", header = TRUE)

#Set the categorical variable 'Groups' to be a factor:

quailData$Groups<-factor(quailData$Groups, levels = c(1:3), labels = c("Fetishistics", "NonFetishistics", "Control"))

#Error bar graphs:

#Create bar object and bar graph for bar graph 1:
bar <- ggplot(quailData, aes(Groups, Duration))

bar + stat_summary(fun.y = mean, geom = "bar", fill = "White", colour = "Black") + stat_summary(fun.data = mean_cl_normal, geom = "pointrange") + labs(x = "Group", y = "Mean Time Spent Near Terrycloth Object")

#Create bar object and bar graph for bar graph 2:
bar <- ggplot(quailData, aes(Groups, Efficiency))

bar + stat_summary(fun.y = mean, geom = "bar", fill = "White", colour = "Black") + stat_summary(fun.data = mean_cl_normal, geom = "pointrange") + labs(x = "Group", y = "Mean Copulatory Efficiancy")

#One-way ANOVA

#Conduct two Levene's tests:

leveneTest(quailData$Duration, quailData$Groups, center = "median")

leveneTest(quailData$Efficiency, quailData$Groups, center = "median")

#Run the one-way ANOVA:

quailModel.1<-aov(Duration~Groups, data = quailData)

quailModel.2<-aov(Efficiency~Groups, data = quailData)

summary(quailModel.1)

summary(quailModel.2)

#Post Hoc tests bonferroni:

pairwise.t.test(quailData$Duration, quailData$Groups, p.adjust.method = "bonferroni")

pairwise.t.test(quailData$Efficiency, quailData$Groups, p.adjust.method = "bonferroni")

#Post Hoc tests Tukey:

postHocs.1<-glht(quailModel.1, linfct = mcp(Groups = "Tukey"))

postHocs.2<-glht(quailModel.2, linfct = mcp(Groups = "Tukey"))

summary(postHocs.1)

confint(postHocs.1)

summary(postHocs.2)

confint(postHocs.2)






