#---------------------------------------------------------------------------------------------------------
#R Code for Chapter 12 of:
#
#Field, A. P., Miles, J. N. V., & Field, Z. C. (2012). Discovering Statistics Using R: and Sex and Drugs and Rock 'N' Roll. #London Sage
#
#(c) 2011 Andy P. Field, Jeremy N. V. Miles & Zoe C. Field
#-----------------------------------------------------------------------------------------------------------

#Set the working directory (you will need to edit this to be the directory where you have stored the data files for this Chapter)

setwd("~/Dropbox/Team Field/DSUR/DSUR_2/DSUR2 Data Files/Chapter 12 (Factorial ANOVA)")
setwd("~/Documents/Academic/Data/DSU_R/Chapter 12 (Factorial ANOVA)")
setwd("~/Public/Academic/Data/DSU_R/Chapter 12 (Factorial ANOVA)")
imageDirectory<-"~/Documents/Academic/Books/Discovering Statistics/DSU R/DSU R I/DSUR I Images"

#Install packages

install.packages("car")
install.packages("effects")
install.packages("compute.es")
install.packages("ggplot2")
install.packages("multcomp")
install.packages("pastecs")
install.packages("reshape")
install.packages("Hmisc")

#Installing the WRS package from R-Forge:
# Step 1: Install dependent packages:
install.packages(c("MASS", "akima", "robustbase"))
#Step 2: Install suggested packages:
install.packages(c("cobs", "robust", "mgcv", "scatterplot3d", "quantreg", "rrcov", "lars", "pwr", "trimcluster", "parallel", "mc2d", "psych", "Rfit"))
#Step 3: install WRS:
install.packages("WRS", repos="http://R-Forge.R-project.org", type="source")


#Initiate packages
library(car)
library(compute.es)
library(effects)
library(ggplot2)
library(multcomp)
library(pastecs)
library(reshape)
library(WRS)
library(Hmisc)
source("http://www-rcf.usc.edu/~rwilcox/Rallfun-v14")


#--------Self Test----------


gogglesReg<-read.delim("GogglesRegression.dat", header = TRUE)


gogglesRegModel<-lm(attractiveness ~ gender + alcohol + interaction, data = gogglesReg)
summary(gogglesRegModel)
summary.lm(gogglesRegModel)



#--------Beer Goggles data----------
gogglesData<-read.csv("goggles.csv", header = TRUE)
gogglesData$alcohol<-factor(gogglesData$alcohol, levels = c("None", "2 Pints", "4 Pints"))

id<-(1:48)
gender<-gl(2, 24, labels = c("Female", "Male"))
alcohol<-gl(3, 8, 48, labels = c("None", "2 Pints", "4 Pints"))
attractiveness<-c(65,70,60,60,60,55,60,55,70,65,60,70,65,60,60,50,55,65,70,55,55,60,50,50,50,55,80,65,70,75,75,65,45,60,85,65,70,70,80,60,30,30,30,55,35,20,45,40)

gogglesData<-data.frame(gender, alcohol, attractiveness)

#Self Test
by(gogglesData$attractiveness, gogglesData$gender, stat.desc)
by(gogglesData$attractiveness, gogglesData$alcohol, stat.desc)
by(gogglesData$attractiveness, list(gogglesData$alcohol, gogglesData$gender), stat.desc, basic = FALSE)


#Graphs
boxplot <- ggplot(gogglesData, aes(alcohol, attractiveness))
boxplot + geom_boxplot() + facet_wrap(~gender) + labs(x = "Alcohol Consumption", y = "Mean Attractiveness of Date (%)")
imageFile <- paste(imageDirectory,"12 Goggles Boxplot.png",sep="/")
ggsave(file = imageFile)

line <- ggplot(gogglesData, aes(alcohol, attractiveness, colour = gender))
line + stat_summary(fun.y = mean, geom = "point") + stat_summary(fun.y = mean, geom = "line", aes(group= gender)) + stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) + labs(x = "Alcohol Consumption", y = "Mean Attractiveness of Date (%)", colour = "Gender") 
imageFile <- paste(imageDirectory,"12 Goggles Interaction Line.png",sep="/")
ggsave(file = imageFile)

bar <- ggplot(gogglesData, aes(alcohol, attractiveness, fill = gender))
bar + stat_summary(fun.y = mean, geom = "bar", position="dodge") + stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position=position_dodge(width=0.90), width = 0.2) + labs(x = "Alcohol Consumption", y = "Mean Attractiveness of Date (%)", fill = "Gender") 
imageFile <- paste(imageDirectory,"12 Goggles Interaction Bar.png",sep="/")
ggsave(file = imageFile)


bar <- ggplot(gogglesData, aes(gender, attractiveness))
bar + stat_summary(fun.y = mean, geom = "bar", fill = "White", colour = "Black") + stat_summary(fun.data = mean_cl_normal, geom = "pointrange") + labs(x = "Gender", y = "Mean Attractiveness of Date (%)") + scale_y_continuous(breaks=seq(0,80, by = 10))
imageFile <- paste(imageDirectory,"12 Goggles Gender Main Effect Bar.png",sep="/")
ggsave(file = imageFile)

bar <- ggplot(gogglesData, aes(alcohol, attractiveness))
bar + stat_summary(fun.y = mean, geom = "bar", fill = "White", colour = "Black") + stat_summary(fun.data = mean_cl_normal, geom = "pointrange") + labs(x = "Alcohol Consumption", y = "Mean Attractiveness of Date (%)") + scale_y_continuous(breaks=seq(0,80, by = 10))
imageFile <- paste(imageDirectory,"12 Goggles Alcohol Main Effect Bar.png",sep="/")
ggsave(file = imageFile)

#Levene's Test
leveneTest(gogglesData$attractiveness, gogglesData$gender, center = median)
leveneTest(gogglesData$attractiveness, gogglesData$alcohol, center = median)
leveneTest(gogglesData$attractiveness, interaction(gogglesData$alcohol, gogglesData$gender), center = median)

levene.test(gogglesData$attractiveness, gogglesData$gender)
levene.test(gogglesData$attractiveness, gogglesData$alcohol)
levene.test(gogglesData$attractiveness, interaction(gogglesData$alcohol, gogglesData$gender))


#ANOVA

contrasts(gogglesData$alcohol)<-cbind(c(-2, 1, 1), c(0, -1, 1))
contrasts(gogglesData$gender)<-c(-1, 1)
gogglesModel<-aov(attractiveness ~ gender+alcohol+gender:alcohol, data = gogglesData)
Anova(gogglesModel, type="III")


genderEffect<-effect("gender", gogglesModel)
summary(genderEffect)
alcoholEffect<-effect("alcohol", gogglesModel)
summary(alcoholEffect)
interactionMeans<-allEffects(gogglesModel)
summary(interactionMeans)

summary.lm(gogglesModel)

pairwise.t.test(gogglesData$attractiveness, gogglesData$alcohol, p.adjust.method = "bonferroni")
pairwise.t.test(gogglesData$attractiveness, gogglesData$alcohol, p.adjust.method = "BH")

postHocs<-glht(gogglesModel, linfct = mcp(alcohol = "Tukey"))
summary(postHocs)
confint(postHocs)


plot(gogglesModel)

#simple effects
gogglesData$simple<-gl(6,8)
gogglesData$simple<-factor(gogglesData$simple, levels = c(1:6), labels = c("F_None","F_2pints", "F_4pints","M_None","M_2pints", "M_4pints"))

alcEffect1<-c(-2, 1, 1, -2, 1, 1)
alcEffect2<-c(0, -1, 1, 0, -1, 1)
gender_none<-c(-1, 0, 0, 1, 0, 0)
gender_twoPint<-c(0, -1, 0, 0, 1, 0)
gender_fourPint<-c(0, 0, -1, 0, 0, 1)
simpleEff<-cbind(alcEffect1, alcEffect2, gender_none, gender_twoPint, gender_fourPint)

contrasts(gogglesData$simple)<-simpleEff
simpleEffectModel<-aov(attractiveness ~ simple, data = gogglesData)
summary.lm(simpleEffectModel)

#--------Robust methods--------

gogglesData$row<-rep(1:8, 6)

#gogglesWide<-reshape(gogglesData, idvar = "row", timevar = "gender", direction = "wide")
gogglesMelt<-melt(gogglesData, id = c("row", "gender", "alcohol"), measured = c( "attractiveness"))
gogglesWide<-cast(gogglesMelt,  row ~ gender + alcohol)
gogglesWide$row<-NULL

t2way(2,3, gogglesWide)
mcp2atm(2,3, gogglesWide)

t2way(2,3, gogglesWide, tr = .1)
mcp2atm(2,3, gogglesWide, tr = .1)

pbad2way(2,3, gogglesWide)
mcp2a(2,3, gogglesWide)

pbad2way(2,3, gogglesWide, est = median)
mcp2a(2,3, gogglesWide, est = median)


#--------Effect Sizes----------

omega_factorial<-function(n,a,b, SSa, SSb, SSab, SSr)
{
	MSa<-SSa/(a-1)
	MSb<-SSb/(b-1)
	MSab<-SSab/((a-1)*(b-1))
	MSr<-SSr/(a*b*(n-1))
	varA<-((a-1)*(MSa-MSr))/(n*a*b)
	varB<-((b-1)*(MSb-MSr))/(n*a*b)
	varAB<-((a-1)*(b-1)*(MSab-MSr))/(n*a*b)
	varTotal<-varA + varB + varAB + MSr
	
	print(paste("Omega-Squared A: ", varA/varTotal))
	print(paste("Omega-Squared B: ", varB/varTotal))
	print(paste("Omega-Squared AB: ", varAB/varTotal))
	}
	
omega_factorial(8,2,3,169,3332,1978,3488)


#Gender effects (within alcohol)


mes(66.875, 60.625, 10.3293963, 4.95515604, 8, 8)
mes(66.875, 62.5, 12.5178444, 6.5465367, 8, 8)
mes(35.625, 57.5, 10.8356225, 7.0710678, 8, 8)


#-------Oliver Twised Contrasts-----

depressionData<-read.csv("Depression.csv", header = TRUE)

contrast1<-c(1, 1, -4, 1, 1)
contrast2<-c(1, 1, 0, -3, 1)
contrast3<-c(-2, 1, 0, 0, 1)
contrast4<-c(0, -1, 0, 0, 1)

contrasts(depressionData$treat)<-cbind(contrast1, contrast2, contrast3, contrast4)

depressionModel<-aov(diff~treat, data = depressionData)
summary(depressionModel)
summary.lm(depressionModel)


bar <- ggplot(depressionData, aes(treat, diff))
bar + stat_summary(fun.y = mean, geom = "bar", fill = "White", colour = "Black") + stat_summary(fun.data = mean_cl_normal, geom = "pointrange") + labs(x = "Treatment", y = "Change in Depression Score") + scale_y_continuous(breaks=seq(-10,4, by = 2), limits = c(-10, 4))
imageFile <- paste(imageDirectory,"12 Depression Treatment Effect.png",sep="/")
ggsave(file = imageFile)

#-------labcoat leni-----

#Read in the data:
daveyData<-read.delim("Davey2003.dat", header = TRUE)

#Set Mood and Stop_Rule to be factors:
daveyData$Mood<-factor(daveyData$Mood, levels = c(1:3), labels = c("Negative", "Positive", "Neutral"))

daveyData$Stop_Rule<-factor(daveyData$Stop_Rule, levels = c(1:2), labels = c("As many as you can", "Feel like continuing"))

#Error bar graph:
bar <- ggplot(daveyData, aes(Mood, Checks, fill = Stop_Rule))
bar + stat_summary(fun.y = mean, geom = "bar", position="dodge") + stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position=position_dodge(width=0.90), width = 0.2) + labs(x = "Mood Induction", y = "Mean Quantity of Items Checked", fill = "Stop Rule") 

#Descriptive statistics;

by(daveyData$Checks, list(daveyData$Stop_Rule, daveyData$Mood), stat.desc)

#Levene's Test:

leveneTest(daveyData$Checks, interaction(daveyData$Mood, daveyData$Stop_Rule), center = median)

#ANOVA with contrasts:
contrasts(daveyData$Mood)<-cbind(c(1, 1, -2), c(1, -1, 0))
contrasts(daveyData$Stop_Rule)<-c(-1, 1)
daveyModel<-aov(Checks ~ Stop_Rule+Mood + Stop_Rule:Mood, data = daveyData)
Anova(daveyModel, type="III")

#To view Contrasts:
summary.lm(daveyModel)

#Plots:
plot(daveyModel)


#-------Smart Alex Task 1-----

#Read in the data:
fugaziData<-read.delim("fugazi.dat", header = TRUE)

#Set music and age to be factors:
fugaziData$music<-factor(fugaziData$music, levels = c(1:3), labels = c("Fugazi", "Abba", "Barf Grooks"))

fugaziData$age<-factor(fugaziData$age, levels = c(1:2), labels = c("40+", "0-40"))

#Error bar graph:
bar <- ggplot(fugaziData, aes(music, liking, fill = age))
bar + stat_summary(fun.y = mean, geom = "bar", position="dodge") + stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position=position_dodge(width=0.90), width = 0.2) + labs(x = "Music", y = "Mean Liking Rating", fill = "Age Group") 

#Descriptive statistics;

by(fugaziData$liking, list(fugaziData$age, fugaziData$music), stat.desc)

#Levene's Test:

leveneTest(fugaziData$liking, interaction(fugaziData$music, fugaziData$age), center = median)

#ANOVA
contrasts(fugaziData$music)<-cbind(c(1, -2, 1), c(1, 0, -1))
contrasts(fugaziData$age)<-c(-1, 1)
fugaziModel<-aov(liking ~ music*age, data = fugaziData)
Anova(fugaziModel, type="III")

#To view contrasts:
summary.lm(fugaziModel)

#Plots:
plot(fugaziModel)

#Error bar graph music vs liking:
bar <- ggplot(fugaziData, aes(music, liking))
bar + stat_summary(fun.y = mean, geom = "bar", position="dodge") + stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position=position_dodge(width=0.90), width = 0.2) + labs(x = "Music", y = "Mean Liking Rating") 

#Error bar graph Age vs liking:
bar <- ggplot(fugaziData, aes(age, liking))
bar + stat_summary(fun.y = mean, geom = "bar", position="dodge") + stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position=position_dodge(width=0.90), width = 0.2) + labs(x = "Age", y = "Mean Liking Rating") 

#Post hocs 
pairwise.t.test(fugaziData$liking, fugaziData$music, p.adjust.method = "bonferroni")

#Effect Sizes	
omega_factorial(15,2,3,1,81864,310790,32553)

#-------Smart Alex Task 2-----
chickFlick<-read.delim("ChickFlick.dat", header = TRUE)

#Levene's tests:
leveneTest(chickFlick$arousal, interaction(chickFlick$film, chickFlick$gender), center = median)

#ANOVA
contrasts(chickFlick$film)<-c(-1, 1)
contrasts(chickFlick$gender)<-c(1, -1)
chickFlick<-aov(arousal ~ gender*film, data = chickFlick)
Anova(chickFlick, type="III")

#Error bar graph (gender vs mean arousal)
bar <- ggplot(chickFlick, aes(gender, arousal))
bar + stat_summary(fun.y = mean, geom = "bar", position="dodge") + stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position=position_dodge(width=0.90), width = 0.2) + labs(x = "Gender", y = "Mean Arousal") 

#Error bar graph (film vs mean arousal)
bar <- ggplot(chickFlick, aes(film, arousal))
bar + stat_summary(fun.y = mean, geom = "bar", position="dodge") + stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position=position_dodge(width=0.90), width = 0.2) + labs(x = "Film", y = "Mean Arousal") 

#Error bar graph- interaction 
bar <- ggplot(chickFlick, aes(gender, arousal, fill = film))
bar + stat_summary(fun.y = mean, geom = "bar", position="dodge") + stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position=position_dodge(width=0.90), width = 0.2) + labs(x = "Gender", y = "Mean Arousal", fill = "Film") 

#View contrasts:

summary.lm(chickFlick)

#Effect Sizes
	
omega_factorial(10,2,2,87,1092,34.2,1467.7)


#-------Smart Alex Task 3-----


#Read in the data:
escapeData<-read.delim("Escape From Inside.dat", header = TRUE)

#Set Song Type and Songwriter to be factors:
escapeData$Song_Type<-factor(escapeData$Song_Type, levels = c(0:1), labels = c("Symphony", "Fly Song"))

escapeData$Songwriter<-factor(escapeData$Songwriter, levels = c(0:1), labels = c("Malcom", "Andy"))

#Error line graph:
line <- ggplot(escapeData, aes(Song_Type, Screams, colour = Songwriter))
line + stat_summary(fun.y = mean, geom = "point") + stat_summary(fun.y = mean, geom = "line", aes(group= Songwriter)) + stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) + labs(x = "Type of Song", y = "Mean Number of Screams Elicited by the Song", colour = "Songwriter") 

#Levene's Test:

leveneTest(escapeData$Screams, interaction(escapeData$Song_Type, escapeData$Songwriter), center = median)

#ANOVA
contrasts(escapeData$Song_Type)<-c(-1, 1)
contrasts(escapeData$Songwriter)<-c(1, -1)
escapeModel<-aov(Screams ~ Song_Type*Songwriter, data = escapeData)
Anova(escapeModel, type="III")

#Error bar graph Type of Song vs Screams:
bar <- ggplot(escapeData, aes(Song_Type, Screams))
bar + stat_summary(fun.y = mean, geom = "bar", position="dodge") + stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position=position_dodge(width=0.90), width = 0.2) + labs(x = "Type of Song", y = "Mean Number of Screams Elicited by the Song") 

#Error bar graph Songwriter vs Screams:
bar <- ggplot(escapeData, aes(Songwriter, Screams))
bar + stat_summary(fun.y = mean, geom = "bar", position="dodge") + stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position=position_dodge(width=0.90), width = 0.2) + labs(x = "Songwriter", y = "Mean Number of Screams Elicited by the Song") 

#Effect Sizes
omega_factorial(17,2,2,74.1,35.3,18,227.3)

#-------Smart Alex Task 4-----

#Load in the data:
gogglesData<-read.csv("goggles.csv", header = TRUE)

#simple effects
gogglesData$simple<-gl(6,8)
gogglesData$simple<-factor(gogglesData$simple, levels = c(1:6), labels = c("Female_0","Female_2", "Female_4","Male_0","Male_2", "Male_4"))

genderEffect1<-c(1, 1, 1, -1, -1, -1)
MaleEffect1<-c(0, 0, 0, -2, 1, 1)
MaleEffect2<-c(0, 0, 0, 0, 1, -1)
FemaleEffect1<-c(-2, 1, 1, 0, 0, 0)
FemaleEffect2<-c(0, 1, -1, 0, 0, 0)
simpleEff<-cbind(genderEffect1, MaleEffect1, MaleEffect2, FemaleEffect1, FemaleEffect2)

contrasts(gogglesData$simple)<-simpleEff
simpleEffectModel<-aov(attractiveness ~ simple, data = gogglesData)

summary.lm(simpleEffectModel)



#-------Smart Alex Task 5-----

athleteData<-read.delim("wii.dat", header = TRUE)

#Graphs
athleteME <- ggplot(athleteData, aes(athlete, injury))
athleteME + stat_summary(fun.y = mean, geom = "bar", fill = "White", colour = "Black") + stat_summary(fun.data = mean_cl_boot, geom = "pointrange") + labs(x = "Athlete", y = "Mean Injury Score") + scale_y_continuous(breaks=seq(0,10, by = 1), limits = c(0, 10))
imageFile <- paste(imageDirectory,"12 Wii Athlete Main Effect.png",sep="/")
ggsave(file = imageFile)

stretchME <- ggplot(athleteData, aes(stretch, injury))
stretchME + stat_summary(fun.y = mean, geom = "bar", fill = "White", colour = "Black") + stat_summary(fun.data = mean_cl_boot, geom = "pointrange") + labs(x = "Stretching Routine", y = "Mean Injury Score") + scale_y_continuous(breaks=seq(0,10, by = 1), limits = c(0, 10))
imageFile <- paste(imageDirectory,"12 Wii Stretch Main Effect.png",sep="/")
ggsave(file = imageFile)

wiiME <- ggplot(athleteData, aes(wii, injury))
wiiME + stat_summary(fun.y = mean, geom = "bar", fill = "White", colour = "Black") + stat_summary(fun.data = mean_cl_boot, geom = "pointrange") + labs(x = "Wii Activity", y = "Mean Injury Score") + scale_y_continuous(breaks=seq(0,10, by = 1), limits = c(0, 10))
imageFile <- paste(imageDirectory,"12 Wii Main Effect.png",sep="/")
ggsave(file = imageFile)

athletestretchInt <- ggplot(athleteData, aes(stretch, injury, colour = athlete))
athletestretchInt + stat_summary(fun.y = mean, geom = "point") + stat_summary(fun.y = mean, geom = "line", aes(group= athlete)) + stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) + labs(x = "Stretching Routine", y = "Mean Injury Score", colour = "Athlete") + scale_y_continuous(breaks=seq(0,10, by = 1), limits = c(0, 10))
imageFile <- paste(imageDirectory,"12 Wii Stretch Athlete Interaction.png",sep="/")
ggsave(file = imageFile)

athleteWiiInt <- ggplot(athleteData, aes(wii, injury, colour = athlete))
athleteWiiInt + stat_summary(fun.y = mean, geom = "point") + stat_summary(fun.y = mean, geom = "line", aes(group= athlete)) + stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) + labs(x = "Wii Activity", y = "Mean Injury Score", colour = "Athlete") + scale_y_continuous(breaks=seq(0,10, by = 1), limits = c(0, 10))
imageFile <- paste(imageDirectory,"12 Wii Athlete Interaction.png",sep="/")
ggsave(file = imageFile)

stretcheWiiInt <- ggplot(athleteData, aes(wii, injury, colour = stretch))
stretcheWiiInt + stat_summary(fun.y = mean, geom = "point") + stat_summary(fun.y = mean, geom = "line", aes(group= stretch)) + stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) + labs(x = "Wii Activity", y = "Mean Injury Score", colour = "Stretching Routine") + scale_y_continuous(breaks=seq(0,10, by = 1), limits = c(0, 10))
imageFile <- paste(imageDirectory,"12 Wii Stretch Interaction.png",sep="/")
ggsave(file = imageFile)

threeWayInt <- ggplot(athleteData, aes(wii, injury, colour = stretch))
threeWayInt + stat_summary(fun.y = mean, geom = "point") + stat_summary(fun.y = mean, geom = "line", aes(group= stretch)) + stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) + labs(x = "Wii Activity", y = "Mean Injury Score", colour = "Stretching Routine") + facet_wrap(~athlete) + scale_y_continuous(breaks=seq(0,10, by = 1), limits = c(0, 10))
imageFile <- paste(imageDirectory,"12 Wii 3 way Interaction.png",sep="/")
ggsave(file = imageFile)

#Levene's test:

leveneTest(athleteData$injury, interaction(athleteData$athlete, athleteData$stretch, athleteData$wii), center = median)


#Remember you MUST set orthogonal contrasts if using Type III sums of squares
contrasts(athleteData$athlete)<-c(-1, 1)
contrasts(athleteData$stretch)<-c(-1, 1)
contrasts(athleteData$wii)<-c(-1, 1)
athleteModel<-aov(injury ~ athlete*stretch*wii, data = athleteData)
Anova(athleteModel, type="III")

summary.lm(athleteModel)

plot(athleteModel)


#Robust analysis

athleteData$row<-rep(1:15, 8)
athleteMelt<-melt(athleteData, id = c("row", "athlete", "stretch", "wii"), measured = c( "injury"))
athleteWide<-cast(athleteMelt,  row ~ athlete + stretch + wii)
athleteWide$row<-NULL

t3way(2,2,2, athleteWide)






