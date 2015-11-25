#---------------------------------------------------------------------------------------------------------
#R Code for Chapter 13 of:
#
#Field, A. P., Miles, J. N. V., & Field, Z. C. (2012). Discovering Statistics Using R: and Sex and Drugs and Rock 'N' Roll. #London Sage
#
#(c) 2011 Andy P. Field, Jeremy N. V. Miles & Zoe C. Field
#-----------------------------------------------------------------------------------------------------------

#Set the working directory (you will need to edit this to be the directory where you have stored the data files for this Chapter)

setwd("~/Documents/Academic/Data/DSU_R/Chapter 13 (Repeated Measures)")
setwd("~/Public/Academic/Data/DSU_R/Chapter 13 (Repeated Measures)")
imageDirectory<-"~/Documents/Academic/Books/Discovering Statistics/DSU R/DSU R I/DSUR I Images"

######Install packages

install.packages("compute.es")
install.packages("ez")
install.packages("ggplot2")
install.packages("multcomp")
install.packages("nlme")
install.packages("pastecs")
install.packages("reshape")
install.packages("WRS", repos="http://R-Forge.R-project.org")


#Initiate packages
library(compute.es)
library(ez)
library(ggplot2)
library(multcomp)
library(nlme)
library(pastecs)
library(reshape)
library(WRS)
source("http://www-rcf.usc.edu/~rwilcox/Rallfun-v14")


#--------Bushtucker Example ----------

bushData<-read.delim("Bushtucker.dat", header = TRUE)

#Enter data by hand

Participant<-gl(8, 4, labels = c("P1", "P2", "P3", "P4", "P5", "P6", "P7", "P8" ))
Animal<-gl(4, 1, 32, labels = c("Stick Insect", "Kangaroo Testicle", "Fish Eye", "Witchetty Grub"))
Retch<-c(8, 7, 1, 6, 9, 5, 2, 5, 6, 2, 3, 8, 5, 3, 1, 9, 8, 4, 5, 8, 7, 5, 6, 7, 10, 2, 7, 2, 12, 6, 8, 1)
longBush<-data.frame(Participant, Animal, Retch)



#using Anova()

dataOnly<-as.matrix(bushData)
means<-lm(dataOnly ~ 1)

animal<-factor(names(bushData))

bushModel<-Anova(means, idata = data.frame(animal), idesign = ~animal, type = "III")
summary(bushModel)

#Exploring Data

longBush <-melt(bushData, id = "participant", measured = c("stick_insect", "kangaroo_testicle", "fish_eye", "witchetty_grub"))
names(longBush)<-c("Participant", "Animal", "Retch")
longBush$Animal<-factor(longBush$Animal, labels = c("Stick Insect", "Kangaroo Testicle", "Fish Eye", "Witchetty Grub"))
longBush<-longBush[order(longBush$Participant),]

bushBoxplot <- ggplot(longBush, aes(Animal, Retch))
bushBoxplot + geom_boxplot() + labs(x = "Type of Animal Eaten", y = "Mean Time to Retch (Seconds)")
imageFile <- paste(imageDirectory,"13 Bushtucker Boxplot.png",sep="/")
ggsave(file = imageFile)

bushBar <- ggplot(longBush, aes(Animal, Retch))
bushBar + stat_summary(fun.y = mean, geom = "bar", fill = "White", colour = "Black") + stat_summary(fun.data = mean_cl_boot, geom = "pointrange") + labs(x = "Type of Animal Eaten", y = "Mean Time to Retch (Seconds)") 
imageFile <- paste(imageDirectory,"03 Bushtucker Bar.png",sep="/")
ggsave(file = imageFile)

#set Contrast

PartvsWhole<-c(1, -1, -1, 1)
TesticlevsEye<-c(0, -1, 1, 0)
StickvsGrub<-c(-1, 0, 0, 1)
contrasts(longBush$Animal)<-cbind(PartvsWhole, TesticlevsEye, StickvsGrub)

by(longBush$Retch, longBush$Animal, stat.desc, basic = FALSE)



#using ezAnova

bushModel<-ezANOVA(data = longBush, dv = .(Retch), wid = .(Participant),  within = .(Animal), type = 3, detailed = TRUE)
bushModel

noSpericity <-subset(longBush, Animal == "Stick Insect"| Animal == "Kangaroo Testicle",)
spherModel<-ezANOVA(data = noSpericity, dv = .(Retch), wid = .(Participant),  within = .(Animal), type = 3, detailed = TRUE)

pairwise.t.test(longBush$Retch, longBush$Animal, paired = TRUE, p.adjust.method = "bonferroni")

ezStats(data = longBush, dv = .(Retch), wid = .(Participant),  within = .(Animal), type = 3)


#Using lme

baseline<-lme(Retch ~ 1, random = ~1|Participant/Animal, data = longBush, method = "ML")
bushModel<-lme(Retch ~ Animal, random = ~1|Participant/Animal, data = longBush, method = "ML")
summary(baseline)
summary(bushModel)
anova(baseline, bushModel)


postHocs<-glht(bushModel, linfct = mcp(Animal = "Tukey"))
summary(postHocs)
confint(postHocs)

#--------Robust Methods

bushData2<-bushData[, -c(1)]

#Trimmed means
rmanova(bushData2)
rmmcp(bushData2)

#bootstrap
rmanovab(bushData2, nboot = 2000)
pairdepb(bushData2, nboot = 2000)



#-------Effect Sizes

rcontrast<-function(t, df)
{r<-sqrt(t^2/(t^2 + df))
	print(paste("r = ", r))
	}

rcontrast(3.149752, 21)
rcontrast(-0.101237, 21)
rcontrast(-1.923500, 21)

#Using aov (glht won't work)

bushModel<-aov(Retch ~ Animal + Error(Participant/Animal), data = longBush)
summary(bushModel)




#--------Attitude Example ----------

attitudeData<-read.delim("Attitude.dat", header = TRUE)

longAttitude <-melt(attitudeData, id = "participant", measured = c( "beerpos", "beerneg", "beerneut", "winepos", "wineneg", "wineneut", "waterpos", "waterneg", "waterneu", "participant"))
names(longAttitude)<-c("participant", "groups", "attitude")

longAttitude$drink<-gl(3, 60, labels = c("Beer", "Wine", "Water"))
longAttitude$imagery<-gl(3,20, 180, labels = c("Positive", "Negative", "Neutral"))

longAttitude<-longAttitude[order(longAttitude$participant),]

#Enter data by hand
participant<-gl(20, 9, labels = c("P01", "P02", "P03", "P04", "P05", "P06", "P07", "P08", "P09", "P10", "P11", "P12", "P13", "P14", "P15", "P16", "P17", "P18", "P19", "P20" ))
drink<-gl(3, 3, 180, labels = c("Beer", "Wine", "Water"))
imagery<-gl(3, 1, 180, labels = c("Positive", "Negative", "Neutral"))
groups<-gl(9, 1, 180, labels = c("beerpos", "beerneg", "beerneut", "winepos", "wineneg", "wineneut", "waterpos", "waterneg", "waterneut"))
attitude<-c(1, 6, 5, 38, -5, 4, 10, -14, -2, 26, 27, 27, 23, -15, 14, 21, -6, 0, 1, -19, -10, 28, -13, 13, 33, -2, 9, 7, -18, 6, 26, -16, 19, 23, -17, 5, 22, -8, 4, 34, -23, 14, 21, -19, 0, 30, -6, 3, 32, -22, 21, 17, -11, 4, 40, -6, 0, 24, -9, 19, 15, -10, 2, 15, -9, 4, 29, -18, 7, 13, -17, 8, 20, -17, 9, 30, -17, 12, 16, -4, 10, 9, -12, -5, 24, -15, 18, 17, -4, 8, 14, -11, 7, 34, -14, 20, 19, -1, 12, 43, 30, 8, 20, -12, 4, 9, -10, -13, 15, -6, 13, 23, -15, 15, 29, -1, 10, 15, 15, 12, 20, -15, 6, 6, -16, 1, 40, 30, 19, 28, -4, 0, 20, -10, 2, 8, 12, 8, 11, -2, 6, 27, 5, -5, 17, 17, 15, 17, -6, 6, 9, -6, -13, 30, 21, 21, 15, -2, 16, 19, -20, 3, 34, 23, 28, 27, -7, 7, 12, -12, 2, 34, 20, 26, 24, -10, 12, 12, -9, 4)

longAttitude<-data.frame(participant, drink, imagery, groups, attitude)


#Graphs
attitudeBoxplot <- ggplot(longAttitude, aes(drink, attitude))
attitudeBoxplot + geom_boxplot() + facet_wrap(~imagery, nrow = 1) + labs(x = "Type of Drink", y = "Mean Preference Score")
imageFile <- paste(imageDirectory,"13 attitude Boxplot.png",sep="/")
ggsave(file = imageFile)


drinkBar <- ggplot(longAttitude, aes(drink, attitude))
drinkBar + stat_summary(fun.y = mean, geom = "bar", fill = "White", colour = "Black") + stat_summary(fun.data = mean_cl_boot, geom = "pointrange") + labs(x = "Type of Drink", y = "Mean Attitude") 
imageFile <- paste(imageDirectory,"13 Drink Main Effect.png",sep="/")
ggsave(file = imageFile)

imageryBar <- ggplot(longAttitude, aes(imagery, attitude))
imageryBar + stat_summary(fun.y = mean, geom = "bar", fill = "White", colour = "Black") + stat_summary(fun.data = mean_cl_boot, geom = "pointrange") + labs(x = "Type of Imagery", y = "Mean Attitude") 
imageFile <- paste(imageDirectory,"13 Imagery Main Effect.png",sep="/")
ggsave(file = imageFile)

attitudeInt <- ggplot(longAttitude, aes(drink, attitude, colour = imagery))
attitudeInt + stat_summary(fun.y = mean, geom = "point") + stat_summary(fun.y = mean, geom = "line", aes(group= imagery)) + stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) + labs(x = "Type of Drink", y = "Mean Attitude", colour = "Type of Imagery") 
imageFile <- paste(imageDirectory,"13 Attitude Interaction Line.png",sep="/")
ggsave(file = imageFile)




options(digits = 3)
by(longAttitude$attitude, list(longAttitude$drink, longAttitude$imagery), stat.desc, basic = FALSE)
by(longAttitude$attitude, longAttitude$drink, stat.desc, basic = FALSE)
by(longAttitude$attitude, longAttitude$imagery, stat.desc, basic = FALSE)
options(digits = 7)


#Setting contrasts

AlcoholvsWater<-c(1, 1, -2)
BeervsWine<-c(-1, 1, 0)
NegativevsOther<-c(1, -2, 1)
PositivevsNeutral<-c(-1, 0, 1)


contrasts(longAttitude$drink)<-cbind(AlcoholvsWater, BeervsWine)
contrasts(longAttitude$imagery)<-cbind(NegativevsOther, PositivevsNeutral)


#using ezAnova

attitudeModel<-ezANOVA(data = longAttitude, dv = .(attitude), wid = .(participant),  within = .(drink, imagery), type = 3, detailed = TRUE)
options(digits = 3)
attitudeModel


pairwise.t.test(longAttitude$attitude, longAttitude$groups, paired = TRUE, p.adjust.method = "bonferroni")
options(digits = 7)


#Using lme


baseline<-lme(attitude ~ 1, random = ~1|participant/drink/imagery, data = longAttitude, method = "ML")
drinkModel<-update(baseline, .~. + drink)
imageryModel<-update(drinkModel, .~. + imagery)
attitudeModel<-update(imageryModel, .~. + drink:imagery)
anova(baseline, drinkModel, imageryModel, attitudeModel)


#attitudeModel<-lme(attitude ~ drink*imagery, random = ~1|participant/drink/imagery, data = longAttitude)
summary(attitudeModel)



postHocs<-glht(gogglesModel, linfct = mcp(Pints = "Tukey"))
summary(postHocs)
confint(postHocs)
postHocs<-glht(gogglesModel, linfct = mcp(Lighting = "Tukey"))
summary(postHocs)
confint(postHocs)

#Effect sizes


rcontrast(3.18, 38)
rcontrast(-1.47, 38)
rcontrast(17.26, 114)
rcontrast(-9.81, 114)
rcontrast(0.69, 114)
rcontrast(6.77, 114)
rcontrast(0.93, 114)
rcontrast(-0.80, 114)

#-------labcoat leni-----
fieldData<-read.delim("Field(2006).dat", header = TRUE)

#convert data into long format: 
longField <-melt(fieldData, id = "code", measured = c("bhvneg", "bhvpos", "bhvnone"))
names(longField)<-c("code", "Information", "Approach")
longField$Information<-factor(longField$Information, labels = c("Negative", "Positive", "None"))
longField<-longField[order(longField$code),]

#Error bar graph:
fieldBar <- ggplot(longField, aes(Information, Approach))
fieldBar + stat_summary(fun.y = mean, geom = "bar", fill = "White", colour = "Black") + stat_summary(fun.data = mean_cl_boot, geom = "pointrange") + labs(x = "Type of Information Given About the Animal", y = "Mean Time to Approach Animal (Seconds)") + coord_cartesian(ylim=c(0,10)) + scale_y_continuous(breaks = 1:10)


#Shapiro-Wilks test for Approach time split by type of information:
by(data=longField$Approach, INDICES=longField$Information, FUN=shapiro.test)

#qqplots for the three variables:
qplot(sample = fieldData$bhvneg, stat="qq")
qplot(sample = fieldData$bhvpos, stat="qq")
qplot(sample = fieldData$bhvnone, stat="qq")

#Log transform the variables:
fieldData$logneg <- log(fieldData$bhvneg+1)
fieldData$logpos <- log(fieldData$bhvpos+1)
fieldData$lognone <- log(fieldData$bhvnone+1)

#Shapiro-Wilks test for the log transformed scores 
shapiro.test(fieldData$logneg)
shapiro.test(fieldData$logpos)
shapiro.test(fieldData$lognone)




#Histograms for the log transformed data:
hist.logneg <- ggplot(fieldData, aes(logneg)) + 
             opts(legend.position = "none")+ 
              geom_histogram(aes(y=..density..), 
               colour="black", fill="white") + 
             labs(x="Log transformation of negative Info", y = "Density") + 
             stat_function(fun=dnorm, 
               args=list (mean=mean (fieldData$logneg, na.rm=TRUE), 
                sd=sd(fieldData$logneg, na.rm=TRUE) ), 
                colour="black", size=1)
hist.logneg

hist.logpos <- ggplot(fieldData, aes(logpos)) + 
             opts(legend.position = "none")+ 
              geom_histogram(aes(y=..density..), 
               colour="black", fill="white") + 
             labs(x="Log transformation of Positive Info", y = "Density") + 
             stat_function(fun=dnorm, 
               args=list (mean=mean (fieldData$logpos, na.rm=TRUE), 
                sd=sd(fieldData$logpos, na.rm=TRUE) ), 
                colour="black", size=1)
hist.logpos

hist.lognone <- ggplot(fieldData, aes(lognone)) + 
             opts(legend.position = "none")+ 
              geom_histogram(aes(y=..density..), 
               colour="black", fill="white") + 
             labs(x="Log transformation of No Info", y = "Density") + 
             stat_function(fun=dnorm, 
               args=list (mean=mean (fieldData$lognone, na.rm=TRUE), 
                sd=sd(fieldData$lognone, na.rm=TRUE) ), 
                colour="black", size=1)
hist.lognone

#Robust Anova:

#Get rid of the first column (code) as we only want the scores:
fieldData2<-fieldData[, -c(1)]

#Trimmed means
rmanova(fieldData2)
#Post hoc for Trimmed means
rmmcp(fieldData2)

#bootstrap
rmanovab(fieldData2, nboot = 2000)
#Post hoc for bootstrap
pairdepb(fieldData2, nboot = 2000)


#-------Smart Alex Task 2-----

tutorData<-read.delim("TutorMarks.dat", header = TRUE)

longTutor<-melt(tutorData, id = "Essay", measured = c("tutor1", "tutor2", "tutor3", "tutor4"))
names(longTutor)<-c("Essay", "Tutor", "Mark")
longTutor$Tutor<-factor(longTutor$Tutor, labels = c("Professor Field", "Professor Smith", "Professor Scrote", "Professor Death"))
longTutor<-longTutor[order(longTutor$Essay),]


#Error Bar Graph:
tutorBar <- ggplot(longTutor, aes(Tutor, Mark))
tutorBar + stat_summary(fun.y = mean, geom = "bar", fill = "White", colour = "Black") + stat_summary(fun.data = mean_cl_boot, geom = "pointrange") + labs(x = "Tutor", y = "Mean Percentage Mark") 

#Descriptives:

by(longTutor$Mark, longTutor$Tutor, stat.desc)


#set Contrast

NicevsNasty<-c(1, 1, 1, -3)
FieldvsScroteSmith<-c(2, -1, -1, 0)
ScrotevsSmith<-c(0, 1, -1, 0)
contrasts(longTutor$Tutor)<-cbind(NicevsNasty, FieldvsScroteSmith, ScrotevsSmith)

#ezAnova

tutorModel<-ezANOVA(data = longTutor, dv = .(Mark), wid = .(Essay),  within = .(Tutor), type = 3, detailed = TRUE)

tutorModel

#Post hoc tests:
pairwise.t.test(longTutor$Mark, longTutor$Tutor, paired = TRUE, p.adjust.method = "bonferroni")


# Means and standard deviation
ezStats(data = longTutor, dv = .(Mark), wid = .(Essay),  within = .(Tutor), type = 3)


#Using lme

baseline<-lme(Mark ~ 1, random = ~1|Essay/Tutor, data = longTutor, method = "ML")
tutorModel<-lme(Mark ~ Tutor, random = ~1|Essay/Tutor, data = longTutor, method = "ML")

tutorModel<-update(baseline, .~. + Tutor)

summary(tutorModel)
anova(baseline, tutorModel)


postHocs<-glht(tutorModel, linfct = mcp(Tutor = "Tukey"))
summary(postHocs)
confint(postHocs)

#--------Robust Methods

tutorData2<-tutorData[, -c(1)]


#Trimmed means
rmanova(tutorData2)
rmmcp(tutorData2)

#bootstrap
rmanovab(tutorData2, nboot = 2000)
pairdepb(tutorData2, nboot = 2000)



#-------Effect Sizes

rcontrast<-function(t, df)
{r<-sqrt(t^2/(t^2 + df))
	print(paste("r = ", r))
	}

rcontrast(3.34182, 21)
rcontrast(1.48533, 21)
rcontrast(-0.31184, 21)

#-------Smart Alex Task 3-----

rovingData<-read.delim("RovingEye.dat", header = TRUE)

longRoving<-melt(rovingData, id = "Participant", measured = c("pint1", "pint2", "pint3", "pint4"))
names(longRoving)<-c("Participant", "Pint", "Number_of_Women")
longRoving$Pint<-factor(longRoving$Pint, labels = c("Pint1", "Pint2", "Pint3", "Pint4"))
longRoving<-longRoving[order(longRoving$Participant),]


#Error Bar Graph:
rovingBar <- ggplot(longRoving, aes(Pint, Number_of_Women))
rovingBar + stat_summary(fun.y = mean, geom = "bar", fill = "White", colour = "Black") + stat_summary(fun.data = mean_cl_boot, geom = "pointrange") + labs(x = "Pint", y = "Mean Number of Women Eyed Up") 


#set Contrasts:

Pint1vsMore<-c(3, -1, -1, -1)
Pint4vsPint2andPint3<-c(0, -1, -1, 2)
Pint2vsPint3<-c(0, 1, -1, 0)
contrasts(longRoving$Pint)<-cbind(Pint1vsMore, Pint4vsPint2andPint3, Pint2vsPint3)

#ezAnova

rovingModel<-ezANOVA(data = longRoving, dv = .(Number_of_Women), wid = .(Participant),  within = .(Pint), type = 3, detailed = TRUE)

rovingModel

#Post hoc tests:
pairwise.t.test(longRoving$Number_of_Women, longRoving$Pint, paired = TRUE, p.adjust.method = "bonferroni")


# Means and standard deviation
ezStats(data = longRoving, dv = .(Number_of_Women), wid = .(Participant),  within = .(Pint), type = 3)


#Using lme

baseline<-lme(Number_of_Women ~ 1, random = ~1|Participant/Pint, data = longRoving, method = "ML")
rovingModel<-lme(Number_of_Women ~ Pint, random = ~1|Participant/Pint, data = longRoving, method = "ML")

rovingModel<-update(baseline, .~. + Pint)

anova(baseline, rovingModel)
summary(rovingModel)

postHocs<-glht(rovingModel, linfct = mcp(Pint = "Tukey"))
summary(postHocs)
confint(postHocs)

#-------Effect Sizes

rcontrast(-2.139071, 57)
rcontrast(1.375047, 57)
rcontrast(-2.778593, 57)


#-------Smart Alex Task 4-----

gogglesData<-read.delim("BeerGogglesLighting.dat", header = TRUE)

longGoggles<-melt(gogglesData, id = "Participant", measured = c( "dim0", "bright0", "dim2", "bright2", "dim4", "bright4", "dim6", "bright6"))
names(longGoggles)<-c("Participant", "Groups", "Attractiveness")

longGoggles$Lighting<-gl(2, 26, 208, labels = c("Dim", "Bright"))
longGoggles$Pints<-gl(4,52, 208, labels = c("0", "2", "4", "6"))

longGoggles<-longGoggles[order(longGoggles$Participant),]


#Boxplot:
gogglesBoxplot <- ggplot(longGoggles, aes(Pints, Attractiveness))
gogglesBoxplot + geom_boxplot() + facet_wrap(~Lighting, nrow = 1) + labs(x = "Type of Lighting", y = "Mean Attractiveness Score")


pintsBar <- ggplot(longGoggles, aes(Pints, Attractiveness))
pintsBar + stat_summary(fun.y = mean, geom = "bar", fill = "White", colour = "Black") + stat_summary(fun.data = mean_cl_boot, geom = "pointrange") + labs(x = "Number of Pints", y = "Mean Attractiveness Score") 


lightingBar <- ggplot(longGoggles, aes(Lighting, Attractiveness))
lightingBar + stat_summary(fun.y = mean, geom = "bar", fill = "White", colour = "Black") + stat_summary(fun.data = mean_cl_boot, geom = "pointrange") + labs(x = "Type of Lighting", y = "Mean Attractiveness Score") 


gogglesInt <- ggplot(longGoggles, aes(Pints, Attractiveness, colour = Lighting))
gogglesInt + stat_summary(fun.y = mean, geom = "point") + stat_summary(fun.y = mean, geom = "line", aes(group= Lighting)) + stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) + labs(x = "Number of Pints", y = "Mean Attractiveness Score", colour = "Type of Lighting") 


options(digits = 3)
by(longGoggles$Attractiveness, list(longGoggles$Pints, longGoggles$Lighting), stat.desc, basic = FALSE)
by(longGoggles$Attractiveness, longGoggles$Pints, stat.desc, basic = FALSE)
by(longGoggles$Attractiveness, longGoggles$Lighting, stat.desc, basic = FALSE)
options(digits = 7)


#Setting contrasts:

NonevsAlcohol<-c(4, -1, -1, -1)
MaxvsLess<-c(0, -1, -1, 2)
TwovsFour<-c(0, 1, -1,0)

DimvsLight<-c(1, -1)

contrasts(longGoggles$Pints)<-cbind(NonevsAlcohol, MaxvsLess, TwovsFour)



#using ezAnova

gogglesModel<-ezANOVA(data = longGoggles, dv = .(Attractiveness), wid = .(Participant),  within = .(Pints, Lighting), type = 3, detailed = TRUE)
options(digits = 3)
gogglesModel


pairwise.t.test(longGoggles$Attractiveness, longGoggles$Pints, paired = TRUE, p.adjust.method = "bonferroni")
options(digits = 7)


pairwise.t.test(longGoggles$Attractiveness, longGoggles$Groups, paired = TRUE, p.adjust.method = "bonferroni")
options(digits = 7)


#Using lme

baseline<-lme(Attractiveness ~ 1, random = ~1|Participant/Pints/Lighting, data = longGoggles, method = "ML")
PintsModel<-update(baseline, .~. + Pints)
LightingModel<-update(PintsModel, .~. + Lighting)
gogglesModel<-update(LightingModel, .~. + Pints:Lighting)
anova(baseline, PintsModel, LightingModel, gogglesModel)


#gogglesModel<-lme(Attractiveness ~ Pints*Lighting, random = ~1|Participant/Pints/Lighting, data = longGoggles)
summary(gogglesModel)



#Effect sizes

rcontrast( 10.21387, 75)
rcontrast(-12.22235, 75)
rcontrast(9.94787, 75)
rcontrast(5.27368, 100)
rcontrast(-4.31284, 100)
rcontrast(4.78794, 100)
rcontrast(-5.03915, 100)


