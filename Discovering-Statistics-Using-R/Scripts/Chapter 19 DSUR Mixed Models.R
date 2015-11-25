#---------------------------------------------------------------------------------------------------------
#R Code for Chapter 19 of:
#
#Field, A. P., Miles, J. N. V., & Field, Z. C. (2012). Discovering Statistics Using R: and Sex and Drugs and Rock 'N' Roll. #London Sage
#
#(c) 2011 Andy P. Field, Jeremy N. V. Miles & Zoe C. Field
#-----------------------------------------------------------------------------------------------------------


#Set the working directory
setwd("~/Dropbox/Team Field/DSUR/DSUR_2/DSUR2 Data Files/Chapter 19 (Mixed Models) R")

setwd("~/Documents/Academic/Data/DSU_R/Chapter 19 (Mixed Models) R")
imageDirectory<-paste(Sys.getenv("HOME"),"/Documents/Academic/Books/Discovering Statistics/DSU R/DSU R I/DSUR I Screenshots",sep="/")

install.packages("car")
install.packages("nlme")
install.packages("Rcmdr", dependencies = T)
install.packages("reshape")

## Install relevant libraries 
library(foreign)
library(car)
library(nlme)
library(ggplot2)
library(reshape)
library(Rcmdr)


##Load the data file into R. This is a tab-delimited file hence use of read.delim
surgeryData = read.delim("Cosmetic Surgery.dat",  header = TRUE)

#Graph the data
pgrid <- ggplot(surgeryData, aes(Base_QoL, Post_QoL)) + opts(title="Quality of Life Pre-Post Surgery at 10 Clinics")
pgrid + geom_point(aes(colour = Surgery_Text)) + geom_smooth(aes(colour = Surgery_Text), method = "lm", se = F) + facet_wrap(~Clinic, ncol = 5) + labs(x = "Quality of Life (Baseline)", y = "Quality of Life (After Surgery)")

imageDirectory<-paste(Sys.getenv("HOME"),"/Documents/Academic/Books/Discovering Statistics/DSU R/DSU R I/DSUR I Screenshots",sep="/")
imageFile <- paste(imageDirectory,"Surgery Data.png",sep="/")
ggsave(file = imageFile)


#Run an ANOVA
surgeryANOVA<-aov(Post_QoL~Surgery, data = surgeryData)
summary(surgeryANOVA)

# run the same model but using the linear models command
surgeryLinearModel<-lm(Post_QoL~Surgery, data = surgeryData)
summary(surgeryLinearModel)

#Run an ANCOVA
surgeryANCOVA<-aov(Post_QoL~Base_QoL + Surgery, data = surgeryData)
summary(surgeryANCOVA)
Anova(surgeryANCOVA, type = "III")

# run the same model but using the linear models command
surgeryLinearModel<-lm(Post_QoL~Surgery + Base_QoL, data = surgeryData)
summary(surgeryLinearModel)


##Fit baseline models
#Fit model with intercept only
interceptOnly <-gls(Post_QoL~1, data = surgeryData, method = "ML")
summary(interceptOnly)
#Fit model allowing intercepts to vary by clinic
randomInterceptOnly <-lme(Post_QoL~1, data = surgeryData, random = ~1|Clinic, method = "ML")
summary(randomInterceptOnly)

logLik(interceptOnly)*-2
logLik(randomInterceptOnly)*-2
anova(interceptOnly, randomInterceptOnly)

#Add surgery as a predictor
randomInterceptSurgery <-lme(Post_QoL~Surgery, data = surgeryData, random = ~1|Clinic, method = "ML")
summary(randomInterceptSurgery)

##Fit effect of surgery and baseline QoL- random intercepts across clinics
randomInterceptSurgeryQoL <-lme(Post_QoL~Surgery + Base_QoL, data = surgeryData, random = ~1|Clinic, method = "ML")
summary(randomInterceptSurgeryQoL)

anova(randomInterceptOnly, randomInterceptSurgery, randomInterceptSurgeryQoL)

##Fit effect of surgery and baseline QoL with interaction- random intercepts across clinics
#randomInterceptAll <-lme(Post_QoL~Surgery*Base_QoL, data = surgeryData, random = ~0|Clinic, method = "ML")
#summary(randomInterceptAll)
#anova(randomInterceptOnly, randomInterceptSurgery, randomIntercept2factors, randomInterceptAll)


##Fit effect of surgery and baseline QoL- random slopes across clinics
#trialSlopes<-lme(Post_QoL~Surgery + Base_QoL, data = surgeryData, random = ~0+Surgery|Clinic, method = "ML")
#summary(trialSlopes)

##Fit effect of surgery and baseline QoL- random slopes and intercepts across clinics
addRandomSlope<-lme(Post_QoL~Surgery + Base_QoL, data = surgeryData, random = ~Surgery|Clinic, method = "ML")
summary(addRandomSlope)
anova(randomInterceptSurgeryQoL,addRandomSlope)

plot(addRandomSlope)

##Fit effect of surgery and baseline QoL, Reason and Reason*Surgery Interaction- random slopes and intercepts across clinics

addReason<-lme(Post_QoL~Surgery + Base_QoL + Reason, data = surgeryData, random = ~Surgery|Clinic, method = "ML")
addReason<-update(addRandomSlope, .~. + Reason)
summary(addReason)

finalModel<-lme(Post_QoL~Surgery + Base_QoL + Reason + Reason:Surgery, data = surgeryData, random = ~Surgery|Clinic, method = "ML")
summary(finalModel)
intervals(finalModel, 0.95)

anova(addRandomSlope, addReason, finalModel)

##Fit effect of surgery and baseline QoL seperately for the two Reason groups.

physicalSubset<- surgeryData$Reason==1 
cosmeticSubset<-surgeryData$Reason==0
print(surgeryData$Surgery);print(physicalSubset);print(cosmeticSubset)

physicalModel<-lme(Post_QoL~Surgery + Base_QoL, data = surgeryData, random = ~Surgery|Clinic, subset= physicalSubset, method = "ML")
cosmeticModel<-lme(Post_QoL~Surgery + Base_QoL, data = surgeryData, random = ~Surgery|Clinic, subset= cosmeticSubset, method = "ML")
summary(physicalModel)
summary(cosmeticModel)


##--------------------------------------------------------------------
#GROWTH MODELS

satisfactionData = read.delim("Honeymoon Period.dat",  header = TRUE)

restructuredData<-melt(satisfactionData, id = c("Person", "Gender"), measured = c("Satisfaction_Base", "Satisfaction_6_Months", "Satisfaction_12_Months", "Satisfaction_18_Months"))
names(restructuredData)<-c("Person", "Gender", "Time", "Life_Satisfaction")


#print(restructuredData)
#restructuredData.sorted<-restructuredData[order(Person),]

intercept <-gls(Life_Satisfaction~1, data = restructuredData, method = "ML", na.action = na.exclude)
randomIntercept <-lme(Life_Satisfaction ~1, data = restructuredData, random = ~1|Person, method = "ML",  na.action = na.exclude, control = list(opt="optim"))
anova(intercept, randomIntercept)

timeRI<-update(randomIntercept, .~. + Time)
timeRS<-update(timeRI, random = ~Time|Person)
ARModel<-update(timeRS, correlation = corAR1(0, form = ~Time|Person))
anova(intercept, randomIntercept, timeRI, timeRS, ARModel)
summary(ARModel)
intervals(ARModel)
plot(ARModel)
plot(ARModel, Life_Satisfaction ~ Time)


timeQuadratic<-update(ARModel, .~. + I(Time^2))
timeCubic <-update(timeQuadratic, .~. + I(Time^3))
anova(ARModel, timeQuadratic, timeCubic)
summary(timeCubic)
intervals(timeCubic)

#Other techniques
polyModel<-update(ARModel, .~ poly(Time, 3))
summary(polyModel)
intervals(polyModel)

library(splines)
bSpline<-update(ARModel, .~ bs(Time))
summary(bSpline)

#the same models longhand

intercept <-gls(Life_Satisfaction~1, data = restructuredData, method = "ML", na.action = na.exclude)
summary(intercept)

randomIntercept <-lme(Life_Satisfaction ~1, data = restructuredData, random = ~1|Person, method = "ML",  na.action = na.exclude, control = list(opt="optim"))
summary(randomIntercept)


timeRI<-lme(Life_Satisfaction~Time, data = restructuredData, random = ~1|Person, method = "ML", na.action = na.exclude, control = list(opt="optim"))
summary(timeRI)

timeRS<-lme(Life_Satisfaction~Time, data = restructuredData, random = ~Time|Person, method = "ML", na.action = na.exclude, control = list(opt="optim"))
summary(timeRS)

ARModel<-lme(Life_Satisfaction~Time, data = restructuredData, random = ~Time|Person, correlation = corAR1(0, form = ~Time|Person), method = "ML", na.action = na.exclude, control = list(opt="optim"))
summary(ARModel)

anova(intercept, randomIntercept, timeRI, timeRS, ARModel)
summary(ARModel)
intervals(ARModel)

timeQuadratic<-lme(Life_Satisfaction~Time + I(Time^2), data = restructuredData, random = ~Time|Person, correlation = corAR1(0, form = ~Time|Person), method = "ML", na.action = na.exclude, control = list(opt="optim"))
summary(timeQuadratic)

timeCubic <-lme(Life_Satisfaction~Time + I(Time^2) + I(Time^3), data = restructuredData, random = ~Time|Person, correlation = corAR1(0, form = ~Time|Person), method = "ML", na.action = na.exclude, control = list(opt="optim"))
summary(timeCubic)

polyModel<-lme(Life_Satisfaction~poly(Time, 3), data = restructuredData, random = ~Time|Person, correlation = corAR1(0, form = ~Time|Person), method = "ML", na.action = na.exclude, control = list(opt="optim"))

anova(intercept, randomIntercept, timeRI, timeRS, ARModel, timeQuadratic, timeCubic)


##--------------------------------------------------------------------
#OLIVER TWISTED: CENTRING
#Grand Mean Centring

surgeryData = read.delim("Cosmetic Surgery.dat",  header = TRUE)
surgeryData$BDI_Centred<-scale(surgeryData$BDI, scale = F)

print(surgeryData)

#Group Mean Centring

groupMeans<-aggregate(surgeryData$BDI, list(surgeryData$Clinic), mean)
names(groupMeans)<-c("Clinic", "BDI_Mean")
print(groupMeans)

surgeryData<-merge(surgeryData, groupMeans, by = "Clinic")
surgeryData$BDI_GpMC<-surgeryData$BDI-surgeryData$BDI_Mean


#LABCOAT LENI

library(car)
dancerData = read.delim("Miller et al. (2007).dat",  header = TRUE)

Cyclephase_Factor <-factor(car::recode(dancerData$Cyclephase, "2=0;0=2"), levels = 0:2, labels = c("Fertile", "Menstural", "Luteal"))

Contraceptive_Factor <-factor(car::recode(dancerData$Contraceptive, "0=1;1=0"), levels = 0:1, labels = c("In Natural Cycle", "Using Pill"))
print(Contraceptive_Factor)

intercept <-gls(Tips~1, data = dancerData, method = "ML", na.action = na.exclude)
summary(intercept)

#Fit model allowing intercepts to vary by clinic
randomInt<-lme(Tips~1, random = ~1|ID, data = dancerData, method = "ML", na.action= na.exclude)
summary(randomInt)
anova(intercept, randomInt)

#Add cyclephase as a predictor
cycleModel<-update(randomInt, .~. + Cyclephase_Factor, method = "REML")

#Add contraceptive as a predictor
pillModel<-update(cycleModel, .~. + Contraceptive_Factor)

#Add interaction as a predictor
finalModel<-update(pillModel, .~. + Cyclephase_Factor:Contraceptive_Factor)
anova(finalModel)
summary(finalModel)
intervals(finalModel)

##--------------------------------------------------------------------
#SMART ALEX
#Task 1

surgeryData = read.delim("Cosmetic Surgery.dat",  header = TRUE)

intercept <-gls(Post_QoL~1, data = surgeryData, method = "ML")
randomIntercept <-lme(Post_QoL~1, data = surgeryData, random = ~1|Clinic, method = "ML")

randomInterceptSurgery<-update(randomIntercept, .~. + Surgery)
randomInterceptSurgeryQoL<-update(randomInterceptSurgery, .~. + Base_QoL)
addRandomSlope<-update(randomInterceptSurgeryQoL, random = ~Surgery|Clinic)
addReason<-update(addRandomSlope, .~. + Reason)
finalModel<-update(addReason, .~. + Reason:Surgery)

BDIModel<-update(finalModel, .~. + BDI)
AgeModel<-update(BDIModel, .~. + Age)
genderModel<-update(AgeModel, .~. + Gender)


anova(finalModel, genderModel); summary(genderModel); intervals(genderModel)


physicalSubset<-surgeryData$Reason==1 
cosmeticSubset<-surgeryData$Reason==0
physicalModel<-update(addRandomSlope, .~. + BDI + Age + Gender, subset= physicalSubset)
cosmeticModel<-update(addRandomSlope, .~. + BDI + Age + Gender, subset= cosmeticSubset)
summary(physicalModel)
summary(cosmeticModel)
--------
#Task 2

satisfactionData = read.delim("Honeymoon Period.dat",  header = TRUE)restructuredData<-reshape(satisfactionData, idvar = c("Person", "Gender"), varying = c("Satisfaction_Base", "Satisfaction_6_Months", "Satisfaction_12_Months", "Satisfaction_18_Months"), v.names = "Life_Satisfaction", timevar = "Time", times = c(0:3), direction = "long")

intercept <-gls(Life_Satisfaction~1, data = restructuredData, method = "ML", na.action = na.exclude)randomIntercept <-lme(Life_Satisfaction ~1, data = restructuredData, random = ~1|Person, method = "ML",  na.action = na.exclude, control = list(opt="optim"))timeRI<-update(randomIntercept, .~. + Time)timeRS<-update(timeRI, random = ~Time|Person)ARModel<-update(timeRS, correlation = corAR1(0, form = ~Time|Person))timeQuadratic<-update(ARModel, .~. + I(Time^2))timeCubic <-update(timeQuadratic, .~. + I(Time^3))genderModel <-update(timeCubic, .~. + Gender)anova(timeCubic, genderModel)summary(genderModel)intervals(genderModel)
-----------
#Task 3

exerciseData = read.delim("Hill et al. (2007).dat",  header = TRUE)

intercept<-gls(Post_Exercise~1, data = exerciseData, method = "ML", na.action = na.exclude)
randomInt <-lme(Post_Exercise~1, data = exerciseData, random = ~1|Classroom, method = "ML")
intervention<-update(randomInt, .~. + Intervention)
anova(intercept, randomInt, intervention)
anova(intervention)
summary(intervention)

#Task 4
finalIntervention<-update(intervention, .~. + Pre_Exercise)
anova(intervention, finalIntervention)
summary(finalIntervention)

