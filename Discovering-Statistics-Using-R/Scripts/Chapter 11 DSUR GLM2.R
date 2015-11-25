#---------------------------------------------------------------------------------------------------------
#R Code for Chapter 11 of:
#
#Field, A. P., Miles, J. N. V., & Field, Z. C. (2012). Discovering Statistics Using R: and Sex and Drugs and Rock 'N' Roll. #London Sage
#
#(c) 2011 Andy P. Field, Jeremy N. V. Miles & Zoe C. Field
#-----------------------------------------------------------------------------------------------------------

#Set the working directory (you will need to edit this to be the directory where you have stored the data files for this Chapter)

setwd("~/Dropbox/Team Field/DSUR/DSUR_2/DSUR2 Data Files/Chapter 11 (ANCOVA)")
setwd("~/Documents/Academic/Data/DSU_R/Chapter 11 (ANCOVA)")
imageDirectory<-"~/Documents/Academic/Books/Discovering Statistics/DSU R/DSU R I/DSUR I Images"

setwd("~/Public/Academic/Data/DSU_R/Chapter 11 (ANCOVA)")
imageDirectory<-"~/Public/Academic/Books/Discovering Statistics/DSU R/DSU R I/DSUR I Images"



######Install packages

install.packages("car")
install.packages("effects")
install.packages("compute.es")
install.packages("multcomp")
install.packages("pastecs")
install.packages("WRS", repos="http://R-Forge.R-project.org")
install.packages("reshape")


#Initiate packages
library(car)
library(compute.es)
library(effects)
library(ggplot2)
library(multcomp)
library(pastecs)
library(WRS)
library(reshape)

source("http://www-rcf.usc.edu/~rwilcox/Rallfun-v14")


viagraData<-read.delim("ViagraCovariate.dat", header = TRUE)


#--------Viagra Theory Section ----------

#Self Test
by(viagraData$libido, viagraData$dose, stat.desc, basic = F)
by(viagraData$partnerLibido, viagraData$dose, stat.desc, basic = F)

stat.desc(viagraData$libido, basic = F)
stat.desc(viagraData$partnerLibido, basic = F)

#Self Test
viagraData$dose<-factor(viagraData$dose, levels = c(1:3), labels = c("Placebo", "Low Dose", "High Dose"))

viagraModel.1<-lm(libido~partnerLibido, data = viagraData)
viagraModel.2<-update(viagraModel.1, .~. + dose)
anova(viagraModel.1, viagraModel.2)
summary(viagraModel.1)
summary(viagraModel.2)


#--------Viagra data----------

libido<-c(3,2,5,2,2,2,7,2,4,7,5,3,4,4,7,5,4,9,2,6,3,4,4,4,6,4,6,2,8,5)
partnerLibido<-c(4,1,5,1,2,2,7,4,5,5,3,1,2,2,6,4,2,1,3,5,4,3,3,2,0,1,3,0,1,0)
dose<-c(rep(1,9),rep(2,8), rep(3,13))
dose<-factor(dose, levels = c(1:3), labels = c("Placebo", "Low Dose", "High Dose"))
viagraData<-data.frame(dose, libido, partnerLibido)



#Graphs
restructuredData<-melt(viagraData, id = c("dose"), measured = c("libido", "partnerLibido")) 
names(restructuredData)<-c("dose", "libido_type", "libido")

boxplot <- ggplot(restructuredData, aes(dose, libido))
boxplot + geom_boxplot() + facet_wrap(~libido_type) + labs(x = "Dose", y = "Libido")
imageFile <- paste(imageDirectory,"11 Viagra Boxplot.png",sep="/")
ggsave(file = imageFile)

scatter <- ggplot(viagraData, aes(partnerLibido, libido, colour = dose))
scatter + geom_point(aes(shape = dose), size = 3) + geom_smooth(method = "lm", aes(fill = dose), alpha = 0.1) + labs(x = "Partner's Libido", y = "Participant's Libido")
imageFile <- paste(imageDirectory,"11 Viagra scatter.png",sep="/")
ggsave(file = imageFile)

scatter <- ggplot(viagraData, aes(partnerLibido, libido))
scatter + geom_point(size = 3) + geom_smooth(method = "lm", alpha = 0.1) + labs(x = "Partner's Libido", y = "Participant's Libido")
imageFile <- paste(imageDirectory,"11 Viagra covariate.png",sep="/")
ggsave(file = imageFile)


#Levene's Test
leveneTest(viagraData$libido, viagraData$dose, center = median)
levene.test(viagraData$libido, viagraData$dose)

#Test whether the IV and covariate are independent

checkIndependenceModel<-aov(partnerLibido ~ dose, data = viagraData)
summary(checkIndependenceModel)
summary.lm(checkIndependenceModel)


#ANCOVA

#contrasts(viagraData$dose)<-contr.helmert(3)
contrasts(viagraData$dose)<-cbind(c(-2,1,1), c(0,-1,1))
viagraModel<-aov(libido~ partnerLibido + dose, data = viagraData)
Anova(viagraModel, type="III")

adjustedMeans<-effect("dose", viagraModel, se=TRUE)
summary(adjustedMeans)
adjustedMeans$se

summary.lm(viagraModel)

postHocs<-glht(viagraModel, linfct = mcp(dose = "Tukey"))
summary(postHocs)
confint(postHocs)

plot(viagraModel)

#Homogeneity of regression slopes

#hoRS<-aov(libido ~ partnerLibido*dose, data = viagraData)
hoRS<-update(viagraModel, .~. + partnerLibido:dose)
Anova(hoRS, type="III")


#Self Test
anovaModel<-aov(libido ~ dose, data = viagraData)
summary(anovaModel)



#--------Robust methods--------


invisibilityData<-read.delim("CloakofInvisibility.dat", header = TRUE)
invisibilityData$cloak<-factor(invisibilityData$cloak, levels = c(1:2), labels = c("No Cloak", "Cloak"))

#Graphs

restructuredData<-melt(invisibilityData, id = c("cloak"), measured = c("mischief1", "mischief2")) 
names(restructuredData)<-c("cloak", "Time", "mischief")

boxplot <- ggplot(restructuredData, aes(cloak, mischief))
boxplot + geom_boxplot() + facet_wrap(~Time) + labs(x = "Cloak of Invisibility", y = "Number of Mischievous Acts")
imageFile <- paste(imageDirectory,"11 Invisibility Boxplot.png",sep="/")
ggsave(file = imageFile)

#Normal ANCOVA

#Levene's Test
leveneTest(invisibilityData$mischief2, invisibilityData$cloak, center = median)
levene.test(invisibilityData$mischief2, invisibilityData$cloak)

#Test whether the IV and covariate are independent

checkIndependenceModel<-aov(mischief1 ~ cloak, data = invisibilityData)
summary(checkIndependenceModel)
summary.lm(checkIndependenceModel)

#ANCOVA
invisibilityModel<-aov(mischief2~ mischief1 + cloak, data = invisibilityData)
Anova(invisibilityModel, type = "III")
summary.lm(invisibilityModel)
plot(invisibilityModel)

adjustedMeans<-effect("cloak", invisibilityModel, se = TRUE)
summary(adjustedMeans)
adjustedMeans$se

hoRS<-update(invisibilityModel, .~. + mischief1:cloak)
Anova(hoRS, type = "III")

#Robust Tests
noCloak<-subset(invisibilityData, cloak=="No Cloak",)
invisCloak<-subset(invisibilityData, cloak=="Cloak",)

covGrp2<-invisCloak$mischief1
dvGrp2<-invisCloak$mischief2
covGrp1<-noCloak$mischief1
dvGrp1<-noCloak$mischief2

ancova(covGrp2, dvGrp2, covGrp1, dvGrp1)
ancboot(covGrp2, dvGrp2, covGrp1, dvGrp1, nboot = 2000)


#--------Effect Sizes----------

t<-c(2.227, 2.785, 0.541)
df<-26
sqrt(t^2/(t^2 + df))


n<-c(9,8,13)
adjustedMeans$se*sqrt(n)


mes(5.988117, 4.151886, 1.755879, 1.788613, 8, 9)
mes(6.184427, 4.151886, 1.812267, 1.788613, 13, 9)
mes(6.184427, 5.988117, 1.812267, 1.755879, 13, 8)

#Effect size from post hoc t values

tes(2.102, 9, 8)
tes(2.771, 9, 13)
tes(0.541, 8, 13)

#Effect size from p-values but probably don't use because post hoc p values have been adjusted which will reduce the effect size.
pes(0.1090, 9, 8, tail = "two")
pes(0.0266, 9, 13, tail = "two")
pes(0.8516, 8, 13, tail = "two")




#--------Jane Supberbrain 1----------


covariateFirst<-aov(libido ~ partnerLibido + dose, data = viagraData)
summary(covariateFirst)
doseFirst<-aov(libido ~ dose + partnerLibido, data = viagraData)
summary(doseFirst)
Anova(covariateFirst, type="III")
Anova(doseFirst, type="III")


#--------Self Test----------

contrastData<-read.delim("Contrast.dat", header = TRUE)
contrastData$dose<-factor(contrastData$dose, levels = c(1:3), labels = c("Placebo", "Low Dose", "High Dose"))

contrast.model<-lm(libido~dummy1 + dummy2, data = contrastData)
summary(contrast.model)

#-------labcoat leni----------------

#Load the data:
murisData<-read.delim("Muris et al. (2008).dat", header = TRUE)

#Set Training to be a factor:
murisData$Training<-factor(murisData$Training, levels = c(1:2), labels = c("Negative Training", "Positive Training"))

#Set Gender to be a factor:
murisData$Gender<-factor(murisData$Gender, levels = c(1:2), labels = c("Boy", "Girl"))

#Levene's test:
leveneTest(murisData$Interpretational_Bias, interaction(murisData$Gender, murisData$Age, murisData$SCARED, murisData$Training), center = median)

#Look at the ANOVA before the covariates are included:
murisModel<-aov(Interpretational_Bias ~ Training, data = murisData)
summary(murisModel)

#Check that the covariate is independent from the experimental manipulation:
checkIndependenceModel.1<-aov(Age ~Training, data = murisData)
checkIndependenceModel.2<-aov(Gender ~Training, data = murisData)
checkIndependenceModel.3<-aov(SCARED ~Training, data = murisData)

summary(checkIndependenceModel.1)
summary.lm(checkIndependenceModel.1)

summary(checkIndependenceModel.2)
summary.lm(checkIndependenceModel.2)

summary(checkIndependenceModel.3)
summary.lm(checkIndependenceModel.3)

#Run the ANCOVA:
contrasts(murisData$Gender)<-c(-1,1)
contrasts(murisData$Training)<-c(-1,1)
murisModel<-aov(Interpretational_Bias~SCARED + Age + Gender + Training, data = murisData)
Anova(murisModel, type = "III")

#adjusted means:
adjustedMeans<-effect("Training", murisModel, se = TRUE)
summary(adjustedMeans)
adjustedMeans$se


#Regression parameter for the covariate:
summary.lm(murisModel)

#Plots:
plot(murisModel)

#Check for homogenity of regression slopes:
hoRS<-update(murisModel, .~. + SCARED:Training + Age:Training + Gender:Training)

Anova(hoRS, type = "III")

#--------Smart Alex Task 1----------

#Load in the data:
stalkerData<-read.delim("Stalker.dat", header = TRUE)

#Set group to be a factor:
stalkerData$group<-factor(stalkerData$group, levels = c(1:2), labels = c("Cruel to be Kind Therapy", "Psychodyshamic Therapy"))

#boxplots
restructuredData<-melt(stalkerData, id = c("group"), measured = c("stalk1", "stalk2")) 
names(restructuredData)<-c("group", "Time", "Stalk")

boxplot <- ggplot(restructuredData, aes(group, Stalk))
boxplot + geom_boxplot() + facet_wrap(~Time) + labs(x = "Type of Therapy", y = "Number of Hours Spent Stalking")


#Look at the ANOVA before the covariate is included:
stalkerModel.1<-aov(stalk2~ group, data = stalkerData)
summary(stalkerModel.1)

#Levene's test:
leveneTest(stalkerData$stalk2, interaction(stalkerData$group, stalkerData$stalk1), center = median)

#Check that the covariate is independent from the experimental manipulation:
checkIndependenceModel<-aov(stalk1~group, data = stalkerData)

summary(checkIndependenceModel)

summary.lm(checkIndependenceModel)

#Run the ANCOVA:

contrasts(stalkerData$group)<-c(-1, 1)
stalkerModel.1<-aov(stalk2~stalk1 + group, data = stalkerData)
Anova(stalkerModel.1, type = "III")

#adjusted means:
adjustedMeans<-effect("group", stalkerModel.1, se = TRUE)
summary(adjustedMeans)
adjustedMeans$se

#Regression parameter for the covariate:
summary.lm(stalkerModel.1)

#Plots:
plot(stalkerModel.1)

#Check for homogenity of regression slopes:
hoRS<-update(stalkerModel.1, .~. + stalk1:group)
Anova(hoRS, type = "III")

#Robust ANCOVA:
CruelGroup<-subset(stalkerData, group=="Cruel to be Kind Therapy",)
PsychoGroup<-subset(stalkerData, group=="Psychodyshamic Therapy",)

covGrp1<- CruelGroup$stalk1
dvGrp1<- CruelGroup$stalk2
covGrp2<-PsychoGroup$stalk1
dvGrp2<-PsychoGroup$stalk2

ancova(covGrp1, dvGrp1, covGrp2, dvGrp2)
ancboot(covGrp1, dvGrp1, covGrp2, dvGrp2, nboot = 2000)

#--------Smart Alex Task 2----------

#Load the data:
hangoverData<-read.delim("HangoverCure.dat", header = TRUE)

#Set drink to be a factor:
hangoverData$drink<-factor(hangoverData$drink, levels = c(1:3), labels = c("Water", "Lucozade", "Cola"))

#Conduct a one-way ANOVA without covariate
hangoverModel<-aov(well~drink, data = hangoverData)
Anova(hangoverModel, type = "III")
summary(hangoverModel)

#Normal ANCOVA
leveneTest(stalkerData$stalk2, interaction(stalkerData$group, stalkerData$stalk1), center = median)


#Levene's Test
leveneTest(hangoverData$well, interaction(hangoverData$drink, hangoverData$drunk), center = median)

#Test whether the IV and covariate are independent

checkIndependenceModel<-aov(drunk ~ drink, data = hangoverData)
summary(checkIndependenceModel)
summary.lm(checkIndependenceModel)

#ANCOVA+ Contrasts
contrasts(hangoverData$drink)<-cbind(c(-1,2,-1), c(1,0,-1))
hangoverModel<-aov(well~drunk + drink, data = hangoverData)
Anova(hangoverModel, type="III")
summary.lm(hangoverModel)

#Plots:
plot(hangoverModel)

adjustedMeans<-effect("drink", hangoverModel, se = TRUE)
summary(adjustedMeans)
adjustedMeans$se

hoRS<-update(hangoverModel, .~. + drunk:drink)
Anova(hoRS, type = "III")

#--------Smart Alex Task 3----------

#Load in the data:
elephantData<-read.delim("Elephant Football.dat", header = TRUE)

#Set the variable elephant to be a factor:
elephantData$elephant<-factor(elephantData$elephant, levels = c(1:2), labels = c("Asian Elephant", "African Elephant"))


#Graphs
boxplot <- ggplot(elephantData, aes(elephant, goals))
boxplot + geom_boxplot() + labs(x = "Type of Elephant", y = "Number of Goals")

boxplot <- ggplot(elephantData, aes(elephant, experience))
boxplot + geom_boxplot() + labs(x = "Type of Elephant", y = "Football Experience (Years)")

#Normal ANCOVA

#Levene's Test:

leveneTest(elephantData$goals, interaction(elephantData$elephant, elephantData$experience), center = median)

#Test whether the IV and covariate are independent

checkIndependenceModel<-aov(experience ~ elephant, data = elephantData)
summary(checkIndependenceModel)
summary.lm(checkIndependenceModel)

#ANCOVA
contrasts(elephantData$elephant)<-c(-1, 1)
elephantModel<-aov(goals~ experience + elephant, data = elephantData)
Anova(elephantModel, type = "III")
summary.lm(elephantModel)
plot(elephantModel)

adjustedMeans<-effect("elephant", elephantModel, se = TRUE)
summary(adjustedMeans)
adjustedMeans$se

hoRS<-update(elephantModel, .~. + experience:elephant)
Anova(hoRS, type = "III")

#Robust Tests
asian<-subset(elephantData, elephant=="Asian Elephant",)
african<-subset(elephantData, elephant=="African Elephant",)

covGrp1<-asian$experience
dvGrp1<-asian$goals
covGrp2<-african$experience
dvGrp2<-african$goals

ancova(covGrp1, dvGrp1, covGrp2, dvGrp2)
ancboot(covGrp1, dvGrp1, covGrp2, dvGrp2, nboot = 2000)










