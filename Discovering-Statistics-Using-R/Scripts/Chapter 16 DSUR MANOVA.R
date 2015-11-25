#---------------------------------------------------------------------------------------------------------
#R Code for Chapter 16 of:
#
#Field, A. P., Miles, J. N. V., & Field, Z. C. (2012). Discovering Statistics Using R: and Sex and Drugs and Rock 'N' Roll. #London Sage
#
#(c) 2011 Andy P. Field, Jeremy N. V. Miles & Zoe C. Field
#-----------------------------------------------------------------------------------------------------------

#Set the working directory (you will need to edit this to be the directory where you have stored the data files for this Chapter)

setwd("~/Dropbox/Team Field/DSUR/DSUR_2/DSUR2 Data Files/Chapter 16 (MANOVA)")
setwd("~/Documents/Academic/Data/DSU_R/Chapter 16 (MANOVA)")
imageDirectory<-"~/Documents/Academic/Books/Discovering Statistics/DSU R/DSU R I/DSUR I Images"

setwd("~/Public/Academic/Data/DSU_R/Chapter 16 (MANOVA)")
imageDirectory<-"~/Public/Academic/Books/Discovering Statistics/DSU R/DSU R I/DSUR I Images"

######Install packages

install.packages("car")
install.packages("ggplot2")
#install.packages("klaR")
install.packages("mvoutlier")
install.packages("mvnormtest")
install.packages("pastecs")
install.packages("reshape")
install.packages("WRS", repos="http://R-Forge.R-project.org")


#Initiate packages
library(car)
library(ggplot2)
#library(klaR)
library(mvnormtest)
library(mvoutlier)
library(pastecs)
library(reshape)
library(WRS)
source("http://www-rcf.usc.edu/~rwilcox/Rallfun-v14")








#--------OCD Example ----------


ocdData<-read.delim("OCD.dat", header = TRUE)
ocdData$Group<-factor(ocdData$Group, levels = c("CBT", "BT", "No Treatment Control"), labels = c("CBT", "BT", "NT"))


# Enter the data by hand

Group<-gl(3, 10, labels = c("CBT", "BT", "NT"))
Actions<-c(5, 5, 4, 4, 5, 3, 7, 6, 6, 4, 4, 4, 1, 1, 4, 6, 5, 5, 2, 5, 4, 5, 5, 4, 6, 4, 7, 4, 6, 5)
Thoughts<-c(14, 11, 16, 13, 12, 14, 12, 15, 16, 11, 14, 15, 13, 14, 15, 19, 13, 18, 14, 17, 13, 15, 14, 14, 13, 20, 13, 16, 14, 18)
ocdData<-data.frame(Group, Actions, Thoughts)


#---Explore the data
# Graphs

ocdScatter <- ggplot(ocdData, aes(Actions, Thoughts))
ocdScatter + geom_point() + geom_smooth(method = "lm")+ labs(x = "Number of Obsession-Related Behaviours", y = "Number of Obsession-Related Thoughts") + facet_wrap(~Group, ncol = 3)
imageFile <- paste(imageDirectory,"16 OCD Scatter.png",sep="/")
ggsave(file = imageFile)


ocdMelt<-melt(ocdData, id = c("Group"), measured = c("Actions", "Thoughts"))
names(ocdMelt)<-c("Group", "Outcome_Measure", "Frequency")


ocdBar <- ggplot(ocdMelt, aes(Group, Frequency, fill = Outcome_Measure))
ocdBar + stat_summary(fun.y = mean, geom = "bar", position = "dodge") + stat_summary(fun.data = mean_cl_boot, geom = "errorbar", position=position_dodge(width=0.90), width = 0.2) + labs(x = "Treatment Group", y = "Number of Thoughts/Actions", fill = "Outcome Measure") + scale_y_continuous(breaks = seq(0, 20, by = 2))
imageFile <- paste(imageDirectory,"16 OCD Bar.png",sep="/")
ggsave(file = imageFile)


ocdBoxplot <- ggplot(ocdMelt, aes(Group, Frequency, colour = Outcome_Measure))
ocdBoxplot + geom_boxplot() + labs(x = "Treatment Group", y = "Number of Thoughts/Actions", colour = "Outcome Measure") + scale_y_continuous(breaks = seq(0, 20, by = 2))
imageFile <- paste(imageDirectory,"16 OCD Boxplot.png",sep="/")
ggsave(file = imageFile)


options(digits = 3)
by(ocdData$Actions, ocdData$Group, stat.desc, basic = FALSE)
by(ocdData$Thoughts, ocdData$Group, stat.desc, basic = FALSE)
options(digits = 7)


by(ocdData[, 2:3], ocdData$Group, cov)


# selects the rows and columns of the data frame for each group, the t() rtransposes the data (so variables are in rows and cases in columns, which is the format that the shapiro test function requires)

cbt<-t(ocdData[1:10, 2:3])
bt<-t(ocdData[11:20, 2:3])
nt<-t(ocdData[21:30, 2:3])

mshapiro.test(cbt)
mshapiro.test(bt)
mshapiro.test(nt)

aq.plot(ocdData[, 2:3])

#self help

ocdNoOutlier<-ocdData[-26,]

nt<-t(ocdNoOutlier[21:29, 2:3])
mshapiro.test(nt)


#---Set contrasts

CBT_vs_NT<-c(1, 0, 0)
BT_vs_NT <-c(0, 1, 0)

contrasts(ocdData$Group)<-cbind(CBT_vs_NT, BT_vs_NT)

#contrasts(ocdData$Group)<-contr.treatment(3, base = 3)

#---Main analysis

outcome<-cbind(ocdData$Actions, ocdData$Thoughts)
ocdModel<-manova(outcome ~ Group, data = ocdData)
summary(ocdModel, intercept = TRUE)

#Anova(ocdModel, type = "III")

summary(ocdModel, intercept = TRUE, test = "Wilks")
summary(ocdModel, intercept = TRUE, test = "Hotelling")
summary(ocdModel, intercept = TRUE, test = "Roy")

#-univariate analysis
summary.aov(ocdModel)

actionModel<-lm(Actions ~ Group, data = ocdData)
thoughtsModel<-lm(Thoughts ~ Group, data = ocdData)
summary.lm(actionModel)
summary.lm(thoughtsModel)


#--Robust MANOVA

ocdData$row<-rep(1:10, 3)

ocdMelt<-melt(ocdData, id = c("Group", "row"), measured = c("Actions", "Thoughts"))
names(ocdMelt)<-c("Group", "row", "Outcome_Measure", "Frequency")
ocdRobust<-cast(ocdMelt, row ~ Group + Outcome_Measure, value = "Frequency")
ocdRobust$row<-NULL

mulrank(3, 2, ocdRobust)
cmanova(3, 2, ocdRobust)


#---DFA

ocdDFA<-lda(Group ~ Actions + Thoughts, data = ocdData, na.action="na.omit")
summary(ocdDFA)
plot(ocdDFA)
predict(ocdDFA)








#--------Self Test----------



#-------labcoat leni-----

#Read in the data:
marazillierData<-read.delim("Marzillier & Davey (2005).dat", header = TRUE)

#Make sure the levels of the factors are in the correct order:
marazillierData$Induction<-factor(marazillierData$Induction, levels = c("Vignettes + Music", "Videos", "Memory Recall + Music"))
marazillierData$Mood<-factor(marazillierData$Mood, levels = c("Anxious", "Disgust", "Neutral"))

#Melt the data:
moodMelt<-melt(marazillierData, id = c("Induction", "Mood"), measured = c("Anxiety.Change", "Sad.Change", "Happy.Change", "Angry.Change", "Disgust.Change", "Contempt.Change"))
names(moodMelt)<-c("Induction", "Mood", "Outcome_Measure", "Frequency")

#Error bar graph:
moodBar <- ggplot(moodMelt, aes(Mood, Frequency, fill = Outcome_Measure))
moodBar + stat_summary(fun.y = mean, geom = "bar", position = "dodge") + stat_summary(fun.data = mean_cl_boot, geom = "errorbar", position=position_dodge(width=0.90), width = 0.2) + labs(x = "Mood Induction Type", y = "Mean Change", fill = "Outcome Measure")+ facet_wrap(~Induction, ncol = 3) + scale_y_continuous(breaks = seq(-40, 70, by = 10))

#descriptives:
by(marazillierData$Anxiety.Change, list(marazillierData$Mood, marazillierData$Induction), stat.desc, basic = FALSE)
by(marazillierData$Sad.Change, list(marazillierData$Mood, marazillierData$Induction), stat.desc, basic = FALSE)
by(marazillierData$Happy.Change, list(marazillierData$Mood, marazillierData$Induction), stat.desc, basic = FALSE)
by(marazillierData$Angry.Change, list(marazillierData$Mood, marazillierData$Induction), stat.desc, basic = FALSE)
by(marazillierData$Disgust.Change, list(marazillierData$Mood, marazillierData$Induction), stat.desc, basic = FALSE)
by(marazillierData$Contempt.Change, list(marazillierData$Mood, marazillierData$Induction), stat.desc, basic = FALSE)
options(digits = 7)


#Set contrasts:
Mood_vs_None<-c(1, 1, -2)
anx_vs_disg <-c(1, -1, 0)
contrasts(marazillierData$Mood)<-cbind(Mood_vs_None, anx_vs_disg)

Music_vs_None<-c(1, -2, 1)
vig_vs_mem <-c(1, 0, -1)
contrasts(marazillierData$Induction)<-cbind(Music_vs_None, vig_vs_mem)


#contrasts(ocdData$Group)<-contr.treatment(3, base = 3)

#---Main analysis

outcome<-cbind(marazillierData$Anxiety.Change, marazillierData$Sad.Change, marazillierData$Happy.Change, marazillierData$Angry.Change, marazillierData$Disgust.Change, marazillierData$Contempt.Change)
marzillierModel<-manova(outcome ~ Induction*Mood, data = marazillierData)
summary(marzillierModel, intercept = TRUE)

#-univariate analysis
summary.aov(marzillierModel)

#To get the contrasts for Mood:

anxiousModel<-lm(Anxiety.Change ~ Mood, data = marazillierData)
sadModel<-lm(Sad.Change ~ Mood, data = marazillierData)
happyModel<-lm(Happy.Change ~ Mood, data = marazillierData)
angryModel<-lm(Angry.Change ~ Mood, data = marazillierData)
disgustModel<-lm(Disgust.Change ~ Mood, data = marazillierData)
contemptModel<-lm(Contempt.Change ~ Mood, data = marazillierData)


summary.lm(anxiousModel)
summary.lm(sadModel)
summary.lm(happyModel)
summary.lm(angryModel)
summary.lm(disgustModel)
summary.lm(contemptModel)


#To get the contrasts for Induction:

anxiousModel<-lm(Anxiety.Change ~ Induction, data = marazillierData)
sadModel<-lm(Sad.Change ~ Induction, data = marazillierData)
happyModel<-lm(Happy.Change ~ Induction, data = marazillierData)
angryModel<-lm(Angry.Change ~ Induction, data = marazillierData)
disgustModel<-lm(Disgust.Change ~ Induction, data = marazillierData)
contemptModel<-lm(Contempt.Change ~ Induction, data = marazillierData)


summary.lm(anxiousModel)
summary.lm(sadModel)
summary.lm(happyModel)
summary.lm(angryModel)
summary.lm(disgustModel)
summary.lm(contemptModel)


#Error bar graph:
moodBar <- ggplot(moodMelt, aes(Mood, Frequency, fill = Outcome_Measure))
moodBar + stat_summary(fun.y = mean, geom = "bar", position = "dodge") + stat_summary(fun.data = mean_cl_boot, geom = "errorbar", position=position_dodge(width=0.90), width = 0.2) + labs(x = "Mood Induction Type", y = "Mean Change", fill = "Outcome Measure") + scale_y_continuous(breaks = seq(-40, 70, by = 10))



#-------Smart Alex Task 1-----

chickenData<-read.delim("chicken.dat", header = TRUE)

chickenData$group<-factor(chickenData$group, levels = c(1:2), labels = c("Manic Psychosis", "Sussex Lecturers"))

#---Explore the data
# Graphs

chickenMelt<-melt(chickenData, id = c("group"), measured = c("quality", "quantity"))
names(chickenMelt)<-c("group","Outcome_Measure", "Value")


Boxplot <- ggplot(chickenMelt, aes(group, Value))
Boxplot + geom_boxplot() + labs(x = "Group", y = "Value") + facet_wrap(~ Outcome_Measure)
imageFile <- paste(imageDirectory,"16 chicken Boxplot.png",sep="/")
ggsave(file = imageFile)


Bar <- ggplot(chickenMelt, aes(group, Value))
Bar + stat_summary(fun.y = mean, geom = "bar", position = "dodge", fill = "white") + stat_summary(fun.data = mean_cl_boot, geom = "errorbar", position=position_dodge(width=0.90), width = 0.2) + labs(x = "Group", y = "Value") + facet_wrap(~ Outcome_Measure, ncol = 2, scales = "free_y")
imageFile <- paste(imageDirectory,"16 chicken Bar.png",sep="/")
ggsave(file = imageFile)


options(digits = 3)
by(chickenData$quality, chickenData$group, stat.desc, basic = FALSE)
by(chickenData$quantity, chickenData$group, stat.desc, basic = FALSE)
options(digits = 7)


by(chickenData[, 2:3], chickenData$group, cov)


# selects the rows and columns of the data frame for each group, the t() rtransposes the data (so variables are in rows and cases in columns, which is the format that the shapiro test function requires)


Manic Psychosis<-t(chickenData[1:10, 2:3])
Sussex Lecturers<-t(chickenData[11:20, 2:3])

mshapiro.test(Manic Psychosis)
mshapiro.test(Sussex Lecturers)


aq.plot(chickenData[, 2:3])


#---Main analysis

#Contrasts
contrasts(chickenData$group)<-c(-1, 1)

outcome<-cbind(chickenData$quality, chickenData$quantity)
chickenModel<-manova(outcome ~ group, data = chickenData)
summary(chickenModel, intercept = TRUE)

summary.aov(chickenModel)

qualityModel<-lm(quality ~ group, data = chickenData)
quantityModel<-lm(quantity ~ group, data = chickenData)

summary.lm(qualityModel)
summary.lm(quantityModel)

#---DFA

chickenDFA<-lda(group ~ quality + quantity, data = chickenData)

plot(chickenDFA)

#-------Smart Alex Task 2-----

lyingData<-read.delim("lying.dat", header = TRUE)
lyingData$lying<-factor(lyingData$lying, levels = c("Lying Prevented", "Normal Parenting", "Lying Encouraged"))

#---Explore the data
# Graphs

lyingMelt<-melt(lyingData, id = c("lying"), measured = c("salary", "family", "work"))
names(lyingMelt)<-c("lying", "Outcome_Measure", "Value")


Boxplot <- ggplot(lyingMelt, aes(lying, Value))
Boxplot + geom_boxplot() + labs(x = "Treatment Group", y = "Value") + facet_wrap(~ Outcome_Measure, ncol = 3, scales = "free_y")
imageFile <- paste(imageDirectory,"16 Lying Boxplot.png",sep="/")
ggsave(file = imageFile)


Bar <- ggplot(lyingMelt, aes(lying, Value))
Bar + stat_summary(fun.y = mean, geom = "bar", position = "dodge", fill = "white") + stat_summary(fun.data = mean_cl_boot, geom = "errorbar", position=position_dodge(width=0.90), width = 0.2) + labs(x = "Treatment Group", y = "Value") + facet_wrap(~ Outcome_Measure, ncol = 3, scales = "free_y")
imageFile <- paste(imageDirectory,"16 lying Bar.png",sep="/")
ggsave(file = imageFile)


options(digits = 3)
by(lyingData$salary, lyingData$lying, stat.desc, basic = FALSE)
by(lyingData$family, lyingData$lying, stat.desc, basic = FALSE)
by(lyingData$work, lyingData$lying, stat.desc, basic = FALSE)
options(digits = 7)


by(lyingData[, 2:4], lyingData$lying, cov)


# selects the rows and columns of the data frame for each group, the t() rtransposes the data (so variables are in rows and cases in columns, which is the format that the shapiro test function requires)

lp<-t(lyingData[1:14, 2:4])
np<-t(lyingData[15:28, 2:4])
le<-t(lyingData[29:42, 2:4])

mshapiro.test(lp)
mshapiro.test(np)
mshapiro.test(le)

aq.plot(lyingData[, 2:4])


#---Main analysis

Encouraged_vs_Not<-c(1, 1, -2)
normal_vs_prevented <-c(1, -1, 0)
contrasts(lyingData$lying)<-cbind(Encouraged_vs_Not, normal_vs_prevented)

outcome<-cbind(lyingData$salary, lyingData$family, lyingData$work)
lyingModel<-manova(outcome ~ lying, data = lyingData)
summary(lyingModel, intercept = TRUE)

summary.aov(lyingModel)

salaryModel<-lm(salary ~ lying, data = lyingData)
familyModel<-lm(family ~ lying, data = lyingData)
workModel<-lm(work ~ lying, data = lyingData)
summary.lm(salaryModel)
summary.lm(familyModel)
summary.lm(workModel)

#---Robust analysis
lyingData$row<-rep(1:14, 3)

lyingMelt<-melt(lyingData, id = c("lying", "row"), measured = c("salary", "family", "work"))
names(lyingMelt)<-c("lying", "row", "Outcome_Measure", "value")
lyingRobust<-cast(lyingMelt, row ~ lying + Outcome_Measure, value = "value")
lyingRobust$row<-NULL

mulrank(3, 3, lyingRobust)
cmanova(3, 3, lyingRobust)

#-------Smart Alex Task 3-----


psychologyData<-read.delim("psychology.dat", header = TRUE)

psychologyData$group<-factor(psychologyData$group, levels = c(0:2), labels = c("Yr_1", "Yr_2", "Yr_3"))

#---Explore the data

by(psychologyData$exper, list(psychologyData$group), stat.desc, basic = FALSE)
by(psychologyData$stats, list(psychologyData$group), stat.desc, basic = FALSE)
by(psychologyData$social, list(psychologyData$group), stat.desc, basic = FALSE)
by(psychologyData$develop, list(psychologyData$group), stat.desc, basic = FALSE)
by(psychologyData$person, list(psychologyData$group), stat.desc, basic = FALSE)

by(psychologyData[, 2:6], psychologyData$group, cov)


# selects the rows and columns of the data frame for each group, the t() rtransposes the data (so variables are in rows and cases in columns, which is the format that the shapiro test function requires)


Yr_1<-t(psychologyData[1:11, 2:6])
Yr_2<-t(psychologyData[12:27, 2:6])
Yr_3<-t(psychologyData[28:40, 2:6])

mshapiro.test(Yr_1)
mshapiro.test(Yr_2)
mshapiro.test(Yr_3)

aq.plot(psychologyData[, 2:6])


#---Main analysis

#Contrasts
Yr1_vs_others<-c(-2, 1, -1)
Yr2_vs_Yr3 <-c(0, -1, 1)
contrasts(psychologyData$group)<-cbind(Yr1_vs_others, Yr2_vs_Yr3)


outcome<-cbind(psychologyData$exper, psychologyData$stats, psychologyData$social, psychologyData$develop, psychologyData$person)
psychologyModel<-manova(outcome ~ group, data = psychologyData)
summary(psychologyModel, intercept = TRUE)

summary.aov(psychologyModel)
#---DFA

psychologyDFA<-lda(group ~ exper + stats + social + develop + person, data = psychologyData, prior = c(11, 16, 13)/40)

psychologyDFA

plot(psychologyDFA)
predict(psychologyDFA)


#---Robust analysis
psychologyData$row<-rep(c(1:11, 1:16, 1:13))

psychologyMelt<-melt(psychologyData, id = c("group", "row"), measured = c("exper", "stats", "social", "develop", "person"))


names(psychologyMelt)<-c("group", "row", "Outcome_Measure", "value")
psychologyRobust<-cast(psychologyMelt, row ~ group + Outcome_Measure, value = "value")
psychologyRobust$row<-NULL

mulrank(3, 5, psychologyRobust)
cmanova(3, 5, psychologyRobust)

