#---------------------------------------------------------------------------------------------------------
#R Code for Chapter 18 of:
#
#Field, A. P., Miles, J. N. V., & Field, Z. C. (2012). Discovering Statistics Using R: and Sex and Drugs and Rock 'N' Roll. #London Sage
#
#(c) 2011 Andy P. Field, Jeremy N. V. Miles & Zoe C. Field
#-----------------------------------------------------------------------------------------------------------




#----Set the working directory------
setwd("~/Dropbox/Team Field/DSUR/DSUR_2/DSUR2 Data Files/Chapter 18 (Categorical)")

setwd("~/Documents/Academic/Data/DSU_R/Chapter 18 (Categorical)")
imageDirectory<-"~/Documents/Academic/Books/Discovering Statistics/DSU R/DSU R I/DSUR I Images"

setwd("~/Public/Academic/Data/DSU_R/Chapter 18 (Categorical)")
imageDirectory<-"~/Public/Academic/Books/Discovering Statistics/DSU R/DSU R I/DSUR I Images"


#----Install Packages-----
install.packages("gmodels")
install.packages("MASS")


#------And then load these packages, along with the boot package.-----

library(gmodels)
library(MASS)


#********************* Chi-Square Example ********************

#Enter raw data

Training<-c(rep(0, 38), rep(1, 162))
Dance<-c(rep(1, 10), rep(0, 28),  rep(1, 114), rep(0, 48))

Training<-factor(Training, labels = c("Food as Reward", "Affection as Reward"))
Dance<-factor(Dance, labels = c("No", "Yes"))

catsData<-data.frame(Training, Dance)


catsData<-read.delim("Cats.dat", header = TRUE)

#Enter contingency table data

food <- c(10, 28)
affection <- c(114, 48)
catsTable <- cbind(food, affection) 


#Do the chi-square test

CrossTable(predictor, outcome, fisher = TRUE, chisq = TRUE, expected = TRUE)
CrossTable(contingencyTable, fisher = TRUE, chisq = TRUE, expected = TRUE)

CrossTable(catsData$Training, catsData$Dance, fisher = TRUE, chisq = TRUE, expected = TRUE, sresid = TRUE, format = "SPSS")
CrossTable(catsTable, fisher = TRUE, chisq = TRUE, expected = TRUE, sresid = TRUE, format = "SPSS")

CrossTable(catsData$Training, catsData$Dance, fisher = TRUE, chisq = TRUE, expected = TRUE, prop.c = FALSE, prop.t = FALSE, prop.chisq = FALSE,  sresid = TRUE, format = "SPSS")

#********************* Loglinear Example ********************
#Self test

catsRegression<-read.delim("Cat Regression.dat", header = TRUE)

catModel<-lm(LnObserved ~ Training + Dance + Interaction, data = catsRegression)
summary(catModel)

#Self test

catsRegression<-read.delim("Cat Regression.dat", header = TRUE)

catModel2<-lm(LNExpected ~ Training + Dance, data = catsRegression)
summary(catModel2)

#chi-square as Loglinear analysis

catsDogs<-read.delim("Cats and Dogs.dat", header = TRUE)
catsDogs


table(catsDogs$Animal, catsDogs$Training, catsDogs$Dance)
xtabs(~Animal + Training + Dance, data = catsDogs)

justCats = subset(catsDogs, Animal=="Cat")
justDogs = subset(catsDogs, Animal=="Dog")


catTable<-xtabs(~ Training + Dance, data = justCats)
CrossTable(justCats$Training, justCats$Dance, sresid = TRUE, prop.t=FALSE, prop.c=FALSE, prop.chisq=FALSE, format = "SPSS")
CrossTable(justDogs$Training, justDogs$Dance, sresid = TRUE, prop.t=FALSE, prop.c=FALSE, prop.chisq=FALSE, format = "SPSS")


catSaturated<-loglm(~ Training + Dance + Training:Dance, data = catTable, fit = TRUE)
catNoInteraction<-loglm(~ Training + Dance, data = catTable, fit = TRUE)
summary(catSaturated); summary(catNoInteraction)
#mosaic plot
mosaicplot(catSaturated$fit, shade = TRUE, main = "Cats: Saturated Model")
mosaicplot(catNoInteraction$fit, shade = TRUE, main = "Cats: Expected Values")

# Loglinear analysis

CatDogContingencyTable<-xtabs(~ Animal + Training + Dance, data = catsDogs)

caturated<-loglm(~ Animal*Training*Dance, data = CatDogContingencyTable)
summary(caturated)
threeWay <- loglm(~ Animal + Training + Dance + Animal:Training + Animal:Dance + Dance:Training, data = CatDogContingencyTable)
#or
threeWay<-update(caturated, .~. -Animal:Training:Dance)
summary(threeWay)
anova(caturated, threeWay)


trainingDance<-update(threeWay, .~. -Training:Dance)
animalDance<-update(threeWay, .~. -Animal:Dance)
animalTraining<-update(threeWay, .~. -Animal:Training)
summary(animalDance)

anova(threeWay, trainingDance)
anova(threeWay, animalDance)
anova(threeWay, animalTraining)

mosaicplot(CatDogContingencyTable, shade = TRUE, main = "Cats and Dogs")

#self tes
justCats = subset(catsDogs, Animal=="Cat")
justDogs = subset(catsDogs, Animal=="Dog")


CrossTable(justCats$Training, justCats$Dance, chisq = TRUE, fisher = TRUE, sresid = TRUE, format = "SPSS")
CrossTable(justDogs$Training, justDogs$Dance, chisq = TRUE, fisher = TRUE, sresid = TRUE, format = "SPSS")

#----Labcoat Leni-------------------

americanData<-read.delim("Beckham(1929).dat", header = TRUE)

#Are black Americans Happy?:

#Enter contingency table for Happy data:
College <- c(1610, 390)
Laborers <- c(122, 378)
Preachers <- c(265, 35)
Physicians <- c(51, 159)
Housewives <- c(122, 78)
Teachers <- c(38, 108)
Lawyers <- c(64, 11)
Musician <- c(19, 31)
happyTable <- cbind(College, Laborers, Preachers, Physicians, Housewives, Teachers, Lawyers, Musician) 

#Run chi square for Happy:
CrossTable(happyTable, fisher = TRUE, chisq = TRUE, expected = TRUE, sresid = TRUE, format = "SPSS")

#Are you happy being a black American?:

#Enter contingency table for You_Happy data:
College <- c(48, 1822)
Laborers <- c(195, 305)
Preachers <- c(0, 230)
Physicians <- c(7, 203)
Housewives <- c(146, 17)
Teachers <- c(28, 79)
Lawyers <- c(0, 30)
Musician <- c(34, 16)
you_happyTable <- cbind(College, Laborers, Preachers, Physicians, Housewives, Teachers, Lawyers, Musician) 

#Run chi square for You_Happy:
CrossTable(you_happyTable, fisher = TRUE, chisq = TRUE, expected = TRUE, sresid = TRUE, format = "SPSS")

#Should black Americans be happy?:

#Enter contingency table for Should_be_Happy data:

College <- c(1810, 141)
Laborers <- c(104, 396)
Preachers <- c(36, 264)
Physicians <- c(36, 174)
Housewives <- c(120, 90)
Teachers <- c(33, 75)
Lawyers <- c(57, 7)
Musician <- c(14, 36)
should_happyTable <- cbind(College, Laborers, Preachers, Physicians, Housewives, Teachers, Lawyers, Musician) 

#Run chi square for Should_be_Happy:
CrossTable(should_happyTable, fisher = TRUE, chisq = TRUE, expected = TRUE, sresid = TRUE, format = "SPSS")

#----------Smart Alex Task 1------------------------

sageFootball<-read.delim("SageEditorsCan'tPlayFootball.dat", header = TRUE)

#Enter contingency table for the sageFootball data:

Sage_Publications <- c(5, 19)
University_of_Sussex <- c(23, 30)
sagefootball_Table <- cbind(Sage_Publications, University_of_Sussex) 

#Run chi square for sagefootball_Table:
CrossTable(sagefootball_Table, fisher = TRUE, chisq = TRUE, expected = TRUE, sresid = TRUE, format = "SPSS")


#----------Smart Alex Task 2------------------------

handlebarsData<-read.delim("Handlebars.dat", header = TRUE)

#Enter contingency table for the handlebarsData:
Dutch <- c(120, 578)
English <- c(17, 154)
handlebars_Table <- cbind(Dutch, English) 

#Run chi square for handlebars_Table:
CrossTable(handlebars_Table, fisher = TRUE, chisq = TRUE, expected = TRUE, sresid = TRUE, format = "SPSS")


#----------Smart Alex Task 3------------------------
#Load Data:
horoscopeData<-read.delim("Horoscope.dat", header = TRUE)

#Generate Contingency Table: 
horoscopeContingencyTable<-xtabs(Frequency ~ Star_Sign + Believe + True, data = horoscopeData)

#Run Loglinear analysis:
horoscopeSaturated<-loglm(Frequency ~ Star_Sign*Believe*True, data = horoscopeContingencyTable)

#View the output of the loglinear analysis:
summary(horoscopeSaturated)

#Remove three way interaction:
threeWay <- loglm(Frequency ~ Star_Sign + Believe + True + Star_Sign:Believe + Star_Sign:True + True:Believe, data = horoscopeContingencyTable)
#or
threeWay<-update(horoscopeSaturated, .~. -Star_Sign:Believe:True)
summary(threeWay)

#Calculate the difference between model with three way interaction and model without three way interaction:
anova(horoscopeSaturated, threeWay)

#Remove two way interactions, one at a time:
BelieveTrue<-update(threeWay, .~. -Believe:True)
Star_SignTrue<-update(threeWay, .~. -Star_Sign:True)
Star_SignBelieve<-update(threeWay, .~. -Star_Sign:Believe)

#Calculate the differences:
anova(threeWay, BelieveTrue)
anova(threeWay, Star_SignTrue)
anova(threeWay, Star_SignBelieve)

#---Follow up analysis--

#Generate a contingency table for the Believe and True variables:
BelieveTrue_ContingencyTable<-xtabs(Frequency ~ Believe + True, data = horoscopeData)

#Run chi square for believe*True:
CrossTable(BelieveTrue_ContingencyTable, fisher = TRUE, chisq = TRUE, expected = TRUE, sresid = TRUE, format = "SPSS")

#Generate a contingency table for the Star_Sign and Believe variables:
Star_SignBelieve_ContingencyTable<-xtabs(Frequency ~ Star_Sign + Believe, data = horoscopeData)

#Run chi square for Star_Sign*Believe:
CrossTable(Star_SignBelieve_ContingencyTable, fisher = TRUE, chisq = TRUE, expected = TRUE, sresid = TRUE, format = "SPSS")

#Test the final model:
horoscopeFinal<-loglm(Frequency ~ Star_Sign + Believe + True + Believe:True + Star_Sign:Believe, data = horoscopeContingencyTable)

#View the output of the final model:
summary(horoscopeFinal)

#----------Smart Alex Task 4------------------------

facebookData<-read.delim("Facebook.dat", header = TRUE)

#Generate Contingency Table: 
facebookContingencyTable<-xtabs(Frequency ~ Attendance + Facebook + Exam, data = facebookData)

#Run Loglinear analysis:
facebookSaturated<-loglm(Frequency ~ Attendance*Facebook*Exam, data = facebookContingencyTable)

#View the output of the loglinear analysis:
summary(facebookSaturated)

#Remove three way interaction:
threeWay <- loglm(Frequency ~ Attendance + Facebook + Exam + Attendance:Facebook + Attendance:Exam + Exam:Facebook, data = facebookContingencyTable)
#or
threeWay<-update(facebookSaturated, .~. -Attendance:Facebook:Exam)
summary(threeWay)

#Calculate the difference between model with three way interaction and model without three way interaction:
anova(facebookSaturated, threeWay)

#Remove two way interactions, one at a time:
FacebookExam<-update(threeWay, .~. -Facebook:Exam)
AttendanceExam<-update(threeWay, .~. -Attendance:Exam)
AttendanceFacebook<-update(threeWay, .~. -Attendance:Facebook)

#Calculate the differences:
anova(threeWay, FacebookExam)
anova(threeWay, AttendanceExam)
anova(threeWay, AttendanceFacebook)

#---Follow up analysis--

#Generate a contingency table for the Facebook and Exam variables:
FacebookExam_ContingencyTable<-xtabs(Frequency ~ Facebook + Exam, data = facebookData)

#Run chi square for Facebook*Exam:
CrossTable(FacebookExam_ContingencyTable, fisher = TRUE, chisq = TRUE, expected = TRUE, sresid = TRUE, format = "SPSS")

#Generate a contingency table for the Attendance and Exam variables:
AttendanceExam_ContingencyTable<-xtabs(Frequency ~ Attendance + Exam, data = facebookData)

#Run chi square for Attendance*Exam:
CrossTable(AttendanceExam_ContingencyTable, fisher = TRUE, chisq = TRUE, expected = TRUE, sresid = TRUE, format = "SPSS")






