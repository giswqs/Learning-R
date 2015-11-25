#---------------------------------------------------------------------------------------------------------
#R Code for Chapter 14 of:
#
#Field, A. P., Miles, J. N. V., & Field, Z. C. (2012). Discovering Statistics Using R: and Sex and Drugs and Rock 'N' Roll. #London Sage
#
#(c) 2011 Andy P. Field, Jeremy N. V. Miles & Zoe C. Field
#-----------------------------------------------------------------------------------------------------------

#Set the working directory (you will need to edit this to be the directory where you have stored the data files for this Chapter)

setwd("~/Documents/Academic/Data/DSU_R/Chapter 14 (Mixed ANOVA)")
imageDirectory<-"~/Documents/Academic/Books/Discovering Statistics/DSU R/DSU R I/DSUR I Images"

setwd("~/Public/Academic/Data/DSU_R/Chapter 14 (Mixed ANOVA)")
imageDirectory<-"~/Public/Academic/Books/Discovering Statistics/DSU R/DSU R I/DSUR I Images"

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


#--------Speed Dating Example ----------


dateData<-read.delim("LooksOrPersonality.dat", header = TRUE)

speedData<-melt(dateData, id = c("participant","gender"), measured = c("att_high", "av_high", "ug_high", "att_some", "av_some", "ug_some", "att_none", "av_none", "ug_none"))
names(speedData)<-c("participant", "gender", "groups", "dateRating")

speedData$personality<-gl(3, 60, labels = c("Charismatic", "Average", "Dullard"))
speedData$looks<-gl(3,20, 180, labels = c("Attractive", "Average", "Ugly"))

speedData<-speedData[order(speedData$participant),]



#Enter data by hand

participant<-gl(20, 9, labels = c("P01", "P02", "P03", "P04", "P05", "P06", "P07", "P08", "P09", "P10", "P11", "P12", "P13", "P14", "P15", "P16", "P17", "P18", "P19", "P20" ))

participant<-gl(20, 9, labels = c(paste("P", 1:20, sep = "_")))


gender<-gl(2, 90, labels = c("Male", "Female"))
personality<-gl(3, 3, 180, labels = c("Charismatic", "Average", "Dullard"))
looks<-gl(3, 1, 180, labels = c("Attractive", "Average", "Ugly"))
dateRating<-c(86, 84, 67, 88, 69, 50, 97, 48, 47, 91, 83, 53, 83, 74, 48, 86, 50, 46, 89, 88, 48, 99, 70, 48, 90, 45, 48, 89, 69, 58, 86, 77, 40, 87, 47, 53, 80, 81, 57, 88, 71, 50, 82, 50, 45, 80, 84, 51, 96, 63, 42, 92, 48, 43, 89, 85, 61, 87, 79, 44, 86, 50, 45, 100, 94, 56, 86, 71, 54, 84, 54, 47, 90, 74, 54, 92, 71, 58, 78, 38, 45, 89, 86, 63, 80, 73, 49, 91, 48, 39, 89, 91, 93, 88, 65, 54, 55, 48, 52, 84, 90, 85, 95, 70, 60, 50, 44, 45, 99, 100, 89, 80, 79, 53, 51, 48, 44, 86, 89, 83, 86, 74, 58, 52, 48, 47, 89, 87, 80, 83, 74, 43, 58, 50, 48, 80, 81, 79, 86, 59, 47, 51, 47, 40, 82, 92, 85, 81, 66, 47, 50, 45, 47, 97, 69, 87, 95, 72, 51, 45, 48, 46, 95, 92, 90, 98, 64, 53, 54, 53, 45, 95, 93, 96, 79, 66, 46, 52, 39, 47)

speedData<-data.frame(participant, gender, personality, looks, dateRating)



#Exploring Data

dateBoxplot <- ggplot(speedData, aes(looks, dateRating, colour = personality))
dateBoxplot + geom_boxplot() + labs(x = "Attractiveness", y = "Mean Rating of Date", colour = "Charisma") + facet_wrap(~gender)
imageFile <- paste(imageDirectory,"14 Speed Date Boxplot.png",sep="/")
ggsave(file = imageFile)


looksBar <- ggplot(speedData, aes(looks, dateRating))
looksBar + stat_summary(fun.y = mean, geom = "bar", fill = "White", colour = "Black") + stat_summary(fun.data = mean_cl_boot, geom = "pointrange") + labs(x = "Attractiveness", y = "Mean Rating of Date") 
imageFile <- paste(imageDirectory,"14 Speed Date Looks.png",sep="/")
ggsave(file = imageFile)


charismaBar <- ggplot(speedData, aes(personality, dateRating))
charismaBar + stat_summary(fun.y = mean, geom = "bar", fill = "White", colour = "Black") + stat_summary(fun.data = mean_cl_boot, geom = "pointrange") + labs(x = "Charisma", y = "Mean Rating of Date") 
imageFile <- paste(imageDirectory,"14 Speed Date Charisma.png",sep="/")
ggsave(file = imageFile)

genderBar <- ggplot(speedData, aes(gender, dateRating))
genderBar + stat_summary(fun.y = mean, geom = "bar", fill = "White", colour = "Black") + stat_summary(fun.data = mean_cl_boot, geom = "pointrange") + labs(x = "Gender", y = "Mean Rating of Date") 
imageFile <- paste(imageDirectory,"14 Speed Date Gender.png",sep="/")
ggsave(file = imageFile)

genderLooks <- ggplot(speedData, aes(looks, dateRating, colour = gender))
genderLooks + stat_summary(fun.y = mean, geom = "point") + stat_summary(fun.y = mean, geom = "line", aes(group= gender)) + stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) + labs(x = "Attractiveness", y = "Mean Rating of Date", colour = "Gender") + scale_y_continuous(limits = c(0,100)) 
imageFile <- paste(imageDirectory,"14 looks * gender.png",sep="/")
ggsave(file = imageFile)

genderCharisma <- ggplot(speedData, aes(personality, dateRating, colour = gender))
genderCharisma + stat_summary(fun.y = mean, geom = "point") + stat_summary(fun.y = mean, geom = "line", aes(group= gender)) + stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) + labs(x = "Charisma", y = "Mean Rating of Date", colour = "Gender") + scale_y_continuous(limits = c(0,100)) 
imageFile <- paste(imageDirectory,"14 personality * gender.png",sep="/")
ggsave(file = imageFile)

looksCharisma <- ggplot(speedData, aes(looks, dateRating, colour = personality))
looksCharisma + stat_summary(fun.y = mean, geom = "point") + stat_summary(fun.y = mean, geom = "line", aes(group= personality)) + stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) + labs(x = "Attractiveness", y = "Mean Rating of Date", colour = "Charisma") + scale_y_continuous(limits = c(0,100)) 
imageFile <- paste(imageDirectory,"14 personality * looks.png",sep="/")
ggsave(file = imageFile)

looksCharismaGender <- ggplot(speedData, aes(looks, dateRating, colour = personality))
looksCharismaGender + stat_summary(fun.y = mean, geom = "point") + stat_summary(fun.y = mean, geom = "line", aes(group= personality)) + stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) + labs(x = "Attractiveness", y = "Mean Rating of Date", colour = "Charisma") + scale_y_continuous(limits = c(0,100)) + facet_wrap(~gender)
imageFile <- paste(imageDirectory,"14 three way interaction.png",sep="/")
ggsave(file = imageFile)

#using ezAnova
options(digits = 3)
by(speedData$dateRating, speedData$looks, stat.desc, basic = FALSE)
by(speedData$dateRating, speedData$personality, stat.desc, basic = FALSE)
by(speedData$dateRating, speedData$gender, stat.desc, basic = FALSE)
by(speedData$dateRating, list(speedData$looks, speedData$gender), stat.desc, basic = FALSE)
by(speedData$dateRating, list(speedData$personality, speedData$gender), stat.desc, basic = FALSE)
by(speedData$dateRating, list(speedData$looks, speedData$personality), stat.desc, basic = FALSE)
by(speedData$dateRating, list(speedData$looks, speedData$personality, speedData$gender), stat.desc, basic = FALSE)
options(digits = 7)

#using ezAnova
SomevsNone<-c(1, 1, -2)
HivsAv<-c(1, -1, 0)

AttractivevsUgly<-c(1, 1, -2)
AttractvsAv<-c(1, -1, 0)

contrasts(speedData$personality)<-cbind(SomevsNone, HivsAv)
contrasts(speedData$looks)<-cbind(AttractivevsUgly, AttractvsAv)

options(digits = 3)
speedModel<-ezANOVA(data = speedData, dv = .(dateRating), wid = .(participant),  between = .(gender), within = .(looks, personality), type = 3, detailed = TRUE)
speedModel
options(digits = 7)

#Using lme


HighvsAv<-c(1, 0, 0)
DullvsAv<-c(0, 0, 1)

AttractivevsAv<-c(1, 0, 0)
UglyvsAv<-c(0, 0, 1)

contrasts(speedData$personality)<-cbind(HighvsAv, DullvsAv)
contrasts(speedData$looks)<-cbind(AttractivevsAv, UglyvsAv)
contrasts(speedData$gender)<-c(1, 0)

#Building the model
baseline<-lme(dateRating ~ 1, random = ~1|participant/looks/personality, data = speedData, method = "ML")
looksM<-update(baseline, .~. + looks)
personalityM<-update(looksM, .~. + personality)
genderM<-update(personalityM, .~. + gender)
looks_gender<-update(genderM, .~. + looks:gender)
personality_gender<-update(looks_gender, .~. + personality:gender)
looks_personality<-update(personality_gender, .~. + looks:personality)
speedDateModel<-update(looks_personality, .~. + looks:personality:gender)


anova(baseline, looksM, personalityM, genderM, looks_gender, personality_gender, looks_personality, speedDateModel)
summary(speedDateModel)



#-------Effect Sizes

rcontrast<-function(t, df)
{r<-sqrt(t^2/(t^2 + df))
	print(paste("r = ", r))
	}



t<-summary(speedDateModel)$tTable[,4]
df<-summary(speedDateModel)$tTable[,3]

rcontrast(-1.20802, 108)
rcontrast(3.85315, 108)
rcontrast(-7.53968, 108)
rcontrast(-0.97891, 108)

#--------Robust Test: Profile Picture Example----------


pictureData<-read.delim("ProfilePicture.dat", header = TRUE)
names(pictureData)<-c("case", "relationship_status", "With Man", "Alone")
pictureData$row<-c(1:17, 1:23)

profileMelt<-melt(pictureData, id = c("case", "row", "relationship_status"), measured = c("couple", "alone"))
names(profileMelt)<-c("case", "row", "relationship_status", "profile_picture", "friend_requests")
profileData<-cast(profileMelt, row~relationship_status + profile_picture, value = "friend_requests")

profileGraph <- ggplot(profileMelt, aes(relationship_status, friend_requests, colour = profile_picture))
profileGraph + stat_summary(fun.y = mean, geom = "point") + stat_summary(fun.y = mean, geom = "line", aes(group= profile_picture)) + stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) + labs(x = "Relationship Status", y = "Number of Friend Requests", colour = "Contents of Profile Picture") + scale_y_continuous(limits = c(0,10)) 
imageFile <- paste(imageDirectory,"14 profile picture.png",sep="/")
ggsave(file = imageFile)

by(profileMelt$friend_requests, list(profileMelt$profile_picture, profileMelt$relationship_status), stat.desc, basic = FALSE)

profileData $row<-NULL



tsplit(2, 2, profileData, tr = .2)
tsplitbt(2, 2, profileData)
sppba(2, 2, profileData, est = mom, nboot = 2000)
sppbb(2, 2, profileData, est = mom, nboot = 2000)
sppbi(2, 2, profileData, est = mom, nboot = 2000)


sppba(2, 2, profileData, est = median, nboot = 2000)
sppbb(2, 2, profileData, est = median, nboot = 2000)
sppbi(2, 2, profileData, est = median, nboot = 2000)

#--------Self Test----------



#-------labcoat leni-----

#Read in the data:
jealousData<-read.delim("schutzwohl(2008).dat", header = TRUE)

#Set Relationship and Gender to be factors:
jealousData$Relationship<-factor(jealousData$Relationship, levels = c(0:1), labels = c("Without Partner", "With Partner"))
jealousData$Gender<-factor(jealousData$Gender, levels = c(1:2), labels = c("Male", "Female"))

jealousLong<-melt(jealousData, id = c("Participant", "Relationship", "Age", "Distractor_Colour", "Gender"), measured = c("Distracter_Neutral", "Distracter_Emotional", "Distracter_Sexual", "Target_Neutral", "Target_Emotional", "Target_Sexual") )

names(jealousLong)<-c("Participant", "Relationship", "Age", "Distractor_Colour", "Gender", "RMVariables", "Sentences_Remembered")

jealousLong$Sentence_Type<-gl(2, 240, labels = c("Distracter", "Target"))
jealousLong$Distracter_Type<-gl(3, 80, 480, labels = c("Neutral", "Emotional", "Sexual"))



#to just get the data for males:
malesOnly<-subset(jealousLong, Gender == "Male")

#to just get the data for females:
femalesOnly<-subset(jealousLong, Gender == "Female")

#Exploring Data

#Male Boxplot:
maleBoxplot <- ggplot(malesOnly, aes(Distracter_Type, Sentences_Remembered, colour = Sentence_Type))
maleBoxplot + geom_boxplot() + labs(x = "Distracter Type", y = "Mean Number of Sentences Remembered", colour = "Sentence Type") + facet_wrap(~Relationship)

#Female Boxplot:
femaleBoxplot <- ggplot(femalesOnly, aes(Distracter_Type, Sentences_Remembered, colour = Sentence_Type))
femaleBoxplot + geom_boxplot() + labs(x = "Distracter Type", y = "Mean Number of Sentences Remembered", colour = "Sentence Type") + facet_wrap(~Relationship)

#Descriptives

by(malesOnly$Sentences_Remembered, list(malesOnly$Distracter_Type, malesOnly$Sentence_Type, malesOnly$Relationship), stat.desc, basic = FALSE)

by(femalesOnly$Sentences_Remembered, list(femalesOnly$Distracter_Type, femalesOnly$Sentence_Type, femalesOnly$Relationship), stat.desc, basic = FALSE)

#using ezAnova for males
NeutralvsEmotionalandSexual<-c(-2, 1, 1)
EmotionalvsSexual<-c(0, -1, 1)

contrasts(malesOnly$Distracter_Type)<-cbind(NeutralvsEmotionalandSexual, EmotionalvsSexual)


malesModel<-ezANOVA(data = malesOnly, dv = .(Sentences_Remembered), wid = .(Participant),  between = .(Relationship), within = .(Distracter_Type, Sentence_Type), type = 3, detailed = TRUE)
malesModel

#using ezAnova for Females:

NeutralvsEmotionalandSexual<-c(-2, 1, 1)
EmotionalvsSexual<-c(0, -1, 1)

contrasts(femalesOnly$Distracter_Type)<-cbind(NeutralvsEmotionalandSexual, EmotionalvsSexual)


options(digits = 3)
femalesModel<-ezANOVA(data = femalesOnly, dv = .(Sentences_Remembered), wid = .(Participant),  between = .(Relationship), within = .(Distracter_Type, Sentence_Type), type = 3, detailed = TRUE)
femalesModel
options(digits = 7)

-----#Using lme for males:

EvsN<-c(0, 1, 0)
SvsN<-c(0, 0, 1)

contrasts(malesOnly$Distracter_Type)<-cbind(EvsN, SvsN)

contrasts(malesOnly$Relationship)<-c(1, 0)
contrasts(malesOnly$Sentence_Type)<-c(1, 0)

#Building the model

baseline<-lme(Sentences_Remembered ~ 1, random = ~1|Participant/Distracter_Type/Sentence_Type, data = malesOnly, method = "ML")
SentenceM<-update(baseline, .~. + Sentence_Type)
DistracterM<-update(SentenceM, .~. + Distracter_Type)
RelationshipM<-update(DistracterM, .~. + Relationship)
Sentence_Relationship<-update(RelationshipM, .~. + Sentence_Type:Relationship)
Distracter_Relationship<-update(Sentence_Relationship, .~. + Distracter_Type:Relationship)
Sentence_Distracter<-update(Distracter_Relationship, .~. + Sentence_Type:Distracter_Type)
malejealousModel<-update(Sentence_Distracter, .~. + Sentence_Type:Distracter_Type:Relationship)


anova(baseline, SentenceM, DistracterM, RelationshipM, Sentence_Relationship, Distracter_Relationship, Sentence_Distracter, malejealousModel)


summary(malejealousModel)

#Using lme for Females:

EvsN<-c(0, 1, 0)
SvsN<-c(0, 0, 1)

contrasts(femalesOnly$Distracter_Type)<-cbind(EvsN, SvsN)

contrasts(femalesOnly$Relationship)<-c(1, 0)
contrasts(femalesOnly$Sentence_Type)<-c(1, 0)

#Building the model

baseline<-lme(Sentences_Remembered ~ 1, random = ~1|Participant/Distracter_Type/Sentence_Type, data = femalesOnly, method = "ML")
SentenceM<-update(baseline, .~. + Sentence_Type)
DistracterM<-update(SentenceM, .~. + Distracter_Type)
RelationshipM<-update(DistracterM, .~. + Relationship)
Sentence_Relationship<-update(RelationshipM, .~. + Sentence_Type:Relationship)
Distracter_Relationship<-update(Sentence_Relationship, .~. + Distracter_Type:Relationship)
Sentence_Distracter<-update(Distracter_Relationship, .~. + Sentence_Type:Distracter_Type)
femalejealousModel<-update(Sentence_Distracter, .~. + Sentence_Type:Distracter_Type:Relationship)


anova(baseline, SentenceM, DistracterM, RelationshipM, Sentence_Relationship, Distracter_Relationship, Sentence_Distracter, femalejealousModel)


summary(femalejealousModel)

#-------Smart Alex Task 1-----

#Read in the data:
mixedAttitude<-read.delim("MixedAttitude.dat", header = TRUE)

#Set gender to be a factor:

mixedAttitude$gender<-factor(mixedAttitude$gender, levels = c(1:2), labels = c("Male", "Female"))

attitudeLong<-melt(mixedAttitude, id = c("Participant", "gender"), measured = c("beerpos", "beerneg", "beerneut", "winepos", "wineneg", "wineneut", "waterpos", "waterneg", "waterneu") )

names(attitudeLong)<-c("Participant", "Gender", "Groups", "Drink_Rating")

attitudeLong$Drink<-gl(3, 60, labels = c("Beer", "Wine", "Water"))
attitudeLong$Imagery<-gl(3, 20, 180, labels = c("Positive", "Negative", "Neutral"))


attitudeLong<-attitudeLong[order(attitudeLong$Participant),]

#Exploring Data
 
Boxplot:

attitudeBoxplot <- ggplot(attitudeLong, aes(Drink, Drink_Rating, colour = Imagery))
attitudeBoxplot + geom_boxplot() + labs(x = "Drink", y = "Mean Drink Rating", colour = "Imagery") + facet_wrap(~Gender)

#Descriptives

by(attitudeLong$Drink_Rating, list(attitudeLong$Drink, attitudeLong$Imagery, attitudeLong$Gender), stat.desc, basic = FALSE)

#using ezAnova:

NeutvsPosandNeg<-c(1, 1, -2)
PosvsNeg<-c(1, -1, 0)

contrasts(attitudeLong$Imagery)<-cbind(NeutvsPosandNeg, PosvsNeg)

WatervsBeerandWine<-c(1, 1, -2)
BeervsWine<-c(1, -1, 0)

contrasts(attitudeLong$Drink)<-cbind(WatervsBeerandWine, BeervsWine)


attitudeModel<-ezANOVA(data = attitudeLong, dv = .(Drink_Rating), wid = .(Participant),  between = .(Gender), within = .(Drink, Imagery), type = 3, detailed = TRUE)

attitudeModel

genderBar <- ggplot(attitudeLong, aes(Gender, Drink_Rating))
genderBar + stat_summary(fun.y = mean, geom = "bar", fill = "White", colour = "Black") + stat_summary(fun.data = mean_cl_boot, geom = "pointrange") + labs(x = "Gender", y = "Mean Rating of Drink") + scale_y_continuous(limits = c(-25,25)) 


GenderDrink <- ggplot(attitudeLong, aes(Drink, Drink_Rating, colour = Gender))
GenderDrink + stat_summary(fun.y = mean, geom = "point") + stat_summary(fun.y = mean, geom = "line", aes(group= Gender)) + stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) + labs(x = "Type of Drink", y = "Mean Rating of Drink", colour = "Gender") + scale_y_continuous(limits = c(-25,25)) 


GenderImagery <- ggplot(attitudeLong, aes(Imagery, Drink_Rating, colour = Gender))
GenderImagery + stat_summary(fun.y = mean, geom = "point") + stat_summary(fun.y = mean, geom = "line", aes(group= Gender)) + stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) + labs(x = "Type of Imagery", y = "Mean Rating of Drink", colour = "Gender") + scale_y_continuous(limits = c(-25,25)) 


GenderDrinkImagery <- ggplot(attitudeLong, aes(Drink, Drink_Rating, colour = Imagery))
GenderDrinkImagery + stat_summary(fun.y = mean, geom = "point") + stat_summary(fun.y = mean, geom = "line", aes(group= Imagery)) + stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) + labs(x = "Type of Drink", y = "Mean Rating of Drink", colour = "Type of Imagery") + scale_y_continuous(limits = c(-20,30)) + facet_wrap(~Gender)
-----#Using lme:

PosvsNeut<-c(1, 0, 0)
NegvsNeut<-c(0, 1, 0)

contrasts(attitudeLong$Imagery)<-cbind(PosvsNeut, NegvsNeut)


BeervsWater<-c(1, 0, 0)
WinevsWater<-c(0, 1, 0)

contrasts(attitudeLong$Drink)<-cbind(BeervsWater, WinevsWater)

contrasts(attitudeLong$Gender)<-c(1, 0)


#Building the model

baseline<-lme(Drink_Rating ~ 1, random = ~1|Participant/Drink/Imagery, data = attitudeLong, method = "ML")
ImageryM<-update(baseline, .~. + Imagery)
DrinkM<-update(ImageryM, .~. + Drink)
GenderM<-update(DrinkM, .~. + Gender)
Imagery_Gender<-update(GenderM, .~. + Imagery:Gender)
Drink_Gender<-update(Imagery_Gender, .~. + Drink:Gender)
Imagery_Drink<-update(Drink_Gender, .~. + Imagery:Drink)
attitudeModel<-update(Imagery_Drink, .~. + Imagery:Drink:Gender)


anova(baseline, ImageryM, DrinkM, GenderM, Imagery_Gender, Drink_Gender, Imagery_Drink, attitudeModel)


summary(attitudeModel)

#-------Effect Sizes

rcontrast<-function(t, df)
{r<-sqrt(t^2/(t^2 + df))
	print(paste("r = ", r))
	}


rcontrast(-1.779832, 108)
rcontrast(1.855570, 108)
rcontrast(-0.170409, 108)
rcontrast(-1.704095, 108)




#-------Smart Alex Task 2-----


#Read in the data:
textMessages<-read.delim("Textmessages.dat", header = TRUE)

#Set Group to be a factor:

textMessages$Group<-factor(textMessages$Group, levels = c(1:2), labels = c("Text Messagers", "Controls"))

textLong<-melt(textMessages, id = c("Participant", "Group"), measured = c("Baseline", "Six_months") )

names(textLong)<-c("Participant", "Group", "Time", "Grammar_Score")

textLong<-textLong[order(textLong$Participant),]

#Exploring Data
 
#Line graph:

textLine <- ggplot(textLong, aes(Time, Grammar_Score, colour = Group))
textLine + stat_summary(fun.y = mean, geom = "point") + stat_summary(fun.y = mean, geom = "line", aes(group= Group)) + stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) + labs(x = "Time", y = "Mean Grammar Score", colour = "Group")


#Descriptives

by(textLong$Grammar_Score, list(textLong$Time, textLong$Group), stat.desc, basic = FALSE)

#using ezAnova:

textModel<-ezANOVA(data = textLong, dv = .(Grammar_Score), wid = .(Participant),  between = .(Group), within = .(Time), type = 3, detailed = TRUE)

textModel

TimeBar <- ggplot(textLong, aes(Time, Grammar_Score))
TimeBar + stat_summary(fun.y = mean, geom = "bar", fill = "White", colour = "Black") + stat_summary(fun.data = mean_cl_boot, geom = "pointrange") + labs(x = "Time", y = "Mean Grammar Score")  

GroupBar <- ggplot(textLong, aes(Group, Grammar_Score))
GroupBar + stat_summary(fun.y = mean, geom = "bar", fill = "White", colour = "Black") + stat_summary(fun.data = mean_cl_boot, geom = "pointrange") + labs(x = "Group", y = "Mean Grammar Score")  


-----#Using lme:

#Building the model

baseline<-lme(Grammar_Score ~ 1, random = ~1|Participant/Time/Group, data = textLong, method = "ML")
TimeM<-update(baseline, .~. + Time)
GroupM<-update(TimeM, .~. + Group)
textModel<-update(GroupM, .~. + Time:Group)


anova(baseline, TimeM, GroupM, textModel)


summary(textModel)

#-------Effect Sizes

rcontrast(2.041172  , 48)




#-------Smart Alex Task 3-----
#Read in the data:
bigBrother<-read.delim("BigBrother.dat", header = TRUE)

#Set bb to be a factor:

bigBrother$bb<-factor(bigBrother$bb, levels = c(0:1), labels = c("No Treatment Control", "Big Brother Contestant"))

brotherLong<-melt(bigBrother, id = c("Participant", "bb"), measured = c("time1", "time2") )

names(brotherLong)<-c("Participant", "Group", "Time", "Personality_Score")

brotherLong<-brotherLong[order(brotherLong$Participant),]

#Exploring Data
 
#Line graph:

PersonalityTime <- ggplot(brotherLong, aes(Time, Personality_Score, colour = Group))
PersonalityTime + stat_summary(fun.y = mean, geom = "point") + stat_summary(fun.y = mean, geom = "line", aes(group= Group)) + stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) + labs(x = "Time", y = "Mean Personality Score (%)", colour = "Group") 

#Descriptives

by(brotherLong$Personality_Score, list(brotherLong$Time, brotherLong$Group), stat.desc, basic = FALSE)

#using ezAnova:

brotherModel<-ezANOVA(data = brotherLong, dv = .(Personality_Score), wid = .(Participant),  between = .(Group), within = .(Time), type = 3, detailed = TRUE)

brotherModel

TimeBar <- ggplot(brotherLong, aes(Time, Personality_Score))
TimeBar + stat_summary(fun.y = mean, geom = "bar", fill = "White", colour = "Black") + stat_summary(fun.data = mean_cl_boot, geom = "pointrange") + labs(x = "Time", y = "Mean Personality Score") 

GroupBar <- ggplot(brotherLong, aes(Group, Personality_Score))
GroupBar + stat_summary(fun.y = mean, geom = "bar", fill = "White", colour = "Black") + stat_summary(fun.data = mean_cl_boot, geom = "pointrange") + labs(x = "Group", y = "Mean Personality Score")  


-----#Using lme:

#Building the model

baseline<-lme(Personality_Score ~ 1, random = ~1|Participant/Time/Group, data = brotherLong, method = "ML")
TimeM<-update(baseline, .~. + Time)
GroupM<-update(TimeM, .~. + Group)
brotherModel<-update(GroupM, .~. + Time:Group)


anova(baseline, TimeM, GroupM, brotherModel)


summary(brotherModel)

#-------Effect Sizes

rcontrast(2.673718, 48)

#-------Smart Alex Task 4-----

pictureData<-read.delim("ProfilePicture.dat", header = TRUE)

pictureLong<-melt(pictureData, id = c("case", "relationship_status"), measured = c("couple", "alone") )

names(pictureLong)<-c("Case", "Relationship_Status", "Photo", "Friend_Requests")


pictureLong<-pictureLong[order(pictureLong$Case),]

#Exploring Data
 
#Line graph:

profileLine <- ggplot(pictureLong, aes(Relationship_Status, Friend_Requests, colour = Photo))
profileLine + stat_summary(fun.y = mean, geom = "point") + stat_summary(fun.y = mean, geom = "line", aes(group= Photo)) + stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) + labs(x = "Relationship Status", y = "Mean Number of Friend Requests", colour = "photo")


#Descriptives

by(pictureLong$Friend_Requests, list(pictureLong$Relationship_Status, pictureLong$Photo), stat.desc, basic = FALSE)

#using ezAnova:

pictureModel<-ezANOVA(data = pictureLong, dv = .(Friend_Requests), wid = .(Case),  between = .(Relationship_Status), within = .(Photo), type = 3, detailed = TRUE)

pictureModel

PhotoBar <- ggplot(pictureLong, aes(Photo, Friend_Requests))
PhotoBar + stat_summary(fun.y = mean, geom = "bar", fill = "White", colour = "Black") + stat_summary(fun.data = mean_cl_boot, geom = "pointrange") + labs(x = "Photo", y = "Mean Number of Friends")  

RelationshipBar <- ggplot(pictureLong, aes(Relationship_Status, Friend_Requests))
RelationshipBar + stat_summary(fun.y = mean, geom = "bar", fill = "White", colour = "Black") + stat_summary(fun.data = mean_cl_boot, geom = "pointrange") + labs(x = "Relationship_Status", y = "Mean Number of Friends")  


-----#Using lme:

#Building the model

baseline<-lme(Friend_Requests ~ 1, random = ~1|Case/Relationship_Status/Photo, data = pictureLong, method = "ML")
RelationshipM<-update(baseline, .~. + Relationship_Status)
PhotoM<-update(RelationshipM, .~. + Photo)
pictureModel<-update(PhotoM, .~. + Relationship_Status:Photo)


anova(baseline, RelationshipM, PhotoM, pictureModel)


summary(pictureModel)

#-------Effect Sizes

rcontrast(2.722811, 38)










