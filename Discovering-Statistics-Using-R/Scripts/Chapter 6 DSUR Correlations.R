#---------------------------------------------------------------------------------------------------------
#R Code for Chapter 6 of:
#
#Field, A. P., Miles, J. N. V., & Field, Z. C. (2012). Discovering Statistics Using R: and Sex and Drugs and Rock 'N' Roll. #London Sage
#
#(c) 2011 Andy P. Field, Jeremy N. V. Miles & Zoe C. Field
#-----------------------------------------------------------------------------------------------------------

#Set the working directory (you will need to edit this to be the directory where you have stored the data files for this Chapter)
setwd("~/Dropbox/Team Field/DSUR/DSUR_2/DSUR2 Data Files/Chapter 06 (Correlation)")
setwd("~/Public/Academic/Data/DSU_R/Chapter 06 (Correlation)")
imageDirectory<-"~/Documents/Academic/Books/Discovering Statistics/DSU R/DSU R I/DSUR I Images"

######Initiate packages

#If you don't have Hmisc installed then use:
install.packages("Hmisc")
install.packages("polycor")
install.packages("ggm")

#Initiate packages
library(Hmisc)
library(ggplot2)
library(boot)
library(polycor)
library(ggm)
library(Rcmdr)

#--------Entering data----------

adverts<-c(5,4,4,6,8)
packets<-c(8,9,10,13,15)
advertData<-data.frame(adverts, packets)

cov(advertData)
var(adverts)

#--------Self Help Task----------

scatter<-ggplot(advertData, aes(adverts, packets))
scatter + geom_point()
imageFile <- paste(imageDirectory,"06 Advert Scatter Basic.png",sep="/")
ggsave(file = imageFile)

scatter + geom_point(size = 3) + labs(x = "Adverts", y = "Packets") + scale_y_continuous(limits=c(0, 15), breaks=0:15) + scale_x_continuous(limits=c(0, 9), breaks=0:9)
imageFile <- paste(imageDirectory,"06 Advert Scatter Nice.png",sep="/")
ggsave(file = imageFile)



#-----Dealing with misisng cases

adverts<-c(5,4,4,6,8)
packetsNA<-c(8,9,10,NA,15)
age<-c(5, 12, 16, 9, 14)
advertNA<-data.frame(adverts, packetsNA, age)
cor(advertNA, use = "everything",  method = "pearson")
cor(advertNA, use = "complete.obs",  method = "kendall")


#--------Pearson r----------

cor(x,y, use = "everything", method = "correlation type")
cor.test(x,y, alternative = "string", method = "correlation type", conf.level = 0.95)

examData = read.delim("Exam Anxiety.dat",  header = TRUE)
examData

cor(examData, use = "complete.obs", method = "pearson")
cor(examData$Exam, examData$Anxiety, use = "complete.obs", method = 'pearson')
examData2 <- examData[, c("Exam", "Anxiety", "Revise")]
cor(examData2)
cor(examData[, c("Exam", "Anxiety", "Revise")])
cor(examData2)^2 * 100

examMatrix<-as.matrix(examData[, c("Exam", "Anxiety", "Revise")])
Hmisc::rcorr(examMatrix)
Hmisc::rcorr(as.matrix(examData[, c("Exam", "Anxiety", "Revise")]))

cor.test(examData$Anxiety, examData$Exam)
cor.test(examData$Revise, examData$Exam)
cor.test(examData$Anxiety, examData$Revise)

#--------Spearman's Rho----------

liarData = read.delim("The Biggest Liar.dat",  header = TRUE)
liarData

liarData = read.delim(file.choose(),  header = TRUE)

cor(liarData$Position, liarData$Creativity, method = "spearman")
cor.test(liarData$Position, liarData$Creativity, alternative = "less", method = "spearman")

liarMatrix<-as.matrix(liarData[, c("Position", "Creativity")])
rcorr(liarMatrix)

#--------Kendall's Tau----------
cor(liarData$Position, liarData$Creativity, method = "kendall")
cor.test(liarData$Position, liarData$Creativity, alternative = "less", method = "pearson")

cor(advertData$adverts, advertData$packets, method = "pearson")
cor.test(advertData$adverts, advertData$packets, alternative = "more", method = "kendall")

#--------Self Test----------

adverts<-c(5,4,4,6,8)
packets<-c(8,9,10,13,15)
cor.test(adverts, packets)

#--------Bootstrapping----------


bootTau<-function(liarData,i)cor(liarData$Position[i], liarData$Creativity[i], use = "complete.obs", method = "kendall")
boot_kendall<-boot(liarData, bootTau, 2000)
boot_kendall
boot.ci(boot_kendall)
boot.ci(boot_kendall, 0.99)

#--------Self Test----------

bootR<-function(examData2,i) cor(examData2$Exam[i], examData2$Anxiety[i], use = "complete.obs")
boot_pearson<-boot(examData2, bootR, 2000)
boot_pearson
boot.ci(boot_pearson)


bootR<-function(examData2,i) cor(examData2$Revise[i], examData2$Anxiety[i], use = "complete.obs")
boot_pearson<-boot(examData2, bootR, 2000)
boot_pearson
boot.ci(boot_pearson)

bootR<-function(examData2,i) cor(examData2$Revise[i], examData2$Exam[i], use = "complete.obs")
boot_pearson<-boot(examData2, bootR, 2000)
boot_pearson
boot.ci(boot_pearson)

bootRho<-function(examData2,i) cor(examData2$Exam[i], examData2$Anxiety[i], use = "complete.obs", method = "spearman")
boot_spearman<-boot(examData2, bootRho, 2000)
boot_spearman
boot.ci(boot_spearman)


bootRho<-function(examData2,i) cor(examData2$Revise[i], examData2$Anxiety[i], use = "complete.obs", method = "spearman")
boot_spearman <-boot(examData2, bootRho, 2000)
boot_spearman
boot.ci(boot_spearman)

bootRho<-function(examData2,i) cor(examData2$Revise[i], examData2$Exam[i], use = "complete.obs", method = "spearman")
boot_spearman <-boot(examData2, bootRho, 2000)
boot_spearman
boot.ci(boot_spearman)



#-------Point Biserial-----

catData = read.csv("pbcorr.csv",  header = TRUE)
cor.test(catData$time, catData$gender)
cor.test(catData$time, catData$recode)
catFrequencies<-table(catData$gender)
prop.table(catFrequencies)

polyserial(catData$time, catData$gender)

#-------Partial-----

maleExam<-subset(examData, Gender == "Male", select= c("Exam", "Anxiety"))
femaleExam<-subset(examData, Gender == "Female", select= c("Exam", "Anxiety"))
cor(maleExam)
cor(femaleExam)

pc<-pcor(c("Exam", "Anxiety", "Revise"), var(examData2))
pc
pc^2
pcor.test(pc, 1, 103)

#-------Differences between independent rs-----

zdifference<-function(r1, r2, n1, n2)
{zd<-(atanh(r1)-atanh(r2))/sqrt(1/(n1-3)+1/(n2-3))
	p <-1 - pnorm(abs(zd))
	print(paste("Z Difference: ", zd))
	print(paste("One-Tailed P-Value: ", p))
	}
	
zdifference(-0.506, -0.381, 52, 51)

#-------Differences between dependent rs-----

tdifference<-function(rxy, rxz, rzy, n) 
{	df<-n-3
	td<-(rxy-rzy)*sqrt((df*(1 + rxz))/(2*(1-rxy^2-rxz^2-rzy^2+(2*rxy*rxz*rzy))))
	p <-pt(td, df)
	print(paste("t Difference: ", td))
	print(paste("One-Tailed P-Value: ", p))
	}
	
tdifference(-0.441, -0.709, 0.397, 103)

#-------labcoat leni-----

#Load the data:
personalityData = read.delim("Chamorro-Premuzic.dat",  header = TRUE)

#Create a matrix from the personalityData dataframe:
personalityMatrix<-as.matrix(personalityData[, c("studentN", "studentE", "studentO", "studentA", "studentC", "lectureN", "lecturE", "lecturO", "lecturA", "lecturC")])

#run the correlation analysis:
rcorr(personalityMatrix)

#or convert the dataframe into a matrix and run the correlation analysis in one:
rcorr(as.matrix(personalityData[, c("studentN", "studentE", "studentO", "studentA", "studentC", "lectureN", "lecturE", "lecturO", "lecturA", "lecturC")]))


#-------------------------Smart Alex Task 1

#Load the data:

essayData = read.delim("EssayMarks.dat",  header = TRUE)

#Create a plot object called scatter:
scatter<-ggplot(essayData, aes(hours, essay))

#Add labels and with to your scatterplot:
scatter + geom_point(size = 3) + labs(x = "Hours Spent on Essay", y = "Essay Mark (%)") 

#Do a Shapiro-Wilks test to see whether the data are normal:

shapiro.test(essayData$essay)
shapiro.test(essayData$hours)
#Because the shapiro-Wilks tests were both non-sig we can use pearsons correltation:
cor.test(essayData$essay, essayData$hours, alternative = "greater", method = "pearson")

#correlation of hours spent on essay and grade:

essayData$grade<-factor(essayData$grade, levels = c("First Class","Upper Second Class", "Lower Second Class", "Third Class"))

cor.test(essayData$hours, as.numeric(essayData$grade), alternative = "less", method = "kendall")
cor.test(essayData$hours, as.numeric(essayData$grade), alternative = "less", method = "spearman")

#Smart Alex Task 2-------------

#load the data:

chickFlick = read.delim("ChickFlick.dat", header = TRUE)

#conduct two point-biserial correlations:

cor.test(chickFlick$gender, chickFlick$arousal)

cor.test(chickFlick$film, chickFlick$arousal)

#Smart Alex Task 3--------

#load in the data

gradesData = read.csv("grades.csv", header = TRUE)

#Conduct a Spearman correlation:
cor.test(gradesData$gcse, gradesData$stats, alternative = "greater", method = "spearman")

#conduct a Kendall correlation:
cor.test(gradesData$gcse, gradesData$stats, alternative = "greater", method = "kendall")


#-------R Souls Tip Writing Functions----

nameofFunction<-function(inputObject1, inputObject2, etc.)
{
	a set of commands that do things to the input objects
	a set of commands that specify the output of the function
}


meanOfVariable<-function(variable)
{
	mean<-sum(variable)/length(variable)
	cat("Mean = ", mean)
	
}

meanOfVariable<-function(HarryTheHungyHippo)
{
	mean<-sum(HarryTheHungyHippo)/length(HarryTheHungyHippo)
    cat("Mean = ", mean)
}


lecturerFriends = c(1,2,3,3,4)

meanOfVariable(lecturerFriends)



