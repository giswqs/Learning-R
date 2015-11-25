#---------------------------------------------------------------------------------------------------------
#R Code for Chapter 4 of:
#
#Field, A. P., Miles, J. N. V., & Field, Z. C. (2012). Discovering Statistics Using R: and Sex and Drugs and Rock 'N' Roll. #London Sage
#
#(c) 2011 Andy P. Field, Jeremy N. V. Miles & Zoe C. Field
#-----------------------------------------------------------------------------------------------------------

#Set the working directory (you will need to edit this to be the directory where you have stored the data files for this Chapter)

setwd("~/Documents/Academic/Data/DSU_R/Chapter 04 (Graphs) R")
imageDirectory<-"~/Documents/Academic/Books/Discovering Statistics/DSU R/DSU R I/DSUR I Images"

imageDirectory<-file.path(Sys.getenv("HOME"), "Documents", "Academic", "Books", "Discovering Statistics", "DSU R", "DSU R I", "DSUR I Images")


######A function to make it quick to save graphs in the image directory

saveInImageDirectory<-function(filename){
	imageFile <- file.path(imageDirectory, filename)
	ggsave(imageFile)	
}



######Initiate packages

#If you don't have ggplot2 installed then use:
install.packages(c("ggplot2", "plyr"))


#Initiate ggplot2
library(ggplot2)
library(reshape)
library(plyr)

#--------Quick Tutorial----------
facebookData <- read.delim("FacebookNarcissism.dat",  header = TRUE)

graph <- ggplot(facebookData, aes(NPQC_R_Total, Rating))
graph + geom_point() + opts(title = "geom_point()")
saveInImageDirectory("04 Tutorial Point.png")

graph + geom_point(shape = 17) + opts(title = "geom_point(shape = 17)")
saveInImageDirectory("04 Tutorial Triangle.png")

graph + geom_point(size = 6) + opts(title = "geom_point(size = 6)")
saveInImageDirectory("04 Tutorial Size.png")

graph + geom_point(aes(colour = Rating_Type)) + opts(title = "geom_point(aes(colour = Rating_Type))")
saveInImageDirectory("04 Tutorial Colour Point.png")

graph + geom_point(aes(colour = Rating_Type), position = "jitter") + opts(title = "geom_point(aes(colour = Rating_Type), position = jitter)")
saveInImageDirectory("04 Tutorial Jitter.png")

graph + geom_point(aes(shape = Rating_Type), position = "jitter") + opts(title = "geom_point(aes(shape = Rating_Type), position = jitter)")
saveInImageDirectory("04 Tutorial Jitter2.png")


#--------Scatterplots----------

examData <- read.delim("Exam Anxiety.dat",  header = TRUE)
names(examData)

#Simple scatter
scatter <- ggplot(examData, aes(Anxiety, Exam))
scatter + geom_point() + labs(x = "Exam Anxiety", y = "Exam Performance %") 
saveInImageDirectory("04 Exam Scatter.png")

#Simple scatter with smooth
scatter <- ggplot(examData, aes(Anxiety, Exam))
scatter + geom_point() + geom_smooth() + labs(x = "Exam Anxiety", y = "Exam Performance %") 
saveInImageDirectory("04 Exam Smooth.png")


#Simple scatter with regression line
scatter <- ggplot(examData, aes(Anxiety, Exam))
scatter + geom_point() + geom_smooth(method = "lm", colour = "Red", se = F) + labs(x = "Exam Anxiety", y = "Exam Performance %") 
saveInImageDirectory("04 Exam Scatter w. Line.png")

#Simple scatter with regression line + CI

scatter <- ggplot(examData, aes(Anxiety, Exam))
scatter + geom_point() + geom_smooth(method = "lm", colour = "Red")+ labs(x = "Exam Anxiety", y = "Exam Performance %") 
saveInImageDirectory("04 Exam Scatter w Line & CI.png")


#Simple scatter with regression line + coloured CI
scatter <- ggplot(examData, aes(Anxiety, Exam))
scatter + geom_point() + geom_smooth(method = "lm", colour = "Red", alpha = 0.1, fill = "Red") + labs(x = "Exam Anxiety", y = "Exam Performance %") 
saveInImageDirectory("04 Exam Scatter w. Line & red CI.png")


#Grouped scatter with regression line + CI

scatter <- ggplot(examData, aes(Anxiety, Exam, colour = Gender))
scatter + geom_point() + geom_smooth(method = "lm", aes(fill = Gender), alpha = 0.1) + labs(x = "Exam Anxiety", y = "Exam Performance %", colour = "Gender") 
saveInImageDirectory("04 Exam Grouped Scatter w Line & CI.png")



#--------HISTOGRAMS----------

##Load the data file into R. This is a tab-delimited file hence use of read.delim

festivalData <- read.delim("DownloadFestival.dat",  header = TRUE)

festivalHistogram <- ggplot(festivalData, aes(day1)) + opts(legend.position="none")
festivalHistogram + geom_histogram(binwidth = 0.4) + labs(x = "Hygiene (Day 1 of Festival)", y = "Frequency")
saveInImageDirectory("04 Download Festival Histogram with Outlier.png")

#Locate outlier

festivalData<-festivalData[order(festivalData$day1),]


#Density without outlier

festivalData2 = read.delim("DownloadFestival(No Outlier).dat",  header = TRUE)
festivalDensity <- ggplot(festivalData2, aes(day1))
festivalDensity + geom_density() + labs(x = "Hygiene (Day 1 of Festival)", y = "Density Estimate")
saveInImageDirectory("04 Download Density.png")

festivalDensity + geom_density(aes(fill = gender), alpha = 0.5) + labs(x = "Hygiene (Day 1 of Festival)", y = "Density Estimate")
saveInImageDirectory("04 Download gender Density.png")

festivalDensity + geom_histogram(binwidth = 0.4) + labs(x = "Hygiene (Day 1 of Festival)", y = "Frequency") + opts(legend.position="none")
saveInImageDirectory("04 Download Festival Histogram without Outlier.png")


#--------BOXPLOTS----------

festivalBoxplot <- ggplot(festivalData, aes(gender, day1))
festivalBoxplot + geom_boxplot() + labs(x = "Gender", y = "Hygiene (Day 1 of Festival)")
saveInImageDirectory("04 Download Festival Boxplot with Outlier.png")

#with outlier removed

festivalData2 = read.delim("DownloadFestival(No Outlier).dat",  header = TRUE)
festivalBoxplot2 <- ggplot(festivalData2, aes(gender, day1))
festivalBoxplot2 + geom_boxplot() + labs(x = "Gender", y = "Hygiene (Day 1 of Festival)")
saveInImageDirectory("04 Download Festival Boxplot.png")

#days 2 and 3

festivalBoxplot <- ggplot(festivalData, aes(gender, day2))
festivalBoxplot + geom_boxplot() + labs(x = "Gender", y = "Hygiene (Day 2 of Festival)")
saveInImageDirectory("04 Download Festival Boxplot day 2.png")

festivalBoxplot <- ggplot(festivalData, aes(gender, day3))
festivalBoxplot + geom_boxplot() + labs(x = "Gender", y = "Hygiene (Day 3 of Festival)")
saveInImageDirectory("04 Download Festival Boxplot day 3.png")



#--------OUTLIERS----------


outlierSummary<-function(variable, digits = 2){
	
	zvariable<-(variable-mean(variable, na.rm = TRUE))/sd(variable, na.rm = TRUE)
		
	outlier95<-abs(zvariable) >= 1.96
	outlier99<-abs(zvariable) >= 2.58
	outlier999<-abs(zvariable) >= 3.29
	
	ncases<-length(na.omit(zvariable))
	
	percent95<-round(100*length(subset(outlier95, outlier95 == TRUE))/ncases, digits)
	percent99<-round(100*length(subset(outlier99, outlier99 == TRUE))/ncases, digits)
	percent999<-round(100*length(subset(outlier999, outlier999 == TRUE))/ncases, digits)
	
	cat("Absolute z-score greater than 1.96 = ", percent95, "%", "\n")
	cat("Absolute z-score greater than 2.58 = ",  percent99, "%", "\n")
	cat("Absolute z-score greater than 3.29 = ",  percent999, "%", "\n")
}

outlierSummary(festivalData$day2)



#--------Bar Charts----------

chickFlick = read.delim("ChickFlick.dat",  header = TRUE)

bar <- ggplot(chickFlick, aes(film, arousal))
bar + stat_summary(fun.y = mean, geom = "bar", fill = "White", colour = "Black") + stat_summary(fun.data = mean_cl_normal, geom = "pointrange") + labs(x = "Film", y = "Mean Arousal") 
saveInImageDirectory("04 Chick Flick Error Bar.png")

colours = c(Female = "Red", Male = "Green")


bar <- ggplot(chickFlick)
bar + stat_summary(aes(film, arousal, fill = gender ), fun.y = mean, geom = "bar", position="dodge") + stat_summary(aes(film, arousal, fill = gender ), fun.data = mean_cl_normal, geom = "errorbar", position=position_dodge(width=0.90), width = 0.2) + labs(x = "Film", y = "Mean Arousal", fill = "Gender")
saveInImageDirectory("04 Chick Flick Clustered Error Bar.png")

bar <- ggplot(chickFlick, aes(film, arousal, fill = gender))
bar + stat_summary(fun.y = mean, geom = "bar", position="dodge") + stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position=position_dodge(width=0.90), width = 0.2) + labs(x = "Film", y = "Mean Arousal", fill = "Gender") + scale_fill_manual("Gender", c("Female" = "Blue", "Male" = "Green"))
saveInImageDirectory("04 Chick Flick Clustered Error Bar Custom Colours 1.png")

bar <- ggplot(chickFlick, aes(film, arousal, fill = gender))
bar + stat_summary(fun.y = mean, geom = "bar", position="dodge") + stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position=position_dodge(width=0.90), width = 0.2) + labs(x = "Film", y = "Mean Arousal", fill = "Gender") + scale_fill_manual("Gender", c("Female" = "#3366FF", "Male" = "#336633"))
saveInImageDirectory("04 Chick Flick Clustered Error Bar Custom Colours 2.png")

bar <- ggplot(chickFlick, aes(film, arousal, fill = film))
bar + stat_summary(fun.y = mean, geom = "bar") + stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) + facet_wrap(~gender) + labs(x = "Film", y = "Mean Arousal") + opts(legend.position="none")
saveInImageDirectory("04 Chick Flick Facet Error Bar.png")




#--------Self Test----------

#---------

graph + geom_line() + opts(title = "geom_line()")
saveInImageDirectory("04 Tutorial Line.png")

graph + geom_line(aes(colour = Rating_Type)) + opts(title = "geom_line(aes(colour = Rating_Type))")
saveInImageDirectory("04 Tutorial colour Line.png")

graph + geom_smooth(aes(colour = Rating_Type)) + opts(title = "geom_smooth(aes(colour = Rating_Type))")
saveInImageDirectory("04 Tutorial colour Smooth.png")

graph + geom_smooth(aes(colour = Rating_Type), method = lm) + opts(title = "geom_smooth(aes(colour = Rating_Type), method = lm)")
saveInImageDirectory("04 Tutorial colour lm.png")

graph + geom_smooth(aes(colour = Rating_Type), method = lm, se = F) + opts(title = "geom_smooth(aes(colour = Rating_Type), method = lm, se = F)")
saveInImageDirectory("04 Tutorial colour lm sef.png")

graph + geom_point(aes(colour = Rating_Type), position = "jitter") + geom_smooth(aes(colour = Rating_Type), method = lm, se = F)
saveInImageDirectory("04 Tutorial colour lm & point.png")

graph + geom_point(aes(colour = Rating_Type), position = "jitter") + geom_smooth(aes(colour = Rating_Type), method = lm, se = F) + labs(x = "Narcissism (NPQC)", y = "Facebook Picture Rating", colour = "Rated Attribute")
saveInImageDirectory("04 Tutorial colour labels.png")


graph + geom_point(aes(colour = Rating_Type), position = "jitter") + geom_smooth(aes(colour = Rating_Type), method = lm, se = F) + labs(x = "Narcissism (NPQC)", y = "Facebook Picture Rating", colour = "Rated Attribute") + scale_x_continuous(limits=c(0, 50))
saveInImageDirectory("04 Tutorial colour axis.png")


bar <- ggplot(chickFlick, aes(film, arousal))
bar + stat_summary(fun.y = mean, geom = "bar", fill = "White", colour = "Black") + stat_summary(fun.data = mean_cl_normal, geom = "errorbar", colour = "Red",  width = 0.2) + labs(x = "Film", y = "Mean Arousal") 
saveInImageDirectory("04 Chick Flick Error Bar Red.png")


bar <- ggplot(chickFlick, aes(film, arousal))
bar + stat_summary(fun.y = mean, geom = "bar", fill = "White", colour = "Black") + stat_summary(fun.data = mean_cl_boot, geom = "errorbar", colour = "Red", width = 0.2) + labs(x = "Film", y = "Mean Arousal") 
saveInImageDirectory("04 Chick Flick Error Boot Red.png")




#--------Line Charts----------

hiccupsData <- read.delim("Hiccups.dat",  header = TRUE)
hiccups<-stack(hiccupsData)
names(hiccups)<-c("Hiccups","Intervention")
hiccups$Intervention_Factor<-factor(hiccups$Intervention, levels(hiccups$Intervention)[c(1, 4, 2, 3)])


line <- ggplot(hiccups,  aes(Intervention_Factor, Hiccups))
line + stat_summary(fun.y = mean, geom = "point") + stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) + labs(x = "Intervention", y = "Mean Number of Hiccups") + stat_summary(fun.y = mean, geom = "line", aes(group=1),colour = "Red", linetype = "dashed")
saveInImageDirectory("04 Hiccups Line.png")


textData <- read.delim("TextMessages.dat",  header = TRUE)
textData$id = row(textData[1])

#textMessages = reshape(textData, idvar = c("id", "Group"), varying = c("Baseline", "Six_months"), v.names = "Grammar_Score", timevar = "Time", times = c(0:1), direction = "long")

textMessages<-melt(textData, id = c("id", "Group"), measured = c("Baseline", "Six_months"))
names(textMessages)<-c("id", "Group", "Time", "Grammar_Score")
textMessages$Time<-factor(textMessages$Time, labels = c("Baseline", "6 Months"))

print (textMessages)


line <- ggplot(textMessages, aes(Time, Grammar_Score, colour = Group))
line + stat_summary(fun.y = mean, geom = "point") + stat_summary(fun.y = mean, geom = "line", aes(group= Group)) + stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) + labs(x = "Time", y = "Mean Grammar Score", colour = "Group") 
saveInImageDirectory("04 Text Message Line.png")

#self test

line <- ggplot(textMessages, aes(Time, Grammar_Score, colour = Group))
line + stat_summary(fun.y = mean, geom = "point", aes(shape = Group), size = 4) + stat_summary(fun.y = mean, geom = "line", aes(group= Group, linetype = Group)) + stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) + labs(x = "Time", y = "Mean Grammar Score", colour = "Group") 
saveInImageDirectory("04 Text Message Line 2.png")








#notes:
# default theme is theme_grey(), you can change to black and white by adding + theme_bw()
# mean_cl_normal gives normal confidence intervals, mean_cl_boot produces bootstrapped CIs
# define any colour with #RRGGBB, e.g. fill = "#336633"

#-------------------------Smart Alex Task 1

lecturerData = read.delim("Lecturer Data.dat", header = TRUE)

#Tell R that 'job' is a factor:

lecturerData$job<-factor(lecturerData$job, levels=c(1:2), labels = c("lecturer", "student"))

#------An error bar chart showing the mean number of friends for students and lecturers. 

bar<-ggplot(lecturerData, aes(job, friends))
bar + stat_summary(fun.y = "mean", geom = "bar", fill = "white", colour = "black") + stat_summary(fun.data = "mean_cl_normal", geom = "errorbar", colour = "red", width = 0.2) + labs(x = "Job", y = "Mean Number of Friends")

#-------An error bar chart showing the mean alcohol consumption for students and lecturers. 

bar < - ggplot(lecturerData, aes(job, alcohol))
bar + stat_summary(fun.y = "mean", geom = "bar", fill = "White", colour = "Black") + stat_summary(fun.data = "mean_cl_normal", geom = "errorbar", colour = "Red", width = 0.2) + labs(x = "Job", y = "Mean Alcohol Consumption")

#-----An error line chart showing the mean income for students and lecturers:

line <- ggplot(lecturerData, aes(job, income))
line + stat_summary(fun.y = "mean", geom = "point") + stat_summary(fun.data = "mean_cl_normal", geom= "errorbar", width = 0.2) + labs(x = "Job", y = "Mean Income")+ stat_summary(fun.y = "mean", geom = "line", aes(group=1),colour = "Red", linetype = "dashed")

#An error line chart showing the mean neuroticism for students and lecturers:
line <- ggplot(lecturerData, aes(job, neurotic))
line + stat_summary(fun.y = "mean", geom =  "point") + stat_summary(fun.y = "mean", geom = "line", aes(group=1),colour = "Red", linetype = "dashed")+ stat_summary(fun.data = "mean_cl_normal", geom = "errorbar", width = 0.2) + labs(x = "Job", y = "Mean Neuroticism")

#----A scatterplot with regression lines of alcohol consumption and neuroticism grouped by lecturer/student. 

scatter <- ggplot(lecturerData, aes(neurotic, alcohol, colour = job))
scatter + geom_point() + geom_smooth(method = "lm", aes(fill = job), alpha = 0.1) + labs(x = "Neuroticism", y = "Alcohol Consumption", colour = "job")

#-------------------------Smart Alex Task 2

infidelityData = read.delim("Infidelity Data.csv", header = TRUE)

#------plot a clustered error bar chart of the mean number of bullets used against the self and the partner for males and females.

infidelityData$id = row(infidelityData[1])
Bullets = reshape(infidelityData, idvar = c("id", "Gender"), varying = c("Partner", "Self"), v.names = "Number_of_Bullets", timevar = "Recipient", times = c(0:1), direction = "long")
Bullets$Recipient<-factor(Bullets$Recipient, labels = c("Partner","Self"))
bar <- ggplot(Bullets, aes(Recipient, Number_of_Bullets, fill = Gender))
bar + stat_summary(fun.y = "mean", geom = "bar", position="dodge")
bar + stat_summary(fun.y = "mean", geom = "bar", position="dodge") + stat_summary(fun.data = "mean_cl_normal", geom = "errorbar", position=position_dodge(width=0.90), width = 0.2) + labs(x ="Recipient", y ="Number of Bullets", fill = "Gender")
