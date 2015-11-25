#---------------------------------------------------------------------------------------------------------
#R Code for Chapter 3 of:
#
#Field, A. P., Miles, J. N. V., & Field, Z. C. (2012). Discovering Statistics Using R: and Sex and Drugs and Rock 'N' Roll. #London Sage
#
#(c) 2011 Andy P. Field, Jeremy N. V. Miles & Zoe C. Field
#-----------------------------------------------------------------------------------------------------------



install.packages("foreign")
install.packages("Hmisc")
install.packages("Rcmdr", dependencies = TRUE)
install.packages("reshape")

library(forign)
library(Rcmdr)
library(reshape)

setwd("~/Public/Academic/Data/DSU_R/Chapter 03 (The R Environment)")
setwd("~/Documents/Academic/Data/DSU_R/Chapter 03 (The R Environment)")

#-----------Metallica Data---------------------------------------------------------------------------

metallica<-c("lars", "james", "Jason", "Kirk")
metallica 
metallica<-metallica[metallica != "Jason"]
metallica
metallica<-c(metallica, "Rob")
metallica
metallicaNames<-c("Lars", "James", "Kirk", "Rob")
metallicaAges<-c(47, 47, 48, 46)

metallica<-list(metallicaNames, metallicaAges)
metallica<-cbind(metallicaNames, metallicaAges)

metallica<-data.frame(Name = metallicaNames, Age = metallicaAges)
metallica$childAge<-c(12, 12, 4, 6)
metallica$fatherhoodAge<-metallica$Age-metallica$childAge

#-----------Lecturer Data---------------------------------------------------------------------------

name<-c("Ben", "Martin","Andy","Paul", "Graham","Carina","Karina","Doug","Mark", "Zoe")

#Default date format is yyyy-mm-dd
birth_date<-as.Date(c("1977-07-03", "1969-05-24", "1973-06-21", "1970-07-16", "1949-10-10", "1983-11-05", "1987-10-08", "1989-09-16", "1973-05-20", "1984-11-12"))

job<-c(1,1,1,1,1,2,2,2,2,2)
job<-c(rep(1, 5),rep(2, 5))
job<-factor(job, levels = c(1:2), labels = c("Lecturer", "Student"))
job<-gl(2, 5, labels = c("Lecturer", "Student"))

friends<-c(5,2,0,4,1,10,12,15,12, 17)
alcohol<-c(10,15,20,5,30,25,20,16,17,18)
income<-c(20000,40000,35000,22000,50000,5000,100,3000,10000,10)
neurotic<-c(10,17,14,13,21,7,13,9,14,13)

lecturerData<-data.frame(name, birth_date, job, friends, alcohol,income, neurotic)



#--------R souls tip 3.5-----------
husband<-c("1973-06-21", "1970-07-16", "1949-10-08", "1969-05-24")
wife<-c("1984-11-12", "1973-08-02", "1948-11-11", "1983-07-23")
agegap <- husband-wife
husband<-as.Date(c("1973-06-21", "1970-07-16", "1949-10-08", "1969-05-24"))
wife<-as.Date(c("1984-11-12", "1973-08-02", "1948-11-11", "1983-07-23"))
agegap <- husband-wife
agegap



#--------Importing files-----------

lecturerData<-read.csv("Lecturer Data.csv", header = TRUE)
lecturerData$job<-factor(lecturerData$job, levels = c(1:2), labels = c("Lecturer", "Student"))

lecturerData<-read.delim("Lecturer Data.dat", header = TRUE)
lecturerData<-read.delim("Lecturer Data.txt", header = TRUE)

library(foreign)
lecturerData<- read.spss("Lecturer Data.sav",use.value.labels=TRUE, to.data.frame=TRUE)
lecturerData$birth_date <- as.Date(as.POSIXct(lecturerData$birth_date , origin="1582-10-14"))

#--------Exporting files-----------
write.table(metallica, "Metallica Data.txt", sep="\t", row.names = FALSE)
write.csv(metallica, "Metallica Data.csv")


#--------Selecting Data-----------

lecturerPersonality <- lecturerData[, c("friends", "alcohol", "neurotic")]
lecturerPersonality
lecturerOnly <- lecturerData[job=="Lecturer",]
lecturerOnly
alcoholPersonality <- lecturerData[alcohol > 10, c("friends", "alcohol", "neurotic")]
alcoholPersonality
alcoholPersonalityMatrix <- as.matrix(alcoholPersonality)
alcoholPersonalityMatrix

alcoholPersonalityMatrix <- as.matrix(lecturerData[alcohol > 10, c("friends", "alcohol", "neurotic")])


#--------Subset-----------

lecturerOnly <- subset(lecturerData, job=="Lecturer")
alcoholPersonality <- subset(lecturerData, alcohol > 10, select = c("friends", "alcohol", "neurotic"))

#--------self test-----------
highEarners <- lecturerData[income>=10000, c("name", "job", "income")]
highEarners <- subset(lecturerData, income>=10000, select = c("name", "job", "income"))

soberPeople <- lecturerData[alcohol<=12, c("name", "job", "income",  "friends")]
soberPeople <- subset(lecturerData, alcohol<=12, select = c("name", "job", "income",  "friends"))

neuroticOrAlcoholic <- lecturerData[alcohol>=20|neurotic > 14,]
neuroticOrAlcoholic <- subset(lecturerData, alcohol>=20|neurotic > 14)

#--------Restructuring Data-----------

satisfactionData = read.delim("Honeymoon Period.dat",  header = TRUE)
satisfactionStacked<-stack(satisfactionData, select = c("Satisfaction_Base", "Satisfaction_6_Months", "Satisfaction_12_Months", "Satisfaction_18_Months"))
satisfactionUnstacked<-unstack(satisfactionStacked, values~ind)

restructuredData<-reshape(satisfactionData, idvar = c("Person", "Gender"), varying = c("Satisfaction_Base", "Satisfaction_6_Months", "Satisfaction_12_Months", "Satisfaction_18_Months"), v.names = "Life_Satisfaction", timevar = "Time", times = c(0:3), direction = "long")

restructuredData.sorted<-restructuredData[order(restructuredData$Person),]

restructuredData<-melt(satisfactionData, id = c("Person", "Gender"), measured = c("Satisfaction_Base", "Satisfaction_6_Months", "Satisfaction_12_Months", "Satisfaction_18_Months"))

wideData<-cast(restructuredData, Person + Gender ~ variable, value = "value")


----Smart Alex Task 1-------

write.table(lecturerData, "Lecturer Data.txt", sep="\t", row.names = FALSE)
write.csv(lecturerData, "Lecturer Data.csv")

----Smart Alex Task 2-------
Method<-c(rep(1,10), rep(2,10))
Method<-factor(Method, levels = c(1:2), labels = c("Electric Shock", "Being Nice"))
Gender<-c(rep(0, 5),rep(1, 5), rep(0, 5),rep(1, 5))
Gender<-factor(Gender, levels = c(0:1), labels = c("Male", "Female"))
Mark<-c(15,14,20,13,13,6,7,5,4,8,10,9,8,8,7,12,10,7,8,13)
teachingMethodData<-data.frame(Method, Gender, Mark)
teachingMethodData
write.table(teachingMethodData, "teachingMethodData.txt", sep="\t", row.names=FALSE)

----Task 3-----
Gender<-c(rep(0,12),rep(1,12)
Gender<-factor(Gender,levels=c(0:1),labels=c("Male","Female"))
Partner<-c(69,76,70,76,72,65,82,71,71,75,52,34,70,74,64,43,51,93,48,51,74,73,41,84)
Self<-c(33,26,10,51,34,28,27,9,33,11,14,46,97,80,88,100,100,58,95,83,97,89,69,82)
infidelityData<-data.frame(Gender, Partner, Self)
infidelityData
write.csv(infidelityData, "Infidelity Data.csv")


