#---------------------------------------------------------------------------------------------------------
#R Code for Chapter 8 of:
#
#Field, A. P., Miles, J. N. V., & Field, Z. C. (2012). Discovering Statistics Using R: and Sex and Drugs and Rock 'N' Roll. #London Sage
#
#(c) 2011 Andy P. Field, Jeremy N. V. Miles & Zoe C. Field
#-----------------------------------------------------------------------------------------------------------




#----Set the working directory------
setwd("~/Dropbox/Team Field/DSUR/DSUR_2/DSUR2 Data Files/Chapter 08 (Logistic Regression)")
setwd("~/Documents/Academic/Data/DSU_R/Chapter 08 (Logistic Regression)")
imageDirectory<-"~/Documents/Academic/Books/Discovering Statistics/DSU R/DSU R I/DSUR I Images"

setwd("~/Public/Academic/Data/DSU_R/Chapter 08 (Logistic Regression)")
imageDirectory<-"~/Public/Academic/Books/Discovering Statistics/DSU R/DSU R I/DSUR I Images"


#----Install Packages-----
install.packages("car")
install.packages("mlogit")


#------And then load these packages, along with the boot package.-----

library(car)
library(mlogit)
library(Rcmdr)


#********************* Eel Example ********************

#load data
eelData<-read.delim("eel.dat", header = TRUE)

#look at first 6 cases of data
head(eelData)


# specify the baseline category
eelData$Cured<-relevel(eelData$Cured, "Not Cured")
eelData$Intervention<-relevel(eelData$Intervention, "No Treatment")


#Alternatively Re-orders the levels of the factyor so that Not Cured and No Treatment are the baseline categories
eelData$Cured<-factor(eelData$Cured, levels = c("Not Cured", "Cured"))
eelData$Intervention<-factor(eelData$Intervention, levels = c("No Treatment", "Intervention"))

#Create the two hierarchical models:

eelModel.1 <- glm(Cured ~ Intervention, data = eelData, family = binomial())
eelModel.2 <- glm(Cured ~ Intervention + Duration, data = eelData, family = binomial())
summary(eelModel.1)
summary(eelModel.2)

#Just to prove what the null deviance is
eelModel.0 <- glm(Cured ~ 1, data = eelData, family = binomial())
summary(eelModel.0)


modelChi <- eelModel.1$null.deviance - eelModel.1$deviance
chidf <- eelModel.1$df.null - eelModel.1$df.residual
chisq.prob <- 1 - pchisq(modelChi, chidf)modelChi; chidf; chisq.prob



R2.hl<-modelChi/eelModel.1$null.deviance
R.cs <- 1 - exp ((eelModel.1$deviance - eelModel.1$null.deviance)/113)R.n <- R.cs /( 1- ( exp (-(eelModel.1$null.deviance/ 113))))



###############################################
# This section creates a function called      #
# logisticPseudoR2s().  To use it             #
# type logisticPseudoR2s(myLogisticModel)     #
###############################################
 logisticPseudoR2s <- function(LogModel) {
	dev <- LogModel$deviance 
	nullDev <- LogModel$null.deviance 
	modelN <-  length(LogModel$fitted.values)
	R.l <-  1 -  dev / nullDev
	R.cs <- 1- exp ( -(nullDev - dev) / modelN)
	R.n <- R.cs / ( 1 - ( exp (-(nullDev / modelN))))
	cat("Pseudo R^2 for logistic regression\n")
	cat("Hosmer and Lemeshow R^2  ", round(R.l, 3), "\n")
	cat("Cox and Snell R^2        ", round(R.cs, 3), "\n")
	cat("Nagelkerke R^2           ", round(R.n, 3),    "\n")
      }
############End of function ######################

logisticPseudoR2s(eelModel.1)


#Compute odds ratio
exp(eelModel.1$coefficients)
exp(confint(eelModel.1))

#compare model1 and model 2
modelChi <- eelModel.1$deviance - eelModel.2$deviance
chidf <- eelModel.1$df.residual - eelModel.2$df.residual
chisq.prob <- 1 - pchisq(modelChi, chidf)
modelChi; chidf; chisq.prob

anova(eelModel.1, eelModel.2)


#Diagnostics for model 1

eelData$predicted.probabilities<-fitted(eelModel.1)eelData$standardized.residuals<-rstandard(eelModel.1)eelData$studentized.residuals<-rstudent(eelModel.1)eelData$dfbeta<-dfbeta(eelModel.1)eelData$dffit<-dffits(eelModel.1)eelData $leverage<-hatvalues(eelModel.1)

head(eelData[, c("Cured", "Intervention", "Duration", "predicted.probabilities")])
eelData[, c("leverage", "studentized.residuals", "dfbeta")]



#********************* Penalty Example ********************

#-----Self test-----

#load data
penaltyData<-read.delim("penalty.dat", header = TRUE)

#look at first 6 cases of data
head(penaltyData)

#Create the two hierarchical models:

penaltyModel.1 <- glm(Scored ~ Previous + PSWQ, data = penaltyData, family = binomial())
penaltyModel.2 <- glm(Scored ~ Previous + PSWQ + Anxious, data = penaltyData, family = binomial())
#This works too:
penaltyModel.2 <- update(penaltyModel.1, .~. + Anxious)


summary(penaltyModel.1)
summary(penaltyModel.2)

#Compute model 1 improvement
modelChi <- penaltyModel.1$null.deviance - penaltyModel.1$deviance
chidf <- penaltyModel.1$df.null - penaltyModel.1$df.residual
chisq.prob <- 1 - pchisq(modelChi, chidf)
modelChi; chidf; chisq.prob

#Compute model 1 R2 (remember to execute the function code first)
logisticPseudoR2s(penaltyModel.1)

#Compute odds ratio
exp(penaltyModel.1$coefficients)
exp(confint(penaltyModel.1))


#compare model1 and model 2
modelChi <- penaltyModel.1$deviance - penaltyModel.2$deviance
chidf <- penaltyModel.1$df.residual - penaltyModel.2$df.residual
chisq.prob <- 1 - pchisq(modelChi, chidf)
modelChi; chidf; chisq.prob

anova(penaltyModel.1, penaltyModel.2)


#Compute model 2 R2 (remember to execute the function code first)
logisticPseudoR2s(penaltyModel.2)

#Compute odds ratio model 2
exp(penaltyModel.2$coefficients)
exp(confint(penaltyModel.2))

#----- Testing multicollinearity ------

vif(penaltyModel.2)
1/vif(penaltyModel.2)

cor(penaltyData[, c("Previous", "PSWQ", "Anxious")])


#----- Testing the linearity of the logit ------

#Create the interaction of PSWQ with log(PSWQ)
penaltyData$logPSWQInt<-log(penaltyData$PSWQ)*penaltyData$PSWQ

#Create the interaction of Anxious and Previous with their logs

penaltyData$logAnxInt<-log(penaltyData$Anxious)*penaltyData$Anxious
penaltyData$logPrevInt<-log(penaltyData$Previous + 1)*penaltyData$Previous

head(penaltyData)



penaltyTest.1 <- glm(Scored ~ PSWQ +
					    Anxious + 
					    Previous +
					    logPSWQInt +
                        logAnxInt +	
					    logPrevInt, 
	data=penaltyData, family=binomial())
summary(penaltyTest.1)





#********************* Chat Up Lines Example ********************

chatData<-read.delim("Chat-Up Lines.dat", header = TRUE)
chatData$Gender<-relevel(chatData$Gender, ref = 2)

head(chatData)
is.factor(chatData$Success)
is.factor(chatData$Gender)

#Rearrange data
#newDataframe<-mlogit.data(oldDataFrame, choice = "text", shape = "wide"/"long")

mlChat <- mlogit.data(chatData, choice="Success", shape="wide")
head(mlChat)

#Create model:

chatModel <- mlogit(Success ~ 1 | Good_Mate + Funny + Gender + Sex + Gender:Sex +  Funny:Gender , data = mlChat, reflevel=3)
summary(chatModel)

data.frame(exp(chatModel$coefficients))
exp(confint(chatModel))



chatBase<-mlogit(Success ~ 1, data = mlChat, reflevel=3)


#-------Self test-------

#Testing for Multicolinearity---

#We need to test for Multicolinearity using the chatData dataframe:

chatData<-read.delim("Chat-Up Lines.dat", header = TRUE)
chatData$Gender<-relevel(chatData$Gender, ref = 2)

chatModel <- glm(Success ~ Funny + Good_Mate + Sex + Gender, data = chatData, family = binomial())

vif(chatModel)
1/vif(chatModel)

#Correlations
cor(chatData[, c("Funny", "Good_Mate", "Sex")])


#----- Testing the linearity of the logit ------



#Create the variables with thier logs:
mlChat$logFunny<-log(mlChat$Funny +1)
mlChat$logGood<-log(mlChat$Good_Mate +1)
mlChat$logSex<-log(mlChat$Sex + 1)

head(mlChat)

chatTest.1 <- mlogit(Success ~ 1 | Good_Mate + Funny + Sex + Funny:logFunny + Good_Mate:logGood + Sex:logSex, data = mlChat, reflevel=3)
summary(chatTest.1)


#----------------------------------Labcoat Leni--------------------------------------

#load data
suicideData<-read.delim("Lacourse et al. (2001) Females.dat", header = TRUE)

#look at first 6 cases of data
head(suicideData)

#Relevel the categorical variable Marital_Status:
suicideData$Marital_Status<-relevel(suicideData$Marital_Status, "Together")

#Create the model:
suicideModel <- glm(Suicide_Risk ~ Age + Marital_Status + Mother_Negligence + Father_Negligence + Self_Estrangement + Isolation + Normlessness + Meaninglessness + Drug_Use + Metal + Worshipping + Vicarious, data = suicideData, family = binomial())

summary(suicideModel)

#Is the model a better fit of the data?:
modelChi <- suicideModel$null.deviance - suicideModel$deviance
chidf <- suicideModel$df.null - suicideModel$df.residual
chisq.prob <- 1 - pchisq(modelChi, chidf)
modelChi; chidf; chisq.prob

#Compute odds ratio
exp(suicideModel$coefficients)
exp(confint(suicideModel))

#----------------------------------Smart Alex Task 1--------------------------
#load data
displayData<-read.delim("Display.dat", header = TRUE)

#look at first 6 cases of data
head(displayData)


#Create the two hierarchical models:

displayModel.1 <- glm(display ~ fb, data = displayData, family = binomial())

displayModel.2 <- update(displayModel.1, .~. + age + fb:age)

summary(displayModel.1)
summary(displayModel.2)


modelChi <- displayModel.1$null.deviance - displayModel.1$deviance
chidf <- displayModel.1$df.null - displayModel.1$df.residual
chisq.prob <- 1 - pchisq(modelChi, chidf)
modelChi; chidf; chisq.prob

#Make sure that you have executed the function from the chapter first:
logisticPseudoR2s(displayModel.1)


#Compute odds ratio
exp(displayModel.1$coefficients)

#Compute confidence intervals
exp(confint(displayModel.1))

#compare model1 and model 2
modelChi <- displayModel.1$deviance - displayModel.2$deviance
chidf <- displayModel.1$df.residual - displayModel.2$df.residual
chisq.prob <- 1 - pchisq(modelChi, chidf)
modelChi; chidf; chisq.prob

anova(displayModel.1, displayModel.2)

#R2
logisticPseudoR2s(displayModel.2)

#odds ratio
exp(displayModel.2$coefficients)

#cofidence intervals
exp(confint(displayModel.2))


#Diagnostics for model 1

displayData$predicted.probabilities<-fitted(displayModel.1)
displayData$standardized.residuals<-rstandard(displayModel.1)
displayData$studentized.residuals<-rstudent(displayModel.1)
displayData$dfbeta<-dfbeta(displayModel.1)
displayData$dffit<-dffits(displayModel.1)

displayData $leverage<-hatvalues(displayModel.1)

#Predicted Probabilities:
head(displayData[, c("display", "fb", "age", "predicted.probabilities")])

#Residuals:
displayData[, c("leverage", "studentized.residuals", "dfbeta")]





#----------------------------------Smart Alex Task 2-----------------------------

#load data
burnoutData<-read.delim("Burnout.dat", header = TRUE)

#look at first 6 cases of data
head(burnoutData)

#Relevel the categorical variable burnout:
burnoutData$burnout<-relevel(burnoutData$burnout, "Not Burnt Out")


#Create the two hierarchical models:

burnoutModel.1 <- glm(burnout ~ loc + cope, data = burnoutData, family = binomial())
burnoutModel.2 <- update(burnoutModel.1, .~. + teaching + research + pastoral)

summary(burnoutModel.1)
summary(burnoutModel.2)

#Compute model 1 improvement
modelChi <- burnoutModel.1$null.deviance - burnoutModel.1$deviance
chidf <- burnoutModel.1$df.null - burnoutModel.1$df.residual
chisq.prob <- 1 - pchisq(modelChi, chidf)
modelChi; chidf; chisq.prob


#Compute model 1 R2 (remember to execute the function code first)
logisticPseudoR2s(burnoutModel.1)

#Compute odds ratio
exp(burnoutModel.1$coefficients)

#Confidence interval
exp(confint(burnoutModel.1))

#compare model1 and model 2
modelChi <- burnoutModel.1$deviance - burnoutModel.2$deviance
chidf <- burnoutModel.1$df.residual - burnoutModel.2$df.residual
chisq.prob <- 1 - pchisq(modelChi, chidf)
modelChi; chidf; chisq.prob

#Compute model 2 improvement from baseline:
modelChi <- burnoutModel.2$null.deviance - burnoutModel.2$deviance
chidf <- burnoutModel.2$df.null - burnoutModel.2$df.residual
chisq.prob <- 1 - pchisq(modelChi, chidf)
modelChi; chidf; chisq.prob

#Compute model 2 R2 (remember to execute the function code first)
logisticPseudoR2s(burnoutModel.2)

#Compute odds ratio model 2
exp(burnoutModel.2$coefficients)
exp(confint(burnoutModel.2))

#----------------------------------Smart Alex Task 3-------------

#Part A:

#load data
condomData<-read.delim("condom.dat", header = TRUE)

#look at first 6 cases of data
head(condomData)

#Relevel the categorical variables:
condomData$use<-relevel(condomData$use, "Unprotected")
condomData$gender<-relevel(condomData$gender, "Male")
condomData$previous<-relevel(condomData$previous, "No Condom")

#Create the two hierarchical models:
condomModel.1 <- glm(use ~ perceive + safety + gender, data = condomData, family = binomial())
condomModel.2 <- update(condomModel.1, .~. + previous + selfcon + sexexp)

summary(condomModel.1)
summary(condomModel.2)

#Compute model 1 improvement
modelChi <- condomModel.1$null.deviance - condomModel.1$deviance
chidf <- condomModel.1$df.null - condomModel.1$df.residual
chisq.prob <- 1 - pchisq(modelChi, chidf)
modelChi; chidf; chisq.problogisticPseudoR2s(condomModel.1)


#Compute model 1 R2 (remember to execute the function code first)
logisticPseudoR2s(condomModel.1)

#Compute odds ratios:
exp(condomModel.1$coefficients)

#Confidence intervals
exp(confint(condomModel.1))

#compare model1 and model 2
modelChi <- condomModel.1$deviance - condomModel.2$deviance
chidf <- condomModel.1$df.residual - condomModel.2$df.residual
chisq.prob <- 1 - pchisq(modelChi, chidf)
modelChi; chidf; chisq.prob


#Compute model 2 R2 (remember to execute the function code first)
logisticPseudoR2s(condomModel.2)

#Compute odds ratio model 2
exp(condomModel.2$coefficients)
exp(confint(condomModel.2))

#Part B:Is the model reliable?:

#Multicolinearity:

vif(condomModel.2)
1/vif(condomModel.2)

#linearity of the logit ------

#Create the variables with thier logs
condomData$logsafety<-log(condomData$safety +1)
condomData$logsexexp<-log(condomData$sexexp +1)
condomData$logselfcon<-log(condomData$selfcon + 1)
condomData$logperceive<-log(condomData$perceive +1)

head(condomData)

condomTest.1 <- glm(use ~ safety + sexexp + selfcon + perceive + safety:logsafety + sexexp:logsexexp + selfcon:logselfcon + perceive:logperceive, data = condomData, family=binomial())
summary(condomTest.1)

---#Diagnostics-----
condomData$predicted.probabilities<-fitted(condomModel.2)
condomData$standardized.residuals<-rstandard(condomModel.2)
condomData$studentized.residuals<-rstudent(condomModel.2)
condomData$dfbeta<-dfbeta(condomModel.2)
condomData$dffit<-dffits(condomModel.2)
condomData $leverage<-hatvalues(condomModel.2)

#Residuals:
condomData[, c("leverage", "studentized.residuals", "dfbeta")]

Part C:

#Predicted Probabilities for participants 12, 53 and 75:
(condomData[c(12,53,75), c("use", "safety", "sexexp","selfcon", "perceive", "previous", "gender", "predicted.probabilities")])

Part D:
-4.9597+0.0027-0.965+0.3608+1.0872+0.6952+5.6934

exp(-1.9146)


