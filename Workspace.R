#Loading Dataset
cervical <- read.csv("/Users/robbiemead/Documents/Graduate School/Courses/Spring 2022/MATH 574/Project/Math 574 Project/cervical.csv",header = TRUE)
# ----------------------------------------------------------------------------------------------------------------------------------------------------------------#
#Part I: Understanding and Cleaning the Dataset
#Loading Packages
library(ggplot2)
library(ggpubr)
library(kableExtra)
library(dplyr)

#a.)Understanding the data
summary(cervical)
View(cervical)
#Comments:In several of the predictor variables there is a lot of missing information occurring in almost each of the predictor variables, but
#is most commonly seen in the predictor variables:
#STDS..Time.since.first.diagnosis.(almost all of the observations.)
#STDS..Time.since.last.diagnosis.(almost all of the observations.)
#These predictor variables are rendered as nonessential in the analysis and can be removed from the dataset.
#STDs is removed because it is very similiar to the STDs.number column. Virtually, having reported the same thing. 

#b.)Cleaning the Dataset
#Removing STDS..Columns because there are over 750 missing values. 
cervical <- subset(cervical,select = -c(STDs..Time.since.first.diagnosis,STDs..Time.since.last.diagnosis,STDs))

#Replacing "?'s" with NA
cervical[cervical == "?"] <- NA
colSums(is.na(cervical))
table <- as.data.frame(setNames(nm=c('Predictor Variable','Missing Values '),stack(colSums(is.na(cervical)))[2:1]))
table%>%
  kbl(caption = "Frequency Table of Missing Values from Each Explanatory Variable",
      format = "latex",col.names = c("Explanatory Variable","Count"))%>%
  kable_minimal(full_width = F)
xtable(table)

#Changing structure of dataset, so it is all numeric
cervical <- as.data.frame(sapply(cervical,as.double))
str(cervical)

#Replacing NA's in Quantitative Columns with Mean of each respective column.
cervical$Number.of.sexual.partners[is.na(cervical$Number.of.sexual.partners)] <- mean(cervical$Number.of.sexual.partners,na.rm = TRUE)
cervical$First.sexual.intercourse[is.na(cervical$First.sexual.intercourse)] <- mean(cervical$First.sexual.intercourse,na.rm = TRUE)
cervical$Num.of.pregnancies[is.na(cervical$Num.of.pregnancies)] <- mean(cervical$Num.of.pregnancies,na.rm = TRUE)
cervical$Smokes..years.[is.na(cervical$Smokes..years.)] <- mean(cervical$Smokes..years.,na.rm = TRUE)
cervical$Smokes..packs.year.[is.na(cervical$Smokes..packs.year.)] <- mean(cervical$Smokes..packs.year.,na.rm = TRUE)
cervical$Hormonal.Contraceptives[is.na(cervical$Hormonal.Contraceptives)] <- mean(cervical$Hormonal.Contraceptives,na.rm = TRUE)
cervical$Hormonal.Contraceptives..years.[is.na(cervical$Hormonal.Contraceptives..years.)] <- mean(cervical$Hormonal.Contraceptives..years.,na.rm = TRUE)
cervical$IUD..years.[is.na(cervical$IUD..years.)] <- mean(cervical$IUD..years.,na.rm = TRUE)

#Replacing NA's in Qualitative Columns with 1's  for their respective columns
cervical$Smokes[is.na(cervical$Smokes)] <- 1
cervical$IUD[is.na(cervical$IUD)] <- 1
cervical$STDs.cervical.condylomatosis[is.na(cervical$STDs.cervical.condylomatosis)] <- 1
cervical$STDs.condylomatosis[is.na(cervical$STDs.condylomatosis)] <- 1
cervical$STDs.vaginal.condylomatosis[is.na(cervical$STDs.vaginal.condylomatosis)] <- 1
cervical$STDs.vulvo.perineal.condylomatosis[is.na(cervical$STDs.vulvo.perineal.condylomatosis)] <- 1
cervical$STDs.syphilis[is.na(cervical$STDs.syphilis)] <- 1
cervical$STDs.pelvic.inflammatory.disease[is.na(cervical$STDs.pelvic.inflammatory.disease)] <- 1
cervical$STDs.genital.herpes[is.na(cervical$STDs.genital.herpes)] <- 1
cervical$STDs.molluscum.contagiosum[is.na(cervical$STDs.molluscum.contagiosum)] <- 1
cervical$STDs.AIDS[is.na(cervical$STDs.AIDS)] <- 1
cervical$STDs.HIV[is.na(cervical$STDs.HIV)] <- 1
cervical$STDs.HPV[is.na(cervical$STDs.HPV)] <- 1
cervical$STDs.Hepatitis.B[is.na(cervical$STDs.Hepatitis.B)] <- 1

colSums(is.na(cervical))
View(cervical)
#Barplot of the Qualitative Variables
ggplot(cervical,aes(x = Smokes))+geom_bar()
ggplot(cervical,aes(x = IUD))+geom_bar()
ggplot(cervical,aes(x = STDs))+geom_bar()
ggplot(cervical,aes(x = STDs.condylomatosis ))+geom_bar()
ggplot(cervical,aes(x = STDs.cervical.condylomatosis))+geom_bar()
ggplot(cervical,aes(x = STDs.vaginal.condylomatosis))+geom_bar()
ggplot(cervical,aes(x = STDs.vulvo.perineal.condylomatosis))+geom_bar()
ggplot(cervical,aes(x = STDs.syphilis))+geom_bar()
ggplot(cervical,aes(x = STDs.pelvic.inflammatory.disease))+geom_bar()
ggplot(cervical,aes(x = STDs.genital.herpes))+geom_bar()
ggplot(cervical,aes(x = STDs.molluscum.contagiosum))+geom_bar()
ggplot(cervical,aes(x = STDs.AIDS ))+geom_bar()
ggplot(cervical,aes(x = STDs.HIV))+geom_bar()
ggplot(cervical,aes(x = STDs.HPV))+geom_bar()
ggplot(cervical,aes(x = STDs.HPV))+geom_bar()
ggplot(cervical,aes(x = STDs.Hepatitis.B))+geom_bar()

# ----------------------------------------------------------------------------------------------------------------------------------------------------------------#
#Part II: Bayesian Analysis
#Loading packages
library(LearnBayes)
library(BaSTA)
library(xtable)
library(stargazer)

#a.)Prior Distribution

#b.) Regression Analysis for Dx and Posterior Samples from a Noninformative Prior
Dxfit <- lm(Dx~. -STDs.cervical.condylomatosis-STDs.AIDS-STDs.HPV ,cervical,x=TRUE,y=TRUE)
summary(Dxfit)
xtable(summary(Dxfit),caption = "Ordinary Least Squares Regression for Cervical Cancer Diagnosis")
stargazer(Dxfit,type = "latex",font.size = "small")

#Sampling 1000 Draws of the Posterior Distributions
theta.dx.sample = blinreg(Dxfit$y,Dxfit$x,1000)
head(theta.dx.sample)


#c.) Graphics
#reminder: add posterior mean, posterior median, and coefficient value(vertical line) to each graph to compare within the distribution.
#Posterior Distributions of Each Beta_i 
par(mfrow = c(3,3))
hist(theta.dx.sample$beta[,8],main = "Smokes Packs Per Year",xlab = expression(beta[1]))
hist(theta.dx.sample$beta[,10],main = "Hormonal Contraceptives Per Year",xlab = expression(beta[2]))
hist(theta.dx.sample$beta[,14],main = "Number of STDs",xlab = expression(beta[3]))
hist(theta.dx.sample$beta[,15],main = "Condylomatosis",xlab = expression(beta[4]))
hist(theta.dx.sample$beta[,16],main = "Vaginal Condylomatosis",xlab = expression(beta[5]))
hist(theta.dx.sample$beta[,17],main = "Vulvo Perineal Condylomatosis",xlab = expression(beta[6]))
hist(theta.dx.sample$beta[,18],main = "Syphilis",xlab = expression(beta[7]))
hist(theta.dx.sample$beta[,19],main = "Pelvic Inflammatory Disease",xlab = expression(beta[8]))
hist(theta.dx.sample$beta[,20],main = "Genital Herpes",xlab = expression(beta[9]))

par(mfrow = c(3,3))
hist(theta.dx.sample$beta[,21],main = "Molluscum Contagiosum",xlab = expression(beta[10]))
hist(theta.dx.sample$beta[,22],main = "HIV",xlab = expression(beta[11]))
hist(theta.dx.sample$beta[,23],main = "Hepatitis B",xlab = expression(beta[12]))
hist(theta.dx.sample$beta[,24],main = "Number of STD Diagnoses",xlab = expression(beta[13]))
hist(theta.dx.sample$beta[,25],main = "Cancer Diagnosis",xlab = expression(beta[14]))
hist(theta.dx.sample$beta[,26],main = "Precancerous Diagnosis",xlab = expression(beta[15]))
hist(theta.dx.sample$beta[,27],main = "HPV Diagnosis",xlab = expression(beta[16]))
hist(theta.dx.sample$beta[,31],main = "Citology",xlab = expression(beta[17]))
hist(theta.dx.sample$sigma,main = "Variance",xlab = expression(sigma^2))
getwd()
#Quantiles of Each Beta Distribution
#Comments:
#i.) computing 5th percentile, 50 percentile (median), and 95 percentile.
#ii.) summarizing the distribution, and comparing to original estimates. 
apply(theta.dx.sample$beta,2,quantile,c(0.05,0.5,0.95))

#Posterior Distribution of Sigma
hist(theta.dx.sample$sigma,main = "Sigma",xlab = expression(sigma))


#d.)Posterior Predictive Distribution 
pred.draws <- blinregpred(Dxfit$x,theta.dx.sample)
pred.sum <- apply(pred.draws,2,quantile,c(0.05,0.95))
#Graphics
ind <- 1:length(cervical$Dx)
matplot(rbind(ind,ind),pred.sum,type = "l",lty = 1,col = 1,
        xlab = "Index",ylab = "Diagnosis",main = "Posterior Predictive Distribution for \n Each Patient")
points(ind,cervical$Dx,pch = 10)
out <- (cervical$Dx>pred.sum[2,])
text(ind[out],cervical$Dx[out],label = cervical$Age[out],pos = 3, offset =0.25 )

