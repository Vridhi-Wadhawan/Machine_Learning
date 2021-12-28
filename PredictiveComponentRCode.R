#If p-value is less than 0.05 then accept alt and reject null.
#If p-value is more than 0.05 then accept null and reject alt.

##To save the data file in the current working directory ####################
save(EmployeeData, file="EmployeeData.RData")

##Packages and Data File Activation ---------------------------------------

#Package Activation
library(DescTools) #For Mode
library(dplyr) #For Pipe Operator
library(ggplot2) #For QQ Plot
library(moments) #For Skewness & Kurtosis
library(rstatix) #For Welch ANOVA Test
library(Hmisc) #For rcorr Function
library(QuantPsyc) #For lm.beta function
library(ggpubr) #For advanced QQ Plots

#New Package and function used for extracting LM output in a CSV File
# install.packages("remotes")
# remotes::install_github("benhollomon/lmOut")
library(lmOut) #for lmout function to export LM data

#Attaching Data Files
attach(EmployeeData)

##Normality Test ----------------------------------------------------------

DataSkewness <- EmployeeData %>% 
  summarise(skewness(satisfaction_level),
            skewness(last_evaluation),
            skewness(number_project),
            skewness(average_montly_hours),
            skewness(time_spend_company),
            skewness(Log_TimeSpent),
            skewness(Inv_TimeSpent),
            skewness(Imp_timeSpend))
DataKurtosis <- EmployeeData %>% 
  summarise(kurtosis(satisfaction_level),
            kurtosis(last_evaluation),
            kurtosis(number_project),
            kurtosis(average_montly_hours),
            kurtosis(time_spend_company),
            kurtosis(Log_TimeSpent),
            kurtosis(Inv_TimeSpent),
            kurtosis(Imp_timeSpend))
as.data.frame(DataSkewness)            
as.data.frame(DataKurtosis)

write.csv(as.data.frame(DataKurtosis),
          file = "DataKurtosis.csv")
write.csv(as.data.frame(DataSkewness),
          file = "DataSkewness.csv")

##Outlier Test -------------------------------------------------------------

boxplot(satisfaction_level, main="satisfaction_level")
boxplot(last_evaluation, main="last_evaluation")
boxplot(number_project, main="number_project")
boxplot(average_montly_hours, main="average_montly_hours")
boxplot(time_spend_company, main="time_spend_company")

boxplot(average_montly_hours~ Work_accident, main="Work_accident")
boxplot(average_montly_hours~ left, main="left")
boxplot(average_montly_hours~ promotion_last_5years, main="promotion_last_5years")
boxplot(average_montly_hours~ Dept_Convert, main="Dept_Convert")
boxplot(average_montly_hours~ Salary_Convert, main="Salary_Convert")

#Outliers and High Skewness detected in Time Spent Variable
#Transformation (Log) attempted
boxplot(Log_TimeSpent, main="Log_TimeSpent")
skewness(Log_TimeSpent)
kurtosis(Log_TimeSpent)
#outliers were still there
#Transformation (Inverse) attempted
Inv_TimeSpent <- 1/(time_spend_company)
boxplot(Inv_TimeSpent, main="inv_TimeSpent")
skewness(Inv_TimeSpent)
kurtosis(Inv_TimeSpent)
#Even after Transformation, although the skewness and kurtosis values were bought down significantly
#The skewness was less in inverse compared to log
#But the outliers were still there thus, we have created a new Time Spent Variable
#With Imputed data (Median Values) for the Time Spent Variable

boxplot.stats(time_spend_company)
mean(time_spend_company)
median(time_spend_company)
Mode(time_spend_company)
Imp_timeSpend <- time_spend_company
Imp_timeSpend[Imp_timeSpend >= 6] <- 3
boxplot(Imp_timeSpend)
boxplot.stats(Imp_timeSpend)
skewness(Imp_timeSpend)
kurtosis(Imp_timeSpend)

##Tests For Linear Relation & Multicollienarity--------------------------
#Continuous Variables
CorVariableMatrix <- cbind(EmployeeData[,c(1:5,13)],Imp_timeSpend,Inv_TimeSpent)
CorResult <- rcorr(as.matrix(CorVariableMatrix))
write.csv(as.data.frame(CorResult$r),
          file = "Correlation Result R Values.csv")

write.csv(as.data.frame(CorResult$P),
          file = "Correlation Result P Values.csv")
#Categorical Variables
#2 category variables
LeveneTest(average_montly_hours,
           as.factor(Work_accident),
           center = mean)        #p=0.00019(<0.05),F=13.89
t.test(average_montly_hours~Work_accident,
       var.equal=T)             #p=0.161(>0.05),T=1.4013

LeveneTest(average_montly_hours,
           as.factor(left),
           center = mean)       #p=2.2e-16(<0.05),F=1085.8
t.test(average_montly_hours~left,
       var.equal=T)             #p=2.76e-14(<0.05),T=-7.6184

LeveneTest(average_montly_hours,
           as.factor(promotion_last_5years),
           center = mean)       #p=0.6522(>0.05),F=0.2031
t.test(average_montly_hours~promotion_last_5years,
       var.equal=F)             #p=0.5799(>0.05),T=0.55447

#2+ Categories in Variable
LeveneTest(average_montly_hours,
           as.factor(Dept_Convert),
           center = mean)       #p=0.6068(>0.05),F=0.8101
welch_anova_test(data = EmployeeData,
                average_montly_hours~Dept_Convert)   #p=0.989(>0.05),F=0.24

LeveneTest(average_montly_hours,
           as.factor(Salary_Convert),
           center = mean)       #p=0.02856(<0.05),F=3.557
aov_salary <- aov(average_montly_hours~Salary_Convert)
summary(aov_salary)             #p=0.954(>0.05),F=0.003

##Linear Regression Models--------------------------------------------------

#Model 1 -------------------------------------------------------------------
#Variables Taken: DV: Average_Monthly_Hours;
#IV: Last_Evaluation, Number_Projects, Left, Imp_TimeSpent

Model1 <- lm(average_montly_hours~
             last_evaluation+
               number_project+
               left+
               Imp_timeSpend)
summary(Model1)
lm.beta(Model1)

lmOut(Model1, file="Model1.csv", writecsv = TRUE)
plot(average_montly_hours,Model1$fitted.values)

#Model 2 -------------------------------------------------------------------
#Variables Taken: DV: Average_Monthly_Hours;
#IV: Last_Evaluation, Number_Projects, Left
Model2 <- lm(average_montly_hours~
               last_evaluation+
               number_project+
               left)
summary(Model2)
lm.beta(Model2)

lmOut(Model2, file="Model2.csv", writecsv = TRUE)
plot(average_montly_hours,Model2$fitted.values)
#Model 3 -------------------------------------------------------------------
#Variables Taken: DV: Average_Monthly_Hours;
#IV: Last_Evaluation, Number_Projects, Left, Inv_TimeSpent
Model3 <- lm(average_montly_hours~
               last_evaluation+
               number_project+
               left+
               Inv_TimeSpent)
summary(Model3)
lm.beta(Model3)

lmOut(Model3, file="Model3.csv", writecsv = TRUE)
plot(average_montly_hours,Model3$fitted.values)

##DATA REDACTION AND TESTING ---------------------------------------------------

#We have Removed the 824 rows out of the 12003 rows of data from the original data set
#this new data set has been imported under the name EmployeeData_Redacted

#since a large number of rows were removed, the assumption testing were done again.

#If p-value is less than 0.05 then accept alt and reject null.
#If p-value is more than 0.05 then accept null and reject alt.

##To save the data file in the current working directory ####################
save(EmployeeData_Redacted, file="EmployeeData_Redacted.RData")


#Attaching Data Files
attach(EmployeeData_Redacted)

##Normality Test (Redacted Data) -----------------------------------------------

DataSkewnessRedacted <- EmployeeData_Redacted %>% 
  summarise(skewness(satisfaction_level2),
            skewness(last_evaluation2),
            skewness(number_project2),
            skewness(average_montly_hours2),
            skewness(time_spend_company2),
            skewness(Log_TimeSpent2),
            skewness(Inv_TimeSpent2))
DataKurtosisRedacted <- EmployeeData_Redacted %>% 
  summarise(kurtosis(satisfaction_level2),
            kurtosis(last_evaluation2),
            kurtosis(number_project2),
            kurtosis(average_montly_hours2),
            kurtosis(time_spend_company2),
            kurtosis(Log_TimeSpent2),
            kurtosis(Inv_TimeSpent2))
as.data.frame(DataSkewnessRedacted)            
as.data.frame(DataKurtosisRedacted)

write.csv(as.data.frame(DataKurtosisRedacted),
          file = "DataKurtosisRed.csv")
write.csv(as.data.frame(DataSkewnessRedacted),
          file = "DataSkewnessRed.csv")

##Outlier Test (Redacted Data)-------------------------------------------------------------
boxplot(time_spend_company2, 
        main="Redacted time_spend_company")

#Transformation (Log) attempted
skewness(Log_TimeSpent2)
kurtosis(Log_TimeSpent2)

#Transformation (Inverse) attempted
skewness(Inv_TimeSpent2)
kurtosis(Inv_TimeSpent2)

#After Transformation, the skewness and kurtosis values were bought down significantly
#The skewness was less in log compared to inverse, whereas, kurtosis was less in inverse.

##Tests For Linear Relation & Multicollienarity (Redacted Data)--------------------------
#Continuous Variables
VariableMatrixRedacted <- cbind(EmployeeData_Redacted[,c(1:5,13)],Inv_TimeSpent2)
CorResultRedacted <- rcorr(as.matrix(VariableMatrixRedacted))
write.csv(as.data.frame(CorResultRedacted$r),
          file = "Correlation Result Redacted R Values.csv")

write.csv(as.data.frame(CorResultRedacted$P),
          file = "Correlation Result Redacted P Values.csv")
#Categorical Variables
#2 category variables
LeveneTest(average_montly_hours2,
           as.factor(Work_accident2),
           center = mean)        #p=0.000151(<0.05),F=14.369
t.test(average_montly_hours2~Work_accident2,
       var.equal=T)             #p=0.4348(>0.05),T=0.78101

LeveneTest(average_montly_hours2,
           as.factor(left2),
           center = mean)       #p=2.2e-16(<0.05),F=1230.1
t.test(average_montly_hours2~left2,
       var.equal=T)             #p=5.185e-09(<0.05),T=-5.84557

LeveneTest(average_montly_hours2,
           as.factor(promotion_last_5years2),
           center = mean)       #p=0.5717(>0.05),F=0.3199
t.test(average_montly_hours2~promotion_last_5years2,
       var.equal=F)             #p=0.3381(>0.05),T=0.9608

#2+ Categories in Variable
LeveneTest(average_montly_hours2,
           as.factor(Dept_Convert2),
           center = mean)       #p=0.5583(>0.05),F=0.8625
welch_anova_test(data = EmployeeData_Redacted,
                 average_montly_hours2~Dept_Convert2)   #p=0.909(>0.05),F=0.45

LeveneTest(average_montly_hours2,
           as.factor(Salary_Convert2),
           center = mean)       #p=0.01421(<0.05),F=4.2554
aov_salaryRedacted <- aov(average_montly_hours2~ Salary_Convert2)
summary(aov_salaryRedacted)             #p=0.925(>0.05),F=0.009

##Model Testing(Redacted Data) ---------------------------------------------------

#Model 4--------------------------------------------------------------------------
#Data: EmployeeData_Redacted
#Variables Taken: DV: Average_Monthly_Hours;
#IV: Last_Evaluation, Number_Projects, Left, TimeSpent
Model4 <- lm(average_montly_hours2 ~
               last_evaluation2 +
               number_project2 +
               left2 +
               time_spend_company2)
summary(Model4)
lm.beta(Model4)

lmOut(Model4, file="Model4.csv", writecsv = TRUE)
plot(average_montly_hours2,Model4$fitted.values)

#Model 5--------------------------------------------------------------------------
#Data: EmployeeData_Redacted
#Variables Taken: DV: Average_Monthly_Hours;
#IV: Last_Evaluation, Number_Projects, Left
Model5 <- lm(average_montly_hours2 ~ 
               last_evaluation2 +
               number_project2 +
               left2)
summary(Model5)
lm.beta(Model5)

lmOut(Model5, file="Model5.csv", writecsv = TRUE)
plot(average_montly_hours2,Model5$fitted.values)

#Model 6--------------------------------------------------------------------------
#Data: EmployeeData_Redacted
#Variables Taken: DV: Average_Monthly_Hours;
#IV: Last_Evaluation, Number_Projects, Left, Log_TimeSpent

Model6 <- lm(average_montly_hours2 ~ 
               last_evaluation2 +
               number_project2 +
               left2+
               Log_TimeSpent2)
summary(Model6)
lm.beta(Model6)

lmOut(Model6, file="Model6.csv", writecsv = TRUE)
plot(average_montly_hours2,Model6$fitted.values)

#Model 7--------------------------------------------------------------------------
#Data: EmployeeData_Redacted
#Variables Taken: DV: Average_Monthly_Hours;
#IV: Last_Evaluation, Number_Projects, Left, Inv_TimeSpent

Model7 <- lm(average_montly_hours2 ~ 
               last_evaluation2 +
               number_project2 +
               left2+
               Inv_TimeSpent2)
summary(Model7)
lm.beta(Model7)

lmOut(Model7, file="Model7.csv", writecsv = TRUE)
plot(average_montly_hours2,Model7$fitted.values)
