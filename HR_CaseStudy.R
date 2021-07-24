# --------------------------------------------------------------------------------------
#                             HR CASE STUDY
# ---------------------------------------------------------------------------------------

#  Objective - 
#        : To understand what factors management should focus on in order to curb attrition.
#        : Management want to know what changes they should make to their workplace, in order to get most of their employees to stay.

#---------------------------------------------------------------------------------------

# loading required library
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(ggplot2)
library(cowplot)
library(MASS)
library(car)
library("MASS")
library(e1071)
library(caret)
library(caTools)
library(GGally)
library(ROCR)
library(ModelMetrics)


# Reading input data
general_data <- read.csv("general_data.csv", stringsAsFactors = F) 
employee_survey <- read.csv("employee_survey_data.csv", stringsAsFactors = F)
manager_survey <- read.csv("manager_survey_data.csv", stringsAsFactors = F)
in_time <- read.csv("in_time.csv", stringsAsFactors = F)
out_time <- read.csv("out_time.csv", stringsAsFactors = F)

# giving col names
colnames(in_time) [1]  <- "EmployeeID"
colnames(out_time) [1] <- "EmployeeID"

# structure of the datasets
str(in_time) # 4410 obs of 262 variables
str(out_time) # 4410 obs of 262 variables
str(employee_survey) # 4410 obs of 4 variables
str(manager_survey) # 4410 obs of 3 variables
str(general_data) # 4410 obs of 24 variables

# Checking for duplicates in employeeID field
sum(duplicated(general_data$EmployeeID))    #Return 0
sum(duplicated(employee_survey$EmployeeID)) #Return 0
sum(duplicated(manager_survey$EmployeeID))  #Return 0
sum(duplicated(in_time$EmployeeID))         #Return 0
sum(duplicated(out_time$EmployeeID))        #Return 0

# checking for NA's in different datasets
sum(is.na(general_data)) # 28 
sum(is.na(employee_survey)) # 83 
sum(is.na(manager_survey))  # 0

# Data manipulation in In time and out time
# Remove the holiday columns from "in_time" and "out_time" dataset
# exclude public holidays
in_time <- in_time[, which(sapply(in_time, function(x) sum(is.na(x))) !=4410)]
out_time <- out_time[, which(sapply(out_time, function(x) sum(is.na(x))) !=4410)]

# Convert all the date columns from character to DateTime format
in_time[,2:250] <- lapply(in_time[,2:250], function(x) as_datetime(x))
out_time[,2:250] <- lapply(out_time[,2:250], function(x) as_datetime(x))

# checking NA in in time and out time
sum(is.na(in_time))
sum(is.na(out_time))

# calculate daily Hour 
dailyHr <- out_time[,2:250] - in_time[,2:250]
dailyHrEmp <- cbind(in_time[ ,c("EmployeeID")], dailyHr)
names(dailyHrEmp)[1] <- paste("EmployeeID")
dailyHrEmp[,2:250] <- lapply(dailyHrEmp[,2:250], function(x) as.numeric(x))

#Calculaing the avg work hours of each employee
dailyHrEmp$workHrs_avg <- rowMeans(dailyHrEmp[,2:250], na.rm = TRUE)
dailyHrEmp <- dailyHrEmp[,-(2:250)]
dailyHrEmp$workHrs_avg <- round(dailyHrEmp$workHrs_avg ,2)


# checking difference in EmployeeId
setdiff(employee_survey$EmployeeID,general_data$EmployeeID)
setdiff(employee_survey$EmployeeID,manager_survey$EmployeeID)
setdiff(employee_survey$EmployeeID,dailyHrEmp$EmployeeID)

#merging data
hr_analytics <- merge(employee_survey,general_data, by="EmployeeID", all = F)
hr_analytics <- merge(hr_analytics, manager_survey, by="EmployeeID", all = F)
hr_analytics <- merge(hr_analytics, dailyHrEmp, by= "EmployeeID", all = F)

str(hr_analytics)

# Checking NA in hr_analytics data -- EnvironmentSatisfaction- 25,
# JobSatisfaction- 20, WorkLifeBalance --38 , 
# TotalWorkingYears - 9 , NumCompaniesWorked - 19
sapply(hr_analytics, function(x) length(which(is.na(x))))

# Replacing NA in EnvironmentSatisfaction with median value
hr_analytics$EnvironmentSatisfaction[which(is.na(hr_analytics$EnvironmentSatisfaction))] <- median(hr_analytics$EnvironmentSatisfaction,na.rm = TRUE)

# Replacing NA in JobSatisfaction with median value
hr_analytics$JobSatisfaction[which(is.na(hr_analytics$JobSatisfaction))] <- median(hr_analytics$JobSatisfaction,na.rm = TRUE)

# Replacing NA in WorkLifeBalance with median value
hr_analytics$WorkLifeBalance[which(is.na(hr_analytics$WorkLifeBalance))] <- median(hr_analytics$WorkLifeBalance,na.rm = TRUE)

# Removing employee ID for NA values
emptWorkYrRemove <- c(which(is.na(hr_analytics$TotalWorkingYears)))
hr_analytics <- hr_analytics[-emptWorkYrRemove,]

empNumCompaniesWorkedRemove <- c(which(is.na(hr_analytics$NumCompaniesWorked)))
hr_analytics <- hr_analytics[-empNumCompaniesWorkedRemove,]


# checking again for NA values
sapply(hr_analytics, function(x) length(which(is.na(x))))

# checking for missing value
sapply(hr_analytics, function(x) length(which(x == "")))

# creating derived columns
# overTime -- if average work hours > 8 - 1 indicates yes while 0 = no
hr_analytics$overTime <- ifelse(hr_analytics$workHrs_avg > 8, 1, 0)


# inadequate work time - employee is working mush less than the required hours on average
hr_analytics$inadq_time <- ifelse(hr_analytics$workHrs_avg < 7, 1, 0)


# removing 3 variables from data frame which have the same value for all rows
hr_analytics$EmployeeCount <- NULL
hr_analytics$Over18 <- NULL
hr_analytics$StandardHours <- NULL

# Discrepancy in NumCompaniesWorked column
# Assuming that if Current Company is not counted in the NumCompaniesWorked
# So if the Total Working years are equal to Years at company replacing it with 0
# If the difference is 1 replacing it with 1 as there are only 1 and 0 values for Num companies worked if the difference is 1
length(which(hr_analytics$TotalWorkingYears < hr_analytics$YearsAtCompany))
hr_analytics[which(hr_analytics$TotalWorkingYears - hr_analytics$YearsAtCompany == 1),]$NumCompaniesWorked <- 1
hr_analytics[which(hr_analytics$TotalWorkingYears == hr_analytics$YearsAtCompany),]$NumCompaniesWorked <- 0


# checking data - 
unique(hr_analytics$Attrition)
unique(hr_analytics$BusinessTravel)
unique(hr_analytics$Department)
unique(hr_analytics$EducationField)
unique(hr_analytics$Gender)
unique(hr_analytics$JobRole)
unique(hr_analytics$MaritalStatus)

# checking outliers in age 
boxplot(hr_analytics$Age)  # No outlier

# checking outliers in DistanceFromHome 
boxplot(hr_analytics$DistanceFromHome) # No outlier

# checking outliers in MonthlyIncome -  outlier present but not removing outlier
quantile(hr_analytics$MonthlyIncome,seq(0,1,0.01))
boxplot(hr_analytics$MonthlyIncome) 

# checking outliers in NumCompaniesWorked - no outlier 
quantile(hr_analytics$NumCompaniesWorked,seq(0,1,0.01))
boxplot(hr_analytics$NumCompaniesWorked) 

# checking outliers in PercentSalaryHike - # No outlier
boxplot(hr_analytics$PercentSalaryHike)

# checking outliers in TrainingTimesLastYear - outlier present but not removing outlier
quantile(hr_analytics$TrainingTimesLastYear,seq(0,1,0.01))
boxplot(hr_analytics$TrainingTimesLastYear) 

# checking outliers in YearsAtCompany - outlier present but not removing outlier
quantile(hr_analytics$YearsAtCompany,seq(0,1,0.01))
boxplot(hr_analytics$YearsAtCompany) 

# checking outliers in YearsSinceLastPromotion - outlier present but not removing outlier
quantile(hr_analytics$YearsSinceLastPromotion,seq(0,1,0.01))
boxplot(hr_analytics$YearsSinceLastPromotion)

# plotting charts to understand attrition rate
bar_theme1<- theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), 
                   legend.position = "none") 

# Plot 1 - plot between Environment Satisfaction and Percentage of employees attrition
plot1 <- ggplot(hr_analytics, aes(x = factor(EnvironmentSatisfaction),fill = Attrition))
plot1 <- plot1 + geom_bar(position = "fill") + bar_theme1
plot1 <- plot1 + scale_y_continuous(labels = scales::percent_format())
plot1 <- plot1 + labs(x = "Environment Satisfaction", y = "Percentage of employees attrition")

# Plot 2 - plot between Job Satisfaction and Percentage of employees attrition
plot2 <- ggplot(hr_analytics, aes(x = factor(JobSatisfaction),fill = Attrition))
plot2 <- plot2 + geom_bar(position = "fill") + bar_theme1 
plot2 <- plot2 + scale_y_continuous(labels = scales::percent_format())
plot2 <- plot2 + labs(x = "Job Satisfaction", y = "Percentage of employees attrition")

# Plot 3 - Plot between Work Life Balance and Percentage of employees attrition
plot3 <- ggplot(hr_analytics, aes(x = factor(WorkLifeBalance),fill = Attrition))
plot3 <- plot3 + geom_bar(position = "fill") + bar_theme1
plot3 <- plot3 + scale_y_continuous(labels = scales::percent_format())
plot3 <- plot3 + labs(x = "Work Life Balance", y = "Percentage of employees attrition")

# Plot 4 - Plot between Job Involvement and Percentage of employees attrition
plot4 <- ggplot(hr_analytics, aes(x = factor(JobInvolvement),fill = Attrition))
plot4 <- plot4 + geom_bar(position = "fill") + bar_theme1
plot4 <- plot4 + scale_y_continuous(labels = scales::percent_format())
plot4 <- plot4 + labs(x = "Job Involvement", y = "Percentage of employees attrition")

# Employees with low EnvironmentSatisfaction, JobSatisfaction, WorkLifeBalance and  JobInvolvement have high attrition
plot_grid(plot1,plot2,plot3,plot4,align = "h")

# Plot 5 - Plot between Performance Rating and Percentage of employees attrition
plot5 <- ggplot(hr_analytics, aes(x = factor(PerformanceRating),fill = Attrition)) 
plot5 <- plot5 + geom_bar(position = "fill") + bar_theme1
plot5 <- plot5 + scale_y_continuous(labels = scales::percent_format())
plot5 <- plot5 + labs(x = "Performance Rating", y = "Percentage of employees attrition")

# Plot 6 - Plot between BusinessTravel and Percentage of employees attrition
plot6 <- ggplot(hr_analytics, aes(x = factor(BusinessTravel),fill = Attrition))
plot6 <- plot6 + geom_bar(position = "fill") + bar_theme1
plot6 <- plot6 + scale_y_continuous(labels = scales::percent_format())
plot6 <- plot6 + labs(x = "Business Travel", y = "Percentage of employees attrition")

# Plot 7 - Plot between Department and Percentage of employees attrition
plot7 <- ggplot(hr_analytics, aes(x = factor(Department),fill = Attrition))
plot7 <- plot7 + geom_bar(position = "fill") + bar_theme1
plot7 <- plot7 + scale_y_continuous(labels = scales::percent_format())
plot7 <- plot7 + labs(x = "Department", y = "Percentage of employees attrition")

# Plot 8 - Plot between Education and Percentage of employees attrition
plot8 <- ggplot(hr_analytics, aes(x = factor(Education),fill = Attrition))
plot8 <- plot8 + geom_bar(position = "fill") + bar_theme1
plot8 <- plot8 + scale_y_continuous(labels = scales::percent_format())
plot8 <- plot8 + labs(x = "Education", y = "Percentage of employees attrition")

# Employees who are Travelling Frequently and are from HR department have  high attrition rate
plot_grid(plot6,plot7,align = "h")  

# Employees who have education leveal as College and higher performance rating have  high attrition rate
plot_grid(plot5,plot8,align = "h")  

# Plot 9 - plot between Education Field and Percentage of employees attrition
plot9 <- ggplot(hr_analytics, aes(x = factor(EducationField),fill = Attrition))
plot9 <- plot9 + geom_bar(position = "fill") + bar_theme1
plot9 <- plot9 + scale_y_continuous(labels = scales::percent_format())
plot9 <- plot9 + labs(x = "Education Field", y = "Percentage of employees attrition")

# Plot 10 - Plot between Gender and Percentage of employees attrition
plot10 <- ggplot(hr_analytics, aes(x = factor(Gender),fill = Attrition))
plot10 <- plot10 + geom_bar(position = "fill") + bar_theme1
plot10 <- plot10 + scale_y_continuous(labels = scales::percent_format())
plot10 <- plot10 + labs(x = "Gender", y = "Percentage of employees attrition")

# Plot 11 - Plot between Job Role and Percentage of employees attrition
plot11 <- ggplot(hr_analytics, aes(x = factor(JobRole),fill = Attrition))
plot11 <- plot11 + geom_bar(position = "fill") + bar_theme1
plot11 <- plot11 + scale_y_continuous(labels = scales::percent_format())
plot11 <- plot11 + labs(x = "Job Role", y = "Percentage of employees attrition")

# Plot 12 - Plot between MaritalStatus  and Percentage of employees attrition
plot12 <- ggplot(hr_analytics, aes(x = factor(MaritalStatus),fill = Attrition))
plot12 <- plot12 + geom_bar(position = "fill") + bar_theme1
plot12 <- plot12 + scale_y_continuous(labels = scales::percent_format())
plot12 <- plot12 + labs(x = "Marital Status", y = "Percentage of employees attrition")

# Employees who are from Human Resoure education field have high attrition rate
plot_grid(plot9,plot10,align = "h")

# Employees who have Job Role as Reaseacrh Director, marital status as single have high attrition rate
plot_grid(plot11,plot12,align = "h")

# Plot 13 - Employees who are doing overTime are more likely to attrite
plot13 <- ggplot(hr_analytics, aes(x = factor(overTime), fill = Attrition))
plot13 <- plot13 + geom_bar(position = "fill") + bar_theme1
plot13 <- plot13 + scale_y_continuous(labels = scales::percent_format())
plot13 <- plot13 + labs(x = "Over Time", y = "Percentage of employees attrition")
plot13

# Plots for continous variables
basicPlot <- geom_boxplot(outlier.colour = "red", outlier.shape = 1)

# Age - Less Age employees have more attrition
ggplot(hr_analytics, aes(x = Attrition, y = Age, fill = Attrition)) + basicPlot

# DistanceFromHome - no inference
ggplot(hr_analytics, aes(x = Attrition, y = DistanceFromHome, fill = Attrition)) + basicPlot

# MonthlyIncome - no inference
ggplot(hr_analytics, aes(x = Attrition, y = MonthlyIncome, fill = Attrition)) + basicPlot

# NumCompaniesWorked - Employee worked for less number of companies has more attrition rate
ggplot(hr_analytics, aes(x = Attrition, y = NumCompaniesWorked, fill = Attrition)) + basicPlot

# PercentSalaryHike - no inference
ggplot(hr_analytics, aes(x = Attrition, y = PercentSalaryHike, fill = Attrition)) + basicPlot

# StockOptionLevel - no inference
ggplot(hr_analytics, aes(x = Attrition, y = StockOptionLevel, fill = Attrition)) + basicPlot

# TotalWorkingYears - Employee with less number of working years has more attrition rate
ggplot(hr_analytics, aes(x = Attrition, y = TotalWorkingYears, fill = Attrition)) + basicPlot

# YearsAtCompany - Employee with less number of years at current company has more attrition rate
ggplot(hr_analytics, aes(x = Attrition, y = YearsAtCompany, fill = Attrition)) + basicPlot

# YearsSinceLastPromotion - 
ggplot(hr_analytics, aes(x = Attrition, y = YearsSinceLastPromotion, fill = Attrition)) + basicPlot

# YearsWithCurrManager - Employee with less number of years with current manager has more attrition rate
ggplot(hr_analytics, aes(x = Attrition, y = YearsWithCurrManager, fill = Attrition)) + basicPlot


# data Preparation for modelling

# Converting the "Attrition,Gender attributes with 2 levels into numbers(0,1)
hr_analytics$Attrition <- ifelse(hr_analytics$Attrition == "Yes", 1,0)
hr_analytics$Gender <- ifelse(hr_analytics$Gender == "Female",1,0)

#Create a dataframe of categorical attributes with more than 2 levels
hr_analytics_fact <- hr_analytics[,c("EnvironmentSatisfaction","JobSatisfaction","WorkLifeBalance",
                                     "BusinessTravel","Department","EducationField", "Education",
                                     "JobRole","MaritalStatus","JobInvolvement","JobLevel",
                                     "PerformanceRating")]

#write.csv(hr_analytics,"HrAnalyticsClean.csv",row.names = FALSE)

#Convert categorical attributes to factors
hr_analytics_fact <- data.frame(sapply(hr_analytics_fact, function(x) factor(x)))
str(hr_analytics_fact)

#Creating dummy attributes for factor attributes
dummies <- data.frame(sapply(hr_analytics_fact, function(x)
  data.frame(model.matrix(~x-1, data = hr_analytics_fact))[,-1]))

#Removing the categorical attributes and adding the corresponding dummy attributes.
hr_analytics <- cbind(hr_analytics[,-c(2,3,4,7,8,10,11,13,14,15,25,26)], dummies)

# Scaling variables
hr_analytics$workHrs_avg <- scale(hr_analytics$workHrs_avg) 
hr_analytics$YearsWithCurrManager <- scale(hr_analytics$YearsWithCurrManager)
hr_analytics$YearsSinceLastPromotion <- scale(hr_analytics$YearsSinceLastPromotion) 
hr_analytics$YearsAtCompany <- scale(hr_analytics$YearsAtCompany) 
hr_analytics$TotalWorkingYears <- scale(hr_analytics$TotalWorkingYears) 
hr_analytics$PercentSalaryHike <- scale(hr_analytics$PercentSalaryHike) 
hr_analytics$NumCompaniesWorked <- scale(hr_analytics$NumCompaniesWorked) 
hr_analytics$MonthlyIncome <- scale(hr_analytics$MonthlyIncome) 
hr_analytics$DistanceFromHome <- scale(hr_analytics$DistanceFromHome) 
hr_analytics$Age <- scale(hr_analytics$Age) 
hr_analytics$StockOptionLevel <- scale(hr_analytics$StockOptionLevel) 
hr_analytics$TrainingTimesLastYear <- scale(hr_analytics$TrainingTimesLastYear) 


# remove employee id
hr_analyticsModel <- cbind(hr_analytics[,c(-1)])

# separate training and testing data
set.seed(100)
trainindices = sample(1:nrow(hr_analyticsModel), 0.7 * nrow(hr_analyticsModel))
train = hr_analyticsModel[trainindices,]
test = hr_analyticsModel[-trainindices,]

########################################################################
# Logistic Regression: 

#Initial model - AIC: 2062.9
model_1 = glm(Attrition ~ ., data = train, family = "binomial")
summary(model_1)

# Stepwise selection - AIC: 2033.7
model_2 <- stepAIC(model_1, direction = "both")
summary(model_2)

# Removing multicollinearity through VIF check
sort(vif(model_2))

# model 3 - 
model_3 = glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                TrainingTimesLastYear + YearsAtCompany + YearsSinceLastPromotion + 
                YearsWithCurrManager + overTime + EnvironmentSatisfaction.x2 + 
                EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                EducationField.xLife.Sciences + EducationField.xMarketing + 
                EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
                Education.x2 + JobRole.xResearch.Director + JobRole.xSales.Executive + 
                MaritalStatus.xMarried + MaritalStatus.xSingle + JobInvolvement.x2 + 
                JobInvolvement.x3 + JobLevel.x2 + JobRole.xResearch.Scientist + 
                JobRole.xLaboratory.Technician, family = "binomial", data = train)
summary(model_3)

# Removing multicollinearity through VIF check
sort(vif(model_3))

# model 4 - remove EducationField.xLife.Sciences as high VIF and low significance
model_4 = glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                TrainingTimesLastYear + YearsAtCompany + YearsSinceLastPromotion + 
                YearsWithCurrManager + overTime + EnvironmentSatisfaction.x2 + 
                EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                EducationField.xMarketing + 
                EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
                Education.x2 + JobRole.xResearch.Director + JobRole.xSales.Executive + 
                MaritalStatus.xMarried + MaritalStatus.xSingle + JobInvolvement.x2 + 
                JobInvolvement.x3 + JobLevel.x2 + JobRole.xResearch.Scientist + 
                JobRole.xLaboratory.Technician, family = "binomial", data = train)
summary(model_4)

# Removing multicollinearity through VIF check
sort(vif(model_4))


# model 5 - remove YearsAtCompany as high VIF and low significance
model_5 = glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                TrainingTimesLastYear  + YearsSinceLastPromotion + 
                YearsWithCurrManager + overTime + EnvironmentSatisfaction.x2 + 
                EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                EducationField.xMarketing + 
                EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
                Education.x2 + JobRole.xResearch.Director + JobRole.xSales.Executive + 
                MaritalStatus.xMarried + MaritalStatus.xSingle + JobInvolvement.x2 + 
                JobInvolvement.x3 + JobLevel.x2 + JobRole.xResearch.Scientist + 
                JobRole.xLaboratory.Technician, family = "binomial", data = train)
summary(model_5)

# Removing multicollinearity through VIF check
sort(vif(model_5))

# model 6 - remove BusinessTravel.xTravel_Rarely as high VIF and low significance
model_6 = glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                TrainingTimesLastYear  + YearsSinceLastPromotion + 
                YearsWithCurrManager + overTime + EnvironmentSatisfaction.x2 + 
                EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                BusinessTravel.xTravel_Frequently  + 
                EducationField.xMarketing + 
                EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
                Education.x2 + JobRole.xResearch.Director + JobRole.xSales.Executive + 
                MaritalStatus.xMarried + MaritalStatus.xSingle + JobInvolvement.x2 + 
                JobInvolvement.x3 + JobLevel.x2 + JobRole.xResearch.Scientist + 
                JobRole.xLaboratory.Technician, family = "binomial", data = train)
summary(model_6)

# Removing multicollinearity through VIF check
sort(vif(model_6))

# model 7 - remove MaritalStatus.xMarried as high VIF and low significance
model_7 = glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                TrainingTimesLastYear  + YearsSinceLastPromotion + 
                YearsWithCurrManager + overTime + EnvironmentSatisfaction.x2 + 
                EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                BusinessTravel.xTravel_Frequently  + 
                EducationField.xMarketing + 
                EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
                Education.x2 + JobRole.xResearch.Director + JobRole.xSales.Executive + 
                  MaritalStatus.xSingle + JobInvolvement.x2 + 
                JobInvolvement.x3 + JobLevel.x2 + JobRole.xResearch.Scientist + 
                JobRole.xLaboratory.Technician, family = "binomial", data = train)
summary(model_7)

# Removing multicollinearity through VIF check
sort(vif(model_7))


# model 8 - remove JobInvolvement.x2 as  low significance
model_8 = glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                TrainingTimesLastYear  + YearsSinceLastPromotion + 
                YearsWithCurrManager + overTime + EnvironmentSatisfaction.x2 + 
                EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                BusinessTravel.xTravel_Frequently  + 
                EducationField.xMarketing + 
                EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
                Education.x2 + JobRole.xResearch.Director + JobRole.xSales.Executive + 
                MaritalStatus.xSingle  + 
                JobInvolvement.x3 + JobLevel.x2 + JobRole.xResearch.Scientist + 
                JobRole.xLaboratory.Technician, family = "binomial", data = train)
summary(model_8)

# Removing multicollinearity through VIF check
sort(vif(model_8))

# model 9 - remove EducationField.xOther as  low significance
model_9 = glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                TrainingTimesLastYear  + YearsSinceLastPromotion + 
                YearsWithCurrManager + overTime + EnvironmentSatisfaction.x2 + 
                EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                BusinessTravel.xTravel_Frequently  + 
                EducationField.xMarketing + 
                EducationField.xMedical  + EducationField.xTechnical.Degree + 
                Education.x2 + JobRole.xResearch.Director + JobRole.xSales.Executive + 
                MaritalStatus.xSingle  + 
                JobInvolvement.x3 + JobLevel.x2 + JobRole.xResearch.Scientist + 
                JobRole.xLaboratory.Technician, family = "binomial", data = train)
summary(model_9)

# Removing multicollinearity through VIF check
sort(vif(model_9))

# model 10 - remove EducationField.xMarketing as  low significance
model_10 = glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                TrainingTimesLastYear  + YearsSinceLastPromotion + 
                YearsWithCurrManager + overTime + EnvironmentSatisfaction.x2 + 
                EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                BusinessTravel.xTravel_Frequently  + 
                EducationField.xMedical  + EducationField.xTechnical.Degree + 
                Education.x2 + JobRole.xResearch.Director + JobRole.xSales.Executive + 
                MaritalStatus.xSingle  + 
                JobInvolvement.x3 + JobLevel.x2 + JobRole.xResearch.Scientist + 
                JobRole.xLaboratory.Technician, family = "binomial", data = train)
summary(model_10)

# Removing multicollinearity through VIF check
sort(vif(model_10))

# model 11 - remove EducationField.xMedical as  low significance
model_11 = glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear  + YearsSinceLastPromotion + 
                 YearsWithCurrManager + overTime + EnvironmentSatisfaction.x2 + 
                 EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                 JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                 WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                 BusinessTravel.xTravel_Frequently  + 
                EducationField.xTechnical.Degree + 
                 Education.x2 + JobRole.xResearch.Director + JobRole.xSales.Executive + 
                 MaritalStatus.xSingle  + 
                 JobInvolvement.x3 + JobLevel.x2 + JobRole.xResearch.Scientist + 
                 JobRole.xLaboratory.Technician, family = "binomial", data = train)
summary(model_11)

# Removing multicollinearity through VIF check
sort(vif(model_11))

# model 12 - remove JobInvolvement.x3 as  low significance
model_12 = glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear  + YearsSinceLastPromotion + 
                 YearsWithCurrManager + overTime + EnvironmentSatisfaction.x2 + 
                 EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                 JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                 WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                 BusinessTravel.xTravel_Frequently  + 
                 EducationField.xTechnical.Degree + 
                 Education.x2 + JobRole.xResearch.Director + JobRole.xSales.Executive + 
                 MaritalStatus.xSingle  + 
                JobLevel.x2 + JobRole.xResearch.Scientist + 
                 JobRole.xLaboratory.Technician, family = "binomial", data = train)
summary(model_12)

# Removing multicollinearity through VIF check
sort(vif(model_12))

# model 13 - remove Education.x2 as  low significance
model_13 = glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear  + YearsSinceLastPromotion + 
                 YearsWithCurrManager + overTime + EnvironmentSatisfaction.x2 + 
                 EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                 JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                 WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                 BusinessTravel.xTravel_Frequently  + 
                 EducationField.xTechnical.Degree + 
                JobRole.xResearch.Director + JobRole.xSales.Executive + 
                 MaritalStatus.xSingle  + 
                 JobLevel.x2 + JobRole.xResearch.Scientist + 
                 JobRole.xLaboratory.Technician, family = "binomial", data = train)
summary(model_13)

# model 14 - remove EducationField.xTechnical.Degree as  low significance
model_14 = glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear  + YearsSinceLastPromotion + 
                 YearsWithCurrManager + overTime + EnvironmentSatisfaction.x2 + 
                 EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                 JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                 WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                 BusinessTravel.xTravel_Frequently  + 
                JobRole.xResearch.Director + JobRole.xSales.Executive + 
                 MaritalStatus.xSingle  + 
                 JobLevel.x2 + JobRole.xResearch.Scientist + 
                 JobRole.xLaboratory.Technician, family = "binomial", data = train)
summary(model_14)

# model 15 - remove JobLevel.x2 as  low significance
model_15 = glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear  + YearsSinceLastPromotion + 
                 YearsWithCurrManager + overTime + EnvironmentSatisfaction.x2 + 
                 EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                 JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                 WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                 BusinessTravel.xTravel_Frequently  + 
                 JobRole.xResearch.Director + JobRole.xSales.Executive + 
                 MaritalStatus.xSingle  + JobRole.xResearch.Scientist + 
                 JobRole.xLaboratory.Technician, family = "binomial", data = train)
summary(model_15)

# Removing multicollinearity through VIF check
sort(vif(model_15))

# model 16 - remove JobRole.xLaboratory.Technician as  low significance
model_16 = glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear  + YearsSinceLastPromotion + 
                 YearsWithCurrManager + overTime + EnvironmentSatisfaction.x2 + 
                 EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                 JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                 WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                 BusinessTravel.xTravel_Frequently  + 
                 JobRole.xResearch.Director + JobRole.xSales.Executive + 
                 MaritalStatus.xSingle  + JobRole.xResearch.Scientist  
                 , family = "binomial", data = train)
summary(model_16)

# Removing multicollinearity through VIF check
sort(vif(model_16))

# model 17 - remove JobRole.xResearch.Scientist as  low significance
model_17 = glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear  + YearsSinceLastPromotion + 
                 YearsWithCurrManager + overTime + EnvironmentSatisfaction.x2 + 
                 EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                 JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                 WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                 BusinessTravel.xTravel_Frequently  + 
                 JobRole.xResearch.Director + JobRole.xSales.Executive + 
                 MaritalStatus.xSingle, family = "binomial", data = train)
summary(model_17)

# Removing multicollinearity through VIF check
sort(vif(model_17))

# model 18 - remove JobRole.xResearch.Director as  low significance
model_18 = glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear  + YearsSinceLastPromotion + 
                 YearsWithCurrManager + overTime + EnvironmentSatisfaction.x2 + 
                 EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                 JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                 WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                 BusinessTravel.xTravel_Frequently  + JobRole.xSales.Executive + 
                 MaritalStatus.xSingle, family = "binomial", data = train)
summary(model_18)

# Removing multicollinearity through VIF check
sort(vif(model_18))

# model 19 - remove JobRole.xSales.Executive as  low significance
model_19 = glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear  + YearsSinceLastPromotion + 
                 YearsWithCurrManager + overTime + EnvironmentSatisfaction.x2 + 
                 EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                 JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                 WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                 BusinessTravel.xTravel_Frequently  + 
                 MaritalStatus.xSingle, family = "binomial", data = train)
summary(model_19)

# Removing multicollinearity through VIF check
sort(vif(model_19))

# model_20 - remove TrainingTimesLastYear as  low significance
model_20 = glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                YearsSinceLastPromotion + 
                 YearsWithCurrManager + overTime + EnvironmentSatisfaction.x2 + 
                 EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                 JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
                 WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                 BusinessTravel.xTravel_Frequently  + 
                 MaritalStatus.xSingle, family = "binomial", data = train)
summary(model_20)

# Removing multicollinearity through VIF check
sort(vif(model_20))

# model_21 - remove JobSatisfaction.x2 as  low significance
model_21 = glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 YearsSinceLastPromotion + 
                 YearsWithCurrManager + overTime + EnvironmentSatisfaction.x2 + 
                 EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
                JobSatisfaction.x3 + JobSatisfaction.x4 + 
                 WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
                 BusinessTravel.xTravel_Frequently  + 
                 MaritalStatus.xSingle, family = "binomial", data = train)
summary(model_21)

# Removing multicollinearity through VIF check
sort(vif(model_21))

# Final Model - With 16 significant variables in the model
final_model <- model_21

# Model Evaluation with Test data
#predicted probabilities of attrition 1 for test data
test_pred = predict(final_model, type = "response", newdata = test[,-2])

# Let's see the summary 
summary(test_pred)
test$prob <- test_pred
#View(test)

test_actual_attrition <- factor(ifelse(test$Attrition == 1,"Yes","No"))

# probability cutoff of 50%.
# Accuracy : 0.8525 , Sensitivity : 0.24324, Specificity : 0.97621
test_predict_attrition <- factor(ifelse(test_pred >= 0.50, "Yes", "No"))
test_conf <- caret::confusionMatrix(test_predict_attrition, test_actual_attrition, positive = "Yes")
test_conf

# probability cutoff of 40%.
# Accuracy : 0.8494 , Sensitivity : 0.39640, Specificity : 0.94145 
test_predict_attrition <- factor(ifelse(test_pred >= 0.40, "Yes", "No"))
test_conf <- caret::confusionMatrix(test_predict_attrition, test_actual_attrition, positive = "Yes")
test_conf

# find out the optimal probablility cutoff 

perform_fn <- function(cutoff) 
{
  predicted_attrition <- factor(ifelse(test_pred >= cutoff, "Yes", "No"))
  conf <- caret::confusionMatrix(predicted_attrition, test_actual_attrition, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

# Summary of test probability
summary(test_pred)

s = seq(.01,.80,length=100)

OUT = matrix(0,100,3)


for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 


plot(s, OUT[,1],xlab = "Cutoff",ylab = "Value",cex.lab = 1.5,cex.axis = 1.5,ylim = c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length = 5),seq(0,1,length = 5),cex.lab = 1.5)
axis(2,seq(0,1,length = 5),seq(0,1,length = 5),cex.lab = 1.5)
lines(s,OUT[,2],col = "darkgreen",lwd = 2)
lines(s,OUT[,3],col = 4,lwd = 2)
box()
legend("topright",col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"),cex = 0.75)

cutoff <- s[which(abs(OUT[,1]-OUT[,2]) < 0.01)]

# Optimal Cutoff - 0.169596
#  Accuracy : 0.7308, Sensitivity : 0.7297, Specificity : 0.7310
test_predict_attrition<-factor(ifelse(test_pred >= cutoff, "Yes", "No"))
conf_final <- caret::confusionMatrix(test_predict_attrition, test_actual_attrition, positive = "Yes")
conf_final


# KS -statistic - Test Data 
test_predict_attrition <- ifelse(test_predict_attrition == "Yes",1,0)
test_actual_attrition <- ifelse(test_actual_attrition == "Yes",1,0)

#on testing  data
pred_object_test <- prediction(test_predict_attrition, test_actual_attrition)
performance_measures_test <- performance(pred_object_test, "tpr", "fpr")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])

max(ks_table_test) # 0.4607453 - good model as > 0.4



# Lift & Gain Chart 

# plotting the lift chart


lift <- function(labels , predicted_prob,groups=10) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups))) 
  return(gaintable)
}

attrition_decile = lift(test_actual_attrition, test_predict_attrition, groups = 10)

# Gain plot
ggplot(attrition_decile,aes(x = bucket, y = Gain)) + geom_line() + 
  geom_point(col = 'red2', size = 5) + geom_text(aes(label = round(Gain,2)),nudge_x = -0.6,nudge_y = 0.3)

# Lift plot
ggplot(attrition_decile,aes(x = bucket, y = Cumlift)) + geom_line() + 
  geom_point(col = 'red2', size = 5) + geom_text(aes(label = round(Cumlift,2)),nudge_x = 0.3,nudge_y = 0.1)



# gain is 75 % in 4th decile which   means that if we sort all employess according to probability, 
# then among the top 40% customers of this sorted list, we would find 
# 75% of all employess that were likely to leave the company.

# Lift is equal to 2.12 by the 3rd decile. 
# This means that the model's gain by the end of the 3rd decile is 2.12 times 
# that of a random model's gain at the end of 3 deciles.


# Conclusion

# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
#   (Intercept)                       -0.71771    0.23113  -3.105 0.001902 ** 
#   Age                               -0.29182    0.07996  -3.649 0.000263 ***
#   NumCompaniesWorked                 0.28737    0.06026   4.769 1.85e-06 ***
#   TotalWorkingYears                 -0.56239    0.10837  -5.189 2.11e-07 ***
#   YearsSinceLastPromotion            0.59042    0.07731   7.637 2.23e-14 ***
#   YearsWithCurrManager              -0.58249    0.08694  -6.700 2.09e-11 ***
#   overTime                           1.59288    0.11837  13.457  < 2e-16 ***
#   EnvironmentSatisfaction.x2        -0.87815    0.17126  -5.128 2.94e-07 ***
#   EnvironmentSatisfaction.x3        -1.02900    0.15742  -6.537 6.28e-11 ***
#   EnvironmentSatisfaction.x4        -1.21679    0.16255  -7.485 7.13e-14 ***
#   JobSatisfaction.x3                -0.44140    0.13246  -3.332 0.000861 ***
#   JobSatisfaction.x4                -0.97019    0.14583  -6.653 2.87e-11 ***
#   WorkLifeBalance.x2                -1.08643    0.22103  -4.915 8.87e-07 ***
#   WorkLifeBalance.x3                -1.35715    0.20464  -6.632 3.31e-11 ***
#   WorkLifeBalance.x4                -1.19145    0.25926  -4.596 4.32e-06 ***
#   BusinessTravel.xTravel_Frequently  0.87364    0.12956   6.743 1.55e-11 ***
#   MaritalStatus.xSingle              0.98784    0.11641   8.486  < 2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 2671.2  on 3066  degrees of freedom
# Residual deviance: 2046.9  on 3050  degrees of freedom
# AIC: 2080.9


# The company should focus on the following factors to curb attrition:
#   1. Less Age of employee
#   2. Less Number of companies employee has worked before joining this organisation
#   3. Less Total number of years the employee has worked so far
#   4. Higher Number of years since last promotion
#   5. Less Number of years under current manager
#   6. High Overtime done by employee
#   7. Low Work Environment Satisfaction Level
#   8. Low Job Satisfaction Level
#   9. Low Work life balance level
#   10.Frequent Business Travel
#   11.Marital status as Single





