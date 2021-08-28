library(lubridate)
library(MASS)
library(car)
library(caTools)
library(caret)
library(cowplot)
library(ggplot2)
library(corrplot) # correlation matrix plot
library(dplyr)
library(tidyr)
library(GGally)
library(ROCR)
library(Information) # IV table
library(scales) # ggplot percent display


#functions to be used to check equivalence of two vectors
#w.r.t NAs. The following compares two vectors and returns
#boolean vector of same size. It wont compare their values
#but it only checks whether an entry is NA or not, but wont
#compare the corresponding entries from two vectors for equality

#Here we assume we invoke with two vectors of same size
comparenas <- function(u,v) {
  match <- (!is.na(u) & !is.na(v)) | (is.na(u) & is.na(v));
  match[is.na(match)] <- FALSE;
  match;
}



#The following function would be used to find the mode of a vector
findmode <- function(v) {
  uniqvalues <- unique(v)
  uniqvalues <- uniqvalues[!is.na(uniqvalues)]
  uniqvalues[which.max(tabulate(match(v, uniqvalues)))]
}

setwd("C:\\work\\personal\\upgrad\\groupcasestudy\\PA-I_Case_Study_HR_Analytics")


original_general_data <- read.csv("general_data.csv", stringsAsFactors = TRUE)
original_employee_survey_data <- read.csv("employee_survey_data.csv", stringsAsFactors = TRUE)
original_manager_survey_data <- read.csv("manager_survey_data.csv", stringsAsFactors = TRUE)

in_time <- read.csv("in_time.csv", stringsAsFactors = TRUE)
out_time <- read.csv("out_time.csv", stringsAsFactors = TRUE)

#View(in_time)

#reading data
str(original_employee_survey_data)    
str(original_manager_survey_data)    
str(original_general_data)    
str(in_time)    
str(out_time)

#cheking the length of unique columns
length(unique(tolower(original_employee_survey_data$EmployeeID)))  
length(unique(tolower(original_manager_survey_data$EmployeeID))) 
length(unique(tolower(original_general_data$EmployeeID)))
length(unique(tolower(in_time$X))) 
length(unique(tolower(out_time$X))) 

#checking for setdiff so that we know all data is for same employees only
setdiff(original_employee_survey_data$EmployeeID,original_manager_survey_data$EmployeeID) 
setdiff(original_manager_survey_data$EmployeeID,original_general_data$EmployeeID) 
#setdiff(general_data$EmployeeID,AvgWorkHrs$EmployeeID)

#nrow for all looks like same (4410, i.e all have data about 4410 employees)
#now need to check if all of them occurs in each table

#We can verify the respective vectors equivalence as below
#sum(general_data$EmployeeID == manager_survey_data$EmployeeID)
#sum(general_data$EmployeeID == employee_survey_data$EmployeeID)
#sum(manager_survey_data$EmployeeID == employee_survey_data$EmployeeID)
#all of the above should result in 4410


#and also we can use the following to check
#setdiff(general_data$EmployeeID, manager_survey_data$EmployeeID)
#setdiff(general_data$EmployeeID, employee_survey_data$EmployeeID)
#setdiff(employee_survey_data$EmployeeID, manager_survey_data$EmployeeID)








#############################################
#Table(s)/Data Frame(s) :in_time and out_time
#############################################

#We have one year data of in time and out time for each employee
#we can find howmuch time each employee spent for work out of the expected
#hours for one whole year and use that metric in the model building

# approach:
# we have merged both out_time and in_time by the common Columnd (empId/X)
newOutInTime <- merge(in_time , out_time, by = "X" , all = TRUE )

# Iterate through each row for given columns to find the date/time difference - which indicates the hours spend in the office
employeesWorkingHours <- data.frame( sapply( (c1 = 2:262) , function(c1) { as_datetime(newOutInTime[, 261  + c1]) - as_datetime(newOutInTime[,c1]) } ))

# We are also calculating the average attendance for each employee in the column 'avgOI'
employeesWorkingHours$AvgWorkhrs <- sapply((c = 1 : nrow(employeesWorkingHours)) , function(c) {mean( as.double(employeesWorkingHours[c,]) , na.rm = TRUE )})


# Set all NAs to zeros
employeesWorkingHours[is.na(employeesWorkingHours)] <- 0

#View(employeesWorkingHours)

#Now we shall divide the whole work hours into quarterly work hours %
quarterOne <- employeesWorkingHours[1:64]
quarterOne <- transform(quarterOne, sum = rowSums(quarterOne))
#after removing what seems to be holidays the expected 
#work per emploee would be 480 hours in querter 1
quarterOne$q1percent <- quarterOne$sum / 480
quarterOne$EmployeeID <- seq.int(nrow(quarterOne))
quarterOneData <- quarterOne[,c("q1percent", "EmployeeID")]
#View(quarterOneData)


quarterTwo <- employeesWorkingHours[65:129]
quarterTwo <- transform(quarterTwo, sum = rowSums(quarterTwo))
#after removing what seems to be holidays the expected 
#work per emploee would be 512 hours in querter 2
quarterTwo$q2percent <- quarterTwo$sum / 512
quarterTwo$EmployeeID <- seq.int(nrow(quarterTwo))
#View(quarterTwo)
quarterTwoData <- quarterTwo[,c("q2percent", "EmployeeID")]
#View(quarterTwoData)

quarterThree <- employeesWorkingHours[130:195]
quarterThree <- transform(quarterThree, sum = rowSums(quarterThree))
#after removing what seems to be holidays the expected 
#work per emploee would be 512 hours in querter 3
quarterThree$q3percent <- quarterThree$sum / 512
quarterThree$EmployeeID <- seq.int(nrow(quarterThree))
#View(quarterThree)
quarterThreeData <- quarterThree[,c("q3percent", "EmployeeID")]
#View(quarterThreeData)


quarterFour <- employeesWorkingHours[196:261]
quarterFour <- transform(quarterFour, sum = rowSums(quarterFour))
#after removing what seems to be holidays the expected 
#work per emploee would be 488 hours in querter 4
quarterFour$q4percent <- quarterFour$sum / 488
quarterFour$EmployeeID <- seq.int(nrow(quarterFour))
#View(quarterFour)
quarterFourData <- quarterFour[,c("q4percent", "EmployeeID")]
#View(quarterFourData)

QuerterOneTwo <- merge(quarterOneData, quarterTwoData, by = "EmployeeID", all=TRUE)
QuerterOneTwoThree <- merge(QuerterOneTwo, quarterThreeData, by = "EmployeeID", all=TRUE)
QuerterOneTwoThreeFour <- merge(QuerterOneTwoThree, quarterFourData, by = "EmployeeID", all=TRUE)
#View(QuerterOneTwoThreeFour)

employeesWorkingHours$EmployeeID <- seq.int(nrow(employeesWorkingHours))
employeesAverageWorkHours <- employeesWorkingHours[,c("EmployeeID", "AvgWorkhrs")]

QuerterOneTwoThreeFourWithAvg <- merge(QuerterOneTwoThreeFour, employeesAverageWorkHours, by = "EmployeeID", all=TRUE)

#merging the quarters data with the general_data
general_data_merged_manager_data <- merge(original_general_data, QuerterOneTwoThreeFourWithAvg, by="EmployeeID", all=TRUE)

######################################
#Table/Data Frame :manager_survey_data
######################################

#This table has three columns employeeid, job involvement and performance rating
#Performance Rating = {1 is Low, 2 is Good, 3 is Excellent, 4 is Outstanding}
#Job Involvement = {1 is Low, 2 is medium, 3 is High, 4 is Very High}


#Lets merge the data set with the original_general_data
general_data_merged_manager_data <- merge(general_data_merged_manager_data, original_manager_survey_data, by="EmployeeID", all=TRUE)

#######################################
#Table/Data Frame :employee_survey_data
#######################################

#We have four columns, empId, environment satisfaction, job satisfaction and work life balance
#environment satisfaction = { 1 is high, 2 Medium, 3 High, 4 Very High}
#job satisfaction = { 1 is high, 2 Medium, 3 High, 4 Very High}
#work life balance = { 1 is bad, 2 good, 3 better, 4 best}

#Since we observe few NAs in this data we shall adopt a way to elimnate/replace wih
#some reasonable values. IF we observe all three columns (excluding Employee ID)
#there are NAs we can use th following to check the same

#unique(original_employee_survey_data$JobSatisfaction)
#unique(original_employee_survey_data$WorkLifeBalance)
#unique(original_employee_survey_data$EnvironmentSatisfaction)

#however, if we try as.factor on each of the variable/column
#we shall observe that there are 4 levels. So, we shall choose
#the mode to replace the NAs

modees <- findmode(original_employee_survey_data$EnvironmentSatisfaction)
original_employee_survey_data$EnvironmentSatisfaction[is.na(original_employee_survey_data$EnvironmentSatisfaction)] <- modees

modejs <- findmode(original_employee_survey_data$JobSatisfaction)
original_employee_survey_data$JobSatisfaction[is.na(original_employee_survey_data$JobSatisfaction)] <- modejs

modewlb <- findmode(original_employee_survey_data$WorkLifeBalance)
original_employee_survey_data$WorkLifeBalance[is.na(original_employee_survey_data$WorkLifeBalance)] <- modewlb


#Lets merge the data set with the general_data_merged_manager_data
general_data <- merge(general_data_merged_manager_data, original_employee_survey_data, by="EmployeeID", all=TRUE)


################################
#Table/Data Frame : general_data
################################


#following are the observations from the "general_data"

#OVer18 column can be ignored I suppose (as it is same for all)
#and in fact age would be a better parameter to consider than
#Over18 and also OVer18 has same value for all...so it indicates
#that should not influence the model

#Similarly Employeecount, since all the values in this column has
#1's, ideally it should not affect the end model, so we can ignore safely

#Standardhours have 8 for all employee IDs, so we can safely ignore 
#this column as its not going to affect the model


#following are the observations for "in_time" and "out_time"
#

#Looks like we can safely assume that each row in in/out time 
#corresponds to the employee whose id is same as row number

#View(general_data)

#We initially observe that NumCompaniesWorked has some NAs
#We would set them to 0's
general_data$NumCompaniesWorked[is.na(general_data$NumCompaniesWorked)] <- 0


###############################
# Now its time to conversions
###############################
# Convert categorical variables
# into numeric in the form of
# dummy variables
###############################
###############################



#*******************************
#two level categorical variables
#*******************************

# convert Gender
levels(general_data$Gender) <- c(1,0)
#then need to convert them from factor to numeric
general_data$Gender <- as.numeric(levels(general_data$Gender))[general_data$Gender]

general_data_plot <- general_data
#View(general_data)

#*********************************
#multi level categorical variables
#*********************************

####################################

# convert Business Travel
dummybt <- data.frame(model.matrix(~BusinessTravel, data=general_data))
#get rid of the first column
dummybt <- dummybt[,-1]
#combine the dummydrivewheel with the data set and remove the original drivewheel
general_data_1 <- cbind(general_data[,-which(names(general_data) %in% c("BusinessTravel"))], dummybt)

# convert Department
dummydepartments <- data.frame(model.matrix(~Department, data=general_data_1))
#get rid of the first column
dummydepartments <- dummydepartments[,-1]
#combine the dummydrivewheel with the data set and remove the original drivewheel
general_data_2 <- cbind(general_data_1[,-which(names(general_data_1) %in% c("Department"))], dummydepartments)

# convert Education Field
dummyef <- data.frame(model.matrix(~EducationField, data=general_data_2))
#get rid of the first column
dummyef <- dummyef[,-1]
#combine the dummydrivewheel with the data set and remove the original drivewheel
general_data_3 <- cbind(general_data_2[,-which(names(general_data_2) %in% c("EducationField"))], dummyef)

# convert Job Role
dummyjr <- data.frame(model.matrix(~JobRole, data=general_data_3))
#get rid of the first column
dummyjr <- dummyjr[,-1]
#combine the dummydrivewheel with the data set and remove the original drivewheel
general_data_4 <- cbind(general_data_3[,-which(names(general_data_3) %in% c("JobRole"))], dummyjr)

# convert Martial Status
dummyms <- data.frame(model.matrix(~MaritalStatus, data=general_data_4))
#get rid of the first column
dummyms <- dummyms[,-1]
#combine the dummydrivewheel with the data set and remove the original drivewheel
general_data_5 <- cbind(general_data_4[,-which(names(general_data_4) %in% c("MaritalStatus"))], dummyms)

#Remove the categorical/numerical variables which we have identified as not influential
#namely EmployeeCount, Over18, Standard Hours
general_data_6 <- general_data_5[,-which(names(general_data_5) %in% c("EmployeeCount", "Over18", "StandardHours"))]


#We also need to scale mulitple variables
#View(general_data_6)


#Now is the time to check if we need to scale any of the variables
#we can see that MonthlyIncome, Age, DistanceFromHome, YearsWithCurrManager
#YearsAtCompany, TotalWorkingYears, PercentSalaryHike etc.
general_data_6$Age <- scale(general_data_6$Age)
general_data_6$DistanceFromHome <- scale(general_data_6$DistanceFromHome)
general_data_6$MonthlyIncome <- scale(general_data_6$MonthlyIncome)
general_data_6$NumCompaniesWorked <- scale(general_data_6$NumCompaniesWorked)
general_data_6$PercentSalaryHike <- scale(general_data_6$PercentSalaryHike)
general_data_6$TotalWorkingYears <- scale(general_data_6$TotalWorkingYears)
general_data_6$TrainingTimesLastYear <- scale(general_data_6$TrainingTimesLastYear)
general_data_6$YearsAtCompany <- scale(general_data_6$YearsAtCompany)
general_data_6$YearsSinceLastPromotion <- scale(general_data_6$YearsSinceLastPromotion)
general_data_6$YearsWithCurrManager <- scale(general_data_6$YearsWithCurrManager)


#Now we have to deal with the Attrition rate categorical and turn it into 1,0
general_data_6$Attrition <- ifelse(general_data_6$Attrition == "Yes", 1, 0)

#We also need to remove EmloyeeID as it should not be part of the model building
model_base_data <- general_data_6[,-which(names(general_data_6) == "EmployeeID")]


#View(model_base_data)




#plotting

# we are converting the numeric values to categorical variables
general_data_plot$Education[which(general_data_plot$Education==1)]<-'Below College'
general_data_plot$Education[which(general_data_plot$Education==2)]<-'College'
general_data_plot$Education[which(general_data_plot$Education==3)]<-'Bachelor'
general_data_plot$Education[which(general_data_plot$Education==4)]<-'Master'
general_data_plot$Education[which(general_data_plot$Education==5)]<-'Doctor'
general_data_plot$EnvironmentSatisfaction[which(general_data_plot$EnvironmentSatisfaction==1)]<-'Low'
general_data_plot$EnvironmentSatisfaction[which(general_data_plot$EnvironmentSatisfaction==2)]<-'Medium'
general_data_plot$EnvironmentSatisfaction[which(general_data_plot$EnvironmentSatisfaction==3)]<-'High'
general_data_plot$EnvironmentSatisfaction[which(general_data_plot$EnvironmentSatisfaction==4)]<-'Very High'
general_data_plot$JobInvolvement[which(general_data_plot$JobInvolvement==1)]<-'Low'
general_data_plot$JobInvolvement[which(general_data_plot$JobInvolvement==2)]<-'Medium'
general_data_plot$JobInvolvement[which(general_data_plot$JobInvolvement==3)]<-'High'
general_data_plot$JobInvolvement[which(general_data_plot$JobInvolvement==4)]<-'Very High'
general_data_plot$JobSatisfaction[which(general_data_plot$JobSatisfaction==1)]<-'Low'
general_data_plot$JobSatisfaction[which(general_data_plot$JobSatisfaction==2)]<-'Medium'
general_data_plot$JobSatisfaction[which(general_data_plot$JobSatisfaction==3)]<-'High'
general_data_plot$JobSatisfaction[which(general_data_plot$JobSatisfaction==4)]<-'Very High'
general_data_plot$WorkLifeBalance[which(general_data_plot$WorkLifeBalance==1)]<-'Bad'
general_data_plot$WorkLifeBalance[which(general_data_plot$WorkLifeBalance==2)]<-'Good'
general_data_plot$WorkLifeBalance[which(general_data_plot$WorkLifeBalance==3)]<-'Better'
general_data_plot$WorkLifeBalance[which(general_data_plot$WorkLifeBalance==4)]<-'Best'
general_data_plot$PerformanceRating[which(general_data_plot$PerformanceRating==1)]<-'Low'
general_data_plot$PerformanceRating[which(general_data_plot$PerformanceRating==2)]<-'Good'
general_data_plot$PerformanceRating[which(general_data_plot$PerformanceRating==3)]<-'Excellent'
general_data_plot$PerformanceRating[which(general_data_plot$PerformanceRating==4)]<-'Outstanding'

# Normalising numerical variable to continuous data 
general_data_plot$YearsWithCurrManager<- scale(general_data_plot$YearsWithCurrManager)
general_data_plot$YearsSinceLastPromotion<- scale(general_data_plot$YearsSinceLastPromotion) 
general_data_plot$YearsAtCompany<- scale(general_data_plot$YearsAtCompany) 
general_data_plot$TotalWorkingYears<- scale(general_data_plot$TotalWorkingYears) 
general_data_plot$PercentSalaryHike<- scale(general_data_plot$PercentSalaryHike) 
general_data_plot$NumCompaniesWorked<- scale(general_data_plot$NumCompaniesWorked) 
general_data_plot$MonthlyIncome<- scale(general_data_plot$MonthlyIncome) 
general_data_plot$DistanceFromHome<- scale(general_data_plot$DistanceFromHome) 
general_data_plot$Age<- scale(general_data_plot$Age) 

#converting data into factors
general_data_plot$EnvironmentSatisfaction<-as.factor(general_data_plot$EnvironmentSatisfaction)
general_data_plot$Attrition<-as.factor(general_data_plot$Attrition)
general_data_plot$JobSatisfaction<-as.factor(general_data_plot$JobSatisfaction)
general_data_plot$WorkLifeBalance<-as.factor(general_data_plot$WorkLifeBalance)
general_data_plot$BusinessTravel<-as.factor(general_data_plot$BusinessTravel)
general_data_plot$Department<-as.factor(general_data_plot$Department)
general_data_plot$Education<-as.factor(general_data_plot$Education)
general_data_plot$EducationField<-as.factor(general_data_plot$EducationField)
general_data_plot$Gender<-as.factor(general_data_plot$Gender)
general_data_plot$JobLevel<-as.factor(general_data_plot$JobLevel)
general_data_plot$JobRole<-as.factor(general_data_plot$JobRole)
general_data_plot$MaritalStatus<-as.factor(general_data_plot$MaritalStatus)
general_data_plot$JobInvolvement<-as.factor(general_data_plot$JobInvolvement)
general_data_plot$PerformanceRating<-as.factor(general_data_plot$PerformanceRating)
general_data_plot$TrainingTimesLastYear<-as.factor(general_data_plot$TrainingTimesLastYear)
general_data_plot$StockOptionLevel<-as.factor(general_data_plot$StockOptionLevel)


#checking the data set again

str(general_data_plot)


#EDA , here we check the impact of categorical and continuous variables on attrition
#checking categorical variables using bar charts
bar_theme1<- theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), 
                   legend.position="none")

plot_grid(ggplot(general_data_plot, aes(x=factor(EnvironmentSatisfaction),fill=Attrition))+ geom_bar(), 
          ggplot(general_data_plot, aes(x=factor(JobSatisfaction),fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(general_data_plot, aes(x=factor(WorkLifeBalance),fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(general_data_plot, aes(x=factor(JobInvolvement),fill=Attrition))+ geom_bar()+bar_theme1,
          align = "h")   
plot_grid(ggplot(general_data_plot, aes(x=factor(PerformanceRating),fill=Attrition))+ geom_bar(), 
          ggplot(general_data_plot, aes(x=factor(BusinessTravel),fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(general_data_plot, aes(x=factor(Department),fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(general_data_plot, aes(x=factor(Education),fill=Attrition))+ geom_bar()+bar_theme1,
          align = "h")  
plot_grid(ggplot(general_data_plot, aes(x=factor(EducationField),fill=Attrition))+ geom_bar(), 
          ggplot(general_data_plot, aes(x=factor(Gender),fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(general_data_plot, aes(x=factor(JobRole),fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(general_data_plot, aes(x=factor(MaritalStatus),fill=Attrition))+ geom_bar()+bar_theme1,
          align = "h")  

# Histogram and Boxplots for numeric variables 
box_theme<- theme(axis.line=element_blank(),axis.title=element_blank(), 
                  axis.ticks=element_blank(), axis.text=element_blank())+theme(
                    panel.background = element_rect(fill = "lightblue",
                                                    colour = "lightblue",
                                                    size = 0.5, linetype = "solid"),
                    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                                    colour = "white"), 
                    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                                    colour = "white")
                  )

box_theme_y<- theme(axis.line.y=element_blank(),axis.title.y=element_blank(), 
                    axis.ticks.y=element_blank(), axis.text.y=element_blank(),
                    legend.position="none")+theme(panel.background = element_rect(fill = "lightblue",
                                                                                  colour = "lightblue",
                                                                                  size = 0.5, linetype = "solid"),
                                                  panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                                                                  colour = "white"), 
                                                  panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                                                                  colour = "white"))

plot_grid(ggplot(general_data_plot, aes(Age))+ geom_histogram(binwidth = 10),
          ggplot(general_data_plot, aes(x="",y=Age))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

plot_grid(ggplot(general_data_plot, aes(AvgWorkhrs))+ geom_histogram(binwidth = 10),
          ggplot(general_data_plot, aes(x="",y=AvgWorkhrs))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

plot_grid(ggplot(general_data_plot, aes(DistanceFromHome))+ geom_histogram(binwidth = 10),
          ggplot(general_data_plot, aes(x="",y=DistanceFromHome))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

plot_grid(ggplot(general_data_plot, aes(MonthlyIncome))+ geom_histogram(binwidth = 10),
          ggplot(general_data_plot, aes(x="",y=MonthlyIncome))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

plot_grid(ggplot(general_data_plot, aes(PercentSalaryHike))+ geom_histogram(binwidth = 10),
          ggplot(general_data_plot, aes(x="",y=PercentSalaryHike))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

plot_grid(ggplot(general_data_plot, aes(YearsAtCompany))+ geom_histogram(binwidth = 10),
          ggplot(general_data_plot, aes(x="",y=YearsAtCompany))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)


plot_grid(ggplot(general_data_plot, aes(NumCompaniesWorked))+ geom_histogram(binwidth = 10),
          ggplot(general_data_plot, aes(x="",y=NumCompaniesWorked))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

plot_grid(ggplot(general_data_plot, aes(TotalWorkingYears))+ geom_histogram(binwidth = 10),
          ggplot(general_data_plot, aes(x="",y=TotalWorkingYears))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)


plot_grid(ggplot(general_data_plot, aes(YearsSinceLastPromotion))+ geom_histogram(binwidth = 10),
          ggplot(general_data_plot, aes(x="",y=YearsSinceLastPromotion))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

plot_grid(ggplot(general_data_plot, aes(YearsWithCurrManager))+ geom_histogram(binwidth = 10),
          ggplot(general_data_plot, aes(x="",y=YearsWithCurrManager))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)




#outlier treatment for the few variables that were found from above box plots

x<-general_data_plot$MonthlyIncome
qnt <- quantile(x, probs=c(.25, .75))
H <- 1.5 * IQR(x, na.rm = TRUE)
outlier_value<-qnt[2]+H
general_data_plot$MonthlyIncome[which(general_data_plot$MonthlyIncome>outlier_value)]<-outlier_value

x<-general_data_plot$YearsAtCompany
qnt <- quantile(x, probs=c(.25, .75))
H <- 1.5 * IQR(x, na.rm = TRUE)
outlier_value<-qnt[2]+H
general_data_plot$YearsAtCompany[which(general_data_plot$YearsAtCompany>outlier_value)]<-outlier_value

x<-general_data_plot$YearsSinceLastPromotion
qnt <- quantile(x, probs=c(.25, .75))
H <- 1.5 * IQR(x, na.rm = TRUE)
outlier_value<-qnt[2]+H
general_data_plot$YearsSinceLastPromotion[which(general_data_plot$YearsSinceLastPromotion>outlier_value)]<-outlier_value


#Since we have eliminated the outliers now push this filtered
#data into our base model data object
model_base_data$MonthlyIncome <- general_data_plot$MonthlyIncome
model_base_data$YearsAtCompany <- general_data_plot$YearsAtCompany
model_base_data$YearsSinceLastPromotion <- general_data_plot$YearsSinceLastPromotion



View(model_base_data)
  
#Now compute train and test data
set.seed(100)
trainindices= sample(1:nrow(model_base_data), 0.7*nrow(model_base_data))
train = model_base_data[trainindices,]
test = model_base_data[-trainindices,]


#Now go ahead with the model building
model1 = glm(Attrition~., data = train, family = "binomial")
summary(model1)

model1stepAIC = stepAIC(model1, direction="both")
summary(model1stepAIC)

vif(model1stepAIC)

#now we can observe that AIC: 2157.3 and
#the variables AvgWorkhrs & q3percent we could see
#has very high vif, in fact AvgWorkhrs has very high VIF
#and also its p-value reasonably high, so lets drop it.

model2 = glm(formula = Attrition ~ Age + NumCompaniesWorked + StockOptionLevel + 
               TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
               YearsWithCurrManager + q3percent + q4percent +  
               JobInvolvement + EnvironmentSatisfaction + JobSatisfaction + 
               WorkLifeBalance + BusinessTravelTravel_Frequently + BusinessTravelTravel_Rarely + 
               EducationFieldLife.Sciences + EducationFieldMarketing + EducationFieldMedical + 
               EducationFieldOther + EducationFieldTechnical.Degree + JobRoleManager + 
               JobRoleManufacturing.Director + JobRoleSales.Executive + 
               MaritalStatusMarried + MaritalStatusSingle, family = "binomial", 
             data = train)

summary(model2)
vif(model2)

#We observe that AIC: 2159.3 is not much difference. Now it can be seen
#that q3percent and q4percent has very high VIF values and q3percent 
#has reasonably high p-value, so lets eliminate it and see

model3 = glm(formula = Attrition ~ Age + NumCompaniesWorked + StockOptionLevel + 
               TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
               YearsWithCurrManager + q4percent + JobInvolvement + 
               EnvironmentSatisfaction + JobSatisfaction + WorkLifeBalance + 
               BusinessTravelTravel_Frequently + BusinessTravelTravel_Rarely + 
               EducationFieldLife.Sciences + EducationFieldMarketing + EducationFieldMedical + 
               EducationFieldOther + EducationFieldTechnical.Degree + JobRoleManager + 
               JobRoleManufacturing.Director + JobRoleSales.Executive + 
               MaritalStatusMarried + MaritalStatusSingle, family = "binomial", 
             data = train)

summary(model3)
vif(model3)


#AIC: 2157.9 still remained same, so we are still good.
#however if we check VIF, we can see EducationFieldLife.Sciences
# and EducationFieldMedical are very high and if the p-value of both
#seems to be low but EducationFieldLife.Sciences higher compared to the
#other so lets try by eliminating it...

model4 = glm(formula = Attrition ~ Age + NumCompaniesWorked + StockOptionLevel + 
               TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
               YearsWithCurrManager + q4percent + JobInvolvement + EnvironmentSatisfaction + 
               JobSatisfaction + WorkLifeBalance + BusinessTravelTravel_Frequently + 
               BusinessTravelTravel_Rarely + 
               EducationFieldMarketing + EducationFieldMedical + EducationFieldOther + 
               EducationFieldTechnical.Degree + JobRoleManager + JobRoleManufacturing.Director + 
               JobRoleSales.Executive + MaritalStatusMarried + MaritalStatusSingle, 
             family = "binomial", data = train)

summary(model4)
vif(model4)


#We see increase in AIC to 2176.3 but guess not too much...so lets continue
#now we can observe that BusinessTravelTravel_Rarely and BusinessTravelTravel_Frequently
#haas very high VIF and if we see their corresponding p-value we can see 
#BusinessTravelTravel_Rarely has higher p-value so lets remove it

model5 = glm(formula = Attrition ~ Age + NumCompaniesWorked + StockOptionLevel + 
      TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
      YearsWithCurrManager + q4percent + JobInvolvement + EnvironmentSatisfaction + 
      JobSatisfaction + WorkLifeBalance + BusinessTravelTravel_Frequently + 
      EducationFieldMarketing + EducationFieldMedical + 
      EducationFieldOther + EducationFieldTechnical.Degree + JobRoleManager + 
      JobRoleManufacturing.Director + JobRoleSales.Executive + 
      MaritalStatusMarried + MaritalStatusSingle, family = "binomial", 
    data = train)

summary(model5)
vif(model5)


#Now the AIC is AIC: 2189.4 small increasse but we are still good.
#Now all VIFs are with in limits...so we shall eliminate based on p-value
#EducationFieldOther has highest p-value so lets remove it

model6 = glm(formula = Attrition ~ Age + NumCompaniesWorked + StockOptionLevel + 
               TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
               YearsWithCurrManager + q4percent + JobInvolvement + EnvironmentSatisfaction + 
               JobSatisfaction + WorkLifeBalance + BusinessTravelTravel_Frequently + 
               EducationFieldMarketing + EducationFieldMedical +  
               EducationFieldTechnical.Degree + JobRoleManager + JobRoleManufacturing.Director + 
               JobRoleSales.Executive + MaritalStatusMarried + MaritalStatusSingle, 
             family = "binomial", data = train)

summary(model6)
vif(model6)

#Now the AIC is AIC: 2189.2
#The p-value of EducationFieldMarketing seems to be on higher side
#So, lets remove it...

model7 = glm(formula = Attrition ~ Age + NumCompaniesWorked + StockOptionLevel + 
               TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
               YearsWithCurrManager + q4percent + JobInvolvement + EnvironmentSatisfaction + 
               JobSatisfaction + WorkLifeBalance + BusinessTravelTravel_Frequently + 
               EducationFieldMedical + EducationFieldTechnical.Degree + 
               JobRoleManager + JobRoleManufacturing.Director + JobRoleSales.Executive + 
               MaritalStatusMarried + MaritalStatusSingle, family = "binomial", 
             data = train)

summary(model7)
vif(model7)

#Now the AIC is AIC: 2189.3 still in limits
#But the variable EducationFieldMedical has very high p-value
#so continue the process after removing it...

model8 = glm(formula = Attrition ~ Age + NumCompaniesWorked + StockOptionLevel + 
               TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
               YearsWithCurrManager + q4percent + JobInvolvement + EnvironmentSatisfaction + 
               JobSatisfaction + WorkLifeBalance + BusinessTravelTravel_Frequently + 
               EducationFieldTechnical.Degree + 
               JobRoleManager + JobRoleManufacturing.Director + JobRoleSales.Executive + 
               MaritalStatusMarried + MaritalStatusSingle, family = "binomial", 
             data = train)

summary(model8)

#Now the AIC: 2188.9
#But theJobInvolvement seems to have high p-value
#so remove it

model9 = glm(formula = Attrition ~ Age + NumCompaniesWorked + StockOptionLevel + 
               TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
               YearsWithCurrManager + q4percent + EnvironmentSatisfaction + 
               JobSatisfaction + WorkLifeBalance + BusinessTravelTravel_Frequently + 
               EducationFieldTechnical.Degree + JobRoleManager + JobRoleManufacturing.Director + 
               JobRoleSales.Executive + MaritalStatusMarried + MaritalStatusSingle, 
             family = "binomial", data = train)

summary(model9)

#Now the AIC: 2189.3
#looks like JobRoleSales.Executive has high p-value

model10 = glm(formula = Attrition ~ Age + NumCompaniesWorked + StockOptionLevel + 
               TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
               YearsWithCurrManager + q4percent + EnvironmentSatisfaction + 
               JobSatisfaction + WorkLifeBalance + BusinessTravelTravel_Frequently + 
               EducationFieldTechnical.Degree + JobRoleManager + JobRoleManufacturing.Director + 
               MaritalStatusMarried + MaritalStatusSingle, 
             family = "binomial", data = train)

summary(model10)

#slight increase in AIC: 2190.3
#Now it looks like EducationFieldTechnical.Degree has high p-value
model11 = glm(formula = Attrition ~ Age + NumCompaniesWorked + StockOptionLevel + 
                TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                YearsWithCurrManager + q4percent + EnvironmentSatisfaction + 
                JobSatisfaction + WorkLifeBalance + BusinessTravelTravel_Frequently + 
                JobRoleManager + JobRoleManufacturing.Director + 
                MaritalStatusMarried + MaritalStatusSingle, 
              family = "binomial", data = train)

summary(model11)

#again a slight increase in AIC: 2191.6
#MaritalStatusMarried has high p-value

model12 = glm(formula = Attrition ~ Age + NumCompaniesWorked + StockOptionLevel + 
                TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                YearsWithCurrManager + q4percent + EnvironmentSatisfaction + 
                JobSatisfaction + WorkLifeBalance + BusinessTravelTravel_Frequently + 
                JobRoleManager + JobRoleManufacturing.Director + 
                MaritalStatusSingle, 
              family = "binomial", data = train)

summary(model12)


#again a slight increase in AIC: 2193.1
#StockOptionLevel has high p-value

model13 = glm(formula = Attrition ~ Age + NumCompaniesWorked +  
                TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                YearsWithCurrManager + q4percent + EnvironmentSatisfaction + 
                JobSatisfaction + WorkLifeBalance + BusinessTravelTravel_Frequently + 
                JobRoleManager + JobRoleManufacturing.Director + 
                MaritalStatusSingle, 
              family = "binomial", data = train)

summary(model13)

#again a slight increase in AIC: 2194.5
#JobRoleManager has high p-value

model14 = glm(formula = Attrition ~ Age + NumCompaniesWorked +  
                TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                YearsWithCurrManager + q4percent + EnvironmentSatisfaction + 
                JobSatisfaction + WorkLifeBalance + BusinessTravelTravel_Frequently + 
                JobRoleManufacturing.Director + 
                MaritalStatusSingle, 
              family = "binomial", data = train)

summary(model14)


#again increase in AIC: 2198
#TrainingTimesLastYear has high p-value

model15 = glm(formula = Attrition ~ Age + NumCompaniesWorked +  
                TotalWorkingYears + YearsSinceLastPromotion + 
                YearsWithCurrManager + q4percent + EnvironmentSatisfaction + 
                JobSatisfaction + WorkLifeBalance + BusinessTravelTravel_Frequently + 
                JobRoleManufacturing.Director + 
                MaritalStatusSingle, 
              family = "binomial", data = train)

summary(model15)

#Now the AIC: 2209.8
#TotalWorkingYears has higher p
model16 = glm(formula = Attrition ~ Age + NumCompaniesWorked +  
                YearsSinceLastPromotion + 
                YearsWithCurrManager + q4percent + EnvironmentSatisfaction + 
                JobSatisfaction + WorkLifeBalance + BusinessTravelTravel_Frequently + 
                JobRoleManufacturing.Director + 
                MaritalStatusSingle, 
              family = "binomial", data = train)

summary(model16)

#Now AIC: 2224.6
#WorkLifeBalance has high p-value
model17 = glm(formula = Attrition ~ Age + NumCompaniesWorked +  
                YearsSinceLastPromotion + 
                YearsWithCurrManager + q4percent + EnvironmentSatisfaction + 
                JobSatisfaction + BusinessTravelTravel_Frequently + 
                JobRoleManufacturing.Director + 
                MaritalStatusSingle, 
              family = "binomial", data = train)

summary(model17)


#At this stage we see that all have very low p-value
#so we shall evaluate this model now...

#View(test)
testDataWithoutAttrition <- test[,-2]
#View(testDataWithoutAttrition)

testPredictions <- predict(model17, type="response", newdata = testDataWithoutAttrition)

summary(testPredictions)
test$prob <- testPredictions
#View(test)
testPredictAttrition <- factor(ifelse(testPredictions >= 0.5, "Yes", "No" ))
testActualAttrition <- factor(ifelse(test$Attrition == 1, "Yes", "No"))
table(testActualAttrition, testPredictAttrition)

testPredictAttrition1 <- factor(ifelse(testPredictions >= 0.5, "Yes", "No" ))
testConf <- confusionMatrix(testPredictAttrition1, testActualAttrition, positive = "Yes")
testConf


#The model we obtained is 85.79 accurate

#So, now lets work on this futher to see if we can get it better...

testPredictAttrition1 <- factor(ifelse(testPredictions > 0.35, "Yes", "No" ))
testConf <- confusionMatrix(testPredictAttrition1, testActualAttrition, positive = "Yes")
testConf


#Now this has decreased the accuracy a little bit and 
#The Sensitivity increased but there is a decrease in Specificity
View(test)
