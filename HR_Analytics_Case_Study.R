##***************************************************************************##
##                                                                           ##
## Project        : Logistic Regression - HR Analytics case Study            ## 
## Objective      : Model the probability of attrition using logistic        ##
##                  regression to understand what factors the company        ##  
##                  management should focus on to curb attrition             ## 
## Date           : 27-May-2018                                              ##
## Version        : 1.0                                                      ##
##                                                                           ##
##***************************************************************************##

# Check and Import required libraries
options(warn = -1)
libs = c("tidyverse", "lubridate", "formattable","corrplot", "cowplot", 
         "caret", "MASS", "Information", "car", "e1071", "caTools", "ROCR")
install.lib <- libs[!libs %in% installed.packages()]
for (pkg in install.lib)
  install.packages(pkg, dependencies = T)
loadlib     <- lapply(libs, library, character.only = T) # load them
remove(list = ls())
options(warn = 0)

# library(tidyverse)
# library(lubridate)
# library(formattable)
# library(cowplot)
# library(caret)
# library(MASS)
# library(corrplot)
# library(Information)
# library(car)
# library(e1071)
# library(caTools)
# library(ROCR)

# Import input files
general     <- read.csv("general_data.csv", stringsAsFactors = F)
emp_survey  <- read.csv("employee_survey_data.csv", stringsAsFactors = F)
mngr_survey <- read.csv("manager_survey_data.csv", stringsAsFactors = F)
in_time     <- read.csv("in_time.csv", stringsAsFactors = F)
out_time    <- read.csv("out_time.csv", stringsAsFactors = F)

str(general)
glimpse(general)
str(emp_survey)
glimpse(emp_survey)
str(mngr_survey)
glimpse(mngr_survey)
# str(in_time)
glimpse(in_time)
# str(out_time)
glimpse(out_time)

##***************************************************************************##
#                                                                             #
#                            Data Understanding                               #
#                                                                             #
##***************************************************************************##
# general data  ->  Contains demographic data (such as age) and other 
#   behavioural deatils(such as travel) of the employees in a company
# emp_survey    ->  Contains survey data from employees depicting how much  
#   satsified are they with the job, the environment etc...
# mngr_survey   ->  Contains survey details from the manager about  
#   each employee on how much involved and how well they perform on the job 
# in_time       -> Contains a year worth data of the employees' office in time
#   in date time format
# in_time       -> Contains a year worth data of the employees' office out time
#   in date time format
##***************************************************************************##

##***************************************************************************##
#                                                                             #
#                           Data Merging                                      #
#                                                                             #
##***************************************************************************##

length(unique(general$EmployeeID))    # 4410 observations
length(unique(emp_survey$EmployeeID)) # 4410 observations
length(unique(mngr_survey$EmployeeID)) # 4410 observations
length(unique(in_time$X))  # 4410 observations. column "X" being EmployeeID
length(unique(out_time$X)) # 4410 observations. column "X" being EmployeeID

# Are there any differences in the Employeed IDs (Key field for merging)? 
setdiff(general$EmployeeID,emp_survey$EmployeeID) 
setdiff(general$EmployeeID,mngr_survey$EmployeeID) 
setdiff(general$EmployeeID,in_time$X) 
setdiff(general$EmployeeID,out_time$X)
# Identical EmployeeID across these datasets. So can be merged. 


# Merge input files based on Employee ID
employee <- merge(general, emp_survey, by = "EmployeeID")
employee <- merge(employee, mngr_survey, by = "EmployeeID")

# Convert In_time and Out_time data frames from wide format to long format
in_time_long  <- in_time %>% 
  gather(Date, InTime, "X2015.01.01":"X2015.12.31", na.rm = T) %>%
  dplyr::select(c(-2)) %>% 
  setNames(c("EmployeeID", "InTime"))

out_time_long <- out_time %>% 
  gather(Date, OutTime, "X2015.01.01":"X2015.12.31", na.rm = T) %>%
  dplyr::select(c(-2)) %>% 
  setNames(c("EmployeeID", "OutTime"))


# Combine intime and outtime dataframes and remove duplicate Employee ID column
# We could use merge for this. But since the count matches exactly in both 
# the dataframes and they are sorted on Employee ID we could do a direct Cbind
# Calculate the duration in office based on Intime and Outtime.
# Take the average duration for each employee and then 
# merge the duration with the master employee dataframe

employee  <- cbind(in_time_long, out_time_long)[,-3] %>%
             mutate(InTime  = ymd_hms(InTime), 
             OutTime = ymd_hms(OutTime),
             hours   = round(difftime(OutTime, InTime, 
                                  units = "hours"),2)) %>% 
            group_by(EmployeeID) %>% 
            summarise(AvgHours = as.numeric(round(mean(hours,
                                          na.rm = T),2))) %>% 
            merge(employee, by = "EmployeeID")

# Check if merged dataset looks fine 
dim(employee)                                    # 4410 obs. of  29 variables
str(employee)
glimpse(employee)
summary(employee)

# Check for duplicates after merging
length(unique(employee$EmployeeID)) != dim(employee)[1]

##***************************************************************************##
#                                                                             #
#                         Derive New variables                                #
#                                                                             #
##***************************************************************************##

# Overtime or Undertime based on the avg working hours of the employee
# If the employee works > 10 AvgHours we will classify him as working overtime
# If the employee works < 7 AvgHours we will classify him as working less time
# and if the employee works somewhere between 7 & 10 AvgHours classify as normal

employee$TimeInOffice      <-  ifelse(employee$AvgHours > 10, "Over",
                              (ifelse(employee$AvgHours < 7, "Less", "Normal")))
table(employee$TimeInOffice )


##***************************************************************************##
#                                                                             #
#                             Data Cleaning                                   #
#                                                                             #
##***************************************************************************##
# Issues identified
#  * NA values
#  * By looking at the summary, the following columns seems unnecessary
#    for our analysis since they have only 1 single constant value
#    EmployeeCount , Over18 , StandardHours
##***************************************************************************##

# Are there are NA values? 
anyNA(employee)
table(colSums(is.na(employee))) 
# 5 columns have NA Values. 
colSums(is.na(employee))[which(colSums(is.na(employee)) > 0)]
# NumCompaniesWorked       TotalWorkingYears EnvironmentSatisfaction          
#      19                       9                      25                      20 
# JobSatisfaction          WorkLifeBalance 
#      20                       38
# Lets check the % of NA values
percent(colSums(is.na(employee))[which(colSums(is.na(employee)) > 0)] /
  length(unique(employee$EmployeeID)))
# Less than 1% values. We can either ignore these or treat them. 
# But since less tahn 1% lets replace NA values with Mode
getMode <- function(x, na.rm = T) {
  xtab  <- table(x)
  xmode <- names(which(xtab == max(xtab)))
  if (length(xmode) > 1) xmode <- ">1 mode"
  return(as.numeric(xmode))
}
employee$NumCompaniesWorked[which(is.na(employee$NumCompaniesWorked))] <- 
  getMode(employee$NumCompaniesWorked)
employee$TotalWorkingYears[which(is.na(employee$TotalWorkingYears))] <-  
  getMode(employee$TotalWorkingYears)
employee$JobSatisfaction[which(is.na(employee$JobSatisfaction))] <- 
  getMode(employee$JobSatisfaction)
employee$WorkLifeBalance[which(is.na(employee$WorkLifeBalance))] <- 
  getMode(employee$WorkLifeBalance)
employee$EnvironmentSatisfaction[which(is.na(employee$EnvironmentSatisfaction))] <-  
  getMode(employee$EnvironmentSatisfaction)

# EmployeeCount column has only a constant value of 1. 
# Over18 column has only a constant value of "Y". 
# StandardHours has only a constant value of "8". 
# All these columns can be removed.
employee$EmployeeCount <-  NULL
employee$Over18        <-  NULL
employee$StandardHours <-  NULL

# Convert categorical columns to character variables
employee$Education <-  as.character(employee$Education)
employee$JobLevel <-  as.character(employee$JobLevel)
employee$StockOptionLevel <-  as.character(employee$StockOptionLevel)
employee$EnvironmentSatisfaction <-  
  as.character(employee$EnvironmentSatisfaction)
employee$JobSatisfaction <-  as.character(employee$JobSatisfaction)
employee$WorkLifeBalance <-  as.character(employee$WorkLifeBalance)
employee$JobInvolvement <-  as.character(employee$JobInvolvement)
employee$PerformanceRating <-  as.character(employee$PerformanceRating)

# Remove Employee ID  variable 
employee$EmployeeID <-  NULL

# Create Categorical variables vector 
catvarnames <- names(Filter(is.character, employee))
# Check the Categorical columns
sapply(employee[catvarnames], table)

# Create Continuous variables vector
contvarnames <- names(Filter(is.numeric, employee))
# Check the Continuous columns 
sapply(employee[contvarnames], summary)

# Converting character variables to Factors
employee <- employee %>% mutate_if(is.character,as.factor)

str(employee)
# Looks good  

##***************************************************************************##
#                                                                             #
#                                EDA                                          #
#                                                                             #
##***************************************************************************##

#####   Common Functions  ##### 
# Setting the theme of plots
plot_theme <- theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5, size = 12,face = 'bold'),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x  = element_text(size = 10),
        axis.text.y  = element_text(size = 10))

# Continuous Univariate plots 
ContUnivar <- function(yfeature, ylabel) {
  ggplot(employee, aes(x = "", y = yfeature)) +
    geom_boxplot(fill = "#F8766D", outlier.colour = "red", outlier.shape = 1) +
    stat_boxplot(geom = "errorbar", width = 0.5) +
    labs( y = ylabel, title = paste(ylabel, "Distribution")) +
    plot_theme
}

# Bivariate plots 
CatBivar <-  function(xfeature, yfeature, xlabel, ylabel) {
  as.data.frame(percent(prop.table(table(yfeature, xfeature), 2))) %>%
    ggplot(aes(x = xfeature, y = Freq,  fill = yfeature)) +
    geom_col( position = "fill" ) +
    geom_text(aes(label = Freq),
              position = position_fill(vjust = .5),  
              size = 2.5) +
    labs(x = xlabel, y = "Attrition Proportion", 
         title = paste(ylabel,"Proportion by", xlabel), fill = "Attrition") +
    plot_theme + 
    theme(legend.position = 'none')
}

# Bivariate plots
ContCatBivar <- function(xfeature, yfeature, xlabel, ylabel) {
  ggplot(employee, aes(x = xfeature, y = yfeature, fill = xfeature)) +
    geom_boxplot(outlier.colour = "red", outlier.shape = 1, show.legend = F) +
    stat_boxplot(geom = "errorbar", width = 0.5) +
    labs(x = xlabel, y = ylabel, title = paste(ylabel, "vs", xlabel)) +
    plot_theme
}

# treating outliers 
treatoutlier <- function(x) {
  x[which(x %in% boxplot.stats(x)$out)] <- 
    boxplot.stats(x)$stats[5]
  return(x)
}

##***************************************************************************##
#                                                                             #
#                         Univariate Analysis                                 #
#                        (Continuos Variables)                                #
#                                                                             #
##***************************************************************************##

plot_grid(ContUnivar(employee$AvgHours, "Employee AvgHours"),
          ContUnivar(employee$Age, "Employee Age"),
          ContUnivar(employee$DistanceFromHome, "DistanceFromHome"),
          ContUnivar(employee$MonthlyIncome, "MonthlyIncome"),
          ContUnivar(employee$NumCompaniesWorked, "NumCompaniesWorked"),
          ContUnivar(employee$PercentSalaryHike, "PercentSalaryHike"),
          ContUnivar(employee$TotalWorkingYears, "TotalWorkingYears"),
          ContUnivar(employee$TrainingTimesLastYear, "TrainingTimesLastYear"),
          ContUnivar(employee$YearsAtCompany, "YearsAtCompany"),
          ContUnivar(employee$YearsSinceLastPromotion, "YearsSinceLastPromotion"),
          ContUnivar(employee$YearsWithCurrManager, "YearsWithCurrManager"))

# Handle Outliers in AvgHours, MonthlyIncome, NumCompaniesWorked, TotalWorkingYears
#   TrainingTimesLastYear, YearsAtCompany, YearsSinceLastPromotion, 
#   YearsWithCurrManager

employee$AvgHours <- 
  treatoutlier(employee$AvgHours)
employee$MonthlyIncome <- 
  treatoutlier(employee$MonthlyIncome)
employee$NumCompaniesWorked <- 
  treatoutlier(employee$NumCompaniesWorked)
employee$TotalWorkingYears <- 
  treatoutlier(employee$TotalWorkingYears)
employee$TrainingTimesLastYear <- 
  treatoutlier(employee$TrainingTimesLastYear)
employee$YearsAtCompany <- 
  treatoutlier(employee$YearsAtCompany)
employee$YearsSinceLastPromotion <- 
  treatoutlier(employee$YearsSinceLastPromotion)
employee$YearsWithCurrManager <- 
  treatoutlier(employee$YearsWithCurrManager)

plot_grid(ContUnivar(employee$AvgHours, "Employee AvgHours"),
          ContUnivar(employee$Age, "Employee Age"),
          ContUnivar(employee$DistanceFromHome, "DistanceFromHome"),
          ContUnivar(employee$MonthlyIncome, "MonthlyIncome"),
          ContUnivar(employee$NumCompaniesWorked, "NumCompaniesWorked"),
          ContUnivar(employee$PercentSalaryHike, "PercentSalaryHike"),
          ContUnivar(employee$TotalWorkingYears, "TotalWorkingYears"),
          ContUnivar(employee$TrainingTimesLastYear, "TrainingTimesLastYear"),
          ContUnivar(employee$YearsAtCompany, "YearsAtCompany"),
          ContUnivar(employee$YearsSinceLastPromotion, "YearsSinceLastPromotion"),
          ContUnivar(employee$YearsWithCurrManager, "YearsWithCurrManager"))

# All outliers are handled

##***************************************************************************##
#                                                                             #
#                         Multivariate Analysis                               #
#                        (Categorical Variables)                              #
#                                                                             #
##***************************************************************************##

# # How does the categorical variables impact attrition? 
plot_grid((CatBivar(employee$BusinessTravel, employee$Attrition, 
                   "BusinessTravel", "Attrition") + coord_flip()),
          (CatBivar(employee$Department, employee$Attrition,
                   "Department",  "Attrition") + coord_flip()),
           CatBivar(employee$Education, employee$Attrition,
                   "Education", "Attrition"),
          (CatBivar(employee$EducationField, employee$Attrition, 
                   "EducationField", "Attrition") + coord_flip()),
          (CatBivar(employee$JobRole, employee$Attrition, 
                   "JobRole", "Attrition") +  coord_flip()),
          CatBivar(employee$Gender, employee$Attrition,
                   "Gender", "Attrition") + theme(legend.position = 'right'))

plot_grid(CatBivar(employee$JobLevel, employee$Attrition,
               "JobLevel", "Attrition"),
          CatBivar(employee$MaritalStatus, employee$Attrition,
               "MaritalStatus",  "Attrition"),
          CatBivar(employee$StockOptionLevel, employee$Attrition,
               "StockOptionLevel", "Attrition"),
          CatBivar(employee$EnvironmentSatisfaction, employee$Attrition, 
               "EnvironmentSatisfaction", "Attrition"),
          CatBivar(employee$JobSatisfaction, employee$Attrition,
               "JobSatisfaction", "Attrition"),
          CatBivar(employee$WorkLifeBalance, employee$Attrition,
               "WorkLifeBalance", "Attrition"),
          CatBivar(employee$JobInvolvement, employee$Attrition,
               "JobInvolvement", "Attrition"),
          CatBivar(employee$JobInvolvement, employee$Attrition,
                   "JobInvolvement", "Attrition"),
          CatBivar(employee$TimeInOffice, employee$Attrition,
                   "TimeInOffice", "Attrition") +
                theme(legend.position = 'right'))

### Summary 
# Attrition is higher for the following variables
# 1. Frequent BusinessTravel  
# 2. Single (Marital Status)
# 3. Low Environment Satisfaction
# 4. Low Job Satisfaction
# 5. Low Work Life balance 
# 6. Low Job Involvement
# 7. Time in office : Over time
# 8. Department :  Human Resources department
# 9. Education field :  Human Resources
# 10. JobRole : Research Director 

##***************************************************************************##
#                                                                             #
#                         Multivariate Analysis                               #
#                  (Continuous & Categorical:target Variable)               #
#                                                                             #
##***************************************************************************##

plot_grid(ContCatBivar(employee$Attrition, employee$AvgHours, 
                   "Attrition", "AvgHours"), 
          ContCatBivar(employee$Attrition, employee$Age, 
                       "Attrition", "Age"),
          ContCatBivar(employee$Attrition, employee$DistanceFromHome, 
                       "Attrition", "DistanceFromHome"),
          ContCatBivar(employee$Attrition, employee$MonthlyIncome, 
                       "Attrition", "MonthlyIncome"),
          ContCatBivar(employee$Attrition, employee$NumCompaniesWorked, 
                       "Attrition", "NumCompaniesWorked"),
          ContCatBivar(employee$Attrition, employee$PercentSalaryHike, 
                       "Attrition", "PercentSalaryHike"),
          ContCatBivar(employee$Attrition, employee$TotalWorkingYears, 
                       "Attrition", "TotalWorkingYears"),
          ContCatBivar(employee$Attrition, employee$TrainingTimesLastYear, 
                       "Attrition", "TrainingTimesLastYear"),
          ContCatBivar(employee$Attrition, employee$YearsAtCompany, 
                       "Attrition", "YearsAtCompany"),
          ContCatBivar(employee$Attrition, employee$YearsSinceLastPromotion, 
                       "Attrition", "YearsSinceLastPromotion"),
          ContCatBivar(employee$Attrition, employee$YearsWithCurrManager, 
                       "Attrition", "YearsWithCurrManager"))

### Summary 
# Attrition is higher for employees with 
# 1. More Average working hours  (over time)
# 2. Lesser Age 
# 3. Less number of total working years
# 4. Less number of years at company
# 5. less number of years with current manager
# 6. Less number of companies worked

##***************************************************************************##
#                                                                             #
#                         Multivariate Analysis                               #
#                        (Continuous Variables)                               #
#                                                                             #
##***************************************************************************##

emp_model <-  employee

emp_model$Attrition <- as.numeric(ifelse(emp_model$Attrition == "Yes" , 1, 0))

corrplot.mixed(cor(cbind(emp_model[contvarnames], emp_model$Attrition)), 
               upper = "ellipse", tl.cex = 0.40, tl.pos = 'd')

### Summary 

# 1. As expected Age & total working years are strongly correlated
# 2. Total working years shows strong correlation with YearsAtComapny

# Attrition shows +ve correlation with 
# 1. Average working hours
# Attrition shows -ve correlation with 
# 1. Age 
# 2. Total Working years
# 3. Years at Company 
# 4. Years with current Manager 
# 5. Number of companies worked

# The above confirms our Multivariate data analysis. The results are the 
# same as those which were identified as the most significant variables in 
# the previous section

##***************************************************************************##
#                                                                             #
#                         Information value                                   #
#                                                                             #
##***************************************************************************##

infoTables <- create_infotables(data = emp_model,
                                y = "Attrition",
                                bins = 10,
                                parallel = T)


plotFrame <- infoTables$Summary[order(-infoTables$Summary$IV), ]
plotFrame$Variable <- factor(plotFrame$Variable,
                             levels = plotFrame$Variable[order(-plotFrame$IV)])

ggplot(plotFrame, aes(x = Variable, y = IV)) +
  geom_bar(width = .35, stat = "identity", color = "darkblue", 
           fill = "lightblue") +
  geom_bar(data = filter(plotFrame, IV >= 0.08),
           width = .35, stat = "identity", color = "darkblue", 
           fill = "lightgreen") +
  ggtitle("Information Value") +
  theme_bw() +
  theme(plot.title = element_text(size = 10)) +
  theme(axis.text.x = element_text(angle = 90))

### Summary 

# Significant variables with high IV:
#                 Variable         IV
# 1        TotalWorkingYears 0.37644850
# 2                 AvgHours 0.37540939
# 3           YearsAtCompany 0.33667579
# 4                      Age 0.30576370
# 5     YearsWithCurrManager 0.29474725
# 6            MaritalStatus 0.21889761
# 7             TimeInOffice 0.16977557
# 8           BusinessTravel 0.12086794
# 9       NumCompaniesWorked 0.11468580
# 10 EnvironmentSatisfaction 0.09581939
# 11         JobSatisfaction 0.08874361
  
# All these variables are the same as those which were identified as the most 
# significant variables during our Exploratory data analysis. 

##***************************************************************************##
#                                                                             #
#                           Data Preparation                                  #
#                         (for Model Buidling)                                #
#                                                                             #
##***************************************************************************##


# Scaling Continuous Variables
emp_model[contvarnames] <-  sapply(emp_model[contvarnames], scale)

# Dummy Variables  for Categorical variables
emp_dummy <- dummyVars(" ~ .", data = emp_model, fullRank = T)
emp_model <- data.frame(predict(emp_dummy, newdata = emp_model))

# Employee dataset now has 58 variables

##***************************************************************************##
#                                                                             #
#                           Model Building                                    #
#                                                                             #
##***************************************************************************##

########################################################################
# splitting the data between train and test
set.seed(100)
indices = sample.split(emp_model$Attrition, SplitRatio = 0.7)
train = emp_model[indices,]
test = emp_model[!(indices),]

m1 = glm(Attrition ~., data = train, family = "binomial")
summary(m1) #AIC  2143.6.... coeff..nullDev 2728.0 ...resDev 2027.6

# Stepwise selection
m2 <- stepAIC(m1, direction = "both")
summary(m2)
# AIC: 2118.6
# Removing multicollinearity through VIF check
vif(m2)

# TimeInOffice.Over has higher VIF around 6 but the p value is significant. 
#  Hence removing  EducationField.Technical.Degree with lower P value 0.160564
m3 <- glm(formula = Attrition ~ AvgHours + Age + 
            BusinessTravel.Travel_Frequently + 
            BusinessTravel.Travel_Rarely + Department.Research...Development + 
            Department.Sales + DistanceFromHome + Education.3 + Education.4 + 
            Education.5 + EducationField.Marketing + EducationField.Other + 
            JobLevel.2 + JobRole.Laboratory.Technician + 
            JobRole.Manufacturing.Director + JobRole.Research.Director + 
            JobRole.Research.Scientist + JobRole.Sales.Executive + 
            MaritalStatus.Single + MonthlyIncome + NumCompaniesWorked + 
            StockOptionLevel.1 + TotalWorkingYears + TrainingTimesLastYear + 
            YearsSinceLastPromotion + YearsWithCurrManager + 
            EnvironmentSatisfaction.2 + EnvironmentSatisfaction.3 + 
            EnvironmentSatisfaction.4 + JobSatisfaction.2 + JobSatisfaction.3 + 
            JobSatisfaction.4 + WorkLifeBalance.2 + WorkLifeBalance.3 + 
            WorkLifeBalance.4 + JobInvolvement.3 + TimeInOffice.Normal + 
            TimeInOffice.Over, family = "binomial", data = train)

summary(m3)
# AIC : 2118.7
vif(m3)

# Remove  EducationField.Marketing   P value : 0.130543
m4 <- glm(formula = Attrition ~ AvgHours + Age + 
            BusinessTravel.Travel_Frequently + 
            BusinessTravel.Travel_Rarely + Department.Research...Development + 
            Department.Sales + DistanceFromHome + Education.3 + Education.4 + 
            Education.5 + EducationField.Other + JobLevel.2 + 
            JobRole.Laboratory.Technician + 
            JobRole.Manufacturing.Director + JobRole.Research.Director + 
            JobRole.Research.Scientist + JobRole.Sales.Executive + 
            MaritalStatus.Single + MonthlyIncome + 
            NumCompaniesWorked + StockOptionLevel.1 + 
            TotalWorkingYears + TrainingTimesLastYear + 
            YearsSinceLastPromotion + YearsWithCurrManager + 
            EnvironmentSatisfaction.2 + EnvironmentSatisfaction.3 + 
            EnvironmentSatisfaction.4 + JobSatisfaction.2 + JobSatisfaction.3 + 
            JobSatisfaction.4 + WorkLifeBalance.2 + WorkLifeBalance.3 + 
            WorkLifeBalance.4 + JobInvolvement.3 + TimeInOffice.Normal + 
            TimeInOffice.Over, family = "binomial", data = train)
summary(m4)
# AIC: 2119
vif(m4)

# Remove  JobRole.Manufacturing.Director    P value : 0.138580
m5 <- glm(formula = Attrition ~ AvgHours + Age + 
            BusinessTravel.Travel_Frequently + 
            BusinessTravel.Travel_Rarely + Department.Research...Development + 
            Department.Sales + DistanceFromHome + Education.3 + Education.4 + 
            Education.5 + EducationField.Other + JobLevel.2 + 
            JobRole.Laboratory.Technician + JobRole.Research.Director + 
            JobRole.Research.Scientist + JobRole.Sales.Executive + 
            MaritalStatus.Single + MonthlyIncome + NumCompaniesWorked + 
            StockOptionLevel.1 + TotalWorkingYears + 
            TrainingTimesLastYear + YearsSinceLastPromotion + 
            YearsWithCurrManager + EnvironmentSatisfaction.2 + 
            EnvironmentSatisfaction.3 + EnvironmentSatisfaction.4 + 
            JobSatisfaction.2 + JobSatisfaction.3 + 
            JobSatisfaction.4 + WorkLifeBalance.2 + WorkLifeBalance.3 + 
            WorkLifeBalance.4 + JobInvolvement.3 + TimeInOffice.Normal + 
            TimeInOffice.Over, family = "binomial", data = train)
summary(m5)
# AIC: 2119.3
vif(m5)


# Remove  EducationField.Other     P value : 0.136407
m6 <- glm(formula = Attrition ~ AvgHours + Age + 
            BusinessTravel.Travel_Frequently + 
            BusinessTravel.Travel_Rarely + Department.Research...Development + 
            Department.Sales + DistanceFromHome + Education.3 + Education.4 + 
            Education.5 + JobLevel.2 + 
            JobRole.Laboratory.Technician + JobRole.Research.Director + 
            JobRole.Research.Scientist + JobRole.Sales.Executive + 
            MaritalStatus.Single + MonthlyIncome + NumCompaniesWorked + 
            StockOptionLevel.1 + TotalWorkingYears + TrainingTimesLastYear + 
            YearsSinceLastPromotion + YearsWithCurrManager + 
            EnvironmentSatisfaction.2 + EnvironmentSatisfaction.3 + 
            EnvironmentSatisfaction.4 + JobSatisfaction.2 + JobSatisfaction.3 + 
            JobSatisfaction.4 + WorkLifeBalance.2 + WorkLifeBalance.3 + 
            WorkLifeBalance.4 + JobInvolvement.3 + TimeInOffice.Normal + 
            TimeInOffice.Over, family = "binomial", data = train)
summary(m6)
# AIC: 2119.7
vif(m6)

# Remove  DistanceFromHome    P value : 0.120976
m7 <- glm(formula = Attrition ~ AvgHours + Age + BusinessTravel.Travel_Frequently + 
            BusinessTravel.Travel_Rarely + Department.Research...Development + 
            Department.Sales + Education.3 + Education.4 + 
            Education.5 + JobLevel.2 + 
            JobRole.Laboratory.Technician + JobRole.Research.Director + 
            JobRole.Research.Scientist + JobRole.Sales.Executive + MaritalStatus.Single + 
            MonthlyIncome + NumCompaniesWorked + StockOptionLevel.1 + 
            TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
            YearsWithCurrManager + EnvironmentSatisfaction.2 + EnvironmentSatisfaction.3 + 
            EnvironmentSatisfaction.4 + JobSatisfaction.2 + JobSatisfaction.3 + 
            JobSatisfaction.4 + WorkLifeBalance.2 + WorkLifeBalance.3 + 
            WorkLifeBalance.4 + JobInvolvement.3 + TimeInOffice.Normal + 
            TimeInOffice.Over, family = "binomial", data = train)
summary(m7)
# AIC: 2120.1
vif(m7)

# Remove  Education.5    P value : 0.102050
m8 <- glm(formula = Attrition ~ AvgHours + Age + BusinessTravel.Travel_Frequently + 
            BusinessTravel.Travel_Rarely + Department.Research...Development + 
            Department.Sales + Education.3 + Education.4 + JobLevel.2 + 
            JobRole.Laboratory.Technician + JobRole.Research.Director + 
            JobRole.Research.Scientist + JobRole.Sales.Executive + MaritalStatus.Single + 
            MonthlyIncome + NumCompaniesWorked + StockOptionLevel.1 + 
            TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
            YearsWithCurrManager + EnvironmentSatisfaction.2 + EnvironmentSatisfaction.3 + 
            EnvironmentSatisfaction.4 + JobSatisfaction.2 + JobSatisfaction.3 + 
            JobSatisfaction.4 + WorkLifeBalance.2 + WorkLifeBalance.3 + 
            WorkLifeBalance.4 + JobInvolvement.3 + TimeInOffice.Normal + 
            TimeInOffice.Over, family = "binomial", data = train)
summary(m8)
# AIC: 2121
vif(m8)

# Remove  Education.3     P value : 0.189069
m9 <- glm(formula = Attrition ~ AvgHours + Age + BusinessTravel.Travel_Frequently + 
            BusinessTravel.Travel_Rarely + Department.Research...Development + 
            Department.Sales + Education.4 + JobLevel.2 + 
            JobRole.Laboratory.Technician + JobRole.Research.Director + 
            JobRole.Research.Scientist + JobRole.Sales.Executive + MaritalStatus.Single + 
            MonthlyIncome + NumCompaniesWorked + StockOptionLevel.1 + 
            TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
            YearsWithCurrManager + EnvironmentSatisfaction.2 + EnvironmentSatisfaction.3 + 
            EnvironmentSatisfaction.4 + JobSatisfaction.2 + JobSatisfaction.3 + 
            JobSatisfaction.4 + WorkLifeBalance.2 + WorkLifeBalance.3 + 
            WorkLifeBalance.4 + JobInvolvement.3 + TimeInOffice.Normal + 
            TimeInOffice.Over, family = "binomial", data = train)
summary(m9)
# AIC: 2120.7
vif(m9)

# Remove  Education.4     P value : 0.272868
m10 <- glm(formula = Attrition ~ AvgHours + Age + BusinessTravel.Travel_Frequently + 
             BusinessTravel.Travel_Rarely + Department.Research...Development + 
             Department.Sales + JobLevel.2 + 
             JobRole.Laboratory.Technician + JobRole.Research.Director + 
             JobRole.Research.Scientist + JobRole.Sales.Executive + MaritalStatus.Single + 
             MonthlyIncome + NumCompaniesWorked + StockOptionLevel.1 + 
             TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
             YearsWithCurrManager + EnvironmentSatisfaction.2 + EnvironmentSatisfaction.3 + 
             EnvironmentSatisfaction.4 + JobSatisfaction.2 + JobSatisfaction.3 + 
             JobSatisfaction.4 + WorkLifeBalance.2 + WorkLifeBalance.3 + 
             WorkLifeBalance.4 + JobInvolvement.3 + TimeInOffice.Normal + 
             TimeInOffice.Over, family = "binomial", data = train)
summary(m10)
# AIC: 2119.9
vif(m10)

# Remove  StockOptionLevel.1     P value : 0.114306
m11 <- glm(formula = Attrition ~ AvgHours + Age + BusinessTravel.Travel_Frequently + 
             BusinessTravel.Travel_Rarely + Department.Research...Development + 
             Department.Sales + JobLevel.2 + 
             JobRole.Laboratory.Technician + JobRole.Research.Director + 
             JobRole.Research.Scientist + JobRole.Sales.Executive + MaritalStatus.Single + 
             MonthlyIncome + NumCompaniesWorked +  
             TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
             YearsWithCurrManager + EnvironmentSatisfaction.2 + EnvironmentSatisfaction.3 + 
             EnvironmentSatisfaction.4 + JobSatisfaction.2 + JobSatisfaction.3 + 
             JobSatisfaction.4 + WorkLifeBalance.2 + WorkLifeBalance.3 + 
             WorkLifeBalance.4 + JobInvolvement.3 + TimeInOffice.Normal + 
             TimeInOffice.Over, family = "binomial", data = train)
summary(m11)
# AIC: 2120.4
vif(m11)

# Remove  MonthlyIncome     P value : 0.074977
m12 <- glm(formula = Attrition ~ AvgHours + Age + BusinessTravel.Travel_Frequently + 
             BusinessTravel.Travel_Rarely + Department.Research...Development + 
             Department.Sales + JobLevel.2 + 
             JobRole.Laboratory.Technician + JobRole.Research.Director + 
             JobRole.Research.Scientist + JobRole.Sales.Executive + 
             MaritalStatus.Single + NumCompaniesWorked + TotalWorkingYears +
             TrainingTimesLastYear + YearsSinceLastPromotion + 
             YearsWithCurrManager + EnvironmentSatisfaction.2 + 
             EnvironmentSatisfaction.3 + EnvironmentSatisfaction.4 + 
             JobSatisfaction.2 + JobSatisfaction.3 + 
             JobSatisfaction.4 + WorkLifeBalance.2 + WorkLifeBalance.3 + 
             WorkLifeBalance.4 + JobInvolvement.3 + TimeInOffice.Normal + 
             TimeInOffice.Over, family = "binomial", data = train)
summary(m12)
# AIC: 2121.6
vif(m12)

# Remove  JobInvolvement.3       P value : 0.061495
m13 <- glm(formula = Attrition ~ AvgHours + Age + BusinessTravel.Travel_Frequently + 
             BusinessTravel.Travel_Rarely + Department.Research...Development + 
             Department.Sales + JobLevel.2 + 
             JobRole.Laboratory.Technician + JobRole.Research.Director + 
             JobRole.Research.Scientist + JobRole.Sales.Executive + 
             MaritalStatus.Single + NumCompaniesWorked + TotalWorkingYears +
             TrainingTimesLastYear + YearsSinceLastPromotion + 
             YearsWithCurrManager + EnvironmentSatisfaction.2 + 
             EnvironmentSatisfaction.3 + EnvironmentSatisfaction.4 + 
             JobSatisfaction.2 + JobSatisfaction.3 + 
             JobSatisfaction.4 + WorkLifeBalance.2 + WorkLifeBalance.3 + 
             WorkLifeBalance.4 + TimeInOffice.Normal + 
             TimeInOffice.Over, family = "binomial", data = train)
summary(m13)
# AIC: 2123.1
vif(m13)

# Remove  JobRole.Laboratory.Technician       P value : 0.025679
m14 <- glm(formula = Attrition ~ AvgHours + Age + BusinessTravel.Travel_Frequently + 
             BusinessTravel.Travel_Rarely + Department.Research...Development + 
             Department.Sales + JobLevel.2 + JobRole.Research.Director + 
             JobRole.Research.Scientist + JobRole.Sales.Executive + 
             MaritalStatus.Single + NumCompaniesWorked + TotalWorkingYears +
             TrainingTimesLastYear + YearsSinceLastPromotion + 
             YearsWithCurrManager + EnvironmentSatisfaction.2 + 
             EnvironmentSatisfaction.3 + EnvironmentSatisfaction.4 + 
             JobSatisfaction.2 + JobSatisfaction.3 + 
             JobSatisfaction.4 + WorkLifeBalance.2 + WorkLifeBalance.3 + 
             WorkLifeBalance.4 + TimeInOffice.Normal + 
             TimeInOffice.Over, family = "binomial", data = train)
summary(m14)
# AIC: 2126
vif(m14)

# Remove  JobLevel.2            P value : 0.023712
m15 <- glm(formula = Attrition ~ AvgHours + Age + BusinessTravel.Travel_Frequently + 
             BusinessTravel.Travel_Rarely + Department.Research...Development + 
             Department.Sales + JobRole.Research.Director + 
             JobRole.Research.Scientist + JobRole.Sales.Executive + 
             MaritalStatus.Single + NumCompaniesWorked + TotalWorkingYears +
             TrainingTimesLastYear + YearsSinceLastPromotion + 
             YearsWithCurrManager + EnvironmentSatisfaction.2 + 
             EnvironmentSatisfaction.3 + EnvironmentSatisfaction.4 + 
             JobSatisfaction.2 + JobSatisfaction.3 + 
             JobSatisfaction.4 + WorkLifeBalance.2 + WorkLifeBalance.3 + 
             WorkLifeBalance.4 + TimeInOffice.Normal + 
             TimeInOffice.Over, family = "binomial", data = train)
summary(m15)
# AIC: 2129.1
vif(m15)

# Remove  JobRole.Research.Scientist    P value : 0.013230  
m16 <- glm(formula = Attrition ~ AvgHours + Age + BusinessTravel.Travel_Frequently + 
             BusinessTravel.Travel_Rarely + Department.Research...Development + 
             Department.Sales + JobRole.Research.Director + 
             JobRole.Sales.Executive + MaritalStatus.Single + 
             NumCompaniesWorked + TotalWorkingYears +
             TrainingTimesLastYear + YearsSinceLastPromotion + 
             YearsWithCurrManager + EnvironmentSatisfaction.2 + 
             EnvironmentSatisfaction.3 + EnvironmentSatisfaction.4 + 
             JobSatisfaction.2 + JobSatisfaction.3 + 
             JobSatisfaction.4 + WorkLifeBalance.2 + WorkLifeBalance.3 + 
             WorkLifeBalance.4 + TimeInOffice.Normal + 
             TimeInOffice.Over, family = "binomial", data = train)
summary(m16)
# AIC: 2133.1
vif(m16)

# Remove  TimeInOffice.Normal     P value : 0.012925  VIF :  2.996716  
m17 <- glm(formula = Attrition ~ AvgHours + Age + BusinessTravel.Travel_Frequently + 
             BusinessTravel.Travel_Rarely + Department.Research...Development + 
             Department.Sales + JobRole.Research.Director + 
             JobRole.Sales.Executive + MaritalStatus.Single + 
             NumCompaniesWorked + TotalWorkingYears +
             TrainingTimesLastYear + YearsSinceLastPromotion + 
             YearsWithCurrManager + EnvironmentSatisfaction.2 + 
             EnvironmentSatisfaction.3 + EnvironmentSatisfaction.4 + 
             JobSatisfaction.2 + JobSatisfaction.3 + 
             JobSatisfaction.4 + WorkLifeBalance.2 + WorkLifeBalance.3 + 
             WorkLifeBalance.4 + TimeInOffice.Over, 
             family = "binomial", data = train)
summary(m17)
# AIC: 2137.4
vif(m17)

# Remove  JobRole.Sales.Executive      P value : 0.005535   
m18 <- glm(formula = Attrition ~ AvgHours + Age + BusinessTravel.Travel_Frequently + 
             BusinessTravel.Travel_Rarely + Department.Research...Development + 
             Department.Sales + JobRole.Research.Director + 
             MaritalStatus.Single + NumCompaniesWorked + TotalWorkingYears +
             TrainingTimesLastYear + YearsSinceLastPromotion + 
             YearsWithCurrManager + EnvironmentSatisfaction.2 + 
             EnvironmentSatisfaction.3 + EnvironmentSatisfaction.4 + 
             JobSatisfaction.2 + JobSatisfaction.3 + 
             JobSatisfaction.4 + WorkLifeBalance.2 + WorkLifeBalance.3 + 
             WorkLifeBalance.4 + TimeInOffice.Over, 
           family = "binomial", data = train)
summary(m18)
# AIC: 2142.9
vif(m18)

# Remove  JobRole.Research.Director      P value : 0.008989   
m19 <- glm(formula = Attrition ~ AvgHours + Age + BusinessTravel.Travel_Frequently + 
             BusinessTravel.Travel_Rarely + Department.Research...Development + 
             Department.Sales + MaritalStatus.Single + NumCompaniesWorked + TotalWorkingYears +
             TrainingTimesLastYear + YearsSinceLastPromotion + 
             YearsWithCurrManager + EnvironmentSatisfaction.2 + 
             EnvironmentSatisfaction.3 + EnvironmentSatisfaction.4 + 
             JobSatisfaction.2 + JobSatisfaction.3 + 
             JobSatisfaction.4 + WorkLifeBalance.2 + WorkLifeBalance.3 + 
             WorkLifeBalance.4 + TimeInOffice.Over, 
           family = "binomial", data = train)
summary(m19)
# AIC: 2147.4
vif(m19)

# Remove  TrainingTimesLastYear      P value : 0.008268   
m20 <- glm(formula = Attrition ~ AvgHours + Age + BusinessTravel.Travel_Frequently + 
             BusinessTravel.Travel_Rarely + Department.Research...Development + 
             Department.Sales + MaritalStatus.Single + NumCompaniesWorked + TotalWorkingYears +
             YearsSinceLastPromotion + YearsWithCurrManager + EnvironmentSatisfaction.2 + 
             EnvironmentSatisfaction.3 + EnvironmentSatisfaction.4 + 
             JobSatisfaction.2 + JobSatisfaction.3 + 
             JobSatisfaction.4 + WorkLifeBalance.2 + WorkLifeBalance.3 + 
             WorkLifeBalance.4 + TimeInOffice.Over, 
           family = "binomial", data = train)
summary(m20)
# AIC: 2152.4
vif(m20)

# Remove  JobSatisfaction.3      P value : 0.007221   
m21 <- glm(formula = Attrition ~ AvgHours + Age + BusinessTravel.Travel_Frequently + 
             BusinessTravel.Travel_Rarely + Department.Research...Development + 
             Department.Sales + MaritalStatus.Single + NumCompaniesWorked + TotalWorkingYears +
             YearsSinceLastPromotion + YearsWithCurrManager + EnvironmentSatisfaction.2 + 
             EnvironmentSatisfaction.3 + EnvironmentSatisfaction.4 + 
             JobSatisfaction.2 + JobSatisfaction.4 + WorkLifeBalance.2 + WorkLifeBalance.3 + 
             WorkLifeBalance.4 + TimeInOffice.Over, 
           family = "binomial", data = train)
summary(m21)
# AIC: 2157.6
vif(m21)

# Remove  JobSatisfaction.2      P value : 0.019098  
m22 <- glm(formula = Attrition ~ AvgHours + Age + BusinessTravel.Travel_Frequently + 
             BusinessTravel.Travel_Rarely + Department.Research...Development + 
             Department.Sales + MaritalStatus.Single + NumCompaniesWorked + TotalWorkingYears +
             YearsSinceLastPromotion + YearsWithCurrManager + EnvironmentSatisfaction.2 + 
             EnvironmentSatisfaction.3 + EnvironmentSatisfaction.4 + 
             JobSatisfaction.4 + WorkLifeBalance.2 + WorkLifeBalance.3 + 
             WorkLifeBalance.4 + TimeInOffice.Over, 
           family = "binomial", data = train)
summary(m22)
# AIC: 2161.2
vif(m22)

# Remove  TimeInOffice.Over      P value : 0.001920 
m23 <- glm(formula = Attrition ~ AvgHours + Age + BusinessTravel.Travel_Frequently + 
             BusinessTravel.Travel_Rarely + Department.Research...Development + 
             Department.Sales + MaritalStatus.Single + NumCompaniesWorked + TotalWorkingYears +
             YearsSinceLastPromotion + YearsWithCurrManager + EnvironmentSatisfaction.2 + 
             EnvironmentSatisfaction.3 + EnvironmentSatisfaction.4 + 
             JobSatisfaction.4 + WorkLifeBalance.2 + WorkLifeBalance.3 + 
             WorkLifeBalance.4, 
           family = "binomial", data = train)
summary(m23)
# AIC: 2168.9
vif(m23)

# Remove  WorkLifeBalance.4      P value : 0.00216 
m24 <- glm(formula = Attrition ~ AvgHours + Age + BusinessTravel.Travel_Frequently + 
             BusinessTravel.Travel_Rarely + Department.Research...Development + 
             Department.Sales + MaritalStatus.Single + NumCompaniesWorked + TotalWorkingYears +
             YearsSinceLastPromotion + YearsWithCurrManager + EnvironmentSatisfaction.2 + 
             EnvironmentSatisfaction.3 + EnvironmentSatisfaction.4 + 
             JobSatisfaction.4 + WorkLifeBalance.2 + WorkLifeBalance.3 ,
           family = "binomial", data = train)
summary(m24)
# AIC: 2176.2
vif(m24)

# Remove  WorkLifeBalance.2      P value : 0.00585 
m25 <-  glm(formula = Attrition ~ AvgHours + Age + BusinessTravel.Travel_Frequently + 
             BusinessTravel.Travel_Rarely + Department.Research...Development + 
             Department.Sales + MaritalStatus.Single + NumCompaniesWorked + TotalWorkingYears +
             YearsSinceLastPromotion + YearsWithCurrManager + EnvironmentSatisfaction.2 + 
             EnvironmentSatisfaction.3 + EnvironmentSatisfaction.4 + 
             JobSatisfaction.4 + WorkLifeBalance.3 ,
           family = "binomial", data = train)
summary(m25)
# AIC: 2181.8
vif(m25)

# We can stop at model m25 since all the variables are significant.
# But there are variables (like department & Business travel) 
# that are affected by multicollinearity 
# Lets try to remove them and build a few more models
# We will later choose the best model  out of these 

# Remove  Department.Research...Development       P value : 8.34e-08  VIF : 3.756636
m26 <- glm(formula = Attrition ~ AvgHours + Age + BusinessTravel.Travel_Frequently + 
             BusinessTravel.Travel_Rarely + Department.Sales + MaritalStatus.Single + 
             NumCompaniesWorked + TotalWorkingYears +
             YearsSinceLastPromotion + YearsWithCurrManager + EnvironmentSatisfaction.2 + 
             EnvironmentSatisfaction.3 + EnvironmentSatisfaction.4 + 
             JobSatisfaction.4 + WorkLifeBalance.3 ,
           family = "binomial", data = train)
summary(m26)
# AIC: 2206.5
vif(m26)

# Remove  Department.Sales      P value : 0.18559 
m27 <- glm(formula = Attrition ~ AvgHours + Age + BusinessTravel.Travel_Frequently + 
             BusinessTravel.Travel_Rarely + MaritalStatus.Single + 
             NumCompaniesWorked + TotalWorkingYears +
             YearsSinceLastPromotion + YearsWithCurrManager + EnvironmentSatisfaction.2 + 
             EnvironmentSatisfaction.3 + EnvironmentSatisfaction.4 + 
             JobSatisfaction.4 + WorkLifeBalance.3 ,
           family = "binomial", data = train)
summary(m27)
# AIC: 2206.2
vif(m27)

# Again all the variables are significant 
# But BusinessTravel is affected with multicollinearity
# Lets remove it and check for one more model
# Remove  BusinessTravel.Travel_Rarely       P value : 7.51e-06 
m28 <- glm(formula = Attrition ~ AvgHours + Age + BusinessTravel.Travel_Frequently + 
             MaritalStatus.Single + NumCompaniesWorked + TotalWorkingYears +
             YearsSinceLastPromotion + YearsWithCurrManager + 
             EnvironmentSatisfaction.2 +
             EnvironmentSatisfaction.3 + EnvironmentSatisfaction.4 + 
             JobSatisfaction.4 + WorkLifeBalance.3 ,
             family = "binomial", data = train)
summary(m28)
# AIC: 2229.6
vif(m28)

# Remove  WorkLifeBalance.3       P value : 0.00148
m29 <- glm(formula = Attrition ~ AvgHours + Age + BusinessTravel.Travel_Frequently + 
             MaritalStatus.Single + NumCompaniesWorked + TotalWorkingYears +
             YearsSinceLastPromotion + YearsWithCurrManager + 
             EnvironmentSatisfaction.2 +
             EnvironmentSatisfaction.3 + EnvironmentSatisfaction.4 + 
             JobSatisfaction.4, family = "binomial", data = train)
summary(m29)
# AIC: 2237.7
vif(m29)

# After evaluating basic tests on models m26, m27, m28 & m29, m28 seems to have
# better metrics.

final_model <- m28

##***************************************************************************##
#                                                                             #
#                           Model Evaluation                                  #
#                                                                             #
##***************************************************************************##
# Accuracy 
# Sensitivity of a model is the proportion of yeses (or positives) correctly
# predicted by the model as yeses (or positives). 
# Specificity is equal to the proportion of nos (or negatives) correctly 
# predicted by the model as nos (or negatives)

# predicted probabilities of Attrition = 1 for test data
test$prob = predict(final_model, type = "response", 
                    newdata =  dplyr::select(test, -c(Attrition)))
 
summary(test$prob)

# Let's use the probability cutoff of 50%
test_pred <- factor(ifelse(test$prob >= 0.50, "Yes", "No"))
test_actual <- factor(ifelse(test$Attrition == 1,"Yes","No"))
table(test_pred,test_actual)

test_conf <- confusionMatrix(test_pred, test_actual, positive = "Yes")
(acc <- test_conf$overall[1])
(sens <- test_conf$byClass[1])
(spec <- test_conf$byClass[2])

# Accuracy    : 86%   
# Sensitivity : 23%        
# Specificity : 98%

# Let's use the probability cutoff of 40%
test_pred <- factor(ifelse(test$prob >= 0.40, "Yes", "No"))
test_actual <- factor(ifelse(test$Attrition == 1,"Yes","No"))
table(test_pred,test_actual)

test_conf <- confusionMatrix(test_pred, test_actual, positive = "Yes")
(acc <- test_conf$overall[1])
(sens <- test_conf$byClass[1])
(spec <- test_conf$byClass[2])

# Accuracy    : 86%  
# Sensitivity : 32%
# Specificity : 97%

# Lets create the metrics for variaous cutoff values from 0.01 to 0.80 
# and then plot them. 

perform_fn <- function(cutoff) 
{
  predicted_attr <- factor(ifelse(test$prob >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_attr, test_actual, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

# Initiallizing a matrix of 100 X 3 for storing the metrcis for various cutoffs
s = seq(.01,.80,length = 100)
OUT = matrix(0,100,3)
for (i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 

# Plot the values obtained
plot(s, OUT[,1],xlab = "Cutoff",ylab = "Value",cex.lab = 1.5,cex.axis = 1.5,
     ylim = c(0,1),type = "l",lwd = 2,axes = FALSE,col = 2)
axis(1,seq(0,1,length = 5),seq(0,1,length = 5),cex.lab = 1.5)
axis(2,seq(0,1,length = 5),seq(0,1,length = 5),cex.lab = 1.5)
lines(s,OUT[,2],col = "darkgreen",lwd = 2)
lines(s,OUT[,3],col = 4,lwd = 2)
box()
legend(0.10,.25,col = c(2,"darkgreen",4,"darkred"),lwd = c(2,2,2,2),
       c("Sensitivity","Specificity","Accuracy"))

# Identify the cutoff value where all 3 (Sensitivity, Specificity, Accuracy)
#  merge at one point
(cutoff <- s[which(abs(OUT[,1] - OUT[,2]) < 0.02)])
# 0.1695960

# Let's use the above cutoff probabality. 
test_pred <- factor(ifelse(test$prob >= cutoff, "Yes", "No"))
test_actual <- factor(ifelse(test$Attrition == 1,"Yes","No"))
table(test_pred,test_actual)

test_conf <- confusionMatrix(test_pred, test_actual, positive = "Yes")
(acc <- test_conf$overall[1])
(sens <- test_conf$byClass[1])
(spec <- test_conf$byClass[2])

## Summary
# Accuracy    : 75%   
# Sensitivity : 74%         
# Specificity : 75%   

##***************************************************************************##
### KS -statistic - Test Data ######

# KS statistic is an indicator of how well the model discriminates between 
# the two classes.
# It is equal to 0% for the random model, and 100% for the perfect model

test_pred_attrition <- ifelse(test_pred == "Yes",1,0)
test_actual_attrition <- ifelse(test_actual == "Yes",1,0)

pred_object_test <- prediction(test_pred_attrition, test_actual_attrition)
performance_measures_test <- performance(pred_object_test, "tpr", "fpr")

# Lets plot the Area under curve
auc <- performance(pred_object_test,"auc")
unlist(auc@y.values)
# AUC 0.747919

plot(performance_measures_test,col = "red")
abline(0,1, lty = 8, col = "grey")


ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])
(max(ks_table_test))

## Summary 
# KS Static - 50%  (KS Static > 40 % indicates a good model)

##***************************************************************************##
# Lift & Gain Chart 

# Gain chart is a popular method to visually inspect model performance 
# in binary prediction. It presents the percentage of captured 
# positive responses as a function of selected percentage of a sample

# Lift basically just tells you the factor by which your model is 
# outperforming a random model, i.e. a model-less situation

lift <- function(labels , predicted_prob,groups=10) {
  if (is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if (is.factor(predicted_prob)) predicted_prob <- 
      as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp = sum(., na.rm = TRUE))) %>%
    mutate(Cumresp = cumsum(totalresp),
           Gain = Cumresp/sum(totalresp)*100,
           Cumlift = Gain/(bucket*(100/groups))) 
  return(gaintable)
}

Attr_decile = lift(test_actual_attrition, test_pred_attrition, groups = 10)

ggplot(Attr_decile,aes(x = bucket, y = Gain)) + 
  geom_line() + 
  geom_point(col = 'red2', size = 5) +
  geom_text(aes(label = round(Gain,2)),  
            nudge_x = -0.40, nudge_y = -0.40)

ggplot(Attr_decile,aes(x = bucket, y = Cumlift)) + 
  geom_line() + 
  geom_point(col = 'red2', size = 5) +
  geom_text(aes(label = round(Cumlift,2)), 
            nudge_x = 0.10, nudge_y = 0.10)

## Summary 
# Gain 
# Given model is 75% by the 4th decile. 
# Basically this means that if we sort all employess according to probability, 
# then among the top 40% customers of this sorted list, we would find 
# 75% of all employess that were likely to leave the company

# Lift 
# Lift is equal to 2.28 by the 3rd decile. 
# This means that the model's gain by the end of the 3rd decile is 2.28 times 
# that of a random model's gain at the end of 3 deciles. In other words, 
# the model catches 2.28 times more attrition than a random model would have 
# caught.

##***************************************************************************##
#                                                                             #
#                               Summary                                       #
#                                                                             #
##***************************************************************************##

# No. of model iterations = 29. The final model selected is m28
# Final Coefficients:
#                                     Estimate Std. Error z value Pr(>|z|)    
#   (Intercept)                      -1.49899    0.12325 -12.162  < 2e-16 ***
#   AvgHours                          0.53708    0.05218  10.294  < 2e-16 ***
#   Age                              -0.31570    0.07751  -4.073 4.64e-05 ***
#   BusinessTravel.Travel_Frequently  0.74619    0.12680   5.885 3.99e-09 ***
#   MaritalStatus.Single              0.98842    0.11103   8.902  < 2e-16 ***
#   NumCompaniesWorked                0.35466    0.05567   6.371 1.88e-10 ***
#   TotalWorkingYears                -0.43583    0.09922  -4.393 1.12e-05 ***
#   YearsSinceLastPromotion           0.40887    0.07166   5.705 1.16e-08 ***
#   YearsWithCurrManager             -0.46500    0.08418  -5.524 3.32e-08 ***
#   EnvironmentSatisfaction.2        -0.96198    0.16693  -5.763 8.28e-09 ***
#   EnvironmentSatisfaction.3        -0.94231    0.14746  -6.390 1.66e-10 ***
#   EnvironmentSatisfaction.4        -1.21263    0.15347  -7.902 2.75e-15 ***
#   JobSatisfaction.4                -0.76907    0.12899  -5.962 2.49e-09 ***
#   WorkLifeBalance.3                -0.35003    0.11010  -3.179  0.00148 ** 
# All the final variables selected for the model:  
# Are Significant i.e P value almost = 0 and negligible 
# Have VIF < 3 indicating no major multicollinearity 

# Cutoff chosen = 0.1616162
# Accuracy    : 75%   
# Sensitivity : 74%         
# Specificity : 75%   

# AUC 0.75

# KS Static - 50%  (KS Static > 40% indicates a good model)

# Gain 
# Given model is 75% by the 4th decile. 
# Basically this means that if we sort all employess according to probability, 
# then among the top 40% customers of this sorted list, we would find 
# 75% of all employess that were likely to leave the company

# Lift 
# Lift is equal to 2.28 by the 3rd decile. 
# This means that the model's gain by the end of the 3rd decile is 2.28 times 
# that of a random model's gain at the end of 3 deciles. In other words, 
# the model catches 2.28 times more attrition than a random model would have 
# caught.

# Conclusion: 
# The company should focus on the following factors to curb attrition
# 1. Frequent BusinessTravel  
# 2. Single (Marital Status)
# 3. Low Environment Satisfaction
# 4. Low Job Satisfaction
# 5. Low Work Life balance 
# 6. Higher average working hours
# 7. Lesser Age 
# 8. Less Total Working years
# 9. Higher Years since last Promotion 
# 10. Less Years with current Manager 
# 11. Less Number of companies worked
#************************************End of file*******************************