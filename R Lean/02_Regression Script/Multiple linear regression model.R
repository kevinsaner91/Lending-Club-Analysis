###### Assignment
###### Multiple Linear Regression Model
###### Created by 

############ loading libraries
library('dplyr') # data manipulation
library('lubridate') # date and time
library(zoo)  # quarter (date)
library(DescTools) # for MAPE, MAE, MSE

############ additional libaries not used in the final version
library("readr") # data input
library('tidyr') # data wrangling
library('stringr') # string manipulation
library('ggplot2') # visualization
library('ggthemes') # visualization
library('corrplot') # visualization
library('purrr') # data manipulation
library('cowplot')
library(plotly)
library(maps)
library(MASS)
library(viridis)
library(car) # outlier function
require(ggiraph) #making plots look better
require(ggiraphExtra)
require(plyr)


# ===================================================================================================
# ===================================================================================================
# ===================================================================================================
#
# I. Initial data load and first data preparation
#
# (setwd before starting!)
#
# ===================================================================================================
# ===================================================================================================
# ===================================================================================================

rm(list = ls())
df_loan <- read.csv("regression_train_loan.csv",sep = ",", header = TRUE)

# Set Seed to 1 so that the results can be reproduced 
set.seed(1)

## get rid of columns having more than 10% of missing data and clean data from columns  that are not needed
df_loan_cleaned <- df_loan[, -which(colMeans(is.na(df_loan)) > 0.1)]
df_loan_cleaned <- within(df_loan_cleaned, rm('X',
                                          'url',
                                          'id',
                                          'member_id',
                                          'installment',
                                          'grade',
                                          'emp_title',
                                          'pymnt_plan',
                                          'url',
                                          'desc',
                                          'title',
                                          'delinq_2yrs',
                                          'earliest_cr_line',
                                          'pub_rec',
                                          'last_pymnt_d',
                                          'last_pymnt_amnt',
                                          'next_pymnt_d',
                                          'last_credit_pull_d',
                                          'mths_since_last_major_derog',
                                          'policy_code',
                                          'open_acc_6m',
                                          'open_il_6m',
                                          'open_il_12m',
                                          'open_il_24m',
                                          'mths_since_rcnt_il',
                                          'total_bal_il',
                                          'il_util',
                                          'open_rv_12m',
                                          'open_rv_24m',
                                          'max_bal_bc',
                                          'all_util',
                                          'total_rev_hi_lim',
                                          'inq_fi',
                                          'total_cu_tl',
                                          'inq_last_12m'))


## write a file of the cleaned set
#write.csv(x = df_loan_cleaned, file = "../regression_loan_cleaned.csv")


# create a sample of 150000 random entries and write a csv file
df_loan_sample <- sample_n(df_loan_cleaned,400000)

#################################################
##
## II.I Data type correction
##
#################################################


#Changing data types (as done and described in the previous file)

df_loan_sample$sub_grade <- as.factor(df_loan_sample$sub_grade)
df_loan_sample$term <- as.factor(df_loan_sample$term)
df_loan_sample$emp_length <- as.factor(df_loan_sample$emp_length)
df_loan_sample$home_ownership <- as.factor(df_loan_sample$home_ownership)
df_loan_sample$verification_status <- as.factor(df_loan_sample$verification_status)
df_loan_sample$loan_status <- as.factor(df_loan_sample$loan_status)
df_loan_sample$application_type <- as.factor(df_loan_sample$application_type)
df_loan_sample$initial_list_status <- as.factor(df_loan_sample$initial_list_status)
df_loan_sample$issue_d <- parse_date(as.character(df_loan_sample$issue_d), format =  "%b-%Y")

# Adding additional date factors (year and year quarters)
df_loan_sample$year <- as.factor(substr(df_loan_sample$issue_d, 1, 4))
df_loan_sample$yq <- as.factor(as.yearqtr(df_loan_sample$issue_d, format = "%Y-%m-%d"))


write.csv(x = df_loan_sample, file = "regression_train_loan_sample_cleaned.csv")
save(df_loan_sample, file = "df_loan_sample.rda")

#################################################
##
## III Spliting test and trainig data
##
#################################################
load("df_loan_sample.rda")

ind <- sample(2, nrow(df_loan_sample), replace = T, prob = c(0.8,0.2))
train <- df_loan_sample[ind==1, ]
test <- df_loan_sample[ind==2, ]

save(train, file = "train.rda")
save(test, file = "test.rda")

#################################################
##
## IV Creating regression model
##
#################################################

#loading the training data
load("train.rda")

### creating a linear model predicting the interest rate with loan_status, sub_grade, verification_status with regard to the year 
linear_fit_economical <- lm(data = train, formula = int_rate~(sub_grade+loan_status+verification_status)*year)
summary(linear_fit_economical)

### creating a linear model predicting the interest rate with loan_status, sub_grade, verification_status with regard to the year quarter
# takes some minutes to load because of 35 date levels
linear_fit_best <- lm(data = train, formula = int_rate~sub_grade*yq+loan_status+verification_status)
summary(linear_fit_best)



###################################################
##
## V Testing the regression model
##
###################################################




#loading the test data
load("test.rda")

############ Predicting on test data for the linear_fit_economical
mypredict <- as.data.frame(predict.lm(linear_fit_economical, newdata = test, level = 0.95, se.fit=TRUE,interval = "confidence"))
mypredict$outcome <- test$int_rate - mypredict$fit.fit


############ Testing Predicted Data 

# Mean absolute error (MAE)
MAE(mypredict$fit.fit, test$int_rate)

# Mean squared error (MSE)
MSE(mypredict$fit.fit, test$int_rate)

# Mean absolute percent error (MAPE)
MAPE(mypredict$fit.fit, test$int_rate)

# Boxplot Prediction Error
boxplot(mypredict$outcome, main="Prediction Error")
boxplot(mypredict$outcome, outline = FALSE, main="Prediction Error (w/o outliers)")

# Boxplot Absolute Prediction Error
boxplot(abs(mypredict$outcome), main="Absolute Prediction Error")
boxplot(abs(mypredict$outcome), outline = FALSE, main="Absolute Prediction Error (w/o outliers)")

# Boxplot Absolute Prediction Error
hist(mypredict$outcome ,breaks = 1000, main = "Histogram of Prediction Error")





############ Predicting on test data for the linear_fit_economical

mypredict <- as.data.frame(predict.lm(linear_fit_best, newdata = test, level = 0.95, se.fit=TRUE,interval = "confidence"))
mypredict$outcome <- test$int_rate - mypredict$fit.fit


############ Testing Predicted Data

# Mean absolute error (MAE)
MAE(mypredict$fit.fit, test$int_rate)

# Mean squared error (MSE)
MSE(mypredict$fit.fit, test$int_rate)

# Mean absolute percent error (MAPE)
MAPE(mypredict$fit.fit, test$int_rate)

# Boxplot Prediction Error
boxplot(mypredict$outcome, main="Prediction Error")
boxplot(mypredict$outcome, outline = FALSE, main="Prediction Error (w/o outliers)")

# Boxplot Absolute Prediction Error
boxplot(abs(mypredict$outcome), main="Absolute Prediction Error")
boxplot(abs(mypredict$outcome), outline = FALSE, main="Absolute Prediction Error (w/o outliers)")

# Boxplot Absolute Prediction Error
hist(mypredict$outcome ,breaks = 1000,  main = "Histogram of Prediction Error")



###################################################
##
## VI Testing the regression model with the official testdata as test_fhnw set
##
###################################################

###### Loading the official testdata as test_fhnw set

test_fhnw <- read.csv("loan_eval.csv",sep = ",", header = TRUE)

## Set Seed to 1 so that the results can be reproduced 
set.seed(1)

###### Cleaning the fhnw test data set and changing the data types 

## get rid of columns having more than 10% of missing data and clean data from colums that are not needed
test_fhnw <- df_loan[, -which(colMeans(is.na(df_loan)) > 0.1)]
test_fhnw <- within(test_fhnw, rm('X',
                                              'url',
                                              'id',
                                              'member_id',
                                              'installment',
                                              'grade',
                                              'emp_title',
                                              'pymnt_plan',
                                              'url',
                                              'desc',
                                              'title',
                                              'delinq_2yrs',
                                              'earliest_cr_line',
                                              'pub_rec',
                                              'last_pymnt_d',
                                              'last_pymnt_amnt',
                                              'next_pymnt_d',
                                              'last_credit_pull_d',
                                              'mths_since_last_major_derog',
                                              'policy_code',
                                              'open_acc_6m',
                                              'open_il_6m',
                                              'open_il_12m',
                                              'open_il_24m',
                                              'mths_since_rcnt_il',
                                              'total_bal_il',
                                              'il_util',
                                              'open_rv_12m',
                                              'open_rv_24m',
                                              'max_bal_bc',
                                              'all_util',
                                              'total_rev_hi_lim',
                                              'inq_fi',
                                              'total_cu_tl',
                                              'inq_last_12m'))


## Changing data types 

test_fhnw$sub_grade <- as.factor(test_fhnw$sub_grade)
test_fhnw$term <- as.factor(test_fhnw$term)
test_fhnw$emp_length <- as.factor(test_fhnw$emp_length)
test_fhnw$home_ownership <- as.factor(test_fhnw$home_ownership)
test_fhnw$verification_status <- as.factor(test_fhnw$verification_status)
test_fhnw$loan_status <- as.factor(test_fhnw$loan_status)
test_fhnw$application_type <- as.factor(test_fhnw$application_type)
test_fhnw$initial_list_status <- as.factor(test_fhnw$initial_list_status)
test_fhnw$issue_d <- parse_date(as.character(test_fhnw$issue_d), format =  "%b-%Y")

## Adding additional date factors (year and year quarters)
test_fhnw$year <- as.factor(substr(test_fhnw$issue_d, 1, 4))
test_fhnw$yq <- as.factor(as.yearqtr(test_fhnw$issue_d, format = "%Y-%m-%d"))

###### Save or load the official test data set
#save(test_fhnw,file = "test_fhnw.rda")
load("test_fhnw.rda")


###### Creating the predication for the linear_fit_best
# in order to predict enough ram is needed (16GB) -> with the command "memory.limit(99999999)" the memory limit can be enlard
mypredict <- as.data.frame(predict.lm(linear_fit_best, newdata = test_fhnw, level = 0.95, se.fit=TRUE,interval = "confidence"))
mypredict$outcome <- test$int_rate - mypredict$fit.fit

###### Testing Predicted Data

# Mean absolute error (MAE)
MAE(mypredict$fit.fit, test_fhnw$int_rate)

# Mean squared error (MSE)
MSE(mypredict$fit.fit, test_fhnw$int_rate)

# Mean absolute percent error (MAPE)
MAPE(mypredict$fit.fit, test_fhnw$int_rate)

# Boxplot Prediction Error
boxplot(mypredict$outcome, main="Prediction Error")
boxplot(mypredict$outcome, outline = FALSE, main="Prediction Error (w/o outliers)")

# Boxplot Absolute Prediction Error
boxplot(abs(mypredict$outcome), main="Absolute Prediction Error")
boxplot(abs(mypredict$outcome), outline = FALSE, main="Absolute Prediction Error (w/o outliers)")

# Boxplot Absolute Prediction Error
hist(mypredict$outcome ,breaks = 1000,  main = "Histogram of Prediction Error")





############  Creating the predication for the linear_fit_economical
mypredict <- as.data.frame(predict.lm(linear_fit_economical, newdata = test_fhnw, level = 0.95, se.fit=TRUE,interval = "confidence"))
mypredict$outcome <- test$int_rate - mypredict$fit.fit

########## Testing Predicted Data

# Mean absolute error (MAE)
MAE(mypredict$fit.fit, test_fhnw$int_rate)

# Mean squared error (MSE)
MSE(mypredict$fit.fit, test_fhnw$int_rate)

# Mean absolute percent error (MAPE)
MAPE(mypredict$fit.fit, test_fhnw$int_rate)

# Boxplot Prediction Error
boxplot(mypredict$outcome, main="Prediction Error")
boxplot(mypredict$outcome, outline = FALSE, main="Prediction Error (w/o outliers)")

# Boxplot Absolute Prediction Error
boxplot(abs(mypredict$outcome), main="Absolute Prediction Error")
boxplot(abs(mypredict$outcome), outline = FALSE, main="Absolute Prediction Error (w/o outliers)")

# Boxplot Absolute Prediction Error
hist(mypredict$outcome ,breaks = 1000,  main = "Histogram of Prediction Error")