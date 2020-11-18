library("readr") # data input
library('tidyr') # data wrangling
library('dplyr') # data manipulation
library('stringr') # string manipulation
library('ggplot2') # visualization
library('ggthemes') # visualization
library('corrplot') # visualization
library('lubridate') # date and time
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

## get rid of columns having more than 10% of missing data
loan_cleaned_I <- df_loan[, -which(colMeans(is.na(df_loan)) > 0.1)]
df_loan_cleaned <- within(loan_cleaned_I, rm('X',
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
write.csv(x = df_loan_cleaned, file = "../regression_loan_cleaned.csv")


set.seed(1)
df_loan_sample <- sample_n(df_loan_cleaned,100000)
write.csv(x = df_loan_sample, file = "regression_train_loan_sample_cleaned.csv")


#################################################
##
## II.I Data type correction
##
#################################################
ggplot(radial,aes(y=NTAV,x=age))+geom_point()+geom_smooth(method="lm")
# converting incorrect data types to be able to work with them as factor variables
df_loan_sample$grade <- as.factor(df_loan_sample$sub_grade)
df_loan_sample$term <- as.factor(df_loan_sample$term)
df_loan_sample$emp_length <- as.factor(df_loan_sample$emp_length)
df_loan_sample$home_ownership <- as.factor(df_loan_sample$home_ownership)
df_loan_sample$verification_status <- as.factor(df_loan_sample$verification_status)
df_loan_sample$loan_status <- as.factor(df_loan_sample$loan_status)
df_loan_sample$application_type <- as.factor(df_loan_sample$application_type)
df_loan_sample$initial_list_status <- as.factor(df_loan_sample$initial_list_status)

# converting dates from string values
df_loan_sample$issue_d <- parse_date(as.character(df_loan_sample$issue_d), format =  "%b-%Y")





################## LM MOODEL ################




ggplot(df_loan_sample_small,aes(y=int_rate,x=sub_grade))+geom_point()+geom_smooth(method="lm")
ggplot(df_loan_sample_small,aes(y=int_rate,x=loan_status))+geom_point()+geom_smooth(method="lm")
ggplot(df_loan_sample_small,aes(y=int_rate,x=verification_status))+geom_point()+geom_smooth(method="lm")
ggplot(df_loan_sample_small,aes(y=int_rate,x=issue_d))+geom_point()+geom_smooth(method="lm")




outliers()





linear_fit <- lm(data = df_loan_sample, formula = int_rate~sub_grade+loan_status+verification_status)
linear_fit <- lm(data = df_loan_sample, formula = int_rate~sub_grade+loan_status+verification_status+issue_d)  ## Best Model!
linear_fit <- lm(data = df_loan_sample, formula = int_rate~sub_grade+issue_d)
linear_fit <- lm(data = df_loan_sample, formula = int_rate~sub_grade)
linear_fit <- lm(data = df_loan_sample, formula = int_rate~.)

summary(linear_fit)

df <- outlierTest(linear_fit)

index <- c(49269,34056,47720,30309,39466,22575,4556,29696,4217,1258)
linear_fit <-linear_fit <- lm(data = df_loan_sample[-index,], formula = int_rate~sub_grade+loan_status+verification_status)

summary(linear_fit_wo_outlier)


############ Testing  ################

set.seed(1)
df_loan_testing <- sample_n(df_loan_cleaned,49999)
# df_loan_testing <- read.csv("regression_loan_cleaned_full.csv",sep = ",", header = TRUE)




######### Converting Test Data
# converting incorrect data types to be able to work with them as factor variables
df_loan_testing$grade <- as.factor(df_loan_testing$sub_grade)
df_loan_testing$term <- as.factor(df_loan_testing$term)
df_loan_testing$emp_length <- as.factor(df_loan_testing$emp_length)
df_loan_testing$home_ownership <- as.factor(df_loan_testing$home_ownership)
df_loan_testing$verification_status <- as.factor(df_loan_testing$verification_status)
df_loan_testing$loan_status <- as.factor(df_loan_testing$loan_status)
df_loan_testing$application_type <- as.factor(df_loan_testing$application_type)
df_loan_testing$initial_list_status <- as.factor(df_loan_testing$initial_list_status)

# converting dates from string values
df_loan_testing$issue_d <- parse_date(as.character(df_loan_testing$issue_d), format =  "%b-%Y")







mypredict <- as.data.frame(predict.lm(linear_fit, newdata = df_loan_testing, level = 0.95, se.fit=TRUE,interval = "confidence"))
# mypredict$outcome <- ifelse(df_loan_testing$int_rate < mypredict$fit.lwr, "to low",ifelse(df_loan_testing$int_rate < mypredict$fit.upr,"ok","to high"))
mypredict$outcome <- df_loan_testing$int_rate - mypredict$fit.fit
# mypredict

mean(mypredict$outcome)
median(mypredict$outcome)
boxplot(mypredict$outcome)

hist(mypredict$outcome)

RMSE = mean((mypredict$outcome - mypredict$fit.fit)^2) %>% sqrt()


df_outliers <- filter(mypredict, mypredict$outcome > 2)
hist(mypredict$outcome)
plot(mypredict$outcome)
write.csv(df_outliers, file ="outlier csv.csv")
