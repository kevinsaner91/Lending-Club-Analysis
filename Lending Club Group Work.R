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
library(fiftystater)
library(viridis)

# setwd("C:/Users/phkem/Switch Drive/02_Studium/01_Master Olten/3. Semester/Data Science/Group Work")
# df <- read.csv("regression_train_loan.csv",sep = ",", header = TRUE)
# df_sample <- sample_n(df,10000)
# write.csv(x = df_sample, file = "regression_train_loan_sample.csv")

rm(list = ls())
df_loan <- read.csv("regression_train_loan_sample.csv",sep = ",", header = TRUE)

#View the Summary and get a glimpse of your data
summary(df_loan)
glimpse(df_loan)

#View the Summary and get a glimpse of your data
sum(duplicated(df_loan))


########## DATA CLEANING

#  check the number of missing or NA values in the data set
colSums(is.na(df_loan))

# Visualize the number of NA values
options(repr.plot.width=6, repr.plot.height=8)
missing_data <- df_loan %>% summarise_all(funs(sum(is.na(.))/n()))
missing_data <- gather(missing_data, key = "variables", value = "percent_missing") 
ggplot(missing_data, aes(x = reorder(variables, percent_missing), y = percent_missing)) +
  geom_bar(stat = "identity", fill = "red", aes(color = I('white')), size = 0.1)+coord_flip()+ theme_few()

# get rid of columns having more than 10% of missing data
loan_cleaned <- loan[, -which(colMeans(is.na(loan)) > 0.1)]

# Removing the redundant & duplicate variables (to be compleated)
loan_cleaned <- within(loan_cleaned, rm('member_id', 'id', 'url', 'emp_title', "title","zip_code"))

# Remove variables which are not relevant or not present when the interest was decided (to be done!)
loan_cleaned <- loan_cleaned[, -c(2,3,13,28,29,30,31,32,33,34,35,36,37,38,40,43)]

# converting incorrect data types --> HAS TO BE CHEKED
loan_cleaned$term <- as.factor(loan_cleaned$term)
loan_cleaned$grade <- as.factor(loan_cleaned$grade)
loan_cleaned$emp_length <- as.factor(loan_cleaned$emp_length)
loan_cleaned$home_ownership <- as.factor(loan_cleaned$home_ownership)
loan_cleaned$verification_status <- as.factor(loan_cleaned$verification_status)
loan_cleaned$loan_status <- as.factor(loan_cleaned$loan_status)
loan_cleaned$application_type <- as.factor(loan_cleaned$application_type)
loan_cleaned$pymnt_plan <- as.factor(loan_cleaned$pymnt_plan)
loan_cleaned$initial_list_status <- as.factor(loan_cleaned$initial_list_status)
loan_cleaned$policy_code <- as.factor(loan_cleaned$policy_code)