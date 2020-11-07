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
# df_sample <- sample_n(df,10000)

df_loan <- read.csv("../regression_train_loan.csv",sep = ",", header = TRUE)

# Checking summary of the data and getting first insights
summary(df_loan)
glimpse(df_loan) 

# Check for duplicates
sum(duplicated(df_loan))


########## DATA CLEANING
########## Based on insights above and further manual analysis 
########## (summary, boxplot, histogram per column if this was possible) (see Excel Sheet)

# Further check for NA values to be sure that we can delete them 
colSums(is.na(df_loan))
# Visualization of the number of NA values
options(repr.plot.width=6, repr.plot.height=8)
missing_data <- df_loan %>% summarise_all(funs(sum(is.na(.))/n()))
missing_data <- gather(missing_data, key = "variables", value = "percent_missing") 
ggplot(missing_data, 
       aes(x = reorder(variables, percent_missing), 
           y = percent_missing)) + geom_bar(stat = "identity", 
                                            fill = "red", 
                                            aes(color = I('white')), 
                                            size = 0.1)+coord_flip() + theme_few()

# get rid of columns having more than 10% of missing data
loan_cleaned_I <- df_loan[, -which(colMeans(is.na(df_loan)) > 0.1)]

# Check again visually  
missing_data <- loan_cleaned_I %>% summarise_all(funs(sum(is.na(.))/n()))
missing_data <- gather(missing_data, key = "variables", value = "percent_missing") 
ggplot(missing_data, 
       aes(x = reorder(variables, percent_missing), 
           y = percent_missing)) + geom_bar(stat = "identity", 
                                            fill = "red", 
                                            aes(color = I('white')), 
                                            size = 0.1)+coord_flip() + theme_few()

# Removing all columns already identified as not usable in the Excel - some already removed by the previous operation
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

# Check again visually  
missing_data <- df_loan_cleaned %>% summarise_all(funs(sum(is.na(.))/n()))
missing_data <- gather(missing_data, key = "variables", value = "percent_missing") 
ggplot(missing_data, 
       aes(x = reorder(variables, percent_missing), 
           y = percent_missing)) + geom_bar(stat = "identity", 
                                            fill = "red", 
                                            aes(color = I('white')), 
                                            size = 0.1)+coord_flip() + theme_few()

write.csv(x = df_loan_cleaned, file = "regression_loan_cleaned.csv")
df_sample <- sample_n(df_loan_cleaned,10000)
write.csv(x = df_sample, file = "regression_train_loan_sample_cleaned.csv")

## CLEANUP ## 
# Must not be performed - but could improve performance!
rm(list = ls())
df_loan_sample <- read.csv("regression_train_loan_sample_cleaned.csv",sep = ",", header = TRUE)


# converting incorrect data types to be able to work with them
# factor variables
df_loan_sample$term <- as.factor(df_loan_sample$term)
# df_loan_sample$grade <- as.factor(df_loan_sample$grade) # not in the data set after our analysis
df_loan_sample$emp_length <- as.factor(df_loan_sample$emp_length)
df_loan_sample$home_ownership <- as.factor(df_loan_sample$home_ownership)
df_loan_sample$verification_status <- as.factor(df_loan_sample$verification_status)
df_loan_sample$loan_status <- as.factor(df_loan_sample$loan_status)
df_loan_sample$application_type <- as.factor(df_loan_sample$application_type)
df_loan_sample$initial_list_status <- as.factor(df_loan_sample$initial_list_status)
# dates
df_loan_sample$issue_d <- parse_date(as.character(df_loan_sample$issue_d), format =  "%b-%Y")

# Analyze which data can be used for prediction and which might not help enough
# selecting, counting and grouping by status in sample: 
df_loan_sample.loan.distribution <- df_loan_sample %>% group_by(loan_status) %>% 
                                    dplyr::summarise(count=n()) %>% mutate(pct=count/sum(count))

ggplot(df_loan_sample.loan.distribution, 
       aes(x=reorder(loan_status, pct), 
           y=pct, colour=loan_status, fill=loan_status)) +
  geom_bar(stat="identity",
           aes(color = I('black')), 
           size = 0.1) + coord_flip() + theme(legend.position = "none")+ xlab("Loan status") + ylab("Percentage in the dataset in the loan status")

