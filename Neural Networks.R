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
library('CRAN')


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

####
##
## get rid of columns having more than 10% of missing data
## first step of cleaning 
##
####
loan_cleaned_I <- df_loan[, -which(colMeans(is.na(df_loan)) > 0.1)]


#################################################
##
## Removing all columns already identified as not usable in the Excel 
## - some already removed by the previous operation
##
#################################################
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

####################################
##
##Until here this is pretty much what we did before
##
###################################

#omit all fully paid loans
df_loan_cleaned <- df_loan_cleaned %>% filter(loan_status != 'Fully Paid')

#TODO determine how the default status is defined

# can't remember but for now we do something
# all late loans are now default
df_loan_cleaned$loan_status <- replace(df_loan_cleaned$loan_status, df_loan_cleaned$loan_status == "Charged Off", "Default") 
df_loan_cleaned$loan_status <- replace(df_loan_cleaned$loan_status, df_loan_cleaned$loan_status != "Default", "No Default") 

#we now only have two levels, default and no default

#let's sample
set.seed(666)
df_loan_cleaned_sample <- sample_n(df_loan_cleaned,10000)


#TODO write to csv, I think general preparation is done

#########################################################
##
## Prepare data for Neural Networks
##
#########################################################

# https://www.kaggle.com/deepanshu08/prediction-of-lendingclub-loan-defaulters

library('keras')
library('fastDummies')

#make them dummies

train_features <- dummy_cols(df_loan_cleaned_sample, select_columns = 'loan_status', remove_selected_columns = TRUE)
loan_status_default <- train_features$loan_status_Default

x <- dummy_cols(df_loan_cleaned_sample, select_columns = 'emp_length')


train_labels <- dummy_cols(df_loan_cleaned_sample, select_columns = c('int_rate', 'dti','term','sub_grade','emp_length', 'home_ownership', 'verification_status', 'purpose', 'delinq_2yrs'), remove_selected_columns = TRUE)

#convert to matrix, this is pure magic
dm_loan_cleaned <- data.matrix(df_loan_cleaned_sample)
#everything is a number now, we'll see if this is sufficient
















