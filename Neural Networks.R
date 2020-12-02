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
                                             'inq_last_12m',
                                             'funded_amnt',
                                             'funded_amnt_inv',
                                             'issue_d',
                                             'zip_code',
                                             'addr_state',
                                             'inq_last_6mths',
                                             'open_acc',
                                             'revol_bal',
                                             'revol_util',
                                             'total_acc',
                                             'initial_list_status',
                                             'out_prncp',
                                             'out_prncp_inv',
                                             'total_pymnt',
                                             'total_pymnt_inv',
                                             'total_rec_prncp',
                                             'total_rec_int',
                                             'total_rec_late_fee',
                                             'recoveries',
                                             'collection_recovery_fee',
                                             'collections_12_mths_ex_med',
                                             'application_type',
                                             'verification_status_joint',
                                             'acc_now_delinq',
                                             'tot_coll_amt',
                                             'tot_cur_bal'
                                             ))

####################################
##
##Until here this is pretty much what we did before
##
###################################

#omit all fully paid loans
df_loan_cleaned <- df_loan_cleaned %>% filter(loan_status == "Fully Paid" |loan_status == "Default")

#TODO determine how the default status is defined

# can't remember but for now we do something
# Charged OFF = Default
# df_loan_cleaned$loan_status <- replace(df_loan_cleaned$loan_status, df_loan_cleaned$loan_status == "Charged Off", "Default") 
# df_loan_cleaned$loan_status <- replace(df_loan_cleaned$loan_status, df_loan_cleaned$loan_status != "Default", "No Default") 

#we now only have two levels, default and no default

write.csv(x = df_loan_cleaned, file = "regression_loan_cleaned_for_nn.csv")

######################################################################
##
## Store and sample
##
######################################################################

rm(list = ls())
df_loan_cleaned <- read.csv("regression_loan_cleaned_for_nn.csv",sep = ",", header = TRUE)
#let's sample
set.seed(666)
df_loan_cleaned_sample <- sample_n(df_loan_cleaned,10000)

write.csv(x = df_loan_cleaned_sample, file = "regression_loan_cleaned_for_nn_sample.csv")


#########################################################
##
## Prepare data for Neural Networks
##
#########################################################

# https://www.kaggle.com/deepanshu08/prediction-of-lendingclub-loan-defaulters

library('keras')
library('fastDummies')
library('OneR')
library("scales")

rm(list = ls())
df_loan_cleaned_sample <- read.csv("regression_loan_cleaned_for_nn_sample.csv",sep = ",", header = TRUE)

df_loan_cleaned_sample <- within(df_loan_cleaned_sample, rm('X.1',
                                                            'X'))

#prepare the loan_status as categorical value where 1 == Default
# Bit complicated but works for now
df_train_labels <- dummy_cols(df_loan_cleaned_sample, select_columns = 'loan_status', remove_selected_columns = TRUE)

dm_train_labels <- to_categorical(df_train_labels$loan_status_Default)

# loan_status, we are done with you BYE!
df_loan_cleaned_sample <- within(df_loan_cleaned_sample, rm('loan_status'))

# all numerical values that are left are rescaled on a scale from 0 to 1
df_loan_cleaned_sample$loan_amnt <- bin(df_loan_cleaned_sample$loan_amnt, nbins = 10)
df_loan_cleaned_sample$annual_inc <- bin(df_loan_cleaned_sample$annual_inc, nbins = 10)
df_loan_cleaned_sample$installment <- bin(df_loan_cleaned_sample$installment, nbins = 10)
df_loan_cleaned_sample$dti <- bin(df_loan_cleaned_sample$dti, nbins = 10)
df_loan_cleaned_sample$delinq_2yrs <- bin(df_loan_cleaned_sample$delinq_2yrs, nbins = 10)

# all categorical are converted to dummies like the mushrooms
df_train_features <- dummy_cols(df_loan_cleaned_sample, select_columns = c('installment',  'dti','delinq_2yrs' ,'annual_inc','loan_amnt','term','sub_grade','emp_length', 'home_ownership', 'verification_status', 'purpose'), remove_selected_columns = TRUE)

# if this step is not done, the loss function is nan, dont know why
# obviously this step should not be necessary
df_train_features <- within(df_train_features, rm('int_rate' ))


#convert to matrix, this is pure magic
#but I heard Holger say we need this
dm_train_features <- data.matrix(df_train_features)


# now we reshape to 10000 rows with 75 * 1 array
dm_train_features <- array_reshape(dm_train_features, c(10000, ncol(dm_train_features) * 1))


# now we create the model
# be aware units in the first layer and input shape must match to what is defined in the line before
create_model_and_train <- function(my_optimizer, my_train_features=dm_train_features, my_train_labels=dm_train_labels) {
  model <- keras_model_sequential() %>% 
    layer_dense(units = ncol(dm_train_features), activation = "relu", input_shape = c(ncol(dm_train_features) * 1)) %>% 
    layer_dense(units = 70, activation = "relu") %>% 
    layer_dense(units = 70, activation = "relu") %>% 
    layer_dense(units = 50, activation = "relu") %>% 
    layer_dense(units = 50, activation = "relu") %>% 
    layer_dense(units = 2, activation = "sigmoid")
  
  model %>% compile(
    optimizer = my_optimizer,
    loss = "binary_crossentropy",
    metrics = c("accuracy"))
  
  history <- model %>% fit(my_train_features, my_train_labels, epochs = 10)
  
  return(history)
}



#define some optimisers
optimizer_adam <- optimizer_adam(
  lr = 0.001,
  beta_1 = 0.9,
  beta_2 = 0.999,
  epsilon = NULL,
  decay = 0,
  amsgrad = FALSE,
  clipnorm = NULL,
  clipvalue = NULL
)

optimizer_sgd <- optimizer_sgd(
  lr = 0.01,
  momentum = 0,
  decay = 0,
  nesterov = FALSE,
  clipnorm = NULL,
  clipvalue = NULL
)

optimizer_rmsprop <- optimizer_rmsprop(
  lr = 0.001,
  rho = 0.9,
  epsilon = NULL,
  decay = 0,
  clipnorm = NULL,
  clipvalue = NULL
)

# dont know what happens now
history <- create_model_and_train(optimizer_rmsprop)
history <- create_model_and_train(optimizer_sgd)
history <- create_model_and_train(optimizer_adam)














