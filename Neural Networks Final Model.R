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
                                             'zip_code',
                                             'addr_state',
                                             'inq_last_6mths',
                                             'open_acc',
                                             'revol_bal',
                                             'revol_util',
                                             'total_acc',
                                             'initial_list_status',
                                             'out_prncp_inv',
                                             'total_pymnt',
                                             'total_pymnt_inv',
                                             'total_rec_late_fee',
                                             'recoveries',
                                             'collection_recovery_fee',
                                             'collections_12_mths_ex_med',
                                             'application_type',
                                             'verification_status_joint',
                                             'acc_now_delinq',
                                             'tot_coll_amt',
                                             'tot_cur_bal',
                                             'int_rate'
                                             ))

####################################
##
##Until here this is pretty much what we did before
##
###################################

#omit all current loans
df_loan_cleaned <- df_loan_cleaned %>% filter(loan_status != "Current")

# !Fully Paid = Default
df_loan_cleaned$loan_status <- replace(df_loan_cleaned$loan_status, df_loan_cleaned$loan_status != "Fully Paid", "Default") 

#########################################################
##
## Prepare data for Neural Networks
##
#########################################################
library('keras')
library('fastDummies')
library('OneR')
library("scales")


df_loan_cleaned <- df_loan_cleaned[complete.cases(df_loan_cleaned), ]
df_train_labels <- dummy_cols(df_loan_cleaned, select_columns = 'loan_status', remove_selected_columns = TRUE)
train_labels <- df_train_labels$loan_status_Default
# creating categoricals of the loan status
dm_train_labels <- to_categorical(train_labels)

# loan_status, we are done with you BYE!
df_loan_cleaned <- within(df_loan_cleaned, rm('loan_status'))

bins <- 100

#we have one more level in home_ownership which is any, this is converted to other
#df_loan_cleaned$home_ownership <- replace(df_loan_cleaned$home_ownership, df_loan_cleaned$home_ownership == "ANY", "OTHER") 

# binning with bin size 100 which proved to be sufficient
# binning is done to reduce the levels of continuous variables
df_loan_cleaned$loan_amnt <- bin(df_loan_cleaned$loan_amnt, nbins = bins)
df_loan_cleaned$annual_inc <- bin(df_loan_cleaned$annual_inc, nbins = bins)
df_loan_cleaned$installment <- bin(df_loan_cleaned$installment, nbins = bins)
df_loan_cleaned$dti <- bin(df_loan_cleaned$dti, nbins = bins)
df_loan_cleaned$delinq_2yrs <- bin(df_loan_cleaned$delinq_2yrs, nbins = 14)
df_loan_cleaned$out_prncp <- bin(df_loan_cleaned$out_prncp, nbins = bins)
df_loan_cleaned$total_rec_int <- bin(df_loan_cleaned$total_rec_int, nbins = bins)
df_loan_cleaned$total_rec_prncp <- bin(df_loan_cleaned$total_rec_prncp, nbins = bins)

df_loan_cleaned$issue_d <- substr(df_loan_cleaned$issue_d,5, 9)

# The level ANY is not apparent in the test, so we convert ANY to OTHER
df_loan_cleaned$home_ownership <- replace(df_loan_cleaned$home_ownership, df_loan_cleaned$home_ownership == "ANY", "OTHER") 

# all variables are converted to dummy variable
df_train_features <- dummy_cols(df_loan_cleaned, select_columns = c('installment','total_rec_int','total_rec_prncp','out_prncp', 'issue_d','dti','delinq_2yrs' ,'annual_inc','loan_amnt','term','sub_grade','emp_length', 'home_ownership', 'verification_status', 'purpose'), remove_selected_columns = TRUE)

save(df_train_features, file = "train_features")
save(train_labels, file = 'train_labels')


##################################################
##
## Create the model and optimizers
##
##################################################

# excute code from here to just see how to learning is done
rm(list = ls())
# now we create the model
# the model is created the same as the one proved to be best in 10-fold CV
create_model_and_train <- function(my_train_features=dm_train_features, my_train_labels=dm_train_labels, epochs=2) {
  model <- keras_model_sequential() %>% 
    layer_dense(units = ncol(dm_train_features), activation = "relu", input_shape = c(ncol(dm_train_features) * 1)) %>% 
    layer_dense(units = 600, activation = "relu") %>% 
    layer_dense(units = 400, activation = "relu") %>% 
    layer_dense(units = 200, activation = "relu") %>%
    layer_dense(units = 50, activation = "relu") %>%
    layer_dense(units = 2, activation = "sigmoid")
  
  model %>% compile(
    optimizer = optimizer_rmsprop(
      lr = 0.001,
      rho = 0.9,
      epsilon = NULL,
      decay = 0,
      clipnorm = NULL,
      clipvalue = NULL
    ),
    loss = "binary_crossentropy",
    metrics = c("accuracy"))
  
  history <- model %>% fit(my_train_features, my_train_labels,validation_split = 0.1, epochs = epochs, batch_size = 100)

  save_model_hdf5 (model, "ff_nn_model", include_optimizer = TRUE)
  return(history)
}

#load data to train the network
load("train_labels")
load('train_features')

# it could be observed before that network learns really quick
# so 4 epochs are enough here
epochs <- 4

dm_train_features <- data.matrix(df_train_features)
dm_train_labels <- to_categorical(train_labels)

# now we train on almost the whole data
# we use 10 percent to validate
history <- create_model_and_train(dm_train_features, dm_train_labels, epochs)

###########################################################
##
## Apply the model to the test data set
## Once again we do the same preparation of the test data as before
##
###########################################################

rm(list = ls())
df_loan_test <- read.csv("loan_eval.csv",sep = ",", header = TRUE)
loan_cleaned_I_test <- df_loan_test[, -which(colMeans(is.na(df_loan_test)) > 0.1)]
df_loan_cleaned_test <- within(loan_cleaned_I_test, rm('X',
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
                                             'zip_code',
                                             'addr_state',
                                             'inq_last_6mths',
                                             'open_acc',
                                             'revol_bal',
                                             'revol_util',
                                             'total_acc',
                                             'initial_list_status',
                                             'out_prncp_inv',
                                             'total_pymnt',
                                             'total_pymnt_inv',
                                             'total_rec_late_fee',
                                             'recoveries',
                                             'collection_recovery_fee',
                                             'collections_12_mths_ex_med',
                                             'application_type',
                                             'verification_status_joint',
                                             'acc_now_delinq',
                                             'tot_coll_amt',
                                             'tot_cur_bal',
                                             'int_rate'
))



df_loan_cleaned_test <- df_loan_cleaned_test %>% filter(loan_status != "Current")
df_loan_cleaned_test$loan_status <- replace(df_loan_cleaned_test$loan_status, df_loan_cleaned_test$loan_status != "Fully Paid", "Default") 
df_loan_cleaned_test <- df_loan_cleaned_test[complete.cases(df_loan_cleaned_test), ]
df_test_labels <- dummy_cols(df_loan_cleaned_test, select_columns = 'loan_status', remove_selected_columns = TRUE)
test_labels <- df_test_labels$loan_status_Default
df_loan_cleaned_test <- within(df_loan_cleaned_test, rm('loan_status'))

bins <- 100
df_loan_cleaned_test$loan_amnt <- bin(df_loan_cleaned_test$loan_amnt, nbins = bins)
df_loan_cleaned_test$annual_inc <- bin(df_loan_cleaned_test$annual_inc, nbins = bins)
df_loan_cleaned_test$installment <- bin(df_loan_cleaned_test$installment, nbins = bins)
df_loan_cleaned_test$dti <- bin(df_loan_cleaned_test$dti, nbins = bins)
df_loan_cleaned_test$out_prncp <- bin(df_loan_cleaned_test$out_prncp, nbins = bins)
df_loan_cleaned_test$delinq_2yrs <- bin(df_loan_cleaned_test$delinq_2yrs, nbins = 14)
df_loan_cleaned_test$total_rec_int <- bin(df_loan_cleaned_test$total_rec_int, nbins = bins)
df_loan_cleaned_test$total_rec_prncp <- bin(df_loan_cleaned_test$total_rec_prncp, nbins = bins)

df_loan_cleaned_test$issue_d <- substr(df_loan_cleaned_test$issue_d,5, 9)
df_test_features <- dummy_cols(df_loan_cleaned_test, select_columns = c('installment','total_rec_int','total_rec_prncp','out_prncp', 'issue_d', 'dti','delinq_2yrs','annual_inc','loan_amnt','term','sub_grade','emp_length', 'home_ownership', 'verification_status', 'purpose'), remove_selected_columns = TRUE)
# test data is prepared, the model trained
# so let's load it and evaluate

save(df_test_features, file = "test_features")
save(test_labels, file = 'test_labels')

load("test_features")
load('test_labels')

#load the model
model <- load_model_hdf5("ff_nn_model", compile = TRUE)

dm_test_labels <- to_categorical(test_labels)
dm_test_features <- data.matrix(df_test_features)

result <- model %>% evaluate(dm_test_features,dm_test_labels, verbose = 1)
# we're happy with the result












