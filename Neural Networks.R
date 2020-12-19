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
                                             'tot_cur_bal'
                                             ))

####################################
##
##Until here this is pretty much what we did before
##
###################################

#omit all fully paid loans
df_loan_cleaned <- df_loan_cleaned %>% filter(loan_status != "Current")

#TODO determine how the default status is defined

# can't remember but for now we do something
# Charged OFF = Default
df_loan_cleaned$loan_status <- replace(df_loan_cleaned$loan_status, df_loan_cleaned$loan_status != "Fully Paid", "Default") 

#we now only have two levels, default and fully paid

write.csv(x = df_loan_cleaned, file = "regression_loan_cleaned_for_nn.csv")

######################################################################
##
## Store and sample
##
######################################################################

rm(list = ls())
df_loan_cleaned <- read.csv("regression_loan_cleaned_for_nn.csv",sep = ",", header = TRUE)
#let's sample
set.seed(1)
df_loan_cleaned_sample <- sample_n(df_loan_cleaned,100050)

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

df_loan_cleaned_sample <- df_loan_cleaned_sample[complete.cases(df_loan_cleaned_sample), ]

#prepare the loan_status as categorical value where 1 == Default
# Bit complicated but works for now
df_train_labels <- dummy_cols(df_loan_cleaned_sample, select_columns = 'loan_status', remove_selected_columns = TRUE)

write.csv(x = df_train_labels$loan_status_Default, file = "loan_status_default.csv")

#train_labels <- df_train_labels$loan_status_Default[1:90000]
#val_labels <- df_train_labels$loan_status_Default[90000:99986]

#dm_train_labels <- to_categorical(train_labels)
#dm_val_labels <- to_categorical(val_labels)

# loan_status, we are done with you BYE!
df_loan_cleaned_sample <- within(df_loan_cleaned_sample, rm('loan_status'))

bins <- 100

# all numerical values that are left are rescaled on a scale from 0 to 1
df_loan_cleaned_sample$loan_amnt <- bin(df_loan_cleaned_sample$loan_amnt, nbins = bins)
df_loan_cleaned_sample$annual_inc <- bin(df_loan_cleaned_sample$annual_inc, nbins = bins)
df_loan_cleaned_sample$installment <- bin(df_loan_cleaned_sample$installment, nbins = 150)
df_loan_cleaned_sample$dti <- bin(df_loan_cleaned_sample$dti, nbins = bins)
df_loan_cleaned_sample$delinq_2yrs <- bin(df_loan_cleaned_sample$delinq_2yrs, nbins = 14)
df_loan_cleaned_sample$out_prncp <- bin(df_loan_cleaned_sample$out_prncp, nbins = bins)
df_loan_cleaned_sample$total_rec_int <- bin(df_loan_cleaned_sample$total_rec_int, nbins = bins)
df_loan_cleaned_sample$total_rec_prncp <- bin(df_loan_cleaned_sample$total_rec_prncp, nbins = bins)



df_loan_cleaned_sample$issue_d <- substr(df_loan_cleaned_sample$issue_d,5, 9)

# all categorical are converted to dummies like the mushrooms
df_train_features <- dummy_cols(df_loan_cleaned_sample, select_columns = c('installment','total_rec_int','total_rec_prncp','out_prncp', 'issue_d','dti','delinq_2yrs' ,'annual_inc','loan_amnt','term','sub_grade','emp_length', 'home_ownership', 'verification_status', 'purpose'), remove_selected_columns = TRUE)

# if this step is not done, the loss function is nan, dont know why
# obviously this step should not be necessary
df_train_features <- within(df_train_features, rm('int_rate' ))

write.csv(x = df_train_features, file = "train_features.csv")


#train_features <- df_train_features[1:90000,]
#val_features <- df_train_features[90000:99986,]


#convert to matrix, this is pure magic
#but I heard Holger say we need this
#dm_train_features <- data.matrix(train_features)
#dm_val_features <- data.matrix(val_features)


##################################################
##
## Create the model and optimizers
##
##################################################




# now we create the model
# be aware units in the first layer and input shape must match to what is defined in the line before
create_model_and_train <- function(my_train_features=dm_train_features, my_train_labels=dm_train_labels, my_val_features=dm_val_features, my_val_labels=dm_val_labels, epochs=2) {
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
  
  all_scores <- c()
  history <- model %>% fit(my_train_features, my_train_labels, epochs = epochs, batch_size = 100)
  all_scores$train <- c(all_scores$train, history$metrics)
  results <- model %>% evaluate(my_val_features, my_val_labels, verbose = 1)
  all_scores$val <- c(all_scores$val, c(results))
  all_scores$model <- model
  
  return(all_scores)
}


# #define some optimisers
# optimizer_adam <- optimizer_adam(
#   lr = 0.001,
#   beta_1 = 0.9,
#   beta_2 = 0.999,
#   epsilon = NULL,
#   decay = 0,
#   amsgrad = FALSE,
#   clipnorm = NULL,
#   clipvalue = NULL
# )
# 
# optimizer_sgd <- optimizer_sgd(
#   lr = 0.01,
#   momentum = 0,
#   decay = 0,
#   nesterov = FALSE,
#   clipnorm = NULL,
#   clipvalue = NULL
# )
# 
# optimizer_rmsprop <- optimizer_rmsprop(
#   lr = 0.001,
#   rho = 0.9,
#   epsilon = NULL,
#   decay = 0,
#   clipnorm = NULL,
#   clipvalue = NULL
# )


#load data to train the network
df_train_labels <- read.csv("loan_status_default.csv",sep = ",", header = TRUE)
df_train_labels <- within(df_train_labels, rm('X'))
df_train_features <- read.csv("train_features.csv",sep = ",", header = TRUE)
df_train_features <- within(df_train_features, rm('X'))
all_scores <- c()
epochs <- 5

# 10-fold CV training and validating
for (i in c(0, 10000, 20000, 30000, 40000, 50000, 60000, 70000, 80000, 90000)){
  ub <- i + 10000
 
  
  cat("    build model for fold #", i, "\n")
  train_labels <- df_train_labels$x[-(i:ub)]
  val_labels <- df_train_labels$x[i:ub]
  
  dm_train_labels <- to_categorical(train_labels)
  dm_val_labels <- to_categorical(val_labels)
  
  
  train_features <- df_train_features[-(i:ub),]
  val_features <- df_train_features[i:ub,]
  
  
  #convert to matrix, this is pure magic
  #but I heard Holger say we need this
  dm_train_features <- data.matrix(train_features)
  dm_val_features <- data.matrix(val_features)
  
  history <- create_model_and_train(dm_train_features, dm_train_labels, dm_val_features, dm_val_labels, epochs)
  all_scores <- c(all_scores, history)
}

# calculate the mean training loss
train_loss <- c()
for (i in c(1,4,7,10,13,16,19,22,25,28)){
  train_loss <- c(train_loss, c(all_scores[i]$train$loss[epochs]))
}
train_loss_av <- mean(train_loss)

# calculate the mean training accuracy
train_acc <- c()
for (i in c(1,4,7,10,13,16,19,22,25,28)){
  train_acc <- c(train_acc, c(all_scores[i]$train$accuracy[epochs]))
}
train_acc_av <- mean(train_acc)

# calculate the mean validation loss
val_loss <- c()
for (i in c(2,5,8,11,14,17,20,23,26,29)){
  val_loss <- c(val_loss, c(all_scores[i]$val[1]))
}
val_loss_av <- mean(val_loss)

# calculate the mean validation accuracy
val_acc <- c()
for (i in c(2,5,8,11,14,17,20,23,26,29)){
  val_acc <- c(val_acc, c(all_scores[i]$val[2]))
}
val_acc_av <- mean(val_acc)

save_model_hdf5 (all_scores[30]$model, "ff_nn_model", include_optimizer = TRUE)



###########################################################
##
## Apply the model to the test data set
##
###########################################################

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
                                             'tot_cur_bal'
))


#omit all fully paid loans
df_loan_cleaned_test <- df_loan_cleaned_test %>% filter(loan_status != "Current")

# can't remember but for now we do something
# Charged OFF = Default
df_loan_cleaned_test$loan_status <- replace(df_loan_cleaned_test$loan_status, df_loan_cleaned_test$loan_status != "Fully Paid", "Default") 

#we now only have two levels, default and fully paid

write.csv(x = df_loan_cleaned_test, file = "loan_eval_clean.csv")

df_loan_cleaned_sample_test <- read.csv("loan_eval_clean.csv",sep = ",", header = TRUE)

df_loan_cleaned_sample_test <- within(df_loan_cleaned_sample_test, rm('X'))

df_loan_cleaned_sample_test <- df_loan_cleaned_sample_test[complete.cases(df_loan_cleaned_sample_test), ]

#prepare the loan_status as categorical value where 1 == Default
# Bit complicated but works for now
df_test_labels <- dummy_cols(df_loan_cleaned_sample_test, select_columns = 'loan_status', remove_selected_columns = TRUE)

write.csv(x = df_test_labels$loan_status_Default, file = "loan_eval_status_default.csv")

test_labels <- df_test_labels$loan_status_Default

# loan_status, we are done with you BYE!
df_loan_cleaned_sample_test <- within(df_loan_cleaned_sample_test, rm('loan_status'))


# all numerical values that are left are rescaled on a scale from 0 to 1
df_loan_cleaned_sample_test$loan_amnt <- bin(df_loan_cleaned_sample_test$loan_amnt, nbins = bins)
df_loan_cleaned_sample_test$annual_inc <- bin(df_loan_cleaned_sample_test$annual_inc, nbins = bins)
df_loan_cleaned_sample_test$installment <- bin(df_loan_cleaned_sample_test$installment, nbins = 150)
df_loan_cleaned_sample_test$dti <- bin(df_loan_cleaned_sample_test$dti, nbins = bins)
df_loan_cleaned_sample_test$out_prncp <- bin(df_loan_cleaned_sample_test$out_prncp, nbins = bins)
df_loan_cleaned_sample_test$delinq_2yrs <- bin(df_loan_cleaned_sample_test$delinq_2yrs, nbins = 14)
df_loan_cleaned_sample_test$total_rec_int <- bin(df_loan_cleaned_sample_test$total_rec_int, nbins = bins)
df_loan_cleaned_sample_test$total_rec_prncp <- bin(df_loan_cleaned_sample_test$total_rec_prncp, nbins = bins)


df_loan_cleaned_sample_test$issue_d <- substr(df_loan_cleaned_sample_test$issue_d,5, 9)

#we have one more level in home_ownership which is any, this is converted to other
df_loan_cleaned_sample_test$home_ownership <- replace(df_loan_cleaned_sample_test$home_ownership, df_loan_cleaned_sample_test$home_ownership == "ANY", "OTHER") 


# all categorical are converted to dummies like the mushrooms
df_test_features <- dummy_cols(df_loan_cleaned_sample_test, select_columns = c('installment','total_rec_int','total_rec_prncp','out_prncp', 'issue_d', 'dti','delinq_2yrs','annual_inc','loan_amnt','term','sub_grade','emp_length', 'home_ownership', 'verification_status', 'purpose'), remove_selected_columns = TRUE)

# here a level of delinq_2yrs is removed to match the input 
df_test_features <- within(df_test_features, rm('int_rate'))

write.csv(x = df_test_features, file = "eval_features.csv")

#load the model
model <- load_model_hdf5("ff_nn_model", compile = TRUE)

dm_test_labels <- to_categorical(test_labels)
dm_test_features <- data.matrix(df_test_features)


result <- model %>% evaluate(dm_test_features,dm_test_labels, verbose = 1)


#########################################################
##
## Write the score sheet
##
#########################################################


df_eval <- data.frame(matrix(ncol = 1, nrow = 1))
df_eval <- data.frame(
  df_eval$network_type <- 'Feed Forward',
  df_eval$comment <- '',
  df_eval$bins <- bins,
  df_eval$epochs <- epochs,
  df_eval$hidden_layers <- 4,
  df_eval$neurons_layer1 <- 600,
  df_eval$neurons_layer2 <- 400,
  df_eval$neurons_layer3 <- 200,
  df_eval$neurons_layer4 <- 50,
  df_eval$neurons_layer5 <- NA,
  df_eval$neurons_layer6 <- NA,
  df_eval$neurons_layer7 <- NA,
  df_eval$optimizer <- 'rms_prop',
  df_eval$train_loss_av <- train_loss_av,
  df_eval$train_acc_av <- train_acc_av,
  df_eval$val_loss_av  <- val_loss_av,
  df_eval$val_acc_av <- val_acc_av,
  df_eval$test_loss <- result[1],
  df_eval$test_acc <- result[2]
)

# lets have a look at it
View(df_eval)


write.table( df_eval,  
             file="score_sheet", 
             append = T, 
             sep=',', 
             row.names=F, 
             col.names=F )












