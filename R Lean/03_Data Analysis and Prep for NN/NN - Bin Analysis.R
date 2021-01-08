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
library('keras')
library('fastDummies')
library('OneR')
library("scales")

############################################################ Data for Training and Testing #############################################################
df_loan <- read.csv("../regression_train_loan.csv",sep = ",", header = TRUE)
loan_cleaned_I <- df_loan[, -which(colMeans(is.na(df_loan)) > 0.1)]
df_loan_cleaned <- df_loan_cleaned %>% filter(loan_status != "Current")
df_loan_cleaned$loan_status <- replace(df_loan_cleaned$loan_status, df_loan_cleaned$loan_status != "Fully Paid", "Default") 
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
df_loan_cleaned <- df_loan_cleaned[complete.cases(df_loan_cleaned), ]
df_train_labels <- dummy_cols(df_loan_cleaned, select_columns = 'loan_status', remove_selected_columns = TRUE)
train_labels <- df_train_labels$loan_status_Default
df_loan_cleaned <- within(df_loan_cleaned, rm('loan_status'))

############################################################ Data for Evaluation #############################################################
df_loan_eval <- read.csv("../loan_eval.csv",sep = ",", header = TRUE)
df_loan_eval <- df_loan_eval[, -which(colMeans(is.na(df_loan_eval)) > 0.1)]
df_loan_eval <- within(df_loan_eval, rm('X',
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

df_loan_eval <- df_loan_eval %>% filter(loan_status != "Current")
df_loan_eval$loan_status <- replace(df_loan_eval$loan_status, df_loan_eval$loan_status != "Fully Paid", "Default") 
df_loan_eval <- df_loan_eval[complete.cases(df_loan_eval), ]
df_test_labels <- dummy_cols(df_loan_eval, select_columns = 'loan_status', remove_selected_columns = TRUE)
test_labels <- df_test_labels$loan_status_Default
df_loan_eval <- within(df_loan_eval, rm('loan_status'))

############################################################ Comparison #############################################################

compare <- data.frame("variable" = "loan_amount", "Train_MIN" = min(df_loan_cleaned$loan_amnt), "Train_MAX" = max(df_loan_cleaned$loan_amnt), "Test_Min" = min(df_loan_eval$loan_amnt), "Test_MAX" = max(df_loan_eval$loan_amnt))
compare <- compare %>% add_row("variable" = "annual_inc", "Train_MIN" = min(df_loan_cleaned$annual_inc), "Train_MAX" = max(df_loan_cleaned$annual_inc), "Test_Min" = min(df_loan_eval$annual_inc), "Test_MAX" = max(df_loan_eval$annual_inc))
compare <- compare %>% add_row("variable" = "installment", "Train_MIN" = min(df_loan_cleaned$installment), "Train_MAX" = max(df_loan_cleaned$installment), "Test_Min" = min(df_loan_eval$installment), "Test_MAX" = max(df_loan_eval$installment))
compare <- compare %>% add_row("variable" = "dti", "Train_MIN" = min(df_loan_cleaned$dti), "Train_MAX" = max(df_loan_cleaned$dti), "Test_Min" = min(df_loan_eval$dti), "Test_MAX" = max(df_loan_eval$dti))
compare <- compare %>% add_row("variable" = "delinq_2yrs", "Train_MIN" = min(df_loan_cleaned$delinq_2yrs), "Train_MAX" = max(df_loan_cleaned$delinq_2yrs), "Test_Min" = min(df_loan_eval$delinq_2yrs), "Test_MAX" = max(df_loan_eval$delinq_2yrs))
compare <- compare %>% add_row("variable" = "out_prncp", "Train_MIN" = min(df_loan_cleaned$out_prncp), "Train_MAX" = max(df_loan_cleaned$out_prncp), "Test_Min" = min(df_loan_eval$out_prncp), "Test_MAX" = max(df_loan_eval$out_prncp))
compare <- compare %>% add_row("variable" = "total_rec_int", "Train_MIN" = min(df_loan_cleaned$total_rec_int), "Train_MAX" = max(df_loan_cleaned$total_rec_int), "Test_Min" = min(df_loan_eval$total_rec_int), "Test_MAX" = max(df_loan_eval$total_rec_int))
compare <- compare %>% add_row("variable" = "total_rec_prncp", "Train_MIN" = min(df_loan_cleaned$total_rec_prncp), "Train_MAX" = max(df_loan_cleaned$total_rec_prncp), "Test_Min" = min(df_loan_eval$total_rec_prncp), "Test_MAX" = max(df_loan_eval$total_rec_prncp))

