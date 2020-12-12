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
# I. Data Analysis in the context of the NN-classification
#
# (setwd before starting!)
#
# ===================================================================================================
# ===================================================================================================
# ===================================================================================================

df_loan <- read.csv("../regression_train_loan.csv",sep = ",", header = TRUE)
save(df_loan, file = "df_loan.RData")

#################################################
##
## Removing all columns already identified as not usable in the Excel 
## - some already removed by the previous operation
##
#################################################
df_loan <- within(df_loan, rm('X',
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


# Kevins approach, deletes even more:
df_loan <- within(df_loan, rm('X',
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


# rempve variables with more than 10% missing values
df_loan <- df_loan[, -which(colMeans(is.na(df_loan)) > 0.1)]

save(df_loan, file = "df_loan.RData")
# ------------------------------------------------------------------------------------------------------------------------------
#################################################
##
## Filtering for only the relevant loan_status values 
##
#################################################
load(file = "df_loan.RData")
# current can be filtered out because they will not provide info about the outcome of the loan
# Separate running (current) loans from the rest
df_loan_current <- df_loan %>% filter(loan_status == "Current")
df_loan <- df_loan %>% filter(loan_status != "Current")
# Checking for the different status distribution
df_loan.loan.distribution <- df_loan %>% group_by(loan_status) %>% dplyr::summarise(count=n()) %>% mutate(pct=count/sum(count))
ggplot(df_loan.loan.distribution, 
       aes(x=reorder(loan_status, pct), 
           y=pct, colour=loan_status, fill=loan_status)) +
  geom_bar(stat="identity",
           aes(color = I('black')), 
           size = 0.1) + coord_flip() + theme(legend.position = "none")+ xlab("Loan status") + ylab("Percentage in the dataset in the loan status")
# --> Relevant values should be "Fully Paid", "Default", "Charged Off", "Does not meet the credit policy. Status:Charged Off", "Does not meet the credit policy. Status:Fully Paid"
# --> Irrelevant because they are not concluded: "Issued", "Late (16-30 days)", "Late (31-120 days)","In Grace Period" 

# further filter based on this:
df_loan <- df_loan %>% filter(loan_status != "Issued")
df_loan <- df_loan %>% filter(loan_status != "Late (16-30 days)")
df_loan <- df_loan %>% filter(loan_status != "Late (31-120 days)")
df_loan <- df_loan %>% filter(loan_status != "In Grace Period")

# all that are not fully paid will be handled as defaulted -> two levels remain "Fully Paid" and "Default"
# Charged OFF = Default 
df_loan$loan_status <- replace(df_loan$loan_status, df_loan$loan_status != "Fully Paid", "Default") 
df_loan$loan_status <- factor(df_loan$loan_status)
# --> resulting in a distribution:
df_loan.loan.distribution <- df_loan %>% group_by(loan_status) %>% dplyr::summarise(count=n()) %>% mutate(pct=count/sum(count))
ggplot(df_loan.loan.distribution, 
       aes(x=reorder(loan_status, pct), 
           y=pct, colour=loan_status, fill=loan_status)) +
  geom_bar(stat="identity",
           aes(color = I('black')), 
           size = 0.1) + coord_flip() + theme(legend.position = "none")+ xlab("Loan status") + ylab("Percentage in the dataset in the loan status")

# iteration over every variable with the comparison to loan_status in a plot
variables <- variable.names(df_loan)
for( i in 1:ncol(df_loan)){
  jpeg(file=paste("plots/NN03/Relationship between loan status and ", variables[i],".jpeg"),width=1920, height=1200)
  plot(df_loan[,i] ~ df_loan$loan_status, 
       ylab = variables[i], 
       xlab = "Loan Status", 
       main = paste("Relationship between loan status and ", variables[i]))
  dev.off()
  
  jpeg(file=paste("plots/NN03/Inverted Relationship between loan status and ", variables[i],".jpeg"),width=1920, height=1200)
  plot(df_loan$loan_status ~ df_loan[,i], 
       xlab = variables[i], 
       ylab = "Loan Status", 
       main = paste("Inverted Relationship between loan status and ", variables[i]))
  dev.off()
  
}


