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
df_loan <- read.csv("../regression_train_loan.csv",sep = ",", header = TRUE)

# Checking summary of the data and getting first insights
summary(df_loan)
glimpse(df_loan) 

# Check for duplicates
sum(duplicated(df_loan))

#################################################
##
## I.I FIRST DATA CLEANING
##     Based on insights above and further manual analysis 
##     summary, boxplot, histogram per column if this was possible - see Excel Sheet
##     Done to avoid unnecessary 
##
#################################################

## Further check for NA values to be sure that we can delete them 
colSums(is.na(df_loan))

## Visualization of the number of NA values
options(repr.plot.width=6, repr.plot.height=8)
missing_data <- df_loan %>% summarise_all(funs(sum(is.na(.))/n()))
missing_data <- gather(missing_data, key = "variables", value = "percent_missing") 
ggplot(missing_data, 
       aes(x = reorder(variables, percent_missing), 
           y = percent_missing)) + geom_bar(stat = "identity", 
                                            fill = "red", 
                                            aes(color = I('white')), 
                                            size = 0.1)+coord_flip() + theme_few()

####
##
## get rid of columns having more than 10% of missing data
## first step of cleaning 
##
####
loan_cleaned_I <- df_loan[, -which(colMeans(is.na(df_loan)) > 0.1)]


## Check again visually  
missing_data <- loan_cleaned_I %>% summarise_all(funs(sum(is.na(.))/n()))
missing_data <- gather(missing_data, key = "variables", value = "percent_missing") 
ggplot(missing_data, 
       aes(x = reorder(variables, percent_missing), 
           y = percent_missing)) + geom_bar(stat = "identity", 
                                            fill = "red", 
                                            aes(color = I('white')), 
                                            size = 0.1)+coord_flip() + theme_few()
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

## Check again visually  
missing_data <- df_loan_cleaned %>% summarise_all(funs(sum(is.na(.))/n()))
missing_data <- gather(missing_data, key = "variables", value = "percent_missing") 
ggplot(missing_data, 
       aes(x = reorder(variables, percent_missing), 
           y = percent_missing)) + geom_bar(stat = "identity", 
                                            fill = "red", 
                                            aes(color = I('white')), 
                                            size = 0.1)+coord_flip() + theme_few()
######
##
## write a file of the cleaned set
##
######

write.csv(x = df_loan_cleaned, file = "../regression_loan_cleaned.csv")
save(df_loan_cleaned, file = "df_loan_cleaned.rda")
######
## 
## sample the set for further analysis on smaller data set and store this in a file as well
## 
######
set.seed(1)
df_sample <- sample_n(df_loan_cleaned,10000)
#write.csv(x = df_sample, file = "regression_train_loan_sample_cleaned.csv")
save(df_sample, file = "df_loan_sample.rda")

# ===================================================================================================
# ===================================================================================================
# ===================================================================================================
#
# II. Data Analysis on the prepared data
#
# ===================================================================================================
# ===================================================================================================
# ===================================================================================================


## CLEANUP ## 
## Must not be performed - but could improve performance!
## important to set workspace to the folder of the csv file

#rm(list = ls())
#load("df_loan_sample.rda")
#load("df_loan_cleaned.rda")
#df_loan_sample <- read.csv("regression_train_loan_sample_cleaned.csv",sep = ",", header = TRUE)


#################################################
##
## II.I Data type correction
##
#################################################
#dataset_to_analyze <- df_loan_sample
dataset_to_analyze <- df_loan_cleaned

# converting incorrect data types to be able to work with them as factor variables
dataset_to_analyze$sub_grade <- as.factor(dataset_to_analyze$sub_grade)
dataset_to_analyze$term <- as.factor(dataset_to_analyze$term)
dataset_to_analyze$emp_length <- as.factor(dataset_to_analyze$emp_length)
dataset_to_analyze$home_ownership <- as.factor(dataset_to_analyze$home_ownership)
dataset_to_analyze$verification_status <- as.factor(dataset_to_analyze$verification_status)
dataset_to_analyze$loan_status <- as.factor(dataset_to_analyze$loan_status)
dataset_to_analyze$application_type <- as.factor(dataset_to_analyze$application_type)
dataset_to_analyze$initial_list_status <- as.factor(dataset_to_analyze$initial_list_status)

# converting dates from string values
dataset_to_analyze$issue_d <- parse_date(as.character(dataset_to_analyze$issue_d), format =  "%b-%Y")


#################################################
##
## II.II Visual analysis of the sample set
##       Analyze which data can be used for prediction and which might not help enough regarding the interest rate
##    
##       Performed on the sample data set
##       (can be performed on all data if the input df is called dataset_to_analize and cleaned as shown above)
##
#################################################



# General analysis of all continuos variables left in the data set with a correlation analysis
dataset_to_analyze %>% keep(is.numeric) %>% gather() %>%  ggplot(aes(value)) + 
  facet_wrap(~ key, scales = "free") +
  geom_histogram(bins=20, color= "black", fill= "#3399FF")

options(repr.plot.width=8, repr.plot.height=6)
corrplot(cor(dataset_to_analyze[,unlist(lapply(dataset_to_analyze, is.numeric))], 
             use = "complete.obs"), type = "lower", method = "number")
# --> the amount of the loan has the highest impact on the interest rate in this illustration 

## Further analysis based on this insights and the not yet covered variables which might have an impact on the interest rate:
# Getting insights about the status of the loans in the dataset
dataset_to_analyze.loan.distribution <- dataset_to_analyze %>% group_by(loan_status) %>% dplyr::summarise(count=n()) %>% mutate(pct=count/sum(count))
ggplot(dataset_to_analyze.loan.distribution, 
       aes(x=reorder(loan_status, pct), 
           y=pct, colour=loan_status, fill=loan_status)) +
      geom_bar(stat="identity",
           aes(color = I('black')), 
           size = 0.1) + coord_flip() + theme(legend.position = "none")+ xlab("Loan status") + ylab("Percentage in the dataset in the loan status")
# --> Still a lot of currently running loans in the dataset

# Insights in Loan status in relation to the interest rate
ggplot(dataset_to_analyze, aes(x= int_rate, fill = loan_status)) +
  geom_histogram(bins = 10, position = "fill", aes(color = I('black')), size = 0.1)+ 
  xlab("Interest Rate")+ 
  ylab("Percent of default Vs No default")+theme_few()
ggplot(dataset_to_analyze, aes(x = loan_status, y = int_rate, fill = loan_status)) + geom_boxplot()
# --> High interest rate is more likely resulting in defaults

# Insights about the relationship of interest rate and the status for verification ans the general loan status
plot(dataset_to_analyze$int_rate~dataset_to_analyze$verification_status)
plot(dataset_to_analyze$int_rate~dataset_to_analyze$loan_status)
# --> both seem to be significant in the relation to the interest rate and should be considered in the regression model

# Further insights in loan status in relation to the sub_grade
options(repr.plot.width=6, repr.plot.height=4)
ggplot(dataset_to_analyze, aes(x = sub_grade, fill = loan_status)) + geom_bar(stat='count', position='fill', aes(color = I('black')), size = 0.1) + 
  labs(x = 'Sub Grade') + scale_fill_discrete(name="Loan_Status") + theme_few()
# --> Default rate increases with a worse sub grade rating --> might be a possible factor to consider

# To check the aspect of time on interest in regard of the grade:
#dataset_to_analyze$grade <- substr(dataset_to_analyze$sub_grade, 1,1)
int_rate_per_year_and_grade <- aggregate(dataset_to_analyze$int_rate, 
                                         list(format(dataset_to_analyze$issue_d, format = "%Y"), 
                                              substr(dataset_to_analyze$sub_grade, 1,1)), 
                                         mean)
# minor cleanup to work with spelling names
int_rate_per_year_and_grade$Year <- int_rate_per_year_and_grade$Group.1
int_rate_per_year_and_grade$Grade <- int_rate_per_year_and_grade$Group.2
int_rate_per_year_and_grade$mean_int_rate <- int_rate_per_year_and_grade$x
int_rate_per_year_and_grade <- within(int_rate_per_year_and_grade, rm("Group.1", "Group.2", "x"))

# GGPLOT examples:
# http://www.sthda.com/english/wiki/ggplot2-line-types-how-to-change-line-types-of-a-graph-in-r-software
# http://www.sthda.com/english/wiki/ggplot2-title-main-axis-and-legend-titles
ggplot(data=int_rate_per_year_and_grade, aes(x=Year, y=mean_int_rate, group = Grade)) +
  geom_line(aes(linetype = Grade, color = Grade)) + 
  geom_point(color="red") +
  ggtitle("Development of the mean interest rate per group over the years") + 
  xlab("Year") + 
  ylab("Mean of Interest rate per Grade")+
  theme(legend.position="right") 
# --> In addition to the grade itself the year of the application also has an impact on the interest rate and should be considered


# Insights in Loan status in relation to the interest rate regardless of the loan status
ggplot(dataset_to_analyze, aes(x = sub_grade, y = int_rate, fill = sub_grade)) + geom_boxplot()
# --> Sub grade is highly affecting the interest ratings --> should be considered as a factor

# Further insights in loan status in relation to the loan amount
options(repr.plot.width=6, repr.plot.height=4)
ggplot(dataset_to_analyze, aes(x= loan_amnt)) + geom_density(aes(fill = as.factor(loan_status))) + xlab("Loan_amount")+theme_few()
# --> Default peak slightly above a loan amount of 10'000 - could be considered in the interest rate calculation

# Analysis based on puprose and the relation to the interest rate
dataset_to_analyze %>% group_by(purpose) %>% dplyr::summarise(count=n()) %>% mutate(pct=count/sum(count))%>% 
  ggplot(aes(x = reorder(purpose, pct), y = pct)) + geom_bar(stat = "identity", fill = "purple", aes(color = I('black')), size = 0.1) + 
  xlab("Purpose of Loan") + ylab("Percent")+ coord_flip()+theme_few()
plot(dataset_to_analyze$int_rate~dataset_to_analyze$purpose)
# --> debt consolidation accounts for 60% of the loans 
# --> the ranges of the interest rates seem only slightly affected by the purpose except for educational loans

# Impact analysis of the loan status based on the home ownership
ggplot(dataset_to_analyze, aes(x =home_ownership, fill = loan_status)) + 
  geom_bar(stat='count', position='fill', aes(color = I('black')), size = 0.1) +labs(x = 'home_ownership') +
  scale_fill_discrete(name="Loan_Status") +theme_few()
# --> Almost no impact regarding the default behavior considering the home ownership status 
plot(dataset_to_analyze$int_rate~dataset_to_analyze$home_ownership)
# --> also almost no difference in the interest rates based on the kind of home ownership 

# Impact analysis of the loan status considering the employment length
ggplot(filter(dataset_to_analyze, emp_length != 'n/a'), aes(x =emp_length, fill = loan_status)) + 
  geom_bar(stat='count', position='fill', aes(color = I('black')), size = 0.1) +labs(x = 'emp_length') + 
  scale_fill_discrete(name="Loan_Status") + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.8, hjust=1))
# --> the length of the employment has almost no impact on the loan status default

# Impact analysis of the interest rate considering the employment length
plot(dataset_to_analyze$int_rate~dataset_to_analyze$emp_length)
# --> the length of the employment has almost no impact on the interest rate

# Default with respect to Term: The percentage of default in case of loans with 60 months term is higher as compared to the loans with 36 months term.
options(repr.plot.width=6, repr.plot.height=4)
#i. Term and Loan Status
ggplot(dataset_to_analyze, aes(x =term, fill = loan_status)) + geom_bar(stat='count', position='fill', aes(color = I('black')), size = 0.1) + 
  labs(x = 'Term') + 
  ylab("Percent of default Vs No default") +theme_few()
#Loans with 60 months term get defaulted more as compared to 36 months term

# Checking for the term distribution 
options(repr.plot.width=5, repr.plot.height=3)
dataset_to_analyze %>% group_by(term) %>% dplyr::summarise(count=n()) %>% mutate(pct=count/sum(count))%>% 
  ggplot(aes(x= term, y = pct)) + geom_bar(stat = "identity", fill = "purple", aes(color = I('black')), size = 0.1)+xlab("Term") + 
  ylab("Percent")+ theme_few()
# --> most of the loans are issued for 36 months


# Distribution of loans regarding the states
options(repr.plot.width=6, repr.plot.height=6)
dataset_to_analyze %>% group_by(addr_state) %>% dplyr::summarise(count=n()) %>% mutate(pct=count/sum(count))%>% 
  ggplot(aes(x = reorder(addr_state, pct), y = pct)) + geom_bar(stat = "identity", fill = "lightblue2", aes(color = I('black')), size = 0.1) + 
  xlab("States where the loans were issued") + ylab("Relative number of loans in the dataset ")+ coord_flip()+theme_few()
# --> most of the loans are applied in California, followed by Texas and New York
plot(dataset_to_analyze$int_rate~dataset_to_analyze$addr_state)
# --> in general - except some outliers - the state does not except the ranges of the interest rate 


#################################################
##
##
##  II.III Investigation through the creation of a linear model 
##
##
#################################################

linear_fit <- lm(data = dataset_to_analyze, formula = int_rate~.)
summary(linear_fit)
# ==> take all seemingly important variables 

linear_fit.II <- lm(data = dataset_to_analyze, formula = int_rate~sub_grade+verification_status+term+issue_d)
summary(linear_fit.II)

linear_fit.III <- lm(data = dataset_to_analyze, formula = int_rate~sub_grade+home_ownership)
summary(linear_fit.III)

##################################################################################################
##################################################################################################
##################################################################################################
# in case of NA replace NA by the moste used sub_grade to avoid errors in the prediction and model creation
#is.na(dataset_to_analyze$sub_grade)
dataset_to_analyze$sub_grade <- replace_na(dataset_to_analyze$sub_grade,replace = "B3")
dataset_to_analyze.loan.sub_grade <- dataset_to_analyze %>% group_by(sub_grade) %>% dplyr::summarise(count=n()) %>% mutate(pct=count/sum(count))
ggplot(dataset_to_analyze.loan.sub_grade, 
       aes(x=reorder(sub_grade, pct), 
           y=pct, colour=sub_grade, fill=sub_grade)) +
      geom_bar(stat="identity",
           aes(color = I('black')), 
           size = 0.1) + coord_flip() + theme(legend.position = "none")+ xlab("Loan status") + ylab("Percentage in the dataset in the loan status")
