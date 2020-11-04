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


summary(df_loan)
glimpse(df_loan)

sum(duplicated(df_loan))
colSums(is.na(df_loan))


options(repr.plot.width=6, repr.plot.height=8)
missing_data <- df_loan %>% summarise_all(funs(sum(is.na(.))/n()))
missing_data <- gather(missing_data, key = "variables", value = "percent_missing") 
ggplot(missing_data, aes(x = reorder(variables, percent_missing), y = percent_missing)) +
  geom_bar(stat = "identity", fill = "red", aes(color = I('white')), size = 0.1)+coord_flip()+ theme_few()



