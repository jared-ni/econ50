rm(list=ls())
cat('\014')

setwd("/Users/jaredhn/codespace/econ50/lab1")

# install packages
if (!require(tidyverse)) install.packages("tidyverse"); library(tidyverse)
if (!require(haven)) install.packages("haven"); library(haven)
if (!require(ggplot2)) install.packages("ggplot2"); library(ggplot2)
if (!require(statar)) install.packages("statar"); library(statar)

# import data set
nlsy <- read_dta("nlsy97.dta")

# problem 1: histogram
hist(nlsy$kid_income, probability = T, main="Kids' Income", xlab="Kid Income")

# problem 2: sample mean
mean_kid_income <- mean(nlsy$kid_income, na.rm=TRUE)

# problem 3: 
# a) calculate below_mean
below_mean <- ifelse(nlsy$kid_income < mean_kid_income, 1, 0)
# b) sample mean of below_mean
mean_below_mean <- mean(below_mean, na.rm=TRUE)

# problem 4: sample median
median_kid_income <- median(nlsy$kid_income, na.rm=TRUE)

# problem 5: standard deviation
sd_kid_income <- sd(nlsy$kid_income)

# problem 6: indicator values
# a) within 1 sd of mean
upper_bound <- mean_kid_income + sd_kid_income
lower_bound <- mean_kid_income - sd_kid_income
one_sd_of_mean <- ifelse(nlsy$kid_income <= upper_bound & 
                         nlsy$kid_income >= lower_bound, 1, 0)
one_sd_percentage <- mean(one_sd_of_mean, na.rm=TRUE)

# b) within 2 sd of mean 
upper_bound2 <- upper_bound + sd_kid_income
lower_bound2 <- lower_bound - sd_kid_income
two_sd_of_mean <- ifelse(nlsy$kid_income <= upper_bound2 & 
                           nlsy$kid_income >= lower_bound2, 1, 0)
two_sd_percentage <- mean(two_sd_of_mean, na.rm=TRUE)

# problem 7
# a) use rank() to generate new variable that = each observation's rank


