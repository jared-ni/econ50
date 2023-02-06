rm(list=ls())
cat('\014')


# note: to run, must change this line to cd into the right directory, and have the data file in the same directory
setwd("/Users/jaredhn/codespace/econ50/lab1")

# install packages
if (!require(tidyverse)) install.packages("tidyverse"); library(tidyverse)
if (!require(haven)) install.packages("haven"); library(haven)
if (!require(ggplot2)) install.packages("ggplot2"); library(ggplot2)
if (!require(statar)) install.packages("statar"); library(statar)

# import data set (must include in the same directory)
nlsy <- read_dta("nlsy97.dta")

# problem 1: histogram
hist(nlsy$kid_income, probability = T, 
     main="Histogram of Kid Income", xlab="Kid income ($)")

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
kid_income_rank <- rank(nlsy$kid_income)

# kid income
# b) sort the data by kid_income

kid_income_rank <- kid_income_rank[order(nlsy$kid_income)]
print(min(nlsy$kid_income))
print(max(nlsy$kid_income))
nlsy$kid_income[order(nlsy$kid_income)][5476:5486]
print(nlsy$kid_income[1:10])
print(nlsy$kid_income[5476:5486])
print(kid_income_rank[0:10])
print(kid_income_rank[5476:5486])

# c) 
max_rank <- max(kid_income_rank)
# normalize
kid_inc_rank <- 100 * kid_income_rank / max_rank
print(max_rank)
print(kid_inc_rank[0:10])
print(kid_inc_rank[5476:5486])

# d) observation

# problem 8: histogram of kid_inc_rank
# a) uniform histogram
hist(kid_inc_rank, probability = T, 
     main="Histogram of Kid Income", xlab="Kid income ($)")
# b) sample mean ~= sample median
rank_mean <- mean(kid_inc_rank, na.rm = TRUE) #50.087
rank_median <- median(kid_inc_rank, na.rm = TRUE)

# problem 9: find linear variables related to kid_income

# education vs. kid income
ggplot(nlsy, aes(x = child_education, y = kid_income)) +
  stat_smooth(method = "lm", se = FALSE) + 
  stat_binmean(n = 100, geom = "point") + 
  xlab("Kid Education (years)") + ylab("Kid Income ($)") + 
  ggtitle("Kid Education vs. Kid Income") + 
  theme(plot.title = element_text(size="14", face="bold", hjust = 0.5))

# SAT score vs. kid income
ggplot(nlsy, aes(x = child_sat, y = kid_income)) +
  stat_smooth(method = "lm", se = FALSE) + 
  stat_binmean(n = 100, geom = "point") + 
  xlab("Kid SAT score (out of 1600)") + ylab("Kid Income ($)") + 
  ggtitle("Kid SAT score vs. Kid Income") + 
  theme(plot.title = element_text(size="14", face="bold", hjust = 0.5))

# parents' income vs kid income
ggplot(nlsy, aes(x = parent_inc, y = kid_income)) +
  stat_smooth(method = "lm", se = FALSE) + 
  stat_binmean(n = 100, geom = "point") + 
  xlab("Parent Income ($)") + ylab("Kid Income ($)") + 
  ggtitle("Parent Income vs. Kid Income") + 
  theme(plot.title = element_text(size="14", face="bold", hjust = 0.5))


# problem 10
# a) 
set.seed(71535355)
# b) 
random_number <- runif(length(nlsy$kid_income))
treatment_group <- ifelse(random_number >= 0.5, 1, 0)
# number in control and treatment groups:
controlled <- sum(1-treatment_group)
treatments <- sum(treatment_group)

#c) compute sample mean and sd for treatment and control. 1 = treatment, 0 = control

# incarcerated. category 0: 0.0905, 1: 0.109
tapply(nlsy$incarcerated, treatment_group, mean)
tapply(nlsy$incarcerated, treatment_group, sd)

# child education
tapply(nlsy$child_education, treatment_group, mean)
tapply(nlsy$child_education, treatment_group, sd)

# child college
tapply(nlsy$child_college, treatment_group, mean)
tapply(nlsy$child_college, treatment_group, sd)

# child sat
tapply(nlsy$child_sat, treatment_group, mean)
tapply(nlsy$child_sat, treatment_group, sd)

# parent income
tapply(nlsy$parent_inc, treatment_group, mean)
tapply(nlsy$parent_inc, treatment_group, sd)

# mother education
tapply(nlsy$mother_education, treatment_group, mean)
tapply(nlsy$mother_education, treatment_group, sd)

# father education
tapply(nlsy$father_education, treatment_group, mean)
tapply(nlsy$father_education, treatment_group, sd)

# female
tapply(nlsy$female, treatment_group, mean)
tapply(nlsy$female, treatment_group, sd)

# black 
tapply(nlsy$black, treatment_group, mean)
tapply(nlsy$black, treatment_group, sd)

# hispanic
tapply(nlsy$hispanic, treatment_group, mean)
tapply(nlsy$hispanic, treatment_group, sd)

# white
tapply(nlsy$white, treatment_group, mean)
tapply(nlsy$white, treatment_group, sd)

# region
tapply(nlsy$region, treatment_group, mean)
tapply(nlsy$region, treatment_group, sd)

# age in year 2015
tapply(nlsy$age2015, treatment_group, mean)
tapply(nlsy$age2015, treatment_group, sd)

# cohort
tapply(nlsy$cohort, treatment_group, mean)
tapply(nlsy$cohort, treatment_group, sd)




