library(tidyverse)
library(psych)
rm(list = ls())

# Distributions ----------------------------------------------------------
# Standard Error is the SD of sample means- larger scale
# Confidence Interval 

# Hypothesis Testing ------------------------------------------------------
# Alternative hypothesis-what theory predicts will be true
# The Null Hypothesis - what the theory predicts will be false

# Type 1 rejecting a true NULL-alpha level of significance-false positive
# Type 2 accepting a false NULL-beta - false negative; 1-beta
# the power of the test-increase the sample set.

# Test the mean - population variance known -------------------------------
# data is drawn from a single population-variance, mean is known

sal <- read.csv("ztest-a.csv")
sal

summary(sal)
describe(sal)

# we need 2 sided test-if the mean salary is signif less or higher H0=113000
# we need to standardize and and z score

# create a function for Z number

z.test <- function(a, mu, sd){
  zeta = (mean(a) - mu) / (sd/sqrt(length(a)))
  return(zeta)
}

z.test(a = sal$salary, mu = 113000, sd = 15000)
# our z small from the table is z = 1.96 because is a two sided test,
# our significance alpha =0.05 become 0.025; 1-0.025 = 0.975 this is the 
# number we look for in z table: z = 196 from the table.
# 4.67 > 1.96 we reject the null hypothesis: H0.

# Practice Exercise-Test for the mean population at 10% significant --------
# sample mean 100200
# population sd: 150000
# standard error: 2739
# sample size:30
# z-score: -4.67
# h0: glssdoor: 113000
# two sided test

z.number <- function(b, mu, sd){
  zeta = (mean(b) - mu) / (sd/sqrt(length(b)))
  return(zeta)
}

z.number(b = sal$salary, mu = 113000, sd = 15000)

# z = 1.96 from the z table
# 4.67 > 1.96 then the H0 is rejected

se <- function(x, sd){
  sigma = (sd/sqrt(length(x)))
  return(sigma)
}

se(x = sal$salary, sd = 2739)

# The p - value the smallest level of significance we can reject  --------
# 1 minus the nr from the z table 

# Test for the mean -Population Variance unknown --------------------------
# we are looking for the t number or t test

rate <- read.csv("ttest-a.csv",stringsAsFactors = F)
rate$Rate <- rate$Open.rate
rate$Open.rate = NULL

summary(rate)
describe(rate)

# null hypothesis is H0: hmean<=0.4 (40%); alternative hypothesis H1=0.4.
# one sided test and alpha = 0.05
# sample population variance is not known we use student t distribution

my.t.test <- function(a, hmean){
  t <- (mean(a) - hmean) / (sd(a) / sqrt(length(a)))
  return(t)
}

# (sd(a)/sqrt(length(a)))- is the actual standard error

my.t.test(a=rate$Rate, 0.4)

# T = 0.53; df = n-1 = 9 ;alpha = 0.05 one sided test; 
# look in T table at the intersection of df with alpha;; results
# t = 1.83
# 0.5295018 < 1.83 if the absolute value is smaller than the value from the
# table we can not reject the Null hypothesis H0.
# p calculated eternally is p = 0.065 and because p > alpha we fail to reject H0


# Compare the means of multiple populations-Dependent Samples -----------

#Hypothesis is: H0: mub >= mua; alternative Hypo: h1: mub < mua
# we expect the levels of Mg to be increasing. Therefore, this is the alternative 
# hypothesis as we are aiming to reject the null.
# mub >= mua results in mub - mua >=0 we consider mub - mua =D0 than:
# h0:D0 >= 0 and H1: D0 < 0; D0 stand for the population mean differences.

install.packages("pastecs")
library(pastecs)
library(psych)

mgn <- read.csv("dependent-samples.csv")
mgn

describe(mgn)
# we apply T test for dependent sample
# assuming similar variance

dep.t.test <- t.test(mgn$Before,mgn$After,paired = T,alternative = "l")
dep.t.test
# p is smaller than alpha 0.0237 < 0.05 so we can reject the H0 null
# but we fail to reject if alpha = 001

stat.desc(mgn) # from pastecs package

wght <- read.csv("weight_data_exercise_kg.csv")
wght

wght.t.test <- t.test(wght$before,wght$after, paired = T,alternative = "l")
wght.t.test

#h0:the difference between before and after is < = 0; D0 <= 0
#the test is one sided test
#p value is 1-(p-value) from our wght.t.test
# we can reject the null hyp. at significance interval alpha = 0.05
# at alpha = 0.01 we accept the null hypothesis , the program is not working.
stat.desc(wght)
describe(wght)

p.value <-  0.9621
p <- 1-p.value
p

# Comparing two mean -Independent Variables -------------------------------
# Ho: the diferences between the two means is: mue - mum = -4
# H1: mue - mum different than -4
# two sided test

grades <- read_csv("independent-samples.csv")
grades

glimpse(grades)
describe(grades)
stat.desc(grades)
#----------------------------------
notes <- grades %>% 
  pivot_longer(
    1:2,
    names_to = "Course",
    values_to = "Grades"
  ) %>% 
  arrange(Course)
notes

ind.test <- t.test(data = notes, Grades~Course, mu=-4)
ind.test
#-------------------------------------
note <- grades %>% 
  gather(
    key = "course",
    value = "grades"
  )
note

ind.t.test <- t.test(data = note,grades~course,mu = -4)
ind.t.test

p.value = 0.01604
p <- 1-p.value
p

#result: estimation was off by a statistically significant amount,
#the difference in department performance is larger than 4%.


















































































