library(tidyverse)
library(pastecs)
library(psych)

#linear regression is a linear approximation of a causal relationship 
#between two or more variables


# Correlation and Regression -----------------------------------------------
#Correlation measures the relationship between 2 variables
#Regression measures the causality or how one var influences the other

college <- read_csv("regression_example.csv")
college

# college <- college %>% 
#   pivot_longer(
#     1:2,
#     names_to = "test",
#     values_to = "grades"
#   ) %>% 
#   arrange(test)

college

describe(college)
stat.desc(college)

linmod <- lm(data = college, GPA ~ SAT)
linmod

college %>% 
  ggplot(aes(SAT,GPA))+
  geom_point(  color = 'darkslategray',
               fill = 'white',
               size=3,
               shape = 21)+
  stat_smooth(se = F, method = "lm", color = '#890000')+
  theme_light()+
  labs(
    title = "SAT and GPA",
    x = "SAT Scores",
    y = "GPA Scores"
  )

summary(linmod)

# Exercise Linear Regression

real <- read_csv("real_estate_price_size_year_view.csv")

str(real)
stat.desc(real)
glimpse(real)

real

real %>% 
  ggplot(aes(price,size))+
  geom_point()

real %>% 
  ggplot(aes(size,price))+
  geom_point(
    shape = 21,
    size = 2.5,
    color = '#002949',
    fill = 'white'
  )+
  stat_smooth(se=F,method = "lm",color = '#890000')+
  labs(
    title = 'Size to Price',
    x = 'Size',
    y = 'Price'
  )+
  scale_y_continuous(labels = scales::comma)+
  theme_light()

regres <- lm(data = real, price ~ size)
regres

summary(regres)




































































































