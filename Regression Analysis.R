library(tidyverse)
library(psych)
library(pastecs)

rm(list = ls())

real <- read_csv("real_estate_price_size_year_view.csv")

stat.desc(real)
summary(real)
glimpse(real)
describe(real)

# Size to Price Regression Analysis ---------------------------------------

real_price <- real %>% 
  ggplot(aes(price,size))+
  geom_point(
    shape =21,
    size = 2.5,
    fill = 'white',
    color = '#890000'
  )+
  stat_smooth(se = F,method = "lm", color = '#01322c')+
  labs(
    title = 'Regression Price to Size',
    x = 'Price',
    y = 'Size'
  )+
  scale_x_continuous(labels = scales::comma)+
  theme_light()

real_price

regres_analysis <- lm(data = real, price ~ size)
regres_analysis
summary(regres_analysis)

install.packages("FinCal",dependencies=TRUE)
library(FinCal)

ls("package:FinCal")

install.packages("tidyquant")
install.packages("knitr")



















































































































