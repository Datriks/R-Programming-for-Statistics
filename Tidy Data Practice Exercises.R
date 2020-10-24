library(tidyverse)

# WEATHER UNTIDY DATA -----------------------------------------------------

?parse_number
parse_number("$1000")

wdf <- as_tibble(read.csv("weather_untidy.csv"))
wdf

names(wdf)

wdf1 <- wdf %>% 
  pivot_longer(
    d1:d31,
    names_to = "day",
    values_to = "values",
    values_drop_na = T
  ) %>% 
  mutate(
    day = as.integer(gsub("d","",day))
  ) %>% 
  select(
    id,year,month,day,element,values
  )
wdf1

wdf2 <- wdf1 %>% 
  pivot_wider(
    names_from = "element",
    values_from = "values"
  ) %>% 
  mutate(
    Tdif = TMAX - TMIN
  )

wdf2

# TB UNTIDY DATA ----------------------------------------------------------

tb <- as_tibble(read.csv("tb_untidy.csv"))
tb

names(tb)
glimpse(tb)

tb1 <- tb %>% 
  pivot_longer(
    !c('country','year'),
    names_to = "description",
    values_to = "cases",
    values_drop_na = T
  ) %>% 
  separate(
    description,c("gender","age"),1
  )
tb1

tb2 <- tb1 %>% 
  group_by(gender) %>% 
  summarise(
    cases.nr = sum(cases)
  )
tb2

tb3 <- tb1 %>% 
  mutate(
    age = str_replace_all(age,"0","0-"),
    age = str_replace_all(age,"15","15-"),
    age = str_replace_all(age,"25","25-"),
    age = str_replace_all(age,"35","35-"),
    age = str_replace_all(age,"45","45-"),
    age = str_replace_all(age,"55","55-"),
    age = str_replace_all(age,"65","65-100")
  )

tb3

tb4 <- tb3 %>% 
  separate(
    age,c('min.age','max.age'),sep = '-'
  )
tb4

rm(list = ls())
































































































