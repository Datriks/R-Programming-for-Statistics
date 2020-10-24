# Tidy Data with tidyr ----------------------------------------------------

library(tidyverse)

rm(list = ls())

#gather; spread; unite; separate; pivot_longer' pivot_wider;

billboard
glimpse(billboard)
colnames(billboard)

billboard %>% 
  pivot_longer(
    wk1:wk76,
    names_to = "week",
    values_to = "rank",
    values_drop_na = T
  ) %>% 
  arrange(artist)

bill1 <- billboard %>% 
  pivot_longer(
    wk1:wk76,
    names_to = "week",
    values_to = "rank",
    values_drop_na =T
  ) %>% 
  arrange(artist)

bill2 <- bill1 %>% 
  mutate(
    week = as.integer(gsub("wk","",week)),
    date = as.Date(date.entered)-7 * (week-1),
    date.entered = NULL
  )
bill2

# Pivot_longer and Separate functions -------------------------------------


tb <- as_tibble(read.csv("tb.csv",stringsAsFactors = FALSE))
tb

glimpse(tb)

tb1 <- tb %>% 
  pivot_longer(
    !c('country','year'),
    names_to = "column",
    values_to = "cases",
    values_drop_na = T
  ) %>% 
  arrange(country)
tb1

tb2 <- tb1 %>% 
  separate(
    column,into = c("gender","age"),1
  )
tb2

tb3 <- tb2 %>% 
  separate(age, into = c("dot","age"),1) %>% 
  mutate(
    dot = NULL
  )

tb4 <- tb3 %>% 
  mutate(
    age = str_replace_all(age,"0","0-"),
    age = str_replace_all(age,"15","15-"),
    age = str_replace_all(age,"25","25-"),
    age = str_replace_all(age,"35","35-"),
    age = str_replace_all(age,"45","45-"),
    age = str_replace_all(age,"55","55-"),
    age = str_replace_all(age,"65","65-100")
  )
tb4

tb5 <- tb4 %>% 
  separate(
    age,c("age.low","age.high")
  )
tb5

tb6 <- tb5 %>% 
  unite(
    new.age,c('age.low','age.high'),sep = '-'
  )
tb6

# Pivot-wider or Spread ---------------------------------------------------

weather <- as_tibble(read.csv("weather.csv",stringsAsFactors = FALSE))
weather

any(is.na(weather))

glimpse(weather)

weather1 <- weather %>% 
  pivot_wider(
    
    names_from = "element",
    values_from = "value",
  ) %>% 
  mutate(
    Tdif = TMAX-TMIN
    )
weather1

# Practice-Exercises ------------------------------------------------------






















































































