# Identifying the NAs in a data frame

library(tidyverse)
data()

ChickWeight %>% 
  summarise(
    count = sum(is.na(Time)),
    median = median(Time),
    mean = mean(Time)
  )

mtcars %>% 
  map(
    ~sum(is.na(.))
  )

mtcars %>% 
  map(
    ~mean(.)
  )

# Count NAs on rows -------------------------------------------------------


apply(mtcars, MARGIN = 1, function(x) sum(is.na(x)))

mtcars %>%
  rowwise %>%
  summarise(
    Row_NA = sum(is.na(.))
  )

mtcars %>%
  select(everything()) %>%  # replace to your needs
  summarise_all(funs(sum(is.na(.))))


mtcars %>% 
  select(everything()) %>% 
  summarise_all(funs(sum(is.na(.))))

mtcars %>% 
  map(
    ~sum(is.na(.))
  )

mtcars %>% 
  select(everything()) %>% 
  summarise_all(funs(median(.)))

mtcars %>% 
  select(everything()) %>% 
  summarise_all(funs(mean(.)))

select

























