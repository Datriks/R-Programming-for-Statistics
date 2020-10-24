library(tidyverse)

rm(list = ls())
employ <- as_tibble(read.csv("employee_data.csv",skip = 23, stringsAsFactors = FALSE))

employ

employ$gender = as.factor(employ$gender)

trans.employ <- employ %>% 
  mutate(
    gender = as.factor(gender),
    title = as.factor(title)
  )

trans.employ

# Check for missing value -------------------------------------------------

any(is.na(trans.employ))

trans.employ %>% 
  select(everything()) %>% 
  summarise_all(funs(sum(is.na(.))))

trans.employ %>% 
  select(
    ends_with("name"),gender,everything()
  ) %>% 
  filter(
    salary > 70000
  ) %>% 
  arrange(
    gender,last_name
  )

big.salary <- trans.employ %>% 
  filter(
    salary > 70000
  ) %>% 
  select(
    emp_no,title,salary
  )

big.salary %>% 
  group_by(
    title
  ) %>% 
  summarise(
    count = n(),
    avg.salary = mean(salary)
  ) %>% 
  arrange(
    desc(count)
  )

salary.month <- employ %>% 
  group_by(title,gender) %>% 
  summarise(
    avg.salary = mean(salary)
  ) %>% 
  mutate(
    monthly = avg.salary/12
  ) %>% 
  arrange(
    gender,desc(monthly)
  )
salary.month

























































