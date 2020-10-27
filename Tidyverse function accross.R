library(tidyverse)

mpg %>% 
  group_by(class) %>% 
  summarise(across(cty, .fns = mean), .groups = 'drop')

mpg %>% 
  group_by(class) %>% 
  summarise(
    across(hwy, .fns = mean), .groups = 'drop')
  

mpg %>% 
  group_by(class) %>% 
  summarise(
    across(c(cty,hwy), .fns = list(mean = mean, std = sd)),
    .groups = 'drop'
  )

mpg %>% 
  group_by(class) %>% 
  summarise(
    across(
      c(cty,hwy),
      .fns = list(mean = mean, stdev = sd),
      .names = '{.fn} {.col} Consumption'
    ),
    .groups = 'drop'
  )

mpg_statistic <- mpg %>% 
  group_by(class) %>% 
  summarise(
    across(
      c(cty,hwy),
      .fns = list(mean = mean, stdev = sd, median = median),
      .names = '{.fn} {.col} Consumption'
    ),
    .groups = 'drop'
  ) %>% 
  rename_with(.fn = str_to_upper)
view(mpg_statistic)

mpg.stat.range <- mpg %>% 
  group_by(class) %>% 
  summarise(
    across(
      c(cty,hwy),
      .fns = list(
        'mean' = ~ mean(.x),
        'range_lo' = ~ (mean(.x) -2*sd(.x)),
        'range_hi' = ~(mean(.x) + 2*sd(.x))
      ),
      .names = '{.fn} {.col} Consumption'
    ),
    .groups = 'drop'
  ) %>% 
  rename_with(.fn = str_to_upper)
mpg.stat.range





























