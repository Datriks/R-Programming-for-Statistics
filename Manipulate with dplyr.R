# Manipulate data with dplyr ----------------------------------------------
# filter, mutate, summarize,group_by, select, arrange
library(tidyverse)

# Total number of NAs -----------------------------------------------------

rm(list = ls())

star <- starwars
star

star %>% 
  glimpse()

star %>% 
  select(everything()) %>% 
  summarise_all(funs(sum(is.na(.))))

# Filter ------------------------------------------------------------------

view(star)

star %>% 
  filter(
    species == "Droid", homeworld == "Tatooine"
  )

star %>% 
  filter(
    eye_color == "red" | eye_color == "yellow" | eye_color == "orange"
  )

star %>% 
  select(
    name,birth_year,homeworld,species,starships
  )

star %>% 
  select(
    name,homeworld:starships
  )

star %>% 
  select(
    ends_with("color")
  )

star %>% 
  select(
    name,vehicles,starships,everything()
  )

# Mutate ------------------------------------------------------------------

star %>% 
  mutate(
    bmi = mass / ((height /100)^2)
  ) %>% 
  select(
    name:bmi
  )

star.trans <- star %>% 
  transmute(
    bmi2 = mass / ((height/100)^2)
  )

star.trans

star %>% 
  arrange(
    mass
  )
star %>% 
  arrange(
    desc(mass)
  )

# Grouping and Summarizing ------------------------------------------------

star %>% 
  summarise(
    avg.height = mean(height, na.rm = TRUE)
  )

star
star %>% 
  group_by(
    species
  ) %>% 
  summarise(
    avg.height = mean(height, na.rm = T)
  )

star %>% 
  filter(
   !is.na(gender)
  ) %>% 
  group_by(
    gender
  ) %>% 
  summarise(
    avg.height = mean(height, na.rm = T)
  )

star %>% 
  filter(
    !is.na(hair_color)
  ) %>% 
  group_by(
    hair_color
  ) %>% 
  summarise(
    avg.height = mean(height, na.rm = T)
  )
  

# Sampling Data with dplyr ------------------------------------------------

# sample_n() - extracts a sample of a fixed number of rows
# sample_frac() - extracts a random sample of fixed fraction of rows

star %>% 
  sample_n(20)

star %>% 
  slice_sample(n = 10)

star %>% 
  sample_n(10)

# if we wants to sample 10% of my data we use:
star %>% 
  sample_frac(0.1)

star %>% 
  sample_frac(0.5)

# Bootstrapping -----------------------------------------------------------

star %>% 
  sample_n(10, replace = T)

star %>% 
  group_by(
    species
  ) %>% 
  summarise(
    avg.mass = mean(mass,na.rm = T)
  ) %>% 
  arrange(
    desc(avg.mass)
  )

star %>% 
  group_by(
    species
  ) %>% 
  summarise(
    count = n(),
    avg.mass = mean(mass, na.rm = T)
  ) %>% 
  filter(
    count > 1
  ) %>% 
  arrange(
    desc(count)
  )

# Sampling-Taking a sample ------------------------------------------------

result <- iris %>% 
  group_by(Species) %>%  
  sample_n(1000,replace = T)

view(result)

averages <- iris %>% 
  group_by(Species) %>% 
  summarise(
    S.Length = mean(Sepal.Length),
    S.Width = mean(Sepal.Width),
    count = n()
  )
averages

averages <- result %>% 
  group_by(Species) %>% 
  summarise(
    S.Length = mean(Sepal.Length),
    S.Width = mean(Sepal.Width),
    count = n()
  )






































































