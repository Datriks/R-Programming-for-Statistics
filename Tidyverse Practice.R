library(tidyverse)
getwd()


my_pok <- read.table(
  "pokRdex-comma.csv",
  sep = ",",
  header = TRUE,
  stringsAsFactors = FALSE
)

my_pok

mypok <- read.csv("pokRdex_tab.csv")
mypok

mypok <- as_tibble(read_csv("pokRdex_tab.csv"))
problems(mypok)

glimpse(mypok)

mypok2 <- as_tibble(read_csv("pokRdex-comma.csv"))
glimpse(mypok2)

mypok3 <- as_tibble(read_csv("pokRdex-comma.csv", n_max = 100))
mypok3

# Data Export in R --------------------------------------------------------

write_csv(mypok3,"mypok100.csv")

title <- c("Star Wars","The Empire Strike Back","Return of the Jedi",
           "The Phantom Menace","Attack of The Clones","Revenge of The Sith",
           "The Force Awakens")

year <- c(1977,1980,1983,1999,2002,2005,2015)
length.min <- c(121,124,133,133,142,140,135)
box.office.mil <- c(787,534,572,1027,657,849,2059)

star_wars <- tibble(
  Title = title,Year = year, Length = length.min,BoxOffice = box.office.mil
)
star_wars

write_csv(star_wars,"star_wars.csv")

nrow(mypok3)
ncol(mypok3)
colnames(mypok3)
names(mypok3)
str(mypok3)
summary(mypok3)
glimpse(mypok3)
attr(mypok3,"spec")
attributes(mypok2)

df2 <- datasets::attenu

summary(df2)
str(df2)
glimpse(df2)

# Indexing and Slicing a data frame ---------------------------------------
starwars

summary(starwars)
names(starwars)
str(starwars)
print(starwars)
view(starwars)
glimpse(starwars)

my.wars <- as_tibble(starwars)

my.wars <- my.wars[,-(11:13)]

my.wars1 <- my.wars %>% 
  select(-(11:13))

glimpse(my.wars1)
head(my.wars1)
tail(my.wars1)

head(my.wars1[['name']])

my.wars1 %>% 
  distinct(name)

my.wars1 %>% 
  select(1:5)

my.wars1 %>% 
  filter(!is.na(hair_color)) %>% 
  distinct(hair_color)

my.wars1[c(1:25),c(1:6)]

# Extending a data frame; Create columns ----------------------------------

star_wars

mark <- c(37.5,34.75,34.25,0,0,0.75,0)
carrie <- c(13.5,22.75,21.25,0,0,0.5,5.75)


star_wars$Mark <- mark
star_wars$Carrie <- carrie
star_wars

star_wars$Mark <- NULL
#star_wars
star_wars$Carrie <- NULL

star_wars <- cbind(star_wars,MarkScreenTime = mark, CarrieScreenTime = carrie)
star_wars

star_wars$MarkScreenTime <- NULL
star_wars$CarrieScreenTime <- NULL


star_wars <- star_wars %>% 
  mutate(
    MarkScrTime = mark,
    CarrieScrTime = carrie
  )

star_wars

star_wars <- star_wars %>% 
  select(
    1:4
  )
star_wars

sw <- star_wars %>% 
  add_column(mark,carrie)

starWars <- sw %>% 
  add_row(
    Title = 'Leustean',
    Year = 1977,
    Length = 500,
    BoxOffice = 2000,
    mark = 100,
    carrie = 150
  )
starWars

data()
UKgas
summary(UKgas)

# Dealing with missing Data -----------------------------------------------

data()
storms


eg.na <- c(NA, 1:10)
eg.na

mean(eg.na)
mean(eg.na, na.rm = TRUE)

which(is.na(eg.na))

head(starwars,5)

star <- starwars

is.na(star)
any(is.na(star))

any(is.na(star[,c('name','homeworld','species')]))

which(is.na(star))

star %>% 
  drop_na()

star %>% 
  mutate(
    hair_color = replace_na(hair_color,0)
  )

star %>% 
  mutate(
    hair_color = replace_na(hair_color,0),
    birth_year = replace_na(birth_year,0)
  )
star

star1 <- star %>% 
  mutate(
    hair_color = replace_na(hair_color,'Unknown'),
    birth_year = replace_na(birth_year,'Unknown')
  )

star1 %>% 
  filter(
    hair_color == 'Unknown' | birth_year == 'Unknown'
  )


star1 %>% 
  filter(
    birth_year == 'Unknown'
  )

mass.median <- median(star$mass, na.rm = TRUE)
mass.median

star2 <-  star %>% 
  mutate(
    mass = replace_na(mass,mass.median)
  )

star2

star3 <- star %>% 
  mutate(
    height = replace_na(height,median(height, na.rm = TRUE)),
    birth_year = replace_na(birth_year,median(birth_year, na.rm = TRUE))
  )

star %>% 
  view()

view(star3)

star3 %>% 
  filter(
    name == c('R2-D2','R5-D4')
  )


# Exercises- Statistics in R -2020 ----------------------------------------

employee <- read.csv("employee_data.csv", nrows = 200,skip = 23)
view(employee)

names(employee)

new_employ <- employee %>% 
  rename(
    EmployeeNumber = emp_no,
    FirstName = first_name,
    LastName = last_name,
    BirtDate = birth_date,
    Gender = gender,
    JobTitle = title,
    Salary = salary,
    FromDate = latest_start_date,
    ToDate = end_of_contract_date
  )

names(new_employ)

write_csv(new_employ,"newEmployee.csv")

read.table("newEmployee.csv",sep = ",", header = TRUE)

mtcars

mtcars["mpg"]
mtcars$mpg
mtcars[,1]

class(mtcars["mpg"])
class(mtcars[,1])
class(mtcars$mpg)

star %>% 
  slice(1:5)
























