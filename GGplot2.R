rm(list = ls())
library(tidyverse)
library(ggthemes)

# Grammar of R ------------------------------------------------------------

hdi <- read_csv("hdi-cpi.csv")

glimpse(hdi)

is_tibble(hdi)

sc <- hdi %>% 
  ggplot(aes(CPI.2015,HDI.2015))

sc+geom_point(aes(color = Region))+
  facet_grid(Region~.)+
  stat_smooth()+
  coord_cartesian(xlim = c(0.75,1))+
  theme_minimal()
  
install.packages("ggthemes")


sc+geom_point(aes(color = Region))+
  stat_smooth()+
  theme_minimal()

# variables and Variable Types --------------------------------------------
#types of variables: NOIR
#nominal,ordinal,interval,percentage


# Histograms --------------------------------------------------------------

df <- read_csv("titanic.csv")          

glimpse(df)

df$Survived <- as.factor(df$Survived)
df$Pclass <- as.factor(df$Pclass)
df$Sex <- as.factor(df$Sex)
df$Embarked <- as.factor(df$Embarked)

glimpse(df)

#bob %>% modify_if(is.factor, as.character) -> bob

df <- df %>% 
  mutate(
    Survived = as.factor(Survived),
    Pclass = as.factor(Pclass),
    Sex = as.factor(Sex),
    Embarked = as.factor((Embarked))
  )
glimpse(df)

hist <- df %>% 
  ggplot(aes(Age))

hist + geom_histogram(binwidth = 5,na.rm = T,color='white',fill = '#002949',alpha = 0.8)+
  ggtitle("Age Distribution on the Titanic")+
  labs(y = 'Number of Passangers', x ='Age')+
  theme_minimal()

emp <- read_csv("employee-data.csv",skip = 23)
glimpse(emp)

big.salary <- emp %>% 
  filter(
    salary > 45000
  )

big.salary

big.salary %>% 
  ggplot(aes(salary))+
  geom_histogram(binwidth = 5000,color = 'white',fill = "#890000", alpha = 0.7)+
  ggtitle("Salary Distribution in the Employee Data")+
  labs(x = 'Salary', y = 'Number of Employees')+
  theme_solarized(light = F, base_size = 15, base_family = "serif")

# Building a bar chart ----------------------------------------------------

view(df %>% 
  select(Survived))

bar <- df %>% 
  ggplot(aes(Sex, fill = Survived))

bar+geom_bar()+
  labs(y = 'Passenger Count', 
       x = 'Gender',
       title = 'Survival rate of the Titanic')+
  theme_light()
  
  

bar+geom_bar()+
  labs(y = 'Passenger Count', 
       x = 'Gender',
       title = 'Survival rate of the Titanic')+
  theme_light()+
  facet_wrap(~Pclass)

bar+geom_bar()+
  labs(y = 'Passenger Count', 
       x = 'Gender',
       title = 'Survival rate of the Titanic')+
  theme_light()+
  facet_wrap(Sex ~ Pclass)

hist <- df %>% 
  ggplot(aes(Age, fill = Survived))

hist+geom_histogram(binwidth = 5, color = 'white')+
  labs(
    title = 'Age distribution of the Titanic',
    x = 'Age',
    y = 'Number of Passengers'
  )+
  theme_light()

hist+geom_histogram(binwidth = 5, color = 'white')+
  labs(
    title = 'Age distribution of the Titanic',
    x = 'Age',
    y = 'Number of Passengers'
  )+
  theme_light()+
  facet_wrap(~Pclass)

# Practice Building a bar chart -------------------------------------------

nr.employ <- read_csv("employee-data.csv",skip = 23)

glimpse(nr.employ)

employ1 <- nr.employ %>% 
  mutate(
    gender = as.factor(gender),
    title = as.factor(title)
  )
employ1

bar.employ <-employ1 %>%
  ggplot(aes(title,fill = gender))

bar.employ+
  geom_bar()+
  labs(
    x = "Job Title",
    y = "Number of Employees",
    title = "Distribution of Jobs by Gender"
  )+
  theme_solarized(light = T)+
  scale_fill_manual(values = c("#890000", "#002949"))

# "magenta", "darkorange", "midnightblue",
# "springgreen4", "brown1", "gold"


bar.employ2 <- nr.employ %>% 
  ggplot(aes(gender, fill = title))

bar.employ2+geom_bar()+
  scale_fill_manual(values = c("magenta", "darkorange", "midnightblue",
                               "springgreen4", "brown1", "gold"))+
  labs(
    title = "JobPositions by Gender"
  )+
  theme_fivethirtyeight()

# Building a box with wiskers Graph ---------------------------------------
titanic

titan1 <- titanic %>% 
  mutate(
    Survived = as.factor(Survived),
    Pclass = as.factor(Pclass),
    Sex = as.factor(Sex),
    Embarked = as.factor((Embarked))
  )

glimpse(titan1)

 mybox<- titan1 %>% 
  ggplot(aes(Survived,Age))

mybox+geom_boxplot(outlier.color = 'red',outlier.size = 2.5)+
  labs(title = 'Survival Rate on the Titanic Based on Age')+
  theme_solarized()+

# interquartile are 1.5 IQR can be modifie with coef = 

mybox+geom_boxplot(outlier.color = 'red',outlier.size = 2.5)+
  geom_jitter(width = 0.2,aes(color = Sex))+
  labs(title = 'Survival Rate on the Titanic Based on Age')+
  theme_solarized()

mybox+geom_boxplot(outlier.color = 'red',outlier.size = 2.5)+
  geom_jitter(width = 0.2,aes(color = Sex))+
  labs(title = 'Survival Rate on the Titanic Based on Age')+
  coord_flip()+
  theme_solarized()

# Exercise Building a boxplt ----------------------------------------------

glimpse(employ1)

sal <- employ1 %>% 
  ggplot(aes(title,salary))

sal+geom_boxplot(outlier.color = 'red',outlier.size = 2)+
  geom_jitter(aes(color = gender))+
  labs(
    title = 'Salary Distribution',
    subtitle = 'based on postion and gender',
    x = 'Job Position',
    y = 'Salary'
  )+
  theme_solarized()+
  theme(legend.position = 'top',axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_color_brewer(palette = 'Set1')

# Building a Scatter Plot -------------------------------------------------
glimpse(hdi)

ch.anal <- hdi %>% 
  ggplot(aes(CPI.2015,HDI.2015))

ch.anal + 
  geom_point(aes(color = Region))+
  labs(
    x = 'Corruption Perceprtion Index',
    y = 'Human Development Index',
    title = 'Corruption and Human Development'
  )+
  theme_light()+
  theme()
 #------------------------------------------------------- 
ch.anal + 
  geom_point(aes(color = Region),
             shape = 21,
             fill = 'white',
             size = 3 ,
             stroke = 1.5)+
  labs(
    x = 'Corruption Perceprtion Index',
    y = 'Human Development Index',
    title = 'Corruption and Human Development'
  )+
  theme_light()+
  stat_smooth(se = F)

ch.anal + 
  geom_point(aes(color = Region),
             shape = 21,
             fill = 'white',
             size = 3 ,
             stroke = 1.5)+
  labs(
    x = 'Corruption Perceprtion Index',
    y = 'Human Development Index',
    title = 'Corruption and Human Development'
  )+
  theme_light()+
  stat_density2d()

























































