library(tidyverse)
library(psych)
rm(list = ls())

ny <- c(1,2,3,3,5,6,7,8,9,66)
la <- c(1,2,3,5,5,6,7,8,9,10)

pizza <- data.frame(ny,la)
pizza

mean(pizza$ny)
mean(pizza$la)

median(pizza$ny)
median(pizza$la)

# own build function to find the mode:

x <- table(pizza$ny)
x

names(x)[which(x==max(x))]

y <- table(pizza$la)
y

names(y)[which(y==max(y))]

summary(pizza)

# Skewness ----------------------------------------------------------------

col.1 <- c(1,1,1,1,2,2,2,2,2,2,3,3,3,3,4,4,5,5,7)
col.2 <- c(1,1,2,2,3,3,3,4,4,4,4,4,5,5,5,6,6,7,7)
col.3 <- c(1,2,3,3,4,4,4,5,5,5,5,6,6,6,6,6,6,7,7)

df <- data.frame(col.1,col.2,col.3)
df

pl.1 <- df %>% 
  ggplot(aes(col.1))+
  geom_histogram(binwidth = 1,color = "white",fill = 'red4')+
  theme_light()+labs(title = 'Positive Skew')
pl.1

pl.2 <- df %>% 
  ggplot(aes(col.2))+
  geom_histogram(binwidth = 1,color = "white",fill = 'red4')+
  theme_light()+labs(title = 'Zero Skew')
pl.2

pl.3 <- df %>% 
  ggplot(aes(col.3))+
  geom_histogram(binwidth = 1,color = "white",fill = 'red4')+
  geom_vline(color = 'blue',xintercept = mean(col.1), linetype = 'dashed')+
  geom_vline(color = 'yellow',xintercept = median(col.1), linetype = 'dashed')+
  theme_light()+labs(title = 'Negative Skew')
pl.3

#-------------------------------------------------------------------------
set.seed(1234)
df1 <- data.frame(
  sex = factor(rep(c('M','F'),each = 200)),
  weight = round(c(rnorm(200, mean = 65, sd = 5),rnorm(200, mean = 55, sd = 5)))
)

df1
#----------------------------------------------------------------------------

summary(df)

median(col.1)
median(col.2)
median(col.3)

set.seed(1234)

df2 <- data.frame(
  gender = factor(rep(c('M','F'),each = 200)),
  weight = round(c(rnorm(200,mean = 65,sd=5),rnorm(200,mean=55,sd=5))),
  height = round(c(rnorm(200,mean = 180,sd=5),rnorm(200,mean = 170,sd=5)))
)
df2

# Exercise Skewness -------------------------------------------------------

skew1 <- read_csv("skew_1.csv")
skew2 <- read_csv("skew_2.csv")

skew1 <- skew1 %>% 
  mutate(
    Dataset1 = as.integer(`Dataset 1`),
    #`Dataset 1` = NULL
  )

glimpse(skew1)
view(skew1)
glimpse(skew2)

sk1 <- skew1 %>% 
  ggplot(aes(x = `Dataset 1`))

sk1+geom_histogram(
  binwidth = 100,
  color = 'darkslategray',
  fill = '#890000',
  alpha = 0.7
)+
  theme_light()+
  labs(title = "Skewness of Dataset 1")
#------------------------------------------------------------------------------
sk2 <- skew2 %>% 
  mutate(
    Dataset2 = as.integer(`Dataset 2`),
    `Dataset 2` = NULL
  )

glimpse(skew2)

sk2 <- skew2 %>% 
  ggplot(aes(`Dataset 2`))

sk2+geom_histogram(
  binwidth = 100,
  color = 'darkslategray',
  fill = '#002949',
  alpha = 0.7
)

install.packages("psych")
library(psych)

describe(skew1)
describe(skew2)

# Measures of Variability --------------------------------------------------
#Variance; standard deviation; (relative) coefficient of variation-univariate measures

ny1 <- c(1,2,3,3,5,6,7,8,9,11)
la1 <- c(1,2,3,4,5,6,7,8,9,10)

pizza <- data.frame(ny1,la1)
pizza$ny.mxn <- c(18.81,37.62,56.43,56.43,94.05,
                  112.86,131.67,150.48,169.29,206.91)
pizza

lapply(pizza, mean)
sapply(pizza,mean)

sapply(pizza,var)
sapply(pizza,sd)
coef.var <- sapply(pizza,sd)/sapply(pizza,mean)
coef.var

# Covariance and Correlation multivariate----------------------------------------------

homes <- read_csv("landdata-states.csv")
attributes(homes)
str(homes)

homes

home.set <- homes %>% 
  subset(Date == 2001.25) 
  

home.set %>% ggplot(aes(log(Land.Value),Structure.Cost))+
  geom_point(color='#890000')+
  theme_light()+
  labs(
    x = 'Land Value (transformed)',
    y = 'Structure Cost (USD)',
    title = 'Relationship between Land and structure'
  )
#the variables are correlated
#have a positive covariance

cor(homes$Structure.Cost,homes$Land.Value)
cor.test(homes$Structure.Cost,homes$Land.Value)

view(homes)

# Practice Exercises-Statistics -------------------------------------------

prod <- read_csv("practical_product.csv")
cust <- read_csv("practical_customer.csv")

glimpse(prod)
glimpse(cust)

prod %>% 
  ggplot(aes(Price))+
  geom_histogram(binwidth = 100000,color = 'darkslategray',fill = "#890000")+
  geom_vline(xintercept = mean(prod$Price), color = 'blue', linetype = 'dashed')+
  theme_light()+
  scale_x_continuous(labels = scales::comma)

prod %>% 
  ggplot(aes(Price, `Area (ft.)`))+
  geom_point(color = 'darkblue')+
  theme_light()+
  stat_smooth(color = 'darkred', method = 'lm')

prod %>% 
  ggplot(aes(`Area (ft.)`,Price))+
  geom_point(color = 'darkblue')+
  theme_light()+
  stat_smooth(color = 'darkred', method = 'lm')+
  scale_y_continuous(labels = scales::comma)
  
describe(prod$Price)
summary(prod$Price)

# create a function that returns the mode

mode <- function(x){
  ta <- table(x)
  tam <- max(ta)
  if(all(ta==tam))
    mod <- NA
  else
    if(is.numeric(x))
      mod <- as.numeric(names(ta)[ta == tam])
  else
    mod <- names(ta)[ta == tam]
  return(mod)
}

mode(prod$Price)

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

getmode(prod$Price)

mode1 <- function(x){
  uniqx <- unique(x)
  uniqx[which.max(tabulate(match(x, uniqx)))]
}

mode1(prod$Price)

describe(prod$Price)
corr.test(prod$`Area (ft.)`,prod$Price)






























