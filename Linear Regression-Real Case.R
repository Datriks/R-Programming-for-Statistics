install.packages("UsingR")

library(tidyverse)
library(UsingR)
library(stats)
library(pastecs)

rm(list = ls())
height <- father.son

describe(height)

head(father.son)

ggplot()+
  geom_point(data = height,aes(fheight,sheight),color = '#890000')+
  theme_light()

x <- height$fheight
y <- height$sheight
group <- 1:1078
dat <- data.frame(x,y,group)
head(dat)

x <- height$fheight
y <- x+3
means <- data.frame(x,y,group)
head(means)

ggplot()+
  geom_point(data=dat,aes(x,y),color='#002949')+
  geom_point(data=means,aes(x,y),color='#890000')+
  geom_line(data=line,aes(x,y))
  

#draw a line
x <- c(58,75)
y <- c(x+3)
line <- data.frame(x,y)

# use rbing to merge the 2 data frames dat, means in order to 
# construct the residuals (lines from our points to the mean).
d <- rbind(dat,means)
head(d)

ggplot()+
  geom_point(data=dat,aes(x,y),color='#002949')+
  geom_point(data=means,aes(x,y),color='#890000')+
  geom_line(data=line,aes(x,y))+
  geom_line(data=d,aes(x,y,group = group))

# Sum of Square Residuals -------------------------------------------------
head(means$y)
head(dat$y)
head(means$y - dat$y)

head(means$y - dat$y)^2
(means$y - dat$y)^2

sum((means$y - dat$y)^2)

# The Least Square Line-best fitting line -final solution--------------------------------

liniar <- lm(y ~ x, dat)

slope <- 0.5141
intercept <- 33.8866

y <- intercept + slope*x
x <- c(58,75)
y

fitting.line <- data.frame(x,y)
fitting.line

group <- 1:length(dat$x)
group # equal with the number of observations
x <- height$fheight
y <- y <- intercept + slope*x
means <- data.frame(x,y,group)
head(means)

d <- rbind(dat,means)
head(d)

ggplot()+
  geom_point(data=dat,aes(x,y),color='#002949')+
  geom_point(data=means,aes(x,y),color='#890000',size=2,shape =21)+
  geom_line(data=fitting.line,aes(x,y),color = 'red')+
  geom_line(data=d,aes(x,y,group = group),color = '#002949')+
  theme_light()

# Prediction --------------------------------------------------------------

ggplot()+
  geom_line(data = fitting.line,aes(x,y))

lm(y ~ x,dat)
y <- 33.8866 + 0.5141*x
x <- 70
y










































































































