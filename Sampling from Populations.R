library(tidyverse)

sample(1:10,100,replace = T)
sample(1:10,100,replace = T)

z <- as.integer(rnorm(100,50,10))
z
class(z)
z <- data.frame(z)
z
rnorm(100,50,30)

# Plotting a vertical sample ----------------------------------------------

x <- rep(1,100)
y <- rnorm(100,50,10)

dat <- data.frame(x,y)
dat

dat %>% 
  ggplot(aes(x,y))+
  geom_point()

x <- 1
y <- 50
mean <- data.frame(x,y)
mean

ggplot()+
  geom_point(
    data = dat,
    aes(x,y),
    shape = 21,
    color = '#890000')+
  geom_point(
    data = mean,
    aes(x,y),
    size = 7,
    color = '#002949'
  )+
  theme_light()+
  coord_flip()
# Plotting several vertical samples ----------------------------------------
x <- 1
y <- 50
mean1 <- data.frame(x,y)
x1 <- rep(1,100)
y1 <- rnorm(100,50,10)
dat1 <- data.frame(x1,y1)
dat1

ggplot()+
  geom_point(data = dat1,
             aes(x1,y1),shape =21,size=1.3,color = '#890000')+
  geom_point(data=mean1,
             aes(x,y),color='red',size=7)
  
x2 <- 9
y2 <- 30
mean2 <- data.frame(x2,y2)
#------------------------------------------------------------------------
x3 <- c(x1, rep(9,100), rep(15,100))
x3
y3 <- c(y1, rnorm(100,30,10), rnorm(100,78,10))
y3

df <- data.frame(x3,y3)
df

x <- c(1,9,15)
y <- c(50,30,78)

mean <- data.frame(x,y)
mean

ggplot()+
  geom_point(data = df,aes(x3,y3), shape=21, color = '#890000')+
  geom_point(data=mean,aes(x,y),size=5,color='red')+
  theme_light()
  #+coord_flip()

# Samples along a line ----------------------------------------------------
# Create 4 vertical samples of 100m points each. Means must lie on the line 
# y = 3*x+1 in x-locations:1,9,15,22

x <- c(0,25)
y <- 3*x+1
y

line <- data.frame(x,y)

ggplot()+
  geom_line(data=line,aes(x,y))

# mean points
x <- c(1,9,15,22)
y <- 3*x+1
mean1 <- data.frame(x,y)
mean

ggplot()+
  geom_line(data=line,aes(x,y))+
  geom_point(data=mean1,aes(x,y),color='red',size=7)

x1 <- c(rep(1,100),rep(9,100),rep(15,100),rep(22,100))
x1
?rep

x1 <- rep(x,each = 100)
x1

y1 <- c(rnorm(100,4,10),rnorm(100,28,10),rnorm(100,46,10),rnorm(100,67,10))
y1

y2 <- sapply(x,function(x) rnorm(100,x,10))
y2

dat <- data.frame(x1,y1)
dat

ggplot()+
  geom_line(data=line,aes(x,y))+
  geom_point(data=mean1,aes(x,y),color='red',size=7)+
  geom_point(data = dat,aes(x1,y1),shape=21,color='#890000')



# Using sapply -------------------------------------------------------------

x <- c(2,4,9,15)
sqrt(x)
x^2
sapply(x, function(x) sqrt(x))
sapply(x, function(x) x^2)

sapply(x,function(x) rnorm(1,x,10))

# Cloud of Points ---------------------------------------------------------
#generate a 100 points in the following way:
# the x coordinates are drawn from a normal population of mean = 10 and sd = 5.
#for each x value, one y value is drawn from a normal population with mean
# 3*x+1


x <- rnorm(100,10,5)
x
y <- sapply(x,function(x) 3*x+1)
y
df3 <- data.frame(x,y)
df3

x <- c(-5,25)
y <- 3*x+1
line <- data.frame(x,y)

ggplot()+
  geom_point(data=df3,aes(x,y),shape=21,size=2,color='#890000')+
  geom_line(data=line,aes(x,y),color = '#002949',size=1)+
  scale_x_continuous(limits = c(-10,30))+
  scale_y_continuous(limits = c(-20,80))+
  theme_light()

x <- df3$x
x
y <- sapply(x,function(x) rnorm(1, 3*x+1, 10))
y

vertical <- data.frame(x,y)
vertical

ggplot()+
  geom_point(data=df3,aes(x,y),shape=21,size=2,color='#890000')+
  geom_point(data=vertical,aes(x,y),color='#002949')+
  geom_line(data=line,aes(x,y),color = '#002949',size=1)+
  scale_x_continuous(limits = c(-10,30))+
  scale_y_continuous(limits = c(-20,80))+
  theme_light()


















































