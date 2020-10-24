library(tidyverse)

x <- c(0,10)
y <- 3*x+1
y

df <- data.frame(x,y)

df %>% 
  ggplot(aes(x,y))+
  geom_line(
    size = 1.3,
    color = '#890000'
  )+
  scale_x_continuous(limits = c(0,13),breaks = seq(0,13,1))+
  scale_y_continuous(limits = c(0,35),breaks = seq(0,35,5))+
  theme_light()
#----------------------------------------------------------------------

x1 <- c(0,10)
y1 <- -5*x-7
y1

df1 <- data.frame(x1,y1)
df1

df1 %>% 
  ggplot(aes(x1,y1))+
  geom_line(
    size = 1.3,
    color = '#002949'
  )+
  scale_x_continuous(limits = c(0,11), breaks = seq(0,11,1))+
  scale_y_continuous(limits = c(-60,0), breaks = seq(0,-60,-5))+
  theme_light()
#-----------------------------------------------------------------------


























































































