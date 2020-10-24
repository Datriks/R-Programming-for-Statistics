library(tidyverse)

lvl <- c(8,10,10,1,10,8,12,1,12)

sum(lvl)
mean(lvl)
median(lvl)
length(lvl)
sd(lvl)
round(sd(lvl))
print(round(sd(lvl)))

args(median)
args(round)

round(2.2345, digits = 2)
round(2.3456,2)

args(ggplot)

args(sample)
args(median)

median(lvl, na.rm = F)

atk <- c(3000,NA,NA,NA,NA,4000,3000,5000,1000,5000)
atk

median(atk)
median(atk,na.rm = T)

# Create a Function -------------------------------------------------------

deck <- c('Duke','Assassin','Captain','Ambassador','Contesa')
deck

sample(deck,3)
args(sample)

hand <- sample(deck,3,replace = T)
hand

draw <- function(){
  deck <- c('Duke','Assassin','Captain','Ambassador','Contesa')
  hand <- sample(deck,3,replace = T)
  print(hand)
}

draw()

flip <- function(){
  coin <- c("head","tail")
  throw <- sample(coin,100,replace = T)
  print(throw)
}

flip()

# make the coin unfair

flip_unfair <- function(){
  coin <- c('head','tail')
  throw <- sample(coin, 100, replace = T, prob = c(0.3,0.7))
  print(throw)
}

flip_unfair()







































































