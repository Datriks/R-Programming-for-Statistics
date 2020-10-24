library(tidyverse)
# Vectors and Operations with vectors -------------------------------------

weight <- c(71,67,83,67)
height <- c(1.75,1.81,1.78,1.82,1.97,2.12,2.75)

bmi <- weight/height^2
bmi

correctweight <- c(71,67,83,67,71,67,83)
bmi1 <- correctweight/height^2
bmi1

atk <- c(3000,NA,NA,NA,NA,4000,3000,5000,1000,5000)

attributes(atk)
names(atk)

names(atk) <- c("Blue-Eyes White Dragon", "Exodius", "The Winged Dragon of Ra", "Raigeki", 
                "Slifer the Sky Dragon", "Obelisk the Tormentor", "Black Luster Soldier", 
                "5-Headed Dragon", "Exodia the Forbidden One", "Dragon Master Knight")
attributes(atk)

atk

?sample
# Bernoulli Trials

x <- 1:12
sample(x)

sample(c(0,1),100,replace = T)

x <- 1:10
sample(x[x>8])
sample(x[x>9])

# safer version would be:

resample <- function(x, ...) x[sample.int(length(x), ...)]
resample(x[x>9])
resample(x[x>10])

# resample <- function(x, ...) x[sample.int(length(x), ...)]

??scales::comma

# Slicing and indexing a Vector -------------------------------------------

n.deck <- c(6,7,8,9,10)
deck <- c('Duke','Assassin','Captain','Ambassador','Contesa')

deck[2]
deck[3]
deck[-4]
deck[c(1,5)]
deck[c(1,3,5)]
# Indexing by name

names(n.deck) <- deck
n.deck
n.deck[["Captain"]]
n.deck[["Contesa"]]
n.deck[5]
n.deck[c('Duke','Captain','Contesa')]

# Slicing

n.deck[3:5]
n.deck[-c(3:5)]

lv <- seq(10,200,by = 10)
lv

?slice

lv[-(12:15)]

atk
atk[6]
atk[-2]
atk[c(1,3,5,7,9)]
atk[-c(4,5,6)]
atk[atk > 2000]
lv[-c(12:15)]

# Changing the Dimensions of a vector -------------------------------------

a <- seq(10,120,10)
a

dim(a) <- c(4,3)
a

?dim
matrice <- matrix(a,4,3)
matrice

dim(a)
class(a)
typeof(a)

s <- seq(2,30,2)
s

dim(s)

dim(s) <- c(3,5)
s
class(s)
typeof(s)
dim(s)

dim(s) <- c(1,3,5)
s








































