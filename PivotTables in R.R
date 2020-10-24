library(tidyverse)
library(tidyquant)
library(knitr)

FANG

shares <- tq_get(c("FB", "AMZN", "NFLX", "GOOG"))
shares
tail(shares)




































