library(tidyverse)
library(rstan)
rstan_options(auto_write=TRUE)
options(mc.cores=parallel::detectCores())

d <- read.csv(file='data-dice.txt')
K <- 6
data <- list(N=nrow(d), K=K, Y=d$Face)
fit <- stan(file='model9-4.stan', data=data, seed=1234)
fit

Y <- table(factor(d$Face, levels=1:K))
data <- list(K=K, Y=Y)
fit <- stan(file='model9-5.stan', data=data, seed=1234)
fit
