library(tidyverse)
library(rstan)
rstan_options(auto_write=TRUE)
options(mc.cores=parallel::detectCores())

d <- read.csv(file='data-conc.txt')
head(d, n=3)

p <- ggplot(data=d, aes(x=Time, y=Y))
p <- p + geom_point() +
  geom_line()
p

T_new <- 60
Time_new <- seq(from=0, to=24, length=T_new)
data <- list(T=nrow(d), Time=d$Time, Y=d$Y, T_new=T_new, Time_new=Time_new)
fit <- stan(file='model7-4.stan', data=data, seed=1234)

fit

ms <- rstan::extract(fit)
d_qua <- t(apply(ms$Y_new, 2, quantile, probs=c(0.025,0.5,0.975)))
colnames(d_qua) <- c('lwr', 'fit', 'upr')
d_qua <- data.frame(Time_new, d_qua)
head(d_qua, n=3)

p <- ggplot(data=d_qua, aes(x=Time_new, y=fit))
p <- p + geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=0.2)
p
