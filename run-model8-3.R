library(tidyverse)
library(rstan)
rstan_options(auto_write=TRUE)
options(mc.cores=parallel::detectCores())

d <- read.csv(file='data-salary-2.txt')
head(d, n=3)

data <- list(N=nrow(d), K=4, X=d$X, Y=d$Y, KID=d$KID)
fit <- stan(file='model8-3.stan', data=data, seed=1234)

fit

ms <- rstan::extract(fit)
d_qua <- t(apply(ms$mu, 2, quantile, probs=c(0.025,0.5,0.975)))
colnames(d_qua) <- c('lwr', 'fit', 'upr')
d_qua <- data.frame(d, d_qua)
head(d_qua, n=3)

p <- ggplot(data=d_qua, aes(x=X, y=fit))
p <- p + facet_wrap(~KID) +
  geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=0.2) +
  geom_line() +
  geom_point(data=d_qua, aes(x=X, y=Y), alpha=0.2) +
  labs(x='X', y='Y', title='Group by KID')

p
