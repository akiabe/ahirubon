library(tidyverse)
library(rstan)
rstan_options(auto_write=TRUE)
options(mc.cores=parallel::detectCores())

d <- read.csv(file='data-aircon.txt')
head(d, n=3)

p <- ggplot(data=d, aes(x=X, y=Y))
p <- p + geom_point() +
  geom_smooth(method='loess')
p

N_new <- 60
X_new <- seq(from=3, to=32, length=N_new)
data <- list(N=nrow(d), X=d$X, Y=d$Y, N_new=N_new, X_new=X_new)
fit <- stan(file='model7-3.stan', data=data, seed=1234)

fit

ms <- rstan::extract(fit)
d_qua <- t(apply(ms$Y_new, 2, quantile, probs=c(0.025,0.5,0.975)))
colnames(d_qua) <- c('lwr', 'fit', 'upr')
d_qua <- data.frame(X_new, d_qua)
head(d_qua, n=3)

p <- ggplot(data=d_qua, aes(x=X_new, y=fit))
p <- p + geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=0.2)
p
