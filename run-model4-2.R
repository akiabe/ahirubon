library(ggplot2)
library(rstan)
rstan_options(auto_write=TRUE)
options(mc.cores=parallel::detectCores())

d <- read.csv(file='data-salary.txt')
head(d, n=3)

p <- ggplot(data=d, aes(x=X, y=Y))
p + geom_point() +
    geom_smooth(method='lm')

data <- list(
  N=nrow(d),
  X=d$X,
  Y=d$Y,
  N_pred=nrow(d),
  X_pred=d$X
)

fit <- stan(
  file='model4-2.stan',
  data=data,
  seed=123
)

fit

ms <- rstan::extract(fit)
qua <- apply(ms$Y_pred, 2, quantile, probs=c(0.025, 0.5, 0.975))
d_qua <- data.frame(d, t(qua))
colnames(d_qua) <- c('X', 'Y', 'lwr', 'fit', 'upr')
head(d_qua, n=3)

p <- ggplot(data=d_qua, aes(x=X, y=fit))
p + geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=0.2) +
    geom_line() +
    geom_point(data=d, aes(x=X, y=Y))
