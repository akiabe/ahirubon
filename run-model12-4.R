library(ggplot2)
library(rstan)
rstan_options(auto_write=TRUE)
options(mc.cores=parallel::detectCores())

d <- read.csv(file='data-ss1.txt')
head(d, n=3)

p <- ggplot(data=d, aes(x=X, y=Y))
p + geom_point() +
  geom_line() +
  labs(x='Time(Day)', y='Y')

T=nrow(d)
T_pred=3

data <- list(
  T=T,
  T_pred=T_pred,
  Y=d$Y
)

data

fit <- stan(
  file='model12-4.stan',
  data=data,
  seed=123
)

fit

ms <- rstan::extract(fit)
qua <- apply(ms$mu_all, 2, quantile, probs=c(0.025, 0.5, 0.975))
d_qua <- data.frame(x=1:(T+T_pred), t(qua))
colnames(d_qua) <- c('x', 'lwr', 'fit', 'upr')
head(d_qua, n=3)

p <- ggplot(data=d_qua, aes(x=x, y=fit))
p + geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=0.2, color=NA) +
  geom_line() +
  geom_point(data=d, aes(x=X, y=Y), alpha=0.5)

