library(ggplot2)
library(rstan)
rstan_options(auto_write=TRUE)
options(mc.cores=parallel::detectCores())

d <- read.csv(file='data-kubo11a.txt')
I <- nrow(d)
d <- data.frame(X=1:I, Y=d$Y)
head(d, n=3)

p <- ggplot(data=d, aes(x=X, y=Y))
p + geom_point() +
  geom_line()

data <- list(
  I=I,
  Y=d$Y
)

data

fit <- stan(
  file='model12-12.stan',
  data=data,
  seed=123
)

fit

ms <- rstan::extract(fit)
qua <- apply(ms$Y_mean, 2, quantile, probs=c(0.025, 0.5, 0.975))
d_qua <- data.frame(x=1:I, t(qua))
colnames(d_qua) <- c('x', 'lwr', 'fit', 'upr')
head(d_qua, n=3)

p <- ggplot(data=d_qua, aes(x=x, y=fit))
p + geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=0.2) +
  geom_line() +
  geom_point(data=d, aes(x=X, y=Y), alpha=0.5)
