library(tidyverse)
library(rstan)

d <- read.csv(file="data-rental.txt")
head(d)

# 説明変数と応答変数を可視化
ggplot(data=d, aes(x=Area, y=Y)) +
  geom_point() +
  labs(xlab="Area", ylab="Y")

colnames(d) <- c("Y", "X")
X_new <- seq(from=10, to=120, length=50)

data <- list(
  N=nrow(d), 
  Area=d$X, 
  Y=d$Y, 
  N_new=50, 
  Area_new=X_new)

fit <- stan(
  "log_linear_regression.stan", 
  data=data, 
  seed=1234)

fit

# 予測分布の可視化
ms <- rstan::extract(fit)
qua <- apply(ms$y_new, 2, quantile, 
             probs=c(0.1, 0.25, 0.50, 0.75, 0.9))
d_est <- data.frame(X=X_new, t(qua), check.names=FALSE)

ggplot() +  
  geom_ribbon(data=d_est, aes(x=X, ymin=`10%`, ymax=`90%`), alpha=1/6) +
  geom_ribbon(data=d_est, aes(x=X, ymin=`25%`, ymax=`75%`), alpha=2/6) +
  geom_line(data=d_est, aes(x=X, y=`50%`)) +
  geom_point(data=d, aes(x=X, y=Y), shape=1, size=2) +
  labs(x="Area", y="Y")

# 実測値と予測値の可視化
qua <- apply(ms$y_pred, 2, quantile, 
             probs=c(0.1, 0.25, 0.50, 0.75, 0.9))
d_est <- data.frame(X=d$Y, t(qua), check.names=FALSE)

ggplot(data=d_est, aes(x=X, y=`50%`)) +
  geom_pointrange(aes(ymin=`10%`, ymax=`90%`), fill="grey95", shape=21) +
  geom_abline(linetype='dashed') +
  labs(x="Observed", y="Predicted")

# 説明変数と応答変数の対数を取った場合
ggplot(data=d, aes(x=X, y=Y)) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10() +
  labs(xlab="X", ylab="Y")

data <- list(
  N=nrow(d), 
  Area=log10(d$X), 
  Y=log10(d$Y), 
  N_new=50, 
  Area_new=log10(X_new))


fit <- stan(
  "log_linear_regression.stan", 
  data=data, 
  seed=1234)

fit

# 予測分布の可視化
ms <- rstan::extract(fit)
qua <- apply(10^ms$y_new, 2, quantile, 
             probs=c(0.1, 0.25, 0.50, 0.75, 0.9))
d_est <- data.frame(X=X_new, t(qua), check.names=FALSE)

ggplot() +  
  geom_ribbon(data=d_est, aes(x=X, ymin=`10%`, ymax=`90%`), alpha=1/6) +
  geom_ribbon(data=d_est, aes(x=X, ymin=`25%`, ymax=`75%`), alpha=2/6) +
  geom_line(data=d_est, aes(x=X, y=`50%`), size=1) +
  geom_point(data=d, aes(x=X, y=Y), shape=1, size=2) +
  labs(x="Area", y="Y")

# 実測値と予測値の可視化
qua <- apply(10^ms$y_pred, 2, quantile, 
             probs=c(0.1, 0.25, 0.50, 0.75, 0.9))
d_est <- data.frame(X=d$Y, t(qua), check.names=FALSE)

ggplot(data=d_est, aes(x=X, y=`50%`)) +
  geom_pointrange(aes(ymin=`10%`, ymax=`90%`), fill="grey95", shape=21) +
  geom_abline(linetype='dashed') +
  labs(x="Observed", y="Predicted")
