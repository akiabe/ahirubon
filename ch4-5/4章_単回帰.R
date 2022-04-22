library(tidyverse)

d <- read.csv("data-salary.txt")
head(d, n=5)

ggplot(d, aes(x=X, y=Y)) +
  geom_point()

## lm関数で推定した場合
res_lm <- lm(Y ~ X, data=d)
X_new <- data.frame(X=23:60)

# 信頼区間の計算
conf_95 <- predict(res_lm, X_new, interval="confidence", level=0.95)
conf_95 <- data.frame(X_new, conf_95)
conf_50 <- predict(res_lm, X_new, interval="confidence", level=0.50)
conf_50 <- data.frame(X_new, conf_50)

# 予測区間の計算
pred_95 <- predict(res_lm, X_new, interval="prediction", level=0.95)
pred_95 <- data.frame(X_new, pred_95)
pred_50 <- predict(res_lm, X_new, interval="prediction", level=0.50)
pred_50 <- data.frame(X_new, pred_50)

# Xの信頼区間を可視化
ggplot() +
  geom_ribbon(data=conf_95, aes(x=X, ymin=lwr, ymax=upr), alpha=1/6) +
  geom_ribbon(data=conf_50, aes(x=X, ymin=lwr, ymax=upr), alpha=2/6) +
  geom_line(data=conf_50, aes(x=X, y=fit), size=1) +
  geom_point(data=d, aes(x=X, y=Y), shape=1, size=3) +
  labs(x="X", y="Y")

# X_newに対する予測区間を可視化
ggplot() +
  geom_ribbon(data=pred_95, aes(x=X, ymin=lwr, ymax=upr), alpha=1/6) +
  geom_ribbon(data=pred_50, aes(x=X, ymin=lwr, ymax=upr), alpha=2/6) +
  geom_line(data=pred_50, aes(x=X, y=fit), size=1) +
  geom_point(data=d, aes(x=X, y=Y), shape=1, size=3) +
  labs(x="X", y="Y")

## Stanで実装した場合
library(rstan)
library(ggExtra)

d <- read.csv("data-salary.txt")
X_new <- 23:60
data <- list(N=nrow(d), X=d$X, Y=d$Y, N_new=length(X_new), X_new=X_new)
fit <- stan(file="linear_regression.stan", data=data, seed=1234)
ms <- rstan::extract(fit)

d_mcmc <- data.frame(a=ms$a, b=ms$b, sigma=ms$sigma)
head(d_mcmc)

# MCMCサンプルの散布図とそれぞれの周辺分布を可視化
p <- ggplot(d_mcmc, aes(x=a, y=b)) +
  geom_point(alpha=1/4, shape=1, size=2)
ggMarginal(p, type="histogram")

# Xの信頼区間を可視化
qua <- apply(ms$y_base_new, 2, quantile, probs=c(0.025, 0.25, 0.50, 0.75, 0.975))
d_est <- data.frame(X=X_new, t(qua), check.names=FALSE)

ggplot() +  
  theme_bw() +
  geom_ribbon(data=d_est, aes(x=X, ymin=`2.5%`, ymax=`97.5%`), fill="black", alpha=1/6) +
  geom_ribbon(data=d_est, aes(x=X, ymin=`25%`, ymax=`75%`), fill="black", alpha=2/6) +
  geom_line(data=d_est, aes(x=X, y=`50%`), size=1) +
  geom_point(data=d, aes(x=X, y=Y), shape=1, size=3) +
  labs(y="Y")

# X_newに対する予測区間を可視化
qua <- apply(ms$y_new, 2, quantile, probs=c(0.025, 0.25, 0.50, 0.75, 0.975))
d_est <- data.frame(X=X_new, t(qua), check.names = FALSE)

ggplot() +  
  theme_bw() +
  geom_ribbon(data=d_est, aes(x=X, ymin=`2.5%`, ymax=`97.5%`), fill="black", alpha=1/6) +
  geom_ribbon(data=d_est, aes(x=X, ymin=`25%`, ymax=`75%`), fill="black", alpha=2/6) +
  geom_line(data=d_est, aes(x=X, y=`50%`), size=1) +
  geom_point(data=d, aes(x=X, y=Y), shape=1, size=3) +
  labs(y="Y")

