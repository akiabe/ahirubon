library(tidyverse)
library(rstan)

d <- read.csv(file="data3a.csv")
head(d)

d_conv <- data.frame(X=c(0, 1))
rownames(d_conv) <- c("C", "T")
data <- list(
  N=nrow(d), 
  Y=d$y, 
  X=d$x, 
  F=d_conv[d$f,])

# Stanでモデルの実装
fit <- stan(
  file="poi_regression_v2.stan",
  data=data,
  seed=123
  # seedが1234の場合、poisson_log_rngのパラメータが大きくなりエラーとなる
)

fit

# 実測値と予測値の可視化
ms <- rstan::extract(fit)
qua <- apply(ms$y_pred, 2, quantile, prob=c(0.1, 0.5, 0.9))
d_est <- data.frame(d, t(qua), check.names=FALSE)
d_est$f <- as.factor(d_est$f)

ggplot(d_est, aes(x=y, y=`50%`, ymin=`10%`, ymax=`90%`, shape=f, color=f)) +
  coord_fixed(xlim=c(0, 15), ylim=c(0, 15)) +
  geom_pointrange() +
  geom_abline(linetype="dashed") +
  labs(x="Observed", y="Predicted") 
