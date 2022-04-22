library(tidyverse)
library(rstan)

d <- read.csv("data-attendance-2.txt")
head(d, n=5)

data <- list(
  N=nrow(d),
  A=d$A,
  Score=d$Score/200,
  M=d$M,
  Y=d$Y
)

# Stanでモデルの実装
fit <- stan(
  file="binomial_logregression.stan",
  data=data,
  seed=1234
)

fit

# 実測値と予測値の可視化
ms <- rstan::extract(fit)
qua <- apply(ms$y_pred, 2, quantile, prob=c(0.1, 0.5, 0.9))
d_est <- data.frame(d, t(qua), check.names=FALSE)
d_est$A <- as.factor(d_est$A)

ggplot(d_est, aes(x=Y, y=`50%`, ymin=`10%`, ymax=`90%`, shape=A, color=A)) +
  geom_pointrange() +
  geom_abline(linetype="dashed") +
  labs(x="Observed", y="Predicted") 
