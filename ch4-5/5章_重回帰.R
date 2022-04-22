library(GGally)
library(tidyverse)
library(rstan)

d <- read.csv("data-attendance-1.txt")
head(d, n=5)

# 分布を可視化
ggpairs(d, aes(color=A),
        upper=list(continuous="density", combo="box_no_facet"), 
        lower=list(continuous="points", combo="dot_no_facet"))

# Stanでモデルを実装
data <- list(
  N=nrow(d), 
  A=d$A, 
  Score=d$Score/200, # スケーリング
  Y=d$Y) 

fit <- stan(
  file="multi_linear_regression.stan", 
  data=data, 
  seed=1234)

fit

# 実測値と予測値を可視化
ms <- rstan::extract(fit)
qua <- apply(ms$y_pred, 2, quantile, prob=c(0.1, 0.5, 0.9))
d_est <- data.frame(d, t(qua), check.names=FALSE)
d_est$A <- as.factor(d_est$A)

ggplot(data=d_est, aes(x=Y, y=`50%`, ymin=`10%`, ymax=`90%`, shape=A, color=A)) +
  geom_pointrange() +
  geom_abline(linetype="dashed") +
  labs(x="Observed", y="Predicted") 

# Stan内で計算したノイズを出力
ms$noise
