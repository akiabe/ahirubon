## Y1とY2の平均に差があるかを調べる。

# データの生成
set.seed(123)
N1 <- 30
N2 <- 20
Y1 <- rnorm(n=N1, mean=0, sd=5)
Y2 <- rnorm(n=N2, mean=1, sd=4)

# 分布の可視化
library(tidyverse)

d1 <- data.frame(group=1, Y=Y1)
d2 <- data.frame(group=2, Y=Y2)
d <- rbind(d1, d2)
d$group <- as.factor(d$group)

ggplot(d, aes(x=group, y=Y, group=group, color=group)) +
  geom_boxplot() +
  geom_point() +
  geom_jitter()

# studentのt検定（sigmaが等しい場合）
library(rstan)

data <- list(N1=N1, N2=N2, Y1=Y1, Y2=Y2)
fit <- stan(file="student_ttest.stan", data=data, seed=1234)

# mu1 < mu2となる確率を計算
ms <- extract(fit)
N_mcmc <- length(ms$mu1)
sum(ms$mu1 < ms$mu2)/N_mcmc

# Welchのt検定（sigmaが異なる場合）
fit <- stan(file="welch_ttest.stan", data=data, seed=1234)
ms <- extract(fit)
N_mcmc <- length(ms$mu1)
sum(ms$mu1 < ms$mu2)/N_mcmc
