library(tidyverse)
library(rstan)

d <- read.csv("data-attendance-3.txt")
head(d, n=5)

# 天気ごとにYの分布の確認
d %>%
  group_by(Weather, Y) %>% # dplyrを使った場合
  summarise(count = n())

aggregate(Y ~ Weather, data=d, FUN=table) # aggregateを使った場合

# アルバイトが好きかどうかごとにYの分布を確認
d %>% 
  group_by(A, Y) %>% 
  summarise(count = n())

conv <- c(0, 0.2, 1)
names(conv) <- c("A", "B", "C")

data <- list(
  I=nrow(d),
  A=d$A,
  Score=d$Score/200,
  W=conv[d$Weather],
  Y=d$Y
)

# Stanでモデルの実装
fit <- stan(
  file="logregression.stan",
  data=data,
  seed=1234
)

fit

# 確率と実測値の可視化
ms <- rstan::extract(fit)
qua <- apply(ms$q, 2, quantile, prob=c(0.1, 0.5, 0.9))
d_est <- data.frame(d, t(qua), check.names=FALSE)
d_est$A <- as.factor(d_est$A)
d_est$Y <- as.factor(d_est$Y)

ggplot(d_est, aes(x=Y, y=`50%`, shape=A, color=A)) +
  coord_flip() +
  geom_jitter() +
  geom_violin() +
  labs(x="Y", y="q") 

# ROC曲線
library(pROC)

spec <- seq(from=0, to=1, len=201)
N_mcmc <- length(ms$lp__)
N_spec <- length(spec)

auces <- numeric(N_mcmc)
m_roc <- matrix(nrow=N_spec, ncol=N_mcmc)
for (i in 1:N_mcmc) {
  roc_res <- roc(d$Y, ms$q[i,], quiet=TRUE)
  auces[i] <- as.numeric(roc_res$auc)
  m_roc[,i] <- coords(roc_res, x=spec, input="specificity", 
                      ret="sensitivity")$sensitivity
}

quantile(auces, prob=probs)

d_est <- data.frame(
  X=1-spec, 
  t(apply(m_roc, 1, quantile, prob=c(0.1, 0.5, 0.9))), 
  check.names=FALSE)

ggplot(data=d_est, aes(x=X, y=`50%`)) +
  geom_abline(linetype="dashed") +
  geom_ribbon(aes(ymin=`10%`, ymax=`90%`), alpha=2/6) +
  geom_line() +
  labs(x="False Positive", y="True Positive")

# 晴れの影響を0とし、曇りと雨の影響をパラメータとするモデルの場合
d_conv <- data.frame(X=c(1, 2, 3))
rownames(d_conv) <- c("A", "B", "C")

data <- list(
  I=nrow(d),
  A=d$A,
  Score=d$Score/200,
  WID=d_conv[d$Weather,],
  Y=d$Y
)

fit <- stan(
  file="logregression_v2.stan",
  data=data,
  seed=1234
)

fit

