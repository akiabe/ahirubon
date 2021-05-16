library(rstan)

d <- read.csv(file = 'data-attendance-1.txt')
head(d, n = 5)

data <- list(
  N = nrow(d),
  A = d$A,
  Score = d$Score/200,
  Y = d$Y
)

fit <- stan(
  file = "model5-3.stan",
  data = data,
  seed = 1
)

fit

save.image('result-model5-3.RData')

