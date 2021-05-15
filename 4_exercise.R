set.seed(1)
N1 <- 30
N2 <- 20
Y1 <- rnorm(n = N1, mean = 0, sd = 5)
Y2 <- rnorm(n = N2, mean = 1, sd = 4)

### visualize ###
d1 <- data.frame(group = 1, Y = Y1)
d2 <- data.frame(group = 2, Y = Y2)
d <- rbind(d1, d2)
d$group <- as.factor(d$group)

ggplot(data = d, aes(x = group, y = Y, group = group, col = group)) +
  geom_boxplot(outlier.size = 0) +
  geom_point(position = position_jitter(w = 0.4, h = 0), size = 2)

### Student t-test ###
data <- list(
  N1 = N1,
  N2 = N2,
  Y1 = Y1,
  Y2 = Y2
)

fit <- stan(
  file = "4_ex3.stan",
  data = data,
  seed = 1
)

fit

ms <- rstan::extract(fit)
prob <- mean(ms$mu1 < ms$mu2)
prob

N_mcmc <- length(ms$mu1)
prob <- sum(ms$mu1 < ms$mu2) / N_mcmc
prob

### Welch t-test ###
fit <- stan(
  file = "4_ex5.stan",
  data = data,
  seed = 1
)

fit

ms <- rstan::extract(fit)
prob <- mean(ms$mu1 < ms$mu2)
prob

