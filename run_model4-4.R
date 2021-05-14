Sys.setenv(LANGUAGE="en")

library(rstan)

#rstan_options(auto_write = TRUE)
#options(mc.cores = parallel::detectCores())

d <- read.csv("data-salary.txt")
head(d, n = 5)

ggplot(data = d, aes(x = X, y = Y)) +
  theme_bw(base_size = 18) +
  geom_point(shape = 1, size = 3)

### lm ###
res_lm <- lm(Y ~ X, data = d)
res_lm

X_new <- data.frame(X = 23:60)

conf_95 <- predict(res_lm, X_new, interval = 'confidence', level = 0.95)
conf_95 <- data.frame(X_new, conf_95)
conf_50 <- predict(res_lm, X_new, interval = 'confidence', level = 0.50)
conf_50 <- data.frame(X_new, conf_50)
pred_95 <- predict(res_lm, X_new, interval = 'prediction', level = 0.95)
pred_95 <- data.frame(X_new, pred_95)
pred_50 <- predict(res_lm, X_new, interval = 'prediction', level = 0.50)
pred_50 <- data.frame(X_new, pred_50)

ggplot() +
  theme_bw(base_size = 18) +
  geom_ribbon(data = conf_95, aes(x = X, ymin = lwr, ymax = upr), alpha = 1/6) +
  geom_ribbon(data = conf_50, aes(x = X, ymin = lwr, ymax = upr), alpha = 2/6) +
  geom_line(data = conf_50, aes(x = X, y = fit), size = 1) +
  geom_point(data = d, aes(x = X, y = Y), shape = 1, size = 3) +
  labs(x = 'X', y = 'Y') + 
  coord_cartesian(xlim = c(22, 61), ylim = c(200, 1400)) +
  scale_y_continuous(breaks = seq(from = 200, to = 1400, by = 400))
  
##ggsave(p, file = 'output/fig4-3-left.png', dpi=300, w=4, h=3)

ggplot() +
  theme_bw(base_size = 18) +
  geom_ribbon(data = pred_95, aes(x = X, ymin = lwr, ymax = upr), alpha = 1/6) +
  geom_ribbon(data = pred_50, aes(x = X, ymin = lwr, ymax = upr), alpha = 2/6) +
  geom_line(data = pred_50, aes(x = X, y = fit), size = 1) +
  geom_point(data = d, aes(x = X, y = Y), shape = 1, size = 3) +
  labs(x = 'X', y = 'Y') + 
  coord_cartesian(xlim = c(22, 61), ylim = c(200, 1400)) +
  scale_y_continuous(breaks = seq(from = 200, to = 1400, by = 400))

##ggsave(p, file='output/fig4-3-right.png', dpi=300, w=4, h=3)

### Stan ###
data <- list(
  N = nrow(d),
  X = d$X,
  Y = d$Y,
  N_new = length(X_new),
  X_new = X_new
)

fit <- stan(
  file = "model4-4.stan",
  data = data,
  seed = 1
)

fit

library(ggmcmc)

write.table(
  data.frame(summary(fit)$summary),
  file = 'fit-summary.txt',
  sep = '\t',
  quote = FALSE,
  col.names = NA
)

ggmcmc(
  ggs(fit, inc_warmup = TRUE, stan_include_auxiliar = TRUE),
  file = 'fit-traceplot.pdf', 
  plot = 'traceplot'
)

#ggmcmc(
#  ggs(fit),
#  file = 'fit-ggmc.pdf',
#  plot = c('traceplot', 'density', 'running', 'autocorelation')
#)

stanmodel <- stan_model(file = 'model4-4.stan')

fit2 <- sampling(
  stanmodel,
  data = data,
  pars = c('b', 'sigma'),
  init = function() {
    list(a = runif(1, -10, 10), 
         b = runif(1, 0, 10), 
         sigma = 10)
  },
  seed = 1,
  chains = 3,
  iter = 1000,
  warmup = 200,
  thin = 2
)

ggmcmc(
  ggs(fit2, inc_warmup = TRUE, stan_include_auxiliar = TRUE),
  file = 'fit-traceplot2.pdf', 
  plot = 'traceplot'
)

ms <- rstan::extract(fit)
ms$b
quantile(ms$b, probs = c(0.025, 0.975))

df_mcmc <- data.frame(a = ms$a, b = ms$b, sigma = ms$sigma)
head(df_mcmc, n = 5)













































