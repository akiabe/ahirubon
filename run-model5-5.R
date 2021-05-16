d <- read.csv("data-attendance-3.txt")
head(d, n = 3)

aggregate(Y ~ Weather, data = d, FUN = table)

conv <- c(0, 0.2, 1)
names(conv) <- c('A', 'B', 'C')
data <- list(
  I=nrow(d), 
  A=d$A, 
  Score=d$Score/200, 
  W=conv[d$Weather], 
  Y=d$Y
)

fit <- stan(
  file='model5-5.stan', 
  data=data, 
  seed=1
)

fit

save.image('result-model5-5.RData')
