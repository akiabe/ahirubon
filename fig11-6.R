library(ggplot2)

N <- 50
I <- 120

d <- read.csv(file='data-lda.txt')
d_cast <- reshape2::dcast(d, PersonID ~ .)
head(d_cast, n=5)

ggplot(data=d_cast, aes(x=.)) + theme_bw(base_size=18) + 
  geom_bar(color='grey5', fill='grey80') + 
  scale_x_continuous(breaks=seq(10,40,10), limits=c(10,40)) + 
  labs(x='count by PersonID', y='count')

d_cast <- reshape2::dcast(d, ItemID ~ .)
ggplot(data=d_cast, aes(x=.)) + 
  theme_bw(base_size=18) + 
  geom_bar(color='grey5', fill='grey80') + 
  labs(x='count by ItemID', y='count')

