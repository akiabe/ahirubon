library(ggplot2)

N <- 50
I <- 120

d_ori <- read.csv(file='data-lda.txt')
head(d_ori, n=5)

d <- subset(data.frame(table(d_ori)), Freq >= 1)
d$PersonID <- as.integer(as.character(d$PersonID))
d$ItemID <- as.integer(as.character(d$ItemID))
head(d, n=5)

ggplot(d, aes(x=ItemID, y=PersonID, fill=Freq)) +
  theme_bw(base_size=12) +
  theme(axis.text=element_blank(), axis.ticks=element_blank()) +
  geom_point(shape=21, color='white', size=1.5) +
  scale_fill_gradient(breaks=c(1,5,10), low='grey80', high='black')
