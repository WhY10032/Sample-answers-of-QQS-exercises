install.packages("ggplot2")
library("tidyverse")
library("patchwork")

v <- read.csv("vignettes.csv")
summary(v)
dim(v)

# exercise 1
v.proptab <- prop.table(with(v, table(self = self, china = china)))
v.proptab
as.data.frame(v.proptab)
barplot1 <- ggplot(as.data.frame(v.proptab), aes(x = self, y = Freq, fill = china)) +
  geom_bar(stat = "identity", position = "dodge")
barplot1
meanchina <- mean(v$self[v$china == 1])
meanmexico <- mean(v$self[v$china == 0])
meanchina > meanmexico

#exercise 2
plot2 <- ggplot(data = v, aes(x = age, fill = factor(china))) +
  geom_histogram(position = "dodge") +
  geom_vline(aes(xintercept = median(age[china == 1])), colour = "#00BFC4") +
  geom_vline(aes(xintercept = median(age[china == 0])), colour = "#F8766D")
plot2
age_china <- v$age[v$china == 1]
age_mexico <- v$age[v$china == 0]
prob <- 1:100 / 100
agesc_china <- quantile(age_china, probs = prob)
agesc_mexico <- quantile(age_mexico, probs = prob)
data3 <- data.frame(agesc_china = agesc_china, agesc_mexico = agesc_mexico)
qqplot3 <- ggplot(data = data3, aes(x = agesc_china, y = agesc_mexico)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, colour = "#F8766D")
qqplot3

#exercise 3
v$true <- ifelse(v$self < v$moses, 1, 0)
propchina <- with(v, sum(true[china == 1]) / length(self[china == 1]))
propmexico <- with(v, sum(true[china == 0]) / length(self[china == 0]))

#exercise 4
v$rank <- ifelse(v$self < v$moses, 1, ifelse(v$self < v$jane, 2, ifelse(v$self < v$alison, 3, 4)))
v.rank.prop <- prop.table(with(v, table(rank = rank, china = china)))
v.rank.prop
barplot2 <- ggplot(as.data.frame(v.rank.prop), aes(x = rank, y = Freq, fill = china)) +
  geom_bar(stat = "identity", position = "dodge")
barplot2
meanrankchina <- mean(v$rank[v$china == 1])
meanrankmexico <- mean(v$rank[v$china == 0])
meanrankchina > meanrankmexico

#exercise 5
v.young <- subset(v, subset = (age < 40))
v.old <- subset(v, subset = (age >= 40))
v.rank.young.prop <- prop.table(with(v.young, table(rank = rank, china = china)))
barplot3 <- ggplot(as.data.frame(v.rank.young.prop), aes(x = rank, y = Freq, fill = china)) +
  geom_bar(stat = "identity", position = "dodge")
barplot3
v.rank.old.prop <- prop.table(with(v.old, table(rank = rank, china = china)))
barplot4 <- ggplot(as.data.frame(v.rank.old.prop), aes(x = rank, y = Freq, fill = china)) +
  geom_bar(stat = "identity", position = "dodge")
barplot4

comb.barplot <- barplot3 + barplot4
comb.barplot

