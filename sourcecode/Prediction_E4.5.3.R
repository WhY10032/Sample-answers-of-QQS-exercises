library("tidyverse")
library("reshape2")
library("patchwork")
trans <- read.csv("transfer.csv")
summary(trans)

#exercise 1

# the assumption is that those observations barely passing the threshold
# and those barely not passing the threshold should be similar in terms of
# non-treatment attributes. In this context, cities whose population barely pass the 
# threshold of receiving government transfers should be similar in non-treatment 
# attributes (attributes other than receiving government transfer) with those barely
# not passing the threshold.

# if the population is not as-if random (for example, is correlated with other confounders)

# high internal validity, low external validity

#exercise 2

thre1 <- 10188
thre2 <- 13584
thre3 <- 16980
mid12 <- (thre1 + thre2) / 2
mid23 <- (thre2 + thre3) / 2

threshold2 <- Vectorize(
  function(x) {
    if(x >= mid23) {
      thre3
    }
    else if(x < mid23 & x >= mid12) {
      thre2
    }
    else {
      thre1
    }
  }
)
trans$thres <- threshold2(trans$pop82)
trans$normaldis <- with(trans, (pop82 - thres) / thres * 100)

# exercise 3 & 4

trans.sub <- subset(trans, subset = (abs(normaldis) <= 3))
trans.posi <- subset(trans.sub, subset = (normaldis > 0))
trans.nega <- subset(trans.sub, subset = (normaldis < 0))
trans.posi.all <- subset(trans, subset = (normaldis > 0))
trans.nega.all <- subset(trans, subset = (normaldis < 0))

fit.posi.edu <- lm(educ91 ~ normaldis, data = trans.posi)
fit.posi.pov <- lm(poverty91 ~ normaldis, data = trans.posi)
fit.posi.lit <- lm(literate91 ~ normaldis, data = trans.posi)

fit.nega.edu <- lm(educ91 ~ normaldis, data = trans.nega)
fit.nega.pov <- lm(poverty91 ~ normaldis, data = trans.nega)
fit.nega.lit <- lm(literate91 ~ normaldis, data = trans.nega)

reg.edu <- fit.posi.edu$coefficients[1] - fit.nega.edu$coefficients[1]
reg.lit
reg.pov <- fit.posi.pov$coefficients[1] - fit.nega.pov$coefficients[1]
reg.lit <- fit.posi.lit$coefficients[1] - fit.nega.lit$coefficients[1]

range.posi <- c(0, max(trans.posi$normaldis))
range.nega <- c(min(trans.nega$normaldis), 0)
codomain.posi.edu <- predict(fit.posi.edu, newdata = data.frame(normaldis = range.posi))
codomain.nage.edu <- predict(fit.nega.edu, newdata = data.frame(normaldis = range.nega))
codomain.posi.pov <- predict(fit.posi.pov, newdata = data.frame(normaldis = range.posi))
codomain.nage.pov <- predict(fit.nega.pov, newdata = data.frame(normaldis = range.nega))
codomain.posi.lit <- predict(fit.posi.lit, newdata = data.frame(normaldis = range.posi))
codomain.nage.lit <- predict(fit.nega.lit, newdata = data.frame(normaldis = range.nega))

p1 <- ggplot() +
  geom_point(data = trans.sub, aes(x = normaldis, y = educ91)) +
  geom_line(aes(x = range.nega, y = codomain.nage.edu), color = "blue") +
  geom_line(aes(x = range.posi, y = codomain.posi.edu), color = "blue") +
  geom_vline(xintercept = 0, color = "red", linetype = "dashed")

p2 <- ggplot() +
  geom_point(data = trans.sub, aes(x = normaldis, y = poverty91)) +
  geom_line(aes(x = range.nega, y = codomain.nage.pov), color = "blue") +
  geom_line(aes(x = range.posi, y = codomain.posi.pov), color = "blue") +
  geom_vline(xintercept = 0, color = "red", linetype = "dashed")

p3 <- ggplot() +
  geom_point(data = trans.sub, aes(x = normaldis, y = literate91)) +
  geom_line(aes(x = range.nega, y = codomain.nage.lit), color = "blue") +
  geom_line(aes(x = range.posi, y = codomain.posi.lit), color = "blue") +
  geom_vline(xintercept = 0, color = "red", linetype = "dashed")

p1 + p2 + p3

# exercise 5
dimean.edu <- mean(trans.posi.all$educ91, na.rm = T) - mean(trans.nega.all$educ91, na.rm = T)
dimean.pov <- mean(trans.posi.all$poverty91, na.rm = T) - mean(trans.nega.all$poverty91, na.rm = T)
dimean.lit <- mean(trans.posi.all$literate91, na.rm = T) - mean(trans.nega.all$literate91, na.rm = T)
dimean.lit

# exercise 6

reg.edu2 <- rep(NA, 5)
reg.pov2 <- rep(NA, 5)
reg.lit2 <- rep(NA, 5)

for(j in 1:5) {
  trans.s <- subset(trans, subset = (abs(normaldis) <= j))
  trans.p <- subset(trans.s, subset = (normaldis > 0))
  trans.n <- subset(trans.s, subset = (normaldis < 0))
  
  fit.posi.edu <- lm(educ91 ~ normaldis, data = trans.p)
  fit.posi.pov <- lm(poverty91 ~ normaldis, data = trans.p)
  fit.posi.lit <- lm(literate91 ~ normaldis, data = trans.p)
  
  fit.nega.edu <- lm(educ91 ~ normaldis, data = trans.n)
  fit.nega.pov <- lm(poverty91 ~ normaldis, data = trans.n)
  fit.nega.lit <- lm(literate91 ~ normaldis, data = trans.n)
  
  reg.edu2[j] <- as.numeric(fit.posi.edu$coefficients[1]) - as.numeric(fit.nega.edu$coefficients[1])
  reg.pov2[j] <- as.numeric(fit.posi.pov$coefficients[1]) - as.numeric(fit.nega.pov$coefficients[1])
  reg.lit2[j] <- as.numeric(fit.posi.lit$coefficients[1]) - as.numeric(fit.nega.lit$coefficients[1])
}
data <- data.frame(
  education = reg.edu2,
  poverty = reg.pov2,
  literacy = reg.lit2
)
data

# exercise 7
trans.s <- subset(trans, subset = (abs(normaldis) <= 3))
trans.p <- subset(trans.s, subset = (normaldis > 0))
trans.n <- subset(trans.s, subset = (normaldis < 0))

fit.posi.edu <- lm(educ80 ~ normaldis, data = trans.p)
fit.posi.pov <- lm(poverty80 ~ normaldis, data = trans.p)

fit.nega.edu <- lm(educ80 ~ normaldis, data = trans.n)
fit.nega.pov <- lm(poverty80 ~ normaldis, data = trans.n)

reg.edu3 <- as.numeric(fit.posi.edu$coefficients[1]) - as.numeric(fit.nega.edu$coefficients[1])
reg.pov3 <- as.numeric(fit.posi.pov$coefficients[1]) - as.numeric(fit.nega.pov$coefficients[1])

reg.pov3
