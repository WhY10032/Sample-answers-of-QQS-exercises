library("tidyverse")
library("dplyr")
library("patchwork")
library("reshape2")

pro <- read.csv("progresa.csv")
summary(pro)

# exercise 1
diffinmean_turnout <- with(data = pro, mean(t2000[treatment == 1]) - mean(t2000[treatment == 0]))
diffinmean_spt <- with(data = pro, mean(pri2000s[treatment == 1]) - mean(pri2000s[treatment == 0]))
fit_t <- lm(t2000 ~ treatment, data = pro)
fit_s <- lm(pri2000s ~ treatment, data = pro)
fit_t

# exercise 2
fit_total1 <- lm(t2000 ~ avgpoverty + pobtot1994 + votos1994 + pri1994 + pan1994 + prd1994 + treatment, data = pro)
fit_total2 <- fit_total <- lm(pri2000s ~ avgpoverty + pobtot1994 + votos1994 + pri1994 + pan1994 + prd1994 + treatment, data = pro)
fit_total2

# exercise 3
fit_total1_share <- lm(t2000 ~ avgpoverty + log(pobtot1994) + t1994 + pri1994s + pan1994s + prd1994s + treatment, data = pro)
fit_total2_share <- lm(pri2000s ~ avgpoverty + log(pobtot1994) + t1994 + pri1994s + pan1994s + prd1994s + treatment, data = pro)
fit_total2_share

# exercise 4
pop <- ggplot(data = pro, aes(x = as.factor(treatment), y = pobtot1994)) +
  geom_boxplot(position = "dodge")
pov <- ggplot(data = pro, aes(x = as.factor(treatment), y = avgpoverty)) +
  geom_boxplot(position = "dodge")
turnout <- ggplot(data = pro, aes(x = as.factor(treatment), y = t1994)) +
  geom_boxplot(position = "dodge")
spt <- ggplot(data = pro, aes(x = as.factor(treatment), y = pri1994s)) +
  geom_boxplot(position = "dodge")
spt
combined_plot <- pop + pov +turnout + spt
combined_plot

# exercise 5
fit_total1_offi <- lm(t2000r ~ avgpoverty + log(pobtot1994) + t1994r + pri1994v + pan1994v + prd1994v + treatment, data = pro)
fit_total2_offi <- lm(pri2000v ~ avgpoverty + log(pobtot1994) + t1994r + pri1994v + pan1994v + prd1994v + treatment, data = pro)
fit_total2_offi

# exercise  6
fit_final <- lm(t2000r ~ treatment + log(pobtot1994) + I(avgpoverty^2) + avgpoverty + I(avgpoverty^2):treatment + avgpoverty:treatment, data = pro)
fit_final

X = 300:500/100
Y_treat = predict(fit_final, newdata = data.frame(avgpoverty = (300:500/100), treatment = 1, pobtot1994 = median(pro$pobtot1994)))
Y_ctrl = predict(fit_final, newdata = data.frame(avgpoverty = (300:500/100), treatment = 0, pobtot1994 = median(pro$pobtot1994)))
df <- data.frame(x = X, y_treat = Y_treat, y_ctrl = Y_ctrl)
df$effect <- df$y_treat - df$y_ctrl

df.long <- melt(df, id.vars = c("x"), measure.vars = c("y_treat", "y_ctrl", "effect"))
ggplot(df.long, aes(x = x, y = value, colour = as.factor(variable))) +
  geom_line() +
  scale_colour_discrete(
    labels = c("Treatment", "Control", "ATE")
  )




