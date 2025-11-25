install.packages("tidyverse")
library("tidyverse")

# exercise 1
star <- read.csv("STAR.csv")
summary(star)
dim(star)
star$kinder <- ifelse(star$classtype == 1, "small", ifelse(star$classtype == 2, "regular", "regular_with_aide"))
star$race <- ifelse(star$race == 1, "white", ifelse(star$race == 2, "african_american", ifelse(star$race == 3, "aisan", "others")))

# exercise 2
diffinmean_r <- mean(star$g4reading[star$classtype == 1], na.rm = TRUE) - mean(star$g4reading[star$classtype == 2], na.rm = T)
diffinmean_m <- mean(star$g4math[star$classtype == 1], na.rm = TRUE) - mean(star$g4math[star$classtype == 2], na.rm = T)
sd_r <- sd(star$g4reading, na.rm = T)
sd_m <- sd(star$g4math, na.rm = T)

# exercise 3
diffinhigh_r <- with(star, quantile(g4reading[star$classtype == 1], probs = 0.66, na.rm = T) - quantile(g4reading[star$classtype == 2], probs = 0.66, na.rm = T))
# the rest can be easily done by similar code

# exercise 4
star.tab <- with(star, table(kinder = kinder, year = yearssmall))
star.proptab <- prop.table(star.tab)
star.proptab
star.nona <- na.omit(star)
mean1 <- with(star.nona, aggregate(star.nona, by = list(year = yearssmall), FUN = mean))
median1 <- with(star.nona, aggregate(star.nona, by = list(year = yearssmall), FUN = median))

# exercise 5
MathWhiteSmall <- with(subset(star.nona, select = c("race", "classtype", "g4math", "g4reading"), subset = (classtype == 1 & race == "white")), mean(g4math))
# the rest can be easily done by similar code

#exercise 6
mean_gra <- with(star.nona, aggregate(star.nona, by = list(classtype = classtype), FUN = mean))
mean_gra_yearsmall <- with(star.nona, aggregate(star.nona, by = list(year = yearssmall), FUN = mean))
mean_gra_race <- with(star.nona, aggregate(star.nona, by = list(race = race), FUN = mean))



