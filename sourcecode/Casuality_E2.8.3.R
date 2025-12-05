library("reshape2")
library("tidyverse")

led <- read.csv("leaders.csv")
summary(led)
head(led)

#exercise 1
n <- dim(led)[1]
leng <- length(unique(led$country))
period <- max(led$year) - min(led$year)
aver <- n / period
aver

#exercise 2
unique(led$result)
die <- unique(led$result)[c(2, 7, 8, 10)]
survive <- unique(led$result)[c(1, 3, 4, 5, 6, 9)]
led$success <- with(led, ifelse(result %in% die, 1, 0))
suc.rate <- mean(led$success)
suc.rate

#exercise 3
diff.poli <- mean(led$politybefore[led$success == 1]) - mean(led$politybefore[led$success == 0])
diff.age <- mean(led$age[led$success == 1]) - mean(led$age[led$success == 0])
diff.age
cor1 <- cor(led$politybefore, led$success)
cor2 <- cor(led$polityafter, led$success)
cor3 <- cor(led$age, led$success)

#exercise 4
led$warbefore <- with(led, ifelse(civilwarbefore == 1 | interwarbefore == 1, 1, 0))
led$warafter <- with(led, ifelse(civilwarafter == 1 | interwarafter == 1, 1, 0))
diff.war <- mean(led$warbefore[led$success == 1]) - mean(led$warbefore[led$success == 0])
diff.war
cor4 <- cor(led$success, led$warbefore)

#exercise 5
diff.treat.poli <- mean(led$polityafter[led$success == 1]) - mean(led$politybefore[led$success == 1])
diff.ctrl.poli <- mean(led$polityafter[led$success == 0]) - mean(led$politybefore[led$success == 0])
did.poli <- diff.treat.poli - diff.ctrl.poli
diff.treat.war <- mean(led$warafter[led$success == 1]) - mean(led$warbefore[led$success == 1])  
diff.ctrl.war <- mean(led$warafter[led$success == 0]) - mean(led$warbefore[led$success == 0])  
did.war <- diff.treat.war - diff.ctrl.war
did.war



