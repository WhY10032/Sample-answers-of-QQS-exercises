library("ggplot2")
library("dplyr")
library("tidyverse")
library("reshape2")

it08 <- read.csv("intrade08.csv")
it12 <- read.csv("intrade12.csv")
summary(it08)
summary(it12)
pl08 <- read.csv("polls08.csv")
pres08 <- read.csv("pres08.csv")
pres12 <- read.csv("pres12.csv")
summary(pres08)

colnames(pres12) <- c("state", "Obama12", "Romney12", "EV12")
colnames(pres08) <- c("state.name", "state", "Obama08", "McCain08", "EV08")
#rename the variables to distinguish between 2008 and 2012 data
pres <- full_join(pres12, pres08, by = c("state"))
#merging columns using full_join
it08$day <- as.Date(it08$day)
it12$day <- as.Date(it12$day)
is.Date(it08$day)
#convert "day" to Date category

#exercise 1
it08.daybe <- subset(it08, subset = (day == "2008-11-03"))
it08.daybe$Dvic <- with(it08.daybe, ifelse(PriceD > PriceR, 1, ifelse(PriceD == PriceR, NA, 0)))
pres$Dvic08 <- ifelse(pres$Obama08 > pres$McCain08, 1, 0)
pres$predDvic08 <- it08.daybe$Dvic
mis_08 <- sum(pres$predDvic != pres$Dvic)

it12.daybe <- subset(it12, subset = (day == "2012-11-05"))
it12.daybe$Dvic <- with(it12.daybe, ifelse(PriceD > PriceR, 1, ifelse(PriceD == PriceR, NA, 0)))
pres$Dvic12 <- ifelse(pres$Obama12 > pres$Romney12, 1, 0)

state <-  unique(it12.daybe[ ,3])
state == unique(pres[ ,5])
#compare to find the lost state, which is D.C.
it12.daybe[51, 3] <- "D.C."
it12.daybe <- arrange(it12.daybe, statename)

pres$predDvic12 <- it12.daybe$Dvic
mis_12 <- sum(pres$predDvic12 != pres$Dvic12, na.rm = T)

#exercise 2
it08$todate <- it08$day - as.Date("2008-11-03")
it08.sub <- it08[it08$todate < 0 & it08$todate >= -90, ]
it08.sub$predDvic <- with(it08.sub, ifelse(PriceD > PriceR, 1, ifelse(PriceD == PriceR, NA, 0)))
it08.sub <- arrange(it08.sub, statename)
#note that it08.sub includes exactly 90 * 51 = 4590 observations, meaning one observation per state per day
#though it is usually not the case when unrecorded observations exist
vote08 <- rep(NA, 90)
for(i in 1:90) {
  it08.temp <- subset(it08.sub, subset = (todate == -91 + i))
  vote08[i] <- sum(it08.temp$predDvic * pres$EV08, na.rm = T)
}
vote08
# as said before, this method can NOT be applied to more general cases
# also, it is VERY VERT important to sort your data according to the SAME order before you do anything!!!
df <- data.frame(
  vote = vote08,
  date = unique(it08.sub$day)
)
ggplot(df, aes(x = date, y = vote)) +
  geom_line() +
  geom_hline(yintercept = 365, col = "red")

# exercise 3
it08.sub1 <- it08[it08$todate < 0 & it08$todate >= -96, ]
it08.sub1 <- arrange(it08.sub1, by = statename)

vote08.avg <- rep(NA, 90)
for(i in 1:90) {
  it08.temp <- subset(it08.sub1, select = c("statename", "PriceD", "PriceR", "todate"), subset = (todate >= -97 + i & todate <= -91 + i))
  tempD <- aggregate(PriceD ~ statename, data = it08.temp, FUN = mean)
  tempR <- aggregate(PriceR ~ statename, data = it08.temp, FUN = mean)
  tempTF <- ifelse(tempD$PriceD > tempR$PriceR, 1, ifelse(tempD$PriceD == tempR$PriceR, NA, 0))
  vote08.avg[i] <- sum(tempTF * pres$EV08, na.rm = T)
}
vote08.avg
df.avg <- data.frame(
  vote = vote08.avg,
  date = unique(it08.sub$day)
)
ggplot(df.avg, aes(x = date, y = vote)) +
  geom_line() +
  geom_hline(yintercept = 365, col = "red")

# exercise 4
pl08$middate <- as.Date(pl08$middate)
pl08 <- arrange(pl08, middate)
pl08$margin <- pl08$Obama - pl08$McCain
pl08$todate <- as.Date("2008-11-03") - pl08$middate

pl08.sub <- pl08[pl08$todate > 0 & pl08$todate <= 105, ]
pl08.sub <- arrange(pl08.sub, by = state)

meanmar <- rep(NA, 51)
stateabbr <- unique(pl08.sub$state)
poll <- rep(NA, 90)
pres.2 <- arrange(pres, by = state)
#note that order of state names and order of state abbreviations are not the SAME!!!
for(i in 1:90) {
  for(j in 1:51) {
    pl.temp <- subset(pl08.sub, subset = (state == stateabbr[j]))
    pl.temp <- subset(pl.temp, subset = (todate - i < 15 & todate - i >= 0))
    temp <- mean(pl.temp$margin)
    meanmar[j] <- ifelse(temp > 0, 1, ifelse(temp == 0, NA, 0))
  }
  poll[91 - i] <- sum(meanmar * pres.2$EV08, na.rm = T)
}
poll
df.poll <- data.frame(
  vote = poll,
  date = unique(it08.sub$day)
)
ggplot(df.poll, aes(x = date, y = vote)) +
  geom_line() +
  geom_hline(yintercept = 365, col = "red")

# exercise 5
it08.daybe$margin <- it08.daybe$PriceD - it08.daybe$PriceR
pres$margin <- pres$Obama08 - pres$McCain08
fit1 <- lm(pres$margin ~ it08.daybe$margin)
fit1
cor(pres$margin, it08.daybe$margin)

margin <- rep(NA, 51)
for(j in 1:51) {
  pl.temp <- subset(pl08, subset = (state == stateabbr[j]))
  pl.temp <- subset(pl.temp, subset = (todate == min(todate)))
  margin[j] <- mean(pl.temp$margin)
}
margin
pres.2$margin <- pres.2$Obama08 - pres.2$McCain08
fit2 <- lm(pres.2$margin ~ margin)
fit2
cor(pres.2$margin, margin)



