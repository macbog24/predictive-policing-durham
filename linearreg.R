
knitr::opts_chunk$set(echo = TRUE)

crimes <- read.csv("durham_crime.csv", stringsAsFactors = FALSE)
counts <- table(crimes$hour_occur)
counts
barplot(counts, main="Crime Distribution by Time", xlab="Time of Day")

lm.count1 <- lm(X ~ hour_occur, data = crimes)
summary(lm.count1)
lm.count2 <- lm(X ~ hour_occur + month_occur, data = crimes)
summary(lm.count2)
lm.count3 <- lm(X ~ hour_occur + month_occur + year_occur, data = crimes)
summary(lm.count3)
lm.count4 <- lm(X ~ hour_occur + month_occur + year_occur + year_rept, data = crimes)
summary(lm.count4)




