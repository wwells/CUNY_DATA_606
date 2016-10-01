#Week 2 lecture code on sampling for lottery via Dr. Bryer

tickets <- as.data.frame(rbind(
    c('$1', 1, 10),
    c('$2', 2, 20), 
    c('$5', 5, 52),
    c('$10', 10, 104),
    c('$20', 20, 208), 
    c('$50', 50, 520), 
    c('$100', 100, 1040),
    c('$1000', 1000, 10600),
    c('$10000', 10000, 100000),
    c('$1,000,000', 1000000, 10000000)
), stringsAsFactors = FALSE)

names(tickets) <- c("Winnings", "Value", "Odds")
tickets$Value <- as.integer(tickets$Value)
tickets$Odds <- as.integer(tickets$Odds)

odds <- sample(max(tickets$Odds), 1000, replace=TRUE)
vals <- rep(-1, length(odds))
for(i in 1:nrow(tickets)) {
    vals[odds %% tickets[i, 'Odds'] ==0] <- tickets[i, 'Value'] - 1
}

head(vals, n=20)
totalwon <- cumsum(vals)
tail(totalwon)