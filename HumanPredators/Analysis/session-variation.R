# Report on per-session variation

lmRSq <- function(l) bquote(R^2 ~ "=" ~ .(round(summary(l)$r.squared, 2)))

ss <- MDbRun(GetDecisionScores)

# sss <- SessionStats(ss)
# rejectedSessionIds <- sss$sessionId[sss$reject]
# unique(ss$created_at[ss$sessionId %in% rejectedSessionIds])

par(mfrow = c(3, 1))
plot(time ~ imageNumber, data = ss, ylab = "Decision time", main = "Decision time")
l <- lm(time ~ imageNumber, data = ss)
abline(l, col = "red")
mtext(lmRSq(l), line = -2, adj = .98)
# Plot escape time
abline(h = 5000, col = "lightGrey")

plot(as.factor(score) ~ imageNumber, data = ss, ylab = "Decision", main = "Scores")
abline(h = 0.5, col = "red", lty = 2)

# Do people either learn or get tired throughout a trial?
# Look at how the proprotion of incorrect answers changes over a trial
pIncorrect <- 1 - aggregate(ss$correct, list(ss$imageNumber), FUN = sum)[[2]] / aggregate(ss$correct, list(ss$imageNumber), FUN = length)[[2]]
names(pIncorrect) <- 1:length(pIncorrect)
barplot(pIncorrect, ylab = "Proportion incorrect", main = "Learning or fatigue?")
#l <- lm(pIncorrect ~ (1:15))
# abline(l, col = "red")
# sl <- summary(l)
# mtext(bquote(R^2 ~ "=" ~ .(round(sl$r.squared, 2))), line = -2, adj = .98)

