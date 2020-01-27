library(lme4)

l <- lm(accuracy ~ species, data = sp)
summary(l)

l <- lm(accuracy ~ angle, data = sp)
summary(l)

l <- lm(accuracy ~ background, data = sp)
summary(l)

sp$ai01 <- ifelse(sp$antennalIllusion, 1, 0)

l <- lm(accuracy ~ ai01, data = sp)
summary(l)

l <- lm(accuracy ~ angle + background + ai01, data = sp)
summary(l)

#Intercept is accuracy at reference levels of each variable
#Others are difference from intercept with that variable value

#Use predict to get std errors for non-intercept values
sp$ai01 <- ifelse(sp$antennalIllusion, 1, 0)
newData <- expand.grid(angle = levels(factor(sp$angle)), background = levels(factor(sp$background)), ai01 = c(0, 1))
p1 <- predict(l, newData, se = TRUE)
newData$predictedAccuracy <- p1$fit
newData$se <- p1$se.fit
newData$upperCI <- newData$predictedAccuracy + newData$se * 1.96
newData$lowerCI <- newData$predictedAccuracy - newData$se * 1.96


l <- lmer(accuracy ~ angle + background + ai01 + (1|species), REML = FALSE, data = sp)
summary(l)
# use predict as before
newData <- expand.grid(angle = levels(factor(sp$angle)), background = levels(factor(sp$background)), ai01 = c(0, 1))
p1 <- predict(l, newdata = newData, se = TRUE, re.form = NA)
newData$predictedAccuracy <- p1$fit
newData$se <- p1$se.fit
newData$upperCI <- newData$predictedAccuracy + newData$se * 1.96
newData$lowerCI <- newData$predictedAccuracy - newData$se * 1.96
