data <- read.csv(file = "data1.csv")

oneWayAnova <- function(col) {
  layout(matrix(c(1,1,2,3,4,5), 3, 2, byrow = TRUE))
  boxplot(data[[col]] ~ factor(W), data = data, main = paste(col, " versus W"), xlab = "W", ylab = col)
  fit <- aov(data[[col]] ~ factor(W), data = data)
  plot(fit)

  print("Fitted model:")
  print(fit)
  
  print("Anova table:")
  print(summary(fit))
  
  print("Shapiro-Wilk test for normality of residuals:")
  print(shapiro.test(fit$residuals))
  
  print("Homogenity of variances:")
  print(bartlett.test(data[[col]] ~ factor(W), data = data))
  
  return(fit)
}

twoAdHoc <- function(fit) {
  print("Tukey HSD method:")
  print(TukeyHSD(fit))
  
  print("Pairwise t-tests method:")
  
}

par(mfrow = c(2,2))
plot(y = data[["Y"]], x = data[["W"]], xlab = "W", ylab = "Y", main = "W vs Y")
plot(y = data[["X1"]], x = data[["W"]], xlab = "W", ylab = "X1", main = "W vs X1")
plot(y = data[["X2"]], x = data[["W"]], xlab = "W", ylab = "X2", main = "W vs X2")
plot(y = data[["X3"]], x = data[["W"]], xlab = "W", ylab = "X3", main = "W vs X3")

fitY <- oneWayAnova("Y")
print(TukeyHSD(fitY))
pairwise.t.test(data[["Y"]], data[["W"]], p.adjust.method = "holm")

fitX1 <- oneWayAnova("X1")
print(TukeyHSD(fitX1))
pairwise.t.test(data[["X1"]], data[["W"]], p.adjust.method = "holm")

fitX2 <- oneWayAnova("X2")
print(TukeyHSD(fitX2))
pairwise.t.test(data[["X2"]], data[["W"]], p.adjust.method = "holm")

fitX3 <- oneWayAnova("X3")
print(TukeyHSD(fitX3))
pairwise.t.test(data[["X3"]], data[["W"]], p.adjust.method = "holm")

kruskal.test(data[["Y"]] ~ data[["W"]])
kruskal.test(data[["X1"]] ~ data[["W"]])
kruskal.test(data[["X2"]] ~ data[["W"]])
kruskal.test(data[["X3"]] ~ data[["W"]])

par(mfrow = c(3,1))
plot(x = data[["X1"]], y = data[["Y"]], ylab = "Y", xlab = "X1", main = "Plot Y ~ X1")
plot(x = data[["X2"]], y = data[["Y"]], ylab = "Y", xlab = "X2", main = "Plot Y ~ X2")
plot(x = data[["X3"]], y = data[["Y"]], ylab = "Y", xlab = "X3", main = "Plot Y ~ X3")

par(mfrow = c(3,1))
plot(x = data[["X1"]], y = data[["Y"]], ylab = "Y", xlab = "X1", main = "Plot Y ~ X1", col = data[["W"]])
legend('topright', legend = levels(data[["W"]]), col = 1:3, cex = 0.8, pch = 1)
plot(x = data[["X2"]], y = data[["Y"]], ylab = "Y", xlab = "X2", main = "Plot Y ~ X2", col = data[["W"]])
legend('bottomright', legend = levels(data[["W"]]), col = 1:3, cex = 0.8, pch = 1)
plot(x = data[["X3"]], y = data[["Y"]], ylab = "Y", xlab = "X3", main = "Plot Y ~ X3", col = data[["W"]])
legend('topleft', legend = levels(data[["W"]]), col = 1:3, cex = 0.8, pch = 1)

fit <- lm(Y ~ X1 + X2 + X3 + W + X1:W + X2:W + X3:W, data = data)
print(summary(fit))
par(mfrow = c(2,2))
plot(fit)
shapiro.test(fit$residuals)

fitnull <- lm(Y ~ 1, data = data)
stepSR <- step(fitnull, scope = list(lower = ~ 1, upper = ~ X1 + X2 + X3 + W + X1:W + X2:W + X3:W), direction = "both", data = data)
print(stepSR)

library(leaps)
leaps <- regsubsets(Y ~ X1 + X2 + X3 + W + X1:W + X2:W + X3:W, data = data, nbest = 1)
print(summary(leaps))
plot(leaps,scale="r2")

bestfit <- lm(Y ~ X2 + X1 + W + X2:W + X1:W, data = data)
print(anova(bestfit))

testdata <- data.frame(X1 = 3.1, X2 = 3.75, X3 = 1.2, W = "A")
predict.lm(bestfit, newdata = testdata)