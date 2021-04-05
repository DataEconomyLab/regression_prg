market2 = read.table("https://raw.githubusercontent.com/DataEconomyLab/regression/main/reg2020/market-2.txt", header=T)
head(market2, 3)

X = market2[, c(2,3)]
X = cbind(1, X)
Y = market2[,4]
X = as.matrix(X)
y = as.matrix(Y)
XTX = t(X) %*% X
XTX

XTX1 = solve(XTX)
XTY = t(X) %*% Y
beta = XTX1 %*% XTY
beta = round(beta, 3)
beta

market2.lm = lm(Y ~ X1 + X2, data=market2)
summary(market2.lm)

qf(0.95, 2, 12)

anova(market2.lm)

summary(market2.lm)

library(lm.beta)
market2.lm = lm(Y ~ X1 + X2, data=market2)
market2.beta = lm.beta(market2.lm)
print(market2.beta)

coef(market2.beta)
summary(market2.beta)

pred.x = data.frame(X1 = 10, X2=10)
pc = predict(market2.lm, int="c", newdata=pred.x)
pc

pc99 = predict(market2.lm, int="c", level=0.99, newdata=pred.x)
pc99


summary(market2.lm)

health = read.table("https://raw.githubusercontent.com/DataEconomyLab/regression/main/reg2020/health.txt", header=T)
head(health, 3)

h1.lm = lm(Y ~ X1, data=health)
h2.lm = lm(Y ~ X1 + X4, data=health)
h3.lm = lm(Y ~ X1 + X3 + X4, data=health)
h4.lm = lm(Y ~ X1 + X2 + X3 + X4, data=health)

anova(h1.lm, h2.lm)

anova(h2.lm, h3.lm)

anova(h3.lm, h4.lm)

library(car)
h4.lm = lm(Y ~ X1 + X2 + X3 + X4, data=health)
avPlots(h4.lm)

