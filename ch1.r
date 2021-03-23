market = read.table("C:/work/data/reg2020/market-1.txt", header=TRUE)
head(market)

market.lm = lm(Y ~ X, data=market)
summary(market.lm)

plot(market$X, market$Y, xlab="광고료", ylab="총판매액", pch=19)
title("광고료와 판매액의 산점도")
abline(market.lm)

names(market.lm)
market.lm$resid
sum(market.lm$resid)
sum(market$X * market.lm$resid)

anova(market.lm)

qf(0.95, 1, 13)
1-pf(192.9,1,13)

summary(market.lm)

pred.frame = data.frame(X=seq(3.5, 14.5, 0.2))
pc = predict(market.lm, int="c", newdata=pred.frame)
pp = predict(market.lm, int="p", newdata=pred.frame)
head(pc)
head(pp)

pc = predict(market.lm, int="c", newdata=pred.frame)
pp = predict(market.lm, int="p", newdata=pred.frame)

pred.X = pred.frame$X
pred.X

plot(market$X, market$Y, ylim=range(market$Y, pp))
matlines(pred.X, pc, lty=c(1,2,3), col="BLUE")
matlines(pred.X, pp, lty=c(1,3,3), col="RED")

summary(market.lm)
2*(1-pt(13.889, 13))

x = c(1,2,3,4,5)
y = c(2,3,5,8,7)
w = 1/x
w.lm = lm(y ~ x, weights=w)
summary(w.lm)

anova(w.lm)

super = read.table("C:/work/data/reg2020/supermarket.txt", header=T)
head(super,3)

attach(super)
plot(price, time, pch=19)

super.lm = lm(time ~ price, data=super)
summary(super.lm)

anova(super.lm)
names(super.lm)

cbind(super, super.lm$resid, super.lm$fitted)
plot(price, super.lm$resid, pch=19)
abline(h=0, lty=2)

p.x = data.frame(price=c(1,45))
pc = predict(super.lm, int="c", newdata=p.x)
pred.x = p.x$price
plot(super$price, super$time, ylim=range(super$time, pc))
matlines(pred.x, pc, lty=c(1,2,2), col="blue")
