market = read.table("C:/work/data/reg2020/market-1.txt", header=TRUE)
head(market)

market.lm = lm(Y ~ X, data=market)
summary(market.lm)

plot(market$X, market$Y, xlab="광고료", ylab="총판매액", pch=19)
title("광고료와 판매액의 산점도")
abline(market.lm)

