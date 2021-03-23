market = read.table("C:/work/data/reg2020/market-1.txt", header=TRUE)
head(market)

market.lm = lm(Y ~ X, data=market)
summary(market.lm)

plot(market$X, market$Y, xlab="�����", ylab="���Ǹž�", pch=19)
title("������ �Ǹž��� ������")
abline(market.lm)

