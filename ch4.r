tcrime = read.table("C:/Users/ljyjj/Documents/GitHub/regression/reg2020/tcrime.txt", header=T)
head(tcrime)

attach(tcrime)
plot(motor, tcratio, pch=19)

tcrime.lm = lm(tcratio ~ motor + I(motor^2), data=tcrime)
summary(tcrime.lm)

maraton = read.table("C:/Users/ljyjj/Documents/GitHub/regression/reg2020/maraton.txt", header=T)
head(maraton, 2)
plot(maraton$sect, maraton$m1990, pch=19)

maraton.lm = lm(m1990 ~ I(sect^2) + I(sect^3), data=maraton)
summary(maraton.lm)

soup = read.table("C:/Users/ljyjj/Documents/GitHub/regression/reg2020/soup.txt", header=T)
soup$D=factor(soup$D, levels=c(0,1), label=c("Line0", "Line1"))
soup[c(1, 15, 16, 27),]

plot(soup$X, soup$Y, type="n")
points(soup$X[soup$D == "Line1"], soup$Y[soup$D == "Line1"], pch=17, col="BLUE")
points(soup$X[soup$D == "Line0"], soup$Y[soup$D == "Line0"], pch=19, col="RED")
legend("bottomright", legend=levels(soup$D), pch=c(19,17), col=c("RED", "BLUE"))

soup.lm = lm(Y ~ X+D, data=soup)
summary(soup.lm)

abline(27.28179, 1.23074, lty=2, col="RED")
abline(28.28179+53.1292, 1.23074, lty=2, col="BLUE")

soup2.lm = lm(Y ~ X+X:D, data=soup)
soup2.lm = lm(Y ~ X*D, data=soup)

soup2.lm = lm(Y ~ X+D+X:D, data=soup)
summary(soup2.lm)
