hospital = read.table("https://raw.githubusercontent.com/DataEconomyLab/regression/main/reg2020/hospital.txt", header=T)
head(hospital)

hospital.lm = lm(Y ~ X1 + X2 + X3 + X4 + X5, data=hospital)
summary(hospital.lm)

anova(hospital.lm)

options("install.lock"=FALSE)
install.packages("fmsb")
library(fmsb)
VIF(lm(X1~X2+X3+X4+X5, data=hospital))
VIF(lm(X2~X1+X3+X4+X5, data=hospital))
VIF(lm(X3~X1+X2+X4+X5, data=hospital))
VIF(lm(X4~X1+X2+X3+X5, data=hospital))
VIF(lm(X5~X1+X2+X3+X4, data=hospital))

cor(hospital[, -6])
summary(lm(Y~X2+X3+X4+X5, data=hospital))

VIF(lm(X2~X3+X4+X5, data=hospital))
VIF(lm(X3~X2+X4+X5, data=hospital))
VIF(lm(X4~X2+X3+X5, data=hospital))
VIF(lm(X5~X2+X3+X4, data=hospital))

hald = read.table("https://raw.githubusercontent.com/DataEconomyLab/regression/main/reg2020/hald.txt", header=T)
head(hald)

install.packages("leaps")
library(leaps)
all.lm = regsubsets(Y~., data=hald)
(rs=summary(all.lm))
names(rs)
rs$rsq
rs$adjr2
rs$cp

start.lm=lm(Y~1, data=hald)
full.lm=lm(Y~., data=hald)
step(start.lm, scope=list(lower=start.lm, upper=full.lm), direction="forward")

step(full.lm, data=hald, direction="backward")

step(start.lm, scope=list(upper=full.lm), data=hald, direction="both")

tcrime = read.table("https://raw.githubusercontent.com/DataEconomyLab/regression/main/reg2020/tcrime.txt", header=T)
head(tcrime)

attach(tcrime)
plot(motor, tcratio, pch=19)

tcrime.lm=lm(tcratio ~ motor + I(motor^2), data=tcrime)
summary(tcrime.lm)

maraton=read.table("https://raw.githubusercontent.com/DataEconomyLab/regression/main/reg2020/maraton.txt", header=T)
head(maraton, 2)

maraton.lm=lm(m1990~sect+I(sect^2)+I(sect^3), data=maraton)
summary(maraton.lm)

soup = read.table("https://raw.githubusercontent.com/DataEconomyLab/regression/main/reg2020/soup.txt", header=T)
soup[c(1, 15, 16, 27),]
soup$D=factor(soup$D, levels=c(0,1), label=c("Line0", "Line1"))
soup[c(1, 15, 16, 27),]

plot(soup$X, soup$Y, type="n")
points(soup$X[soup$D=="Line1"], soup$Y[soup$D=="Line1"], pch=17, col="BLUE")
points(soup$X[soup$D=="Line0"], soup$Y[soup$D=="Line0"], pch=19, col="RED")
legend("bottomright", legend=levels(soup$D), pch=c(19,17), col=c("RED", "BLUE"))

soup.lm=lm(Y~X+D, data=soup)
summary(soup.lm)

abline(27.28179, 1.23074, lty=2, col="RED")
abline(27.28179+53.1292, 1.23074, lty=2, col="BLUE")

