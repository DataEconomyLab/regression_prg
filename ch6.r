goose=read.table("https://raw.githubusercontent.com/DataEconomyLab/regression/main/reg2020/goose.txt", header=T)
head(goose, 3)

goose.lm=lm(photo~obsA, data=goose)
plot(goose.lm$fitted, goose.lm$resid, pch=19)

# install.packages("car")
library(car)
ncvTest(goose.lm)

tree=read.table("https://raw.githubusercontent.com/DataEconomyLab/regression/main/reg2020/tree.txt", header=T)
head(tree, 3)

tree.lm=lm(V~D+H, data=tree)
plot(tree$D, tree.lm$resid, pch=19)
plot(tree$H, tree.lm$resid, pch=19)

library(car)
goose.lm=lm(photo~obsA, data=goose)
qqPlot(goose.lm)

options("install.lock"=FALSE)
install.packages("mvnormtest")
library(mvnormtest)
goose.rstudent=rstudent(goose.lm)
shapiro.test(goose.rstudent)

energy=read.table("https://raw.githubusercontent.com/DataEconomyLab/regression/main/reg2020/energy.txt", header=T)
head(energy, 3)

energy.lm=lm(Y~X, data=energy)
plot(energy.lm$fitted, energy.lm$resid, pch=19)

library("MASS")
boxcox(Y~X, data=energy, lambda=seq(-2,2,1/2), plotit=TRUE)
