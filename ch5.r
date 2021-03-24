forbes = read.csv("C:/Users/ljyjj/Documents/GitHub/regression/reg2020/forbes.txt", header=T)
forbes$Lpress = 100*log10(forbes$press)
head(forbes)

plot(forbes$temp, forbes$press, pch=19)
identify(forbes$temp, forbes$Lpress)

forbes.lm = lm(Lpress ~ temp, data=forbes)
summary(forbes.lm)

anova(forbes.lm)
forbes.res =ls.diag(forbes.lm)
names(forbes.res)

resid.result = cbind(forbes.res$std.res, forbes.res$stud.res, forbes.res$hat)
colnames(resid.result) = c("standardize resid", "studentized resid", "Hat")
resid.result = round(resid.result, 3)
print(resid.result)

2*17*(1-pt(12.374, 14))
qt(0.01/(2*17), 14)

rstudent(forbes.lm)

library(cat)
outlierTest(forbes.lm)

prater = read.table("C:/Users/ljyjj/Documents/GitHub/regression/reg2020/prater.txt", header=T)
head(prater,2)

prater.lm = lm(Y ~ X1+X2+X3+X4, data=prater)
summary(prater.lm)

start.lm = lm(Y ~ 1, data=prater)
full.lm = lm(Y ~ X1+X2+X3+X4, data=prater)
step(start.lm, scope=list(upper=full.lm), data=prater, direction="both")

soil = read.table("C:/Users/ljyjj/Documents/GitHub/regression/reg2020/soil.txt", header=T)
head(soil, 3)

soil.lm = lm(SL ~ SG+LOBS + PGC, data=soil)
summary(soil.lm)

anova(soil.lm)

plot(soil.lm$fitted, soil.lm$resid, pch=19)
identify(soil.lm$fitted, soil.lm$resid)

soil.diag=ls.diag(soil.lm)
names(soil.diag)

diag.st = cbind(soil.diag$hat, soil.diag$std.res, soil.diag$stud.res, soil.diag$cooks)
colnames(diag.st) = c("Hiii", "ri", "ti", "Di")
round(diag.st, 3)

Di = cooks.distance(soil.lm)
round(Di, 3)

library(car)
outlierTest(soil.lm)