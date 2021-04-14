2*(1-pt(2.131, 15))

forbes=read.table("https://raw.githubusercontent.com/DataEconomyLab/regression/main/reg2020/forbes.txt", header=T)
forbes$Lpress=100*log10(forbes$press)
head(forbes)

plot(forbes$temp, forbes$Lpress, pch=19)

# 아래 돌리면 R스튜디오가 멈춰버림.
# identify(forbes$temp, forbes$Lpress)

forbes.lm=lm(forbes$Lpress~forbes$temp, data=forbes)
summary(forbes.lm)

anova(forbes.lm)

forbes.res=ls.diag(forbes.lm)
names(forbes.res)

resid.result=cbind(forbes.res$std.res, forbes.res$stud.res, forbes.res$hat)
colnames(resid.result) = c("standardized resid", "studentized resid", "Hat")
resid.result=round(resid.result, 3)
print(resid.result)

rstudent(forbes.lm)

library(car)
outlierTest(forbes.lm)

prater=read.table("https://raw.githubusercontent.com/DataEconomyLab/regression/main/reg2020/prater.txt", header=T)
head(prater, 2)

prater.lm=lm(Y~X1+X2+X3+X4, data=prater)
summary(prater.lm)

start.lm=lm(Y~1, data=prater)
full.lm=lm(Y~X1+X2+X3+X4, data=prater)
step(start.lm, scope=list(upper=full.lm), data=prater, direction="both")

soil=read.table("https://raw.githubusercontent.com/DataEconomyLab/regression/main/reg2020/soil.txt", header=T)
head(soil, 3)

soil.lm=lm(SL~SG+LOBS+PGC, data=soil)
summary(soil.lm)

anova(soil.lm)

plot(soil.lm$fitted, soil.lm$resid, pch=19)
# identify(soil.lm$fitted, soil.lm$resid)

soil.diag=ls.diag(soil.lm)
names(soil.diag)

diag.st=cbind(soil.diag$hat, soil.diag$std.res, soil.diag$stud.res, soil.diag$cooks)
colnames(diag.st)=c("Hii", "ri", "ti", "Di")

round(diag.st, 3)

Di=cooks.distance(soil.lm)
round(Di, 3)

library(car)
outlierTest(soil.lm)
