hospital = read.table("https://raw.githubusercontent.com/DataEconomyLab/regression/main/reg2020/hospital.txt", header=T)
head(hospital)

hospital.lm = lm(Y ~ X1 + X2 + X3 + X4 + X5, data=hospital)
summary(hospital.lm)

anova(hospital.lm)

options("install.lock"=FALSE)
# install.packages("fmsb")
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

# install.packages("leaps")
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