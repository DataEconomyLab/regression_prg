# 자료읽기
glider <- read.csv("https://raw.githubusercontent.com/DataEconomyLab/regression/main/reg2020/chap7/sugar_glider_binomial.csv")
head(glider, 3)

# 모형적합
logit_m1 <- glm(occurr~p_size_km+con_metric, family=binomial(link=logit), data=glider)
# 적합 결과 요약
summary(logit_m1)

1-pchisq(68.994-54.661,2)
logit_m0 <- glm(occurr~1, family=binomial(link=logit), data=glider)
anova(logit_m0, logit_m1, test="Chisq")

1-pchisq(54.661, 47)

logit_m2 <- glm(occurr~p_size_km, family=binomial(link=logit), data=glider)
summary(logit_m2)

anova(logit_m2, logit_m1, test="Chisq")

AIC(logit_m2, logit_m1)

library(MASS)
stepAIC(logit_m1, direction="both")

p_size <- seq(20,230,1)
hat_eta <- predict(logit_m2, list(p_size_km=p_size), type="link")
par(mfrow=c(1,2))
plot(glider$p_size_km, glider$occurr, xlab="구획의 크기(x)", ylab="hat pi(x) occurr", sub="(a)", pch=20)
lines(p_size, exp(hat_eta)/(1+exp(hat_eta)), lwd=1.5, col="red")

glider_g <- read.csv("https://raw.githubusercontent.com/DataEconomyLab/regression/main/reg2020/chap7/sugar_glider_binomial_g.csv")
plot(glider_g$p_size_med, glider_g$cases/glider_g$count, xlab="구획의 크기(x)", ylim=c(0,1), ylab="hat pi(x) sampleprop.", sub="(b)", pch=20, col="blue")
lines(p_size, exp(hat_eta)/(1+exp(hat_eta)), lwd=1.5, col="red")

glider_g <- read.csv("https://raw.githubusercontent.com/DataEconomyLab/regression/main/reg2020/chap7/sugar_glider_binomial_g.csv")
head(glider_g,3)

y <- cbind(glider_g$cases, glider_g$count-glider_g$cases)
logit_mg <- glm(y~glider_g$p_size_med, family=binomial(link=logit))
summary(logit_mg)

logit_m2 <- glm(occurr~p_size_km, family=binomial(link=logit), data=glider)
exp(coef(logit_m2))

exp(confint(logit_m2, perm="p_size_km", level=0.95))

x <- 150
predict(logit_m2, list(p_size_km=x), type="response")

vcov(logit_m2)
coef(logit_m2)

sqrt(vcov(logit_m2)[1, 1]+x^2*vcov(logit_m2)[2,2]+2*x*vcov(logit_m2)[2,1])

data(esoph)
head(esoph, 3)

attach(esoph)

y <- cbind(ncases, ncontrols)
levels(alcgp)

n.alcgp <- factor(alcgp, ordered=FALSE)
levels(n.alcgp) <- c("0-39", "40-79", "80-119", "120+")
levels(tobgp)

n.tobgp <- factor(tobgp, ordered=FALSE)
levels(n.tobgp) <- c("0-9", "10-19", "20-29", "30+")
levels(agegp)

n.agegp <- factor(agegp, ordered=FALSE)
levels(n.agegp) <- c("25-34", "35-44", "45-54", "55-64", "65-74", "75+")

logit_tob <- glm(y~n.tobgp, family=binomial(link=logit))
summary(logit_tob)

n3.tobgp <- n.tobgp
levels(n3.tobgp)[2:3] <- "10-29"
levels(n3.tobgp)

logit_tob3 <- glm(y~n3.tobgp, family=binomial(link=logit))
summary(logit_tob3)
