## ---- message=FALSE------------------------------------------------------
library(MASS)
library(hnp)

## ------------------------------------------------------------------------
dados <- read.table("dados/crabs.txt", header = TRUE)
str(dados)

## ------------------------------------------------------------------------
plot(dados)

## ---- fig.show='hold'----------------------------------------------------
par(mfrow = c(1, 2))
hist(dados$satell, xlab = "Número de satélites",
     ylab = "Frequência", main = "")
boxplot(dados$satell)
par(mfrow = c(1, 1))

## ------------------------------------------------------------------------
plot(satell ~ width, data = dados,
     xlab = "Comprimento (cm)", ylab = "Número de satélites")

## ------------------------------------------------------------------------
## Quebra de classes (reproduz as classes do livro)
quebra <- c(20.25, seq(23.25, 29.25, 1), 34.25)
## Identifica classes
classes <- cut(dados$width, breaks = quebra)
## Agrega os valores em uma tabela de frequência
fn <- function(x) cbind(length(x), sum(x), mean(x), var(x))
tab <- aggregate(satell ~ classes, data = dados, FUN = fn)
tab <- data.frame("classes" = tab[,1], tab[,2])
names(tab)[2:5] <- c("n", "nsatell", "med", "vari")
tab

## ------------------------------------------------------------------------
plot(tab$med, xlab = "Classe de comprimento",
     ylab = "Número médio de satélites")

## ------------------------------------------------------------------------
plot(vari ~ med, data = tab, xlim = c(0, 17), ylim = c(0, 17),
     xlab = "Média", ylab = "Variância")
abline(a = 0, b = 1)

## ------------------------------------------------------------------------
m.norm <- glm(satell ~ width, data = dados,
              family = gaussian(link = "identity"))

## ------------------------------------------------------------------------
anova(m.norm, test = "Chisq")

## ------------------------------------------------------------------------
summary(m.norm)

## ---- echo=FALSE---------------------------------------------------------
al.norm <- unname(round(coef(m.norm)[1], 3))
be.norm <- unname(round(coef(m.norm)[2], 3))

## ---- echo=FALSE---------------------------------------------------------
mw <- mean(dados$width)

## ---- fig.show='hold'----------------------------------------------------
par(mfrow = c(2, 2))
plot(m.norm)
par(mfrow = c(1, 1))

## ------------------------------------------------------------------------
hnp(m.norm)

## ------------------------------------------------------------------------
pred.norm <- predict(m.norm)
tab.norm <- aggregate(pred.norm ~ classes, FUN = mean)
plot(tab$med, xlab = "Classe de comprimento",
     ylab = "Número médio de satélites", pch = 19)
lines(tab.norm$pred.norm, col = 2)

## ----pois----------------------------------------------------------------
m.pois <- glm(satell ~ width, data = dados,
              family = poisson(link = "log"))

## ------------------------------------------------------------------------
anova(m.pois, test = "Chisq")

## ------------------------------------------------------------------------
summary(m.pois)

## ---- echo=FALSE---------------------------------------------------------
al.pois <- unname(round(coef(m.pois)[1], 3))
be.pois <- unname(round(coef(m.pois)[2], 3))

## ---- fig.show='hold'----------------------------------------------------
par(mfrow = c(2, 2))
plot(m.pois)
par(mfrow = c(1, 1))

## ------------------------------------------------------------------------
hnp(m.pois)

## ------------------------------------------------------------------------
pred.pois <- predict(m.pois, type = "response")
tab.pois <- aggregate(pred.pois ~ classes, FUN = mean)
plot(tab$med, xlab = "Classe de comprimento",
     ylab = "Número médio de satélites", pch = 19)
lines(tab.norm$pred.norm, col = 2)
lines(tab.pois$pred.pois, col = 3)

## ----pois2---------------------------------------------------------------
m.pois2 <- glm(satell ~ width, data = dados,
               family = poisson(link = "identity"),
               start = coef(m.pois))

## ------------------------------------------------------------------------
anova(m.pois2, test = "Chisq")

## ------------------------------------------------------------------------
summary(m.pois2)

## ---- echo=FALSE---------------------------------------------------------
al.pois2 <- unname(round(coef(m.pois2)[1], 3))
be.pois2 <- unname(round(coef(m.pois2)[2], 3))

## ---- fig.show='hold'----------------------------------------------------
par(mfrow = c(2, 2))
plot(m.pois2)
par(mfrow = c(1, 1))

## ---- error=TRUE---------------------------------------------------------
hnp(m.pois2)

## ------------------------------------------------------------------------
pred.pois2 <- predict(m.pois2, type = "response")
tab.pois2 <- aggregate(pred.pois2 ~ classes, FUN = mean)
plot(tab$med, xlab = "Classe de comprimento",
     ylab = "Número médio de satélites", pch = 19)
lines(tab.norm$pred.norm, col = 2)
lines(tab.pois$pred.pois, col = 3)
lines(tab.pois2$pred.pois, col = 4)

## ------------------------------------------------------------------------
m.bn <- glm.nb(satell ~ width, data = dados,
               link = "identity", start = coef(m.pois))

## ------------------------------------------------------------------------
anova(m.bn, test = "Chisq")

## ------------------------------------------------------------------------
summary(m.bn)

## ------------------------------------------------------------------------
anova(m.norm, m.pois, m.pois2, m.bn, test = "F")

