##======================================================================
## The Ecological Detective
## Chapter 4: Incidental catch in fisheries: seabirds in the New Zealand
## squid trawl fisheries
##======================================================================

##----------------------------------------------------------------------
## Dados
dados <- data.frame(n.aves = 0:17,
                    n.arr = c(807, 37, 27, 8, 4, 4, 1, 3, 1, 0, 0, 2, 1,
                              1, 0, 0, 0, 1))
## Frequencia relativa
freq.obs <- dados$n.arr/sum(dados$n.arr)
str(dados)

##----------------------------------------------------------------------
## Análise exploratória

## Barplot da captura de aves
barplot(freq.obs, names.arg = dados$n.aves, ylim = c(0, 1),
        xlab = "Número de aves capturadas",
        ylab = "Frequência relativa")

## Média das capturas
med <- sum(dados$n.aves * freq.obs)

## Variância
vari <- sum((dados$n.aves - med)^2)/(sum(dados$n.arr) - 1)

## Var/Med
vari/med

## k (parâmetro de superdispersão): calculado pelo método dos momentos
## (sera usado adiante)
k <- med^2/(vari - med)


##======================================================================
## Ajustando modelos
##======================================================================

##======================================================================
## Normal

## Frequência esperada, de acordo com um modelo Normal
freq.esp.norm <- dnorm(x = dados$n.aves, mean = med, sd = sqrt(vari))

## Barplot
barplot(t(cbind(freq.obs, freq.esp.norm)),
        names.arg = dados$n.aves,
        ylim=c(0, 1), xlab = "Número de aves capturadas",
        ylab = "Frequência relativa", beside = TRUE,
        legend.text = c("Observado", "Esperado"),
        main = "Normal")

## Gráfico dos resíduos
plot(freq.esp.norm - freq.obs,
     xlim = c(1, 18), ylim = c(-0.6, 0.6),
     xlab = "Número de aves capturadas", ylab = "Resíduos",
     axes = FALSE, main = "Resíduos Normal", pch = 19)
abline(h = 0)
axis(1, 1:18, 0:17)
axis(2)
box()

## Cálculo da estatistica qui-quadrado para testar se o modelo se ajusta
## bem aos dados
qui.norm <- with(dados,
                 sum((n.arr - freq.esp.norm*sum(n.arr))^2/(freq.esp.norm
                     * sum(n.arr))))
pchisq(q = qui.norm, df = nrow(dados) - 1, lower.tail = FALSE)

##======================================================================
## Poisson

## Frequência esperada, de acordo com um modelo Poisson
freq.esp.pois <- dpois(x = dados$n.aves, lambda = med)

## Barplot
barplot(t(cbind(freq.obs, freq.esp.pois)),
        names.arg = dados$n.aves,
        ylim=c(0, 1), xlab = "Número de aves capturadas",
        ylab = "Frequência relativa", beside = TRUE,
        legend.text = c("Observado", "Esperado"),
        main = "Poisson")

## Gráfico dos resíduos
plot(freq.esp.pois - freq.obs,
     xlim = c(1, 18), ylim = c(-0.17, 0.17),
     xlab = "Número de aves capturadas", ylab = "Resíduos",
     axes = FALSE, main = "Resíduos Poisson", pch = 19)
abline(h = 0)
axis(1, 1:18, 0:17)
axis(2)
box()

## Cálculo da estatistica qui-quadrado para testar se o modelo se ajusta
## bem aos dados
qui.pois <- with(dados,
                 sum((n.arr - freq.esp.pois*sum(n.arr))^2/(freq.esp.pois
                     * sum(n.arr))))
pchisq(q = qui.pois, df = nrow(dados) - 1, lower.tail = FALSE)
## O que eh igual a:
chisq.test(x = dados$n.arr, p = freq.esp.pois)


##======================================================================
## Binomial negativo

## Frequência esperada pelo modelo binomial negativo
## Nessa funcao, size eh o parametro de superdispersão e mu é a média
## (isto é uma reparametrização pois a função usa normalmente prob, que
## é a probabilidade de sucesso em cada experimento). Ver os detalhes em
## ?dbinom.
freq.esp.bn <- dnbinom(x = dados$n.aves, size = k, mu = med)

barplot(t(cbind(freq.obs, freq.esp.bn)), names.arg = 0:17,
        ylim = c(0, 1), xlab = "Número de aves capturadas",
        ylab = "Frequência relativa", beside = TRUE,
        legend.text = c("Observado", "Esperado"),
        main = "Binomial negativo")

## Gráfico dos resíduos
plot(freq.esp.bn - freq.obs, xlim = c(1, 18), ylim = c(-0.015, 0.015),
     xlab = "Número de aves capturadas", ylab = "Resíduos",
     axes = FALSE, main = "Resíduos binomial negativo", pch = 19)
abline(h=0)
axis(1, 1:18, 0:17)
axis(2)
box()

## Cálculo da estatistica qui-quadrado para testar se o modelo se ajusta
## bem aos dados
qui.bn <- with(dados,
               sum((n.arr - freq.esp.bn*sum(n.arr))^2/(freq.esp.bn
                   * sum(n.arr))))
pchisq(q = qui.bn, df = nrow(dados) - 1, lower.tail = FALSE)

##======================================================================
## Juntando os gráficos

##----------------------------------------------------------------------
## Barplots e resíduos
par(mfrow = c(3, 2))
## Normal
barplot(t(cbind(freq.obs, freq.esp.norm)),
        names.arg = dados$n.aves,
        ylim=c(0, 1), xlab = "Número de aves capturadas",
        ylab = "Frequência relativa", beside = TRUE,
        legend.text = c("Observado", "Esperado"),
        main = "Normal")
box()
plot(freq.esp.norm - freq.obs,
     xlim = c(1, 18), ylim = c(-0.6, 0.6),
     xlab = "Número de aves capturadas", ylab = "Resíduos",
     axes = FALSE, main = "Resíduos Normal", pch = 19)
abline(h = 0)
axis(1, 1:18, 0:17)
axis(2)
box()
## Poisson
barplot(t(cbind(freq.obs, freq.esp.pois)),
        names.arg = dados$n.aves,
        ylim=c(0, 1), xlab = "Número de aves capturadas",
        ylab = "Frequência relativa", beside = TRUE,
        legend.text = c("Observado", "Esperado"),
        main = "Poisson")
box()
plot(freq.esp.pois - freq.obs,
     xlim = c(1, 18), ylim = c(-0.6, 0.6),
     xlab = "Número de aves capturadas", ylab = "Resíduos",
     axes = FALSE, main = "Resíduos Poisson", pch = 19)
abline(h = 0)
axis(1, 1:18, 0:17)
axis(2)
box()
## Binomial negativo
barplot(t(cbind(freq.obs, freq.esp.bn)), names.arg = 0:17,
        ylim = c(0, 1), xlab = "Número de aves capturadas",
        ylab = "Frequência relativa", beside = TRUE,
        legend.text = c("Observado", "Esperado"),
        main = "Binomial negativo")
box()
plot(freq.esp.bn - freq.obs, xlim = c(1, 18), ylim = c(-0.6, 0.6),
     xlab = "Número de aves capturadas", ylab = "Resíduos",
     axes = FALSE, main = "Resíduos binomial negativo", pch = 19)
abline(h=0)
axis(1, 1:18, 0:17)
axis(2)
box()
par(mfrow = c(1, 1))

##----------------------------------------------------------------------
## Barplot conjunto
barplot(t(cbind(freq.obs, freq.esp.norm, freq.esp.pois, freq.esp.bn)),
        names.arg = dados$n.aves,
        ylim=c(0, 1), xlab = "Número de aves capturadas",
        ylab = "Frequência relativa", beside = TRUE,
        legend.text = c("Observado", "Normal", "Poisson",
                        "Binomial negativo"), main = "")
box()
