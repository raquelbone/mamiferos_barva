####PROYECTO FINAL CURSO CIENCIA REPRODUCIBLE######

### ANALISIS RIQUEZA MAMIFEROS TERRESTRES EN EL GRADIENTE DEL VOLCAN BARVA ###

# Establecemos directorio de trabajo

setwd("C:/Users/raque/Desktop/Ciencia_rep/mamiferos_barva")

# Diagrama causal de hipótesis con variables de interés y sus relaciones

library(ggdag)

dagified <- dagify(R ~ M, R ~ L, R ~ TE, R ~ A, M ~ TE, A ~ TE, A ~ L, M ~ L, M ~ A,
                   exposure = "M",
                   outcome = "R")

ggdag(dagified, layout = "circle") + theme_dag()

## PREPARACION DE BASE DE DATOS

# set de datos
dat <- read.csv("barva.csv", header = T, sep = ',')
head(dat)

# primero hacemos un subset con las variables de interés
barva <- dat[c(8, 15:18, 29, 30)]
head(barva)

# cambiamos los nombres de las variables
names(barva) <- c("fecha","familia", "genero", "especie", "abund", "luna", "temp")
head(barva)
str(barva)

# cambiamos formato de variables
barva$fecha <- as.factor(barva$fecha)
barva$fecha <- as.POSIXlt(strptime(as.character(barva$fecha), "%d/%m/%Y"))
barva$fecha <- as.POSIXct(barva$fecha, origin = "1960/01/01", tz = "UTC")

# hacemos nueva columna para mes
barva$mes <- format(as.Date(barva$fecha), "%b")
barva$mes <- as.factor(barva$mes)
levels(barva$mes)

# ordenamos meses 
barva$mes <- factor(barva$mes,levels= c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug"))
levels(barva$mes)

# hacemos nueva columna para año
barva$año <- format(as.Date(barva$fecha), "%Y")
barva$año <- as.factor(barva$año)
levels(barva$año)

# hacemos nueva columna con codigo de especie
barva$spp <- paste(substr(barva$genero, 1, 1), substr(barva$especie, 1, 3), sep = "")
barva$spp <- as.factor(barva$spp)
levels(barva$spp)

# arreglamos diferencia de codigos en variable luna
barva$luna <- as.factor(barva$luna)
levels(barva$luna)
levels(barva$luna) <- c("New", "NCres", "FQua", "WaxG", "Full", "WanG", "LQua", "OCres", "FQua",
                        "Full", "LQua", "New", "NCres", "OCres", "WanG", "WaxG")
levels(barva$luna)

# quitamos valores extraños de temperatura que no estan en base de datos original
barva <- subset(barva, barva$temp>7)
barva <- subset(barva, barva$temp<33)

# revisamos cambios
head(barva)
str(barva)

# generamos una nueva base que contenga riqueza de especies
library(plyr)
barva2 <- ddply(barva, .(año, mes, luna, temp), summarize,
                riqsp = length(unique(spp)))
head(barva2)

# ordenamos la base por año y mes
barva2 <- barva2[order(barva2$año, barva2$mes), ]
head(barva2)

## EVALUACION DE SUPUESTOS PARA PRUEBAS Y MODELOS

# chequeamos distribucion de datos
hist(barva2$riqsp)
qqnorm(barva2$riqsp)

library(psych)
library(lme4)
library(sjPlot)
library(ggplot2)
pairs.panels(barva2, 
             method = "pearson",
             hist.col = "#00AFBB",
             density = T,
             ellipses = F)


## Modelos GLMM (con distribucion Poisson)

# Modelo nulo
m0 <- glmer(riqsp ~ 1 + (1 | año), data = barva2, family = poisson(link = "log"))
summary(m0)
plot(ranef(m0))

m1 <- glmer(riqsp ~ mes + luna + temp + (1 | año), data = barva2, family = poisson(link = "log"))
summary(m1)

p1 <- plot_model(m1, type = "re", title= "Efecto Aleatorio", colors = c("olivedrab2", "turquoise1"),
                   line.size = 1)+ xlab("Año") + theme_classic()
p1

AIC(m0, m1) #elegimos el modelo con menor valor de AIC ya que es el que mejor se ajusta a los datos

library(blmeco)
dispersion_glmer(m1) #no hay sobredispersion

## Modelos GAMM (con distribucion Poisson)

# Modelo nulo
library(gamm4)
mg0 <- gamm4(riqsp ~ 1, data = barva2, family = poisson(link = "log"),
              random = ~ (1 | año))

mg1 <- gamm4(riqsp ~ mes + luna + s(temp), data = barva2, family = poisson(link = "log"),
              random = ~ (1 | año))
summary(mg1$gam)

AIC(m1, mg0$mer, mg1$mer)

# Nos quedamos con el GAMM mg1

## Ahora buscamos el mejor modelo

mg2 <- gamm4(riqsp ~ mes + s(temp), data = barva2, family = poisson(link = "log"),
              random = ~ (1 | año))
mg3 <- gamm4(riqsp ~ luna + s(temp), data = barva2, family = poisson(link = "log"),
              random = ~ (1 | año))
mg4 <- gamm4(riqsp ~ s(temp), data = barva2, family = poisson(link = "log"),
              random = ~ (1 | año))

AIC(mg1$mer, mg2$mer, mg3$mer, mg4$mer)

# Elegimos el modelo 2 con mes y temp
summary(mg2$gam)

## PREDICCIONES DEL MODELO mg2 CON MEJOR AJUSTE

# para la riqueza de especies de mamíferos en el Barva

pm1 <- plot_model(mg2$gam, type = "pred", terms = c("mes", "temp"), title= "", 
                   colors = c("turquoise1", "olivedrab2", "red3"), line.size = 1)+ 
  xlab("Mes") + ylab("Pred. Riqueza de especies")+ theme_classic()
pm1

pm2 <- plot_model(mg2$gam, type = "pred", terms = c("mes"), title= "", 
                   colors = c("turquoise1"), line.size = 1)+ 
  xlab("Mes") + ylab("Pred. Riqueza de especies")+ theme_classic()
pm2

pm3 <- plot_model(mg2$gam, type = "pred", terms = c("temp"), title= "", 
                   colors = c("turquoise1"), line.size = 1)+ 
  xlab("Temperatura") + ylab("Pred. Riqueza de especies")+ theme_classic()
pm3
