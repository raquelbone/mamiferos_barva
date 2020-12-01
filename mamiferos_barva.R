####PROYECTO FINAL CURSO CIENCIA REPRODUCIBLE######

## ANALISIS RIQUEZA MAMIFEROS TERRESTRES EN EL GRADIENTE DEL VOLCAN BARVA

# Establecemos directorio de trabajo

setwd("C:/Users/raque/Desktop/Ciencia_rep/mamiferos_barva")

# Diagrama causal de hipótesis

require(ggdag)

dagified <- dagify(D ~ H,
                   D ~ L,
                   H ~ L,
                   exposure = "H",
                   outcome = "D")

ggdag(dagified, layout = "circle") + theme_dag()