#######################################
# Analisis de Componentes Principales #
#######################################

#-------------------------------------------------------------------------------
# Descripcion de la base de datos:

# Investment recomendation data (S&P Dow Jones, 2011).
# Porcentaje de inversion recomendada en:
# - USst : stocks en US
# - Fst  : stocks en paises desarrollados fuera de US
# - Dst  : stocks en paises en desarrollo
# - USb  : bonds en US
# - Fb   : bonds en paises desarrollados fuera de US
# - Db   : bonds en paises en desarrollo
# - Alt  : inversiones alternativas
# - Cash : inversiones en efectivo
#
# Conjunto de porcentajes recomendados en cada uno de ocho de diferentes tipos 
# de inversión (Stocks [acciones]/Bonds [bonos]/Alternatives [Alternativas])
# por parte de empresas importantes de gestión financiera a principios de 2011.
#
# Cuando una empresa emite Stocks, está vendiendo una parte de sí misma a 
# cambio de efectivo. Cuando una entidad emite un bono, está adquiriendo una 
# deuda con el acuerdo de pagar intereses por el uso del dinero.
#
# Las inversiones alternativas incluyen arrendamientos, asociaciones de petróleo 
# y gas, propiedad inmobiliaria, metales preciosos e inversiones similares.
#
# El efectivo incluye inversiones a corto plazo, como el mercado monetario,
# depósitos bancarios y certificados de depósito.
#
# Decidir sobre la combinación adecuada de acciones y bonos en su cartera es una 
# función de su horizonte temporal, tolerancia al riesgo y objetivos de inversión.
#
# Para mass información ver Zelterman, p. 11.
# Zelterman, D. (2015). Applied Multivariate Statistics with R. Springer.
#-------------------------------------------------------------------------------

# importar data
invest <- read.delim("C:/Users/Juan Camilo/Dropbox/UE/Estadistica 1  II-2018/data/investment.txt", row.names=1)
#View(invest)

n <- nrow(invest)  # numero de individuos, 27

p <- ncol(invest)  # numero de variables, 8

# Las ocho variables de estudio son variables cuantitativas, continuas, de razon.

#############################
### Analisis exploratorio ###
#############################

# medidas de localizacion
summary( invest )

# estadisticas univariadas
round( colMeans(invest), 2)

# coeficientes de variacion
# MARGIN = 1 (filas)
# MARGIN = 2 (columnas)
round( apply(X = invest, MARGIN = 2, FUN = function(x) 100 * sd(x)/mean(x)), 2)

# diagramas de caja
windows(width = 10, height = 10)
boxplot(invest, horizontal = TRUE, boxwex = 0.5, 
        xlab = "Porcentajes de inverción recomendados")

# matriz de correlacion
round(100 * cor(invest), 1)

# matrix de covarianza
round( cov(invest), 1)

# dispersogramas
windows()
pairs(x = invest, lwd = 1, pch = 16, gap = 0, xaxt = "n", yaxt = "n", col = "blue")

###########
### ACP ###
###########

# calculo de componentes principales
# - Utilizar la matriz de covarianza cuando no hay problemas de conmensurabilidad,
#   es decir, cuando todas las viables tienen la misma escala.
# - Conmensurabilidad quiere decir que todas las variables tienen apro la misma 
#   escala.
# - Cuando las variables tengan diferentes escalas se recomienda utilizar la matriz.
# de correlacion
pc <- princomp(x = invest, cor = FALSE, scores = TRUE)
# cor = FALSE (matriz de covarianza)
# cor = TRUE  (matriz de correlacion)

# resumen (vizualizar los elementos de pc)
summary(pc)
# Y1 retiene el 64.7% de la info aprox.
# Y2 retiene el 16.1% de la info aprox.
# Las dos primeas componentes se retienen el 80.8% de la información.

#-------------------------------------------------------------------------------

# coeficientes (cargas o pesos)
pc$loadings

#-------------------------------------------------------------------------------

# scree plot
windows()
screeplot(pc, col = "blue", pch = 16, type = "lines", cex = 2, lwd = 2, 
          cex.axis = 0.8, cex.lab = 0.8, main = " ")

#-------------------------------------------------------------------------------

# scores
# Valores de las componentes principales en cada una de las compañias
head( pc$scores )

windows(width = 10, height = 5)
boxplot(pc$scores[ , 1], pc$scores[ , 2], horizontal = T, boxwex = 0.5, 
        names = c("Comp. 1", "Comp. 2"), xlab = "Puntaje")

#-------------------------------------------------------------------------------

# biplot
# plano factorial
windows()
biplot(pc, col = c(2,3), cex = c(.75, 1.5),
       xlab = "Primera componente",
       ylab = "Segunda componente",
       main = "Primer plano factorial de asignaciones de inversión")
grid()

#-------------------------------------------------------------------------------

# Diferencia entre USst y Alt frente a la primera componente
windows()
plot(pc$scores[ , 1], invest[ , "USst"] - invest[ , "Alt"],
     pch = 16, col = "blue", cex = 1.1,
     ylab = "Diferencia entre USst y Alt",
     xlab = "Primera componente",
     main = "")
grid()

# correlacion
round(cor(pc$scores[ , 1], invest$USst - invest$Alt), 3)

#-------------------------------------------------------------------------------

# correlacion entre variables y componentes
cor.vars.comps <- round( cor( cbind(invest, pc$scores) ), 2 )

cor.vars.comps[1:8, 9:10]

#-------------------------------------------------------------------------------
# INTERPRETACION:
# - La primera componente representa la diferencia entre recomendaciones de 
#   inversion en stocks de US y las alternativas. 
# - La segunda componente representa principalmente el reciproco de los bonos de EU. 
# - Las cargas para los otros tipos de inversiones (efectivo o acciones y bonos 
#   extranjeros) no juegan un papel determinante en el análisis.
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------