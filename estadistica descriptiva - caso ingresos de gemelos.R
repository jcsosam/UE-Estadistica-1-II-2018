##########################
### SOLUCION ENTREGA 1 ###
##########################

#-------------------------------------------------------------------------------

######
# 1. #
######

# importar datos
TWINS <- read.delim("C:/Users/Juan Camilo/Dropbox/UE/Estadistica 1  II-2018/data/TWINS.txt")

#-------------------------------------------------------------------------------

######
# 2. #
######

# dimension de la base de datos original
dim(TWINS)
# 183  16

# Hay 183 registros y 16 variables.

#-------------------------------------------------------------------------------

######
# 3. #
######

# remover registros con informacion faltante
TWINS <- TWINS[complete.cases(TWINS), ]

# dimension de la base de datos con registros completos
dim(TWINS)
# 147  16

# Hay 147 registros (completos) y 16 variables.

#-------------------------------------------------------------------------------

######
# 4. #
######

# Clasificacion de las variables.
# EDUCL   : cuantitativa discreta  de razon.
# HRWAGEL : cuantitativa contianua de razon.

#-------------------------------------------------------------------------------

attach(TWINS)

#-------------------------------------------------------------------------------

######
# 5. #
######

# WHITEH : 1 if twin 2 is white, 0 otherwise.
# MALEH  : 1 if twin 2 is male,  0 otherwise.

n <- nrow(TWINS)

##########
# tablas #
##########

tabla.WHITEH <- round(100 * table(WHITEH)/n, 1)
tabla.WHITEL <- round(100 * table(WHITEL)/n, 1)
tabla.MALEH  <- round(100 * table(MALEH) /n, 1)
tabla.MALEL  <- round(100 * table(MALEL) /n, 1)

names(tabla.WHITEL) <- c("Otro", "Blanco")
names(tabla.WHITEH) <- c("Otro", "Blanco")
names(tabla.MALEL)  <- c("Mujer", "Hombre")
names(tabla.MALEH)  <- c("Mujer", "Hombre")

######################
# graficos de barras #
######################

windows(width = 10, height = 10)
par(mfrow = c(2,2))
# 
x <- tabla.WHITEL
a <- barplot(height = x, density = 15, ylim = c(0, max(x) + 10), 
             xlab = "Raza", ylab = "Porcentaje (%)", main = "Gemelo 1")
text(x = a, y = x + 5, labels = paste0(x,"%"))
# 
x <- tabla.WHITEH
a <- barplot(height = x, density = 15, ylim = c(0, max(x) + 10), 
             xlab = "Raza", ylab = "Porcentaje (%)", main = "Gemelo 2")
text(x = a, y = x + 5, labels = paste0(x,"%"))
# 
x <- tabla.MALEL
a <- barplot(height = x, density = 15, ylim = c(0, max(x) + 10), 
             xlab = "Raza", ylab = "Porcentaje (%)", main = "Gemelo 1")
text(x = a, y = x + 5, labels = paste0(x,"%"))
# 
x <- tabla.MALEH
a <- barplot(height = x, density = 15, ylim = c(0, max(x) + 10), 
             xlab = "Raza", ylab = "Porcentaje (%)", main = "Gemelo 2")
text(x = a, y = x + 5, labels = paste0(x,"%"))

#-------------------------------------------------------------------------------

######
# 6. #
######

###################################
# graficos de educacion y salario #
###################################

windows(width = 10, height = 5)
par(mfrow = c(1,2))
# educacion
boxplot(EDUCL, EDUCH, horizontal = TRUE, boxwex = 0.3,
                col = c("lightblue","mistyrose"), border = c("blue","red"),
        names = c("Gemelo 1", "Gemelo 2"),
        xlab = "Años de educación", main = "Nivel educativo")
# salario por hora (ingreso)
boxplot(HRWAGEL, HRWAGEH, horizontal = TRUE, boxwex = 0.3,
        col = c("lightblue","mistyrose"), border = c("blue","red"),
        names = c("Gemelo 1", "Gemelo 2"),
        xlab = "Salario por hora (US $)", main = "Ingresos")

########################
# medidas estadisticas #
########################

# base de datos con edad, educacion y salario
TWINS2 <- data.frame(AGE, EDUCL, EDUCH, HRWAGEL, HRWAGEH)

# rango
TWINS2.R <- round( apply(X = TWINS2, MARGIN = 2, FUN = function(x) max(x) - min(x)), 1 ) 

# rango intercuartilico
TWINS2.RI <- round( apply(X = TWINS2, MARGIN = 2, FUN = function(x) quantile(x, probs = 0.75) - quantile(x, probs = 0.25)), 1 )

# desviacion estandar
TWINS2.sd <- round( apply(X = TWINS2, MARGIN = 2, FUN = function(x) sd(x)), 1 )

# coeficiente de variacion
TWINS2.cv <- round( apply(X = TWINS2, MARGIN = 2, FUN = function(x) 100 * sd(x)/mean(x)), 1 )

TWINS2.dispersion <- rbind( TWINS2.R, TWINS2.RI, TWINS2.sd, TWINS2.cv)
rownames(TWINS2.dispersion) <- c("Rango", "R. Intercuartilico", "Desv. Estandar", "Coef. Variacion")

# Medidas de tendencia central y localización
summary(TWINS2)

# Medidas de dispersion
TWINS2.dispersion

#-------------------------------------------------------------------------------

######
# 7. #
######

#################
# dispersograma #
#################

windows()
pairs(TWINS2, labels = c("Edad", "Educ. 1", "Educ. 2", "Salario 1", "salario 2"), 
      col = "blue", cex = 0.7)

#########################
# matriz de correlacion #
#########################

round( 100 * cor(TWINS2), 1 )

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------