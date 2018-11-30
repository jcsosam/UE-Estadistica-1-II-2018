###################################
#Ejemplo 1: FUNCION DE MASA (VAD) #
###################################

#VARIABLE ALEATORIA DISCRETA
rango = c(0, 1, 2, 3)
fx    = c(1/8, 3/8, 3/8, 1/8)
k     = length(rango)
#k = cantidad de ELEMENTOS en el rango
#k = 4

#grafico VAD
windows()
plot(rango, fx, type = "h", col = "red", lwd = 2, 
     main = "Funcion de masa", xlab = "x", ylab = "f (x)")
points(rango, fx, col = "red", pch = 16)


#####################################################
# Ejemplo 2: FUNCION DE DISTRIBUCION ACUMULDA (VAD) #
#####################################################

#FDA
#cumsum es la suma acumulada de f
Fx <- cumsum(fx)

#grafico
windows()
plot(c(0, rango), c(0, Fx), type = "s", ylab = "F (x)", col = "red", xlab = "x",
     main = "Funcion de distribucion acumulada", lwd = 2)
points(rango, Fx, col = "red", pch = 16)
grid()

#tabla
tabla <- data.frame(rango, fx, Fx)
colnames(tabla) <- c("x","f(x)","F(x)")
print(tabla)

#FDA (Ejemplo b)
Fx = c(0.25, 0.55, 0.9, 1)

#k es la cantidad de ELEMENTOS en el rango
k <- length(Fx)

#"derivada" discreta es una DIFERENCIA
fx = Fx - c(0, Fx[-k])

sum(fx)

######################################
#Ejemplo 3 FUNCION DE DENSIDAD (VAC) #
######################################

fun <- function(x) x^2

#a. calcular el valor de k
integral <- integrate(f = fun, lower = -1, upper = 2)

integral$value

k <- 1/integral$value

#el valor de k es 1/3 = 0.33333

#usar tambien Wolfram

fx <- function(x) (1/3) * x^2

#grafico
windows()
curve(expr = fx, from = -1, to = 2, lwd = 2, col = "blue",
      main = "Funcion de densidad", ylab = "f (x)", xlab = "x")
grid()

#b. calcular la probabilidad de que la empresa tenga perdidas
integrate(f = fx, lower = -1, upper = 0)

#INTERPRETACION: la probabilidad de que la empresa presente
#perdidas es del 11.11%

# probabilidad de obtener utilidades entre 0.5 y 1.5
integrate(f = fx, lower = 0.5, upper = 1.5)

# area bajo toda la curva
integrate(f = fx, lower = -1, upper = 2)

##############################################
#Ejemplo 4 Funcion de distribucion acumulada #
##############################################

#funcion de densidad
fx <- function(x) (1/3) * x^2

#probabilidad de obtener ganancias
integrate(f = fx, lower = 1, upper = 2)


Fx <- function (x) (x^3 + 1)/9

#grafico
windows()
curve(expr = Fx, from = -1, to = 2, lwd = 2, col = "blue",
      main = "Funcion de distribucion", ylab = "F (x)", xlab = "x")
grid()