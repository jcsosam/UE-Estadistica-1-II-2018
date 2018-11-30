#############################################
# EJEMPLO 1: DISTRIBUCION UNIFORME DISCRETA #
#############################################

# a. la variable de interes es X = numero de articulos publicados
# Rango X = {1, 2, 3, 4, 5}

# b. la distribucion probabilistica de X es UNIFORME DISCRETA. 
# EQUIPROBABLE : seleccion con la misma probabilidad.

# c. Pr ( X >= 4 ) = ?
#    Pr ( X >= 4 ) = Pr( X = 4 ) + Pr ( X = 5 )
#                  = f(4) + f(5) 
#                  = 1 - ( f(1) + f(2) + f(3) )

n = 5

1/n + 1/n
#0.4
# Interpretacion: la probabilidad de que al seleccionar un profesor, 
# el número de artículos que haya publicado sea al menos de cuatro es 40%.

# d. Calcular la media y el coeficiente de variacion

x = c(1, 2, 3, 4, 5)

#valor esperado de X
sum(x)/n
#3
# Interpretación: se espera que el numero de articulos publicados sea 3.

#CV
sqrt( sum(x^2)/n - 3^2 ) / 3
#0.4714045
# Intepretación: la variabilidad del numero de articulos publicados es ALTA 
# respecto al valor esperado (3), dado que el CV = 47.1%.

####################################
# EJEMPLO 2: DISTRIBUCION BINOMIAL #
####################################

# a. X = numero de articulos SIN defectos en los veinte seleccionados.
# exito = articulo sin defectos.
# Rango X = {0, 1, 2, 3, 4, ... , 20} (21 valores)

n = 20   # la cantidad de veces que se repite el experimento.
p = 0.9  # la probabilidad de exito.

# b. la distribucion probabilisitica de X es BINOMIAL, porque el muestreo se
# hace CON REEMPLAZO (es decir las repeticiones son INDEPENDIENTES)

# c. Pr (Aceptar el envio) = Pr (X >= 18)
sum( dbinom(x = c(18, 19, 20), size = n, prob = p) )
#0.6769268

1 - sum( dbinom(x = 0:17, size = n, prob = p) )
#0.6769268

1 - pbinom(q = 17, size = n, prob = p)
#0.6769268

# otra forma
dbinom(x = c(0, 1, 2), size = 20, prob = 0.1)

sum(dbinom(x = c(0, 1, 2), size = 20, prob = 0.1))

pbinom(q = 2, size = 20, prob = 0.1)

# INTERPRETACION: la probabilidad de aceptar el envio es de 67.69%.

x = dbinom(x = 0:20, size = 20, prob = 0.1)

windows()
barplot(height = x, names.arg = 0:20, col = "lightblue")

# d. 

#valor esperado
n * p
#18

#coeficiente de variacion
sqrt(n * p * (1-p))/(n * p)
#0.0745356

# INTERPRETACION: la variabilidad de numero de articulos sin defectos es 
# moderada repecto al valor esperado (18), dado que el CV = 7.45%.

############################################
# EJEMPLO 3 : DISTRIBUCION HIPERGEOMETRICA #
############################################

# caracteristica de interes: fabricas sin cumplir los reglamentos
# a. X = numero de fabricas visitadas sin cumplir los reglamentos.
# rango X = {0, 1, 2, 3, 4, 5}

# b. La distribucion probabilisitca de X es hipergeometrica, porque el muestro
# de las fabricas se hace sin reemplazo.

N = 25
M = 10
n = 5

# c. Pr (X >= 1) = ?
sum( dhyper(x = 1:5, m = M, n = N - M, k = n) )
#0.9434783

1 - dhyper(x = 0, m = M, n = N - M, k = n) 
#0.9434783

# INTERPRETACION: la probabilidad de que al menos una de las 
# fabricas seleccionas este operando fuera del reglamento es
# 94.3%.

# d. 

# valor esperado
n * M / N
#2

# coeficiente de variacion
sqrt( n * ( M/N )*( (N-M)/N )*( (N-n)/(N-1) ) )/(n * M / N)
#0.5

# INTEPRETACION : la variabilidad del numero de fabricas seleccionadas 
# que no siguen el regrlamento es alta respecto al 
# valor esperado (2), ya que CV = 50%.

################################
# EJEMPLO DISTRIBUCION POISSON #
################################

# a. variable de estudio X = numero de clientes por minuto
#    intervalo de tiempo que se esta considerando es de 1 minuto
#    Rango X = {0, 1, 2, 3, ... }

# b. La distribucion de X es Poisson con parametro lamnda = 1 por minuto

# c. Pr ( X = 0 ) = ? (en cinco minutos -> lambda = 5)
dpois(x = 0, lambda = 5)
#0.006737947
# intepretetacion:
# La probabilidad de que no llegue ningun cliente en un espacio de cinco
# minutos es de 0.673%.

# c. Pr ( X >= 1) = ? (en un minuto -> lambda = 1)
1 - dpois(x = 0, lambda = 1)
#0.6321206
# interpretacion:
# la probabilidad de que llegue al menos un cliente en un espacio de un
# minuto es de 63.2%.

# c. Pr ( X <= 2 ) = ? (en un minuto -> lambda = 1)
ppois(q = 2, lambda = 1)
#0.9196986

sum( dpois(x = 0:2, lambda = 1) )
#0.9196986

# interpretaicion:
# la probabilidad de que lleguen no mas de dos clientes en un espacio de
# un minuto es 91.96%. 

# d. media y CV

# valor esperado = 1 (por minuto)

# CV
sqrt(1)/1
#1 

# intepretatacion: la dispersion del numero de clientes con respecto al
# numero de clientes esperado (1) es ALTA, ya que el CV = 100%.

# grafico
f = dpois(x = 0:10, lambda = 1)

windows()
barplot(height = f, names.arg = 0:10, col = "gray",
        ylab = "Probabilidad", xlab = "x", main = "Distr. POISSON")

#############################
# EJEMPLO BINOMIAL NEGATIVA #
#############################

# a.

# X = numero de estados financieros que se deben seleccionar hasta 
#     obtener r = 3 defectuosos (con algun tipo de error)

# exito = estados financieros con algun defecto
# Rango X = {3, 4, 5, 6, ..... }

p = 0.03
r = 3
# b. la distribucion probabilistica de X es binomial negativa con
# parametros r = 3 y p = 0.03

# c. Pr (X = 20) = ?   r = 3
dnbinom(x = 20, size = 3, prob = 0.03)
# 0.003391645

# interpretacion :
# la probabilidad de que el vigésimo estado financiero investigado sea 
# el tercero que tenga algún error es del 0.339%.

# d. media y CV

# media
r/p
# el numero de estados financieros que se espera se deben investigar 
# hasta obtener el tercero con algun error es 100-

# CV
sqrt( r * (1- p)/p^2 ) / (r/p)
#0.5686241

#grafica
f = dnbinom(x = 3:500, size = 3, prob = 0.03)

windows()
barplot(height = f, names.arg = 3:500)
