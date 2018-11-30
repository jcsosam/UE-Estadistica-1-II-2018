# Importar la base de datos.
# ¡CUIDADO! no olvide cambiar el directorio.
saber <- read.csv("C:/Users/Juan Camilo/Dropbox/UE/Estadistica 1  II-2018/data/saber11_2015_1.csv")

# Visualizar la base ded datos.
View(saber)

dim(saber)
# 965  16
# Esta base de datos consta de 965 registros y 16 variables.

# 1.

sum( as.numeric( !complete.cases(saber) ) )
# 19
# 19 registros tienen datos faltantes.

# Remover datos faltantes. 
saber <- saber[complete.cases(saber), ]

dim(saber)
# 946  16
# Esta base de datos completa consta de 946 registros y 16 variables.

n <- nrow(saber)  # 946 (tamaño de la muestra)
p <- ncol(saber)  # 16  (numero de variables)

# Adjuntar la base de datos: permite trabajar con las variables directamente.
attach(saber)

# 2.

tabla.nombre <- table( factor(NOMBREINSTITUCION) )
table( tabla.nombre )
#   1   2   3   4 
# 809  46  11   3

# Es una buena constumbre incluir "factor" cuando la variable sea calitativa y 
# expresada con caracteres.

# Hay 809 instituciones que aparecen 1 vez.
# Hay 46  instituciones que aparecen 2 veces.
# Hay 11  instituciones que aparecen 3 veces.
# Hay 3   instituciones que aparecen 4 veces.

# Numero de instituciones educativas
length( tabla.nombre )
# 869
# Otra forma usando la funcion "unique"
length( unique(NOMBREINSTITUCION) )
# 869
# Esta base de datos contiene 869 instituciones educativas diferentes.

# 3.

# código del municipio: cualitativa/nominal.
# nombre del municipio: cualitativa/nominal.
# departamento: cualitativa/nominal.
# calendario: cualitativa/nominal.
# naturaleza: cualitativa/nominal.
# jornada: cualitativa/nominal.
# número de estudiantes evaluados: cuantitativa discreta/razon.
# calificaciones promedio de la institución en: cuantitativa continua/razon.


# 4.

length( unique(NOMBREMUNICIPIO) )
# Esta base de datos contiene la informacion de 142 municipios.

# 5. 

sum( EVALUADOS )
# La cantidad total de estudiantes evaluados es 25586.

# 6.

tabla.mun <- sort(x = table( factor(NOMBREMUNICIPIO) ), decreasing = TRUE)
tabla.mun[1:5]
# NOMBREMUNICIPIO
# BOGOTÁ D.C.        CALI     PALMIRA    MEDELLIN     POPAYAN 
#         200         186          33          30          28 

# Los cinco municipios con mayor cantidad de instituciones educativas son:
# Bogota D. C., Cali, Palmira, Medellin y Popayan.

# 7.

saber[EVALUADOS == max(EVALUADOS), c("NOMBREINSTITUCION","EVALUADOS","JORNADA","CALENDARIO","NATURALEZA","NOMBREMUNICIPIO")]
#                NOMBREINSTITUCION EVALUADOS              JORNADA CALENDARIO NATURALEZA NOMBREMUNICIPIO
#800 COL BRITANICO SEDE TUNJUELITO       295 SABATINA - DOMINICAL          A NO OFICIAL     BOGOTÁ D.C.

# La institucion con mayor cantidad de evaluados es el COL BRITANICO SEDE 
# TUNJUELITO con 295 evaluados.
# Jornada: SABATINA - DOMINICAL.
# Calendario: A.
# Naturaleza: NO OFICIAL.
# Municipio: BOGOTÁ D.C.

# 8.

tabla.jornada         <- table( factor(JORNADA) )
frec.absoluta.jornada <- as.numeric( tabla.jornada )
frec.relativa.jornada <- as.numeric( round(tabla.jornada/n, 3) )

# Distribucion de frecuencias
distr.frecs.jornada <- data.frame(frec.absoluta.jornada, frec.relativa.jornada)
colnames(distr.frecs.jornada) <- c("F. Abs", "F. Rel")  # nombres columnas 
rownames(distr.frecs.jornada) <- c("Ordinaria","Manana","Noche","Fin de semana","Tarde")  # nombres filas

distr.frecs.jornada
#                F. Abs  F. Rel
# Ordinaria         265   0.280
# Manana            276   0.292
# Noche             161   0.170
# Fin de semana     201   0.212
# Tarde              43   0.045

# Nombres del grafico
nombres.jornada <- c("Ordinaria","Manana","Noche","Fin de semana","Tarde")
# Grafico
windows()
barplot(height = frec.absoluta.jornada, ylim = c(0, 284),
        col="lightblue", border = "blue",
        xlab = "JORNADA", ylab = "Frecuencia", main = "Diagrama de barras",
        names.arg = nombres.jornada, cex.names = 0.6)

# 9.

tabla.naturaleza <- table( factor(NATURALEZA) )
frec.absoluta.naturaleza <- as.numeric( tabla.naturaleza )
frec.relativa.naturaleza <- as.numeric( round(tabla.naturaleza/n, 3) )

# Distribucion de frecuencias
distr.frecs.naturaleza <- data.frame(frec.absoluta.naturaleza, frec.relativa.naturaleza)
colnames(distr.frecs.naturaleza) <- c("F. Abs", "F. Rel")      # nombres columnas 
rownames(distr.frecs.naturaleza) <- c("No oficial","Oficial")  # nombres filas

distr.frecs.naturaleza
#            F. Abs F. Rel
# No oficial    830  0.877
# Oficial       116  0.123

# La mayoria de la instituciones son de caracter NO OFICIAL (87.7%).

# Nombres del grafico
nombres.naturaleza <- c("No oficial","Oficial")

# Grafico
windows()
pie(frec.absoluta.naturaleza, 
    labels = paste(nombres.naturaleza, 100 * frec.relativa.naturaleza, "%"), 
    col = c("lightblue","lightgray"), main = "Torta de Naturaleza")

# La funcion "paste" es util para pegar valores.

# 10.

# Tabla de frecuencias relativas
tabla.jornada.naturaleza <- round( table( JORNADA, NATURALEZA ) / n, 3 )

# Grafico
windows()
barplot(height = tabla.jornada.naturaleza, horiz = FALSE, beside = TRUE, 
        legend.text = TRUE, xlab = "Naturaleza", ylab = "Porcentaje", 
        main = "Naturaleza por jornada")

# 11.

# Perfiles fila (total de las filas es 100%)
round( prop.table(x = tabla.jornada.naturaleza, margin = 1), 3 )
# Por ej., de todos los colegios no oficiales, el 99.3% son de jornada completa
# u ordinaria.

# Perfiles columna (total de las columnas es 100%)
round( prop.table(x = tabla.jornada.naturaleza, margin = 2), 3 )
# Por ej., de todos los colegios de jornada completa u ordinaria, el 31.7% son de
# naturaleza no oficial.

# 12. 

summary( PROMLECTURACRITICA )
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 22.50   45.26   49.00   51.65   57.39   74.53 

# Eel promedio de lectura critica menor es 22.50.
# El 25% de las instituciones educativas tiene promedios de lectura critica inferiores a 45.26.
# El 50% de las instituciones educativas tiene promedios de lectura critica inferiores a 49.00.
# El 50% de las instituciones educativas tiene promedios de lectura critica inferiores a 51.65.
# La media del promedio de lectura critica es 57.39.
# El promedio de lectura critica mayor es 74.53.

# 13.

# Media
round( mean(PROMLECTURACRITICA), 3 )
# La media de del promedio de lectura critica de las instituciones educativas es 51.65.

# Varianza
round( var(PROMLECTURACRITICA), 3 )
# 74.474

# Desviacion estandar
round( sd(PROMLECTURACRITICA), 3 )
# 8.63

# Coeficiente de variacion
round(100 * sd(PROMLECTURACRITICA) / mean(PROMLECTURACRITICA), 1)
# 16.7%

# La dispersion del promedio de lectura critica de las instituciones educativas 
# es ALTA con respecto a la calilicacion promedio (51.65).

# Grafico
windows()
hist(x = PROMLECTURACRITICA, freq = TRUE, 
     xlab = "Promedio lectura critica", ylab = "Porcentaje",
     main = "Histograma", col ="lightgray", border = "black")
abline(v = mean(PROMLECTURACRITICA),   col = "red",  lty = 2, lwd = 2)
abline(v = median(PROMLECTURACRITICA), col = "blue", lty = 2, lwd = 2)
legend("topright", legend = c("Media","Mediana"), fill = c("red","blue"))

# Dados los valores que toman la media y la mediana, y la forma del histograma, 
# la distribucion de los promedios de lectura critica de las instituciones 
# presenta un ligero sesgo negativo, lo cual indica que hay algunas instituciones 
# cuyos estudiantes tienen falencias serias en lectura.

# 14.

# Medidas de tendencia central
summary(PROMLECTURACRITICA)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 22.50   45.26   49.00   51.65   57.39   74.53 

summary(PROMMATEMATICA)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 22.00   44.20   48.31   51.96   57.45   87.82 

# Coeficiente de variacion
round(100 * sd(PROMLECTURACRITICA) / mean(PROMLECTURACRITICA), 1)
# 16.7%

round(100 * sd(PROMMATEMATICA) / mean(PROMMATEMATICA), 1)
# 20.9%

# Grafico
windows()
boxplot(PROMLECTURACRITICA, PROMMATEMATICA, col = c("lightblue", "bisque"),
        names = c("Lectura", "Matematicas"),
        main = "Diagramas de caja", 
        horizontal = TRUE,
        border = c("blue","red"), boxwex = 0.3)
grid()
legend("topright", legend = c("Lectura", "Matematicas"), fill = c("lightblue", "bisque"))

# La dispersion del promedio en matematicas parece ser un poco mayor que la 
# dispersion del promedio en lectura critca. En ambos casos hay una institucion 
# cuyos estudiantes presentan serias deficiencias en comparacion con el resto de 
# las instituciones. Finalmente, existe una disparidad notoria en la enseñanza 
# de las matematicas dado que hay una cantidad considerable de instituciones 
# que sobresalen al respecto. Tales diferencias en el desempeño de los 
# estudiantes no es marcado en las habilidades lectoras de los estudiantes a lo 
# largo de las instituciones.


# 15.
# Datos
PROMLECTURACRITICA.NOOFICIAL <- PROMLECTURACRITICA[NATURALEZA == "NO OFICIAL"]
PROMLECTURACRITICA.OFICIAL   <- PROMLECTURACRITICA[NATURALEZA == "OFICIAL"]

# Medidas de tendencia central
summary(PROMLECTURACRITICA.NOOFICIAL)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 31.33   45.78   49.66   52.51   59.12   74.53

summary(PROMLECTURACRITICA.OFICIAL)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 22.50   42.95   45.36   45.53   47.42   57.00 

# Coeficiente de variacion
round(100 * sd(PROMLECTURACRITICA.NOOFICIAL) / mean(PROMLECTURACRITICA.NOOFICIAL), 1)
# 16.5%

round(100 * sd(PROMLECTURACRITICA.OFICIAL) / mean(PROMLECTURACRITICA.OFICIAL), 1)
# 11.6%

# Grafico
windows()
boxplot(formula = PROMLECTURACRITICA ~ NATURALEZA, boxwex = 0.3, horizontal = TRUE, 
        xlab = "Promedio en lectura crítica", ylab = "Naturaleza",
        main = "Lectura crítica por naturaleza", 
        col = c("lightgray", "palegreen"), border = c("gray","darkgreen"))
grid()
legend("topright", legend = c("No oficial", "Oficial"), fill = c("gray", "darkgreen"))

# En promedio, los colegios no oficiales tienen mejores resultados en lectura 
# crítica que los colegios oficiales. Aunque en terminos generales, la 
# distribucion del promedio en lectura critica de los colegios oficiales parece 
# mas homogenea en comparacion con las instituciones no oficiales para las 
# cuales la variabilidad es mayor.

# 16.

datos <- data.frame(PROMLECTURACRITICA, PROMMATEMATICA, PROMSOCIALESYCIUDADANAS)
colnames(datos) <- c("Lectura","Matematicas","Sociales")

# Grafico
windows()
pairs(datos, col = "purple", labels = c("Lectura","Matematicas","Sociales"))

# Matrix de correlacion
round( cor(datos), 3 )

# Los dispersogramas muestran que la relacion entre el promedio de lectura 
# critica, matematicas y sociales es directa. Ademas tal relacion es muy fuerte 
# dado que todos los coeficientes de correlacion son mayores a 90%. Aunque todas 
# las relaciones son fuertes, la relacion mas marcada ocurre entre el promedio 
# de lectura y el promedio de sociales (95.4%).

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------