#########################################
######## TABLAS DE CONTINGENCIA #########
#########################################

# importar datos
saber <- read.csv("C:/Users/Juan Camilo/Dropbox/UE/Estadistica 1  II-2018/data/saber11_2015_1.csv")

# Remover datos faltantes
saber <- saber[complete.cases(saber), ]

# dimension de la base de datos
dim(saber)

n <- nrow(saber)

# adjuntar variables
attach(saber)

###############################
# DISTRIBUCION DE FRECUENCIAS #
###############################

windows()
histograma <- hist(PROMMATEMATICA, freq = FALSE, density = 20,
                   border = "blue", col  = "orange",
                   main = "Matematicas", font.main = 4,
                   xlab = "Promedio", ylab = "Densidad")


summary(PROMMATEMATICA)

# puntos medios
puntos  <- histograma$mids

length(puntos)

# inter
limites <- histograma$breaks

# categorias
PROMMATEMATICA.cat <- cut(x = PROMMATEMATICA, breaks = limites)

tab1 <- table(PROMMATEMATICA.cat)

sum( tab1 )

length( tab1 )

histograma$counts

# distribucion de frecuencias
fr.abs    <- histograma$counts  
fr.abs.ac <- cumsum( fr.abs )
fr.rel    <- round( 100 * fr.abs/n, 2)
fr.rel.ac <- round( cumsum( fr.rel ), 2)

distr.frecs = data.frame(fr.abs, fr.rel, fr.abs.ac, fr.rel.ac)
colnames(distr.frecs) = c("F.Abs.","F.Rel.","F.A.Acum.","F.R.Acum")
rownames(distr.frecs) = names( tab1 )


########################
#TABLAS DE CONTINGENCIA#
########################
#TABLAS BIDIMENSIONALES PARA VARIABLES CUALITATIVAS

#FRECUENCIAS ABSOLUTAS (CONTEOS)
tabla = table(NATURALEZA, CALENDARIO)
tabla

#sumar todas las frecuencias absolutas
sum(tabla)

margin.table(x = tabla)

#distribucion de frecuencias por FILAS
margin.table(x = tabla, margin = 1)

round( 100 * margin.table(x = tabla, margin = 1)/n, 2 )

#distribucion de frecuencias por COLUMNAS
margin.table(x = tabla, margin = 2)

round( 100 * margin.table(x = tabla, margin = 2)/n, 2 )

#FRECUENCIAS RELATIVAS CON RESPECTO AL TOTAL
round( 100 * prop.table(x = tabla), 2)

#FRECUENCIAS RELATIVAS CON RESCPECTO A LAS FILAS
#PERFILES FILA
round( 100 * prop.table(x = tabla, margin = 1), 2)

#FRECUENCIAS RELATIVAS CON RESCPECTO A LAS COLUMNAS
#PERFILES COLUMNA
round(100 * prop.table(x = tabla, margin = 2), 2)

# GRAFICOS COMPUESTOS
windows()
barplot(tabla, legend = TRUE, beside = TRUE, horiz = FALSE, ylim = c(0, 500),
        main = "Calendario por Naturaleza", 
        names.arg = c("Calendario A", "Calendario B"),
        xlab = "Calendario", border = c("blue","gray"), 
        col = c("lightblue","lightgray"))

windows()
barplot(tabla, legend = TRUE, horiz = FALSE, beside = FALSE,
        main = "Calendario por Naturaleza", 
        names.arg = c("Calendario A", "Calendario B"),
        xlab = "Calendario", border = c("blue","gray"), 
        col = c("lightblue","lightgray"))


tabla2 <- table(NATURALEZA, PROMMATEMATICA.cat)

windows()
barplot(tabla2, legend = TRUE, beside = TRUE, horiz = FALSE,
        main = "Naturaleza por promedio en matematicas", cex.names = 0.8,
        xlab = "Promedio (categoria)", 
        border = c("blue","gray"), col = c("lightblue","lightgray"))


tabla2 <- table(PROMMATEMATICA.cat, NATURALEZA)

windows()
barplot(tabla2, legend = TRUE, beside = TRUE, horiz = FALSE,
        main = "Promedio en matematicas por Naturaleza", 
        xlab = "Naturaleza del coloegio" )
