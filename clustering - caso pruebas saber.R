###############################
# EJEMPLO 1, datos saber 2015 #
###############################

#################
### Librerias ###
#################

#install.packages("ape")

library(ape)  # graphics for trees

##################################
### Importar conjunto de datos ###
##################################

# establecer el directorio de trabajo
setwd("C:/Users/Toshiba/Dropbox/UE/Estadistica 1  II-2018/data/")

# importar datos
saber15 <- read.csv(file = "saber11_2015_1.csv", header = TRUE, sep = ",")

#matriz de datos
X <- saber15[ , c("PROMLECTURACRITICA","PROMMATEMATICA","PROMSOCIALESYCIUDADANAS",
                  "PROMCIENCIASNATURALES","PROMINGLES","PROMRAZONAMIENTOCUANTITA",
                  "PROMCOMPETENCIASCIUDADAN")]

colnames(X) <- c("Lectura","Matematicas","Sociales","Naturales", "Ingles",
                 "Razonamiento","Ciudadanas")

n <- nrow(X)  # numero de individuos
p <- ncol(X)  # numero de variables

###############################
### Hierarchical clustering ###
###############################

# matriz de distancias (entre individuos)
dm  <- dist(x = X, method = "euclidean")

# ajuste del algoritmo
fit <- hclust(d = dm, method = "average")   

# dendograma
windows()
plot(fit, hang = -1, labels = F)

# representacion usando componentes principales
pc   <- princomp(X)         # componentes principales
labs <- cutree(fit, k = 3)  # membresias (labels)

windows()
plot(pc$scores[ , 1:2], col = labs, pch = labs, cex = 0.7)

##############################
### Clustering de K medias ###
##############################