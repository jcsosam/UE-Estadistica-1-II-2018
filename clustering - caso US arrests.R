#########################################
# EJEMPLO 1: Arrestos en Estados Unidos #
#########################################

#-------------------------------------------------------------------------------
# Descripcion
# Este conjunto de datos contiene estadísticas, en arrestos por cada 100,000 
# residentes por asalto, asesinato y violación en cada uno de los 50 estados de 
# Estados Unidos en 1973. También se da el porcentaje de la población que vive 
# en áreas urbanas.
#-------------------------------------------------------------------------------

head(USArrests)

n <- nrow(USArrests)  # numero de individuos
p <- ncol(USArrests)  # numero de variables

#################
### Librerias ###
#################

library(MASS)
library(mclust)  ## model-based clustering
library(fpc)     ## diagnosticos

###############################
### Estadistica descriptiva ###
###############################

# algunas medidas estadisticas
summary(USArrests)

# prisma de dispersogramas
windows()
pairs(USArrests, pch = 16, col = "red", gap = 0, xaxt = "n", yaxt = "n")

# Parallel coordinate plot
windows()
parcoord(USArrests, cex.axis = 1.4, col = "red")

###############################
### Hierarchical clustering ###
###############################

# matriz de distancias (entre individuos)
dm  <- dist(x = USArrests, method = "euclidean")

# ajuste del algoritmo
fit <- hclust(d = dm, method = "average")   

# dendograma
windows()
plot(fit, hang = -1, cex = 0.75)

#-------------------------------------------------------------------------------
# cuantos clusters?
# Examinar los tamaños de los cambios de altura en el dendrograma y tomar un 
# "gran" cambio para indicar el número apropiado de clusters para los datos.
#-------------------------------------------------------------------------------

# alturas (n-1 valores)
h <- fit$height

# cambios
dh <- h[-1] - h[-(n-1)]

# corte
lh <- mean( h[c(which.max(dh), which.max(dh) + 1)] )

# dendograma
windows()
plot(fit, hang = -1, cex = 0.75)
abline(h = lh, col = "gray")

# representacion usando componentes principales
pc   <- princomp(USArrests)  # componentes principales
labs <- cutree(fit, h = lh)  # membresias (labels)

windows()
plot(pc$scores[ , 1:2], type = "n")
text(pc$scores[ , 1:2], labels = rownames(USArrests), col = labs, cex = 0.7)

# representacion usando componentes principales con 4 clusters
labs <- cutree(fit, k = 4)

windows()
plot(pc$scores[ , 1:2], type = "n")
text(pc$scores[ , 1:2], labels = rownames(USArrests), col = labs, cex = 0.7)

##############################
### Clustering de K medias ###
##############################

# variabilidad
sapply(X = USArrests, FUN = var)

# estandarizacion
rge <- sapply(USArrests, function(x) diff(range(x)))
X   <- sweep(x = USArrests, MARGIN = 2, STATS = rge, FUN = "/")

sapply(X, var)

# Las varianzas de los datos estandarizado son muy similares, y ahora podemos
# proceder con la agrupación de los datos.

pctp   <- 0
within <- kmeans(x = X, centers = 1)$totss
for (k in 2:8) {
     clk    <- kmeans(X, centers = k)     # ajustar clustering de k medias
     wi     <- sum(clk$withinss)          # WGSS
     pcte   <- clk$betweenss / clk$totss  # % de variabilidad explicado
     pctp   <- c(pctp, pcte)
     within <- c(within, wi)
}

# grafico
windows(width = 10, height = 5)
par(mfrow = c(1, 2))
plot(pctp, type = "b", pch = 16, cex = 2, cex.lab = 1.1, lwd = 2, ylim = c(0,1),
     col = "blue",xlab = "K", ylab = "% variabilidad explicado")
plot(within, type = "b", pch = 16, cex = 2, cex.lab = 1.1, lwd = 2,
     col = "red",xlab = "K", ylab = "WGSS")

#-------------------------------------------------------------------------------
# INTERPRETACION
# - El porcentaje de variabilidad explicada aumenta con el número de clusters.
# - Del mismo modo, el WGSS disminuye con el número de clusters.
# - En K = 1, los datos se originan a partir de una población y no hay nada 
#   explicado nada.
# - El otro extremo, si K = n, entonces el 100% de la variabilidad se explica 
#   teniendo cada observación en su propio clúster, pero nada se aprende.
#-------------------------------------------------------------------------------

# representacion usando componentes principales
pc   <- princomp(USArrests)             # componentes principales
labs <- kmeans(X, centers = 4)$cluster  # membresias (labels)

windows()
plot(pc$scores[ , 1:2], type = "n")
text(pc$scores[ , 1:2], labels = rownames(USArrests), col = labs, cex = 0.7)

#prisma
windows()
pairs(X, pch = 16, gap = 0, xaxt = "n", yaxt = "n", col = labs)

######################################
### Clustering basado en el modelo ###
######################################

# ajuste del modelo
mc <- Mclust(data = USArrests)

# ver resultados (probabilidades, vector de medias, matriz de covarianzas)
summary(mc, parameters = T)

# criterio de seleccion del modelo
BIC <- mclustBIC(data = USArrests)

windows()
plot(BIC)

# visualizar resultados
plot(mc, what = "classification")


# representacion usando componentes principales
pc   <- princomp(USArrests)   # componentes principales
labs <- mc$classification     # membresias (labels)

windows()
plot(pc$scores[ , 1:2], type = "n")
text(pc$scores[ , 1:2], labels = rownames(USArrests), col = labs, cex = 0.7)

####################
### Diagnosticos ###
####################

totss <- kmeans(USArrests, centers = 1)$totss
pve   <- 0
Kmax  <- 12
for (i in 2:Kmax) {
     # % de vriabilidad explicada
     pve <- c(pve, kmeans(USArrests, centers = i)$betweenss / totss) 
}

# fuerza de prediccion simulada (CV de la calidad de los clusters)
ps <- prediction.strength(USArrests, Gmax = Kmax)

# graficos
windows(width = 10, height = 5)
par(mfrow = c(1, 2))
plot(1:Kmax, pve, type = "b", cex = 2, pch = 16, col = "red", ylim = c(0,1),
     xlab = "K", ylab = "% variabilidad explicada") 
plot(1:Kmax, ps$mean.pred, type = "b", cex = 2, pch = 16, col = "blue", 
     ylim = c(0,1), xlab = "K", ylab = "Fuerza de prediccion")

# Jittering y Bootstrap resampling

cbj <- clusterboot(USArrests, bootmethod = "jitter", krange = 3, 
                   clustermethod = kmeansCBI)

cbj$jittermean  # Indice de Jaccard Medio (usando Jittering)

cbb <- clusterboot(USArrests, bootmethod = "boot",   krange = 3, 
                   clustermethod = kmeansCBI)

cbb$bootmean    # Indice de Jaccard Medio (usando Boostrap resampling)

# Indice de Dunn
d    <- dist(USArrests)                    # matriz de distancias
cl   <- kmeans(USArrests, centers = 3)     # K-means clustering
dunn <- cluster.stats(d, cl$cluster)$dunn  # Dunn Index de la data original

#-------------------------------------------------------------------------------
# INTERPRETACION
# El Índice de Dunn para K = 3 grupos es 0.2503, pero no tenemos contexto para
# entender este valor.
#-------------------------------------------------------------------------------

# boostrap
B <- 1000  # numero de muestras (bootstrap)
for (i in 1:B) {
     print(i)
     booti <- as.integer(1 + n * runif(n))   # indices de la muestra
     bd    <- dist( USArrests[booti, ] )     # distances of bootstrap sample         
     clb   <- kmeans(bd, centers = 3)        # cluster bootstrap sample
     dunn  <- c(dunn, cluster.stats(bd, clb$cluster)$dunn )  # Indices de Dunn 
}

# grafico
windows()
hist(dunn, col = "lightgray", main = " ", ylab = " ", yaxt = "n", freq = F, 
     xlab = " ", breaks = 15)
abline(v = dunn[1], lwd = 3, col = "blue")

#-------------------------------------------------------------------------------
# INTERPRETACION
# El índice de Dunn para los datos observados aparece cerca del centro de una 
# distribución sesgada a la derecha que también parece ser una distribución 
# bimodal.
#-------------------------------------------------------------------------------

