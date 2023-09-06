# Creamos vectores con los datos de cada conjunto
c <- c(6.8, 7.3, 6.3, 6.9, 7.1)
x1 <- c(8.2, 8.7, 9.7, 9.2, 8.6)
x2 <- c(7.7, 8.4, 8.6, 8.1, 8.0)
x3 <- c(6.9, 5.8, 7.2, 6.8, 7.4)
x4 <- c(5.9, 6.1, 6.9, 5.7, 6.1)

# Creamos una matriz con los datos para podes calcular SCD, SCE, SCT
X <- cbind(c, x1, x2, x3, x4)


5*apply(x, 2, function(i) (mean(i)-mean(x))**2) # No entendí que era esto que quiso hacer

#SCE
promedio <- mean(X)
SCE <- sum(apply(X, 2, function(i) length(i)*(mean(i)-promedio)^2))

#SCD
SCDParciales <- apply(X, 2, function(i) (i-mean(i))^2)

SCD <- sum(SCDParciales)

SCTOTAL <- SCD + SCE

Fobs <- (SCE/ (5-1)) / (SCD / (25-5))


# Análisis de residuos
medias <- apply(x, 2, function(i) mean(i)) #Calculamos un vestor para calcular las medias de cada grupo

#Tabla de residuos
res <- apply(x,1, function(i) i-medias) #Me crea una tabla con los epsilon
plot(data.frame(res))



