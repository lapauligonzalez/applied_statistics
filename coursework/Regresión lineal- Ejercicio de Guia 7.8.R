#-----Ejercicio 7.8 de la Guia

# a) Plantear un modelo lineal de regresión para la facturación anual.  
# Estimar los parámetros del modelo y el error estándar

x <- locales$superficie #variable explicativa
y <- locales$ventas #variable explicada

B1 <- cov(x,y)/var(x)
B0 <- mean(y) - B1*mean(x)
y_sombrero <- B0 + B1*x
S2 <- sum((y-y_sombrero)**2)/(length(x)-2)
se_B1 <- sqrt((S2)/sum((x-mean(x))^2))

# b) Hacer un scatterplot de las variables y un grafico de residuos estandarizados 
#versus valores predichos yi. Deberıa observarse un punto at́ıpico. A que podría deberse?
#Explicar cómo puede influir en el ajuste del modelo.

#Scatterplot con la recta de regresión estimada
plot(x,y,pch=20)
abline(a = B0, b = B1, col = "red", lwd = 4)

#Gráfico de los residuos estandarizados
modelo <- lm(y ~ x, data = locales) 
summary(modelo)

e_std <- rstandard(modelo)
plot(e_std)

# c) Hacer un test de hipótesis donde:
#H0 : B1 = 0
#H1 : B0 =! 0

#Pivote (bajo H0 verdadero): T = (B1)/se(B1)

#T observado:
t_obs <- B1/se_B1
n <- length(locales$superficie)

#Calculamos el p-valor
1-pt(t_obs, n-2) #n - 2 porque estimamos B0 y B1
#Como el valor que tira el p-valor es mucho menos al alfa/2, entonces se rechaza H0

# d) ¿Qué facturación mínima puede garantizarse con 90% de probabilidad para un local 
#500m2 en el próximo año?


